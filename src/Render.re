/*
   See: https://wiki.nesdev.com/w/index.php/PPU_scrolling#Summary

   The PPU has a single 15 bit address register, `v`, used for all reads and writes to VRAM.
   However, since the NES only has an 8-bit data bus, all modifications to the address
   register must be done one byte at a time. As a consequence, a buffer is used to modify `v`.
   The 15-bit buffer register, `t`, must receive two writes before forming a completed address.
   Two different interfaces are exposed for the comfort of the application programmer:

   1. The PPUSCROLL interface at $2005 for setting the scroll position of the next frame.
     * Each byte it receives specifies either the coarse x and fine x or coarse y and fine y coordinates.
     * It can be written to at any point during vblank and will copy `t` to `v` just before rendering.
     * It can also be written to during hblank for mid-frame raster effects.
   2. The PPUADDR interface at $2006 for updating the address before using PPUDATA to read/write VRAM.
     * Each byte it receives is either the low byte or high byte for the buffer.
     * It immediately copies `t` to `v` after the second write.

   The current value of the buffer is copied to the address register during the last vblank scanline.
   During rendering, the address register is updated by the PPU to reflect the current memory access.
 */

module ScrollInfo = {
  type t = {
    mutable nt_index: int,
    mutable coarse_x: int,
    mutable coarse_y: int,
    mutable fine_x: int,
    mutable fine_y: int,
  };

  let from_registers = (base, control, fine_x): t => {
    nt_index: control land 0x3,
    coarse_x: base land 0x1f,
    coarse_y: base lsr 5 land 0x1f,
    fine_x,
    fine_y: base lsr 12,
  };

  let build = (ppu: Ppu.t) => {
    from_registers(
      ppu.registers.ppu_address,
      ppu.registers.control,
      ppu.registers.fine_x,
    );
  };

  let next_tile = scroll =>
    if (scroll.coarse_x == 31) {
      scroll.coarse_x = 0;
      scroll.nt_index = scroll.nt_index lxor 1;
    } else {
      scroll.coarse_x = scroll.coarse_x + 1;
    };

  let next_scanline = scroll =>
    switch (scroll.fine_y == 7, scroll.coarse_y == 29) {
    | (true, true) =>
      scroll.fine_y = 0;
      scroll.coarse_y = 0;
      scroll.nt_index = scroll.nt_index lxor 2;
    | (true, _) =>
      scroll.fine_y = 0;
      scroll.coarse_y = scroll.coarse_y + 1;
    | _ => scroll.fine_y = scroll.fine_y + 1
    };

  let quad_position = (scroll: t): Types.quadrant =>
    switch (scroll.coarse_x mod 2, scroll.coarse_y mod 2) {
    | (0, 0) => TopLeft
    | (0, 1) => TopRight
    | (1, 0) => BottomLeft
    | _ => BottomRight
    };
};

type frame = array(int);

type t = {
  mutable scanline: int,
  mutable sprites: Sprite.Table.t,
  mutable scroll: ScrollInfo.t,
  mutable cache: Pattern.Table.t,
  mutable frame,
};

type sprite = {
  high_bits: int,
  line_bits: array(int),
  attributes: int,
  x_position: int,
  zero: bool,
};

type background = {
  high_bits: int,
  line_bits: array(int),
};

type pixel_type =
  | Backdrop
  | Background(int)
  | Sprite(int);

let width = 256;
let height = 240;

// NOTE: One CPU cycle is 3 PPU cycles.
let cycles_per_scanline = 341 / 3;
let scanlines_per_frame = 262;

let color_palette_data = {j|
7c 7c 7c  00 00 fc  00 00 bc  44 28 bc
94 00 84  a8 00 20  a8 10 00  88 14 00
50 30 00  00 78 00  00 68 00  00 58 00
00 40 58  00 00 00  00 00 00  00 00 00
bc bc bc  00 78 f8  00 58 f8  68 44 fc
d8 00 cc  e4 00 58  f8 38 00  e4 5c 10
ac 7c 00  00 b8 00  00 a8 00  00 a8 44
00 88 88  00 00 00  00 00 00  00 00 00
f8 f8 f8  3c bc fc  68 88 fc  98 78 f8
f8 78 f8  f8 58 98  f8 78 58  fc a0 44
f8 b8 00  b8 f8 18  58 d8 54  58 f8 98
00 e8 d8  78 78 78  00 00 00  00 00 00
fc fc fc  a4 e4 fc  b8 b8 f8  d8 b8 f8
f8 b8 f8  f8 a4 c0  f0 d0 b0  fc e0 a8
f8 d8 78  d8 f8 78  b8 f8 b8  b8 f8 d8
00 fc fc  f8 d8 f8  00 00 00  00 00 00
|j};

let empty_sprite = {
  high_bits: 0,
  line_bits: [||],
  attributes: 0,
  x_position: 0,
  zero: false,
};

let color_palette =
  String.trim(color_palette_data)
  |> Js.String.splitByRe([%bs.re "/\\s+/g"])
  |> Array.map(str => int_of_string("0x" ++ Util.default("00", str)));

let make = (ppu: Ppu.t, rom: Rom.t, ~on_nmi: unit => unit) => {
  let context = {
    scanline: 0,
    sprites: Array.make(8, None),
    scroll: ScrollInfo.build(ppu),
    cache: Pattern.Table.load(rom.chr),
    frame: Array.make(width * height * 3, 0),
  };

  let draw = (color_index, pixel) => {
    let frame_offset =
      (context.scanline * 256 + context.scroll.coarse_x * 8) * 3;
    for (i in 0 to 2) {
      let byte = color_palette[color_index * 3 + i];
      let pixel_offset = frame_offset + pixel * 3 + i;
      context.frame[pixel_offset] = byte;
    };
  };

  let find_bg_tile = (x, y) => {
    let nt_offset = Ppu.nt_offset(context.scroll.nt_index);
    let nt = Ppu.read_vram(ppu, nt_offset + 32 * y + x);
    let pattern_index = Ppu.background_offset(ppu) + nt;
    context.cache[pattern_index];
  };

  let find_attr = (x, y) => {
    let at_offset = Ppu.nt_offset(context.scroll.nt_index) + 0x3c0;
    Ppu.read_vram(ppu, at_offset + (y / 4) lsl 3 + x / 4);
  };

  let find_background = (x, y) => {
    let pattern = find_bg_tile(x, y);
    let at_byte = find_attr(x, y);
    let quad = ScrollInfo.quad_position(context.scroll);
    {
      high_bits: Pattern.Tile.high_bits(at_byte, quad),
      line_bits: pattern[context.scroll.fine_y],
    };
  };

  let find_sprite = (x, i): option(sprite) => {
    // TODO: We can save a lot of computation if we don't recompute sprites _per pixel_.
    // However, we can't just assume only one sprite falls on a given tile. I've also seen wild
    // ass index out of bounds bugs from skipping the index to on_tile. Let's think hard on this.
    let matcher = (acc, item) =>
      switch (acc, item) {
      | (Some(_s), _) => acc
      | (None, Some(s)) => Sprite.Tile.on_tile(x * 8 + i, s) ? item : None
      | (None, None) => None
      };
    let sprite = Array.fold_left(matcher, None, context.sprites);
    switch (sprite) {
    | Some(sprite) =>
      let tile = context.cache[Ppu.sprite_offset(ppu) + sprite.tile_index];
      let line = context.scanline - sprite.y_position;
      Some({
        line_bits: Sprite.Tile.line_bits(sprite, tile, line),
        high_bits: Sprite.Tile.high_bits(sprite),
        attributes: sprite.attributes,
        x_position: sprite.x_position,
        zero: sprite.zero,
      });
    | None => None
    };
  };

  let background_color = (background, i) =>
    Background(background.high_bits lsl 2 lor background.line_bits[i]);

  let sprite_color = (sprite, i) => {
    let x_position = context.scroll.coarse_x * 8 + i - sprite.x_position;
    let index =
      Sprite.Tile.flip_hori(sprite.attributes) ? 7 - x_position : x_position;
    Sprite(sprite.high_bits lsl 2 lor sprite.line_bits[index]);
  };

  let sprite_match = (sprite, i): option(sprite) => {
    switch (sprite) {
    | Some(s) =>
      let sprite_x = context.scroll.coarse_x * 8 + i - s.x_position;
      let index =
        Sprite.Tile.flip_hori(s.attributes) ? 7 - sprite_x : sprite_x;
      s.line_bits[index] > 0 ? Some(s) : None;
    | None => None
    };
  };

  let pixel_priority = (background, sprite, i): pixel_type => {
    let bg_match = background.line_bits[i] > 0;
    switch (bg_match, sprite_match(sprite, i)) {
    | (true, Some(s)) =>
      if (s.zero) {
        Ppu.set_sprite_zero_hit(ppu.registers, true);
      };
      let behind = Sprite.Tile.behind(s.attributes);
      behind ? background_color(background, i) : sprite_color(s, i);
    | (false, Some(s)) => sprite_color(s, i)
    | (true, None) => background_color(background, i)
    | _ => Backdrop
    };
  };

  let render_tile = () => {
    let x = context.scroll.coarse_x;
    let y = context.scroll.coarse_y;
    let background = find_background(x, y);
    let backdrop = Ppu.read_vram(ppu, 0x3f00);
    for (i in 0 to 7) {
      let sprite = find_sprite(x, i);
      let pixel_type = pixel_priority(background, sprite, i);
      let color =
        switch (pixel_type) {
        | Backdrop => backdrop
        | Background(index) => Ppu.read_vram(ppu, 0x3f00 + index)
        | Sprite(index) => Ppu.read_vram(ppu, 0x3f10 + index)
        };
      draw(color, i);
    };
    ScrollInfo.next_tile(context.scroll);
  };

  let render_scanline = () => {
    if (Ppu.rendering_enabled(ppu)) {
      context.sprites = Sprite.Table.build(ppu.oam, context.scanline);
      for (_ in 0 to 31) {
        render_tile();
      };
    };
    ScrollInfo.next_scanline(context.scroll);
  };

  let start_vblank = () => {
    Ppu.set_vblank(ppu.registers, true);
    if (Ppu.vblank_nmi(ppu.registers) == Ppu.NMIEnabled) {
      on_nmi();
    };
  };

  let finish_vblank = () => {
    if (Ppu.rendering_enabled(ppu)) {
      ppu.registers.ppu_address = ppu.registers.buffer;
    };
    Ppu.set_sprite_zero_hit(ppu.registers, false);
    Ppu.set_vblank(ppu.registers, false);
    context.scroll = ScrollInfo.build(ppu);
    context.frame;
  };

  (~on_frame: frame => unit) => {
    if (context.scanline < 240) {
      render_scanline();
    } else if (context.scanline == 241) {
      start_vblank();
    } else if (context.scanline == 261) {
      finish_vblank() |> on_frame;
    };

    context.scanline = (context.scanline + 1) mod scanlines_per_frame;
  };
};