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

  type nametable =
    | Horizontal
    | Vertical;

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

  let next_nametable = (mirroring, kind, index) => {
    switch (mirroring, kind) {
    | (Rom.Horizontal, Horizontal) => index lxor 1
    | (Rom.Horizontal, Vertical) => index lxor 2
    | (Rom.Vertical, Horizontal) => index lxor 2
    | (Rom.Vertical, Vertical) => index lxor 1
    };
  };

  let next_tile = (scroll, mirror) =>
    if (scroll.coarse_x == 31) {
      scroll.coarse_x = 0;
      scroll.nt_index = next_nametable(mirror, Horizontal, scroll.nt_index);
    } else {
      scroll.coarse_x = scroll.coarse_x + 1;
    };

  let next_scanline = (scroll, mirror) =>
    switch (scroll.fine_y == 7, scroll.coarse_y == 29) {
    | (true, true) =>
      scroll.fine_y = 0;
      scroll.coarse_y = 0;
      scroll.nt_index = next_nametable(mirror, Vertical, scroll.nt_index);
    | (true, _) =>
      scroll.fine_y = 0;
      scroll.coarse_y = scroll.coarse_y + 1;
    | _ => scroll.fine_y = scroll.fine_y + 1
    };

  let quad_position = (scroll: t): Types.quadrant =>
    switch (scroll.coarse_x / 2 mod 2, scroll.coarse_y / 2 mod 2) {
    | (0, 0) => TopLeft
    | (0, 1) => BottomLeft
    | (1, 0) => TopRight
    | (1, 1) => BottomRight
    };
};

type frame = array(int);
type sprite = (int, bool, bool);

type background = {
  high_bits: int,
  line_bits: array(int),
};

type pixel_type =
  | Backdrop
  | Background(int)
  | Sprite(int);

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

let color_palette =
  String.trim(color_palette_data)
  |> Js.String.splitByRe([%bs.re "/\\s+/g"])
  |> Array.map(str => int_of_string("0x" ++ Util.default("00", str)));

let width = 256;
let height = 240;

// NOTE: One CPU cycle is 3 PPU cycles.
let cycles_per_scanline = 341 / 3;
let scanlines_per_frame = 262;

module Context = {
  type t = {
    mutable scanline: int,
    mutable sprites: array(sprite),
    mutable scroll: ScrollInfo.t,
    mutable cache: Pattern.Table.t,
    mutable frame,
    on_nmi: unit => unit,
    ppu: Ppu.t,
  };
  let empty_sprite = (0, false, false);

  let make = (ppu: Ppu.t, rom: Rom.t, ~on_nmi: unit => unit) => {
    {
      scanline: 0,
      sprites: Array.make(256, empty_sprite),
      scroll: ScrollInfo.build(ppu),
      cache: Pattern.Table.load(rom.chr),
      frame: Array.make(width * height * 3, 0),
      ppu,
      on_nmi,
    };
  };

  let draw = (context, color_index, pixel) => {
    let frame_offset =
      (context.scanline * 256 + context.scroll.coarse_x * 8) * 3;
    for (i in 0 to 2) {
      let byte = color_palette[color_index * 3 + i];
      let pixel_offset = frame_offset + pixel * 3 + i;
      context.frame[pixel_offset] = byte;
    };
  };

  let find_bg_tile = (context, x, y) => {
    let nt_offset = Ppu.nt_offset(context.scroll.nt_index);
    let nt = Ppu.read_vram(context.ppu, nt_offset + 32 * y + x);
    let pattern_index = Ppu.background_offset(context.ppu) + nt;
    context.cache[pattern_index];
  };

  let find_attr = (context, x, y) => {
    let at_offset = Ppu.nt_offset(context.scroll.nt_index) + 0x3c0;
    Ppu.read_vram(context.ppu, at_offset + (y / 4) lsl 3 + x / 4);
  };

  let find_background = (context, x, y) => {
    let pattern = find_bg_tile(context, x, y);
    let at_byte = find_attr(context, x, y);
    let quad = ScrollInfo.quad_position(context.scroll);
    {
      high_bits: Pattern.Tile.high_bits(at_byte, quad),
      line_bits: pattern[context.scroll.fine_y],
    };
  };

  let pixel_priority = (context, background, sprite, i): pixel_type => {
    let bg_pixel = background.line_bits[i];
    let background_color = background.high_bits lsl 2 lor bg_pixel;
    let (sprite_color, behind, sprite_zero) = sprite;
    let sp_pixel = sprite_color land 0x3;
    if (sprite_zero) {
      Ppu.check_zero_hit(context.ppu, sp_pixel, bg_pixel);
    };
    switch (bg_pixel > 0, sp_pixel > 0, behind) {
    | (true, true, true) => Background(background_color)
    | (true, true, false) => Sprite(sprite_color)
    | (false, true, _) => Sprite(sprite_color)
    | (true, false, _) => Background(background_color)
    | _ => Backdrop
    };
  };

  let render_sprite_line = (context, sprite: Sprite.Tile.t) => {
    let start = sprite.x_position;
    let tile = context.cache[Ppu.sprite_offset(context.ppu)
                             + sprite.tile_index];
    let bits = Sprite.Tile.line_bits(sprite, tile, context.scanline);
    let high = Sprite.Tile.high_bits(sprite);
    for (i in start to min(start + 7, 255)) {
      let (color, _, _) = context.sprites[i];
      if (color == 0) {
        let column = i - start;
        let index = Sprite.Tile.flip_hori(sprite) ? 7 - column : column;
        let color = high lsl 2 lor bits[index];
        let behind = Sprite.Tile.behind(sprite);
        context.sprites[i] = (color, behind, sprite.zero);
      };
    };
  };

  let sprite_match = (context, sprite) =>
    switch (sprite) {
    | Some(s) => render_sprite_line(context, s)
    | None => ()
    };

  let precompute_sprites = context => {
    for (i in 0 to 255) {
      context.sprites[i] = empty_sprite;
    };
    let sprites = Sprite.Table.build(context.ppu.oam, context.scanline);
    Array.iter(sprite_match(context), sprites);
  };

  let render_tile = context => {
    let x = context.scroll.coarse_x;
    let y = context.scroll.coarse_y;
    let background = find_background(context, x, y);
    let backdrop = Ppu.read_vram(context.ppu, 0x3f00);
    for (i in 0 to 7) {
      let sprite = context.sprites[x * 8 + i];
      let pixel_type = pixel_priority(context, background, sprite, i);
      let color =
        switch (pixel_type) {
        | Backdrop => backdrop
        | Background(index) => Ppu.read_vram(context.ppu, 0x3f00 + index)
        | Sprite(index) => Ppu.read_vram(context.ppu, 0x3f10 + index)
        };
      draw(context, color, i);
    };
    ScrollInfo.next_tile(
      context.scroll,
      (context.ppu.pattern_table)#mirroring,
    );
  };

  let render_scanline = context => {
    if (Ppu.rendering_enabled(context.ppu)) {
      precompute_sprites(context);
      for (_ in 0 to 31) {
        render_tile(context);
      };
    };
    ScrollInfo.next_scanline(
      context.scroll,
      (context.ppu.pattern_table)#mirroring,
    );
  };

  let start_vblank = context => {
    Ppu.set_vblank(context.ppu.registers, true);
    if (Ppu.vblank_nmi(context.ppu.registers) == Ppu.NMIEnabled) {
      context.on_nmi();
    };
  };

  let finish_vblank = context => {
    if (Ppu.rendering_enabled(context.ppu)) {
      context.ppu.registers.ppu_address = context.ppu.registers.buffer;
      Ppu.set_sprite_zero_hit(context.ppu.registers, false);
    };
    Ppu.set_vblank(context.ppu.registers, false);
    context.scroll = ScrollInfo.build(context.ppu);
    context.frame;
  };
};

let sync = (context: Context.t, ~on_frame: frame => unit) => {
  if (context.scanline < 240) {
    Context.render_scanline(context);
  } else if (context.scanline == 241) {
    Context.start_vblank(context);
  } else if (context.scanline == 261) {
    Context.finish_vblank(context) |> on_frame;
  };

  context.scanline = (context.scanline + 1) mod scanlines_per_frame;
};