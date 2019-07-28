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

  type quad_position =
    | TopLeft
    | TopRight
    | BottomLeft
    | BottomRight;

  let quadrant = (scroll: t): quad_position =>
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

type attr_byte =
  | SpriteAttr
  | BackgrAttr;

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

let color_palette =
  String.trim(color_palette_data)
  |> Js.String.splitByRe([%bs.re "/\\s+/g"])
  |> Array.map(str => int_of_string("0x" ++ Util.default("00", str)));

let make = (ppu: Ppu.t, rom: Rom.t, ~on_nmi: unit => unit) => {
  let context = {
    scanline: 0,
    sprites: [],
    scroll: ScrollInfo.build(ppu),
    cache: Pattern.Table.load(rom.chr),
    frame: Array.make(width * height * 3, 0),
  };

  let render_pixel = (color_index, pixel) => {
    let frame_offset =
      (context.scanline * 256 + context.scroll.coarse_x * 8) * 3;
    for (i in 0 to 2) {
      let byte = color_palette[color_index * 3 + i];
      let pixel_offset = frame_offset + pixel * 3 + i;
      context.frame[pixel_offset] = byte;
    };
  };

  let palette_high_bits = (at_byte, kind) =>
    switch (ScrollInfo.quadrant(context.scroll), kind) {
    | (_, SpriteAttr) => at_byte land 3
    | (BottomLeft, _) => at_byte lsr 0 land 3
    | (BottomRight, _) => at_byte lsr 2 land 3
    | (TopLeft, _) => at_byte lsr 4 land 3
    | (TopRight, _) => at_byte lsr 6 land 3
    };

  let find_tile = (x, y) => {
    let nt_offset = Ppu.nt_offset(context.scroll.nt_index);
    let nt = Ppu.read_vram(ppu, nt_offset + 32 * y + x);
    let pattern_index = Ppu.background_offset(ppu) + nt;
    context.cache[pattern_index];
  };

  let find_attr = (x, y) => {
    let at_offset = Ppu.nt_offset(context.scroll.nt_index) + 0x3c0;
    let at = Ppu.read_vram(ppu, at_offset + (y / 4) lsl 3 + x / 4);
    palette_high_bits(at, BackgrAttr);
  };

  let get_color = (high_bits, low_bits, kind) => {
    let palette_offset = high_bits lsl 2 lor low_bits;
    switch (kind) {
    | BackgrAttr => Ppu.read_vram(ppu, 0x3f00 + palette_offset)
    | SpriteAttr => Ppu.read_vram(ppu, 0x3f10 + palette_offset)
    };
  };

  let sprite_color = (sprite: Sprite.Tile.t, x_position) => {
    // TODO: Handle horizontally or vertically flipped sprites.
    let pattern_index = Ppu.sprite_offset(ppu) + sprite.tile_index;
    let tile = context.cache[pattern_index];
    let high_bits = palette_high_bits(sprite.attributes, SpriteAttr);
    let low_bits = tile[context.scroll.fine_y];
    let pattern_x = x_position - sprite.x_position;
    get_color(high_bits, low_bits[pattern_x], SpriteAttr);
  };

  let sprite_pixel = x => {
    // TODO: Does on_tile find the first sprite that overlaps this _pixel_?
    switch (List.find(Sprite.Tile.on_tile(x), context.sprites)) {
    | Some(item) => sprite_color(item, x)
    | None => 0
    | exception Not_found => 0
    };
  };

  let pixel_priority = (backdrop, bg_color, sprite_color) => {
    // TODO: Sprite Priority!
    switch (bg_color > 0, sprite_color > 0) {
    | (true, true) => sprite_color
    | (true, false) => bg_color
    | (false, true) => sprite_color
    | (false, false) => backdrop
    };
  };

  let render_tile = () => {
    let x = context.scroll.coarse_x;
    let y = context.scroll.coarse_y;
    let high_bits = find_attr(x, y);
    let low_bits = find_tile(x, y)[context.scroll.fine_y];
    let backdrop = Ppu.read_vram(ppu, 0x3f00);
    for (i in 0 to 7) {
      let bg_color = get_color(high_bits, low_bits[i], BackgrAttr);
      let sprite_color = sprite_pixel(x * 8 + i);
      let final_color = pixel_priority(backdrop, bg_color, sprite_color);
      render_pixel(final_color, i);
    };
    ScrollInfo.next_tile(context.scroll);
  };

  let render_scanline = () => {
    if (Ppu.rendering_enabled(ppu)) {
      // TODO: Check for sprite zero hit.
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