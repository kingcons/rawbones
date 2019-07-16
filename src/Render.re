type frame = array(int);

type t = {
  mutable scanline: int,
  mutable scroll: Ppu.ScrollInfo.t,
  mutable cache: Pattern.Table.t,
  mutable frame,
};

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
    scroll: Ppu.scroll_info(ppu),
    cache: Pattern.Table.load(rom.chr),
    frame: Array.make(width * height * 3, 0),
  };

  let palette_high_bits = at_byte =>
    switch (Ppu.quadrant(context.scroll)) {
    | BottomLeft => at_byte lsr 0 land 3
    | BottomRight => at_byte lsr 2 land 3
    | TopLeft => at_byte lsr 4 land 3
    | TopRight => at_byte lsr 6 land 3
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

  let render_tile = () => {
    // TODO: Factor this to allow for rendering sprites too?
    let x = context.scroll.coarse_x;
    let y = context.scroll.coarse_y;
    let nt_offset = Ppu.nt_offset(context.scroll.nt_index);
    let at_offset = nt_offset + 0x3c0;
    let nt = Ppu.read_vram(ppu, nt_offset + 32 * y + x);
    let at = Ppu.read_vram(ppu, at_offset + (y / 4) lsl 3 + x / 4);
    let tile_index = Ppu.background_offset(ppu) + nt;
    let high_bits = palette_high_bits(at);
    let tile = context.cache[tile_index];
    let scanline_low_bits = tile[context.scroll.fine_y];
    // Check PPU scrolling docs. One of fine_y or fine_x does not change during rendering. Which one?
    for (i in 0 to 7) {
      let palette_index = high_bits lsl 2 lor scanline_low_bits[i];
      let color_index = Ppu.read_vram(ppu, 0x3f00 + palette_index);
      render_pixel(color_index, i);
    };

    Ppu.ScrollInfo.next_tile(context.scroll);
  };

  let render_scanline = () => {
    if (Ppu.rendering_enabled(ppu)) {
      for (_ in 0 to 31) {
        render_tile();
      };
    };
    Ppu.ScrollInfo.next_scanline(context.scroll);
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
    context.scroll = Ppu.scroll_info(ppu);
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