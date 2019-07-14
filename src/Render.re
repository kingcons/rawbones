type frame = array(int);

let width = 256;
let height = 240;

let cycles_per_scanline = 341;
let scanlines_per_frame = 262;

let color_palatte_data = {j|
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

let color_palatte =
  String.trim(color_palatte_data)
  |> Js.String.splitByRe([%bs.re "/\\s+/g"])
  |> Array.map(str => int_of_string("0x" ++ Util.default("00", str)));

type context = {
  mutable scanline: int,
  mutable frame,
  mutable scroll: Ppu.ScrollInfo.t,
};

let draw = (ppu: Ppu.t, tiles: Pattern.Table.t): frame => {
  let frame = Array.make(width * height * 4, 0);
  let offset = ref(0);

  for (x in 0 to 31) {
    for (y in 0 to 29) {
      let tile = tiles[x];

      for (i in 0 to 7) {
        for (j in 0 to 7) {
          let tileValue = tile[i][j];

          frame[offset^ + 3] = 255;

          switch (tileValue) {
          | 3 => frame[offset^] = 255
          | 2 => frame[offset^ + 1] = 255
          | 1 => frame[offset^ + 2] = 255
          | _ => ()
          };

          offset := offset^ + 4;
        };
      };
    };
  };

  frame;
};

let make = (ppu: Ppu.t, ~on_nmi: unit => unit) => {
  let context = {
    scanline: 0,
    frame: Array.make(width * height * 3, 0),
    scroll: Ppu.scroll_info(ppu),
  };

  let render_tile = () => {
    let x = context.scroll.coarse_x;
    let y = context.scroll.coarse_y;
    let nt_offset = Ppu.nt_offset(context.scroll.nt_index);
    let at_offset = nt_offset + 0x3c0;
    let nt = Ppu.read_vram(ppu, nt_offset + 32 * y + x);
    let at = Ppu.read_vram(ppu, at_offset + y / 4 << 3, x / 4);
    // The nametable byte should now points to the starting offset
    // of a 16 byte Tile in the Pattern Table. The attribute byte
    // has the two high bits of the palette index of the tile in
    // question. To determine which two high bits, we need a helper
    // along the lines of `color-quadrant` from clones.
    Ppu.ScrollInfo.next_tile(context.scroll);
  };

  let render_scanline = () => {
    for (_ in 0 to 31) {
      render_tile();
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