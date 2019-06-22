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

let render_tile = (_, _) => ();

let render_tiles = ppu => {
  for (i in 0 to 31) {
    render_tile(ppu, i);
  };
};

let draw_frame = _ => [||];

let handle_scanline =
    (
      ppu: Ppu.t,
      scanline: int,
      ~on_nmi: unit => unit,
      ~on_frame: frame => unit,
    ) => {
  if (scanline < 240) {
    render_tiles(ppu);
  } else if (scanline == 241) {
    Ppu.set_vblank(ppu.registers, true);
    if (Ppu.vblank_nmi(ppu.registers) == Ppu.NMIEnabled) {
      on_nmi();
    };
  } else if (scanline == 261) {
    Ppu.set_vblank(ppu.registers, false);
    on_frame(draw_frame(ppu));
  };

  (scanline + 1) mod scanlines_per_frame;
};