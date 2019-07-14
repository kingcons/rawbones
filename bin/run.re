let rom_path = Node.Process.argv[2];

let nes = File.rom(rom_path) |> Nes.load;

let continue = _ => Nes.Continue;

let write: (string, Render.frame) => unit = [%bs.raw
  {|
    function(path, frame) {
      const Jimp = require('jimp');
      new Jimp({
        data: Buffer.from(frame),
        width: 256,
        height: 240,
    }, (err, image) => {
      image.write(path);
    });
   }
|}
];

let table = Pattern.Table.load(nes.rom.chr);

// for (tick in 0 to 9) {
//   Nes.step_frame(nes);
//   write({j|tmp/out.$tick.png|j}, Render.draw(nes.ppu, table));
// };