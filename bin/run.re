let rom_path = Node.Process.argv[2];

let nes = File.rom(rom_path) |> Nes.load;

let continue = _ => Nes.Continue;

let on_frame = frame => Js.log(frame);

Nes.step_cc(nes, ~continue, ~on_frame);