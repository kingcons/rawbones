let rom_path = Node.Process.argv[2];

let rom = File.rom(rom_path);

{
  open Pattern;

  let table = Table.load(rom.chr);

  let format = n =>
    switch (n) {
    | 1 => {js|▒|js}
    | 2 => {js|▓|js}
    | 3 => {js|█|js}
    | _ => {js|░|js}
    };
  Array.iter(tile => Js.log(Tile.inspect(tile, format)), table.tiles^);
};