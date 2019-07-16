type address = int;
type byte = int;

type t = {
  ram: bytes,
  mapper: Mapper.t,
  ppu: Ppu.t,
};

let build = (rom: Rom.t): t => {
  let ram = Bytes.make(0x800, Char.chr(0));
  let mapper = Mapper.for_rom(rom);
  let ppu = Ppu.build(mapper);

  {ram, mapper, ppu};
};

let ppu = memory => memory.ppu;

let copy = (memory: t): t => {
  {...memory, ram: Bytes.copy(memory.ram)};
};

let get_byte = (mem: t, loc: address): int =>
  if (loc < 0x2000) {
    Char.code(Bytes.get(mem.ram, loc land 0x7ff));
  } else if (loc < 0x4000) {
    Ppu.fetch(mem.ppu, loc);
  } else if (loc < 0x8000) {
    0;
    // TODO: APU, I/O and such
  } else {
    (mem.mapper)#get_prg(loc);
  };

let set_byte = (mem: t, loc: address, value: byte) =>
  if (loc < 0x2000) {
    Bytes.set(mem.ram, loc, Char.chr(value));
  } else if (loc < 0x4000) {
    Ppu.store(mem.ppu, loc, value);
  } else if (loc < 0x8000) {
    ();
  } else {
    (mem.mapper)#set_prg(loc, value);
  };

let get_word = (mem: t, loc: address) => {
  let low = get_byte(mem, loc);
  let high = get_byte(mem, loc + 1);
  high lsl 8 + low;
};

let get_indirect = (mem: t, loc: address) => {
  // If an indirect fetch would wrap to the next page,
  // it instead wraps to the beginning of the current page.
  let wrapped = loc land 0xff00 + (loc + 1) land 0xff;
  let low = get_byte(mem, loc);
  let high = get_byte(mem, wrapped);
  high lsl 8 + low;
};