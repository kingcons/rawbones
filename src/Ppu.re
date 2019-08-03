type registers = {
  mutable control: int,
  mutable mask: int,
  mutable status: int,
  mutable oam_address: int,
  mutable oam_data: int,
  mutable ppu_address: int,
  mutable ppu_data: int,
  mutable buffer: int,
  mutable fine_x: int,
  mutable write_latch: bool,
};

type vblank_nmi =
  | NMIEnabled
  | NMIDisabled;

type t = {
  registers,
  oam: array(int),
  name_table: array(int),
  palette_table: array(int),
  pattern_table: Mapper.t,
};

let build = (~name_table=Array.make(0x800, 0), mapper) => {
  registers: {
    control: 0,
    mask: 0,
    status: 0,
    oam_address: 0,
    oam_data: 0,
    ppu_address: 0,
    ppu_data: 0,
    buffer: 0,
    fine_x: 0,
    write_latch: false,
  },
  oam: Array.make(0x100, 0),
  name_table,
  palette_table: Array.make(0x20, 0),
  pattern_table: mapper,
};

let ctrl_helper = (n, unset, set, regs) =>
  Util.read_bit(regs.control, n) ? set : unset;

let mask_helper = (n, regs) => Util.read_bit(regs.mask, n);

let status_helper = (n, regs, to_) => {
  regs.status = Util.set_bit(regs.status, n, to_);
};

let x_scroll_offset = ctrl_helper(0, 0, 256);
let y_scroll_offset = ctrl_helper(1, 0, 240);
let vram_step = ctrl_helper(2, 1, 32);
let sprite_address = ctrl_helper(3, 0, 0x1000);
let background_address = ctrl_helper(4, 0, 0x1000);
let vblank_nmi = ctrl_helper(7, NMIDisabled, NMIEnabled);

let sprite_offset = ppu => Util.read_bit(ppu.registers.control, 3) ? 256 : 0;
let background_offset = ppu =>
  Util.read_bit(ppu.registers.control, 4) ? 256 : 0;

let show_background_left = mask_helper(1);
let show_sprites_left = mask_helper(2);
let show_background = mask_helper(3);
let show_sprites = mask_helper(4);

let set_sprite_zero_hit = status_helper(6);
let set_vblank = status_helper(7);
let rendering_enabled = ppu =>
  show_background(ppu.registers) || show_sprites(ppu.registers);

let nt_offset = nt_index => 0x2000 + nt_index * 0x400;
let nt_mirror = (ppu, address) => {
  let mirroring = (ppu.pattern_table)#mirroring;
  // Bit 11 indicates we're reading from nametables 3 and 4, i.e. 2800 and 2C00.
  switch (mirroring, Util.read_bit(address, 11)) {
  | (Rom.Horizontal, false) => address land 0x3ff
  | (Rom.Horizontal, true) => 0x400 + address land 0x3ff
  | (Rom.Vertical, _) => address land 0x7ff
  };
};
let palette_mirror = addr =>
  addr > 0x3f0f && addr mod 4 == 0 ? (addr - 16) land 0x1f : addr land 0x1f;

let read_vram = (ppu, address) =>
  if (address < 0x2000) {
    (ppu.pattern_table)#get_chr(address);
  } else if (address < 0x3f00) {
    let mirrored_addr = nt_mirror(ppu, address);
    ppu.name_table[mirrored_addr];
  } else {
    let mirrored_addr = palette_mirror(address);
    ppu.palette_table[mirrored_addr];
  };

let write_vram = (ppu, value) => {
  let address = ppu.registers.ppu_address;
  if (address < 0x2000) {
    (ppu.pattern_table)#set_chr(address, value);
  } else if (address < 0x3f00) {
    let mirrored_addr = nt_mirror(ppu, address);
    ppu.name_table[mirrored_addr] = value;
  } else {
    let mirrored_addr = palette_mirror(address);
    ppu.palette_table[mirrored_addr] = value;
  };
  ppu.registers.ppu_address = address + vram_step(ppu.registers);
};

let read_status = ppu => {
  let result = ppu.registers.status;
  ppu.registers.status = result land 0x7f;
  ppu.registers.write_latch = false;
  result;
};

let read_ppu_data = ppu => {
  let {ppu_address as address, ppu_data as buffer} = ppu.registers;
  let result = read_vram(ppu, address);
  ppu.registers.ppu_data = result;
  ppu.registers.ppu_address = address + vram_step(ppu.registers);
  address < 0x3f00 ? buffer : result;
};

let fetch = (ppu: t, address) =>
  switch (address land 7) {
  | 2 => read_status(ppu)
  | 7 => read_ppu_data(ppu)
  | _ => 0
  };

let write_oam = (ppu: t, value) => {
  let {oam_address} = ppu.registers;
  ppu.oam[oam_address] = value;
  ppu.registers.oam_address = (oam_address + 1) land 0xff;
};

let write_scroll = (ppu: t, value) => {
  let regs = ppu.registers;
  if (regs.write_latch) {
    let coarse_y_bits = (value lsr 3) lsl 5;
    let fine_y_bits = (value land 7) lsl 12;
    regs.buffer = regs.buffer lor coarse_y_bits lor fine_y_bits;
    regs.write_latch = false;
  } else {
    let coarse_x_bits = value lsr 3;
    let fine_x_bits = value land 7;
    regs.buffer = coarse_x_bits;
    regs.fine_x = fine_x_bits;
    regs.write_latch = true;
  };
};

let write_address = (ppu: t, value) => {
  let regs = ppu.registers;
  if (regs.write_latch) {
    regs.buffer = regs.buffer lor value;
    regs.ppu_address = regs.buffer;
    regs.write_latch = false;
  } else {
    regs.buffer = value lsl 8 land 0x7fff;
    regs.write_latch = true;
  };
};

let store = (ppu: t, address, value) =>
  switch (address land 7) {
  | 0 => ppu.registers.control = value
  | 1 => ppu.registers.mask = value
  | 3 => ppu.registers.oam_address = value
  | 4 => write_oam(ppu, value)
  | 5 => write_scroll(ppu, value)
  | 6 => write_address(ppu, value)
  | 7 => write_vram(ppu, value)
  | _ => ()
  };