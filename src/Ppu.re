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
  pattern_cache: Pattern.Table.t,
};

let build = (~name_table=Array.make(0x800, 0), mapper, chr) => {
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
  pattern_cache: Pattern.Table.load(chr),
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

let background_offset = ppu =>
  Util.read_bit(ppu.registers.control, 3) ? 0 : 256;
let sprite_offset = ppu => Util.read_bit(ppu.registers.control, 4) ? 0 : 256;

let show_background_left = mask_helper(1);
let show_sprites_left = mask_helper(2);
let show_background = mask_helper(3);
let show_sprites = mask_helper(4);

let set_sprite_zero_hit = status_helper(6);
let set_vblank = status_helper(7);

let nt_mirror = (ppu, address) => {
  let mirroring = (ppu.pattern_table)#mirroring;
  // Bit 11 indicates we're reading from nametables 3 and 4, i.e. 2800 and 2C00.
  switch (mirroring, Util.read_bit(address, 11)) {
  | (Rom.Horizontal, false) => address land 0x3ff
  | (Rom.Horizontal, true) => 0x400 + address land 0x3ff
  | (Rom.Vertical, _) => address land 0x7ff
  };
};

let nt_offset = nt_index => 0x2000 + nt_index * 0x400;

let read_vram = (ppu, address) =>
  if (address < 0x2000) {
    (ppu.pattern_table)#get_chr(address);
  } else if (address < 0x3f00) {
    let mirrored_addr = nt_mirror(ppu, address);
    ppu.name_table[mirrored_addr];
  } else {
    ppu.palette_table[address land 0x1f];
  };

let write_vram = (ppu, value) => {
  let address = ppu.registers.ppu_address;
  if (address < 0x2000) {
    (ppu.pattern_table)#set_chr(address, value);
  } else if (address < 0x3f00) {
    let mirrored_addr = nt_mirror(ppu, address);
    ppu.name_table[mirrored_addr] = value;
  } else {
    ppu.palette_table[address land 0x1f] = value;
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
  let {registers as regs} = ppu;
  if (regs.write_latch) {
    regs.buffer = registers.buffer lor value;
    regs.ppu_address = regs.buffer;
    regs.write_latch = false;
  } else {
    registers.buffer = value lsl 8 land 0x7fff;
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
};

let scroll_info = (ppu: t): ScrollInfo.t =>
  ScrollInfo.from_registers(
    ppu.registers.ppu_address,
    ppu.registers.control,
    ppu.registers.fine_x,
  );

type quad_position =
  | TopLeft
  | TopRight
  | BottomLeft
  | BottomRight;

let quadrant = (scroll: ScrollInfo.t): quad_position =>
  switch (scroll.coarse_x mod 2, scroll.coarse_y mod 2) {
  | (0, 0) => TopLeft
  | (0, 1) => TopRight
  | (1, 0) => BottomLeft
  | _ => BottomRight
  };