open Jest;
open Expect;
open Spec;

let nestest = rom("nestest");

describe("PPU", () => {
  let ppu = Ppu.build(Memory.mapper_for_rom(nestest));
  let regs = ppu.registers;

  describe("PPUCTRL", () => {
    let ctrl_test_helper = (n, reader, unset, set) => {
      test({j|checking CTRL $n bit unset|j}, () => {
        regs.control = 0;
        expect(reader(regs)) |> toEqual(unset);
      });

      test({j|checking CTRL $n bit set|j}, () => {
        regs.control = 1 lsl n;
        expect(reader(regs)) |> toEqual(set);
      });
    };

    ctrl_test_helper(0, Ppu.x_scroll_offset, 0, 256);
    ctrl_test_helper(1, Ppu.y_scroll_offset, 0, 240);
    ctrl_test_helper(2, Ppu.vram_step, 1, 32);
    ctrl_test_helper(3, Ppu.sprite_address, 0, 0x1000);
    ctrl_test_helper(4, Ppu.background_address, 0, 0x1000);
    ctrl_test_helper(7, Ppu.vblank_nmi, Ppu.NMIDisabled, Ppu.NMIEnabled);
  });

  describe("PPUMASK", () => {
    let mask_test_helper = (n, reader) => {
      test({j|checking MASK $n bit unset|j}, () => {
        regs.mask = 0;
        expect(reader(regs)) |> toEqual(false);
      });
      test({j|checking MASK $n bit set|j}, () => {
        regs.mask = 1 lsl n;
        expect(reader(regs)) |> toEqual(true);
      });
    };

    mask_test_helper(1, Ppu.show_background_left);
    mask_test_helper(2, Ppu.show_sprites_left);
    mask_test_helper(3, Ppu.show_background);
    mask_test_helper(4, Ppu.show_sprites);
  });

  describe("fetch", () => {
    let bad_addresses = [0x2000, 0x2001, 0x2003, 0x2005, 0x2006];

    testAll("fetching from write only registers", bad_addresses, address =>
      expect(Ppu.fetch(ppu, address)) |> toEqual(0)
    );

    test("fetching status", () => {
      regs.status = 131;
      regs.write_latch = true;
      let return_value = Ppu.fetch(ppu, 0x2002);
      // After reading PPUSTATUS, the top bit of status (vblank) is cleared.
      expect((return_value, regs.status, regs.write_latch))
      |> toEqual((131, 3, false));
    });

    test("fetching from OAMDATA returns sprite data", () => {
      ppu.oam[7] = 17;
      ppu.registers.oam_address = 7;

      let return_value = Ppu.fetch(ppu, 0x2004);
      expect(return_value) |> toEqual(17);
    });

    test("fetching from PPUDATA returns a buffered value", () => {
      regs.ppu_address = 0x2020;
      regs.ppu_data = 0;
      ppu.name_table[0x20] = 42;
      let return_value = Ppu.fetch(ppu, 0x2007);
      expect((return_value, regs.ppu_data)) |> toEqual((0, 42));
    });

    test("fetching palette info through PPUDATA is unbuffered", () => {
      regs.ppu_address = 0x3f01;
      ppu.palette_table[1] = 33;
      regs.ppu_data = 0;
      let return_value = Ppu.fetch(ppu, 0x2007);
      expect((return_value, regs.ppu_data)) |> toEqual((33, 33));
    });

    test("fetching from PPUDATA increments the PPUADDR by vram_step", () => {
      regs.ppu_address = 0x2010;
      let _ = Ppu.fetch(ppu, 0x2007);
      let small_step = regs.ppu_address;
      regs.control = 132;
      let _ = Ppu.fetch(ppu, 0x2007);
      let big_step = regs.ppu_address;
      expect((small_step, big_step)) |> toEqual((0x2011, 0x2031));
    });
  });

  describe("store", () => {
    test("storing to PPUCTRL", () => {
      let value = 0b10001010;
      Ppu.store(ppu, 0x2000, value);
      let nt_index = ppu.registers.buffer lsr 10 land 0x3;
      expect([regs.control, nt_index]) |> toEqual([value, 2]);
    });

    test("storing to PPUMASK", () => {
      Ppu.store(ppu, 0x2001, 0b01101010);
      expect(regs.mask) |> toEqual(0b01101010);
    });

    test("storing to PPUSTATUS", () => {
      regs.status = 0;
      Ppu.store(ppu, 0x2002, 0b11111111);
      expect(regs.status) |> toEqual(0);
    });

    test("storing to OAMADDR", () => {
      Ppu.store(ppu, 0x2003, 0b01011000);
      expect(regs.oam_address) |> toEqual(0b01011000);
    });

    test("storing to OAMDATA", () => {
      let oam_addr = regs.oam_address;
      Ppu.store(ppu, 0x2004, 42);
      let result = ppu.oam[oam_addr];
      expect((result, regs.oam_address)) |> toEqual((42, 89));
    });

    test("storing to PPUSCROLL", () => {
      regs.write_latch = false;
      // First byte.
      Ppu.store(ppu, 0x2005, 0b00111011);
      let latch1 = regs.write_latch;
      // Second byte.
      Ppu.store(ppu, 0x2005, 0b11000100);
      let latch2 = regs.write_latch;
      expect((latch1, latch2, regs.buffer, regs.control, regs.fine_x))
      |> toEqual((true, false, 0b100001100000111, 0b10001010, 0b011));
    });

    test("storing to PPUADDR", () => {
      regs.write_latch = false;
      Ppu.store(ppu, 0x2006, 0b11010000);
      let latch1 = regs.write_latch;
      Ppu.store(ppu, 0x2006, 0b01010101);
      let latch2 = regs.write_latch;
      let result = 0b0101000001010101;
      expect((latch1, latch2, regs.buffer, regs.ppu_address))
      |> toEqual((true, false, result, result));
    });

    test("writing to pattern table", () => {
      regs.ppu_address = 0x20;
      Ppu.store(ppu, 0x2007, 13);
      let result = Ppu.read_vram(ppu, 0x20);
      expect((result, regs.ppu_address)) |> toEqual((13, 0x21));
    });

    test("writing to nametable", () => {
      regs.ppu_address = 0x2030;
      regs.control = 4;
      Ppu.store(ppu, 0x2007, 17);
      let result = Ppu.read_vram(ppu, 0x2030);
      expect((result, regs.ppu_address)) |> toEqual((17, 0x2050));
    });

    test("writing to palette table", () => {
      regs.ppu_address = 0x3f10;
      Ppu.store(ppu, 0x2007, 19);
      expect(Ppu.read_vram(ppu, 0x3f10)) |> toEqual(19);
    });
  });

  describe("nametable mirroring", () => {
    let ppu =
      Ppu.build(
        Memory.mapper_for_rom(nestest),
        ~name_table=Array.init(0x800, i => i),
      );

    test("horizontal mirroring", () => {
      ppu.pattern_table.set_mirroring(Rom.Horizontal);
      let nt1 = Ppu.read_vram(ppu, 0x2042);
      let nt2 = Ppu.read_vram(ppu, 0x2442);
      let nt3 = Ppu.read_vram(ppu, 0x2842);
      let nt4 = Ppu.read_vram(ppu, 0x2c42);
      expect((nt1, nt2, nt3, nt4)) |> toEqual((0x42, 0x42, 0x442, 0x442));
    });

    test("vertical mirroring", () => {
      ppu.pattern_table.set_mirroring(Rom.Vertical);
      let nt1 = Ppu.read_vram(ppu, 0x2042);
      let nt2 = Ppu.read_vram(ppu, 0x2442);
      let nt3 = Ppu.read_vram(ppu, 0x2842);
      let nt4 = Ppu.read_vram(ppu, 0x2c42);
      expect((nt1, nt2, nt3, nt4)) |> toEqual((0x42, 0x442, 0x42, 0x442));
    });
  });
});