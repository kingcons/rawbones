open Jest;

open Expect;

describe("Mappers", () => {
  let make = (mapper, prg_count, chr_count): Mapper.t => {
    let prg_size = prg_count * 0x4000;
    let chr_size = chr_count * 0x2000;

    Memory.mapper_for_rom({
      pathname: "memrom",
      prg: Bytes.init(prg_size, n => Char.chr(n / 0x4000)),
      chr: Bytes.init(chr_size, n => Char.chr(n / 0x1000)),
      prg_size,
      chr_size,
      prg_count,
      chr_count,
      mirroring: Rom.Horizontal,
      mapper_id: (-1),
      mapper,
    });
  };

  describe("mmc1", () => {
    let init = () => make(MMC1, 4, 8);

    let write = (mapper: Mapper.t, address, value) => {
      for (i in 0 to 4) {
        mapper.set_prg(address, Util.read_bit(value, i) ? 1 : 0);
      };
    };

    test("it can read from chr", () => {
      let mapper = init();

      expect((mapper.get_chr(0), mapper.get_chr(0x1000)))
      |> toEqual((0, 0));
    });

    test("it can switch the lower chr bank", () => {
      let mapper = init();

      write(mapper, 0xa000, 5);

      expect((mapper.get_chr(0), mapper.get_chr(0x1000)))
      |> toEqual((5, 0));
    });

    test("it can switch the upper chr bank", () => {
      let mapper = init();

      write(mapper, 0xc000, 3);

      expect((mapper.get_chr(0), mapper.get_chr(0x1000)))
      |> toEqual((0, 3));
    });

    test("it can switch both chr banks", () => {
      let mapper = init();

      write(mapper, 0x8000, 0b10000);
      write(mapper, 0xa000, 2);

      expect((mapper.get_chr(0), mapper.get_chr(0x1000)))
      |> toEqual((2, 3));
    });

    test("it can switch the prg bank", () => {
      let mapper = init();

      write(mapper, 0xe000, 3);

      expect(mapper.get_prg(0)) |> toEqual(3);
    })

    test("it can switch mirroring modes", () => {
      let mapper = init();

      write(mapper, 0x8000, 0b00000);
      let lower = mapper.mirroring();
      write(mapper, 0x8000, 0b00001);
      let upper = mapper.mirroring();
      write(mapper, 0x8000, 0b00010);
      let vertical = mapper.mirroring();
      write(mapper, 0x8000, 0b00011);
      let horizontal = mapper.mirroring();

      expect((lower, upper, horizontal, vertical))
      |> toEqual((Rom.Lower, Rom.Upper, Rom.Horizontal, Rom.Vertical));
    });
  });

  describe("nrom", () => {
    let init = () => make(NROM, 1, 1);

    test("it can read from prg", () => {
      let mapper = init();
      expect(mapper.get_prg(100)) |> toEqual(0);
    });

    test("it can read from chr", () => {
      let mapper = init();
      expect(mapper.get_chr(100)) |> toEqual(0);
    });

    test("it can write to chr", () => {
      let mapper = init();
      mapper.set_chr(100, 100);
      expect(mapper.get_chr(100)) |> toEqual(100);
    });
  });

  describe("unrom", () => {
    let init = () => make(UNROM, 2, 1);

    test("it can read from prg", () => {
      let mapper = init();
      expect(mapper.get_prg(100)) |> toEqual(0);
    });

    test("it can switch prg banks", () => {
      let mapper = init();
      mapper.set_prg(0, 1);
      expect(mapper.get_prg(100)) |> toEqual(1);
    });

    test("it can read from chr", () => {
      let mapper = init();
      expect(mapper.get_chr(100)) |> toEqual(0);
    });

    test("it can write to chr", () => {
      let mapper = init();
      mapper.set_chr(100, 100);
      expect(mapper.get_chr(100)) |> toEqual(100);
    });
  });

  describe("cnrom", () => {
    let init = () => make(CNROM, 1, 3);

    test("it can read from prg", () => {
      let mapper = init();
      expect(mapper.get_prg(100)) |> toEqual(0);
    });

    test("it can read from chr", () => {
      let mapper = init();
      expect(mapper.get_chr(100)) |> toEqual(0);
    });

    test("it can write to chr", () => {
      let mapper = init();
      mapper.set_chr(100, 100);
      expect(mapper.get_chr(100)) |> toEqual(100);
    });

    test("it can switch chr banks", () => {
      let mapper = init();
      mapper.set_prg(0, 1);
      expect(mapper.get_chr(100)) |> toEqual(2);
    });

    test("it can write to switched chr banks", () => {
      let mapper = init();
      mapper.set_prg(0, 1);
      mapper.set_chr(100, 100);
      expect(mapper.get_chr(100)) |> toEqual(100);
    });
  });
});