open Jest;

open Expect;

describe("Mappers", () => {
  let make = (mapper, prg_count, chr_count): Mapper.t => {
    let prg_size = prg_count * 0x4000;
    let chr_size = chr_count * 0x2000;

    Mapper.for_rom({
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
    let init = () => make(MMC1, 2, 8);

    let write = (mapper, address, value) => {
      for (i in 0 to 4) {
        mapper#set_prg(address, Util.read_bit(value, i) ? 1 : 0);
      };
    };

    test("it can read from chr", () => {
      let mapper = init();

      expect(mapper#get_chr(0)) |> toEqual(0);
    });

    test("it can switch chr banks", () => {
      let mapper = init();

      write(mapper, 0xa001, 5);

      expect(mapper#get_chr(0)) |> toEqual(5);
    });
  });

  // describe("nrom", () => {
  //   let init = () => make(NROM, 1, 1);
  //   test("it can read from prg", () => {
  //     let mapper = init();
  //     expect(mapper#get_prg(100)) |> toEqual(0);
  //   });
  //   test("it can read from chr", () => {
  //     let mapper = init();
  //     expect(mapper#get_chr(100)) |> toEqual(0);
  //   });
  //   test("it can write to chr", () => {
  //     let mapper = init();
  //     mapper#set_chr(100, 100);
  //     expect(mapper#get_chr(100)) |> toEqual(100);
  //   });
  // });
  // describe("unrom", () => {
  //   let init = () => make(UNROM, 2, 1);
  //   test("it can read from prg", () => {
  //     let mapper = init();
  //     expect(mapper#get_prg(100)) |> toEqual(0);
  //   });
  //   test("it can switch prg banks", () => {
  //     let mapper = init();
  //     mapper#set_prg(0, 1);
  //     expect(mapper#get_prg(100)) |> toEqual(1);
  //   });
  //   test("it can read from chr", () => {
  //     let mapper = init();
  //     expect(mapper#get_chr(100)) |> toEqual(0);
  //   });
  //   test("it can write to chr", () => {
  //     let mapper = init();
  //     mapper#set_chr(100, 100);
  //     expect(mapper#get_chr(100)) |> toEqual(100);
  //   });
  // });
  // describe("cnrom", () => {
  //   let init = () => make(CNROM, 1, 3);
  //   test("it can read from prg", () => {
  //     let mapper = init();
  //     expect(mapper#get_prg(100)) |> toEqual(0);
  //   });
  //   test("it can read from chr", () => {
  //     let mapper = init();
  //     expect(mapper#get_chr(100)) |> toEqual(0);
  //   });
  //   test("it can write to chr", () => {
  //     let mapper = init();
  //     mapper#set_chr(100, 100);
  //     expect(mapper#get_chr(100)) |> toEqual(100);
  //   });
  //   test("it can switch chr banks", () => {
  //     let mapper = init();
  //     mapper#set_prg(0, 1);
  //     expect(mapper#get_chr(100)) |> toEqual(1);
  //   });
  //   test("it can write to switched chr banks", () => {
  //     let mapper = init();
  //     mapper#set_prg(0, 1);
  //     mapper#set_chr(100, 100);
  //     expect(mapper#get_chr(100)) |> toEqual(100);
  //   });
  // });
});