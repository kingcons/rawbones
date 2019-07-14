open Jest;
open Expect;
open Spec;

describe("NES", () => {
  let load = name => rom(name) |> Nes.load;

  describe("step_frame", () => {
    let nes = load("nestest");

    ignore(Nes.step_frame(nes, _frame => ()));
    ignore(Nes.step_frame(nes, _frame => ()));
    ignore(Nes.step_frame(nes, _frame => ()));

    let frame = Nes.step_frame(nes, _frame => ());

    // test("produces a frame", ()
    //   // TODO: actually render
    //   =>
    //     expect(frame[0]) |> toEqual(0)
    //   );

    test("has updated the nametable", () =>
      expect(Array.sub(nes.ppu.name_table, 0, 960)) |> toEqual([|
        32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,
        32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,
        32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,
        32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,
        32,  32,  42,  32,  45,  45,  32,  82, 117, 110,  32,  97, 108, 108,  32, 116, 101, 115, 116, 115,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,
        32,  32,  32,  32,  45,  45,  32,  66, 114,  97, 110,  99, 104,  32, 116, 101, 115, 116, 115,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,
        32,  32,  32,  32,  45,  45,  32,  70, 108,  97, 103,  32, 116, 101, 115, 116, 115,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,
        32,  32,  32,  32,  45,  45,  32,  73, 109, 109, 101, 100, 105,  97, 116, 101,  32, 116, 101, 115, 116, 115,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,
        32,  32,  32,  32,  45,  45,  32,  73, 109, 112, 108, 105, 101, 100,  32, 116, 101, 115, 116, 115,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,
        32,  32,  32,  32,  45,  45,  32,  83, 116,  97,  99, 107,  32, 116, 101, 115, 116, 115,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,
        32,  32,  32,  32,  45,  45,  32,  65,  99,  99, 117, 109, 117, 108,  97, 116, 111, 114,  32, 116, 101, 115, 116, 115,  32,  32,  32,  32,  32,  32,  32,  32,
        32,  32,  32,  32,  45,  45,  32,  40,  73, 110, 100, 105, 114, 101,  99, 116,  44,  88,  41,  32, 116, 101, 115, 116, 115,  32,  32,  32,  32,  32,  32,  32,
        32,  32,  32,  32,  45,  45,  32,  90, 101, 114, 111, 112,  97, 103, 101,  32, 116, 101, 115, 116, 115,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,
        32,  32,  32,  32,  45,  45,  32,  65,  98, 115, 111, 108, 117, 116, 101,  32, 116, 101, 115, 116, 115,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,
        32,  32,  32,  32,  45,  45,  32,  40,  73, 110, 100, 105, 114, 101,  99, 116,  41,  44,  89,  32, 116, 101, 115, 116, 115,  32,  32,  32,  32,  32,  32,  32,
        32,  32,  32,  32,  45,  45,  32,  65,  98, 115, 111, 108, 117, 116, 101,  44,  89,  32, 116, 101, 115, 116, 115,  32,  32,  32,  32,  32,  32,  32,  32,  32,
        32,  32,  32,  32,  45,  45,  32,  90, 101, 114, 111, 112,  97, 103, 101,  44,  88,  32, 116, 101, 115, 116, 115,  32,  32,  32,  32,  32,  32,  32,  32,  32,
        32,  32,  32,  32,  45,  45,  32,  65,  98, 115, 111, 108, 117, 116, 101,  44,  88,  32, 116, 101, 115, 116, 115,  32,  32,  32,  32,  32,  32,  32,  32,  32,
        32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,
        32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,
        32,  32,  32,  32,  85, 112,  47,  68, 111, 119, 110,  58,  32, 115, 101, 108, 101,  99, 116,  32, 116, 101, 115, 116,  32,  32,  32,  32,  32,  32,  32,  32,
        32,  32,  32,  32,  32,  32,  83, 116,  97, 114, 116,  58,  32, 114, 117, 110,  32, 116, 101, 115, 116,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,
        32,  32,  32,  32,  32,  83, 101, 108, 101,  99, 116,  58,  32,  73, 110, 118,  97, 108, 105, 100,  32, 111, 112, 115,  33,  32,  32,  32,  32,  32,  32,  32,
        32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,
        32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,
        32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,
        32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,
        32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,
        32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,
        32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32,  32
      |])
    )
  });
});