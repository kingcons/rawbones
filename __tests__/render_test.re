open Jest;
open Expect;

describe("Render", () =>
  describe("color_palatte", () => {
    test("length", () =>
      expect(Array.length(Render.color_palatte)) |> toEqual(192)
    );

    test("has values between 0 and 252", () => {
      let bounds =
        Array.fold_left(
          ((min, max), n) => (n < min ? n : min, n > max ? n : max),
          (1000, (-1000)),
          Render.color_palatte,
        );
      expect(bounds) |> toEqual((0, 252));
    });
  })
);