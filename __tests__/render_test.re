open Jest;
open Expect;
open Types;

describe("Render", () => {
  describe("scrolling", () => {
    let builder = (x, y): Render.ScrollInfo.t => {
      nt_index: 0,
      coarse_x: x,
      coarse_y: y,
      fine_x: 0,
      fine_y: 0,
    };

    test("we create ScrollInfo from registers correctly", () => {
      let address = 0b010001111001111;
      let control = 0b10000001;
      let scroll = Render.ScrollInfo.from_registers(address, control, 5);
      let result: Render.ScrollInfo.t = {
        nt_index: 0b01,
        coarse_x: 0b01111,
        coarse_y: 0b11110,
        fine_x: 5,
        fine_y: 0b010,
      };
      expect(scroll) |> toEqual(result);
    });

    test("next_tile advances the coarse_x position", () => {
      let scroll = Render.ScrollInfo.from_registers(0b0000000011110, 0, 0);
      let result: Render.ScrollInfo.t = {
        nt_index: 0b00,
        coarse_x: 0b11111,
        coarse_y: 0b00000,
        fine_x: 0,
        fine_y: 0b000,
      };
      Render.ScrollInfo.next_tile(scroll, Rom.Horizontal);
      expect(scroll) |> toEqual(result);
    });

    test("next_tile wraps to the next horizontal nametable", () => {
      let scroll = Render.ScrollInfo.from_registers(0b0000000011111, 0, 0);
      let result: Render.ScrollInfo.t = {
        nt_index: 0b01,
        coarse_x: 0b00000,
        coarse_y: 0b00000,
        fine_x: 0,
        fine_y: 0b000,
      };
      Render.ScrollInfo.next_tile(scroll, Rom.Horizontal);
      expect(scroll) |> toEqual(result);
    });

    test(
      "next_tile wraps to the next horizontal nametable based on mirroring", () => {
      let scroll = Render.ScrollInfo.from_registers(0b0000000011111, 0, 0);
      let result: Render.ScrollInfo.t = {
        nt_index: 0b10,
        coarse_x: 0b00000,
        coarse_y: 0b00000,
        fine_x: 0,
        fine_y: 0b000,
      };
      Render.ScrollInfo.next_tile(scroll, Rom.Vertical);
      expect(scroll) |> toEqual(result);
    });

    test("next_scanline bumps the fine_y position", () => {
      let scroll = Render.ScrollInfo.from_registers(0b110000000000000, 0, 0);
      let result: Render.ScrollInfo.t = {
        nt_index: 0b00,
        coarse_x: 0b00000,
        coarse_y: 0b00000,
        fine_x: 0,
        fine_y: 0b111,
      };
      Render.ScrollInfo.next_scanline(scroll, Rom.Horizontal);
      expect(scroll) |> toEqual(result);
    });

    test("next_scanline wraps fine_y to coarse_y appropriately", () => {
      let scroll = Render.ScrollInfo.from_registers(0b111000000000000, 0, 0);
      let result: Render.ScrollInfo.t = {
        nt_index: 0b00,
        coarse_x: 0b00000,
        coarse_y: 0b00001,
        fine_x: 0,
        fine_y: 0b000,
      };
      Render.ScrollInfo.next_scanline(scroll, Rom.Horizontal);
      expect(scroll) |> toEqual(result);
    });

    test("next_scanline wraps to the next vertical nametable", () => {
      let scroll = Render.ScrollInfo.from_registers(0b111001110100000, 1, 0);
      let result: Render.ScrollInfo.t = {
        nt_index: 0b11,
        coarse_x: 0b00000,
        coarse_y: 0b00000,
        fine_x: 0,
        fine_y: 0b000,
      };
      Render.ScrollInfo.next_scanline(scroll, Rom.Horizontal);
      expect(scroll) |> toEqual(result);
    });

    test("can detect top left quadrants using the scroll", () => {
      let s1 = builder(0, 0);
      let s2 = builder(1, 1);
      let q1 = Render.ScrollInfo.quad_position(0, 0);
      let q2 = Render.ScrollInfo.quad_position(1, 1);
      expect([q1, q2]) |> toEqual([TopLeft, TopLeft]);
    });

    test("can detect top right quadrants using the scroll", () => {
      let s1 = builder(2, 0);
      let s2 = builder(3, 1);
      let q1 = Render.ScrollInfo.quad_position(2, 0);
      let q2 = Render.ScrollInfo.quad_position(3, 1);
      expect([q1, q2]) |> toEqual([TopRight, TopRight]);
    });

    test("can detect bottom left quadrants using the scroll", () => {
      let s1 = builder(0, 2);
      let s2 = builder(1, 3);
      let q1 = Render.ScrollInfo.quad_position(0, 2);
      let q2 = Render.ScrollInfo.quad_position(1, 3);
      expect([q1, q2]) |> toEqual([BottomLeft, BottomLeft]);
    });

    test("can detect bottom right quadrants using the scroll", () => {
      let s1 = builder(2, 2);
      let s2 = builder(3, 3);
      let q1 = Render.ScrollInfo.quad_position(2, 2);
      let q2 = Render.ScrollInfo.quad_position(3, 3);
      expect([q1, q2]) |> toEqual([BottomRight, BottomRight]);
    });
  });

  describe("color_palette", () => {
    test("length", () =>
      expect(Array.length(Render.color_palette)) |> toEqual(192)
    );

    test("has values between 0 and 252", () => {
      let bounds =
        Array.fold_left(
          ((min, max), n) => (n < min ? n : min, n > max ? n : max),
          (1000, (-1000)),
          Render.color_palette,
        );
      expect(bounds) |> toEqual((0, 252));
    });
  });
});