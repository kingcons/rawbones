module Tile = {
  type t = array(array(int));

  let from_bytes = (bytes: bytes, offset: int) => {
    Array.init(8, y =>
      Array.init(
        8,
        x => {
          let low = Bytes.get(bytes, 16 * offset + y) |> Char.code;
          let high = Bytes.get(bytes, 16 * offset + y + 8) |> Char.code;

          (Util.read_bit(low, 7 - x) ? 1 : 0)
          + (Util.read_bit(high, 7 - x) ? 2 : 0);
        },
      )
    );
  };

  let from_codes = (cs): t =>
    from_bytes(Bytes.init(16, i => Char.chr(cs[i])), 0);

  let inspect = (tile: t, format: int => string): string => {
    let result = ref("");

    Array.iter(
      row => {
        Array.iter(n => result := result^ ++ format(n), row);
        result := result^ ++ "\n";
      },
      tile,
    );

    result^;
  };
};

module Table = {
  type t = {
    recompute: ref(bool),
    chr_bank1: ref(int),
    chr_bank2: ref(int),
    tiles: ref(array(Tile.t))
  }

  let load = (bytes): t => {
    {
      recompute: ref(false),
      chr_bank1: ref(0),
      chr_bank2: ref(1),
      tiles: ref(Array.init(512, i => Tile.from_bytes(bytes, i)))
    }
  };

  let rebuild = (table, bytes) => {
    let offset = (i) => { i < 256 ? table.chr_bank1^ * 256 + i : table.chr_bank2^ * 256 + i };
    table.tiles := Array.init(512, i => Tile.from_bytes(bytes, offset(i)));
  };
};