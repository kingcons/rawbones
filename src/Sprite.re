module Tile = {
  type t = {
    x_position: int, // byte 3
    y_position: int, // byte 0
    tile_index: int, // byte 1
    attributes: int, // byte 2
    zero: bool,
  };

  let make = (oam, start) => {
    {
      x_position: oam[start + 3],
      y_position: oam[start + 0],
      tile_index: oam[start + 1],
      attributes: oam[start + 2],
      zero: start == 0,
    };
  };

  let behind = sprite => Util.read_bit(sprite.attributes, 5);
  let flip_hori = sprite => Util.read_bit(sprite.attributes, 6);
  let flip_vert = attr_byte => Util.read_bit(attr_byte, 7);
  let line_bits = (sprite, tile, scanline) => {
    let row = scanline - sprite.y_position;
    let line = flip_vert(sprite.attributes) ? 7 - row : row;
    tile[line];
  };

  let on_line = (scanline, top_of_sprite) => {
    let y_distance = scanline - top_of_sprite;
    y_distance >= 0 && y_distance < 8;
  };

  let on_tile = (tile_x, sprite: t) =>
    sprite.x_position <= tile_x && tile_x < sprite.x_position + 8;
};

module Table = {
  type t = array(option(Tile.t));

  let build = (oam, scanline): t => {
    let sprites = Array.make(8, None);
    let count = ref(0);
    for (index in 0 to 63) {
      if (Tile.on_line(scanline, oam[index * 4])) {
        // TODO: Sprite Overflow.
        if (count^ < 8) {
          sprites[count^] = Some(Tile.make(oam, index * 4));
          count := count^ + 1;
        };
      };
    };
    sprites;
  };
};