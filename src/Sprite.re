module Tile = {
  type t = {
    x_position: int, // byte 3
    y_position: int, // byte 0
    tile_index: int, // byte 1
    attributes: int // byte 2
  };

  let make = (oam, start) => {
    {
      x_position: oam[start + 3],
      y_position: oam[start + 0],
      tile_index: oam[start + 1],
      attributes: oam[start + 2],
    };
  };
};

module Table = {
  type t = array(option(Tile.t));

  let build = (oam, scanline): t => {
    let sprites = Array.make(8, None);
    let count = ref(0);
    // Go through all 64 sprites.
    for (index in 0 to 63) {
      let top_of_sprite = oam[index * 4];
      // Compute the distance between top of sprite and current line.
      let y_distance = scanline - top_of_sprite;
      // Only 8 sprites can be displayed per scanline.
      // If it's non-negative and less than 8, it's visible.
      if (y_distance >= 0 && y_distance < 8 && count^ < 8) {
        // Add the sprite to the current scanline and bump the count.
        sprites[count^] = Some(Tile.make(oam, index * 4));
        count := count^ + 1;
      };
    };
    sprites;
  };
};