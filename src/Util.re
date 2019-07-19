[@bs.val] external __dirname: string = "";

let default = (def, mval) => {
  switch (mval) {
  | Some(a) => a
  | None => def
  };
};

let ljust = (width: int, str: string) =>
  if (width > String.length(str)) {
    str ++ String.make(width - String.length(str), ' ');
  } else {
    String.sub(str, 0, width);
  };

let show_hex = (string: string) => {
  Array.init(String.length(string), index =>
    Printf.sprintf("%X", Char.code(string.[index]))
  )
  |> Js.Array.joinWith(" ");
};

let expand_path = (path: string) => {
  Node.Path.resolve(__dirname, "../" ++ path);
};

let set_bit = (value: int, index: int, to_: bool) => {
  let mask = 1 lsl index;
  if (to_) {
    value lor mask;
  } else {
    value land lnot(mask);
  };
};

let read_bit = (value: int, index: int): bool => {
  let mask = 1 lsl index;
  value land mask == mask;
};
