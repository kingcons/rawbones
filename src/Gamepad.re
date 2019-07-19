type button =
  | Up
  | Down
  | Left
  | Right
  | A
  | B
  | Select
  | Start;

type t = {
  mutable up: bool,
  mutable down: bool,
  mutable left: bool,
  mutable right: bool,
  mutable a: bool,
  mutable b: bool,
  mutable select: bool,
  mutable start: bool,
  mutable strobe: button,
};

let build = () => {
  {
    up: false,
    down: false,
    left: false,
    right: false,
    a: false,
    b: false,
    select: false,
    start: false,
    strobe: A,
  };
};

let next = button =>
  switch (button) {
  | Up => Down
  | Down => Left
  | Left => Right
  | Right => A
  | A => B
  | B => Select
  | Select => Start
  | Start => Up
  };

let read = gamepad =>
  switch (gamepad.strobe) {
  | Up => gamepad.up
  | Down => gamepad.down
  | Left => gamepad.left
  | Right => gamepad.right
  | A => gamepad.a
  | B => gamepad.b
  | Select => gamepad.select
  | Start => gamepad.start
  };

let fetch = gamepad => {
  let result = read(gamepad);
  gamepad.strobe = next(gamepad.strobe);
  result ? 1 : 0;
};

let reset = gamepad => gamepad.strobe = A;