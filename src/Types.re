type address = int;
type byte = int;

type cpu = {
  memory: Memory.t,
  mutable status: Flag.Register.t,
  mutable cycles: int,
  mutable x: byte,
  mutable y: byte,
  mutable acc: byte,
  mutable stack: byte,
  mutable pc: int,
};