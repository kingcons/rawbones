open Types;
open Flag;

type t = cpu;

module InstructionTable =
  Map.Make({
    type t = Opcode.code;
    let compare = compare;
  });

module Lens = {
  type t('a, 'b) = ('a => 'b, ('b, 'a) => unit);

  let view = ((getter, _): t('a, 'b), a: 'a) => getter(a);

  let set = ((_, setter): t('a, 'b), b: 'b, a: 'a) => setter(b, a);

  let apply = ((getter, setter): t('a, 'b), a: 'a, f): 'b => {
    let result = f(getter(a));
    setter(result, a);
    result;
  };
};

exception AddressingModeNotImplemented(AddressingMode.t);
exception InstructionNotImplemented(string);
exception OpcodeNotFound(int);

let build = memory => {
  {
    memory,
    cycles: 0,
    x: 0,
    y: 0,
    acc: 0,
    stack: 253,
    status: Register.from_int(0b100100),
    pc: 0xfffc,
  };
};

let x = (cpu => cpu.x, (x, cpu) => cpu.x = x);
let y = (cpu => cpu.y, (y, cpu) => cpu.y = y);
let acc = (cpu => cpu.acc, (acc, cpu) => cpu.acc = acc);
let memory = address => (
  cpu => Memory.get_byte(cpu.memory, address),
  (value, cpu) => Memory.set_byte(cpu.memory, address, value),
);

let check_overflow = (result, acc, arg) => {
  let result_sign = Util.read_bit(result, 7);
  let acc_sign = Util.read_bit(acc, 7);
  let arg_sign = Util.read_bit(arg, 7);
  !(result_sign == acc_sign || result_sign == arg_sign);
};

let copy = cpu => {
  ...cpu,
  memory: Memory.copy(cpu.memory),
  status: Register.copy(cpu.status),
};

let reset = cpu => {
  cpu.pc = Memory.get_word(cpu.memory, cpu.pc);
};

let now = cpu => {
  Disassemble.make(cpu.memory, cpu.pc, 1);
};

let debug_log = cpu => {
  Printf.sprintf(
    "%04X A:%02X X:%02X Y:%02X P:%02X SP:%02X CYC:%i",
    cpu.pc,
    cpu.acc,
    cpu.x,
    cpu.y,
    Register.to_int(cpu.status),
    cpu.stack,
    cpu.cycles,
  );
};

let set_flag = (flag, value, cpu: t, _argument) => {
  Register.set(cpu.status, flag, value);
};

let set_flags_zn = (cpu: t, value: int) => {
  Register.set(cpu.status, Zero, value == 0);
  Register.set(cpu.status, Negative, Util.read_bit(value, 7));
};

let stack_pop = (cpu: t) => {
  cpu.stack = cpu.stack + 1;
  Memory.get_byte(cpu.memory, 0x100 + cpu.stack);
};

let stack_push = (cpu: t, value: int) => {
  Memory.set_byte(cpu.memory, 0x100 + cpu.stack, value);
  cpu.stack = cpu.stack - 1;
};

let add_with_carry = (cpu, argument) => {
  let carry_bit = Register.get(cpu.status, Carry) ? 1 : 0;
  let result = cpu.acc + argument + carry_bit;
  Register.set(
    cpu.status,
    Overflow,
    check_overflow(result, cpu.acc, argument),
  );
  Register.set(cpu.status, Carry, result > 0xff);
  cpu.acc = result land 0xff;

  set_flags_zn(cpu, cpu.acc);
};

let and_with_acc = (cpu, argument) => {
  cpu.acc = cpu.acc land argument;

  set_flags_zn(cpu, cpu.acc);
};

let branch_on_flag = (flag, expected, cpu, argument) =>
  if (Register.get(cpu.status, flag) == expected) {
    cpu.cycles = cpu.cycles + 1;
    cpu.pc = argument;
  } else {
    cpu.pc = cpu.pc + 1;
  };

let compare = (location, cpu, argument) => {
  let value = Lens.view(location, cpu);
  set_flags_zn(cpu, value - argument);
  Register.set(cpu.status, Carry, value >= argument);
};

let decrement = (location, cpu, _) => {
  Lens.apply(location, cpu, value => value == 0 ? 0xff : value - 1)
  |> set_flags_zn(cpu);
};

let increment = (location, cpu, _) => {
  Lens.apply(location, cpu, value => (value + 1) land 0xff)
  |> set_flags_zn(cpu);
};

let jump = (cpu, argument) => {
  cpu.pc = argument;
};

let jump_subroutine = (cpu, argument) => {
  let target = cpu.pc + 1;

  stack_push(cpu, target lsr 8);
  stack_push(cpu, target land 0xff);

  cpu.pc = argument;
};

let load = (location, cpu, argument) => {
  Lens.set(location, argument, cpu);
  set_flags_zn(cpu, argument);
};

let nop = (_cpu, _argument) => {
  ();
};

let or_with_acc = (cpu, argument) => {
  cpu.acc = cpu.acc lor argument;
  set_flags_zn(cpu, cpu.acc);
};

let pop_acc = (cpu, _argument) => {
  cpu.acc = stack_pop(cpu);

  set_flags_zn(cpu, cpu.acc);
};

let pop_status = (cpu, _argument) => {
  // See https://wiki.nesdev.com/w/index.php/Status_flags#The_B_flag
  cpu.status =
    Register.from_int(stack_pop(cpu) lor 0x20 land 0xef);
};

let push_acc = (cpu, _argument) => {
  stack_push(cpu, cpu.acc);
};

let push_status = (cpu, _argument) => {
  // See https://wiki.nesdev.com/w/index.php/Status_flags#The_B_flag
  stack_push(
    cpu,
    Register.to_int(cpu.status) lor 0x10,
  );
};

let return_from_interrupt = (cpu, _argument) => {
  cpu.status = Register.from_int(stack_pop(cpu) lor 0x20 land 0xef);
  let low = stack_pop(cpu);
  let high = stack_pop(cpu);

  cpu.pc = high lsl 8 + low;
};

let return_from_subroutine = (cpu, _argument) => {
  let low = stack_pop(cpu);
  let high = stack_pop(cpu);

  cpu.pc = high lsl 8 + low + 1;
};

let rotate_left = (location, cpu, argument) => {
  let carry_bit = Register.get(cpu.status, Carry) ? 1 : 0;
  Register.set(cpu.status, Carry, Util.read_bit(argument, 7));

  let result = argument lsl 1 lor carry_bit land 0xff;
  Lens.set(location, result, cpu);
  set_flags_zn(cpu, result);
};

let rotate_right = (location, cpu, argument) => {
  let carry_bit = Register.get(cpu.status, Carry) ? 0x80 : 0;
  Register.set(cpu.status, Carry, Util.read_bit(argument, 0));

  let result = argument lsr 1 lor carry_bit land 0xff;
  Lens.set(location, result, cpu);
  set_flags_zn(cpu, result);
};

let shift_left = (location, cpu, argument) => {
  Register.set(cpu.status, Carry, Util.read_bit(argument, 7));
  let result = argument lsl 1 land 0xff;
  Lens.set(location, result, cpu);
  set_flags_zn(cpu, result);
};

let shift_right = (location, cpu, argument) => {
  Register.set(cpu.status, Carry, Util.read_bit(argument, 0));
  let result = argument lsr 1;
  Lens.set(location, result, cpu);
  set_flags_zn(cpu, result);
};

let store = (location, cpu, argument) => {
  Memory.set_byte(cpu.memory, argument, Lens.view(location, cpu));
};

let subtract_with_borrow = (cpu, argument) => {
  let carry_bit = Register.get(cpu.status, Carry) ? 0 : 1;
  let result = cpu.acc - argument - carry_bit;
  Register.set(
    cpu.status,
    Overflow,
    check_overflow(result, cpu.acc, argument lxor 0b10000000),
  );
  Register.set(cpu.status, Carry, result >= 0);
  cpu.acc = result land 0xff;

  set_flags_zn(cpu, cpu.acc);
};

let test_bits = (cpu, argument) => {
  Register.set(cpu.status, Negative, Util.read_bit(argument, 7));
  Register.set(cpu.status, Overflow, Util.read_bit(argument, 6));
  Register.set(cpu.status, Zero, argument land cpu.acc == 0);
};

let transfer = (from, to_, cpu, _) => {
  let value = Lens.view(from, cpu);
  Lens.set(to_, value, cpu);
  set_flags_zn(cpu, value);
};

let transfer_stack_to_x = (cpu, _) => {
  cpu.x = cpu.stack;
  set_flags_zn(cpu, cpu.x);
};

let transfer_x_to_stack = (cpu, _) => {
  cpu.stack = cpu.x;
};

let xor_with_acc = (cpu, argument) => {
  cpu.acc = cpu.acc lxor argument;
  set_flags_zn(cpu, cpu.acc);
};

let step_size = (definition: Instruction.t, opcode: Opcode.t) => {
  switch (definition.access_pattern) {
  | Jump => 0
  | _ => opcode.length - 1
  };
};

let rmw_update = (opcode: Opcode.t, address): Lens.t(cpu, int) => {
  switch (opcode.addressing_mode) {
  | AddressingMode.Accumulator => acc
  | _ => memory(address)
  };
};

let maybe_update_cycle_count = (cpu: t, def: Instruction.t, start, final) => {
  switch (def.access_pattern) {
  | Instruction.Read =>
    if (start land 0xff00 != final land 0xff00) {
      cpu.cycles = cpu.cycles + 1;
    };
    final;
  | _ => final
  };
};

let handle = (definition: Instruction.t, opcode: Opcode.t, cpu) => {
  open AddressingMode;
  let result = get_address(cpu, opcode.addressing_mode);
  let address =
    switch (result) {
    | MemIndex(value) => value
    | MemRange(start, final) =>
      maybe_update_cycle_count(cpu, definition, start, final)
    };

  let operand =
    switch (definition.access_pattern, opcode.addressing_mode) {
    | (Instruction.Static, _) => 0
    | (Instruction.Write, _) => address
    | (Instruction.Jump, _) => address
    | (Instruction.ReadModifyWrite, Accumulator) => address
    | _ => Memory.get_byte(cpu.memory, address)
    };

  let operation =
    switch (definition.label) {
    | "adc" => add_with_carry
    | "and" => and_with_acc
    | "asl" => shift_left(rmw_update(opcode, address))
    | "bcc" => branch_on_flag(Carry, false)
    | "bcs" => branch_on_flag(Carry, true)
    | "beq" => branch_on_flag(Zero, true)
    | "bit" => test_bits
    | "bmi" => branch_on_flag(Negative, true)
    | "bne" => branch_on_flag(Zero, false)
    | "bpl" => branch_on_flag(Negative, false)
    | "bvc" => branch_on_flag(Overflow, false)
    | "bvs" => branch_on_flag(Overflow, true)
    | "clc" => set_flag(Carry, false)
    | "cld" => set_flag(Decimal, false)
    | "clv" => set_flag(Overflow, false)
    | "cmp" => compare(acc)
    | "cpx" => compare(x)
    | "cpy" => compare(y)
    | "dec" => decrement(rmw_update(opcode, address))
    | "dex" => decrement(x)
    | "dey" => decrement(y)
    | "eor" => xor_with_acc
    | "inc" => increment(rmw_update(opcode, address))
    | "inx" => increment(x)
    | "iny" => increment(y)
    | "jmp" => jump
    | "jsr" => jump_subroutine
    | "lda" => load(acc)
    | "ldx" => load(x)
    | "ldy" => load(y)
    | "lsr" => shift_right(rmw_update(opcode, address))
    | "nop" => nop
    | "ora" => or_with_acc
    | "pha" => push_acc
    | "php" => push_status
    | "pla" => pop_acc
    | "plp" => pop_status
    | "ror" => rotate_right(rmw_update(opcode, address))
    | "rol" => rotate_left(rmw_update(opcode, address))
    | "rti" => return_from_interrupt
    | "rts" => return_from_subroutine
    | "sbc" => subtract_with_borrow
    | "sec" => set_flag(Carry, true)
    | "sed" => set_flag(Decimal, true)
    | "sei" => set_flag(InterruptDisable, true)
    | "sta" => store(acc)
    | "stx" => store(x)
    | "sty" => store(y)
    | "tax" => transfer(acc, x)
    | "tay" => transfer(acc, y)
    | "tsx" => transfer_stack_to_x
    | "txa" => transfer(x, acc)
    | "txs" => transfer_x_to_stack
    | "tya" => transfer(y, acc)
    | _ => raise(InstructionNotImplemented(definition.label))
    };

  operation(cpu, operand);

  cpu.cycles = cpu.cycles + opcode.timing;
  cpu.pc = cpu.pc + step_size(definition, opcode);
};

let add_instruction = (definition: Instruction.t) =>
  Array.fold_right(
    opcode =>
      InstructionTable.add(
        (opcode: Opcode.t).code,
        handle(definition, opcode),
      ),
    definition.opcodes,
  );

let table: InstructionTable.t(t => unit) =
  Array.fold_right(add_instruction, Instruction.all, InstructionTable.empty);

type error =
  | AddressingModeNotImplemented(AddressingMode.t)
  | InstructionNotImplemented(string)
  | OpcodeNotFound(int);

let step = (cpu: t) => {
  let opcode = Memory.get_byte(cpu.memory, cpu.pc);
  cpu.pc = cpu.pc + 1;

  switch (InstructionTable.find(opcode, table)) {
  | command => command(cpu)
  | exception Not_found => raise(OpcodeNotFound(opcode))
  };
};

let interrupt_goto = (cpu: t, address: address) => {
  stack_push(cpu, cpu.pc lsr 8);
  stack_push(cpu, cpu.pc land 0xff);
  stack_push(cpu, Register.to_int(cpu.status));
  cpu.pc = Memory.get_word(cpu.memory, address);
};

let nmi = (cpu: t) => interrupt_goto(cpu, 0xfffa);