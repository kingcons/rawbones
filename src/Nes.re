type t = {
  cpu: Cpu.t,
  ppu: Ppu.t,
  rom: Rom.t,
  gamepad: Gamepad.t,
  render: Render.Context.t,
  mutable frame: Render.frame,
};

type result('a) =
  | Continue
  | Done('a);

let load = (rom: Rom.t): t => {
  let memory = Memory.build(rom);
  let gamepad = memory.gamepad;
  let cpu = Cpu.build(memory);
  Cpu.reset(cpu);

  let ppu = memory.ppu;
  let render = Render.Context.make(ppu, rom, ~on_nmi=() => Cpu.nmi(cpu));

  {cpu, ppu, rom, gamepad, render, frame: [||]};
};

/*
   See: https://wiki.nesdev.com/w/index.php/PPU_programmer_reference#OAM_DMA_.28.244014.29_.3E_write

   DMA is a very mutable and rather strange operation. Technically, the CPU suspends operation while a page
   of memory is copied from its address space to the PPU's OAM. This process takes ~513 CPU cycles. The PPU
   is running concurrently but all docs I've found indicate that DMA is only safe to use during vblank.
   At any other time, you'd be modifying sprite memory _while_ the PPU is attempting to draw the frame.

   The PPU should render a scanline every ~113 CPU cycles. Thus, in 513 cycles it renders 4 scanlines with
   61 cycles left over. The CPU cannot modify any system state or perform I/O with any hardware during DMA.
   Therefore, it should be safe to model DMA at the system level as an event that:
     1. Copies the page of data specfied to the PPU's memory.
     2. Renders 4 scanlines, none of which should fall outside vblank.
     3. Bumps the CPU cycle count by 61 cycles and continues as normal.
   Note that this depends on our clock sync being "scanline-accurate" rather than "cycle-accurate".
 */

let dma = (nes, ~on_frame) => {
  Render.sync(nes.render, ~on_frame);
  Render.sync(nes.render, ~on_frame);
  Render.sync(nes.render, ~on_frame);
  Render.sync(nes.render, ~on_frame);
  nes.cpu.cycles = nes.cpu.cycles + 61;
  nes.cpu.memory.dma = false;
};

let step = (~on_frame=_ => (), nes: t) => {
  Cpu.step(nes.cpu);

  if (nes.cpu.memory.dma) {
    dma(nes, ~on_frame);
  };

  if (nes.cpu.cycles >= Render.cycles_per_scanline) {
    Render.sync(nes.render, ~on_frame);
    nes.cpu.cycles = nes.cpu.cycles mod Render.cycles_per_scanline;
  };
};

let step_cc =
    (nes: t, ~continue: unit => result('a), ~on_frame: Render.frame => unit) => {
  let rec go = () => {
    step(nes, ~on_frame);
    switch (continue()) {
    | Continue => go()
    | Done(result) => result
    };
  };

  go();
};

let step_frame = (~on_frame=_ => (), nes: t) => {
  let frame = ref(None);
  let continue = _ =>
    switch (frame^) {
    | Some(result) =>
      on_frame(result);
      Done(result);
    | None => Continue
    };

  step_cc(nes, ~continue, ~on_frame=result => frame := Some(result));
};