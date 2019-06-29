type t = {
  cpu: Cpu.t,
  ppu: Ppu.t,
  rom: Rom.t,
  render: (~on_frame: Render.frame => unit) => unit,
};

type result('a) =
  | Continue
  | Done('a);

let load = (rom: Rom.t): t => {
  let memory = Memory.build(rom);

  let cpu = Cpu.build(memory);
  Cpu.reset(cpu);

  let ppu = memory.ppu;
  let render = Render.make(ppu, ~on_nmi=() => Cpu.nmi(cpu));

  {cpu, ppu, rom, render};
};

let step = (nes: t, ~on_frame: Render.frame => unit) => {
  Cpu.step(nes.cpu);

  if (nes.cpu.cycles >= 113) {
    nes.render(~on_frame);
    nes.cpu.cycles = nes.cpu.cycles mod 113;
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

let step_frame = (nes: t) => {
  let frame = ref(None);
  let continue = _ =>
    switch (frame^) {
    | Some(result) => Done(result)
    | None => Continue
    };

  step_cc(nes, ~continue, ~on_frame=result => frame := Some(result));
};