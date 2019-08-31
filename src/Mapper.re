type address = int;
type byte = int;

type t = {
  .
  get_prg: address => byte,
  set_prg: (address, byte) => unit,
  get_chr: address => byte,
  set_chr: (address, byte) => unit,
  mirroring: Rom.mirroring,
  set_mirroring: Rom.mirroring => unit,
};

exception MapperNotImplemented(Rom.mapper);
exception NotAllowed(string);

let aref = (mem, address) => Bytes.get(mem, address) |> Char.code;
let set = (mem, address, byte) => Char.chr(byte) |> Bytes.set(mem, address);

let nrom = (rom: Rom.t): t => {
  val mirroring = ref(rom.mirroring);
  pub get_prg = address => aref(rom.prg, address land (rom.prg_size - 1));
  pub set_prg = (_, _) => raise(NotAllowed("Cannot write to prg"));
  pub get_chr = address => aref(rom.chr, address);
  pub set_chr = (address, byte) => set(rom.chr, address, byte);
  pub mirroring = mirroring^;
  pub set_mirroring = style => mirroring := style
};

let unrom = (rom: Rom.t): t => {
  val mirroring = ref(rom.mirroring);
  val prg_bank = ref(0);
  pub get_prg = address => {
    let bank = address < 0xc000 ? prg_bank^ : rom.prg_count - 1;
    aref(rom.prg, bank * 0x4000 + address land 0x3fff);
  };
  pub set_prg = (_, value) => prg_bank := value land 0b111;
  pub get_chr = address => aref(rom.chr, address);
  pub set_chr = (address, byte) => set(rom.chr, address, byte);
  pub mirroring = mirroring^;
  pub set_mirroring = style => mirroring := style
};

let cnrom = (rom: Rom.t): t => {
  val mirroring = ref(rom.mirroring);
  val chr_bank = ref(0);
  pub get_prg = address => aref(rom.prg, address land (rom.prg_size - 1));
  pub set_prg = (_, value) => chr_bank := value land 0b11;
  pub get_chr = address => aref(rom.chr, this#chr_addr(address));
  pub set_chr = (address, byte) =>
    set(rom.chr, this#chr_addr(address), byte);
  pri chr_addr = offset => chr_bank^ * 0x2000 + offset;
  pub mirroring = mirroring^;
  pub set_mirroring = style => mirroring := style
};

type mmc1_prg_mode =
  | SwitchBoth
  | SwitchHigh
  | SwitchLow;
type mmc1_chr_mode =
  | Switch1
  | Switch2;

let mmc1 = (rom: Rom.t): t => {
  val mirroring = ref(rom.mirroring);
  val prg_bank = ref(0);
  val chr_bank1 = ref(0);
  val chr_bank2 = ref(0);
  val write_count = ref(0);
  val accumulator = ref(0);
  val prg_mode = ref(SwitchBoth);
  val chr_mode = ref(Switch1);
  pub get_prg = address => aref(rom.prg, address land (rom.prg_size - 1));
  pub set_prg = (address, value) => {
    let bit = value land 1;
    accumulator := accumulator^ + bit lsl write_count^;
    write_count := write_count^ + 1;

    if (write_count^ == 5) {
      this#update(address);
      write_count := 0;
      accumulator := 0;
    };
  };
  pub get_chr = address => aref(rom.chr, this#chr_addr(address));
  pub set_chr = (address, byte) =>
    set(rom.chr, this#chr_addr(address), byte);
  pri chr_addr = offset => {
    let bank = offset < 0x1000 ? chr_bank1^ : chr_bank2^;
    bank * 0x1000 + offset land 0xfff;
  };
  pri update = address => {
    // if address ...
    chr_bank1 := accumulator^;
  };
  pub mirroring = mirroring^;
  pub set_mirroring = style => mirroring := style
};

let for_rom = (rom: Rom.t): t =>
  switch (rom.mapper) {
  | NROM => nrom(rom)
  | UNROM => unrom(rom)
  | CNROM => cnrom(rom)
  | MMC1 => mmc1(rom)
  | mapper => raise(MapperNotImplemented(mapper))
  };