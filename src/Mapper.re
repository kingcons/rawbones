type address = int;
type byte = int;

type t = {
  rom: Rom.t,
  get_prg: address => byte,
  set_prg: (address, byte) => unit,
  get_chr: address => byte,
  set_chr: (address, byte) => unit,
  mirroring: unit => Rom.mirroring,
  set_mirroring: Rom.mirroring => unit,
};

exception NotImplemented(Rom.mapper);
exception NotAllowed(string);

let tile_cache = Pattern.Table.load(Bytes.make(0x2000, Char.chr(0)));

let nrom = (rom: Rom.t): t => {
  let mirroring = ref(rom.mirroring);

  {
    rom,
    get_prg: address => Util.aref(rom.prg, address land (rom.prg_size - 1)),
    set_prg: (_, _) => raise(NotAllowed("Cannot write to prg")),
    get_chr: address => Util.aref(rom.chr, address),
    set_chr: (address, byte) => {
      Util.set(rom.chr, address, byte);
      tile_cache.recompute := true;
    },
    mirroring: () => mirroring^,
    set_mirroring: style => mirroring := style,
  };
};

let unrom = (rom: Rom.t): t => {
  let mirroring = ref(rom.mirroring);
  let prg_bank = ref(0);

  let get_prg = address => {
    let bank = address < 0xc000 ? prg_bank^ : rom.prg_count - 1;
    Util.aref(rom.prg, bank * 0x4000 + address land 0x3fff);
  };
  let set_prg = (_, value) => prg_bank := value land 0b111;

  let get_chr = address => Util.aref(rom.chr, address);
  let set_chr = (address, byte) => {
    Util.set(rom.chr, address, byte);
    tile_cache.recompute := true;
  };

  {
    rom,
    get_prg,
    set_prg,
    get_chr,
    set_chr,
    mirroring: () => mirroring^,
    set_mirroring: style => mirroring := style,
  };
};

let cnrom = (rom: Rom.t): t => {
  let mirroring = ref(rom.mirroring);

  let chr_addr = offset => tile_cache.chr_bank1^ * 0x1000 + offset;

  let get_prg = address =>
    Util.aref(rom.prg, address land (rom.prg_size - 1));
  let set_prg = (_, value) => {
    let new_bank = value land 0b11 * 2;
    tile_cache.chr_bank1 := new_bank;
    tile_cache.chr_bank2 := new_bank + 1;
    tile_cache.recompute := true;
  }

  let get_chr = address => Util.aref(rom.chr, chr_addr(address));
  let set_chr = (address, byte) => {
    Util.set(rom.chr, chr_addr(address), byte);
    tile_cache.recompute := true;
  };

  {
    rom,
    get_prg,
    set_prg,
    get_chr,
    set_chr,
    mirroring: () => mirroring^,
    set_mirroring: style => mirroring := style,
  };
};