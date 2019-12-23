type prg_mode =
  | SwitchBoth
  | SwitchHigh
  | SwitchLow;

type chr_mode =
  | Switch1
  | Switch2;

let mmc1 = (rom: Rom.t): Mapper.t => {
  let mirroring = ref(rom.mirroring);
  let prg_bank = ref(0);
  let chr_bank1 = ref(0);
  let chr_bank2 = ref(0);
  let write_count = ref(0);
  let accumulator = ref(0);
  let prg_mode = ref(SwitchLow);
  let chr_mode = ref(Switch1);

  let select_mirroring = acc =>
    switch (acc land 0x3) {
    | 0 => Rom.Lower
    | 1 => Rom.Upper
    | 2 => Rom.Vertical
    | _ => Rom.Horizontal
    };

  let select_prg_mode = acc =>
    switch (acc lsr 2 land 0x3) {
    | 2 => SwitchHigh
    | 3 => SwitchLow
    | _ => SwitchBoth
    };

  let select_chr_mode = acc =>
    switch (acc lsr 4 land 0x1) {
      | 0 => Switch1
      | _ => Switch2
    }

  let update = address =>
    if (address < 0xa000) {
      mirroring := select_mirroring(accumulator^);
      prg_mode := select_prg_mode(accumulator^);
      chr_mode := select_chr_mode(accumulator^);
    } else if (address < 0xc000) {
      if (chr_mode^ == Switch1) {
        chr_bank1 := accumulator^;
      } else {
        chr_bank1 := accumulator^;
        chr_bank2 := accumulator^ lor 1;
      };
    } else if (address < 0xe000) {
      if (chr_mode^ == Switch1) {
        chr_bank2 := accumulator^;
      };
    } else {
      prg_bank := accumulator^;
    };

  let chr_addr = offset => {
    let bank = offset < 0x1000 ? chr_bank1^ : chr_bank2^;
    bank * 0x1000 + offset land 0xfff;
  };
  let prg_addr = offset => {
    let bank = offset < 0xC000 ? prg_bank^ : rom.prg_count - 1;
    bank * 0x4000 + offset land 0x3fff;
  };

  let get_prg = address => Util.aref(rom.prg, prg_addr(address));
  let set_prg = (address, value) => {
    if (Util.read_bit(value, 7)) {
      write_count := 0;
      accumulator := 0;
    } else {
      let bit = value land 1;
      accumulator := accumulator^ + bit lsl write_count^;
      write_count := write_count^ + 1;

      if (write_count^ == 5) {
        update(address);
        write_count := 0;
        accumulator := 0;
      };
    }
  };

  let get_chr = address => Util.aref(rom.chr, chr_addr(address));
  let set_chr = (address, byte) =>
    Util.set(rom.chr, chr_addr(address), byte);

  {
    get_prg,
    set_prg,
    get_chr,
    set_chr,
    mirroring: () => mirroring^,
    set_mirroring: style => mirroring := style,
  };
};