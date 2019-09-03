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
  let chr_bank0 = ref(0);
  let chr_bank1 = ref(0);
  let write_count = ref(0);
  let accumulator = ref(0);
  let prg_mode = ref(SwitchBoth);
  let chr_mode = ref(Switch1);

  let chr_addr = offset => {
    let bank = offset < 0x1000 ? chr_bank0^ : chr_bank1^;
    bank * 0x1000 + offset land 0xfff;
  };
  let update = address =>
    if (address < 0xa000) {
      mirroring :=
        (
          switch (address land 0x3) {
          | 0 => Lower
          | 1 => Upper
          | 2 => Vertical
          | _ => Horizontal
          }
        );
      prg_mode :=
        (
          switch (address lsr 2 land 0x3) {
          | 2 => SwitchHigh
          | 3 => SwitchLow
          | _ => SwitchBoth
          }
        );
      chr_mode :=
        (
          switch (address lsr 4 land 0x1) {
          | 0 => Switch2
          | _ => Switch1
          }
        );
    } else if (address < 0xc000) {
      if (chr_mode^ == Switch1) {
        chr_bank0 := accumulator^;
      } else {
        chr_bank0 := accumulator^;
        chr_bank1 := accumulator^ land 1;
      };
    } else if (address < 0xe000) {
      if (chr_mode^ == Switch1) {
        chr_bank1 := accumulator^;
      };
    };

  let get_prg = address =>
    Util.aref(rom.prg, address land (rom.prg_size - 1));
  let set_prg = (address, value) => {
    let bit = value land 1;
    accumulator := accumulator^ + bit lsl write_count^;
    write_count := write_count^ + 1;

    if (write_count^ == 5) {
      update(address);
      write_count := 0;
      accumulator := 0;
    };
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