# Running

```
yarn start           # watch for changes and rebuild
yarn test --watchAll # run tests
```

# NES Documentation

* [Instruction reference](https://www.masswerk.at/6502/6502_instruction_set.html)
* The [status register](https://wiki.nesdev.com/w/index.php/Status_flags)
* [.NES File Format](http://fms.komkon.org/EMUL8/NES.html#LABM)
* [Addressing modes](http://www.emulator101.com/6502-addressing-modes.html)
* [Mappers](http://tuxnes.sourceforge.net/nesmapper.txt)
* [Graphics](https://opcode-defined.quora.com/How-NES-Graphics-Work-Pattern-tables)
* [PPU Registers](https://wiki.nesdev.com/w/index.php/PPU_registers#Summary)
* [PPU Scrolling](https://wiki.nesdev.com/w/index.php/PPU_scrolling#The_common_case)

## TODO

* Nametable Mirroring: https://wiki.nesdev.com/w/index.php/Mirroring#Nametable_Mirroring
* Scrolling behavior - tiles & lines: https://wiki.nesdev.com/w/index.php/PPU_scrolling#Wrapping_around
* OAM DMA: https://wiki.nesdev.com/w/index.php/PPU_programmer_reference#OAM_DMA_.28.244014.29_.3E_write
* Sprite zero hit: https://wiki.nesdev.com/w/index.php?title=PPU_OAM&redirect=no#Sprite_zero_hits
* Sprite priority: https://wiki.nesdev.com/w/index.php/PPU_sprite_priority
