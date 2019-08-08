## About

Rawbones is an early-stage NES emulator written in ReasonML and compiled to JS.
It currently powers [epiderNES](https://kingcons.io/epiderNES), [source here][epi].

It is inspired by long standing beliefs about the power of computers for kinesthetic learning. See: [Research Goals][goals].

There is a guide to rawbones intended for developers exploring the project. It can be found [here][docs]. Feedback and pull requests are welcome!

[docs]: https://github.com/kingcons/rawbones/blob/master/OVERVIEW.md
[epi]: https://github.com/jamesdabbs/epiderNES
[goals]: https://blog.kingcons.io/posts/Research-Goals.html

## Why another emulator?

<blockquote class="twitter-tweet" data-lang="en"><p lang="en" dir="ltr">Reading things teaches people how to write. Analogous, if we are to place programming at the same fundamental level, using a program should teach how it works. But we don&#39;t see this.</p>&mdash; Tony Garnock-Jones (@leastfixedpoint) <a href="https://twitter.com/leastfixedpoint/status/1026567416229314561?ref_src=twsrc%5Etfw">August 6, 2018</a></blockquote>

Many NES emulators already exist, both on the web and the desktop. Our goal is to create
a readable, tested, and compact code base sufficient for mostly accurate emulation of many but not all popular Nintendo titles.

Rawbones exists to support curious programmers learning about how the system and its titles worked rather than purely being a vehicle for reliving childhood gaming experiences. To be effective though, it must be able to run the games first. The codebase strives to be accessible for learning about emulation, sacrificing total accuracy and performance. The real product is not the code but epiderNES.

Now that rawbones is reaching playable status, our focus will increasingly shift towards building out debugging and reverse engineering tools in epiderNES. In addition to the usual tools to disassemble memory, inspect machine registers, or view VRAM, we hope to add support for tracing the game and building a directed graph of blocks and jumps as it runs.

Afterwards, we'll provide tools for the user to annotate the graph with notes about the code. This will move us towards our overall goal of making the high-level structure of programs "discoverable" through using them, calling back to the Tony Garnock-Jones quote above.

## Compatibility

The CPU and Input handling is complete and well tested.
The PPU (graphics support) is mostly complete and tested but some scrolling bugs remain.

Scrolling bugs are our first priority. In the near future, we hope to implement support for additional cartridge types (mappers) and APU support (audio). NROM, UNROM, and CNROM are currently supported.

* NROM -> Arkanoid, Donkey Kong, Excite Bike, Super Mario Bros
* UNROM -> Contra, Metal Gear, Prince of Persia
* CNROM -> Paperboy, Q-Bert

To be implemented:

* MMC1 -> Mega Man 2, Legend of Zelda
* MMC3 -> Super Mario Bros 3, etc

## Installation

We assume a recent version of Node is installed.
This software is tested on node v10.12.0 and v12.8.0 on Mac OS High Sierra.
It should run equivalently on a Linux system or newer versions of Mac OS.

Rawbones is currently available on NPM though doesn't currently export a JS API.
[ReasonML](https://reasonml.github.io/) programs are the likely consumers.

We would be enthusiastic about receiving patches that document a formal API
(along with any needed helper functions) to make using rawbones convenient from JS.

`yarn` fetches all dependencies and `yarn build` will produce JS in the src directory.

## Usage

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
* [Sprite handling](https://wiki.nesdev.com/w/index.php?title=PPU_OAM)

## TODO

* [APU](https://wiki.nesdev.com/w/index.php/APU)
