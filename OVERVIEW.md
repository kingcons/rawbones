# Rawbones: A Technical Guide

- [Narrative](#narrative)
  * [Building a NES](#building-a-nes)
  * [Running an Instruction](#running-an-instruction)
- [Architecture](#architecture)
- [Testing](#testing)
- [Performance](#performance)

## Narrative

As Peter Seibel wrote, [code is not literature][cinl]. However, I can give a rough idea of how execution progresses which may suggest modules or functions of interest.

### Building a NES

Initially, we need to load a program or ROM for the Nintendo to run. These ROMs are in a *.NES binary file format (spec linked in the README) consisting of a 16 byte header and then the program code. The `Rom.re` module defines a struct `t` and a helper function `parse` for processing these files into useful data.

Once we have a parsed Rom, we can build a memory interface with it, defined in `Memory.re`. We need the Rom first since the cartridge contains graphics data which will be consumed by the graphics card. The NES did not have an operating system and so anytime the developer wanted to interact with the graphics card, controllers, etc, they would read or write specific addresses in memory which mapped to those components. Our Memory object will serve as the link between the CPU and other hardware and this can be seen in the `get_byte` and `set_byte` functions in the Memory module.

We can build an object representing the full Nintendo from the CPU and Memory and this work is done in the `load` function in `Nes.re`. This function produces a completely loaded Nintendo from a ROM path and the module provides helpers to:
  * Step the CPU forward one instruction - `step`
  * Step the CPU forward one frame - `step_frame`

There is also a disassembler since it is often helpful to be able to view the assembly code being executed. It lives in `Disassemble.re` and can be built by passing an instance of `Memory.t` to `Disassemble.make`. This will return a closure with access to memory, which can be called by passing a start address and a number of bytes to process.

### Running an Instruction

Writing an emulator isn't very interesting if you can't run instructions! Once there is a built `Nes.t`, we can use the `Nes.step` function to run the next instruction. Instructions are processed as follows:

  1. We check the CPU's program counter to see where it points to in memory. That's the address of the next instruction so we read it from memory. Afterwards, we advance the program counter so it is pointing at either arguments to the instruction kept adjacent in memory _or_ the following instruction. (see: `cpu.re`, lines 418-419)

  2. We use an `InstructionTable` to look up the instruction. The InstructionTable maps bytes to functions that implement the instruction. Instructions are just single bytes and not all 256 bytes correspond to a valid command. If the instruction is invalid, we throw an `OpcodeNotFound` exception here. Otherwise, we call the found function passing it the CPU. (see: `cpu.re`, lines 421-424)
    * Note: The InstructionTable is built from a JSON encoding of the instruction set for the 6502, in our source tree at `src/instructions.json`.

  3. All functions in the InstructionTable are closures over the opcode byte and instruction metadata that take a CPU and then run `handle`. You can see how we add these closures to the InstructionTable in `cpu.re`, lines 402-405. At a high level,
  `handle` does the following:

    * Compute the addresses in memory that the instruction uses. (lines 316-323)
    * Fetch any operand or data from memory the instruction needs to process. (lines 325-332)
    * Uses a large switch statement to locate and run the appropriate function. (lines 334-393)
    * Updates the CPU cycle count to reflect the time passed and moves the program counter to point to the next instruction. (lines 395-396)

  4. If the NES executed a special write to memory, a DMA takes place in which normal CPU activity is suspended and a page of sprite data is copied to the graphics card VRAM. (`nes.re`, lines 55-57)

  5. If the CPU has run for more than the number of cycles it takes the video card to render a "scanline" on the screen, we need to render that scanline to synchronize them. (`nes.re`, lines 59-62)

Whew! And that's how a CPU instruction is processed. We read bytes from memory, look up the matching function to run, and then execute that function and update some bookkeeping information. Much like a language interpreter, just for assembly. :)

## Architecture

## Testing

## Performance

[cinl]: http://www.gigamonkeys.com/code-reading/