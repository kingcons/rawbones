Change types:
* New Feature
* Enhancement
* Optimization
* Bugfix

## Changes for 0.3.2 (08/07/19):

* Enhancement: Reimplement the sprite renderer, cutting runtime in half due to reduced allocation and other factors.
* Bugfix: Writes to the PPU ctrl register now update the nametable in the buffer register appropriately.

## Changes for 0.3.1 (08/04/19):

* Bugfix: A last minute change broke the buffering of reads from PPUDATA.

## Changes for 0.3.0 (08/04/19):

* New Feature: The renderer now supports sprite rendering! 🔥🔥🔥
* Enhancement: Nes.t now has a field for accessing the gamepad.
* Bugfix: The quadrant of the attribute byte is now detected correctly based on the scroll position.
* Bugfix: Background palette high bits were pulled from the attribute byte incorrectly.
* Bugfix: Scrolling switches nametables correctly based on the ROM mirroring.
* Bugfix: The renderer mirrors palette reads to the backdrop correctly.

## Changes for 0.2.2 (07/24/19):

* Enhancement: Nes.t now has a mutable field for storing the framebuffer.
* Bugfix: The bsconfig expected our unpackaged bin files to be present, breaking the build.

## Changes for 0.2.0 (07/19/19):

* New feature: Add PPU with support for background rendering, nametable mirroring, and scrolling. 🎉✨🎉
* Enhancement: Memory interface supports DMA and reading player 1 gamepad.
* Enhancement: Support UNROM.
* Enhancement: Support CNROM.
* Enhancement: Support CHR-RAM carts.

## Changes for 0.1.5 (06/11/19):

* Initial Release passing CPU tests.

