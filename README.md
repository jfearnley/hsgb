# HSGB
## A Gameboy emulator written in Haskell

This is a mostly working Gameboy emulator that supports MBC1 roms. 

Compilation is via stack. To build, first ensure that libsdl2 is installed, then run
```
$ stack build
```

To use the emulator run
```
$ stack run hsgb [path to rom file]
```
Controls are

* D-pad - arrow keys
* A - X
* B - Z
* Start - S
* Select - A
* Quit - Escape

You can also press B at any time to drop into a debugger on the terminal. Press
tab to view the available commands in the debugger. The debugger can also be
run separately using
```
$ stack run debug [path to rom file]
```
