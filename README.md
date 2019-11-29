# 0asm

0asm is a 16-bit x86 assembler written in 16-bit x86 assembly which fits
in under 512 bytes of x86 machine code! It runs on
[bootOS](https://github.com/nanochess/bootOS/). 

## Assembling

You can assemble and run 0asm using nasm and QEMU (respectively):

    make run

This also copies the input file `test.asm` into the disk under `H`. One
can then run the assembler in bootOS:

    $0asm

and then run the output program under `P`:

    $P
    Hello, world! $

Further examples are available under the `examples/` directory.

## Anticipated FAQ

### What does it support?

0asm supports several common instructions, labels (absolute and relative
relocations), and octal literals (but only octal literals). It also
supports all 16-bit x86 general purpose registers, but does not support
segment registers.

Many jump instructions are supported (call, jmp, jb, jnb, jz, jnz, jbe,
jnbe), along with several string instructions (stosb, stosw, lodsb, lodsw,
movsw, cbw, scasw), several stack instructions (push, pop, ret), a few
fundamental arithmetic instructions in both register/register and
register/immediate form (add, or, adc, and, xor, cmp, mov), and some misc
instructions (int, stc, inc, dec, the pseudo-instruction db).

Errors are not really handled, but the assembler does usually exit cleanly
instead of producing garbage.

### How does it work?

0asm is a simple 2-pass assembler. The first pass collects labels into a
"symbol table", and the addresses of instructions which need to be fixed
into a "fixup table". The first pass also outputs the machine code for
instructions which do not require any relocation.

### Is it self-hosting?

Unfortunately not yet, for several reasons.

1. The underlying bootOS only supports 512 byte files.
2. The instruction encoding produced is not optimal, so it will not fit
in 512 bytes once assembled.
3. Shift instructions are not yet supported.

These problems are not insurmountable, although it seems difficult.
We could easily move the goalpost by typing the entire program using db,
but of course that would be no fun.

## Development

The code is decently commented. Many of the files in this repo are
generated from `0asm.asm`, including the README. You can also debug by
running `make debug` (to start QEMU in one pane) and `make gdbdebug`
(to start GDB along with some helpful default scripts).

## License

Copyright (c) 2019 Keyhan Vakil

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
