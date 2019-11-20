.PHONY: all
all: 0asm.bin os.img hash README.md

0asm.o 0asm.lst: 0asm.asm
	nasm -f elf32 -g3 -F dwarf 0asm.asm -l 0asm.lst -o 0asm.o

0asm.lst: 0asm.o

0asm.elf: 0asm.o
	ld -Ttext=0x7c00 -melf_i386 $^ -o $@

0asm.bin: 0asm.elf
	objcopy -O binary $^ $@

os.img: osbase.img 0asm.bin test.asm
	cp osbase.img os.img
	./copy-to-sector.sh test.asm  os.img 1
	./copy-to-sector.sh /dev/zero os.img 2
	./copy-to-sector.sh 0asm.bin  os.img 3

README.md: 0asm.asm
	sed -n 's/;;; \?\(.*\)/\1/p' $^ > $@

hash: hash.c

.PHONY: run
run: os.img
	qemu-system-i386 \
	    -drive file=os.img,format=raw,index=0,if=floppy \
	    -curses

.PHONY: debug
debug: os.img
	qemu-system-i386 \
	    -S -s \
	    -drive file=os.img,format=raw,index=0,if=floppy \
	    -curses

.PHONY: gdbdebug
gdbdebug: os.img
	gdb -x gdb-real-mode 0asm.elf

.PHONY: clean
clean:
	rm -f os.img 0asm.lst 0asm.elf 0asm.o 0asm.bin hash README.md
