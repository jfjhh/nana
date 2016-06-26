CC=gcc
CFLAGS=-Wall -Werror -pedantic -O2 -std=c11
DISK=bochs.img

all: os

os: write_mbr mbr.bin boot.bin | $(DISK)

$(DISK): make_disk.sh
	./make_disk.sh "$(DISK)"


mbr.bin: mbr.asm | $(DISK)
	nasm mbr.asm -o mbr.bin
	./write_mbr mbr.bin $(DISK)

boot.bin: boot.asm | $(DISK)
	nasm boot.asm -o boot.bin
	./write_mbr boot.bin $(DISK) bootloader


write_mbr: write_mbr.c

clean:
	rm *.bin write_mbr *.o; true

diskclean: clean
	rm $(DISK)

qemu: os
	qemu-system-i386 -drive file=$(DISK),index=0,media=disk

bochs: os bochsrc.txt
	bochs

.PHONY: os qemu bochs clean diskclean disk

