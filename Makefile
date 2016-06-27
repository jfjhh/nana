CC=gcc
ASM=nasm
CFLAGS=-Wall -Werror -pedantic -O2 -std=c11
DISK=bochs.img

all: os

os: write_data mbr.bin boot.bin data | $(DISK)

$(DISK): make_disk.sh
	./$< $@
	touch data/.newdisk

 data: $(DISK) write_data
	@make -C data/ DISK=$(DISK)

mbr.bin: mbr.asm write_data | $(DISK)
	$(ASM) $< -o $@
	./write_data $@ $(DISK)

boot.bin: boot.asm write_data | $(DISK)
	$(ASM) $< -o $@
	./write_data $@ $(DISK) 2048

write_data: write_data.c

clean:
	rm *.bin write_data *.o 2> /dev/null; true

diskclean: clean
	rm $(DISK)

qemu: os
	qemu-system-i386 -drive file=$(DISK),index=0,media=disk

bochs: os bochsrc.txt
	bochs

.PHONY: os qemu bochs clean diskclean disk data

.PRECIOUS: $(SPLASH)

