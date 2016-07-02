CC=gcc
CFLAGS_HOST+=-Wall -Werror -O2 -pedantic-errors -ansi
CFLAGS=$(CFLAGS_HOST)

ASM=nasm
ASMFLAGS_BIN+=-f bin
ASMFLAGS=$(ASMFLAGS_BIN)

FAT_SEC=2048
DISK=disk.img
BOOT=boot.img
VOLNAME="OSMomo Boot"

all: nana

%.bin: %.asm
	$(ASM) $(ASMFLAGS) $< -o $@

$(DISK): mbr.bin make_disk.sh write_data
	./make_disk.sh $@ $(FAT_SEC)
	./write_data mbr.bin $(DISK)

# Write all sectors to the image, and then re-do the VBR with a BPT (mformat).
# Strange dd values are for a zero'ed 1440-byte, 3.5-inch floppy.
$(BOOT): boot.bin write_data
	dd if=/dev/zero of=$@ bs=96 count=15
	./write_data $< $@ 0
	mformat -i $@ -v $(VOLNAME) -f 1440 -S 2 -H 18 -d 2 -r 512 -C -B $< ::
	dd if=$@ bs=512 count=2 2> /dev/null | xxd
	touch data/.newdisk -r $(DISK)

nana: make_disk.sh write_data | $(DISK) $(BOOT)
	@./write_data $(BOOT) $(DISK) $(FAT_SEC)
	@make -C data/ DISK=$(DISK)

clean:
	rm *.bin write_data *.o 2> /dev/null; true

diskclean: clean
	rm $(DISK) $(BOOT) 2> /dev/null; true

qemu: nana
	qemu-system-i386 -drive file=$(DISK),index=0,media=disk

bochs: nana bochsrc.txt
	bochs

.PHONY: nana qemu bochs clean diskclean

