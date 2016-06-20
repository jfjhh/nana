DISK=bochs.img

all: disk.img

$(DISK): write_mbr mbr.bin boot.bin
	./write_mbr mbr.bin $(DISK)
	./write_mbr boot.bin $(DISK) bootloader

mbr.bin: mbr.asm
	nasm mbr.asm -o mbr.bin

boot.bin: boot.asm
	nasm boot.asm -o boot.bin

qemu: $(DISK)
	qemu-system-i386 -drive file=disk.img,index=0,media=disk

bochs: $(DISK) bochsrc.txt
	bochs

write_mbr: write_mbr.c

clean:
	rm *.bin write_mbr *.o; true

