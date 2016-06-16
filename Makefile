all: disk.img

disk.img: write_mbr mbr.bin boot.bin
	./write_mbr mbr.bin disk.img
	./write_mbr boot.bin disk.img bootloader

mbr.bin: mbr.asm
	nasm mbr.asm -o mbr.bin

boot.bin: boot.asm
	nasm boot.asm -o boot.bin

qemu: disk.img
	qemu-system-i386 -drive file=disk.img,index=0,media=disk

write_mbr: write_mbr.c

