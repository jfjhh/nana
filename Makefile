IMGDIR       := out
DATADIR      := data
SRCDIR       := src
BINDIR       := bin
EXEDIR       := exe
DEPDIR       := .deps
INCDIRS      := include
MODULES      := $(SRCDIR) $(DATADIR)
ASMSOURCES   :=
CSOURCES     :=
EXECUTABLES  :=
BINARIES     :=
DATALIST     :=
FAT_SEC      := 2048
DISK         := $(IMGDIR)/disk.img
BOOT         := $(IMGDIR)/boot.img
VOLNAME      := "OSMomo Boot"
UPDATE_DATA  :=
WRITE_DATA   := $(EXEDIR)/write_data
CC            = gcc
CFLAGS_HOST  += -std=c11
CFLAGS       += -Wall -Werror -O2 -pedantic-errors
CFLAGS       += $(patsubst %,-I%/,$(MODULES))
CFLAGS       += $(patsubst %,-I%/,$(INCDIRS))
ASM           = nasm
ASMFLAGS_BIN += -f bin
ASMFLAGS     += -w+all
ASMFLAGS     += $(patsubst %,-I%/,$(MODULES))
ASMFLAGS     += $(patsubst %,-I%/,$(INCDIRS))

include $(patsubst %,%/module.mk,$(MODULES))

all: make_disk.sh $(WRITE_DATA) $(UPDATE_DATA) | $(DISK) $(BOOT)
	$(WRITE_DATA) $(BOOT) $(DISK) $(FAT_SEC)
	$(UPDATE_DATA) $(DATALIST) $(WRITE_DATA) $(DISK)

include $(patsubst %.asm,$(DEPDIR)/%.deps,$(ASMSOURCES))
include $(patsubst %.c,$(DEPDIR)/%.deps,$(CSOURCES))

$(DEPDIR)/%.deps: %.asm
	set -e; rm -f $@; \
		$(ASM) -M -MQ $*.bin $(ASMFLAGS) $< > $@.$$$$; \
		sed 's,\($*\)\.bin[ :]*,\1.bin $@ : ,g' < $@.$$$$ > $@; \
		rm -f $@.$$$$

$(DEPDIR)/%.deps: %.c
	set -e; rm -f $@; \
		$(CC) -MM $(CFLAGS) $< > $@.$$$$; \
		sed 's,\($*\)\.o[ :]*,\1.o $@ : ,g' < $@.$$$$ > $@; \
		rm -f $@.$$$$

$(BINDIR)/%.bin: $(SRCDIR)/%.asm
	$(ASM) $(ASMFLAGS) $(ASMFLAGS_BIN) $< -o $@

$(EXEDIR)/%: $(SRCDIR)/%.c
	$(CC) $(CFLAGS) $(CFLAGS_HOST) $< -o $@

$(DISK): $(BINDIR)/mbr.bin make_disk.sh $(WRITE_DATA)
	./make_disk.sh $@ $(FAT_SEC)
	$(WRITE_DATA) $< $(DISK)

# Write all sectors to the image, and then re-do the VBR with a BPT (mformat).
# Strange dd values are for a zero'ed 1440-byte, 3.5-inch floppy.
$(BOOT): $(BINDIR)/vbr.bin $(BINDIR)/bootloader.bin $(WRITE_DATA)
	dd if=/dev/zero of=$@ bs=96 count=15
	$(WRITE_DATA) $< $@ 0
	mformat -i $@ -v $(VOLNAME) -f 1440 -S 2 -H 18 -d 2 -r 512 -C -B $< ::
	mcopy -i $@ $(BINDIR)/bootloader.bin ::/BOOTMOMO.BIN
	mdir -i $@
	touch $(DATADIR)/.newdisk -r $(DISK)

clean:
	-rm -fv $(BINDIR)/* $(EXEDIR)/*

diskclean: clean
	-rm -fv $(IMGDIR)/*

distclean: diskclean
	-rm -rfv $(DEPDIR) $(BINDIR) $(EXEDIR) $(IMGDIR)

qemu: all
	qemu-system-i386 -drive file=$(DISK),index=0,media=disk

bochs: all bochsrc.txt
	bochs

.PHONY: all qemu bochs clean diskclean

