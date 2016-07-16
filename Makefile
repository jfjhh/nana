BUILDDIR     := $(CURDIR)
IMGDIR       := $(BUILDDIR)/out
DATADIR      := data
SRCDIR       := src
BINDIR       := $(BUILDDIR)/bin
EXEDIR       := $(BUILDDIR)/exe
OBJDIR       := $(BUILDDIR)/obj
INCDIRS      := include
TARGDIRS     := $(IMGDIR) $(BINDIR) $(EXEDIR) $(OBJDIR)
MODULES      := $(SRCDIR) $(DATADIR)
ASMSOURCES   :=
CSOURCES     :=
DATALIST     :=
FAT_SEC      := 2048
DISK         := $(IMGDIR)/disk.img
BOOT         := $(IMGDIR)/boot.img
VOLNAME      := "OSMomo Boot"
UPDATE_DATA  :=
WRITE_DATA   := $(EXEDIR)/write_data
MAKE_DISK    := ./make_disk.sh
LD            = ld
LDFLAGS_HOST :=
LDFLAGS      :=
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
VPATH        += $(SRCDIR)

include $(patsubst %,%/module.mk,$(MODULES))

all: $(TARGDIRS) $(WRITE_DATA) $(UPDATE_DATA) | $(DISK) $(BOOT)
	set -e; \
		$(WRITE_DATA) $(BOOT) $(DISK) $(FAT_SEC); \
		$(UPDATE_DATA) $(DATALIST) $(WRITE_DATA) $(DISK); \
		printf "\033[0;1m"; \
		printf "*** Disk image '$(DISK)' is up-to-date. ***\n"; \
		printf "*** Use make {bochs,qemu} to emulate '$(DISK)'. ***\n"; \
		printf "\033[0m";

$(TARGDIRS):
	for DIR in $@; do \
		[ -d $$DIR ] || mkdir -p $$DIR; \
		done;

include $(patsubst %.asm,.%.deps,$(ASMSOURCES))
include $(patsubst %.c,.%.deps,$(CSOURCES))

.%.deps: %.asm
	set -e; rm -f $@; \
		$(ASM) -M -MQ $*.bin $(ASMFLAGS) $< > $@.$$$$; \
		sed 's,\($*\)\.bin[ :]*,\1.bin $@ : ,g' < $@.$$$$ > $@; \
		rm -f $@.$$$$

.%.deps: %.c
	set -e; rm -f $@; \
		$(CC) -MM $(CFLAGS) $(CFLAGS_HOST) $< > $@.$$$$; \
		sed 's,\($*\)\.o[ :]*,\1.o $@ : ,g' < $@.$$$$ > $@; \
		rm -f $@.$$$$

$(BINDIR)/%.bin: %.asm
	$(ASM) $(ASMFLAGS) $(ASMFLAGS_BIN) $< -o $@

$(OBJDIR)/%.o: %.c
	$(CC) $(CFLAGS) $(CFLAGS_HOST) -c $< -o $@

$(EXEDIR)/%: $(OBJDIR)/%.o
	$(CC) $(CFLAGS) $(CFLAGS_HOST) $< -o $@

$(DISK): $(BINDIR)/mbr.bin $(MAKE_DISK) $(WRITE_DATA)
	set -e; \
		$(MAKE_DISK) $@ $(FAT_SEC); \
		$(WRITE_DATA) $< $(DISK);

# Write all sectors to the image, and then re-do the VBR with a BPT (mformat).
# Strange dd values are for a zero'ed 1440-byte, 3.5-inch floppy.
$(BOOT): $(BINDIR)/vbr.bin $(BINDIR)/bootloader.bin $(WRITE_DATA)
	set -e; \
		dd if=/dev/zero of=$@ bs=96 count=15; \
		$(WRITE_DATA) $< $@ 0; \
		mformat -i $@ -v $(VOLNAME) \
		-f 1440 -S 2 -H 18 -d 2 -r 512 -C -B $< :: ; \
		mcopy -i $@ $(BINDIR)/bootloader.bin ::/BOOTMOMO.BIN; \
		mdir -i $@; \
		touch $(DATADIR)/.newdisk -r $(DISK); \

clean:
	-rm -fv $(BINDIR)/* $(EXEDIR)/* $(OBJDIR)/* *.deps

diskclean: clean
	-rm -fv $(IMGDIR)/*

distclean: diskclean
	-rm -rfv $(BINDIR) $(EXEDIR) $(IMGDIR) $(OBJDIR)
	@printf "\033[0;1m"
	@printf "*** You will need to run ./bootstrap.sh to build again. ***\n"
	@printf "\033[0m"

qemu: all
	qemu-system-i386 -drive file=$(DISK),index=0,media=disk

bochs: all bochsrc.txt
	bochs

print-%:
	@echo $* = $($*)

.PRECIOUS: $(OBJDIR)/%.o

.PHONY: all qemu bochs clean diskclean distclean $(BINDIR)

