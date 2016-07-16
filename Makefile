### VPATH Build ###
ifeq (,$(filter _%,$(notdir $(CURDIR))))
	include target.mk
else
	### VPATH Build ###

include $(PROJDIR)/config.mk

DATADIR      := $(PROJDIR)/data
SRCDIR       := $(PROJDIR)/src
INCDIRS      := include
INCDIRS      := $(patsubst %,$(PROJDIR)/%,$(INCDIRS))
MODULES      := $(SRCDIR) $(DATADIR)
VPATH        := $(patsubst %,%:,$(MODULES) $(INCDIRS))

ASMSOURCES   :=
CSOURCES     :=

DATALIST     :=
UPDATE_DATA  :=
BOCHSRC      := bochsrc.txt
BOCHSRC_IN   := $(PROJDIR)/bochsrc.txt.in
WRITE_DATA   := ./write_data
MAKE_DISK    := $(PROJDIR)/make_disk.sh

FAT_SEC      := 2048
DISK         := disk.img
BOOT         := boot.img
VOLNAME      := "OSMomo Boot"

LD            = ld
LDFLAGS_HOST :=
LDFLAGS      :=

CC            = gcc
CFLAGS_HOST  += -std=c11
CFLAGS       += -Wall -Werror -O2 -pedantic-errors
CFLAGS       += $(patsubst %,-I%/,$(subst :, ,$(VPATH)))

ASM           = nasm
ASMFLAGS_BIN += -f bin
ASMFLAGS     += -w+all
ASMFLAGS     += $(patsubst %,-I%/,$(subst :, ,$(VPATH)))

include $(patsubst %,%/module.mk,$(MODULES))

all: $(WRITE_DATA) $(UPDATE_DATA) | $(DISK) $(BOOT)
	set -e; \
		$(WRITE_DATA) $(BOOT) $(DISK) $(FAT_SEC); \
		$(UPDATE_DATA) $(DATALIST) $(WRITE_DATA) $(DISK) $(DATADIR); \
		printf "\033[0;1m"; \
		printf "*** Disk image \`$(CURDIR)/$(DISK)\` is up-to-date. ***\n"; \
		printf "*** Use \`make bochs\` or \`make qemu\` to emulate \`$(DISK)\`. ***\n"; \
		printf "*** Use \`make drive [DRIVE=/dev/sdX]\` to image a drive (Default: \`$(DRIVE)\`). ***\n"; \
		printf "\033[0m";

-include $(patsubst %.asm,.%.deps,$(ASMSOURCES))
-include $(patsubst %.c,.%.deps,$(CSOURCES))

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

%.bin: %.asm
	$(ASM) $(ASMFLAGS) $(ASMFLAGS_BIN) $< -o $@

%.o: %.c
	$(CC) $(CFLAGS) $(CFLAGS_HOST) -c $< -o $@

%: %.o
	$(CC) $(CFLAGS) $(CFLAGS_HOST) $< -o $@

$(DISK): mbr.bin $(MAKE_DISK) $(WRITE_DATA)
	set -e; \
		$(MAKE_DISK) $@ $(FAT_SEC); \
		$(WRITE_DATA) $< $(DISK);

# Write all sectors to the image, and then re-do the VBR with a BPT (mformat).
# Strange dd values are for a zero'ed 1440-byte, 3.5-inch floppy.
$(BOOT): vbr.bin bootloader.bin $(WRITE_DATA)
	set -e; \
		dd if=/dev/zero of=$@ bs=96 count=15; \
		$(WRITE_DATA) $< $@ 0; \
		mformat -i $@ -v $(VOLNAME) \
		-f 1440 -S 2 -H 18 -d 2 -r 512 -C -B $< :: ; \
		mcopy -i $@ bootloader.bin ::/BOOTMOMO.BIN; \
		mdir -i $@; \
		touch .newdisk -r $(DISK); \

$(BOCHSRC): $(BOCHRC_IN)
	set -e; \
		cp $(BOCHSRC_IN) $(BOCHSRC); \
		sed -i $(BOCHSRC) \
		-e "s,@DISK@,$(DISK),g" \
		-e "s,@PROJDIR@,$(PROJDIR),g";

qemu: all
	qemu-system-i386 -drive file=$(DISK),index=0,media=disk

bochs: $(BOCHSRC) all
	bochs -qf $<

drive: all
	set -e; \
		if ! [ -b "$(DRIVE)" ]; then \
		printf "\033[0;31m*** \`$(DRIVE)\` is not a block device! ***\033[0m\n"; \
		exit 1; \
		else \
		printf "\033[0;1m*** This will overwrite all data on \`$(DRIVE)\`! ***\033[0m\n"; \
		printf "\033[0;1m*** Are you sure you want to continue? [y/N]: \033[0m"; \
		read ANSWER; \
		if [ "$$ANSWER" = "y" ]; then \
		printf "\033[0;34m"; \
		if [ -x "$$(which pv)" ]; then \
		dd if=$(DISK) | pv | sudo dd of=$(DRIVE) bs=1M; \
		else \
		sudo dd if=$(DISK) of=$(DRIVE) bs=1M; \
		fi; \
		sudo eject $(DRIVE); \
		printf "\n\033[0;32m*** \`$(DRIVE)\` was imaged. ***\033[0m\n"; \
		else \
		printf "\033[0;31m*** Cancelled. ***\033[0m\n"; \
		fi; \
		fi;
	@printf "\033[0m"

.PRECIOUS: %.o

.PHONY: all qemu bochs drive

endif ### VPATH Build ###

