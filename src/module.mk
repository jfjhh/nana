ASMSOURCES  += $(wildcard $(SRCDIR)/*.asm)
CSOURCES    += $(wildcard $(SRCDIR)/*.c)
EXECUTABLES += $(patsubst %,$(EXEDIR)/%,\
	       $(notdir $(basename $(wildcard $(SRCDIR)/*.c))))
BINARIES    += $(patsubst %,$(BINDIR)/%.bin,\
	       $(notdir $(basename $(wildcard $(SRCDIR)/*.asm))))

