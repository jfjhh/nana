ASMSOURCES  += $(SRCDIR)/$(wildcard *.asm)
CSOURCES    += $(SRCDIR)/$(wildcard *.c)
EXECUTABLES += $(EXEDIR)/$($(wildcard *.c):.c=)
BINARIES    += $(BINDIR)/$($(wildcard *.asm):.asm=.bin)

