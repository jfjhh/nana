.SUFFIXES:

CONFIG    = config.mk
BOOTSTRAP = bootstrap.sh

ifeq ("$(wildcard $(CONFIG))","")

.PHONY: $(CONFIG)
$(CONFIG): $(BOOTSTRAP)
	@printf "\033[0;1m"
	@printf "*** Configure with '$<'. Optionally edit generated '$@'. ***\n"
	@printf "\033[0m"

else

include $(CONFIG)

BUILDDIR := $(PREFIX)/$(BUILDDIRNAME)

MAKETARGET = $(MAKE) -C $@ -f $(CURDIR)/Makefile \
	     PROJDIR=$(CURDIR) $(MAKECMDGOALS)

.PHONY: $(BUILDDIR)
$(BUILDDIR):
	+@[ -d $@ ] || mkdir -p $@
	+@$(MAKETARGET)

Makefile : ;
%.mk :: ;

% :: $(BUILDDIR) ; @:

clean:
	rm -rfv $(BUILDDIR)

distclean: clean
	rm -rfv _$(PROJNAME)* $(CONFIG)

endif

print-%:
	@echo $* = $($*)

.PHONY: clean distclean print-%

