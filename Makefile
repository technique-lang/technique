all: dirs build

BUILDDIR = /tmp/build/technique

ifdef V
MAKEFLAGS=-R
else
MAKEFLAGS=-s -R
REDIRECT=>/dev/null
endif

.PHONY: dirs

SOURCES=$(shell find . -name '*.hs')

dirs: $(BUILDDIR)/.dir

$(BUILDDIR)/.dir:
	@echo "MKDIR\t$(BUILDDIR)"
	mkdir -p $(BUILDDIR)
	touch $(BUILDDIR)/.dir

build: $(BUILDDIR)/technique.bin $(BUILDDIR)/check.bin

$(BUILDDIR)/technique.bin: $(SOURCES)
	hasktags -cx .
	@echo "GHC\tTechnique.hs"
	ghc --make -O -threaded  \
		-prof -fprof-auto \
		-outputdir $(BUILDDIR) -o $(BUILDDIR)/technique.bin Technique.hs
	@echo "STRIP\ttechnique"
	strip $(BUILDDIR)/technique.bin
	@echo

$(BUILDDIR)/check.bin: $(SOURCES)
	hasktags -cx .
	@echo "GHC\tCheck.hs"
	ghc --make -O -threaded  \
		-prof -fprof-auto \
		-outputdir $(BUILDDIR) -o $(BUILDDIR)/check.bin Check.hs
	@echo "STRIP\tcheck"
	strip $(BUILDDIR)/check.bin
	@echo

clean:
	@echo "RM\ttemp files"
	-rm -f *.hi *.o technique snippet tags
	-rm -rf $(BUILDDIR)
