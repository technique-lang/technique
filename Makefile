all: dirs technique

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
	touch $(BUILDDIR).dir

technique: $(SOURCES)
	hasktags -cx .
	@echo "GHC\tTechnique.hs"
	ghc --make -O -outputdir $(BUILDDIR) -o $(BUILDDIR)/technique.bin Technique.hs
	@echo "STRIP\ttechnique"
	strip -o ./technique $(BUILDDIR)/technique.bin
	-rm -f $(BUILDDIR)/technique.bin

clean:
	@echo "RM\ttemp files"
	-rm -f *.hi *.o technique snippet tags
	-rm -rf $(BUILDDIR)
