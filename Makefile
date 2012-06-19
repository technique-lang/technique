all: dirs technique

ifdef V
MAKEFLAGS=-R
else
MAKEFLAGS=-s -R
REDIRECT=>/dev/null
endif

.PHONY: dirs

SOURCES=$(shell find . -name '*.hs')

dirs: /tmp/build/technique/.dir

/tmp/build/technique/.dir:
	@echo "MKDIR\t/tmp/build"
	mkdir -p /tmp/build
	touch /tmp/build/.dir

technique: $(SOURCES)
	hasktags -cx .
	@echo "GHC\tTechnique.hs"
	ghc --make -O -outputdir /tmp/build/technique -o /tmp/build/technique/technique.bin Technique.hs
	@echo "STRIP\ttechnique"
	strip -o ./technique /tmp/build/technique/technique.bin
	-rm -f /tmp/build/technique/technique.bin

clean:
	-rm -f *.hi *.o technique snippet tags
