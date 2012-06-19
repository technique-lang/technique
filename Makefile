all: technique

ifdef V
MAKEFLAGS=-R
else
MAKEFLAGS=-s -R
REDIRECT=>/dev/null
endif

SOURCES=$(shell find . -name '*.hs')

technique: $(SOURCES)
	hasktags -cx .
	@echo "GHC\tTechnique.hs"
	ghc --make -O -outputdir /tmp/build/technique -o /tmp/build/technique.bin Technique.hs
	@echo "STRIP\ttechnique"
	strip -o ./technique /tmp/build/technique.bin
	-rm -f /tmp/build/technique.bin

clean:
	-rm -f *.hi *.o technique snippet tags
