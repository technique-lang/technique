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
	ghc --make -O -o technique Technique.hs

clean:
	-rm -f *.hi *.o technique tags
