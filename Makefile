all: build-core

BUILDDIR = /tmp/build/technique

ifdef V
MAKEFLAGS=-R
else
MAKEFLAGS=-s -R
REDIRECT=>/dev/null
endif

.PHONY: all depends dirs test build-core build-test

#
# Disable missing signatures so that you can actually do development and
# let type-inference get on with things without Haskell bothering you.
# Likewise ignore unused functions since they're usually there while exploring
# various alternative implementations of a function.
#

GHC=ghc -Wall -Werror -fwarn-tabs -fno-warn-missing-signatures -fno-warn-unused-binds

SOURCES=$(shell find src -name '*.hs')
#OBJECTS=$(patsubst %.hs,%.o,$(SOURCES))
OBJECTS=$(patsubst ./src/%.hs,$(BUILDDIR)/%.o,$(SOURCES))

dirs: $(BUILDDIR)/.dir

$(BUILDDIR)/.dir:
	@echo "MKDIR\t$(BUILDDIR)"
	mkdir -p $(BUILDDIR)
	touch $(BUILDDIR)/.dir

depends: .depends

.depends: $(SOURCES)
	@echo "GHC -M\t*.hs"
	ghc -M -isrc:tests -outputdir $(BUILDDIR) \
		-dep-makefile .depends \
		$(SOURCES)
	@echo "HASKTAGS"
	hasktags -cx .

include .depends




build-core: dirs $(BUILDDIR)/technique.bin

$(BUILDDIR)/%.hi: $(BUILDDIR)/%.o

$(BUILDDIR)/%.o: src/%.hs 
	@echo "GHC\t$<"
	$(GHC) -O -threaded  \
		-prof -fprof-auto \
		-outputdir $(BUILDDIR) -i"src:$(BUILDDIR)" -o $@ -c $<

$(BUILDDIR)/technique.bin: $(OBJECTS)
	@echo "LINK\tTechnique.hs"
	$(GHC) -O -threaded  \
		-prof -fprof-auto \
		-outputdir $(BUILDDIR) -i"src:$(BUILDDIR)" -o $@ $(OBJECTS)
	@echo "STRIP\t$@"
	strip $@

build-test: dirs $(BUILDDIR)/check.bin

$(BUILDDIR)/check.bin: tests/CheckServer.hs src/HttpServer.hs
	hasktags -cx .
	@echo "GHC\tCheck.hs"
	$(GHC) --make -O -threaded  \
		-prof -fprof-auto \
		-outputdir $(BUILDDIR) -i"src:tests:$(BUILDDIR)" -o $(BUILDDIR)/check.bin tests/CheckServer.hs
	@echo "STRIP\t$(BUILDDIR)/check"
	strip $(BUILDDIR)/check.bin

test: build-test
	@echo "EXEC\tcheck"
	$(BUILDDIR)/check.bin

clean:
	@echo "RM\ttemp files"
	-rm -f *.hi *.o technique snippet tags
	-rm -rf $(BUILDDIR)
	-rm -f .depends
