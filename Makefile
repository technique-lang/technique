all: build-core

BUILDDIR = /tmp/build/technique

ifdef V
MAKEFLAGS=-R
else
MAKEFLAGS=-s -R
REDIRECT=>/dev/null
endif

.PHONY: all dirs test build-core build-test

SOURCES=$(shell find . -name '*.hs')

dirs: $(BUILDDIR)/.dir

$(BUILDDIR)/.dir:
	@echo "MKDIR\t$(BUILDDIR)"
	mkdir -p $(BUILDDIR)
	touch $(BUILDDIR)/.dir


build-core: dirs $(BUILDDIR)/technique.bin

$(BUILDDIR)/technique.bin: src/Technique.hs src/HttpServer.hs src/Lookup.hs
	hasktags -cx .
	@echo "GHC\tTechnique.hs"
	ghc --make -O -threaded  \
		-prof -fprof-auto \
		-outputdir $(BUILDDIR) -i"src" -o $@ src/Technique.hs
	@echo "STRIP\t$@"
	strip $@

build-test: dirs $(BUILDDIR)/check.bin

$(BUILDDIR)/check.bin: tests/CheckServer.hs src/HttpServer.hs
	hasktags -cx .
	@echo "GHC\tCheck.hs"
	ghc --make -O -threaded  \
		-prof -fprof-auto \
		-outputdir $(BUILDDIR) -i"src:tests" -o $(BUILDDIR)/check.bin tests/CheckServer.hs
	@echo "STRIP\t$(BUILDDIR)/check"
	strip $(BUILDDIR)/check.bin

test: build-test
	@echo "EXEC\tcheck"
	$(BUILDDIR)/check.bin

clean:
	@echo "RM\ttemp files"
	-rm -f *.hi *.o technique snippet tags
	-rm -rf $(BUILDDIR)
