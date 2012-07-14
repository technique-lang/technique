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

CORE_SOURCES=$(shell find src -name '*.hs')
TEST_SOURCES=$(shell find tests -name '*.hs')
CORE_OBJECTS=$(patsubst src/%.hs,$(BUILDDIR)/%.o,$(CORE_SOURCES))
TEST_OBJECTS=$(patsubst tests/%.hs,$(BUILDDIR)/%.o,$(TEST_SOURCES))

PACKAGES=\
-package base \
-package bytestring \
-package MonadCatchIO-transformers \
-package mtl \
-package hedis \
-package snap-core \
-package snap-server

dirs: $(BUILDDIR)/.dir

$(BUILDDIR)/.dir:
	@echo "MKDIR\t$(BUILDDIR)"
	mkdir -p $(BUILDDIR)
	touch $(BUILDDIR)/.dir

depends: .depends

.depends: $(CORE_SOURCES) $(TEST_SOURCES)
	@echo "GHC -M\t.depends"
	ghc -M -isrc:tests -outputdir $(BUILDDIR) \
		-dep-makefile .depends \
		$(PACKAGES) \
		$(CORE_SOURCES) $(TEST_SOURCES)
	@echo "HASKTAGS"
	hasktags -cx .

include .depends




build-core: dirs $(BUILDDIR)/technique.bin

#$(BUILDDIR)/%.hi: $(BUILDDIR)/%.o
#

$(BUILDDIR)/%.o: src/%.hs 
	@echo "GHC\t$<"
	$(GHC) -O -threaded  \
		-prof -fprof-auto \
		-outputdir $(BUILDDIR) -i"$(BUILDDIR):src" \
		-main-is Technique \
		-o $@ -c $<

$(BUILDDIR)/technique.bin: $(CORE_OBJECTS)
	@echo "LINK\t$@"
	$(GHC) -O -threaded  \
		-prof -fprof-auto \
		-outputdir $(BUILDDIR) -i"$(BUILDDIR):src" \
		-main-is Technique \
		$(PACKAGES) \
		-o $@ $(CORE_OBJECTS)
	@echo "STRIP\t$@"
	strip $@

build-test: dirs $(BUILDDIR)/check.bin

$(BUILDDIR)/%.o: tests/%.hs 
	@echo "GHC\t$<"
	$(GHC) -O -threaded  \
		-prof -fprof-auto \
		-outputdir $(BUILDDIR) -i"$(BUILDDIR):src:tests" \
		-main-is CheckServer \
		-o $@ -c $<

$(BUILDDIR)/check.bin: $(CORE_OBJECTS) $(TEST_OBJECTS)
	hasktags -cx .
	@echo "GHC\t$@"
	$(GHC) --make -O -threaded  \
		-prof -fprof-auto \
		-main-is CheckServer \
		$(PACKAGES) \
		-outputdir $(BUILDDIR) -i"$(BUILDDIR):src:tests" \
		-o $@ $(CORE_OBJECTS) $(TEST_OBJECTS) 
	@echo "STRIP\t$@"
	strip $@

test: build-test
	@echo "EXEC\tcheck"
	$(BUILDDIR)/check.bin

clean:
	@echo "RM\ttemp files"
	-rm -f *.hi *.o technique snippet tags
	-rm -rf $(BUILDDIR)
	-rm -f .depends
