all: technique


#
# Top-level targets. This is ugly. A program to extract these from the .cabal
# file would work, but is there anything easier?
#

technique: 
	stack build 


#
# Setup
#

ifdef V
MAKEFLAGS=-R
else
MAKEFLAGS=-s -R
REDIRECT=2>/dev/null
endif

.PHONY: all build test

#
# Build rules. This just wraps Cabal doing its thing in a Haskell
# language-specific fashion.
#

build: dist/setup-config tags
	@/bin/echo -e "STACK\tbuild"
	stack build

test: dist/setup-config tags
	@/bin/echo -e "STACK\ttest"
	stack test


# This will match writer-test/writer-test, so we have to strip the directory
# portion off. Annoying, but you can't use two '%' in a pattern rule.
dist/build/%: dist/setup-config tags $(SOURCES)
	@/bin/echo -e "STACK\tbuild $@"
	cabal build $(notdir $@)

#
# Build ctags file
#

SOURCES=$(shell find lib -name '*.hs' -type f) \
	$(shell find src -name '*.hs' -type f) \
	$(shell find tests -name '*.hs' -type f)

HOTHASKTAGS=$(shell which hothasktags 2>/dev/null)
CTAGS=$(if $(HOTHASKTAGS),$(HOTHASKTAGS),/bin/false)

tags: $(SOURCES)
	if [ "$(HOTHASKTAGS)" ] ; then /bin/echo -e "CTAGS\ttags" ; fi
	-$(CTAGS) $^ > tags $(REDIRECT)

format: $(SOURCES)
	stylish-haskell -i $^

clean:
	@/bin/echo -e "STACK\tclean"
	-stack clean >/dev/null
	@/bin/echo -e "RM\ttemporary files"
	-rm -f tags
	-rm -f *.prof
	-rm -f src/Package.hs

doc:
	stack haddock

install:
	stack install
