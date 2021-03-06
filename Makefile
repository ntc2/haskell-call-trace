all: ghc-build proof-trees

################################################################
# GHC

GHC = ghc -Wall
OUT = tmp

# Build everything, even if it's not all used in 'Test' yet.
.PHONY: ghc-build
ghc-build:
	mkdir -p $(OUT) && \
	$(GHC) \
	-main-is Data.Function.Decorator.Test \
	-isrc \
	-outputdir $(OUT) \
	-o $(OUT)/Test \
	$(shell find src -name '*.hs')

.PHONY: ghc-build-opt
ghc-build-opt: GHC := $(GHC) -O2
ghc-build-opt: OUT := $(OUT)/opt
ghc-build-opt: ghc-build

################################################################
# Cabal sandbox

.cabal-sandbox cabal.sandbox.config:
	cabal sandbox init

.PHONY: cabal-sandbox-install
cabal-sandbox-install: .cabal-sandbox cabal.sandbox.config
	cabal install $(CABAL_OPTS)

.PHONY: cabal-sandbox-install-prof
cabal-sandbox-install-prof: \
  CABAL_OPTS += --ghc-options="-fprof-auto" \
                --builddir=prof-dist \
                --disable-optimization
cabal-sandbox-install-prof: cabal-sandbox-install


################################################################
# Misc

# Does not work for literate Haskell; see spire.git/Makefile if you
# need that.
.PHONY: tags
tags: tmp
	-rm tmp/TAGS
	cd tmp \
	&& find ../src \
	   -name '*.hs' -print \
	   | xargs hasktags --etags

# Print the module names corresponding to the source files.
.PHONY: modules
modules:
	find src -name '*.hs' | sed -re 's|src/|import |' -e 's|/|.|g' -e 's/.hs//'

.PHONY: clean
clean:
	-rm -rf tmp

tmp:
	mkdir -p tmp

# Build the proof-trees code.
proof-trees:
	$(MAKE) -C experiments/proof-trees
