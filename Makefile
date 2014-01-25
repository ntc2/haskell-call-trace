all: ghc-build

################################################################
# GHC

# Build everything, even if it's not all used in 'Test' yet.
.PHONY: ghc-build
ghc-build: tmp
	ghc -Wall \
	-main-is Debug.Trace.LogTree.Test \
	-isrc \
	-outputdir tmp \
	-o tmp/Test \
	$(shell find src -name '*.hs')

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

# Print the module names corresponding to the source files.
.PHONY: modules
modules:
	find src -name '*.hs' | sed -re 's|src/|import |' -e 's|/|.|g' -e 's/.hs//'

.PHONY: clean
clean:
	-rm -rf tmp

tmp:
	mkdir -p tmp
