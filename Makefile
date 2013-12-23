# Build everything, even if it's not all used in 'Test' yet.
all: tmp
	ghc -Wall \
	-main-is Debug.Trace.LogTree.Test \
	-isrc \
	-outputdir tmp \
	-o tmp/Test \
	$(shell find src -name '*.hs')

# Print the module names corresponding to the source files.
modules:
	find src -name '*.hs' | sed -re 's|src/|import |' -e 's|/|.|g' -e 's/.hs//'

clean:
	-rm -rf tmp

tmp:
	mkdir -p tmp
