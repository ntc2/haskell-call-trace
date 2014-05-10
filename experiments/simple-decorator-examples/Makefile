all: tmp/main

GHC = ghc -Wall

# Library source directory
SRC = $(shell git rev-parse --show-toplevel)/src

tmp/main: Main.hs tmp
	$(GHC) \
	    -outputdir tmp \
	    -i$(SRC) \
	    -o $@ \
	    $<

tmp:
	mkdir -p tmp

clean:
	-rm -rf tmp
