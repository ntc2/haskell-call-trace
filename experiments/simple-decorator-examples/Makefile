all: tmp/main

GHC = ghc -Wall

# Library source directory
SRC = $(shell git rev-parse --show-toplevel)/src

tmp/main: OUT=tmp
tmp/main: main

tmp/opt/main: OUT=tmp/opt
tmp/opt/main: OPTS=-O2
tmp/opt/main: main

.PHONY: main
main: Main.hs
	mkdir -p $(OUT) && \
	$(GHC) $(OPTS) \
	    -outputdir $(OUT) \
	    -i$(SRC) \
	    -o $(OUT)/$@ \
	    $<

clean:
	-rm -rf tmp
	-rm -rf tmp/opt
