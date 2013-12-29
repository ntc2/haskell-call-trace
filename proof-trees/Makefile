all: tmp/proof-tree

tmp/proof-tree: Test.hs tmp
	ghc -outputdir tmp \
	    -i../src \
	    -main-is Test \
	    -o $@ \
	    Test.hs

tmp/example.pdf: example.tex tmp
	pdflatex -output-directory=tmp $<

tmp/%.pdf: tmp/%.tex tmp
	pdflatex -output-directory=tmp $<

tmp:
	mkdir -p tmp

clean:
	-rm -rf tmp
