all: tmp/proof-tree tmp/self-contained-proof-tree

GHC = ghc -Wall

# Library source directory
SRC = $(shell git rev-parse --show-toplevel)/src

# usage: make PT=tmp/<proof tree executable> tests
tests: $(PT)
	./tm2latex.sh $(PT) S.ctx  'x:P->Q->R,y:P->Q,z:P' 'xz(yz)'
	./tm2latex.sh $(PT) S.lam  ''                     '\x:P->Q->R.\y:P->Q.\z:P.xz(yz)'
	./tm2latex.sh $(PT) S.err  ''                     '\x:P->Q->R.\y:P->Q.\z:P.xzyz'
	./tm2latex.sh $(PT) K.ctx  'x:P,y:Q'              'x'
	./tm2latex.sh $(PT) K.lam  ''                     '\x:P.\y:Q.x'
	./tm2latex.sh $(PT) I.ctx  'x:P'                  'x'
	./tm2latex.sh $(PT) I.lam  ''                     '\x:P.x'
	./tm2latex.sh $(PT) MP.ctx 'x:P,y:P->Q'           'yx'
	./tm2latex.sh $(PT) MP.lam ''                     '\x:P.\y:P->Q.yx'
	./tm2latex.sh $(PT) ZERO   ''                     '\s:A->A.\z:A.z'
	./tm2latex.sh $(PT) SUCC   ''                     '\n:(A->A)->(A->A).\s:A->A.\z:A.s(nsz)'
	./tm2latex.sh $(PT) ADD    '' '\m:(A->A)->(A->A).\n:(A->A)->(A->A).\s:A->A.\z:A.ms(nsz)'
	./tm2latex.sh $(PT) MULT   '' '\m:(A->A)->(A->A).\n:(A->A)->(A->A).\s:A->A.\z:A.m(ns)z'
	./tm2latex.sh $(PT) Y.err  ''                        '\f:A->A.(\x:A.f(xx))(\x:A.f(xx))'
	./tm2latex.sh $(PT) Y.ctx  'a:A->(A->A),y:(A->A)->A' '\f:A->A.(\x:A.f(axx))(y(\x:A.f(axx)))'

tmp/proof-tree: Test.hs tmp
	$(GHC) \
	    -outputdir tmp \
	    -i$(SRC) \
	    -main-is Test \
	    -o $@ \
	    $<

tmp/self-contained-proof-tree: SelfContainedTest.hs tmp
	$(GHC) \
	    -outputdir tmp/self-contained \
	    -i$(SRC) \
	    -main-is SelfContainedTest \
	    -o $@ \
	    $<

tmp/example.pdf: example.tex tmp
	pdflatex -output-directory=tmp $<

tmp/%.pdf: tmp/%.tex tmp
	pdflatex -output-directory=tmp $<

tmp:
	mkdir -p tmp

clean:
	-rm -rf tmp
