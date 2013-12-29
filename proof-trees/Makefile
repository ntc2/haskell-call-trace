all: tests

tests: tmp/proof-tree
	./tm2latex.sh S.ctx  'x:P->Q->R,y:P->Q,z:P' 'xz(yz)'
	./tm2latex.sh S.lam  ''                     '\x:P->Q->R.\y:P->Q.\z:P.xz(yz)'
	./tm2latex.sh S.err  ''                     '\x:P->Q->R.\y:P->Q.\z:P.xzyz'
	./tm2latex.sh K.ctx  'x:P,y:Q'              'x'
	./tm2latex.sh K.lam  ''                     '\x:P.\y:Q.x'
	./tm2latex.sh I.ctx  'x:P'                  'x'
	./tm2latex.sh I.lam  ''                     '\x:P.x'
	./tm2latex.sh MP.ctx 'x:P,y:P->Q'           'yx'
	./tm2latex.sh MP.lam ''                     '\x:P.\y:P->Q.yx'
	./tm2latex.sh ZERO   ''                     '\s:A->A.\z:A.z'
	./tm2latex.sh SUCC   ''                     '\n:(A->A)->(A->A).\s:A->A.\z:A.s(nsz)'
	./tm2latex.sh ADD    '' '\m:(A->A)->(A->A).\n:(A->A)->(A->A).\s:A->A.\z:A.ms(nsz)'
	./tm2latex.sh MULT   '' '\m:(A->A)->(A->A).\n:(A->A)->(A->A).\s:A->A.\z:A.m(ns)z'
	./tm2latex.sh Y.err  ''                        '\f:A->A.(\x:A.f(xx))(\x:A.f(xx))'
	./tm2latex.sh Y.ctx  'a:A->(A->A),y:(A->A)->A' '\f:A->A.(\x:A.f(axx))(y(\x:A.f(axx)))'

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
