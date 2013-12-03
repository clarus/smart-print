default:
	ocamlbuild smartPrint.native

clean:
	ocamlbuild -clean

doc:
	ocamlbuild smartPrint.docdir/index.html

test: default
	./smartPrint.native |diff -s - test.out