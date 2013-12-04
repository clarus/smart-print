default:
	ocamlbuild smartPrintLib.cma smartPrintLib.cmxa

install: default
	ocamlfind install smart_print META _build/smartPrint.cmi _build/smartPrint.cmx _build/smartPrintLib.a _build/smartPrintLib.cma _build/smartPrintLib.cmxa _build/smartPrintLib.mllib

uninstall:
	ocamlfind remove smart_print

clean:
	ocamlbuild -clean

doc:
	ocamlbuild smartPrint.docdir/index.html

test: default
	ocamlbuild test.native -libs str
	./test.native |diff -s - test.out