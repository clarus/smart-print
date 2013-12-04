default:
	ocamlbuild smartPrintLib.cma smartPrintLib.cmxa

install: default
	ocamlfind install smart_print META _build/smartPrintLib.* _build/*.cmx _build/*.cmi

uninstall:
	ocamlfind remove smart_print

clean:
	ocamlbuild -clean

doc:
	ocamlbuild smartPrint.docdir/index.html

test: default
	ocamlbuild test.native -libs str
	./test.native |diff -s - test.out