default: native

native:
	ocamlbuild  total.native

byte:
	ocamlbuild total.byte

clean:
	ocamlbuild -clean

doc:
	ocamlbuild -docflag -keep-code total.docdir/index.html
