default: native

native:
	ocamlbuild  tt.native

byte:
	ocamlbuild tt.byte

clean:
	ocamlbuild -clean

doc:
	ocamlbuild -docflag -keep-code tt.docdir/index.html
