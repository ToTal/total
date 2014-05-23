DEBUG = true
#PROFILE = true
#WARN_PATTERN = true
#VERBOSE = 0
BYTE = 1

PARALLEL = 4


EXT = $(if $(BYTE),byte,native)

OCAMLBUILD = ocamlbuild -use-ocamlfind \
        $(if $(PARALLEL),-j $(PARALLEL),) \
        $(if $(PROFILE),-tag profile,) \
        $(if $(DEBUG),-tag debug,) \
        $(if $(VERBOSE),-verbose $(VERBOSE),) \
        $(if $(WARN_PATTERN),-tag warn\(P\) -tag warn-error\(p\),)

.PHONY: all clean

all : total

total:
	$(OCAMLBUILD)  total.$(EXT)

doc:
	$(OCAMLBUILD) -docflag -keep-code total.docdir/index.html

clean:
	$(OCAMLBUILD) -clean

%:
	$(OCAMLBUILD) $@
