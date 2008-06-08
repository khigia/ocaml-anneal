TARGETS=\
	$(patsubst test/%Test.ml,test/%Test.byte,$(wildcard test/*Test.ml))

OCAMLBUILD=ocamlbuild -classic-display

.PHONY: build
build:
	$(OCAMLBUILD) $(TARGETS)
