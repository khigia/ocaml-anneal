TARGETS=\
	$(patsubst test/%Test.ml,test/%Test.byte,$(wildcard test/*Test.ml))\
	$(patsubst ex/%Run.ml,ex/%Run.native,$(wildcard ex/*Run.ml))

OCAMLBUILD=ocamlbuild -classic-display -I ex

.PHONY: build
build:
	$(OCAMLBUILD) $(TARGETS)

.PHONY: clean
clean:
	$(OCAMLBUILD) -clean
