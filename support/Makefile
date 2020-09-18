CC = cc -O2 -g -I $(LIBDIR)/support/c -I $(shell ocamlc -where)
OCAMLOPT = ocamlfind opt -I +threads -g

.PHONY: clean

all: .ts-build

.ts-build: rts.o Rts.cmi Rts.cmx
	touch .ts-build

rts.o: rts.c
	$(CC) -o $@ -c $<

Rts.mli: Rts.ml
	$(OCAMLOPT) -i Rts.ml > Rts.mli

Rts.cmi: Rts.mli
	$(OCAMLOPT) -c Rts.mli

Rts.cmx: Rts.ml
	$(OCAMLOPT) -c Rts.ml

clean:
	-rm -f Rts.{mli,cmi,cmx} rts.o
