CC = cc -O2 -g -I $(LIBDIR)/include -I $(shell ocamlc -where)
OCAMLOPT = ocamlfind opt -I +threads -g

.PHONY: clean

all: .ts-build

# order matters; Rts.cmi should be built before Rts.o and Rts.cmx
.ts-build: rts_c.o Rts.cmi Rts.o Rts.cmx
	touch .ts-build

rts_c.o: rts_c.c
	$(CC) -o $@ -c $<

Rts.mli: Rts.ml
	$(OCAMLOPT) -i Rts.ml > Rts.mli

Rts.cmi: Rts.mli
	$(OCAMLOPT) -c Rts.mli

Rts.cmx Rts.o: Rts.ml
	$(OCAMLOPT) -c Rts.ml

clean:
	-rm -f Rts.{mli,cmi,cmx} rts_c.o
