IDRIS2 ?= idris2
export LIBDIR ?= $(shell $(IDRIS2) --libdir)

.PHONY: all build install

all: build

build: build/exec/idris2-mlf support/.ts-build

build/exec/idris2-mlf: src/*.idr
	$(IDRIS2) --build idris2-mlf.ipkg

.PHONY: support/.ts-build
support/.ts-build:
	make -C support

install: support/.ts-build build/exec/idris2-mlf
	mkdir -p $(LIBDIR)/support/mlf
	install support/mlf/rts.o $(LIBDIR)/support/mlf
	install support/mlf/Rts.{cmi,cmx} $(LIBDIR)/support/mlf
