IDRIS2 ?= idris2
PREFIX ?= $(shell $(IDRIS2) --prefix)
export LIBDIR ?= $(shell $(IDRIS2) --libdir)

.PHONY: all clean build install

all: build

build: build/exec/idris2-mlf support/.ts-build

build/exec/idris2-mlf: src/*.idr
	$(IDRIS2) --build idris2-mlf.ipkg

.PHONY: support/.ts-build
support/.ts-build:
	make -C support

install-support: support/.ts-build
	# install the support code
	mkdir -p $(LIBDIR)/support/mlf
	install support/rts.o $(LIBDIR)/support/mlf
	install support/Rts.{cmi,cmx} $(LIBDIR)/support/mlf

clean:
	make -C support clean
	idris2 --clean idris2-mlf.ipkg
