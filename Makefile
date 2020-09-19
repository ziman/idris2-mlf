IDRIS2 ?= idris2
PREFIX ?= $(shell $(IDRIS2) --prefix)
export LIBDIR ?= $(shell $(IDRIS2) --libdir)

.PHONY: all clean build install self

all: build

build: build/exec/idris2-mlf support/.ts-build

self: build/exec/idris2-mlf-mlf

build/exec/idris2-mlf-mlf: src/*.idr build/exec/idris2-mlf install-support
	build/exec/idris2-mlf --build idris2-mlf-mlf.ipkg

build/exec/idris2-mlf: src/*.idr
	$(IDRIS2) --build idris2-mlf.ipkg

.PHONY: support/.ts-build
support/.ts-build:
	make -C support

# install a self-compiled, statically linked version
install: build/exec/idris2-mlf-mlf
	install -m 755 build/exec/idris2-mlf-mlf $(PREFIX)/bin/idris2-mlf

install-support: support/.ts-build
	# install the support code
	mkdir -p $(LIBDIR)/support/mlf
	install -m 644 support/rts.o $(LIBDIR)/support/mlf
	install -m 644 support/Rts.{o,cmi,cmx} $(LIBDIR)/support/mlf

clean:
	make -C support clean
	-rm -rf build/
