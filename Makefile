# executables
IDRIS2     ?= idris2
IDRIS2_MLF ?= $(shell $(IDRIS2) --prefix)/bin/idris2-mlf

# paths
PREFIX ?= $(shell $(IDRIS2) --prefix)
export LIBDIR ?= $(shell $(IDRIS2) --libdir)

.PHONY: all clean build bootstrap install

all: build

build: build/exec/idris2-mlf support/.ts-build

build/exec/idris2-mlf-bootstrap: src/*.idr
	$(IDRIS2) --build idris2-mlf-bootstrap.ipkg

bootstrap: build/exec/idris2-mlf-bootstrap install-support
	build/exec/idris2-mlf-bootstrap --build idris2-mlf.ipkg
	make install

rebootstrap: install-support  # do not rebuild the bootstrap compiler
	build/exec/idris2-mlf-bootstrap --build idris2-mlf.ipkg
	make install

build/exec/idris2-mlf: src/*.idr
	@[ -e "$(IDRIS2_MLF)" ] || echo -e "\n  !!! idris2-mlf not found, run 'make bootstrap' !!!\n"
	$(IDRIS2_MLF) --build idris2-mlf.ipkg

.PHONY: support/.ts-build
support/.ts-build:
	make -C support

# install a self-compiled, statically linked version
install: build install-support
	install -m 755 build/exec/idris2-mlf $(IDRIS2_MLF)

install-support: support/.ts-build
	# install the support code
	mkdir -p $(LIBDIR)/support/mlf
	install -m 644 support/rts.o           $(LIBDIR)/support/mlf
	install -m 644 support/Rts.{o,cmi,cmx} $(LIBDIR)/support/mlf

clean:
	make -C support clean
	-rm -rf build/
