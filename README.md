# idris2-mlf

[Malfunction](https://github.com/stedolan/malfunction) backend
for [Idris 2](https://github.com/idris-lang/Idris2).

## Installation

### Install Idris2 API

Get an `idris2` checkout.
In its root directory, run `make install-api`.

### Install Malfunction

Follow [the instructions in the Malfunction README](https://github.com/stedolan/malfunction).
I use `ocaml-4.09.1+flambda`.

Here's my [patched version](https://github.com/ziman/malfunction/)
that fixes some issues with Dune on my machine.

### Build idris2-mlf

In the root directory of `idris2-mlf`, run `make`.
Then you can use `build/exec/idris2-mlf --codegen mlf` to compile stuff.

### Install self-hosted idris2-mlf

In the root directory of `idris2-mlf`, run `make install`.
This will build `idris2-mlf-mlf` using `--codegen mlf` to obtain a statically
linked native binary that will be installed in `${IDRIS_PREFIX}/bin`.

## Usage

* `idris2-mlf --codegen mlf` builds the whole project with inter-module optimisations.
  If a module is changed, all its dependencies have to be rebuilt, even if its
  interface stays the same.

* `idris2-mlf --codegen mlf-incremental` uses `ocamlopt -opaque` to avoid
  rebuilding dependencies of a module if its interface did not change.

## License

[BSD-3](https://github.com/ziman/idris2-mlf/blob/master/LICENSE),
same as Idris 2.

## Contributors

* @ziman
* [@markuspf](https://github.com/markuspf)
* This project exchanges patches with [idris2-ocaml](https://github.com/karroffel/Idris2-Ocaml) by @karroffel.
