#!/bin/sh
if which opam > /dev/null; then
    opam install "$@" \
        ocamlbuild=0.14.0 \
        menhir=20201201 \
        ounit=2.2.3 \
        cmdliner=1.0.4 \
        ANSITerminal=0.8.2 \
        yojson=1.7.0 \
        ppx_deriving=5.2 \
        ppx_hash=v0.14.0
else
    >&2 echo "ERROR: Install opam first!"
    exit 1
fi
