#!/bin/sh
if which opam > /dev/null; then
    opam install \
        ocamlbuild=0.14.0 \
        menhir=20181113 \
        sexplib=v0.12.0 \
        ounit=2.0.8 \
        cmdliner=1.0.3 \
        ANSITerminal=0.8.1 \
        yojson=1.7.0
else
    >&2 echo "ERROR: Install opam first!"
    exit 1
fi
