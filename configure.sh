#!/bin/sh
if which opam > /dev/null; then
    opam install "$@" \
        dune=3.3.1 \
        menhir=20211128 \
        ounit=2.2.6 \
        cmdliner=1.1.1 \
        ANSITerminal=0.8.5 \
        yojson=2.0.1 \
        z3=4.9.1 \
        printbox-text=0.6.1
        toml=7.0.0
else
    >&2 echo "ERROR: Install opam first!"
    exit 1
fi
