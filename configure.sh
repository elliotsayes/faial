#!/bin/sh
if which opam > /dev/null; then
    opam install "$@" \
        dune=3.14.0 \
        ounit=2.2.7 \
        cmdliner=1.2.0 \
        ANSITerminal=0.8.5 \
        yojson=2.1.2 \
        z3=4.12.6 \
        printbox-text=0.10 \
        feather=0.3.0 \
        fpath=0.7.3

else
    >&2 echo "ERROR: Install opam first!"
    exit 1
fi
