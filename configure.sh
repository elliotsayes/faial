#!/bin/sh
if which opam > /dev/null; then
    opam install "$@" \
        dune=3.16.0 \
        ounit=2.2.7 \
        cmdliner=1.3.0 \
        ANSITerminal=0.8.5 \
        toml=7.1.0 \
        otoml=1.0.5 \
        yojson=2.2.2 \
        z3=4.13.0 \
        printbox-text=0.11 \
        fpath=0.7.3
else
    >&2 echo "ERROR: Install opam first!"
    exit 1
fi
