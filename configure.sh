#!/bin/sh
if which opam > /dev/null; then
    opam install "$@" \
        dune=3.6.1 \
        ounit=2.2.6 \
        cmdliner=1.1.1 \
        ANSITerminal=0.8.5 \
        yojson=2.0.1 \
        z3=4.11.2 \
        printbox-text=0.6.1

else
    >&2 echo "ERROR: Install opam first!"
    exit 1
fi
