#!/bin/bash
cu-to-json "$1" | ./_build/default/bin/parser.exe
exit "$?" 
