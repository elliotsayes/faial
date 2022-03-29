#!/bin/bash
cu-to-json "$1" | faial-infer --provenance -t json --skip-access - | ./_build/src/cast.native
 
