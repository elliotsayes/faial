#!/bin/bash
cu-to-json "$1" | faial-infer --provenance -t json --skip-access -X 3 - | ./_build/src/test_c.native
exit "$?" 
