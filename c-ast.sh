#!/bin/bash
cu-to-json "$1" | ./_build/src/test_c.native
exit "$?" 
