#!/bin/bash
cu-to-json "$1" | ./_build/default/bin/test_c.exe
exit "$?" 
