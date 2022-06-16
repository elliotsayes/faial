#!/bin/bash
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
file1=$(mktemp)

"${SCRIPT_DIR}/run-proto-to-cuda.sh" $1 > "$file1"
"${SCRIPT_DIR}/run-proto-to-cuda.sh" "$file1" | diff "$file1" -
