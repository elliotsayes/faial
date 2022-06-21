#!/bin/bash
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" &>/dev/null && pwd)"
file1=$(mktemp /tmp/file1.XXXXXX)
file2=$(mktemp /tmp/file2.XXXXXX)

"${SCRIPT_DIR}/proto-to-cuda" --cuda $1 > "$file1"
"${SCRIPT_DIR}/proto-to-cuda" --cuda "$file1" > "$file2"

if [ ! -s "$file1" ]; then
    echo "$1 could not be parsed by proto-to-cuda."
elif [ ! -s "$file2" ]; then
    echo "$1 could not be parsed by proto-to-cuda twice."
elif cmp -s "$file1" "$file2"; then
    echo "$1 reaches a fixed point."
else
    echo "$1 does not reach a fixed point:"
    diff -u "$file1" "$file2"
fi
