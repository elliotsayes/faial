#!/bin/bash
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
faial --parse-gv-args --infer-only "$1" --infer-output-json | "${SCRIPT_DIR}/proto-to-cuda" --json $2
