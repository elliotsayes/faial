#!/bin/bash
faial --parse-gv-args --infer-only "$1" --infer-output-json | ~/Work/faial/bank-conflicts
