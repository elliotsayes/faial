#!/bin/bash
params=("$@")
arg=0
steps=2

# Parse the number of times to run faial-gen, if given.
while getopts "s:" opt; do
    case ${opt} in
    s)
        steps=$OPTARG

        if [[ ! $steps =~ ^[0-9]+$ ]]; then
            echo "$steps is not a positive integer"
            exit 1
        elif [[ $steps -lt 2 ]]; then
            echo "steps must be greater than or equal to 2"
            exit 2
        fi
        ;;
    ?)
        exit 3
        ;;
    esac

    arg=$((arg + 2))
done

# Use temporary files to contain the generated kernels.
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" &>/dev/null && pwd)"
tmp1=$(mktemp /tmp/kernel1.XXXXXX)
tmp2=$(mktemp /tmp/kernel2.XXXXXX)
input_file=${params[$arg]}

# Generate the first kernel.
"${SCRIPT_DIR}/faial-gen" $input_file >"$tmp1"

case $? in
0) ;;
124) # If the input file was not found, exit immediately.
    exit 124 ;;
*) # For all other errors, exit with an error message.
    echo "$input_file could not be parsed by faial-gen (1)."
    exit 4
    ;;
esac

# Generate the second kernel.
if ! "${SCRIPT_DIR}/faial-gen" "$tmp1" >"$tmp2"; then
    echo "$input_file could not be parsed by faial-gen (2)."
    exit 5
# A kernel reaches a fixed point if both generated kernels have the same code.
elif cmp -s "$tmp1" "$tmp2"; then
    echo "$input_file reaches a fixed point."
    exit 0
else
    # Tests whether an additional faial-gen run
    # will cause the kernel to reach a fixed point.
    fixpoint() {
        if ! "${SCRIPT_DIR}/faial-gen" "$1" >"$2"; then
            echo "$input_file could not be parsed by faial-gen ($4)."
            echo "Printing previous diff."
            echo "$input_file does not reach a fixed point:"
            diff -u "$3" "$2"
            exit 6
        elif cmp -s "$1" "$2"; then
            echo "$input_file reaches a fixed point ($4)."
            exit 0
        elif [ $4 -eq $steps ]; then
            echo "$input_file does not reach a fixed point:"
            diff -u "$1" "$2"
            exit 7
        fi
    }

    # If a steps parameter was supplied, continue generating kernels until
    # a fixed point is reached, an error occurs, or all steps are exhausted.
    for ((step = 3; step <= steps; step++)); do
        case $(($step % 3)) in
        0)
            tmp3=$(mktemp /tmp/kernel$step.XXXXXX)
            fixpoint $tmp2 $tmp3 $tmp1 $step
            ;;
        1)
            tmp1=$(mktemp /tmp/kernel$step.XXXXXX)
            fixpoint $tmp3 $tmp1 $tmp2 $step
            ;;
        2)
            tmp2=$(mktemp /tmp/kernel$step.XXXXXX)
            fixpoint $tmp1 $tmp2 $tmp3 $step
            ;;
        esac
    done

    # Otherwise, print the diff between the generated kernels.
    echo "$input_file does not reach a fixed point:"
    diff -u "$tmp1" "$tmp2"
    exit 8
fi
