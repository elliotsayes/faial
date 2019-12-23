#!/usr/bin/env python3

import sys
import argparse
import subprocess
import time
import psutil

# Input kernel for verification.
def main(arg):

    parser = argparse.ArgumentParser(description='Record Time and Memory usage.')
    parser.add_argument('--input-file',type=str, required=True, \
        help='Input file containing CUDA kernel relative paths.')
    args = parser.parse_args()
    files = args.input_file


    GV_cmd = ["gvtester.py","--from-file",files,"--time-as-csv","--csv-file",,"."]

    FAIAL_cmd = ["faialtester.py",files]

    subprocess.call(GV_cmd)
    #subprocess.call(FAIAL_cmd)

if __name__ == '__main__':
  main(sys.argv[1:])
