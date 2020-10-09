#!/usr/bin/env python3

import sys
import argparse
import subprocess
import time
import psutil
import csv
import os
import re

ARGUMENTS =[
    "--time-as-csv",
]

def get_time(kernel_dir):
    kernel_dir = os.getcwd() + "/" + kernel_dir[:-1]
    kernel_dir = kernel_dir.replace("/home/dennisliew/Work/faial/faial/","/home/dennisliew/Work/faial/gpuverify-cav2014experiments/")
    #print("FAIAL DIRECTORY: ",kernel_dir)
    faial_cmd = ["/home/dennisliew/Work/faial/faial/check-drf",kernel_dir]
    start=time.time()
    subprocess.Popen(faial_cmd)
    end=time.time()
    return (end - start)

def main(arg):
    parser = argparse.ArgumentParser(description="Frontend script to run Faial on CAV2014 dataset.")
    parser.add_argument('--from-file',type=str, required=True)
    parser.add_argument('--csv-file',type=str, default = "Faial_Time.csv")

    args = parser.parse_args()

    text_file = open(args.from_file, "r")

    with open(args.csv_file, "w+") as output_file:
        output_file = csv.writer(output_file, delimiter=',')
        output_file.writerow(['kernel','total'])
        for line in text_file:
            current_time = get_time(line)
            output_file.writerow([line[:-1],current_time])

    text_file.close()


    #virtual_memory=psutil.virtual_memory()
    #disk_usage=psutil.disk_usage('/')

    #print("Total time: {}\nMemory used: {}\nDisk used: {}\
    #    ".format(end-start, virtual_memory, disk_usage ))




if __name__ == '__main__':
  sys.exit(main(sys.argv[1:]))
