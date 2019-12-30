#!/usr/bin/env python3

import sys
import argparse
import subprocess
import time
import psutil
import csv


ARGUMENTS =[
    "--time-as-csv",
]

def get_time(kernel_dir):
    faial_cmd = ["check-drf",kernel_dir[1:-1]]
    #print(faial_cmd)
    start=time.time()
    subprocess.call(faial_cmd,stdout=subprocess.DEVNULL)
    end=time.time()
    return (end - start)

def main(arg):
    parser = argparse.ArgumentParser(description="Frontend script to run Faial on CAV2014 dataset.")
    parser.add_argument('--from-file',type=str, required=True)
    args = parser.parse_args()

    text_file = open(args.from_file, "r")

    with open("Faial_TimeMem.csv", "w+") as output_file:
        output_file = csv.writer(output_file, delimiter=',')
        for line in text_file:
            current_time = get_time(line)
            output_file.writerow([line,current_time])

    text_file.close()


    #virtual_memory=psutil.virtual_memory()
    #disk_usage=psutil.disk_usage('/')

    #print("Total time: {}\nMemory used: {}\nDisk used: {}\
    #    ".format(end-start, virtual_memory, disk_usage ))




if __name__ == '__main__':
  sys.exit(main(sys.argv[1:]))
