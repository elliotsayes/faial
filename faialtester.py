#!/usr/bin/env python3

import sys
import argparse
import subprocess
import time
import psutil


ARGUMENTS =[
    "--time-as-csv",
]

def main(arg):
    parser = argparse.ArgumentParser(description="Frontend script to run Faial on CAV2014 dataset.")
    cmds=["check-drf",]
    cmds.append(arg[0])
    start=time.time()
    subprocess.call(cmds,stdout=subprocess.DEVNULL)
    end=time.time()
    #virtual_memory=psutil.virtual_memory()
    #disk_usage=psutil.disk_usage('/')

    #print("Total time: {}\nMemory used: {}\nDisk used: {}\
    #    ".format(end-start, virtual_memory, disk_usage ))

if __name__ == '__main__':
  sys.exit(main(sys.argv[1:]))
