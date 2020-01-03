#!/usr/bin/env python3

import sys
import argparse
import subprocess
import time
import psutil
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import os

# Given a map of kernels, appends each kernel with a new time value.
def csv_to_time(this_map,csv_filename):
    df = pd.read_csv(csv_filename,usecols=['kernel','total'])
    for idx,row in df.iterrows():
        kernel_name = row['kernel']
        kernel_time = row['total']
        if kernel_name not in this_map:
            this_map[kernel_name] = []
        else:
            this_map[kernel_name].append(kernel_time)

def create_barChart(this_dict,plot_name):
    #Initialize data into arrays
    kernels = []
    kernel_mean = []
    kernel_std = []

    for key in this_dict:
        time_array = np.array(this_dict[key])
        kernels.append(key.replace("/home/dennis/Work/gpuverify-cav2014experiments/",""))
        kernel_mean = np.mean(time_array)
        kernel_std = np.std(time_array)

    x_pos = np.arange(len(kernels))

    #Build pyplot
    fig, ax = plt.subplots()
    ax.bar(x_pos, kernel_mean, yerr=kernel_std, align='center', alpha=0.5, ecolor='black', capsize=10)
    ax.set_ylabel('Average Time Taken (s)')
    ax.set_xticks(x_pos)
    ax.set_xticklabels(kernels)
    ax.set_title('Time of verification of CUDA kernels')
    ax.yaxis.grid(True)

    # Save the figures
    plt.tight_layout()
    plt.savefig(plot_name + '.png')
    plt.close(fig)

def main(arg):

    parser = argparse.ArgumentParser(description='Record Time and Memory usage.')
    parser.add_argument('--from-file',type=str, required=True, \
        help='Input file containing CUDA kernel relative paths.')
    parser.add_argument('--gv-csv-file',type=str, required=False, default="GV_TimeMem.csv")
    parser.add_argument('--f-csv-file',type=str, required=False, default="Faial_TimeMem.csv")
    parser.add_argument('--runs',type=int,required=False,default=1)
    parser.add_argument('--timeout',type=int,required=False,default=300)

    args = parser.parse_args()
    files = args.from_file
    runs = args.runs
    timeout = "--gvopt=--timeout=" + str(args.timeout)
    silent = "--gvopt=--silent"

    # Calls the respective verification tools.
    GV_cmd = ["gvtester.py","--from-file",files,"--time-as-csv","--csv-file",args.gv_csv_file,"--threads","1",silent,timeout,"."]
    FAIAL_cmd = ["faialtester.py","--from-file",files,"--csv-file",args.f_csv_file]

    #Initialize time maps to store time.
    GPUVerify_time = {}
    Faial_time = {}

    for i in range(runs):
        subprocess.call(GV_cmd,stdout=subprocess.DEVNULL,stderr=subprocess.DEVNULL)
        csv_to_time(GPUVerify_time,args.gv_csv_file)
        os.remove(args.gv_csv_file)
        subprocess.call(FAIAL_cmd,stdout=subprocess.DEVNULL,stderr=subprocess.DEVNULL)
        csv_to_time(Faial_time,args.f_csv_file)
        os.remove(args.f_csv_file)

    create_barChart(GPUVerify_time,"GPUVerify time graph")
    create_barChart(Faial_time,"Faial time graph")

if __name__ == '__main__':
  main(sys.argv[1:])
