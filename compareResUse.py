#!/usr/bin/env python3

import sys
import argparse
import subprocess
import time
import matplotlib.pyplot as plt
import psutil
import pandas as pd
import numpy as np
import os
import re
import csv
from scipy import stats
from scipy.optimize import curve_fit

# Given a map of kernels, appends each kernel with a new time value.
def csv_to_time(this_map,csv_filename):
    df = pd.read_csv(csv_filename,usecols=['kernel','total'])
    for idx,row in df.iterrows():
        kernel_name = row['kernel'].replace("/home/dennisliew/Work/faial/gpuverify-cav2014experiments/","")
        kernel_time = row['total']
        if kernel_name not in this_map:
            this_map[kernel_name] = []
        else:
            this_map[kernel_name].append(kernel_time)

def fitfunc(x, a, b, c ):
    return a * np.power(b, x) + c


def create_time_barChart(faial_dict,gpuverify_dict,runs):
    #Initialize data into arrays
    kernels = []
    kernel_mean = []
    kernel_std = []

    for key in faial_dict:
        time_array = np.array(faial_dict[key])
        g_time_array = np.array(gpuverify_dict[key])
        kernels.append(key.replace("~/Work/faial/gpuverify-cav2014experiments/",""))
        if runs > 1:
            kernel_std += [np.std(time_array), np.std(g_time_array)]
            kernel_mean += [np.mean(time_array), np.mean(g_time_array)]
        else:
            kernel_std += [0,0]
            kernel_mean += [time_array, g_time_array]

    x_pos = np.arange(1,(len(kernels)/2)+1)
    width = 0.35  # the width of the bars
    ##x_color = list(map(lambda x: 'blue' if x%2==0 else 'red', x_pos))

    ###############
    kernel_mean1,kernel_mean2,kernel_std1,kernel_std2 = kernel_mean[::4],kernel_mean[1::4],kernel_std[::4],kernel_std[1::4]
    slope1, intercept1, r_value1, p_value1, std_err1 = stats.linregress(x_pos,kernel_mean1)
    slope2, intercept2, r_value2, p_value2, std_err2 = stats.linregress(x_pos,kernel_mean2)
    popt1, pcov1 = curve_fit(fitfunc, x_pos, kernel_mean1, bounds=(0, [100, 10, 20]))
    popt2, pcov2 = curve_fit(fitfunc, x_pos, kernel_mean2, bounds=(0, [100, 10, 20]))
    print(popt1,popt2)
    #Build pyplot
    fig, ax = plt.subplots()
    ##ax.bar(x_pos, kernel_mean, yerr=kernel_std, align='center', alpha=0.5, ecolor='black', capsize=10, color=x_color)
    rects1 = ax.errorbar(x_pos, kernel_mean1, marker='x', yerr=kernel_std1,alpha=0.5,label='Faial',color='blue', ecolor='black', capsize=10)
    rects2 = ax.errorbar(x_pos, kernel_mean2, marker='o', yerr=kernel_std2,alpha=0.5,label='GPUVerify',color='red', ecolor='black', capsize=10)
    ax.set_ylabel('Average Time Taken (s)')
    ax.set_xticks(x_pos)
    ax.legend(loc='upper left')
    #ax.legend(('','','curve-fit-F = {}'.format(popt1),'curve-fit-GV = {}'.format(popt2)),loc='upper left')
    ##ax.set_xticklabels(kernels) #filenames were too long.
    ax.set_title('Time of verification of CUDA kernels for Faial,GPUVerify Data Race Kernels')
    ax.yaxis.grid(True)

    # Save the figures
    plt.tight_layout()
    plt.savefig('Faial_GPUVerify_time_DR.png')
    plt.close(fig)
    ###############

    ###############
    kernel_mean1,kernel_mean2,kernel_std1,kernel_std2 = kernel_mean[2::4],kernel_mean[3::4],kernel_std[2::4],kernel_std[3::4]
    slope1, intercept1, r_value1, p_value1, std_err1 = stats.linregress(x_pos,kernel_mean1)
    slope2, intercept2, r_value2, p_value2, std_err2 = stats.linregress(x_pos,kernel_mean2)
    popt1, pcov1 = curve_fit(fitfunc, x_pos, kernel_mean1, bounds=(0, [100, 10, 20]))
    popt2, pcov2 = curve_fit(fitfunc, x_pos, kernel_mean2, bounds=(0, [100, 10, 20]))
    print(popt1,popt2)
    #Build pyplot
    fig, ax = plt.subplots()
    ##ax.bar(x_pos, kernel_mean, yerr=kernel_std, align='center', alpha=0.5, ecolor='black', capsize=10, color=x_color)
    rects1 = ax.errorbar(x_pos, kernel_mean1, marker='x', yerr=kernel_std1,alpha=0.5,label='Faial',color='blue', ecolor='black', capsize=10)
    rects2 = ax.errorbar(x_pos, kernel_mean2, marker='o', yerr=kernel_std2,alpha=0.5,label='GPUVerify',color='red', ecolor='black', capsize=10)
    ax.set_ylabel('Average Time Taken (s)')
    ax.set_xticks(x_pos)
    ax.legend(loc='upper left')
    #ax.legend(('','','curve-fit-F = {}'.format(popt1),'curve-fit-GV = {}'.format(popt2)),loc='upper left')
    ##ax.set_xticklabels(kernels) #filenames were too long.
    ax.set_title('Time of verification of CUDA kernels for Faial,GPUVerify Data Race Free Kernels')
    ax.yaxis.grid(True)

    # Save the figures
    plt.tight_layout()
    plt.savefig('Faial_GPUVerify_time_DRF.png')
    plt.close(fig)
    ###############

def populate_memory_arr(mem_info):
    m = len(mem_info)
    n = 16
    return [mem_info[i*n:(i*n+n)] for i in range(m//n)]

def output_mem_file(mem_arr,this_dict):
    i=0
    for key in this_dict:
        mem_arr[i].insert(0,str(key))
        i+=1

    with open("Faial_mem.csv", "w+") as output_file:
        output_file = csv.writer(output_file, delimiter=',')
        output_file.writerow(['kernel','added-eqs','arith-assert-lower',\
        'arith-assert-upper','arith-eq-adapter','arith-pivots','del-clause',\
        'final-checks','max-memory','memory','mk-bool-var','mk-clause',\
        'num-allocs','num-checks','propagations','rlimit-count','total-time'])
        for line in mem_arr:
            output_file.writerow(line)


def main(arg):

    parser = argparse.ArgumentParser(description='Record Time and Memory usage.')
    parser.add_argument('--from-file',type=str, required=True, \
        help='Input file containing CUDA kernel relative paths.')
    parser.add_argument('--type',type=int, required=False,help='Number of different kernel types to test for (e.g. nested loops data-race-free, non-nested loops data-race). ',default=1)
    parser.add_argument('--gv-csv-file',type=str, required=False, default="GV_Time.csv")
    parser.add_argument('--f-csv-file',type=str, required=False, default="Faial_Time.csv")
    parser.add_argument('--runs',type=int,required=False,default=1)
    parser.add_argument('--timeout',type=int,required=False,default=300)

    args = parser.parse_args()
    files = args.from_file
    runs = args.runs
    timeout = "--gvopt=--timeout=" + str(args.timeout)
    silent = "--gvopt=--silent"
    k_type = args.type

    # Calls the respective verification tools.
    GV_cmd = ["/home/dennisliew/Work/faial/gpuverify/gvtester.py","--from-file",files,\
        "--time-as-csv","--csv-file",args.gv_csv_file,"--threads","4",silent,timeout,"../gpuverify-cav2014experiments/"]
    FAIAL_cmd = ["/home/dennisliew/Work/faial/faial/faialtester.py","--from-file",files,"--csv-file",args.f_csv_file]

    #Initialize time maps to store time.
    GPUVerify_time = {}
    Faial_time = {}
    mem_info = []

    print("Comparing Faial to GPUVerify...")

    for i in range(runs):
        process = subprocess.check_call(GV_cmd,stdout=subprocess.PIPE,stderr=subprocess.PIPE)
        #stdout,err = process.communicate()
        csv_to_time(GPUVerify_time,args.gv_csv_file)
        #os.remove(args.gv_csv_file)
        process = subprocess.check_call(FAIAL_cmd,stdout=subprocess.PIPE,stderr=subprocess.PIPE,text=True)
        #stdout,err = process.communicate()
        #output = re.findall(r":added-eqs.*\n|:arith-assert-lower.*\n|:arith-assert-upper.*\n|:arith-eq-adapter.*\n|:arith-pivots.*\n|:del-clause.*\n|:final-checks.*\n|:max-memory.*\n|:memory.*\n|:mk-bool-var.*\n|:mk-clause.*\n|:num-allocs.*\n|:num-checks.*\n|:propagations.*\n|:rlimit-count.*\n|:total-time.*\n",str(stdout))
        csv_to_time(Faial_time,args.f_csv_file)
        #os.remove(args.f_csv_file)

    #for ii in output:
    #    x = re.search("\d+.*\d*",ii).group().replace(")","")
    #    mem_info.append(x)

    # Creates csv file from memory data of z3 -st option
    ##Faial_mem = populate_memory_arr(mem_info)
    ##output_mem_file(Faial_mem,Faial_time)

    # Creates plots of total time of GPUVerify & Faial.
    create_time_barChart(Faial_time,GPUVerify_time,runs)

if __name__ == '__main__':
  main(sys.argv[1:])
