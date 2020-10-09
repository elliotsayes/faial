
#!/usr/bin/env python3

import sys
import argparse
import subprocess
import os

def outToText(dataset):
    with open("../../vecAdd.txt", "w+") as txtfile:
        for line in dataset:
            txtfile.write(line + '\n')

def main(arg):

    parser = argparse.ArgumentParser(description='Uses VecAdd to create 1 to N dataset. IMPORTANT:\nThis script must be called in the directory that you want the dataset to be created in (same directory as the compiled vecAdd).')
    parser.add_argument('--n',type=int, required=True, \
        help='N number to repeat parameter. e.g. N will create N nested loops.')
    parser.add_argument('--l',type=str, required=False, default="n", \
        help='L number of iterations per loop. Default set to variable n.')

    args = parser.parse_args()
    n = args.n
    loops = args.l

    dataset = []

    for i in range(1,n+1):
        vecAdd_NE_cmd = ["./vecAdd",str(i),"NE",loops]
        vecAdd_SE_cmd = ["./vecAdd",str(i),"SE",loops]

        process = subprocess.Popen(vecAdd_NE_cmd,stdout=subprocess.PIPE,stderr=subprocess.PIPE,text=True)

        process = subprocess.Popen(vecAdd_SE_cmd,stdout=subprocess.PIPE,stderr=subprocess.PIPE,text=True)

        dataset.append("testfeature/VecAuto/vectorAdd"+str(i)+".cu")
        dataset.append("testfeature/VecAuto/vectorAdd"+str(i)+"-race.cu")

    outToText(dataset)

if __name__ == '__main__':
  main(sys.argv[1:])
