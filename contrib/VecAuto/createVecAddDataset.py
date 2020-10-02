
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
    parser.add_argument('--n',type=str, required=True, \
        help='N number to repeat parameter. e.g. N will create N nested loops.')

    args = parser.parse_args()
    n = args.n

    dataset = []

    for i in range(1,int(n)+1):
        vecAdd_cmd = ["./vecAdd-auto",str(i)]
        vecAdd_race_cmd = ["./vecAdd-auto-race",str(i)]

        process = subprocess.Popen(vecAdd_cmd,stdout=subprocess.PIPE,stderr=subprocess.PIPE,text=True)

        process = subprocess.Popen(vecAdd_race_cmd,stdout=subprocess.PIPE,stderr=subprocess.PIPE,text=True)

        dataset.append("testfeature/VecAuto/vectorAdd"+str(i)+"-auto.cu")
        dataset.append("testfeature/VecAuto/vectorAdd"+str(i)+"-auto-race.cu")

    outToText(dataset)

if __name__ == '__main__':
  main(sys.argv[1:])
