#!/bin/bash


FILES="$1"

while IFS= read -r cur_file 
do
	
	echo "$cur_file"
	time cu-to-json "$cur_file" | faial-infer - -t json | main --json /dev/stdin $(faial-gv "$cur_file") 
done < "$FILES"
