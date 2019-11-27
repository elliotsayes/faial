#!/bin/bash


#FILES="$1"

#while IFS= read -r cur_file 
#do
#	
#	echo "$cur_file"
#	time cu-to-json "$cur_file" | faial-infer - -t json | main --json /dev/stdin $(faial-gv "$cur_file") 
#done < "$FILES"


#!/bin/bash

trap "exit 255" SIGINT SIGTERM

TIME_LOG="time.log"

if [ -f "${PARSE_LOG}" ]; then
  >&2 echo -e "Remove log file first:\n\n\trm -f '${TIME_LOG}'\n"
  exit 1
fi


while read fname
do
  case "${fname}" in
    *.cu)
      faial-time "${fname}" \
        && echo "OK: ${fname}"
      ;;
    *)
      >&2 echo "SKIP: ${fname}"
      ;;
  esac
done < "$1"
exit 0


