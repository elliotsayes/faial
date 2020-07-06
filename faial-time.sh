#!/bin/bash

FILE="$1"
TIME_LOG="time.log"
shift

# Setup temporary file
TMP1=$(tempfile)
TMP2=$(tempfile)
TMP3=$(tempfile)

cleanup(){
  rm -f "${TMP2}" "${TMP1}" "${TMP3}" ;
}

trap "{ cleanup; }" EXIT

if ! time cu-to-json "$FILE" --extra-arg=-ferror-limit=1 > "${TMP1}" 2> "${TMP2}"
then
  >&2 echo "ERROR: cu-to-json $FILE"
  echo "-------------------------------------------------------------------" >> "${TIME_LOG}";
  cat "${TMP2}" >> "$TIME_LOG"
  cleanup
  exit 1
fi

if ! time faial-infer "${TMP1}" -t json > "${TMP2}" 2> "${TMP3}"; then
  >&2 echo "ERROR: faial-infer $FILE"
  echo "-------------------------------------------------------------------" >> "${TIME_LOG}";
  echo "ERROR: $FILE" >> "${TIME_LOG}";
  cat "${TMP3}" >> "${TIME_LOG}";
  cleanup;
  exit 2
fi

if ! time main --json "${TMP2}" $(faial-gv "$FILE") > "${TMP1}"; then
  >&2 echo "ERROR: faial $FILE"
  echo "-------------------------------------------------------------------" >> "${TIME_LOG}";
  echo "ERROR: $FILE" >> "${TIME_LOG}";
  cat "${TMP1}" >> "${TIME_LOG}";
  cleanup
  exit 3
fi

cleanup