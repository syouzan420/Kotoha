#!/bin/bash

echo "*** Kotoha Inline Coder *** 2019/2/24 yokoP"

while :
  do
    read  -p "> " line
    if [ "${line}" = "exit" -o "${line}" = "q" ]; then
      break
    fi
    ./Conv "${line}" | cat | while read tx; do\
      echo ${tx:1:-2}; ghc -e "${tx:1:-2}" | cat |\
      while read rs; do rs="${rs:0:-1}"; rs=${rs// % //};\
      rs=${rs%/1}; rs=${rs///1]/]}; rs=${rs///1,/,}; rs=${rs//,/, };\
      echo ${rs}; done; done
  done

  exit 0
  

