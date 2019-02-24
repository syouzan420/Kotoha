#!/bin/bash

echo "*** Kotoha Inline Coder *** 2019/2/24 yokoP"

while :
  do
    read  -p "> " line
    if [ "${line}" = "exit" ]; then
      break
    fi
    ./Conv "${line}" | cat | while read tx; do echo ${tx:1:-2}; ghc -e "${tx:1:-2}"; done
  done

  exit 0
  

