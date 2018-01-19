#!/bin/bash

make && ./compiler.exe src/examples/fibo.c && gcc output.s -o output
./output
echo $?
