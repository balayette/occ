#!/bin/bash

make && ./compiler.exe src/examples/fibo.c && gcc -m32 output.s -o output
./output
echo $?
