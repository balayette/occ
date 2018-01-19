#!/bin/bash

make && ./compiler.exe $1 && gcc output.s -o output
./output
echo $?
