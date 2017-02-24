#!/bin/bash

echo "Generating input data ..."
time emacs -q --batch --script generate-data.el
echo ""
echo "Compiling the benchmark ..."
time gcc -O3 main.c
echo ""
echo "Running the benchmark ..."
time ./a.out
