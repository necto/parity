#!/bin/bash
# Run the benchmark: generate a number of sample calls,
# compile and run the benchmark.
# You need
# * emacs to generate the input sequence and
# * gcc to compile the resulting source code.

NUMBER_OF_DIFFERENT_NUMBERS=100000
NUMBER_OF_ITERATIONS=100000
OPTIMIZATION_LEVEL=0

rm -f data.hi
echo "Generating input data ..."
time emacs -Q --batch --load generate-data.el \
     --eval "(datagen-process-file \"data.hi\" $NUMBER_OF_DIFFERENT_NUMBERS)"
echo ""
echo "Compiling the benchmark ..."
rm bench
time gcc -O$OPTIMIZATION_LEVEL \
     -DNITERATIONS=$NUMBER_OF_ITERATIONS \
     main.c -o bench
echo ""
echo "Running the benchmark ..."
time ./bench
