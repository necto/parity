#!/bin/bash
# Run the benchmark: generate a number of sample calls,
# compile and run the benchmark.
# You need
# * emacs to generate the input sequence and
# * gcc to compile the resulting source code.

NUMBER_OF_DIFFERENT_NUMBERS=100000
NUMBER_OF_ITERATIONS=1000
OPTIMIZATION_LEVEL=0

rm -f data.hi
echo "[run.sh] Running the benchmark for \
     $NUMBER_OF_DIFFERENT_NUMBERS generated numbers,"
echo "         $NUMBER_OF_ITERATIONS iteration,"
echo "         with -o$OPTIMIZATION_LEVEL optimization level"
echo "[run.sh] Generating input data ..."
time emacs -Q --batch --load generate-data.el \
     --eval "(datagen-process-file \
              \"data.hi\" \
              $NUMBER_OF_DIFFERENT_NUMBERS)"
echo ""
echo "[run.sh] Compiling the benchmark ..."
rm -f bench
time gcc -O$OPTIMIZATION_LEVEL \
     -DNITERATIONS=$NUMBER_OF_ITERATIONS \
     main.c -o bench
echo ""
echo "[run.sh] Running the benchmark ..."
time ./bench
