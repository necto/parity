# Parity-bit computation comparison.

A simple benchmark that compares serveral approaches to compute a parity bit.
Most of the approaches are taken from
http://www.graphics.stanford.edu/~seander/bithacks.html
and somewhat modified:
* Naive(`parity_naive`): extract each bit individually (with shift and mask) and
  xor them
* Loop-based(`parity_loop`): iterate over the 64 positions and count the set
  bits. Execution time is proportional to the number of bits set. The presence
  of a conditional branch makes it the slowest algorithm on x86.
* Two arithmetic formulas:
  * Logarithmic arithmetic computation (`parity_arith_shift_xor`), using a logarithmic
    formula. It computes the parity bit for a 64 bit number in 6 steps, using 13
    arithmetic operations: xor, left_shift and bit_and.
  * Multiplication formula (`parity_arith_mul`), uses just 8 arithmetic operations, but one of them
    is multiplication
* 2 variants of a lookup computation. it breaks 64-bit number into 4 16-bit
  registers, retreives precomputed parity bits for each and sums them.
  * with shift decomposition (`parity_mem_shift`) - extracts 16-bit words using
    shift and masks
  * with type case decomposition (`parity_mem_cast`) - extracts the 16-bit words
    using static type cast of the 64-bit integer into an array of 16-bit integers.
    
## Try it yourself!

To run the benchmark, you'll need `emacs` to generate the random input sequence,
and `gcc` to compile the resulting source file. Once these two tools are in
place, just:

    ./run.sh

To see the results for you machine. The benchmark expects a 64-bit system.

## Files

- `main.c` contains the test driver and the parity computation implementations to
compare 
- `run.sh` a shell wrapper that runs the three steps (data generation, compilation
and execution). Use the three variables at the top of the file, to play with the
benchmark parameters.
- `generate-data.el` an Emacs lisp file that generates `data.hi` that contains a
  list of calls for parity computation on a number of randomly generated inputs, 
  to be included into `main.c`
  
## Output
Here is an example output of the benchmark on and Intel(R) Core(TM) i7-6600U CPU
system running at 3.4 GHz:

```
$ ./run.sh 
[run.sh] Running the benchmark for      100000 generated numbers,
         100000 iteration,
         with -o0 optimization level
[run.sh] Generating input data ...

real	0m0.474s
user	0m0.368s
sys	0m0.044s

[run.sh] Compiling the benchmark ...

real	2m7.871s
user	2m1.828s
sys	0m5.884s

[run.sh] Running the benchmark ...
Testing 6 parity bit computation implementations with:
100000 iterations;
unsigned long long size: 8 (must be 8);
unsigned short size: 2 (must be 2);
Clock resolution: 0.000000

Check the equivalence of the benchmarked algorithms...
All computatins agree.
Running the benchmark. This may take long time...

naive                                   : 205.437784 s
loop                                    : 815.794358 s
lookup with arith decomposition         : 49.138156 s
lookup with type cast decomposition     : 57.031758 s
arithmetic with shift&xor formula       : 61.058782 s
arithmetic with multiplication formula  : 56.664941 s

the grand total (sum of all parities for all methods and numbers): even

real	20m45.142s
user	20m44.448s
sys	0m0.072s
```

Do you know a faster algorithm? Did you spot a measurement mistake? Pull
requests are welcome!
