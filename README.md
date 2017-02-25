# Parity-bit computation comparison.

A simple benchmark that compares serveral approaches to compute a parity bit.
Most of the approaches are taken from
http://www.graphics.stanford.edu/~seander/bithacks.html#ParityLookupTable
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
  
To run the benchmark, you'll need `emacs` to generate the random input sequence,
and `gcc` to compile the resulting source file. Once these two tools are in
place, just:

    ./run.sh

To see the results for you machine.

# Files

- `main.c` contains the test driver and the parity computation implementations to
compare 
- `run.sh` a shell wrapper that runs the three steps (data generation, compilation
and execution). Use the three variables at the top of the file, to play with the
benchmark parameters.
- `generate-data.el` an Emacs lisp file that generates `data.hi` that contains a
  list of calls for parity computation on a number of randomly generated inputs, 
  to be included into `main.c`
