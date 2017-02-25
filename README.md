A simple benchmark that compares serveral approaches to compute a parity bit.
Most of the approaches are taken from
http://www.graphics.stanford.edu/~seander/bithacks.html#ParityLookupTable
and somewhat modified:
* Naive
* Loop-based
* Pure arithmetic computation (`parity_arith`), using a logarithmic formula. It computes the
  parity bit for a 64 bit number in 6 steps, using 13 arithmetic operations:
  xor, left_shift and bit_and.
  * with shift and xor
  * with multiplication
* Lookup computation (`parity_mem`). it breaks 64-bit number into 4 16-bit registers, retreives
  precomputed parity bits for each and sums them.
  * with shift decomposition
  * with type case decomposition
  
To run the benchmark, you'll need `emacs` to generate the random input sequence,
and `gcc` to compile the resulting source file. Once these two tools are in
place, just run:

    ./run.sh

To see the results for you machine.

# Files

- `main.c` contains the test driver and the parity computation implementations to
compare 
- `run.sh` a shell wrapper that runs the three steps (data generation, compilation
and execution). Use the three variables at the top of the file, to play with the
benchmark parameters.
- `generate-data.el` an Emacs lisp file that generates `data.hi` that contains a
  list of calls for parity computation on a number of randomly generated numbers
  to be included into `main.c`
