A simple benchmark that compares two approaches to compute a parity bit. The
approaches are:
* Pure arithmetic computation (`parity_arith`), using a logarithmic formula. It computes the
  parity bit for a 64 bit number in 6 steps, using 13 arithmetic operations:
  xor, left_shift and bit_and.
* Lookup computation. it breaks 64-bit number into 4 16-bit registers, retreives
  precomputed parity bits for each and sums them.
  
  
To run the benchmark, you'll need `emacs` to generate the random input sequence,
and `gcc` to compile the resulting source file. After these two tools are in
place, just run:

    ./run.sh

To see the results for you machine.
