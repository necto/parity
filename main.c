#include <time.h>
#include <stdio.h>
#include <limits.h>

/*
  A prepopulated array for parity value of each possible unsigned short value.
 */
int parities[USHRT_MAX];

static inline int parity_arith_shift_xor(unsigned long long x) {
  unsigned long long y;
  y = x ^ (x >> 1);
  y = y ^ (y >> 2);
  y = y ^ (y >> 4);
  y = y ^ (y >> 8);
  y = y ^ (y >> 16);
  y = y ^ (y >> 32);
  return y & 1;
}

static inline int parity_arith_mul(unsigned long long x) {
  x ^= x >> 1;
  x ^= x >> 2;
  x = (x & 0x1111111111111111UL) * 0x1111111111111111UL;
  return (x >> 60) & 1;
}


static inline int parity_mem_shift(unsigned long long x) {
  return parities[0xffff&(x ^ (x >> 16) ^ (x >> 32) ^ (x >> 48))];
}

static inline int parity_mem_cast(unsigned long long x) {
  unsigned short* parts = (unsigned short*)(&x);
  return parities[parts[0] ^ parts[1] ^ parts[2] ^ parts[3]];
}

static inline int parity_naive(unsigned long long x) {
  return
    0x1&x ^
    0x1&(x>>1) ^ 0x1&(x>>2) ^ 0x1&(x>>3) ^ 0x1&(x>>4) ^
    0x1&(x>>5) ^ 0x1&(x>>6) ^ 0x1&(x>>7) ^ 0x1&(x>>8) ^
    0x1&(x>>9) ^ 0x1&(x>>10) ^ 0x1&(x>>11) ^ 0x1&(x>>12) ^
    0x1&(x>>13) ^ 0x1&(x>>14) ^ 0x1&(x>>15) ^ 0x1&(x>>16) ^
    0x1&(x>>17) ^ 0x1&(x>>18) ^ 0x1&(x>>19) ^ 0x1&(x>>20) ^
    0x1&(x>>21) ^ 0x1&(x>>22) ^ 0x1&(x>>23) ^ 0x1&(x>>24) ^
    0x1&(x>>25) ^ 0x1&(x>>26) ^ 0x1&(x>>27) ^ 0x1&(x>>28) ^
    0x1&(x>>29) ^ 0x1&(x>>30) ^ 0x1&(x>>31) ^ 0x1&(x>>32) ^
    0x1&(x>>33) ^ 0x1&(x>>34) ^ 0x1&(x>>35) ^ 0x1&(x>>36) ^
    0x1&(x>>37) ^ 0x1&(x>>38) ^ 0x1&(x>>39) ^ 0x1&(x>>40) ^
    0x1&(x>>41) ^ 0x1&(x>>42) ^ 0x1&(x>>43) ^ 0x1&(x>>44) ^
    0x1&(x>>45) ^ 0x1&(x>>46) ^ 0x1&(x>>47) ^ 0x1&(x>>48) ^
    0x1&(x>>49) ^ 0x1&(x>>50) ^ 0x1&(x>>51) ^ 0x1&(x>>52) ^
    0x1&(x>>53) ^ 0x1&(x>>54) ^ 0x1&(x>>55) ^ 0x1&(x>>56) ^
    0x1&(x>>57) ^ 0x1&(x>>58) ^ 0x1&(x>>59) ^ 0x1&(x>>60) ^
    0x1&(x>>61) ^ 0x1&(x>>62) ^ 0x1&(x>>63);
}

static inline int parity_loop(unsigned long long x) {
  int parity = 0;
  while (x) {
    parity = !parity;
    x = x & (x - 1);
  }
  return parity;
}

void fill_parity_table() {
  unsigned int i = 0;
  for (i = 0; i < USHRT_MAX; ++i) {
    parities[i] = parity_arith_shift_xor(i);
  }
}

double sec_diff(struct timespec* begin, struct timespec* end) {
  return (double)(end->tv_sec - begin->tv_sec) +
    (double)(1e-9 * (end->tv_nsec - begin->tv_nsec));
}

void report_time_interval(const char* name,
                          struct timespec* begin,
                          struct timespec* end) {
  printf("%-40s: %f s\n", name, sec_diff(begin, end));
}

int check_parity_computations_equivalence() {
  int naive, loop, mem_shift, mem_cast, arith_shift_xor, arith_mul;
  printf("Check the equivalence of the benchmarked algorithms...\n");
#define COMPUTE_PARITY(x)                               \
  naive = parity_naive(x);                              \
  loop = parity_loop(x);                                \
  mem_shift = parity_mem_shift(x);                      \
  mem_cast = parity_mem_cast(x);                        \
  arith_shift_xor = parity_arith_shift_xor(x);          \
  arith_mul = parity_arith_mul(x);                      \
  if (naive != loop && naive != mem_shift &&            \
      naive != mem_cast && naive != arith_shift_xor &&  \
      naive != arith_mul) {                             \
    printf("Error! Parity computations disagree on the parity for %llu:\n" \
           "naive: %d\n"                                                \
           "loop: %d\n"                                                 \
           "lookup+shift: %d\n"                                         \
           "lookup+cast: %d\n"                                          \
           "arith+shift&xor: %d\n"                                      \
           "arith+mul: %d\n",                                           \
           x,                                                           \
           naive, loop,                                                 \
           mem_shift, mem_cast,                                         \
           arith_shift_xor, arith_mul);                                 \
    return 0;                                                           \
  }

# include "data.hi"
#undef COMPUTE_PARITY

  printf("All computatins agree.\n");
  return 1;
}

static inline int bench_function(int (* fun)(unsigned long long),
                                 struct timespec *end_time,
                                 int p) {
  unsigned long long i;
#define COMPUTE_PARITY(x) p ^= fun(x)
  for (i = 0; i < NITERATIONS; ++i) {
#   include "data.hi"
  }
#undef COMPUTE_PARITY

  clock_gettime(CLOCK_REALTIME, end_time);
  return p;
}

void run_benchmark() {
  int p = 0;
  struct timespec start;
  struct timespec after_naive;
  struct timespec after_loop;
  struct timespec after_mem_shift;
  struct timespec after_mem_cast;
  struct timespec after_arith_shift_xor;
  struct timespec after_arith_mul;

  clock_gettime(CLOCK_REALTIME, &start);

  p = bench_function(parity_naive, &after_naive, p);
  p = bench_function(parity_loop, &after_loop, p);
  p = bench_function(parity_mem_shift, &after_mem_shift, p);
  p = bench_function(parity_mem_cast, &after_mem_cast, p);
  p = bench_function(parity_arith_shift_xor, &after_arith_shift_xor, p);
  p = bench_function(parity_arith_mul, &after_arith_mul, p);

  report_time_interval("naive", &start, &after_naive);
  report_time_interval("loop", &after_naive, &after_loop);
  report_time_interval("lookup with arith decomposition",
                       &after_loop, &after_mem_shift);
  report_time_interval("lookup with type cast decomposition",
                       &after_mem_shift, &after_mem_cast);
  report_time_interval("arithmetic with shift&xor formula",
                       &after_mem_cast, &after_arith_shift_xor);
  report_time_interval("arithmetic with multiplication formula",
                       &after_arith_shift_xor, &after_arith_mul);

  if (p) {
    printf("\nthe grand total (sum of all parities for"
           " all methods and numbers): odd\n");
  } else {
    printf("\nthe grand total (sum of all parities for"
           " all methods and numbers): even\n");
  }
}

int main() {
  struct timespec resolution;
  printf("Testing 6 parity bit computation implementations with:\n"
         "%d iterations;\n"
         "unsigned long long size: %lu (must be 8);\n"
         "unsigned short size: %lu (must be 2);\n",
         NITERATIONS,
         sizeof(unsigned long long),
         sizeof(unsigned short));

  fill_parity_table();

  clock_getres(CLOCK_REALTIME, &resolution);
  printf("Clock resolution: %f\n\n",
         (double)(resolution.tv_sec) + (double)(1e-9*resolution.tv_nsec));

  if (!check_parity_computations_equivalence())
    return 1;

  run_benchmark();
  return 0;
}
