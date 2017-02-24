#include <time.h>
#include <stdio.h>

static inline int parity_arith(unsigned long long x) {
  unsigned long long y;
  y = x ^ (x >> 1);
  y = y ^ (y >> 2);
  y = y ^ (y >> 4);
  y = y ^ (y >> 8);
  y = y ^ (y >> 16);
  y = y ^ (y >> 32);
  return y & 1;
}

int parities[65536];

static inline int parity_mem(unsigned long long x) {
  unsigned short* parts = (unsigned short*)(&x);
  return 1 & (parities[parts[0]] ^
              parities[parts[1]] ^
              parities[parts[2]] ^
              parities[parts[3]]);

  /*
  unsigned short a = 0xffff&x;
  unsigned short b = 0xffff&(x >> 16);
  unsigned short c = 0xffff&(x >> 32);
  unsigned short d = 0xffff&(x >> 48);
  return 1 & (parities[a] ^ parities[b] ^ parities[c] ^ parities[d]);
  */
}

void fill_parity_hash() {
  unsigned int i = 0;
  for (i = 0; i < 65536; ++i) {
    parities[i] = parity_arith(i);
  }
}

double sec_diff(struct timespec* begin, struct timespec* end) {
  return (double)(end->tv_sec - begin->tv_sec) + 
    (double)(1e-9 * (end->tv_nsec - begin->tv_nsec));
}

int main() {
  unsigned long long i;
  int p = 0;
  fill_parity_hash();

  struct timespec resolution;
  struct timespec start;
  struct timespec after_mem;
  struct timespec after_arith;

  clock_getres(CLOCK_REALTIME, &resolution);
  clock_gettime(CLOCK_REALTIME, &start);

#define COMPUTE_PARITY(x) p ^= parity_mem(x)

  for (i = 0; i < NITERATIONS; ++i) {
#   include "data.hi"
  }


#undef COMPUTE_PARITY

  clock_gettime(CLOCK_REALTIME, &after_mem);

#define COMPUTE_PARITY(x) p ^= parity_arith(x)
  for (i = 0; i < NITERATIONS; ++i) {
#   include "data.hi"
  }
#undef COMPUTE_PARITY

  clock_gettime(CLOCK_REALTIME, &after_arith);

  printf("%d iterations; llu size: %lu; su size: %lu\n",
         NITERATIONS,
         sizeof(unsigned long long),
         sizeof(unsigned short));
  printf("memory parity calculation: %fs\n",
         sec_diff(&start, &after_mem));
  printf("arithmetic parity calculation: %fs\n",
         sec_diff(&after_mem, &after_arith));
  if (p) {
    printf("odd (incorrect)\n");
  }
  else {
    printf("even (correct)\n");
  }
}
