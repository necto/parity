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
  /* unsigned short* parts = (unsigned short*)(&x); */
  /* return 1 & (parities[parts[0]] ^ */
  /*             parities[parts[1]] ^ */
  /*             parities[parts[2]] ^ */
  /*             parities[parts[3]]); */

  /* unsigned short a = 0xffff&x; */
  /* unsigned short b = 0xffff&(x >> 16); */
  /* unsigned short c = 0xffff&(x >> 32); */
  /* unsigned short d = 0xffff&(x >> 48); */
  /* return 1 & (parities[a] ^ parities[b] ^ parities[c] ^ parities[d]); */

  return
    parities[0xffff&x] ^
    parities[0xffff&(x >> 16)] ^
    parities[0xffff&(x >> 32)] ^
    parities[0xffff&(x >> 48)];
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

void fill_parity_table() {
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
  fill_parity_table();

  struct timespec resolution;
  struct timespec start;
  struct timespec after_naive;
  struct timespec after_mem;
  struct timespec after_arith;

  clock_getres(CLOCK_REALTIME, &resolution);
  clock_gettime(CLOCK_REALTIME, &start);

#define COMPUTE_PARITY(x) p ^= parity_naive(x)

  for (i = 0; i < NITERATIONS; ++i) {
#   include "data.hi"
  }


#undef COMPUTE_PARITY

  clock_gettime(CLOCK_REALTIME, &after_naive);

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
  printf("naive parity calculation: %lfs\n",
         sec_diff(&start, &after_naive));
  printf("memory parity calculation: %lfs\n",
         sec_diff(&after_naive, &after_mem));
  printf("arithmetic parity calculation: %lfs\n",
         sec_diff(&after_mem, &after_arith));
  if (p) {
    printf("odd (incorrect)\n");
  }
  else {
    printf("even (correct)\n");
  }
}
