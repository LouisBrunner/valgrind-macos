#ifndef DFP_UTILS_H
#define DFP_UTILS_H

#include <stddef.h>      /* size_t */
#include <stdio.h>       /* printf */

typedef float reg_d32;
typedef double reg_d64;
typedef long double reg_d128;
typedef union { unsigned int i; reg_d32 f; } pun_d32;
typedef union { unsigned long i; reg_d64 f; } pun_d64;
typedef union { unsigned long i[2]; reg_d128 f; } pun_d128;

#define DFP_VAL_PRINT(op, type)                                         \
  {                                                                     \
    enum { n = sizeof(type) };                                          \
    union { type x; unsigned char i[n]; } u = { .x = op };              \
    for (int k = 0; k < n; k++)                                         \
       printf("%02x", u.i[k]);                                          \
  }

#endif /* DFP_UTILS_H */
