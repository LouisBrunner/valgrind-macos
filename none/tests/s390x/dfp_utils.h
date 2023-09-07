#ifndef DFP_UTILS_H
#define DFP_UTILS_H

#include <stddef.h>      /* size_t */
#include <stdio.h>       /* printf */

#define DFP_VAL_PRINT(op, type)                                         \
  {                                                                     \
    enum { n = sizeof(type) };                                          \
    union { type x; unsigned char i[n]; } u = { .x = op };              \
    for (int k = 0; k < n; k++)                                         \
       printf("%02x", u.i[k]);                                          \
  }

#define DFP_BINOP_PRINT(op1, op2, result, type, op, cc)                 \
  {                                                                     \
    DFP_VAL_PRINT(op1, type);                                           \
    printf(" "op" ");                                                   \
    DFP_VAL_PRINT(op2, type);                                           \
    printf(" = ");                                                      \
    DFP_VAL_PRINT(result, type);                                        \
    printf(" cc = %d\n", cc);                                           \
  }

#endif /* DFP_UTILS_H */
