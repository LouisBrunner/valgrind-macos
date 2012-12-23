#ifndef DFP_UTILS_H
#define DFP_UTILS_H

#include <stddef.h>      /* size_t */
#include <stdio.h>       /* printf */

/* convinience macros to print DFP values to avoid linking libdfp to
   DFP testcases */

#define DFP_VAL_PRINT(op, type)                                         \
  {                                                                     \
    size_t n = sizeof(type);                                            \
    if (n == 4)                                                         \
      printf("%x", *((unsigned int *) &op));                            \
    else if (n == 8)                                                    \
      printf("%lx", *((unsigned long *) &op));                          \
    else                                                                \
      printf("%lx%08lx", *((unsigned long *) &op),                      \
             *(((unsigned long *) &op) + 1));                           \
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
