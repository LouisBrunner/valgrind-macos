#include <stdio.h>
#include "opcodes.h"
#include "dfp_utils.h"
#define __STDC_WANT_DEC_FP__ 1
#include <float.h>

#define L2D(insn,  initial, target,round)                               \
  ({                                                                    \
    register unsigned long source asm("2") =  initial;                  \
    register typeof(target) _t asm("f0");                               \
    asm volatile(insn(round,0,0,2) :"=f" (_t):"d"(source));             \
    _t;                                                                 \
  })

#define I2D(insn,  initial, target,round)                               \
  ({                                                                    \
    register int source asm("2") =  initial;                            \
    register typeof(target) _t asm("f0");                               \
    asm volatile(insn(round,0,0,2) :"=f" (_t):"d"(source));             \
    _t;                                                                 \
})

#define D2L(insn, initial, type, round, cc)                             \
  ({                                                                    \
    register type source asm("f0") =  initial;                          \
    register unsigned long target asm ("2") = 0;                        \
    asm volatile(insn(round,0,2,0)                                      \
                 "ipm %1\n\t"                                           \
                 "srl %1,28\n\t"                                        \
                 :"=d" (target), "=d" (cc) :"f"(source):"cc");          \
    target;                                                             \
  })

#define D2I(insn, initial, type, round, cc)                             \
  ({                                                                    \
    register type source asm("f0") =  initial;                          \
    register int target asm ("2") = 0;                                  \
    asm volatile(insn(round,0,2,0)                                      \
                 "ipm %1\n\t"                                           \
                 "srl %1,28\n\t"                                        \
                 :"=d" (target), "=d" (cc) :"f"(source):"cc");          \
    target;                                                             \
})


#define DO_PRINT_L2D(insn, l, d, round)                                 \
  ({                                                                    \
    printf(#insn " round=%d %lu -> ", 0x##round, l);                    \
    d = L2D(insn, l, d, round);                                         \
    DFP_VAL_PRINT(d, typeof(d));                                        \
    printf("\n");                                                       \
  })

#define DO_INSN_L2D(insn, round, type)                                  \
  ({                                                                    \
    type d;                                                             \
    DO_PRINT_L2D(insn, 0UL, d, round);                                  \
    DO_PRINT_L2D(insn, 1UL, d, round);                                  \
    DO_PRINT_L2D(insn, 0xffffffffUL, d, round);                         \
    DO_PRINT_L2D(insn, 0x80000000UL, d, round);                         \
    DO_PRINT_L2D(insn, 0x7fffffffUL, d, round);                         \
    DO_PRINT_L2D(insn, 0x100000000UL, d, round);                        \
    DO_PRINT_L2D(insn, 0xffffffffffffffffUL, d, round);                 \
    DO_PRINT_L2D(insn, 0x8000000000000000UL, d, round);                 \
    DO_PRINT_L2D(insn, 0x7fffffffffffffffUL, d, round);                 \
  })

#define DO_PRINT_I2D(insn, l, d, round)                                 \
  ({                                                                    \
    printf(#insn " round=%d %d -> ", 0x##round, l);                     \
    d = I2D(insn, l, d, round);                                         \
    DFP_VAL_PRINT(d, typeof(d));                                        \
    printf("\n");                                                       \
  })

#define DO_INSN_I2D(insn, round, type)                                  \
  ({                                                                    \
    type d;                                                             \
    DO_PRINT_I2D(insn, 0, d, round);                                    \
    DO_PRINT_I2D(insn, 1, d, round);                                    \
    DO_PRINT_I2D(insn, 0xffffffff, d, round);                           \
    DO_PRINT_I2D(insn, 0x80000000, d, round);                           \
    DO_PRINT_I2D(insn, 0x7fffffff, d, round);                           \
   })

#define DO_PRINT_D2L(insn, d, type, round, cc)                          \
  ({                                                                    \
    printf(#insn " round=%d ", 0x##round);                              \
    DFP_VAL_PRINT(d, type);                                             \
    printf(" -> %lu ", D2L(insn, d, type, round, cc));                  \
    printf("cc=%d\n", cc);                                              \
  })

#define DO_INSN_D2L(insn, round, type)                                  \
  ({                                                                    \
    int cc;                                                             \
    type d;                                                             \
    d = -1.1DD;                                                         \
    DO_PRINT_D2L(insn, d, type, round, cc);                             \
    d = 0.DD;                                                           \
    DO_PRINT_D2L(insn, d, type, round, cc);                             \
    d = 1.DD;                                                           \
    DO_PRINT_D2L(insn, d, type, round, cc);                             \
    d = 1.4DD;                                                          \
    DO_PRINT_D2L(insn, d, type, round, cc);                             \
    d = 1.5DD;                                                          \
    DO_PRINT_D2L(insn, d, type, round, cc);                             \
    d = 1.6DD;                                                          \
    DO_PRINT_D2L(insn, d, type, round, cc);                             \
    d = 1.6E+4DD;                                                       \
    DO_PRINT_D2L(insn, d, type, round, cc);                             \
    d = 1.6E+8DD;                                                       \
    DO_PRINT_D2L(insn, d, type, round, cc);                             \
    d = 1.6E+4DD;                                                       \
    DO_PRINT_D2L(insn, d, type, round, cc);                             \
    d = 1.6E+12DD;                                                      \
    DO_PRINT_D2L(insn, d, type, round, cc);                             \
    d = 1.6E+20DD;                                                      \
    DO_PRINT_D2L(insn, d, type, round, cc);                             \
    d = 1.6E+200DD;                                                     \
    DO_PRINT_D2L(insn, d, type, round, cc);                             \
    d = 1.6E-4DD;                                                       \
    DO_PRINT_D2L(insn, d, type, round, cc);                             \
    d = DEC32_MIN;                                                      \
    DO_PRINT_D2L(insn, d, type, round, cc);                             \
    d = DEC32_MAX;                                                      \
    DO_PRINT_D2L(insn, d, type, round, cc);                             \
    d = DEC64_MIN;                                                      \
    DO_PRINT_D2L(insn, d, type, round, cc);                             \
    d = DEC64_MAX;                                                      \
    DO_PRINT_D2L(insn, d, type, round, cc);                             \
  })

#define DO_PRINT_D2I(insn, d, type, round, cc)                          \
  ({                                                                    \
    printf(#insn " round=%d ", 0x##round);                              \
    DFP_VAL_PRINT(d, type);                                             \
    printf(" -> %d ", D2I(insn, d, type, round, cc));                   \
    printf("cc=%d\n", cc);                                              \
  })

#define DO_INSN_D2I(insn, round, type)                                  \
  ({                                                                    \
    int cc;                                                             \
    type d;                                                             \
    d = -1.1DD;                                                         \
    DO_PRINT_D2I(insn, d, type, round, cc);                             \
    d = 0.DD;                                                           \
    DO_PRINT_D2I(insn, d, type, round, cc);                             \
    d = 1.DD;                                                           \
    DO_PRINT_D2I(insn, d, type, round, cc);                             \
    d = 1.4DD;                                                          \
    DO_PRINT_D2I(insn, d, type, round, cc);                             \
    d = 1.5DD;                                                          \
    DO_PRINT_D2I(insn, d, type, round, cc);                             \
    d = 1.6DD;                                                          \
    DO_PRINT_D2I(insn, d, type, round, cc);                             \
    d = 1.6E+4DD;                                                       \
    DO_PRINT_D2I(insn, d, type, round, cc);                             \
    d = 1.6E+8DD;                                                       \
    DO_PRINT_D2I(insn, d, type, round, cc);                             \
    d = 1.6E+4DD;                                                       \
    DO_PRINT_D2I(insn, d, type, round, cc);                             \
    d = 1.6E+12DD;                                                      \
    DO_PRINT_D2I(insn, d, type, round, cc);                             \
    d = 1.6E+20DD;                                                      \
    DO_PRINT_D2I(insn, d, type, round, cc);                             \
    d = 1.6E+200DD;                                                     \
    DO_PRINT_D2I(insn, d, type, round, cc);                             \
    d = 1.6E-4DD;                                                       \
    DO_PRINT_D2I(insn, d, type, round, cc);                             \
    d = DEC32_MIN;                                                      \
    DO_PRINT_D2I(insn, d, type, round, cc);                             \
    d = DEC32_MAX;                                                      \
    DO_PRINT_D2I(insn, d, type, round, cc);                             \
    d = DEC64_MIN;                                                      \
    DO_PRINT_D2I(insn, d, type, round, cc);                             \
    d = DEC64_MAX;                                                      \
    DO_PRINT_D2I(insn, d, type, round, cc);                             \
  })

#define DO_D2L(round)                                                   \
  ({                                                                    \
    DO_INSN_D2L(CLFDTR, round, _Decimal64);                             \
    DO_INSN_D2L(CLGDTR, round, _Decimal64);                             \
    DO_INSN_D2I(CFDTR,  round, _Decimal64);                             \
    DO_INSN_D2L(CLFXTR, round, _Decimal128);                            \
    DO_INSN_D2L(CLGXTR, round, _Decimal128);                            \
    DO_INSN_D2I(CFXTR,  round, _Decimal128);                            \
  })


int main()
{
  /* rounding mode is not used for the following insns */
  DO_INSN_I2D(CDFTR,  0, _Decimal64);
  DO_INSN_I2D(CXFTR,  0, _Decimal128);
  DO_INSN_L2D(CDLFTR, 0, _Decimal64);
  DO_INSN_L2D(CXLFTR, 0, _Decimal128);
  DO_INSN_L2D(CXLGTR, 0, _Decimal128);

  /* Omit rounding mode value 0 and 2 as the current DFP rounding
     mode is chosen for these values. */
  DO_INSN_L2D(CDLGTR, 1, _Decimal64);
  DO_D2L(1);

  DO_INSN_L2D(CDLGTR, 3, _Decimal64);
  DO_D2L(3);

  DO_INSN_L2D(CDLGTR, 4, _Decimal64);
  DO_D2L(4);

  DO_INSN_L2D(CDLGTR, 5, _Decimal64);
  DO_D2L(5);

  DO_INSN_L2D(CDLGTR, 6, _Decimal64);
  DO_D2L(6);

  DO_INSN_L2D(CDLGTR, 7, _Decimal64);
  DO_D2L(7);

  DO_INSN_L2D(CDLGTR, 8, _Decimal64);
  DO_D2L(8);

  DO_INSN_L2D(CDLGTR, 9, _Decimal64);
  DO_D2L(9);

  DO_INSN_L2D(CDLGTR, a, _Decimal64);
  DO_D2L(a);

  DO_INSN_L2D(CDLGTR, b, _Decimal64);
  DO_D2L(b);

  DO_INSN_L2D(CDLGTR, c, _Decimal64);
  DO_D2L(c);

  DO_INSN_L2D(CDLGTR, d, _Decimal64);
  DO_D2L(d);

  DO_INSN_L2D(CDLGTR, e, _Decimal64);
  DO_D2L(e);

  DO_INSN_L2D(CDLGTR, f, _Decimal64);
  DO_D2L(f);

  return 0;
}
