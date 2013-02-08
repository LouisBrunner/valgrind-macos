#include <stdio.h>
#include "opcodes.h"
#include "dfp_utils.h"
#define __STDC_WANT_DEC_FP__ 1
#include <float.h>

#define I2D(insn,  initial, target,round)                               \
  ({                                                                    \
    register int source asm("2") =  initial;                            \
    register typeof(target) _t asm("f0");                               \
    asm volatile(insn(round,0,0,2) :"=f" (_t):"d"(source));             \
    _t;                                                                 \
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

#define DO_D2I(round)                                                   \
  ({                                                                    \
    DO_INSN_D2I(CGDTRA,  round, _Decimal64);                            \
    DO_INSN_D2I(CGXTRA,  round, _Decimal128);                           \
  })


int main()
{
  /* rounding mode is not used for the I64 -> D128 conversion */
  DO_INSN_I2D(CXGTRA, 0, _Decimal128);

  /* Omit rounding mode value 0 and 2 as the current DFP rounding
     mode is chosen for these values. */
  DO_INSN_I2D(CDGTRA, 1, _Decimal64);
  DO_D2I(1);

  DO_INSN_I2D(CDGTRA, 3, _Decimal64);
  DO_D2I(3);

  DO_INSN_I2D(CDGTRA, 4, _Decimal64);
  DO_D2I(4);

  DO_INSN_I2D(CDGTRA, 5, _Decimal64);
  DO_D2I(5);

  DO_INSN_I2D(CDGTRA, 6, _Decimal64);
  DO_D2I(6);

  DO_INSN_I2D(CDGTRA, 7, _Decimal64);
  DO_D2I(7);

  DO_INSN_I2D(CDGTRA, 8, _Decimal64);
  DO_D2I(8);

  DO_INSN_I2D(CDGTRA, 9, _Decimal64);
  DO_D2I(9);

  DO_INSN_I2D(CDGTRA, a, _Decimal64);
  DO_D2I(a);

  DO_INSN_I2D(CDGTRA, b, _Decimal64);
  DO_D2I(b);

  DO_INSN_I2D(CDGTRA, c, _Decimal64);
  DO_D2I(c);

  DO_INSN_I2D(CDGTRA, d, _Decimal64);
  DO_D2I(d);

  DO_INSN_I2D(CDGTRA, e, _Decimal64);
  DO_D2I(e);

  DO_INSN_I2D(CDGTRA, f, _Decimal64);
  DO_D2I(f);

  return 0;
}
