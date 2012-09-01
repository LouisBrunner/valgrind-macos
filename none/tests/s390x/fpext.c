#include <float.h>
#include <stdio.h>
#include "opcodes.h"


#define L2F(insn,  initial, target,round)                                \
({                                                                       \
   register unsigned long source asm("2") =  initial;                    \
   register typeof(target) _t asm("f0");                                 \
   asm volatile(insn(round,0,0,2) :"=f" (_t):"d"(source));               \
   _t;                                                                   \
})

#define F2L(insn, initial, type, round, cc)                              \
({                                                                       \
   register type source asm("f0") =  initial;                            \
   register unsigned long target asm ("2") = 0;                          \
   asm volatile(insn(round,0,2,0)                                        \
                "ipm %1\n\t"                                             \
                "srl %1,28\n\t"                                          \
 		:"=d" (target), "=d" (cc) :"f"(source):"cc");            \
   target;                                                               \
})


#define DO_INSN_L2F32(insn, round)                                       \
({                                                                       \
   float f32;                                                            \
   printf(#insn " %f\n", L2F(insn, 0, f32, round));                      \
   printf(#insn " %f\n", L2F(insn, 1, f32, round));                      \
   printf(#insn " %f\n", L2F(insn, 0xffffffffUL, f32, round));           \
   printf(#insn " %f\n", L2F(insn, 0x80000000UL, f32, round));           \
   printf(#insn " %f\n", L2F(insn, 0x7fffffffUL, f32, round));           \
   printf(#insn " %f\n", L2F(insn, 0x100000000UL, f32, round));          \
   printf(#insn " %f\n", L2F(insn, 0xffffffffffffffffUL, f32, round));   \
   printf(#insn " %f\n", L2F(insn, 0x8000000000000000UL, f32, round));   \
   printf(#insn " %f\n", L2F(insn, 0x7fffffffffffffffUL, f32, round));   \
})

#define DO_INSN_L2F64(insn, round)                                       \
({                                                                       \
   double f64;                                                           \
   printf(#insn " %f\n", L2F(insn, 0, f64, round));                      \
   printf(#insn " %f\n", L2F(insn, 1, f64, round));                      \
   printf(#insn " %f\n", L2F(insn, 0xffffffffUL, f64, round));           \
   printf(#insn " %f\n", L2F(insn, 0x80000000UL, f64, round));           \
   printf(#insn " %f\n", L2F(insn, 0x7fffffffUL, f64, round));           \
   printf(#insn " %f\n", L2F(insn, 0x100000000UL, f64, round));          \
   printf(#insn " %f\n", L2F(insn, 0xffffffffffffffffUL, f64, round));   \
   printf(#insn " %f\n", L2F(insn, 0x8000000000000000UL, f64, round));   \
   printf(#insn " %f\n", L2F(insn, 0x7fffffffffffffffUL, f64, round));   \
})

#define DO_INSN_L2F128(insn, round)                                      \
({                                                                       \
   long double f128;                                                     \
   printf(#insn " %Lf\n", L2F(insn, 0, f128, round));                    \
   printf(#insn " %Lf\n", L2F(insn, 1, f128, round));                    \
   printf(#insn " %Lf\n", L2F(insn, 0xffffffffUL, f128, round));         \
   printf(#insn " %Lf\n", L2F(insn, 0x80000000UL, f128, round));         \
   printf(#insn " %Lf\n", L2F(insn, 0x7fffffffUL, f128, round));         \
   printf(#insn " %Lf\n", L2F(insn, 0x100000000UL, f128, round));        \
   printf(#insn " %Lf\n", L2F(insn, 0xffffffffffffffffUL, f128, round)); \
   printf(#insn " %Lf\n", L2F(insn, 0x8000000000000000UL, f128, round)); \
   printf(#insn " %Lf\n", L2F(insn, 0x7fffffffffffffffUL, f128, round)); \
})

#define DO_INSN_F2L(insn, round, type)                                   \
({                                                                       \
   int cc;                                                               \
   printf(#insn " %lu ", F2L(insn, -1.1, type, round, cc));              \
   printf("cc=%d\n", cc);                                                \
   printf(#insn " %lu ", F2L(insn, 0, type, round, cc));                 \
   printf("cc=%d\n", cc);                                                \
   printf(#insn " %lu ", F2L(insn, 1, type, round, cc));                 \
   printf("cc=%d\n", cc);                                                \
   printf(#insn " %lu ", F2L(insn, 1.4, type, round, cc));               \
   printf("cc=%d\n", cc);                                                \
   printf(#insn " %lu ", F2L(insn, 1.5, type, round, cc));               \
   printf("cc=%d\n", cc);                                                \
   printf(#insn " %lu ", F2L(insn, 1.6, type, round, cc));               \
   printf("cc=%d\n", cc);                                                \
   printf(#insn " %lu ", F2L(insn, 1.6E+4, type, round, cc));            \
   printf("cc=%d\n", cc);                                                \
   printf(#insn " %lu ", F2L(insn, 1.6E+8, type, round, cc));            \
   printf("cc=%d\n", cc);                                                \
   printf(#insn " %lu ", F2L(insn, 1.6E+12, type, round, cc));           \
   printf("cc=%d\n", cc);                                                \
   printf(#insn " %lu ", F2L(insn, 1.6E+20, type, round, cc));           \
   printf("cc=%d\n", cc);                                                \
   printf(#insn " %lu ", F2L(insn, 1.6E+200, type, round, cc));          \
   printf("cc=%d\n", cc);                                                \
   printf(#insn " %lu ", F2L(insn, 1.6E+2000L, type, round, cc));        \
   printf("cc=%d\n", cc);                                                \
   printf(#insn " %lu ", F2L(insn, 1.6E-4, type, round, cc));            \
   printf("cc=%d\n", cc);                                                \
   printf(#insn " %lu ", F2L(insn, FLT_MIN, type, round, cc));           \
   printf("cc=%d\n", cc);                                                \
   printf(#insn " %lu ", F2L(insn, FLT_MAX, type, round, cc));           \
   printf("cc=%d\n", cc);                                                \
   printf(#insn " %lu ", F2L(insn, DBL_MIN, type, round, cc));           \
   printf("cc=%d\n", cc);                                                \
   printf(#insn " %lu ", F2L(insn, DBL_MAX, type, round, cc));           \
   printf("cc=%d\n", cc);                                                \
   printf(#insn " %lu ", F2L(insn, LDBL_MIN, type, round, cc));          \
   printf("cc=%d\n", cc);                                                \
   printf(#insn " %lu ", F2L(insn, LDBL_MAX, type, round, cc));          \
   printf("cc=%d\n", cc);                                                \
})

#define DO_L2F(round)                                                    \
({                                                                       \
   DO_INSN_L2F32(CELFBR, round);                                         \
   DO_INSN_L2F32(CELGBR, round);                                         \
   DO_INSN_L2F64(CDLFBR, round);                                         \
   DO_INSN_L2F64(CDLGBR, round);                                         \
   DO_INSN_L2F128(CXLFBR, round);                                        \
   DO_INSN_L2F128(CXLGBR, round);                                        \
})

#define DO_F2L(round)                                                    \
({                                                                       \
   DO_INSN_F2L(CLFEBR, round, float);                                    \
   DO_INSN_F2L(CLGEBR, round, float);                                    \
   DO_INSN_F2L(CLFDBR, round, double);                                   \
   DO_INSN_F2L(CLGDBR, round, double);                                   \
   DO_INSN_F2L(CLFXBR, round, long double);                              \
   DO_INSN_F2L(CLGXBR, round, long double);                              \
})


int main()
{
   DO_L2F(4);
   DO_F2L(4);

   DO_L2F(5);
   DO_F2L(5);

   DO_L2F(6);
   DO_F2L(6);

   DO_L2F(7);
   DO_F2L(7);

   return 0;
}
