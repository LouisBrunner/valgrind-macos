#include <float.h>
#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>
#include <limits.h>

/* The following opcodes are tested:

   Convert to fixed:    cfebr, cgebr, cfdbr, cgdbr
   Convert from fixed:  cefbr, cdfbr, cegbr, cdgbr

   We do not test rounding here. Just making sure the insn selector
   picks the correct insn.
*/

#define I2F(insn, initial, target_type)                         \
do {                                                            \
   int64_t source = initial;                                    \
   target_type target;                                          \
   asm volatile(insn " %0,%1\n\t" :"=f" (target) :"d"(source)); \
   printf(insn " %"PRId64" -> %f\n", source, target);           \
} while (0)

#define DO_INSN_I32_TO_F(insn, target_type)        \
do {                                               \
   printf("\n----- int32_t -> " #target_type "\n");\
   I2F(insn,   0, target_type);                    \
   I2F(insn,   1, target_type);                    \
   I2F(insn,  -1, target_type);                    \
   I2F(insn,  42, target_type);                    \
   I2F(insn, SHRT_MAX, target_type);               \
   I2F(insn, SHRT_MIN, target_type);               \
   I2F(insn, INT_MAX, target_type);                \
   I2F(insn, INT_MIN, target_type);                \
} while (0)

#define DO_INSN_I64_TO_F(insn, target_type)        \
do {                                               \
   printf("\n----- int64_t -> " #target_type "\n");\
   I2F(insn,   0, target_type);                    \
   I2F(insn,   1, target_type);                    \
   I2F(insn,  -1, target_type);                    \
   I2F(insn,  42, target_type);                    \
   I2F(insn, SHRT_MAX, target_type);               \
   I2F(insn, SHRT_MIN, target_type);               \
   I2F(insn, INT_MAX, target_type);                \
   I2F(insn, INT_MIN, target_type);                \
   I2F(insn, LONG_MAX, target_type);               \
   I2F(insn, LONG_MIN, target_type);               \
} while (0)

#define DO_I2F()                        \
do {                                    \
   DO_INSN_I32_TO_F("cefbr", float);    \
   DO_INSN_I32_TO_F("cdfbr", double);   \
   DO_INSN_I64_TO_F("cegbr", float);    \
   DO_INSN_I64_TO_F("cdgbr", double);   \
} while (0)


#define F2I(insn, initial, source_type, target_type)               \
do {                                                               \
   int cc;                                                         \
   source_type source = initial;                                   \
   target_type target = 0;                                         \
   asm volatile(insn " %0,0,%2\n\t"                                \
                "ipm %1\n\t"                                       \
                "srl %1,28\n\t"                                    \
 		: "=d" (target), "=d" (cc) : "f"(source) : "cc");  \
   printf(insn " %f -> %ld   cc = %d\n", source, (long)target, cc); \
} while (0)

#define DO_INSN_F32_TO_I(insn, type)          \
do {                                          \
   printf("\n----- float -> " #type "\n");    \
   F2I(insn, -1.0f, float, type);             \
   F2I(insn,  0.0f, float, type);             \
   F2I(insn,  1.0f, float, type);             \
   F2I(insn, 1.4f, float, type);              \
   F2I(insn, 1.5f, float, type);              \
   F2I(insn, 1.6f, float, type);              \
   F2I(insn, 1.6E+4f, float, type);           \
   F2I(insn, 1.6E+8f, float, type);           \
   F2I(insn, 1.6E-4f, float, type);           \
   F2I(insn, FLT_MAX, float, type);           \
} while (0)

#define DO_INSN_F64_TO_I(insn, type)          \
do {                                          \
   printf("\n----- double -> " #type "\n");   \
   F2I(insn, -1.0, double, type);             \
   F2I(insn,  0.0, double, type);             \
   F2I(insn,  1.0, double, type);             \
   F2I(insn, 1.4, double, type);              \
   F2I(insn, 1.5, double, type);              \
   F2I(insn, 1.6, double, type);              \
   F2I(insn, 1.6E+4, double, type);           \
   F2I(insn, 1.6E+8, double, type);           \
   F2I(insn, 1.6E-4, double, type);           \
   F2I(insn, FLT_MAX, double, type);          \
   F2I(insn, DBL_MAX, double, type);          \
} while (0)

#define DO_F2I()                        \
do {                                    \
   DO_INSN_F32_TO_I("cfebr", int32_t);  \
   DO_INSN_F32_TO_I("cgebr", int64_t);  \
   DO_INSN_F64_TO_I("cfdbr", int32_t);  \
   DO_INSN_F64_TO_I("cgdbr", int64_t);  \
} while (0)


int main()
{
   DO_I2F();
   DO_F2I();

   return 0;
}
