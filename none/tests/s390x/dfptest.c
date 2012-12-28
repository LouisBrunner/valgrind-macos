#include <math.h>
#include <stdio.h>

/* Following macros adopted from dfp/math.h from libdfp */
#define DEC_INFINITY    __builtin_infd64()
#define DEC_NAN         (0.0DF * DEC_INFINITY)

/* Following instructions are tested:
test data class tests for
   _Decimal32  - TDCET
   _Decimal64  - TDCDT
   _decimal128 - TDCXT
test data group tests for
   _Decimal32  - TDGET
   _Decimal64  - TDGDT
   _decimal128 - TDGXT
*/

#define TEST_128(opcode, d, n)                                  \
  ({                                                            \
    int match;                                                  \
    _Decimal128 f = d;                                          \
    long long num = n;                                          \
    asm volatile(opcode ", %1,0(%2)\n"                          \
                 "ipm %0\n"                                     \
                 "srl %0,28\n"                                  \
                 : "=d" (match) : "f" (f), "a" (num) : "cc");   \
    match;                                                      \
 })

#define TEST_64(opcode, d, n)                                   \
  ({                                                            \
    int match;                                                  \
    _Decimal64 f = d;                                           \
    long long num = n;                                          \
    asm volatile(opcode ", %1,0(%2)\n"                          \
                 "ipm %0\n"                                     \
                 "srl %0,28\n"                                  \
                 : "=d" (match) : "f" (f), "a" (num) : "cc");   \
    match;                                                      \
 })

#define TEST_32(opcode, d, n)                                   \
  ({                                                            \
    int match;                                                  \
    _Decimal32 f = d;                                           \
    long long num = n;                                          \
    asm volatile(opcode ", %1,0(%2)\n"                          \
                 "ipm %0\n"                                     \
                 "srl %0,28\n"                                  \
                 : "=d" (match) : "f" (f), "a" (num) : "cc");   \
    match;                                                      \
 })

int main()
{
	int i;

    /* The right most 12 bits 52:63 of the second operand are set and tested */
    for (i = 0; i < 12; i++) {
        /* DFP 128 bit - TDCXT */
        printf("%d", TEST_128(".insn rxe, 0xed0000000058", +0.0DF, 1UL<<i));
        printf("%d", TEST_128(".insn rxe, 0xed0000000058", -0.0DF, 1UL<<i));
        printf("%d", TEST_128(".insn rxe, 0xed0000000058", +2.2DF, 1UL<<i));
        printf("%d", TEST_128(".insn rxe, 0xed0000000058", -2.2DF, 1UL<<i));
        printf("%d", TEST_128(".insn rxe, 0xed0000000058",+DEC_INFINITY,1UL<<i));
        printf("%d", TEST_128(".insn rxe, 0xed0000000058",-DEC_INFINITY,1UL<<i));
        printf("%d", TEST_128(".insn rxe, 0xed0000000058", +DEC_NAN, 1UL<<i));
        printf("%d", TEST_128(".insn rxe, 0xed0000000058", -DEC_NAN, 1UL<<i));

        /* DFP 128 bit - TDGXT */
        printf("%d", TEST_128(".insn rxe, 0xed0000000059", +0.0DF, 1UL<<i));
        printf("%d", TEST_128(".insn rxe, 0xed0000000059", -0.0DF, 1UL<<i));
        printf("%d", TEST_128(".insn rxe, 0xed0000000059", +2.2DF, 1UL<<i));
        printf("%d", TEST_128(".insn rxe, 0xed0000000059", -2.2DF, 1UL<<i));
        printf("%d", TEST_128(".insn rxe, 0xed0000000059",+DEC_INFINITY,1UL<<i));
        printf("%d", TEST_128(".insn rxe, 0xed0000000059",-DEC_INFINITY,1UL<<i));
        printf("%d", TEST_128(".insn rxe, 0xed0000000059", +DEC_NAN, 1UL<<i));
        printf("%d", TEST_128(".insn rxe, 0xed0000000059", -DEC_NAN, 1UL<<i));

        /* DFP 64 bit - TDCDT */
        printf("%d", TEST_64(".insn rxe, 0xed0000000054", +0.0DF, 1UL<<i));
        printf("%d", TEST_64(".insn rxe, 0xed0000000054", -0.0DF, 1UL<<i));
        printf("%d", TEST_64(".insn rxe, 0xed0000000054", +2.2DF, 1UL<<i));
        printf("%d", TEST_64(".insn rxe, 0xed0000000054", -2.2DF, 1UL<<i));
        printf("%d", TEST_64(".insn rxe, 0xed0000000054",+DEC_INFINITY,1UL<<i));
        printf("%d", TEST_64(".insn rxe, 0xed0000000054",-DEC_INFINITY,1UL<<i));
        printf("%d", TEST_64(".insn rxe, 0xed0000000054", +DEC_NAN, 1UL<<i));
        printf("%d", TEST_64(".insn rxe, 0xed0000000054", -DEC_NAN, 1UL<<i));

        /* DFP 64 bit - TDGDT */
        printf("%d", TEST_64(".insn rxe, 0xed0000000055", +0.0DF, 1UL<<i));
        printf("%d", TEST_64(".insn rxe, 0xed0000000055", -0.0DF, 1UL<<i));
        printf("%d", TEST_64(".insn rxe, 0xed0000000055", +2.2DF, 1UL<<i));
        printf("%d", TEST_64(".insn rxe, 0xed0000000055", -2.2DF, 1UL<<i));
        printf("%d", TEST_64(".insn rxe, 0xed0000000055",+DEC_INFINITY,1UL<<i));
        printf("%d", TEST_64(".insn rxe, 0xed0000000055",-DEC_INFINITY,1UL<<i));
        printf("%d", TEST_64(".insn rxe, 0xed0000000055", +DEC_NAN, 1UL<<i));
        printf("%d", TEST_64(".insn rxe, 0xed0000000055", -DEC_NAN, 1UL<<i));

        /* DFP 32 bit - TDCET */
        printf("%d", TEST_32(".insn rxe, 0xed0000000050", +0.0DF, 1UL<<i));
        printf("%d", TEST_32(".insn rxe, 0xed0000000050", -0.0DF, 1UL<<i));
        printf("%d", TEST_32(".insn rxe, 0xed0000000050", +2.2DF, 1UL<<i));
        printf("%d", TEST_32(".insn rxe, 0xed0000000050", -2.2DF, 1UL<<i));
        printf("%d", TEST_32(".insn rxe, 0xed0000000050",+DEC_INFINITY,1UL<<i));
        printf("%d", TEST_32(".insn rxe, 0xed0000000050",-DEC_INFINITY,1UL<<i));
        printf("%d", TEST_32(".insn rxe, 0xed0000000050", +DEC_NAN, 1UL<<i));
        printf("%d", TEST_32(".insn rxe, 0xed0000000050", -DEC_NAN, 1UL<<i));

        /* DFP 32 bit - TDGET */
        printf("%d", TEST_32(".insn rxe, 0xed0000000051", +0.0DF, 1UL<<i));
        printf("%d", TEST_32(".insn rxe, 0xed0000000051", -0.0DF, 1UL<<i));
        printf("%d", TEST_32(".insn rxe, 0xed0000000051", +2.2DF, 1UL<<i));
        printf("%d", TEST_32(".insn rxe, 0xed0000000051", -2.2DF, 1UL<<i));
        printf("%d", TEST_32(".insn rxe, 0xed0000000051",+DEC_INFINITY,1UL<<i));
        printf("%d", TEST_32(".insn rxe, 0xed0000000051",-DEC_INFINITY,1UL<<i));
        printf("%d", TEST_32(".insn rxe, 0xed0000000051", +DEC_NAN, 1UL<<i));
        printf("%d", TEST_32(".insn rxe, 0xed0000000051", -DEC_NAN, 1UL<<i));

        printf("\n");

	}
	return 0;
}
