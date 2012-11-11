#include <stdio.h>
#include "opcodes.h"

volatile _Decimal64 d64_1, d64_2;

#define DFP64_BINOP(insn, op1, op2, type, round, cc)                     \
({                                                                       \
  register type f1 asm("f1") =  op1;                                     \
  register type f2 asm("f2") =  op2;                                     \
  /* f1 = f1 (op) f2    */                                               \
  asm volatile(insn(2,round,1,1)                                         \
                "ipm %1\n\t"                                             \
               "srl %1,28\n\t"                                           \
               :"+f" (f1), "=d" (cc)                                     \
               :"f"(f2)                                                  \
               );                                                        \
  f1;                                                                    \
})

int main() {
  _Decimal64 result;
  int cc;

  printf("Decimal floating point 64-bit arithmetic\n");
  // fixs390: print result in DFP format once required insns are supported.

    /* 64-bit ADD */
  /* case 1: result has maximum significand digits */
  d64_1 = 999999999.0DD;
  d64_2 = 0.999999DD;
  result = DFP64_BINOP(ADTRA, d64_1, d64_2, _Decimal64, 1, cc);
  printf("dfp64_add: %lx\n", *((unsigned long *) &result));
  /* case 2: result is rounded */
  d64_1 = 99999999999.0DD;
  d64_2 = 0.999999DD;
  result = DFP64_BINOP(ADTRA, d64_1, d64_2, _Decimal64, 3, cc);
  printf("dfp64_add: %lx\n", *((unsigned long *) &result));

    /* 64-bit SUBTRACT */
  /* case 1: result has maximum significand digits */
  d64_1 = 0.000001DD;
  d64_2 = 10000000000.0DD;
  result = DFP64_BINOP(SDTRA, d64_1, d64_2, _Decimal64, 4, cc);
  printf("dfp64_sub: %lx\n", *((unsigned long *) &result));
  /* case 2: result is rounded */
  d64_1 = 0.000001DD;
  d64_2 = 100000000000.0DD;
  result = DFP64_BINOP(SDTRA, d64_1, d64_2, _Decimal64, 5, cc);
  printf("dfp64_sub: %lx\n", *((unsigned long *) &result));

    /* 64-bit MULTIPLY */
  /* case 1: result has maximum significand digits */
  d64_1 = 9999999999.999999DD;
  d64_2 = .99DD;
  result = DFP64_BINOP(MDTRA, d64_1, d64_2, _Decimal64, 6, cc);
  printf("dfp64_mul: %lx\n", *((unsigned long *) &result));
  /* case 2: result is rounded */
  d64_1 = 99999999999.999999DD;
  d64_2 = .99DD;
  result = DFP64_BINOP(MDTRA, d64_1, d64_2, _Decimal64, 7, cc);
  printf("dfp64_mul: %lx\n", *((unsigned long *) &result));

    /* 64-bit DIVIDE */
  /* case 1: result has maximum significand digits */
  d64_1 = 8888888888.888877DD;
  d64_2 = 0.999999DD;
  result = DFP64_BINOP(DDTRA, d64_1, d64_2, _Decimal64, d, cc);
  printf("dfp64_div: %lx\n", *((unsigned long *) &result));
  /* case 2: result is rounded */
  d64_1 = 88888888888.888877DD;
  d64_2 = 0.000003DD;
  result = DFP64_BINOP(DDTRA, d64_1, d64_2, _Decimal64, e, cc);
  printf("dfp64_div: %lx\n", *((unsigned long *) &result));
}
