#include <stdio.h>
#include "opcodes.h"
#include "dfp_utils.h"

volatile _Decimal64 d64_1, d64_2, result_64;
volatile _Decimal128 d128_1, d128_2, result_128;

#define DFP_BINOP(insn, op1, op2, type, round, cc)                      \
  ({                                                                    \
    register type d1 asm("f0") =  op1;                                  \
    register type d2 asm("f1") =  op2;                                  \
    /* d1 = d1 (op) d2    */                                            \
    asm volatile(insn(1,round,0,0)                                      \
                 "ipm %1\n\t"                                           \
                 "srl %1,28\n\t"                                        \
                 :"+f" (d1), "=d" (cc)                                  \
                 :"f"(d2)                                               \
                 );                                                     \
    d1;                                                                 \
  })

int main() {
  int cc;

  printf("Decimal floating point arithmetic\n");

    /* 64-bit ADD */
  printf("64-bit ADD\n");
  /* case 1: cc = 2 */
  d64_1 = 3.14DD;
  d64_2 = 0.005DD;
  result_64 = DFP_BINOP(ADTRA, d64_1, d64_2, _Decimal64, 1, cc);
  DFP_BINOP_PRINT(d64_1, d64_2, result_64, _Decimal64, "+", cc);
  /* case 2: cc = 1 */
  d64_1 = -3.14DD;
  d64_2 = 0.005DD;
  result_64 = DFP_BINOP(ADTRA, d64_1, d64_2, _Decimal64, 1, cc);
  DFP_BINOP_PRINT(d64_1, d64_2, result_64, _Decimal64, "+", cc);
  /* case 2: cc = 0 */
  d64_1 = 3.14DD;
  d64_2 = -d64_1;
  result_64 = DFP_BINOP(ADTRA, d64_1, d64_2, _Decimal64, 3, cc);
  DFP_BINOP_PRINT(d64_1, d64_2, result_64, _Decimal64, "+", cc);


    /* 64-bit SUBTRACT */
  printf("64-bit SUBTRACT\n");
  /* case 1: cc = 2 */
  d64_1 = 3.14DD;
  d64_2 = 0.005DD;
  result_64 = DFP_BINOP(SDTRA, d64_1, d64_2, _Decimal64, 4, cc);
  DFP_BINOP_PRINT(d64_1, d64_2, result_64, _Decimal64, "-", cc);
  /* case 2: cc = 1 */
  d64_1 = -3.14DD;
  d64_2 = 0.005DD;
  result_64 = DFP_BINOP(SDTRA, d64_1, d64_2, _Decimal64, 5, cc);
  DFP_BINOP_PRINT(d64_1, d64_2, result_64, _Decimal64, "-", cc);
  /* case 3: cc = 0 */
  d64_1 = 3.14DD;
  d64_2 = d64_1;
  result_64 = DFP_BINOP(SDTRA, d64_1, d64_2, _Decimal64, 5, cc);
  DFP_BINOP_PRINT(d64_1, d64_2, result_64, _Decimal64, "-", cc);

    /* 64-bit MULTIPLY */
  printf("64-bit MULTIPLY\n");
  /* case 1: cc = 2 */
  d64_1 = 3.14DD;
  d64_2 = 7.DD;
  result_64 = DFP_BINOP(MDTRA, d64_1, d64_2, _Decimal64, 6, cc);
  DFP_BINOP_PRINT(d64_1, d64_2, result_64, _Decimal64, "*", cc);
  /* case 2: cc = 1 */
  d64_1 = -3.14DD;
  d64_2 = 7.DD;
  result_64 = DFP_BINOP(MDTRA, d64_1, d64_2, _Decimal64, 7, cc);
  DFP_BINOP_PRINT(d64_1, d64_2, result_64, _Decimal64, "*", cc);
  /* case 3: cc = 0 */
  d64_1 = -3.14DD;
  d64_2 = 0.DD;
  result_64 = DFP_BINOP(MDTRA, d64_1, d64_2, _Decimal64, 7, cc);
  DFP_BINOP_PRINT(d64_1, d64_2, result_64, _Decimal64, "*", cc);

    /* 64-bit DIVIDE */
  printf("64-bit DIVIDE\n");
  /* case 1: cc = 2 */
  d64_1 = 22.DD;
  d64_2 = 7.DD;
  result_64 = DFP_BINOP(DDTRA, d64_1, d64_2, _Decimal64, d, cc);
  DFP_BINOP_PRINT(d64_1, d64_2, result_64, _Decimal64, "/", cc);
  /* case 2: cc = 1 */
  d64_1 = -22.DD;
  d64_2 = 7.DD;
  result_64 = DFP_BINOP(DDTRA, d64_1, d64_2, _Decimal64, e, cc);
  DFP_BINOP_PRINT(d64_1, d64_2, result_64, _Decimal64, "/", cc);
  /* case 3: cc = 0 */
  d64_1 = 0.DD;
  d64_2 = 7.DD;
  result_64 = DFP_BINOP(DDTRA, d64_1, d64_2, _Decimal64, e, cc);
  DFP_BINOP_PRINT(d64_1, d64_2, result_64, _Decimal64, "/", cc);

    /* 128-bit ADD */
  printf("128-bit ADD\n");
  /* case 1: cc = 2 */
  d128_1 = 3.14DL;
  d128_2 = 0.005DL;
  result_128 = DFP_BINOP(AXTRA, d128_1, d128_2, _Decimal128, 1, cc);
  DFP_BINOP_PRINT(d128_1, d128_2, result_128, _Decimal128, "+", cc);
  /* case 2: cc = 1 */
  d128_1 = -3.14DL;
  d128_2 = 0.005DL;
  result_128 = DFP_BINOP(AXTRA, d128_1, d128_2, _Decimal128, 1, cc);
  DFP_BINOP_PRINT(d128_1, d128_2, result_128, _Decimal128, "+", cc);
  /* case 3: cc = 0 */
  d128_1 = 3.14DL;
  d128_2 = -d128_1;
  result_128 = DFP_BINOP(AXTRA, d128_1, d128_2, _Decimal128, 3, cc);
  DFP_BINOP_PRINT(d128_1, d128_2, result_128, _Decimal128, "+", cc);

    /* 128-bit SUBTRACT */
  printf("128-bit SUBTRACT\n");
  /* case 1: cc = 2 */
  d128_1 = 3.14DL;
  d128_2 = 0.005DL;
  result_128 = DFP_BINOP(SXTRA, d128_1, d128_2, _Decimal128, 4, cc);
  DFP_BINOP_PRINT(d128_1, d128_2, result_128, _Decimal128, "-", cc);
  /* case 2: cc = 1 */
  d128_1 = -3.14DL;
  d128_2 = 0.005DL;
  result_128 = DFP_BINOP(SXTRA, d128_1, d128_2, _Decimal128, 5, cc);
  DFP_BINOP_PRINT(d128_1, d128_2, result_128, _Decimal128, "-", cc);
  /* case 3: cc = 0 */
  d128_1 = 3.14DL;
  d128_2 = d128_1;
  result_128 = DFP_BINOP(SXTRA, d128_1, d128_2, _Decimal128, 5, cc);
  DFP_BINOP_PRINT(d128_1, d128_2, result_128, _Decimal128, "-", cc);

    /* 128-bit MULTIPLY */
  printf("128-bit MULTIPLY\n");
  /* case 1: cc = 2 */
  d128_1 = 3.14DL;
  d128_2 = 7.DL;
  result_128 = DFP_BINOP(MXTRA, d128_1, d128_2, _Decimal128, 6, cc);
  DFP_BINOP_PRINT(d128_1, d128_2, result_128, _Decimal128, "*", cc);
  /* case 2: cc = 1 */
  d128_1 = -3.14DL;
  d128_2 = 7.DL;
  result_128 = DFP_BINOP(MXTRA, d128_1, d128_2, _Decimal128, 7, cc);
  DFP_BINOP_PRINT(d128_1, d128_2, result_128, _Decimal128, "*", cc);
  /* case 3: cc = 0 */
  d128_1 = 3.14DL;
  d128_2 = 0.DL;
  result_128 = DFP_BINOP(MXTRA, d128_1, d128_2, _Decimal128, 7, cc);
  DFP_BINOP_PRINT(d128_1, d128_2, result_128, _Decimal128, "*", cc);

    /* 128-bit DIVIDE */
  printf("128-bit DIVIDE\n");
  /* case 1: cc = 2 */
  d128_1 = 22.DL;
  d128_2 = 7.DL;
  result_128 = DFP_BINOP(DXTRA, d128_1, d128_2, _Decimal128, d, cc);
  DFP_BINOP_PRINT(d128_1, d128_2, result_128, _Decimal128, "/", cc);
  /* case 2: cc = 1 */
  d128_1 = -22.DL;
  d128_2 = 7.DL;
  result_128 = DFP_BINOP(DXTRA, d128_1, d128_2, _Decimal128, e, cc);
  DFP_BINOP_PRINT(d128_1, d128_2, result_128, _Decimal128, "/", cc);
  /* case 3: cc = 0 */
  d128_1 = 0.DL;
  d128_2 = 7.DL;
  result_128 = DFP_BINOP(DXTRA, d128_1, d128_2, _Decimal128, e, cc);
  DFP_BINOP_PRINT(d128_1, d128_2, result_128, _Decimal128, "/", cc);

  return 0;
}
