#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>
#include "opcodes.h"
#include "rounding.h"

/* Test "convert to fixed"  with rounding mode given in insn (m3 field)
   Covers all generally available rounding modes that can be mapped to
   IRRoundingMode. As a consequence m3=1 which is "round to nearest with
   ties away from 0" is not tested here.
*/

const char *
rtext(unsigned m3_round)
{
   switch (m3_round) {
   case 0: return "[-> per fpc]";
   case 1: return "[-> nearest away]";
   case 3: return "[-> prepare short]";   // floating point extension fac needed
   case 4: return "[-> nearest even]";
   case 5: return "[-> 0]";
   case 6: return "[-> +inf]";
   case 7: return "[-> -inf]";
   }
   assert(0);
}

#define convert_to_int(opcode,src_type,dst_type,dst_fmt,round,value) \
do { \
   src_type src = value; \
   dst_type dst;         \
   unsigned cc;          \
                         \
   __asm__ volatile (opcode " %[dst]," #round ",%[src]\n\t"     \
                     "ipm %[cc]\n\t"                  \
                     "srl %[cc],28\n\t"               \
                     : [dst] "=d"(dst), [cc] "=d"(cc) \
                     : [src] "f"(src)                 \
                     : "cc");                         \
                                                      \
   printf("%s %f\t-> %"dst_fmt"\tcc = %u  %s\n",    \
          opcode, src, dst, cc, rtext(round));        \
} while (0)


#define cfebr(value, round) \
        convert_to_int("cfebr",float,int32_t,PRId32,round,value)
#define cfdbr(value, round) \
        convert_to_int("cfdbr",double,int32_t,PRId32,round,value)
#define cgebr(value, round) \
        convert_to_int("cgebr",float,int64_t,PRId64,round,value)
#define cgdbr(value, round) \
        convert_to_int("cgdbr",double,int64_t,PRId64,round,value)

void
set_rounding_mode(unsigned mode)
{
   register unsigned r asm("1") = mode;
   __asm__ volatile ( SFPC(1) : : "d"(r) );
}


int main(void)
{
   int j;
   static const float fval[] = {
      1.4f, 1.5f, 2.5f, 1.6f, -1.4f, -1.5f, -2.5f, -1.6f, 0.0f,
   };
   static const double dval[] = {
      1.4, 1.5, 2.5, 1.6, -1.4, -1.5, -2.5, -1.6, 0.0,
   };

   /* Note when testing M3_NEAR need to set the FPC rounding mode
      to something else. FPC rounding mode is NEAR by default.
      Setting the FPC rounding mode to != NEAR is the only way to make
      sure the M3 field is not ignored. */

   /* f32 -> i32 */
   for (j = 0; j < sizeof fval / sizeof fval[0]; ++j) {
      set_rounding_mode(FPC_BFP_ROUND_ZERO);
      cfebr(fval[j], M3_BFP_ROUND_NEAREST_EVEN);
      set_rounding_mode(FPC_BFP_ROUND_NEAREST_EVEN);
      cfebr(fval[j], M3_BFP_ROUND_ZERO);
      cfebr(fval[j], M3_BFP_ROUND_POSINF);
      cfebr(fval[j], M3_BFP_ROUND_NEGINF);
   }

   /* f32 -> i64 */
   for (j = 0; j < sizeof fval / sizeof fval[0]; ++j) {
      set_rounding_mode(FPC_BFP_ROUND_ZERO);
      cgebr(fval[j], M3_BFP_ROUND_NEAREST_EVEN);
      set_rounding_mode(FPC_BFP_ROUND_NEAREST_EVEN);
      cgebr(fval[j], M3_BFP_ROUND_ZERO);
      cgebr(fval[j], M3_BFP_ROUND_POSINF);
      cgebr(fval[j], M3_BFP_ROUND_NEGINF);
   }

   /* f64 -> i32 */
   for (j = 0; j < sizeof dval / sizeof dval[0]; ++j) {
      set_rounding_mode(FPC_BFP_ROUND_ZERO);
      cfdbr(dval[j], M3_BFP_ROUND_NEAREST_EVEN);
      set_rounding_mode(FPC_BFP_ROUND_NEAREST_EVEN);
      cfdbr(dval[j], M3_BFP_ROUND_ZERO);
      cfdbr(dval[j], M3_BFP_ROUND_POSINF);
      cfdbr(dval[j], M3_BFP_ROUND_NEGINF);
   }

   /* f64 -> i64 */
   for (j = 0; j < sizeof dval / sizeof dval[0]; ++j) {
      set_rounding_mode(FPC_BFP_ROUND_ZERO);
      cgdbr(dval[j], M3_BFP_ROUND_NEAREST_EVEN);
      set_rounding_mode(FPC_BFP_ROUND_NEAREST_EVEN);
      cgdbr(dval[j], M3_BFP_ROUND_ZERO);
      cgdbr(dval[j], M3_BFP_ROUND_POSINF);
      cgdbr(dval[j], M3_BFP_ROUND_NEGINF);
   }

   return 0;
}
