#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>
#include "opcodes.h"

/* Test "convert to fixed"  with "per fpc" rounding.
   Covers all generally available rounding modes.
*/

void
set_rounding_mode(unsigned mode)
{
   register unsigned r asm("1") = mode;
   __asm__ volatile ( SFPC(1) : : "d"(r) );
}

unsigned
get_rounding_mode(void)
{
   unsigned fpc;

   __asm__ volatile ("stfpc  %0" : "=Q"(fpc));

   return fpc & 0x7;
}


const char *
rtext(unsigned fpc_round)
{
   switch (fpc_round) {
   case 0: return "[-> near]";
   case 1: return "[-> zero]";
   case 2: return "[-> +inf]";
   case 3: return "[-> -inf]";
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
   printf("%s %f\t-> %"dst_fmt"\tcc = %u\n",    \
          opcode, src, dst, cc);        \
} while (0)


#define cfebr(value) \
        convert_to_int("cfebr",float,int32_t,PRId32,0,value)
#define cfdbr(value) \
        convert_to_int("cfdbr",double,int32_t,PRId32,0,value)
#define cgebr(value) \
        convert_to_int("cgebr",float,int64_t,PRId64,0,value)
#define cgdbr(value) \
        convert_to_int("cgdbr",double,int64_t,PRId64,0,value)

int main(void)
{
   int i, j;
   static const unsigned rmodes[] = { 0, 1, 2, 3 };
   static const float fval[] = {
      1.25f, 1.5f, 2.5f, 1.75f, -1.25f, -1.5f, -2.5f, -1.75f, 0.0f,
   };
   static const double dval[] = {
      1.25, 1.5, 2.5, 1.75, -1.25, -1.5, -2.5, -1.75, 0.0,
   };


   for (i = 0; i < sizeof rmodes / sizeof rmodes[0]; ++i) {
      printf("setting rounding mode to %s\n", rtext(rmodes[i]));
      set_rounding_mode(rmodes[i]);
      assert(get_rounding_mode() == rmodes[i]);

      /* f32 -> i32 */
      for (j = 0; j < sizeof fval / sizeof fval[0]; ++j) {
         cfebr(fval[j]);
         assert(get_rounding_mode() == rmodes[i]);
      }

      /* f32 -> i64 */
      for (j = 0; j < sizeof fval / sizeof fval[0]; ++j) {
         cgebr(fval[j]);
         assert(get_rounding_mode() == rmodes[i]);
      }

      /* f64 -> i32 */
      for (j = 0; j < sizeof dval / sizeof dval[0]; ++j) {
         cfdbr(dval[j]);
         assert(get_rounding_mode() == rmodes[i]);
      }

      /* f64 -> i64 */
      for (j = 0; j < sizeof dval / sizeof dval[0]; ++j) {
         cgdbr(dval[j]);
         assert(get_rounding_mode() == rmodes[i]);
      }

   }

   return 0;
}
