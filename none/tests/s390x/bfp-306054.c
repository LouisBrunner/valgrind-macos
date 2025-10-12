#include <stdlib.h>
#include <assert.h>
#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>

/* This testcase is to illustrate that for convert to fixed the condition
   code depends on the rounding mode. */

const char *
rtext(unsigned round)
{
   switch (round) {
   case 0: return "[fpc]";
   case 1: return "[->near/away]";
      /* 2 is invalid */
   case 3: return "[prep short]";
   case 4: return "[->near/even]";
   case 5: return "[->0]";
   case 6: return "[->+inf]";
   case 7: return "[->-inf]";
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
   printf("%s %-20s %f\t-> %"dst_fmt"\tcc = %u\n",    \
          opcode, rtext(round), src, dst, cc);        \
} while (0)


#define cfdbr(round,value) \
        convert_to_int("cfdbr",double,int32_t,PRId32,round,value)

int main(void)
{
   double dval;

   dval = -2147483648.5;  // a < MN

   // f64 -> i32

   cfdbr(4, dval);  // round to nearest with ties to even
   cfdbr(5, dval);  // round to zero
   cfdbr(6, dval);  // round to +inf

   /* The next invocation needs to give cc=3. It used to give cc=1 when
      we were considering the to-be-converted value ONLY */
   cfdbr(7, dval);  // round to -inf

   return 0;
}
