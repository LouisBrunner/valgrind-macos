/* https://bugs.kde.org/show_bug.cgi?id=308626 */

#include "../../memcheck.h"

#include <stdio.h>
#include <assert.h>

typedef unsigned int UInt;

/* Calculate ctz(x) using bsfl instruction. */
static int ctz(UInt x)
{
   assert(sizeof(UInt) == 4);
   int result=8*sizeof(UInt);
   /* BSFL does not change the destination when the input is zero. */
   asm("bsfl %1,%0" : "=r" (result) : "r" (x), "0" (result) : "cc");
   return result;
}

/* Set the V bits at "addr".  Note the convention: A zero bit means
   "defined"; 1 means "undefined". */
static void set_vbits(UInt *addr, UInt vbits)
{
   (void)VALGRIND_SET_VBITS(addr, &vbits, sizeof(unsigned));
}

static void doit(unsigned vbits, unsigned val)
{
   /* Since we are about to mark "val" partially undefined, make a
      copy that we can use without generating a memcheck warning. */
   unsigned val_copy = val;

   /* Mark "val" partially undefined. */
   set_vbits(&val, vbits);

   /* Convince GCC it does not know what is in "val" so it does not
      optimize away any uses of it. */
   __asm__ ("" : "=r" (val) : "0" (val));

   int result = ctz(val);

   /* The following code is carefully constructed: The conditional
      itself should generate a memcheck warning if and only if it is
      false.  (Specifically, if the first "1" bit in val comes before
      the first "undefined" bit, or the entire word is valid, then
      ctz(val) is defined; else it is undefined.) */
   if (result < ctz(vbits) || vbits == 0) {
      /* There should be no memcheck warning on this printf, since
         "result" is fully-defined in this case. */
      printf("vbits=0x%08x ctz(0x%08x)=%d\n", vbits, val_copy, result);
   }
   else {
       /* memcheck should have output a warning.  But we want
          something here so there is no possibiliy of Valgrind
          optimizing out the conditional itself. */
       fprintf(stderr, "0x%08x: Invalid value is %d\n", val_copy,
               ctz(val_copy));
   }
}

int main(int argc, char *argv[])
{
   doit(0x00000000, 0x00000000);
   doit(0x00000000, 0x00000001);
   doit(0x00000001, 0x00000000);
   doit(0x00000001, 0x00000001);

   /* valid bit / data bit sandwich */
   doit(0x00000090, 0x00000040);
   doit(0x00000040, 0x00000090);

   /* Interleaving */
   doit(0x00000500, 0x00000a00);
   doit(0x00000a00, 0x00000500);

   doit(0x000f0000, 0x001e0000);
   doit(0x001e0000, 0x000f0000);

   doit(0xffffffff, 0xffffffff);
   doit(0xfffffffe, 0xffffffff);
   doit(0xffffffff, 0xfffffffe);
   doit(0xfffffffe, 0xfffffffe);

   return 0;
}
