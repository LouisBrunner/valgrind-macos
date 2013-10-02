#include <stdio.h>
#ifdef HAS_ISA_2_07
int __attribute__ ((noinline)) htm_begin (int r3, int r4)
{
   int ret;

   if (__builtin_tbegin (0)) {
      ret = r3;
      __builtin_tend (0);
   } else {
      ret = r4;
   } return ret;
}
#endif

int main (void) {
#ifdef HAS_ISA_2_07
   int ret;
   ret = htm_begin (10, 20);
   printf ("ret = %d, expected = 10\n", ret);
#endif
   return 0;
}
