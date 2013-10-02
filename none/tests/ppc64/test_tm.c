#include <stdio.h>
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

int main (void) {
   int ret;
   ret = htm_begin (10, 20);
   printf ("ret = %d, expected = 10\n", ret);
   return 0;
}
