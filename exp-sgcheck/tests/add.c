#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <setjmp.h>
#include <assert.h>

#include "arith_include1.c"

int main(void)
{
   #include "arith_include2.c"

   // ADD =========================================================
   g(+, n,  n,  n);  // det, det

   g(+, n,  p,  p);  // ok, det

   g(+, p,  n,  p);  // ok, det

   g(+, p,  p,  e);  // detected bad add;  det, det

   g(+, n,  un, u);  // undet, undet
   g(+, n,  up, u);  // ok, undet

   g(+, un, n,  u);  // undet, undet
   g(+, up, n,  u);  // ok, undet

   g(+, un, un, u);  // undet, undet
   g(+, un, up, u);  // undet, undet
   g(+, up, un, u);  // undet, undet
   g(+, up, up, u);  // undetected bad add; undet, undet

   g(+, un, p,  u);  // undet, undet
   g(+, up, p,  u);  // undetected bad add; undet, undet

   g(+, p,  un, u);  // undet, undet
   g(+, p,  up, u);  // undetected bad add; undet, undet
  
   return 0;
}
