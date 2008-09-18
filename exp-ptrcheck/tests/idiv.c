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
   g(/, n,  n2, n);  // det, det

   g(/, n,  p,  e);  // detected bad idiv;  det, det

   g(/, p,  n2, n);  // ok, det

   g(/, p,  p,  e);  // detected bad idiv;  det, det

   g(/, n,  un, n);  // undet, undet
   g(/, n,  up, n);  // undetected bad idiv;  ok, undet

   g(/, un, n2, n);  // undet, undet
   g(/, up, n2, n);  // ok, undet

   g(/, un, un, n);  // undet, undet
   g(/, un, up, n);  // undetected bad idiv;  undet, undet
   g(/, up, un, n);  // undet, undet
   g(/, up, up, n);  // undetected bad idiv;  undet, undet

   g(/, un, p,  n);  // detected bad idiv;  undet, undet
   g(/, up, p,  n);  // detected bad idiv;  undet, undet

   g(/, p,  un, n);  // undet, undet
   g(/, p,  up, n);  // undetected bad idiv;  undet, undet
  
   return 0;
}
