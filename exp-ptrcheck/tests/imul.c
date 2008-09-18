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
   g(*, n,  n,  n);  // det, det

   g(*, n,  p,  n);  // det, det

   g(*, p,  n,  n);  // ok, det

   g(*, p,  p,  e);  // detected bad mul;  det, det

   g(*, n,  un, n);  // det, det
   g(*, n,  up, n);  // ok, det

   g(*, un, n,  n);  // det, det
   g(*, up, n,  n);  // ok, det

   g(*, un, un, n);  // det, det
   g(*, un, up, n);  // det, det
   g(*, up, un, n);  // det, det
   g(*, up, up, n);  // undetected bad imul; det, det

   g(*, un, p,  n);  // det, det
   g(*, up, p,  n);  // undetected bad imul; det, det

   g(*, p,  un, n);  // det, det
   g(*, p,  up, n);  // undetected bad imul; det, det
  
   return 0;
}
