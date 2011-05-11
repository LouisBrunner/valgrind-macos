#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <setjmp.h>
#include <assert.h>

#include "arith_include1.c"

int main(void)
{
   #include "arith_include2.c"
   
   // XOR =========================================================
   g(^, n,  n,  n);  // det, det

   g(^, n,  p,  u);  // ok, undet

   g(^, p,  n,  u);  // ok, undet

   g(^, p,  p,  n);  // det, det

   g(^, n,  un, u);  // undet, undet
   g(^, n,  up, u);  // ok, undet

   g(^, un, n,  u);  // undet, undet
   g(^, up, n,  u);  // ok, undet

   g(^, un, un, n);  // det, det (range)
   g(^, un, up, u);  // ok, undet
   g(^, up, un, u);  // ok, undet
   g(^, up, up, n);  // det, det

   g(^, un, p,  u);  // ok, undet
   g(^, up, p,  u);  // undet, undet

   g(^, p,  un, u);  // ok, undet
   g(^, p,  up, u);  // undet, undet

   return 0;
}
