#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <setjmp.h>
#include <assert.h>

#include "arith_include1.c"
int aaa(void) { int x = 0x66; return x; }
int bbb(void) { return 0x55; }
int main(void)
{
   #include "arith_include2.c"

   // SUB =========================================================
   g(-, n,  n2, n);  // det, det       // undet, undet?

   g(-, n,  p,  e);  // det, det

   g(-, p,  n,  p);  // ok, det

   g(-, p,  pp, B);  // det, det

   g(-, n,  un, u);  // undet, undet
   g(-, n,  up, u);  // undet, undet

   g(-, un, n,  u);  // undet, undet
   g(-, up, n,  u);  // ok, undet

   g(-, un, un2,u);  // det, det
   g(-, un, up, u);  // undet, undet
   g(-, up, un, u);  // ok, undet
   g(-, up, up, u);  // det, det

   g(-, un, p,  B);  // undet, undet
   g(-, up, p,  B);  // undet, undet

   g(-, p,  un, p);  // det, det
   g(-, p,  up, p);  // det, det

   return 0;
}
