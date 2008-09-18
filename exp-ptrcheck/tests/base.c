#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <setjmp.h>
#include <assert.h>

#include "arith_include1.c"

int main(void)
{
   #include "arith_include2.c"
   
   // Base ========================================================
   b(p,  p);         // ok

   b(up, u);         // ok

   b(un, u);         // undet

   b(n,  n);         // det

   b(nn, n);         // det

   return 0;
}
