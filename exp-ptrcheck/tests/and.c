#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <setjmp.h>
#include <assert.h>

#include "arith_include1.c"

int main(void)
{
   #include "arith_include2.c"

   // Not testing the n&p-->p type cases, too hard to find an 'n' that gives
   // something that looks like a pointer!  (Eg. if the pointer is
   // 0x40c38000, masking to 0x40000000 won't give invalid memory, and
   // masking below that, eg. to 0x00c38000 doesn't give a pointer result.)

   // AND =========================================================
   g(&, n,  n,  n);  // det, det

   g(&, n,  p,  n);  // det, det
   g(&, nFF,p,  n);  // ok, det

   g(&, p,  n,  n);  // det, det
   g(&, p,  nFF,n);  // ok, det

   g(&, p,  p,  p);  // ok, det
   g(&, p,  p2, e);  // bad AND detected;  det, det

   g(&, n,  un, n);  // det, det
   g(&, n,  up, n);  // det, det

   g(&, un, n,  n);  // det, det
   g(&, up, n,  n);  // det, det

   g(&, un, un, u);  // undet, undet
   g(&, un, up, n);  // det, det
   g(&, up, un, n);  // det, det
   g(&, up, up, u);  // ok,  undet
   g(&, up, up2,u);  // undet, undet 

   g(&, un, p,  n);  // det, det
   g(&, up, p,  n);  // det, det (result doesn't look like a pointer)

   g(&, p,  un, n);  // det, det
   g(&, p,  up, u);  // det, det

   return 0;
}
