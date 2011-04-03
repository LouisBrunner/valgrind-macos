
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main ( void )
{
   char* x = strdup("hello");
   char c __attribute__((unused));
   char c0[8], c1[8], c2[8], c3[8], c4[8];

   // Each of these pointers has a different alignment
   char** p0 = (char**)&c0[0];   char** p1 = (char**)&c1[1];
   char** p2 = (char**)&c2[2];   char** p3 = (char**)&c3[3];
   char** p4 = (char**)&c4[4];
   *p0 = x;   *p1 = x;   *p2 = x;
   *p3 = x;   *p4 = x;

   // These 10 are ok
   c = (*p0)[0];
   c = (*p1)[0];
   c = (*p2)[0];
   c = (*p3)[0];
   c = (*p4)[0];

   c = (*p0)[5];
   c = (*p1)[5];
   c = (*p2)[5];
   c = (*p3)[5];
   c = (*p4)[5];

   // These 10 are bad
   c = (*p0)[-1];    // always word aligned, so det
   c = (*p1)[-1];    // undet
   c = (*p2)[-1];    // undet
   c = (*p3)[-1];    // undet
   c = (*p4)[-1];    // undet on 64-bit since not 64-bit aligned

   c = (*p0)[6];     // always word aligned, so det
   c = (*p1)[6];     // undet
   c = (*p2)[6];     // undet
   c = (*p3)[6];     // undet
   c = (*p4)[6];     // undet on 64-bit since not 64-bit aligned

   return 0;
}

/* What this program does: verifies that (unfortunately) if you store a
   pointer misaligned, then the associated shadow value decays to Unknown,
   and so when you retrieve the pointer later and dereference it, you
   get no check :-( */
