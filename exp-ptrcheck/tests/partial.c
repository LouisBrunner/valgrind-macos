
#include <stdlib.h>

int main ( void )
{
   int* x3 = malloc(3);    float         f,   *f3   = malloc(3);
   int* x4 = malloc(4);    double        d,   *d7   = malloc(7);
   int* x5 = malloc(5);    long long int lli, *lli7 = malloc(7);
   int* x6 = malloc(6);    char          c,   *c0   = malloc(0);
   int* x7 = malloc(7);    short int     s,   *s1   = malloc(1);
   int  x;
   int* y4 = malloc(4);
   int* y5 = malloc(5);
   int* y6 = malloc(6);
   int* y7 = malloc(7);

   #define ADDB(ptr, n)  ((int*)(((unsigned long)(ptr)) + (n)))

   // All these overrun by a single byte;  the reads are happening at
   // different alignments.
   x = * ADDB(x3,0);    // ok if --partial-loads-ok=yes
   x = * ADDB(x4,1);
   x = * ADDB(x5,2);
   x = * ADDB(x6,3);
   x = * ADDB(x7,4);    // ok if --partial-loads-ok=yes

   // These are fine
   x = * ADDB(y4,0);
   x = * ADDB(y5,1);
   x = * ADDB(y6,2);
   x = * ADDB(y7,3);

   // These are all bad, at different points along
   x = * ADDB(x3,-1);   // before
   x = * ADDB(x3, 0);   // inside      // ok if --partial-loads-ok=yes ...
   x = * ADDB(x3, 1);   // inside      // ... but only on 32 bit platforms
   x = * ADDB(x3, 2);   // inside      // ... ditto
   x = * ADDB(x3, 3);   // after

   // These are all bad
   f   = * f3;    // ok if --partial-loads-ok=yes  // ... ditto
   d   = * d7;
   lli = * lli7;  // ok if --partial-loads-ok=yes  see XXX below
   c   = * c0;
   s   = * s1;

   return 0;
}

/* Note re XXX, this gives different behaviour on 32 and 64 bit
platforms, because on 64-bit it's one load whereas as on 32 bit
platforms it's necessarily 2 32-bit loads, and the first one is OK. */
