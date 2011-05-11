
#include <stdlib.h>

typedef unsigned long Ulong;

int main(void)
{
   long* x = malloc(sizeof(long) * 10);
   long* y = malloc(sizeof(long) * 10);
   long* y2 = y + 3;

   // ok -- same segment
   long  w = y2 - y;

   // ok -- different heap segments (result can only be used to index off
   // 'x', but glibc's strcpy() does this...)
   long* z = (long*)((long)x - (long)y);

   w = (long)y2 + (long)y;           // bad (same segment)

   w = (long)x  & (long)y;           // bad (different segments)

   w = (long)y2 / (long)4;           // bad, but indistinguishable from
                                     // acceptable '%' cases...

   w = (long)y2 % (long)4;           // ok
   w = (long)y2 % (long)y;           // bad -- modulor(?) is a pointer
   w = (long)0xffffffff % (long)y;   // bad -- modulend(?) is a non-pointer

   w = (Ulong)y2 % (Ulong)4;         // ok
   w = (Ulong)y2 % (Ulong)y;         // bad -- modulor(?) is a pointer
   w = (Ulong)0xffffffff % (Ulong)y; // bad -- modulend(?) is a non-pointer

   w = (long)y * (long)y2;           // bad

   w = (long)y >> (long)2;           // ok
   w = (long)y << (long)2;           // ok

   w = (long)y &  0xffff;            // ok
   w = (long)y |  0xffff;            // ok
   w = (long)y ^  (long)y2;          // ok

   w = ~((long)y);                   // ok

   w = -((long)y);                   // bad -- operand is a non-polonger

   w = (long)x ^ (long)x;            // xor(ptr,ptr) --> constant (0)
   z = x + w;                        // ok, because xor result was zero

   w = (long)x ^ ((long)x+1);        // xor(ptr,ptr') --> constant (small)
   z = x + w;                        // ok, because xor result was constant

   w = (long)x ^ (long)y;            // xor(ptr,ptr') --> constant (small)
   z = x + w;                        // ok, because xor result was constant

   return (long)z;
}
