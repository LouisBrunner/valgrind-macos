// Testing accesses of blocks.

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

void* m1(size_t n) { return malloc(n); }

void* m2(size_t n) { return malloc(n); }

int main(void)
{
   // 0th char is written 0 times, 1st char is written once, etc.
   char* a = malloc(32);
   for (int i = 1; i < 32; i++) {
      for (int j = 0; j < i; j++) {
         a[i] = 0;
      }
   }
   free(a);

   // Repetition and gaps.
   int* b = malloc(20);
   b[0] = 1;
   b[2] = b[0];
   for (int i = 0; i < 10; i++) {
      b[4] = 99;
   }
   free(b);

   // 33 bytes, goes onto a second line in dh_view.
   char* c = calloc(33, 1);
   c[32] = 0;
   free(c);

   // 1024 bytes, accesses are shown.
   char* d = malloc(1024);
   for (int i = 0; i < 1024; i++) {
      d[i] = d[1023 - i];
   }
   for (int i = 500; i < 600; i++) {
      d[i] = 0;
   }
   free(d);

   // 1025 bytes, accesses aren't shown.
   char* e = calloc(1025, 1);
   for (int i = 0; i < 1025; i++) {
      e[i] += 1;
   }
   free(e);

   // Lots of accesses, but fewer than the 0xffff max value.
   int* f1 = m1(100);
   int* f2 = m1(100);
   for (int i = 0; i < 50000; i++) {
      f1[0] = 0;
      f2[0] = 0;
   }
   free(f1);
   free(f2);

   // Lots of accesses, more than the 0xffff max value: treated as Infinity.
   int* g1 = m2(100);
   int* g2 = m2(100);
   for (int i = 0; i < 100000; i++) {
      g1[0] = 0;
      g2[0] = 0;
   }
   free(g1);
   free(g2);

   return 0;
}
