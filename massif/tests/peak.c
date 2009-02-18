#include <stdlib.h>

int main(void)
{
   int i;
   for (i = 0; i < 20; i++) {
      int* p;           // Sizes are divisible by 16 -- no slop.
      p = malloc(1600); // With --peak-inaccuracy=1000, the first 10 of
      p = malloc(16);   // 'free' calls result in peaks, but after that,
      free(p);          // only every second one does.
   }
   return 0;
}
