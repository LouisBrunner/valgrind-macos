#include <stdlib.h>

int main(void)
{
   int i;
   for (i = 0; i < 20; i++) {
      int* p;
      p = malloc(800);  // With --peak-inaccuracy=1000, the first 10 of
      p = malloc(8);    // 'free' calls result in peaks, but after that,
      free(p);          // only every second one does.
   }
   return 0;
}
