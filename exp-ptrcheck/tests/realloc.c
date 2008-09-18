
#include <stdlib.h>

int main(void)
{
   int i;
   int* y;
   int** x  = malloc(sizeof(int*) * 100);
   int* x2 = malloc(sizeof(int) * 100);
   void* sink;
   x[0]  = x2;  // this is to check the pointerness is copied across ok
   x[49] = x2;  // this is to check the pointerness is copied across ok
   
   i = *x[0];
   i = *x[49];
   
   x = realloc(x, sizeof(int*)*50);     // smaller
   y = x[0];   // ok
   y = x[49];  // ok
   y = x[-1];  // bad
   y = x[50];  // bad
   i = *x[0];  // ok
   i = *x[49]; // ok

   x = realloc(x, sizeof(int*)*50);     // same size
   y = x[0];   // ok
   y = x[49];  // ok
   y = x[-1];  // bad
   y = x[50];  // bad
   i = *x[0];  // ok
   i = *x[49]; // ok

   x = realloc(x, sizeof(int*)*100);     // bigger
   y = x[0];   // ok
   y = x[49];  // ok
   y = x[50];  // ok
   y = x[99];  // ok
   y = x[-1];  // bad
   y = x[100]; // bad
   i = *x[0];  // ok
   i = *x[49]; // ok

   sink = realloc((void*)0x99, 10);    // fails

   return 0;
}
