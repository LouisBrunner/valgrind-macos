
#include <stdlib.h>

int* mk_dangle(void)
{
   int* x = malloc(400);
   free(x);

   return x;
}

int main(void)
{
   int  y __attribute__((unused));
   int* x = mk_dangle();

   y = x[5];
   y = x[-1];
   
   return 0;
}
