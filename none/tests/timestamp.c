#include <stdlib.h>
#include <unistd.h>

int main(void)
{
   int y = 0;
   int* x = calloc(1,sizeof(int));
   free(x);

   y += x[0];
   sleep(1);
   y += x[0];

   return y;
}
