#include <stdlib.h>
#include <unistd.h>

int main(void)
{
   // uninitialised, but we know pi[0] is 0x0
   int* pi  = malloc(sizeof(int));

   // uninitialised, but we know pc[0] points to 0x0
   char** pc  = malloc(sizeof(char*));
   
   // Four errors:  each of the scalar args are undefined, plus the 2nd arg
   // points to unaddressable memory.
   write(pi[0], pc[0], pi[0]+1);

   return 0;
}

