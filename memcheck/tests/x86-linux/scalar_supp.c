#include <stdlib.h>
#include <unistd.h>
#include <sys/syscall.h>

int main(void)
{
   // uninitialised, but we know pi[0] is 0x0
   int* pi  = malloc(sizeof(int));

   // uninitialised, but we know pc[0] points to 0x0
   char** pc  = malloc(sizeof(char*));
   
   // Five errors:  
   // - the syscall number itself is undefined (but we know it's
   //   0 + __NR_write :)
   // - each of the scalar args are undefined
   // - the 2nd arg points to unaddressable memory.
   syscall(pi[0]+__NR_write, pi[0], pc[0], pi[0]+1);

   return 0;
}

