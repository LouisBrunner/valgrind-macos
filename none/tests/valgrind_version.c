#include <stdio.h>
#include <assert.h> 
#include "valgrind.h"

int main (void)
{
   int valgrind_running_version = VALGRIND_RUNNING_VERSION;
   assert(valgrind_running_version <= 2000);
   if(valgrind_running_version>=328){
      printf("OK\n");
   } else {
      // should always be 0
      printf("Version: %d\n", valgrind_running_version);
   }
}
