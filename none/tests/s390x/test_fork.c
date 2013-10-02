#include <asm/unistd.h>
#include "test.h"

int main()
{
   switch (svc0(__NR_fork)) {
   case 0:
      SAY("child\n");
      break;
   case -1:
      SAY("error\n");
      break;
   default:
      svc4(__NR_wait4, 0, 0, 0, 0);
      SAY("parent\n");
      break;
   }
   EXIT(0);
   return 0;      // shuts up the compiler
}
