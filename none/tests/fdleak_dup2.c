#include <unistd.h>
#include <fcntl.h>
#include "fdleak.h"
int
main (int argc, char **argv)
{
   int s1;
   int s2;






   CLOSE_INHERITED_FDS;

   s1 = open("/dev/null", O_RDONLY);
   s2 = open("/dev/null", O_RDONLY);

   dup2(s1, 20);
   dup2(s1, s2);
   return 0;
}
