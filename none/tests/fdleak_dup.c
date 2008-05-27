#include <unistd.h>
#include <fcntl.h>
#include "fdleak.h"
int
main (int argc, char **argv)
{
   int s;






   CLOSE_INHERITED_FDS;

   s = open("/dev/null", O_RDONLY);
   dup(s);
   return 0;
}
