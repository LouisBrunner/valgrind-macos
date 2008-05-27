#include <unistd.h>
#include "fdleak.h"
int
main (int argc, char **argv)
{
   int fds[2];






   CLOSE_INHERITED_FDS;

   pipe(fds);
   return 0;
}
