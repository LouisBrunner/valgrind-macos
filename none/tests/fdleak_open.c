#include <fcntl.h>
#include <unistd.h>
#include "fdleak.h"

int
main (int argc, char **argv)
{



   CLOSE_INHERITED_FDS;

   open("/dev/null", O_RDONLY);
   return 0;
}
