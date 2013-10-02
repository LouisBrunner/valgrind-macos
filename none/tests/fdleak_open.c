#include <fcntl.h>
#include <unistd.h>
#include "fdleak.h"

int main (int argc, char **argv)
{
   CLOSE_INHERITED_FDS;

   (void) DO( open("/dev/null", O_RDONLY) );

   return 0;
}
