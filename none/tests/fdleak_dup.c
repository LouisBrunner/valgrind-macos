#include <unistd.h>
#include <fcntl.h>
#include "fdleak.h"

int main (int argc, char **argv)
{
   int s;



   s = DO( open("/dev/null", O_RDONLY) );
   (void) DO( dup(s) );

   return 0;
}
