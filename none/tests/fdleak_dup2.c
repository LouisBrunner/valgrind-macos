#include <unistd.h>
#include <fcntl.h>
#include "fdleak.h"

int main (int argc, char **argv)
{
   int s1;
   int s2;



   s1 = DO( open("/dev/null", O_RDONLY) );
   s2 = DO( open("/dev/null", O_RDONLY) );

   (void) DO( dup2(s1, 20) );  // dup s1 as fd 20
   (void) DO( dup2(s1, s2) );  // dup s1 as fd s2, which closes existing s2 fd

   return 0;
}
