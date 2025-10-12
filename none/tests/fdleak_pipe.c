#include <unistd.h>
#include "fdleak.h"

int main (int argc, char **argv)
{
   int fds[2];



   (void) DO( pipe(fds) );

   return 0;
}
