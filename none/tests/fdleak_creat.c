#include <unistd.h>
#include <stdio.h>
#include <fcntl.h>
#include "fdleak.h"

int main (int argc, char **argv)
{
   char filename[24];

   CLOSE_INHERITED_FDS;

   sprintf(filename, "/tmp/file.%d", getpid());
   (void) DO( creat(filename, 0) );
   (void) DO( unlink(filename) );
   return 0;
}
