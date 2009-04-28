#include <unistd.h>
#include <stdio.h>
#include <fcntl.h>
#include "fdleak.h"

int main (int argc, char **argv)
{
   char filename[24];

   CLOSE_INHERITED_FDS;

   sprintf(filename, "/tmp/file.%d", getpid());
   DO( creat(filename, 0) );
   DO( unlink(filename) );
   return 0;
}
