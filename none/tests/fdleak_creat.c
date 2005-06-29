#include <unistd.h>
#include <stdio.h>
#include <fcntl.h>

int
main (int argc, char **argv)
{
   char filename[24];
   
   /*
    * Fedora Core 1's Perl opens /dev/pts/2 as fd 10.  Let's close it
    * now to get consistent results across different releases.
    */

   close(10);

   sprintf(filename, "/tmp/file.%d\n", getpid());
   creat(filename, 0);
   unlink(filename);
   return 0;
}
