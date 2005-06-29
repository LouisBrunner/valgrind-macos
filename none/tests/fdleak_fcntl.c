#include <unistd.h>
#include <stdio.h>
#include <fcntl.h>

int
main (int argc, char **argv)
{
   int s1;

   /*
    * Fedora Core 1's Perl opens /dev/pts/2 as fd 10.  Let's close it
    * now to get consistent results across different releases.
    */

   close(10);

   s1 = open("/dev/null", O_RDONLY);
   if(fcntl(s1, F_DUPFD, s1) == -1) perror("fcntl");
   return 0;
}
