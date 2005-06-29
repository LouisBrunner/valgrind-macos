#include <unistd.h>
#include <fcntl.h>

int
main (int argc, char **argv)
{
   int s1;
   int s2;

   /*
    * Fedora Core 1's Perl opens /dev/pts/2 as fd 10.  Let's close it
    * now to get consistent results across different releases.
    */

   close(10);

   s1 = open("/dev/null", O_RDONLY);
   s2 = open("/dev/null", O_RDONLY);

   dup2(s1, 20);
   dup2(s1, s2);
   return 0;
}
