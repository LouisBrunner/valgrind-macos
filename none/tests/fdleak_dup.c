#include <unistd.h>
#include <fcntl.h>

int
main (int argc, char **argv)
{
   int s;

   /*
    * Fedora Core 1's Perl opens /dev/pts/2 as fd 10.  Let's close it
    * now to get consistent results across different releases.
    */

   close(10);

   s = open("/dev/null", O_RDONLY);
   dup(s);
   return 0;
}
