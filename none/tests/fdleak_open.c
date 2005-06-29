#include <fcntl.h>
#include <unistd.h>
int
main (int argc, char **argv)
{
   /*
    * Fedora Core 1's Perl opens /dev/pts/2 as fd 10.  Let's close it
    * now to get consistent results across different releases.
    */

   close(10);

   open("/dev/null", O_RDONLY);
   return 0;
}
