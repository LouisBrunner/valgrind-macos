#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
int main ( void )
{
   char* arr = malloc(10);
   int fd = open("/dev/null", O_WRONLY);
   if (fd < 0) {
      fprintf(stderr, "open failed\n");
   } else {
      (void)write(fd, arr, 10);
      (void)close(fd);
   }

   return 0;
}
