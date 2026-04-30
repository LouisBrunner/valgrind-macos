#include<stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>

int main(void)
{
   char *string = "bad\n";
   int fd = dup(2);

   /* OK. */
   write(fd, string, 4);
   close(fd);

   /* Already closed. */
   write(fd, string, 4);

   /* Never created. */
   write(10, string, 4);

   /* Invalid. */
   write(-7, string, 4);

   /* Double double close. */
   close(fd);

   /* Invalid close. */
   close (-7);

   return 0;
}


