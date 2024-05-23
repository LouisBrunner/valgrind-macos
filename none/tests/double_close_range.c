#define _GNU_SOURCE
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#if defined(__linux__)
#include <linux/close_range.h>
#endif

int main ()
{
   // Create a new file descriptor that we immediately close.
   // This is of course fine.
   int fd = dup (2);
   if (fd != -1){
      fprintf(stderr, "close fd %d\n", fd);
      close (fd);
   }
   
   // Shouldn't warn, we are closing everything
   // This is a special case for close_range if the last
   // argument is ~0U (infinity).
   fprintf(stderr, "Closing range (%d, %d).\n", fd, ~0U);
   close_range(fd, ~0U, 0);

   int fd5 = dup2(1, 5);
   int fd7 = dup2(2, 7);
   fprintf(stderr, "Closing range (%d, %d).\n", fd5, fd7);
   close_range(5, 7, 0);

   // Should warn, we close a small range
   fprintf(stderr, "Double closing range (%d, %d).\n", fd5, fd7);
   close_range(5, 7, 0);

   return 0;
}
