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
   int fd = dup (2);
   if (fd != -1){
      fprintf(stderr, "close fd %d\n", fd);
      close (fd);
   }
   
   // Shouldn't warn, we are closing everything
   fprintf(stderr, "Closing range (%d, %d).\n", fd, ~0U);
   close_range(6, ~0U, 0);

   // Should warn, we close a small range (but only for fd itself
   fprintf(stderr, "Closing range (%d, %d).\n", fd, 7);
   close_range(5, 7, 0);

   return 0;
}
