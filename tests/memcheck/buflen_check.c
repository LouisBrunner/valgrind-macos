#include <sys/socket.h>
#include <stdlib.h>
#include <stdio.h>

int main(void)
{
   struct sockaddr name;
   int res1, res2;
   int len = 10;

   res1 = socket(PF_UNIX, SOCK_STREAM, 0);
   if (res1 == 0) {
      fprintf(stderr, "socket() failed\n");
      exit(1);
   }

   /* Valgrind 1.0.X doesn't report the second error */
   res1 = getsockname(-1, NULL,  &len);    /* NULL is bogus */
   res2 = getsockname(-1, &name, NULL);    /* NULL is bogus */
   if (res1 == -1) {
      fprintf(stderr, "getsockname(1) failed\n");
   }
   if (res2 == -1) {
      fprintf(stderr, "getsockname(2) failed\n");
   }
   
   return 0;
}

