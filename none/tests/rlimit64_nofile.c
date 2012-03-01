#define _LARGEFILE_SOURCE
#define _LARGEFILE64_SOURCE

#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/resource.h>
#include <unistd.h>
#include "fdleak.h"

int main(int argc, char **argv)
{
   struct rlimit64 oldrlim;
   struct rlimit64 newrlim;
   int fd;

   CLOSE_INHERITED_FDS;

   if (getrlimit64(RLIMIT_NOFILE, &oldrlim) < 0)
   {
      perror("getrlimit");
      exit(1);
   }

   newrlim.rlim_cur = oldrlim.rlim_max+1;
   newrlim.rlim_max = oldrlim.rlim_max;
   if (setrlimit64(RLIMIT_NOFILE, &newrlim) == -1)
   {
      if (errno != EINVAL) {
         fprintf(stderr, "setrlimit64 exceeding hardlimit must set errno=EINVAL\n");
         exit(1);
      }
   }
   else
   {
        fprintf(stderr, "setrlimit64 exceeding hardlimit must return -1\n");
        exit(1);
   }

   newrlim.rlim_cur = oldrlim.rlim_max;
   newrlim.rlim_max = oldrlim.rlim_max+1;
   if (setrlimit64(RLIMIT_NOFILE, &newrlim) == -1)
   {
      if (errno != EPERM) {
         fprintf(stderr, "setrlimit64 changing hardlimit must set errno=EPERM\n");
         exit(1);
      }
   }
   else
   {
        fprintf(stderr, "setrlimit64 changing hardlimit must return -1\n");
        exit(1);
   }

   newrlim.rlim_cur = oldrlim.rlim_cur / 2;
   newrlim.rlim_max = oldrlim.rlim_max;

   if (setrlimit64(RLIMIT_NOFILE, &newrlim) < 0)
   {
      perror("setrlimit64");
      exit(1);
   }

   if (getrlimit64(RLIMIT_NOFILE, &newrlim) < 0)
   {
      perror("getrlimit");
      exit(1);
   }

   if (newrlim.rlim_cur != oldrlim.rlim_cur / 2)
   {
      fprintf(stderr, "rlim_cur is %llu (should be %llu)\n",
              (unsigned long long)newrlim.rlim_cur,
              (unsigned long long)oldrlim.rlim_cur / 2);
   }

   if (newrlim.rlim_max != oldrlim.rlim_max)
   {
      fprintf(stderr, "rlim_max is %llu (should be %llu)\n",
              (unsigned long long)newrlim.rlim_max,
              (unsigned long long)oldrlim.rlim_max);
   }

   newrlim.rlim_cur -= 3; /* allow for stdin, stdout and stderr */

   while (newrlim.rlim_cur-- > 0)
   {
      if (open("/dev/null", O_RDONLY) < 0)
      {
         perror("open");
      }
   }

   if ((fd = open("/dev/null", O_RDONLY)) >= 0)
   {
      fprintf(stderr, "open succeeded with fd %d - it should have failed!\n", fd);
   }
   else if (errno != EMFILE)
   {
      perror("open");
   }

   exit(0);
}
