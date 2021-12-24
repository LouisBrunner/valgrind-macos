#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <unistd.h>
#include "fdleak.h"
#include "config.h"

int main(int argc, char **argv)
{
   struct rlimit oldrlim;
   struct rlimit newrlim;
   int fd;

   CLOSE_INHERITED_FDS;

   if (getrlimit(RLIMIT_NOFILE, &oldrlim) < 0)
   {
      perror("getrlimit");
      exit(1);
   }

   int expected_errno;
#if defined(VGO_freebsd)
   expected_errno = EPERM;
#else
   expected_errno = EINVAL;
#endif
   newrlim.rlim_cur = oldrlim.rlim_max+1;
   newrlim.rlim_max = oldrlim.rlim_max;
   if (setrlimit(RLIMIT_NOFILE, &newrlim) == -1)
   {
      if (errno != expected_errno) {
#if defined(VGO_freebsd)
         fprintf(stderr, "setrlimit exceeding hardlimit must set errno=EPERM\n");
#else
         fprintf(stderr, "setrlimit exceeding hardlimit must set errno=EINVAL\n");
#endif
         exit(1);
      }
   }
   else
   {
        fprintf(stderr, "setrlimit exceeding hardlimit must return -1\n");
        exit(1);
   }

   newrlim.rlim_cur = oldrlim.rlim_max;
   newrlim.rlim_max = oldrlim.rlim_max+1;
   if (setrlimit(RLIMIT_NOFILE, &newrlim) == -1)
   {
      if (errno != EPERM) {
         fprintf(stderr, "setrlimit changing hardlimit must set errno=EPERM\n");
         exit(1);
      }
   }
   else
   {
        fprintf(stderr, "setrlimit changing hardlimit must return -1\n");
        exit(1);
   }

   newrlim.rlim_cur = oldrlim.rlim_cur / 2;
   newrlim.rlim_max = oldrlim.rlim_max;
     
   if (setrlimit(RLIMIT_NOFILE, &newrlim) < 0)
   {
      perror("setrlimit");
      exit(1);
   }
     
   if (getrlimit(RLIMIT_NOFILE, &newrlim) < 0)
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

   /* We used to test setrlimit(RLIMIT_NOFILE, NULL) -1 || errno != EFAULT,
      but glibc doesn't give any guarantees that won't just crash, in
      newer versions it just silently succeeds... See bug #385912. */
   
   exit(0);
}
