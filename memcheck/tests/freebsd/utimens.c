/*
 * Tests for the utimens functions
 *
 * futimens
 * utimensat
 */

#include <sys/stat.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <dirent.h>
#include <fcntl.h>

int main(void)
{
   char tmpfile1[] = "/tmp/testutimens1.XXXXXX";
   char tmpfile2[] = "/tmp/testutimens2.XXXXXX";
   int tmpfd1 = mkstemp(tmpfile1);
   int tmpfd2 = mkstemp(tmpfile2);
   close(tmpfd1);
   close(tmpfd2);
   struct timespec times[2];
   times[0].tv_sec = 30;
   times[0].tv_nsec = 271828;
   times[1].tv_sec = 42;
   times[1].tv_nsec = 314159;

   if (-1 == futimens(tmpfd1, times))
   {
      perror("futimens failed:");
   }

   DIR* tmpdir = opendir("/tmp");
   if (tmpdir) {
      int tmpdirfd = dirfd(tmpdir);
      if (-1 == utimensat(tmpdirfd, tmpfile2+5, times, AT_SYMLINK_NOFOLLOW))
      {
         perror("utimensat failed:");
      }

      close(tmpdirfd);
   }

   // some errors
   struct timespec badtimes[2];
   int badfd;
   char* badname = strdup("foo");
   free(badname);
   futimens(badfd, badtimes);
   utimensat(badfd, badname, badtimes, badfd);

   unlink(tmpfile1);
   unlink(tmpfile2);
}

