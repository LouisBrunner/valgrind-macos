/*
 * Tests for the utimes functions
 *
 * utimes, lutimes, futimes, futimesat
 */

#include <sys/time.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <dirent.h>
#include <string.h>

int main(void)
{
   char tmpfile1[] = "/tmp/testutimes1.XXXXXX";
   int tmpfd1 = mkstemp(tmpfile1);

   struct timeval times[2];
   times[0].tv_sec = 12;
   times[0].tv_usec = 14142;
   times[1].tv_sec = 13;
   times[1].tv_usec = 12345;
   if (-1 == utimes("utimes.c", NULL)) {
      perror("utimes failed: ");
   }

   if (-1 == utimes("utimes.c", times)) {
      perror("utimes failed: ");
   }

   times[0].tv_sec = 14;
   times[0].tv_usec = 70711;
   times[1].tv_sec = 15;
   times[1].tv_usec = 23456;
   if (-1 == futimes(tmpfd1, NULL)) {
      perror("futimes failed: ");
   }
   if (-1 == futimes(tmpfd1, times)) {
      perror("futimes failed: ");
   }

   char tmpsymlink[] = "/tmp/testutimes2.XXXXXX";
   mktemp(tmpsymlink);

   DIR* tmpdir = opendir("/tmp");
   if (tmpdir) {
      int tmpdirfd = dirfd(tmpdir);

      if (-1 == symlinkat(tmpfile1+5, tmpdirfd, tmpsymlink+5)) {
         perror("symlinkat failed");
      }

      times[0].tv_sec = 16;
      times[0].tv_usec = 15708;
      times[1].tv_sec = 17;
      times[1].tv_usec = 34567;

      if (-1 == lutimes(tmpsymlink, NULL)) {
         perror("lutimes failed: ");
      }

      if (-1 == lutimes(tmpsymlink, times)) {
         perror("lutimes failed: ");
      }

      times[0].tv_sec = 18;
      times[0].tv_usec = 63662;
      times[1].tv_sec = 20;
      times[1].tv_usec = 31831;

      if (-1 == futimesat(tmpdirfd, tmpfile1+5, NULL)) {
         perror("fuutimesat failed: ");
      }

      if (-1 == futimesat(tmpdirfd, tmpfile1+5, times)) {
         perror("fuutimesat failed: ");
      }

      closedir(tmpdir);
   }

   close(tmpfd1);

   unlink(tmpfile1);
   unlink(tmpsymlink);

   // error section

   char* badstring = strdup("foo");
   free(badstring);
   int badint;

   utimes(badstring, (struct timeval*)badstring);
   lutimes(badstring, (struct timeval*)badstring);
   futimes(badint, (struct timeval*)badstring);
   futimesat(badint, badstring, (struct timeval*)badstring);
}

