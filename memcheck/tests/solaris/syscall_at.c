/* Tests various combinations of dfd and pathname for *at syscalls.
   In particular, dfd should not be checked when pathname is absolute.
   See https://bugs.kde.org/show_bug.cgi?id=307103 for more information.
 */

#include "config.h"

#include <fcntl.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/syscall.h>

#define DIRECTORY "/tmp/"
#define FILENAME "abc123"

int main(void)
{
   char buf[1];
   struct stat stats;

   int dfd = open(DIRECTORY, O_RDONLY);

   /* linkat */
   linkat(dfd, FILENAME, dfd, FILENAME, 0);
   linkat(0x9879151, DIRECTORY FILENAME, 0x9879152, DIRECTORY FILENAME, 0);
   linkat(AT_FDCWD, FILENAME, AT_FDCWD, FILENAME, 0);
   linkat(0x9879153, FILENAME, 0x9879154, FILENAME, 0); /* warning for this one */

   /* symlinkat */
   symlinkat(FILENAME, dfd, FILENAME);
   symlinkat(DIRECTORY FILENAME, 0x26868151, DIRECTORY FILENAME);
   symlinkat(FILENAME, AT_FDCWD, FILENAME);
   symlinkat(FILENAME, 0x26868152, FILENAME); /* warning for this one */

   /* readlinkat */
   readlinkat(dfd, FILENAME, buf, 1);
   readlinkat(0x4368151, DIRECTORY FILENAME, buf, 1);
   readlinkat(AT_FDCWD, FILENAME, buf, 1);
   readlinkat(0x4368152, FILENAME, buf, 1); /* warning for this one */

#if defined(SOLARIS_FREALPATHAT_SYSCALL)
   /* frealpathat - not available directly */
   syscall(SYS_frealpathat, dfd, FILENAME, buf, 1);
   syscall(SYS_frealpathat, 0x443115, DIRECTORY FILENAME, buf, 1);
   syscall(SYS_frealpathat, AT_FDCWD, FILENAME, buf, 1);
#endif /* SOLARIS_FREALPATHAT_SYSCALL */

   /* faccessat */
   faccessat(dfd, FILENAME, F_OK, 0);
   faccessat(0x4132151, DIRECTORY FILENAME, F_OK, 0);
   faccessat(AT_FDCWD, FILENAME, F_OK, 0);
   faccessat(0x4132152, FILENAME, F_OK, 0); /* warning for this one */

   /* fchownat */
   fchownat(dfd, FILENAME, -1, -1, 0);
   fchownat(0x4369251, DIRECTORY FILENAME, -1, -1, 0);
   fchownat(AT_FDCWD, FILENAME, -1, -1, 0);
   fchownat(0x4369252, FILENAME, -1, -1, 0); /* warning for this one */

   /* renameat */
   renameat(dfd, FILENAME, dfd, FILENAME);
   renameat(0x4371151, DIRECTORY FILENAME, 0x4371152, DIRECTORY FILENAME);
   renameat(AT_FDCWD, FILENAME, AT_FDCWD, FILENAME);
   renameat(0x4371153, FILENAME, 0x4371154, FILENAME); /* warning for this one */

   /* unlinkat */
   unlinkat(dfd, FILENAME, 0);
   unlinkat(0x7608151, DIRECTORY FILENAME, 0);
   unlinkat(AT_FDCWD, FILENAME, 0);
   unlinkat(0x7608152, FILENAME, 0); /* warning for this one */

   /* fstatat */
   fstatat(dfd, FILENAME, &stats, 0);
   fstatat(0x42515151, DIRECTORY FILENAME, &stats, 0);
   fstatat(AT_FDCWD, FILENAME, &stats, 0);
   fstatat(0x42515152, FILENAME, &stats, 0); /* warning for this one */

   /* openat */
   openat(dfd, FILENAME, O_RDONLY);
   openat(0x9038151, DIRECTORY FILENAME, O_RDONLY);
   openat(AT_FDCWD, FILENAME, O_RDONLY);
   openat(0x9038152, FILENAME, O_RDONLY); /* warning for this one */

   /* fchmodat */
   fchmodat(dfd, FILENAME, S_IRUSR | S_IWUSR, 0);
   fchmodat(0x4303151, DIRECTORY FILENAME, S_IRUSR | S_IWUSR, 0);
   fchmodat(AT_FDCWD, FILENAME, S_IRUSR | S_IWUSR, 0);
   fchmodat(0x4303152, FILENAME, S_IRUSR | S_IWUSR, 0); /* warning for this one */

   /* mkdirat */
   mkdirat(dfd, FILENAME, S_IRUSR | S_IWUSR);
   mkdirat(0x9384151, DIRECTORY FILENAME, S_IRUSR | S_IWUSR);
   mkdirat(AT_FDCWD, FILENAME, S_IRUSR | S_IWUSR);
   mkdirat(0x9384152, FILENAME, S_IRUSR | S_IWUSR); /* warning for this one */

   /* utimensat */
   utimensat(dfd, FILENAME, NULL, 0);
   utimensat(0x59837215, DIRECTORY FILENAME, NULL, 0);
   utimensat(AT_FDCWD, FILENAME, NULL, 0);
   utimensat(0x59837216, FILENAME, NULL, 0); /* warning for this one */

   rmdir(DIRECTORY FILENAME);
   rmdir(FILENAME);
   close(dfd);
   return 0;
}
