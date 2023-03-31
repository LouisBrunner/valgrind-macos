/* Test (somewhat) stats and stat.  */
#define _GNU_SOURCE
#include "config.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <assert.h>
#include <string.h>
#include <sys/syscall.h>
#if defined(MUSL_LIBC)
/* musl has __NR_statx but not the structs that it needs */
#undef __NR_statx
#else
#if !defined(HAVE_STRUCT_STATX_IN_SYS_STAT_H)
#include <linux/stat.h>
#endif /* HAVE_STRUCT_STATX_IN_SYS_STAT_H */
#endif /* MUSL_LIBC */
#include <errno.h>

int check_stat2;

#define field(fieldname,s) s->st_##fieldname
#if defined(__NR_statx)
#define checkfield(fieldname) \
   assert(!check_stat2 || stat1.st_##fieldname == stat2.stx_##fieldname)
#else
#define checkfield(fieldname) \
   assert(!check_stat2 || stat1.st_##fieldname == stat2.st_##fieldname)
#endif

int main (void)
{
   struct stat stat1;

   memset(&stat1, 0x55, sizeof(stat1));

   assert (stat ("/tmp", &stat1) == 0);
#if defined(__NR_statx)
   struct statx stat2;
   memset(&stat2, 0x22, sizeof(stat2));
   if (syscall (__NR_statx, 0, "/tmp", 0, STATX_ALL, &stat2) == 0)
      check_stat2 = 1;
   else {
      if (errno == ENOSYS)
         check_stat2 = 0; // Defined but not provided by kernel.
      else
         check_stat2 = 1; // Probably better fail ...
   }
#else
   struct stat stat2;
   check_stat2 = 1;
   memset(&stat2, 0x22, sizeof(stat2));
   assert (stat ("/tmp", &stat2) == 0);
#endif

   checkfield(nlink);
   checkfield(uid);
   checkfield(gid);
   checkfield(mode);
   checkfield(ino);

   return 0;
}
