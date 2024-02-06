#include <unistd.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <fcntl.h>
#include <assert.h>

/* It looks like close_range was initially implemented for FreeBSD 13
 * but without CLOSE_RANGE_CLOEXEC
 * That implementation got back ported to FreeBSD 12.2
 * And then CLOSE_RANGE_CLOEXEC added to 13 but not backported
 * so 12 has close_range but not CLOSE_RANGE_CLOEXEC */
#if !defined(CLOSE_RANGE_CLOEXEC)
#define CLOSE_RANGE_CLOEXEC 1
#endif

int main(void)
{
   struct rlimit rl;
   // I'm assuming opens start at 3 and get recycled
   int fd1 = open("close_range.c", O_RDONLY);
   int fd2 = open("close_range.vgtest", O_RDONLY);
   int fd3 = open("close_range.stderr.exp", O_RDONLY);
   
   // all open
   close_range(fd1, fd3, 0);
   // all closed
   close_range(fd1, fd3, 0);
   
   fd1 = open("close_range.c", O_RDONLY);
   fd2 = open("close_range.vgtest", O_RDONLY);
   
   // 3 and 4 open 5 closed
   close_range(fd1, fd3, 0);
   
   fd1 = open("close_range.c", O_RDONLY);
   fd3 = open("close_range.stderr.exp", O_RDONLY);
   
   // 3 and 5 open 4 closed
   close_range(fd1, fd3, 0);
   
   fd1 = open("close_range.c", O_RDONLY);
   fd2 = open("close_range.vgtest", O_RDONLY);
   fd3 = open("close_range.stderr.exp", O_RDONLY);
   
   // good flag
   close_range(fd1, fd3, CLOSE_RANGE_CLOEXEC);
   close_range(fd1, fd3, 0);

   fd1 = open("close_range.c", O_RDONLY);
   fd2 = open("close_range.vgtest", O_RDONLY);
   fd3 = open("close_range.stderr.exp", O_RDONLY);
   
   errno = 0;
   // bad flag
   close_range(fd1, fd3, 2);
   assert(errno = EINVAL);
   
   errno = 0;
   // wrong order
   close_range(fd3, fd1, 2);
   assert(errno = EINVAL);
   
   errno = 0;
   getrlimit(RLIMIT_NOFILE, &rl);
   
   // should do nothing
   close_range(rl.rlim_cur+100, rl.rlim_cur+200, 0);
   
   close_range(3, rl.rlim_cur, 0);
   
   fd1 = open("close_range.c", O_RDONLY);
   fd2 = open("close_range.vgtest", O_RDONLY);
   fd3 = open("close_range.stderr.exp", O_RDONLY);
   
   close_range(3, rl.rlim_cur+1, 0);
   
   {
      unsigned a;
      unsigned b;
      int c;
      close_range(a, b, c);
   }
}

