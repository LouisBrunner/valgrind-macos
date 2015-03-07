#include <stdio.h>
#include <stdlib.h>
 
#include <sys/syscall.h>
#include <unistd.h>
#include <fcntl.h>
 
#ifndef SYS___pthread_chdir
# define SYS___pthread_chdir 348
#endif
 
#ifndef SYS___pthread_fchdir
# define SYS___pthread_fchdir 349
#endif
 
int __pthread_chdir(const char *path)
{
   return syscall(SYS___pthread_chdir, path);
}
 
int __pthread_fchdir(int dirfd)
{
   return syscall(SYS___pthread_fchdir, dirfd);
}
 
int main(void)
{
   int dirfd;
 
   dirfd = open("/", O_RDONLY);
   if (dirfd == -1)
      perror("open"), exit(1);
 
   if (__pthread_chdir("/"))
      perror("__pthread_chdir");
 
   if (__pthread_fchdir(dirfd))
      perror("__pthread_fchdir");
 
   return 0;
}
