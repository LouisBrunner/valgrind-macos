#include <sys/syscall.h>
#include <errno.h>
#include <dirent.h>
#include <unistd.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <fcntl.h>

static int sys_execveat (int dirfd, const char *pathname,
			 char *const argv[], char *const envp[],
			 int flags)
{
#if defined(__NR_execveat)
  return syscall(__NR_execveat, dirfd, pathname, argv, envp, flags);
#else
  errno = ENOSYS;
  return -1;
#endif
}


int main()
{
  char *argv[] = { "foobar", "execveat exists", NULL };
  char *envp[] = { NULL };
  DIR  *dirp;
  int  fd;

  dirp = opendir("/bin");
  if (dirp == NULL) {
      perror("execveat");
      exit(EXIT_FAILURE);
  }
  fd = dirfd(dirp);

  /* Check valgrind will produce expected warnings for the
     various wrong arguments. */
  do {
      char *mem = malloc(16);
      void *t = (void *) &mem[0];
      void *z = (void *) -1;
      int flag = *((int *) &mem[8]);

      sys_execveat(-1, "bin/xecho", argv, envp, 0);
      sys_execveat(-1, "xecho", argv, envp, 0);
      sys_execveat(fd, "xecho", argv, envp, flag);
      sys_execveat(fd, "", argv, envp, 0);
      sys_execveat(fd, NULL, argv, envp, 0);
      sys_execveat(fd, "xecho", t, envp, 0);
      sys_execveat(fd, "xecho", z, envp, 0);
  } while (0);

  /* Check execveat called with the correct arguments works. */
  if (sys_execveat(fd, "echo", argv, envp, 0) == -1) {
      perror("execveat");
      exit(EXIT_FAILURE);
  }

  closedir(dirp);
  exit(EXIT_SUCCESS);
}

