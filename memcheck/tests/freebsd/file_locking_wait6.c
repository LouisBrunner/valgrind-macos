/**
 * Version of same in parent directory using wait6
 */


#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include "tests/sys_mman.h"
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/resource.h>
#include <sys/signal.h>
#include <unistd.h>


/** Lock an entire file exclusively.
 *
 *  @return 1 upon success, 0 upon failure.
 */
static int lock_file(const int fd)
{
  struct flock fl;

  fl.l_type   = F_WRLCK;  /* exclusive lock */
  fl.l_whence = SEEK_SET;
  fl.l_start  = 0;
  fl.l_len    = 0;        /* lock entire file */
  fl.l_pid    = 0;
  return fcntl(fd, F_SETLK, &fl) >= 0;
}

static int open_lock_and_map(const char* const process_name,
                             const char* const filename)
{
  int fd;
  int flags;

  fd = open(filename, O_RDWR | O_CREAT, S_IRUSR | S_IWUSR);
  if (fd < 0)
  {
    perror("open");
    goto err1;
  }

  flags = fcntl(fd, F_GETFD);
  assert(flags >= 0);
  if (fcntl(fd, F_SETFD, flags | FD_CLOEXEC) < 0)
    assert(0);

  fprintf(stderr, "%s: about to lock file for writing.\n", process_name);
  if (! lock_file(fd))
  {
    perror("fcntl");
    goto err2;
  }

  fprintf(stderr, "%s: file locking attempt succeeded.\n", process_name);
  if (mmap(NULL, 1, PROT_WRITE, MAP_SHARED, fd, 0) == 0)
  {
    perror("mmap");
    goto err2;
  }

  goto out;

err2:
  close(fd);
err1:
out:
  return fd;
}

int main(int argc, char *argv[])
{
  int fd1;
  int fd2;
  int exitcode = 1;
  char filename[256];

  snprintf(filename, sizeof(filename), "/tmp/valgrind-file-locking-test.%ld",
           (long) getpid());

  unlink(filename);

  if ((fd1 = open_lock_and_map("parent", filename)) >= 0)
  {
    pid_t fork_result;

    fork_result = fork();
    switch (fork_result)
    {
    case -1:
      perror("fork");
      break;

    case 0:
      /* child */
      fd2 = open_lock_and_map("child", filename);
      if (fd2 >= 0)
      {
        close(fd2);
      }
      exit(0);
      break;

    default:
      /* parent */
      {
        int child_status;
        int wait_result;
        struct __wrusage wrusage;
        siginfo_t info;
        wait_result = wait6(P_PID, fork_result, &child_status, WEXITED, &wrusage, &info);
        assert(wait_result >= 0);
      }
    }
  }

  close(fd1);

  unlink(filename);

  fprintf(stderr, "Test finished.\n");

  return exitcode;
}
