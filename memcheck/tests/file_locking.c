/** Test program for POSIX advisory record locking. See also #164669
 *  (http://bugs.kde.org/show_bug.cgi?id=164669).
 *  See also http://www.opengroup.org/onlinepubs/007908799/xsh/fcntl.html.
 */


#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
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


int main(int argc, char *argv[]) 
{
  int fd1;
  int fd2;
  int exitcode = 1;
  char filename[256];

  snprintf(filename, sizeof(filename), "/tmp/valgrind-file-locking-test.%d",
           getpid());

  unlink(filename);

  if ((fd1 = open(filename, O_RDWR | O_CREAT)) >= 0)
  {
    fprintf(stderr, "About to lock file for writing.\n");
    if (lock_file(fd1))
    {
      fprintf(stderr, "First locking attempt succeeded.\n");
      if ((fd2 = open(filename, O_RDWR)) >= 0)
      {
        if (! lock_file(fd2))
        {
          fprintf(stderr, "Second locking attempt failed.\n");
          exitcode = 0;
        }
        else
        {
          fprintf(stderr, "ERROR: second lock attempt succeeded !\n");
        }
        close(fd2);
      }
      else
      {
        perror("second open call");
      }
    }
    else
    {
      perror("first locking attempt");
    }
    close(fd1);
  }
  else
  {
    perror("first open call");
  }

  unlink(filename);

  fprintf(stderr, "Test finished successfully.\n");

  return exitcode;
}
