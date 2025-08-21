#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>
#include <string.h>

int
main (void)
{
  char buf[20];
  size_t nbytes;
  int ret;

  /* close stdin */
  close (0);
  /* open /dev/null as new stdin */
  (void)open ("/dev/null", O_RDONLY);
  /* redirect stdout as stderr */
  close (1);
  /* stdout becomes stderr */
  ret = dup (2);

  if (ret == 1) {
    strcpy(buf, "hello world\n");
    nbytes = strlen(buf);

    /* should come out on stderr */
    write (1, buf, nbytes);
  }

  return 0;
}
