#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>

int
main (void)
{
  int oldfd = open ("foobar.txt", O_RDWR|O_CREAT, S_IRUSR | S_IWUSR);
  /*... do something with oldfd ...*/
  close (oldfd);

  /* Lets open another file... */
  int newfd = open ("foobad.txt", O_RDWR|O_CREAT, S_IRUSR | S_IWUSR);
  /* ... oops we are using the wrong fd (but same number...) */
  write(oldfd, "some new text\n", 14);

  close (newfd);
  return 0;
}
