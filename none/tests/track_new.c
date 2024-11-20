#include <fcntl.h>
#include <stdio.h>
#include <unistd.h>

int
main ()
{
  int oldfd = open ("foobar.txt", O_RDWR|O_CREAT, S_IRUSR | S_IWUSR);
  /*... do something with oldfd ...*/
  close (oldfd);

  /* Lets open another file... */
  int newfd = open ("foobad.txt", O_RDWR|O_CREAT, S_IRUSR | S_IWUSR);
  /* ... oops we are using the wrong fd (but same number...) */
  dprintf (oldfd, "some new text\n");

  close (newfd);
  return 0;
}
