 
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

int main ( void )
{
  int fd, n;
  char buf[10];
  fd = open("foo/bar/xyzzy", O_RDONLY); /* fails */
  printf("fd = %d\n", fd);
  n = read ( fd, buf, 10 );
  printf ("n = %d\n", n);
  return 0;
}
