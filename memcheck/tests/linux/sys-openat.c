
/* The bug that gave rise to this is
   https://bugs.kde.org/show_bug.cgi?id=307103
*/

#define _GNU_SOURCE
#include <fcntl.h>
#include <unistd.h>
int main (void)
{ 
  int dfd = open ("/tmp", O_RDONLY);
  __attribute__((unused)) int fd1 = openat (dfd, "abc", O_RDONLY); 
  /* This is fine, absolute path. */ 
  __attribute__((unused)) int fd2 = openat (0x12345678, "/tmp/abc", O_RDONLY); 
  __attribute__((unused)) int fd3 = openat (AT_FDCWD, "abc", O_RDONLY); 
  /* This is the only one that should warn. */ 
  __attribute__((unused)) int fd4 = openat (0x12345678, "abc", O_RDONLY); 
  return 0;
} 
