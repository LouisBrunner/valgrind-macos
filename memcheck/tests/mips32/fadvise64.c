#include <errno.h>
#include <fcntl.h>
#include <stdio.h>

#define BAD_FD 42

int main() {
   int x;

   (void)posix_fadvise(x,      1, 2, POSIX_FADV_NORMAL);
   (void)posix_fadvise(BAD_FD, x, 2, POSIX_FADV_NORMAL);
   (void)posix_fadvise(BAD_FD, 1, x, POSIX_FADV_NORMAL);
   (void)posix_fadvise(BAD_FD, 1, 2, x);

   x = posix_fadvise(BAD_FD, 1, 2, POSIX_FADV_NORMAL);

   if (x != EBADF)
      fprintf(stderr, "Unexpected return value: %d\n", x);

   return 0;
}
