#include <sys/syscall.h>
#include <errno.h>
#include <unistd.h>
#include <stddef.h>

int main(int argc, char **argv)
{
   int has_preadv2 = 0;
   int has_pwritev2 = 0;
#if defined(__NR_preadv2)
   errno = 0;
   syscall(__NR_preadv2, 0, NULL, 0, 0, 0);
   has_preadv2 = errno != ENOSYS;
#else
   has_preadv2 = 0;
#endif

#if defined(__NR_pwritev2)
   errno = 0;
   syscall(__NR_pwritev2, 0, NULL, 0, 0, 0);
   has_pwritev2 = errno != ENOSYS;
#else
   has_pwritev2 = 0;
#endif

   return !(has_preadv2 && has_pwritev2);
}
