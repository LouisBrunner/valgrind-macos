#include <sys/syscall.h>
#include <errno.h>
#include <unistd.h>
#include <stddef.h>

int main(int argc, char **argv)
{
   int has_execveat = 0;
#if defined(__NR_execveat)
   errno = 0;
   syscall(__NR_execveat, 0, NULL, 0, 0, 0);
   has_execveat = (errno != ENOSYS);
#else
   has_execveat = 0;
#endif

   return has_execveat ? 0 : 1;
}
