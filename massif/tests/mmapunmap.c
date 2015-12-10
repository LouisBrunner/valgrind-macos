#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include "tests/sys_mman.h"

int main()
{
   void *m;

   m = mmap(NULL, 80 * 1000 * 1024,
            PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS,
            -1, 0);
   munmap(m, 80 * 1000 * 1024);
   return 0;
}
