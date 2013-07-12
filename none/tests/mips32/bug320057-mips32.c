#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include "tests/sys_mman.h"
#include <errno.h>
#include <stdio.h>
#include <string.h>

/* Test case supplied by Vasile Floroiu. */

#define DO(cmd) printf(#cmd "; status: %s\n", strerror(errno))
#define SZ 48216 + 1024

int main()
{
   int fd;

   fd = shm_open("/hw_mngr.c", (O_CREAT | O_EXCL | O_RDWR),
                 (S_IREAD | S_IWRITE));
   DO(shm_open());
   {
      void *ptr;
      ftruncate(fd, SZ);
      DO(ftruncate(fd, SZ));

      ptr = mmap(0, SZ, (PROT_READ | PROT_WRITE), MAP_SHARED, fd, 0);
      DO(mmap());

      munmap(ptr, SZ);
      DO(munmap());
   }
   shm_unlink("/hw_mngr.c");
   DO(shm_unlink());
   return 0;
}
