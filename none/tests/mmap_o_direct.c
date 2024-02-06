#include <unistd.h>
#define __USE_GNU
#include <fcntl.h>
#include <sys/mman.h>
#include <stddef.h>

int main(void)
{
   int fd = open("mmap_o_direct.c", O_RDONLY | O_DIRECT);
   if (-1 != fd)
   {
      void* m = mmap(NULL, 307, PROT_READ, MAP_PRIVATE, fd, 0);
      munmap(m, 307);
      close(fd);
   }
}
