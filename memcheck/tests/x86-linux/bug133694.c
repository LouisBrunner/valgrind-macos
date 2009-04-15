#include <stdio.h>
#include <stdlib.h>
#include "tests/sys_mman.h"

int main(int argc, char **argv)
{
   void *a1;
   
   if ((a1 = mmap((void *)0x200000, 4096, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_FIXED|MAP_ANONYMOUS, -1, 0)) == MAP_FAILED)
   {
      perror("mmap1");
      exit(1);
   }

   if (munmap(a1, 4096) < 0)
   {
      perror("munmap1");
      exit(1);
   }
    
   if (mmap((void *)0x100000, 4096, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_FIXED|MAP_ANONYMOUS, -1, 0) == MAP_FAILED)
   {
      perror("mmap2");
      exit(1);
   }
     
   if (mmap((void *)0x100000, 4096, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0) == MAP_FAILED)
   {
      perror("mmap2");
      exit(1);
   }

   printf("success\n");
   exit(0);
}
