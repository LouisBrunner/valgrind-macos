#define _GNU_SOURCE 1
#include <sys/mman.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

int main(void)
{
   /* first  find free segment of 40K, then unmap it */
   void *initial_area = mmap((void *)0x10000000, 40960, PROT_READ|PROT_WRITE,
                             MAP_ANONYMOUS|MAP_PRIVATE,0,0);
   
   if (initial_area == MAP_FAILED)
      perror ("initial area");
   printf("initial_area= %p\n", initial_area);
   if (munmap(initial_area, 40960) != 0)
      perror ("munmap initial_area");

   /* remap the same segment, but with 4K size */
   void *area = mmap(initial_area, 4096, PROT_READ|PROT_WRITE,
                     MAP_ANONYMOUS|MAP_PRIVATE,0,0);
   if (area == MAP_FAILED)
      perror ("area");
   if (area != initial_area)
      printf("FAILED : was expecting to get back the initial_area\n");
   printf("area= %p\n", area);
   strcpy(area, "Hello World");

   /* extend it to 40K */
   void *a2 = mremap(area, 4096, 40960, 0);
   if (a2 == MAP_FAILED) {
      perror("mremap");
   }
   if (a2 != initial_area)
      printf("FAILED : was expecting to get back the same area increased\n");
   printf("increased area= %p\n", a2);
   printf("%s\n", (char *)a2);
   return 0;
}
