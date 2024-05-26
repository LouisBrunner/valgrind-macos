#include <assert.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

int main(void)
{
   char buf[64] __attribute__((aligned(64)));
   unsigned long check_dc_zva;
   unsigned long check_dc_cvap;
   memset(buf, 0xAA, 64);
   asm volatile("mrs %0, dczid_el0" : "=r" (check_dc_zva));
   asm volatile("dc cvac, %0" :: "r" (buf));
   asm volatile("dc cvau, %0" :: "r" (buf));
   asm volatile("mrs %0, id_aa64isar1_el1" : "=r" (check_dc_cvap));
   if (check_dc_cvap & 0x3) {
      asm volatile("add x8, sp, #0x40\n"
                   ".inst 0xd50b7c28\n");
   }
   if (check_dc_cvap & 0x2) {
      asm volatile("add x8, sp, #0x40\n"
                   ".inst 0xd50b7c28");
   }
   asm volatile("dc civac, %0" :: "r" (buf));
   if (!(check_dc_zva & 0x10)) {
      size_t buf_size = (1U << ((check_dc_zva & 0xf) + 2));
      //printf("lg 2 word count %lu byte count %zu\n", check_dc_zva & 0xfUL, buf_size);
      char* var_buf;
      if (posix_memalign((void**)&var_buf, buf_size, buf_size) == 0) {
         asm volatile("dc zva, %0" :: "r" (var_buf));
         assert(var_buf[0] == 0);
         assert(var_buf[buf_size - 1] == 0);
         free(var_buf);
      } else {
         perror("posix_memalign failed:");
         exit(1);
      }
   }
}
