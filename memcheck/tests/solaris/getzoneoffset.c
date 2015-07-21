/* Test for get_zone_offset fasttrap. */

#include "config.h"
#include <stdio.h>
#include <sys/time.h>
#include <sys/trap.h>

int main(void)
{
#if defined(VGP_x86_solaris)
   __asm__ ( \
      "movl %[FASTTRAP],%%eax\n"
      "int $0xd2\n"
      :
      : [FASTTRAP] "i" (T_GETZONEOFFSET)
      : "eax", "edx", "cc");
#elif defined(VGP_amd64_solaris)
   __asm__ ( \
      "movq %[FASTTRAP],%%rax\n"
      "int $0xd2\n"
      :
      : [FASTTRAP] "i" (T_GETZONEOFFSET)
      : "rax", "rdx", "cc");
#else
#  error "Unknown platform"
#endif

   return 0;
}

