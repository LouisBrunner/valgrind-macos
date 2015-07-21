/* Test for gethrtime which either issues a classic syscall
   or leverages fasttrap available on Solaris with tscp hwcap. */

#include "config.h"
#include <stdio.h>
#include <sys/time.h>
#include <sys/trap.h>

int main(void)
{
   hrtime_t hrt = gethrtime();
   if (hrt > 0)
      printf("PASS\n");

/* Exercise the fasttrap directly if available. When tscp hwcap
   is not supported, it simply returns NULL. */
#if defined(SOLARIS_GETHRT_FASTTRAP)
#if defined(VGP_x86_solaris)
   __asm__ ( \
      "movl %[FASTTRAP],%%eax\n"
      "int $0xd2\n"
      :
      : [FASTTRAP] "i" (T_GETHRT)
      : "eax", "edx", "cc");
#elif defined(VGP_amd64_solaris)
   __asm__ ( \
      "movq %[FASTTRAP],%%rax\n"
      "int $0xd2\n"
      :
      : [FASTTRAP] "i" (T_GETHRT)
      : "rax", "rdx", "cc");
#else
#  error "Unknown platform"
#endif
#endif /* SOLARIS_GETHRT_FASTTRAP */

   return 0;
}

