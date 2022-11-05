#include <sys/types.h>
#include <sys/sysctl.h>
#include <sys/elf_common.h>
#include <sys/auxv.h>
#include <unistd.h>
#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>
#include "../../../config.h"

int main(void)
{
#if (FREEBSD_VERS >= FREEBSD_13_0)
   unsigned long ul_ps_strings;
   struct ps_strings* v1;
   struct ps_strings* v2;
   struct ps_strings* v3;
   int name[] = {CTL_KERN, KERN_PS_STRINGS};

   size_t struct_len = sizeof(ul_ps_strings);

   if (sysctlbyname("kern.ps_strings", &ul_ps_strings, &struct_len, NULL, 0) < 0) {
      perror("sysctlbyname failed:");
      exit(-1);
   }
   v1 = (struct ps_strings*)ul_ps_strings;
   
   (void)elf_aux_info(AT_PS_STRINGS, &v2, sizeof(v2));
   
   if (sysctl(name, 2, &ul_ps_strings, &struct_len, NULL, 0) < 0) {
      perror("sysctl failed:");
      exit(-1);
   }
   v3 = (struct ps_strings*)ul_ps_strings;
   
   if (v1 == v2 && v1 == v2) {
       printf("OK\n");
   } else {
       printf("FAIL ps_strings different\n");
       printf("v1 %p v2 %p v3 %p\n", v1, v2, v3);
   }
#else
    printf("OK\n");
#endif
   
   setproctitle("foo %s %d", "bar", 42);
}

