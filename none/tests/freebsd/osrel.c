#include <unistd.h>
#include <sys/types.h>
#include <sys/sysctl.h>
#include <stdio.h>
#include <sys/param.h>

int main(void)
{
   u_int32_t osrel;
   int name[4];
   name[0] = CTL_KERN;
   name[1] = KERN_PROC;
   name[2] = KERN_PROC_OSREL;
   name[3] = getpid();
   size_t newlen = sizeof(osrel);
   sysctl(name, 4, &osrel, &newlen, NULL, 0U);

   // this doesn't change much for each release
   u_int32_t shortProcOsrel = osrel/1000;
   // this changes with each patch, see https://www.freebsd.org/doc/en_US.ISO8859-1/books/porters-handbook/versions.html
   u_int32_t shortSysparamOsrel = __FreeBSD_version/1000;

   if (shortProcOsrel == shortSysparamOsrel)
   {
       printf("OK osrel values match\n");
   }
   else
   {
       printf("FAIL osrel values different (kernel proc %u compiler %u)\n", shortProcOsrel, shortSysparamOsrel);
   }
}
