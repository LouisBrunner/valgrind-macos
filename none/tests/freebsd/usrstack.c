#include <sys/types.h>
#include <sys/sysctl.h>
#include <unistd.h>
#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>

int main(void)
{
   int stack_variable = 1;
   unsigned long v1;
   unsigned long v2;
   int name[] = {CTL_KERN, KERN_USRSTACK};
   size_t len = sizeof(unsigned long);

   if (sysctlbyname("kern.usrstack", &v1, &len, NULL, 0) < 0) {
      perror("sysctlbyname failed:");
      exit(-1);
   }
   
   if (sysctl(name, 2, &v2, &len, NULL, 0) < 0) {
      perror("sysctl failed:");
      exit(-1);
   }
   
   if (v1 == v2) {
       printf("OK 1\n");
   } else {
       printf("FAIL usrstack different\n");
       printf("v1 %lx v2 %lx\n", v1, v2);
   }


   /* the purpose of this is to check that the sysctl isn't
      returning the host stack. I expec the difference to be less than 0x1000 */
   if (v1 - (unsigned long)&stack_variable > 0x2000U) {
      printf("FAIL usrstack fishy\n");
      printf("v1 %lx stack_variable %p diff %lx\n", v1, &stack_variable,  v1 - (unsigned long)&stack_variable);
   } else {
      printf("OK 2\n");
   }
}

