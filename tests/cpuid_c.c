
#include <stdio.h>

// in cpuid_s.s
extern void get_cpuid0 ( unsigned int* buf );
extern void get_cpuid1 ( unsigned int* buf );

unsigned int buf[4];

int main ( void )
{
   get_cpuid0(&buf[0]);
   printf("cpuid words (0): 0x%x 0x%x 0x%x 0x%x\n", 
          buf[0], buf[1], buf[2], buf[3] );

   get_cpuid1(&buf[0]);
   printf("cpuid words (1): 0x%x 0x%x 0x%x 0x%x\n", 
          buf[0], buf[1], buf[2], buf[3] );

   return 0;
}
