#include <stdio.h>

int main()
{
   unsigned long p;
   register unsigned long *msg = &p;

   /* Load "hi\n\0" into P; then convert the 'i' into an 'h' */
   __asm__ volatile ( "iihl %[p],0x0a00\n\t" 
                      "iihh %[p],0x6869\n\t" 
                      "nihh %[p],0x6868\n\t" : [p] "+d" (p) : : "cc");

   /* Write P to stdout; should read "hh\n" */
   printf("%s", (char *)msg);

   return 0;
}
