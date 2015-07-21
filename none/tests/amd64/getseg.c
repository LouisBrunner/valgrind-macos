/* Test segment register getting. */

#include <stdio.h>

int main(void)
{
   unsigned short csw = -1;
   unsigned int csl = -1;
   unsigned long csq = -1;
   unsigned short csw_mem = -1;

   __asm__ __volatile__ (
       "movw %%cs, %0\n" /* mov segReg, r16 */
       "movl %%cs, %1\n" /* mov segReg, r32 */
       "movq %%cs, %2\n" /* mov segReg, r64 */
       "movw %%cs, %3\n" /* mov segReg, mem16 */
       : "=r" (csw), "=r" (csl), "=r" (csq), "=m" (csw_mem));

   printf("cs(w)=%u\n", csw);
   printf("cs(l)=%u\n", csl);
   printf("cs(q)=%lu\n", csq);
   printf("cs(w_mem)=%u\n", csw_mem);

   return 0;
}
