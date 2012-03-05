#include<stdio.h>
#include<stdlib.h>
#include<asm/types.h>
#include<stdint.h>
#include<string.h>
#include "table.h"

uint8_t buff[40];

void tr(uint8_t *codepage, uint8_t *addr, uint64_t len)
{
   asm volatile(
                "   larl    1,1f\n"
                "1: tr      0(1,%0),0(%2)\n"
                "   ex      %1,0(1)"
                : "+&a" (addr), "+a" (len)
                : "a" (codepage) : "cc", "memory", "1");
}

void run_test(void *tran_table, void *srcaddr, uint64_t len)
{
   int i;

   tr(tran_table, buff, len);
   printf("the translated string is ");
   for (i = 0; i < len; i++) {
      printf("%c", buff[i]);
   }
   printf("\n");
}

int main()
{
   /* Test 1: length = 0 */
   run_test((char *)&touppercase, &buff, 0);
   run_test((char *)&touppercase, &buff, 0);

   /* Test 2 : length > 0 */
   memset(buff, 'a', 1);
   run_test((char *)&touppercase, &buff, 1);

   memcpy(buff, "abcdefgh", 8);
   run_test((char *)&touppercase, &buff, 3);
   run_test((char *)&touppercase, &buff, 3);
   run_test((char *)&touppercase, &buff, 8);

   memcpy(buff, "ABCDEFGH", 8);
   run_test((char *)&tolowercase, &buff, 3);
   run_test((char *)&tolowercase, &buff, 3);
   run_test((char *)&tolowercase, &buff, 8);

   memcpy(buff, "0123456789", 9);
   run_test((char *)&touppercase, &buff, 9);
   run_test((char *)&tolowercase, &buff, 9);
   return 0;
}
