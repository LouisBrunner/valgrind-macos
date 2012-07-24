#include<stdio.h>
#include<stdlib.h>
#include<stdint.h>
#include<inttypes.h>
#include<string.h>
#include "table.h"

/* Register contents after executing an TRE insn */
typedef struct {
   uint64_t addr;
   uint64_t len;
   uint64_t tabaddr;
   uint8_t testbyte;
   uint64_t cc;
} tre_regs;

uint8_t buff[40];

tre_regs tre(uint8_t *codepage, uint8_t *addr, uint64_t len, uint8_t test_byte)
{
   int cc;
   tre_regs regs;

   register uint64_t param asm("0") = test_byte;
   register uint64_t a2 asm ("4") = (uint64_t)codepage;
   register uint64_t a1 asm ("2") = (uint64_t)addr;
   register uint64_t l1 asm ("3") = len;

   asm volatile(
                " tre  %1,%2\n"
                " ipm  %0\n"
                " srl  %0,28\n"
		:"=d"(cc),"+&d"(a1)
                :"d"(a2),"d"(param),"d"(l1),"d"(test_byte):  "memory" );

   regs.addr = a1;
   regs.len = l1;
   regs.tabaddr = a2;
   regs.testbyte = param;
   regs.cc = cc;

   return regs;
}

void run_test(void *tran_table, void *srcaddr, uint64_t len, uint8_t test)
{
   tre_regs regs;
   int i;

   regs = tre(tran_table, srcaddr, len, test);

   if ((uint64_t)tran_table != regs.tabaddr)
      printf("translation table address changed\n");
   if (test != regs.testbyte)
      printf("test byte changed\n");
   if ((uint64_t)srcaddr + (len - regs.len) != regs.addr)
      printf("source address/length not updated properly\n");

   printf("Resulting cc is %"PRIu64" and the string is ", regs.cc);
   for ( i = 0; i < len; i++) {
      printf("%c", buff[i]);
   }
   
   printf("\n");
}

int main()
{

   /* Test 1: length = 0 */
   run_test(NULL, NULL, 0, 0x0);
   run_test((char *)&touppercase, &buff, 0, 0x0);
   run_test((char *)&touppercase, &buff, 0, 'b');

   /* Test 2 : length > 0 */
   memset(buff, 'a', 1);
   run_test((char *)&touppercase, &buff, 1, 'a');   //cc = 1
   run_test((char *)&touppercase, &buff, 1, 'b');

   memcpy(buff, "abcdefgh", 8);
   run_test((char *)&touppercase, &buff, 3, 'a');   //cc = 1
   run_test((char *)&touppercase, &buff, 3, 'f');   //cc = 0
   run_test((char *)&touppercase, &buff, 8, 'l');   //cc = 0

   memcpy(buff, "ABCDEFGH", 8);
   run_test((char *)&tolowercase, &buff, 3, 'A');   // cc = 1
   run_test((char *)&tolowercase, &buff, 3, 'C');   // cc = 0
   run_test((char *)&tolowercase, &buff, 8, 0x0);   // cc = 0

   memcpy(buff, "01234567", 8);
   run_test((char *)&touppercase, &buff, 8, 'A');
   run_test((char *)&tolowercase, &buff, 8, 'A');
   return 0;
}
