#include<stdio.h>
#include<stdint.h>
#include<string.h>
#include<assert.h>

/* Register contents after executing an TRTT insn */
typedef struct {
   uint64_t srcaddr;
   uint64_t len;
   uint64_t desaddr;
   uint64_t tabaddr;
   uint16_t testbyte;
   uint64_t cc;
} trtt_regs;

uint16_t tran_table[40] = {
   0xaaaa,0xcccc,0xcccc,0xdddd,0xffff,0xdada,0xbcbc,0xabab,0xcaca,0xeaea,
   0xbbbb,0xeeee
};

uint16_t src[40] = {
   0x4,0x03,0x04,0x02,0x07,0x08,0x06,0x02,0x05,0x09,0xa
};

uint16_t des[20];

trtt_regs tr(uint16_t *addr, uint16_t *codepage, uint16_t *dest, uint64_t len,
             uint16_t test)
{
   trtt_regs regs;
   register uint64_t test_byte asm("0") = test;
   register uint64_t length asm("3") = len;
   register uint64_t srcaddr asm("4") = (uint64_t)addr;
   register uint64_t codepage2 asm("1") = (uint64_t)codepage;
   register uint64_t desaddr asm("2") = (uint64_t)dest;
   register uint64_t cc asm("5");

   cc = 2;  /* cc result will never be 2 */
   asm volatile(
                " trtt  %1,%2\n"
                " ipm   %0\n"
                " srl   %0,28\n"
                : "=d"(cc),"+d"(desaddr),"+d"(srcaddr)
                : "d"(test_byte),"d" (codepage2),"d"(length)
                : "memory" );

   regs.srcaddr = srcaddr;
   regs.len = length;
   regs.desaddr = desaddr;
   regs.tabaddr = codepage2;
   regs.testbyte = test_byte;
   regs.cc = cc;

   return regs;
}

int run_test(void *srcaddr, void *tableaddr, void *desaddr, uint64_t len,
             uint16_t testbyte)
{
   trtt_regs regs;
   int i;

   assert(len <= sizeof src);

   if ((testbyte & 0xffff) != testbyte)
      printf("testbyte should be 2 byte only\n");

   regs = tr(srcaddr, tableaddr, desaddr, len, testbyte);

   if ((uint64_t)tableaddr != regs.tabaddr)
      printf("translation table address changed\n");
   if ((uint64_t)srcaddr + (len - regs.len) != regs.srcaddr)
      printf("source address/length not updated properly\n");
   if ((uint64_t)desaddr + (len - regs.len) != regs.desaddr)
      printf("destination address/length not updated properly\n");
   if (regs.cc == 0  && regs.len != 0)
      printf("length is not zero but cc is zero\n");
   printf("%u bytes translated\n", ((unsigned)(len - regs.len))/2);
   printf("the translated values is");
   for (i = 0; i < len/2; i++) {
      printf(" %hx", des[i]);
   }
   printf("\n");

   return regs.cc;
}


int main()
{
   int cc;

   assert(sizeof des <= sizeof src);

   /* Test 1 : len == 0 */
   cc = run_test(NULL, NULL, NULL, 0, 0x0);
   if (cc != 0)
      printf("cc not updated properly:%d", cc);

   cc = run_test(&src, &tran_table, &des, 0, 0x0);
   if (cc != 0)
      printf("cc not updated properly:%d",cc);

   cc = run_test(&src, &tran_table, &des, 0, 0xcaca);
   if (cc != 0)
      printf("cc not updated properly:%d",cc);

   /* Test 2 : len > 0, testbyte not matching */
   cc = run_test(&src, &tran_table, &des, 4, 0xdada);
   if (cc != 0)
      printf("cc not updated properly:%d",cc);

   cc = run_test(&src, &tran_table, &des, 10, 0x00);
   if (cc != 0)
      printf("cc not updated properly:%d",cc);

   memset((uint16_t *)&des, 0, 10);

   /* Test 3 : len > 0 , testbyte matching */
   cc = run_test(&src, &tran_table, &des, 10, 0xffff);
   if (cc != 1)
      printf("cc not updated properly:%d",cc);

   cc = run_test(&src, &tran_table, &des, 10, 0xcccc);
   if (cc != 1)
      printf("cc not updated properly:%d",cc);

   cc = run_test(&src, &tran_table, &des, 20, 0xeaea);
   if (cc != 1)
      printf("cc not updated properly:%d",cc);

   return 0;
}
