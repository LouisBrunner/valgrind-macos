#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>

/* Register contents after executing an TROO insn */
typedef struct {
   uint64_t srcaddr;
   uint64_t len;
   uint64_t desaddr;
   uint64_t tabaddr;
   uint8_t testbyte;
   uint64_t cc;
} troo_regs;

uint8_t tran_table[20] __attribute__((aligned(8))) = {
   0xaa,0xbb,0xcc,0xdd,0xff,0xda,0xbc,0xab,0xca,0xea,0xcc,0xee
};

uint8_t src[20] = {
   0x04,0x01,0x03,0x07,0x08,0x06,0x02,0x05,0x09
};

uint8_t des[20];

troo_regs tr(uint8_t *addr, uint8_t *codepage, uint8_t *dest, uint64_t len,
             uint8_t test)
{
   troo_regs regs;
   register uint64_t test_byte asm("0") = test;
   register uint64_t length asm("3") = len;
   register uint64_t srcaddr asm("4") = (uint64_t)addr;
   register uint64_t codepage2 asm("1") = (uint64_t)codepage;
   register uint64_t desaddr asm("2") = (uint64_t)dest;
   register uint64_t cc asm("5");

   cc = 2;  /* cc result will never be 2 */
   asm volatile(
                " troo %1,%2\n"
                " ipm   %0\n"
                " srl   %0,28\n"
                : "=d"(cc), "+a"(desaddr),
                  "+a"(srcaddr), "+d"(test_byte), "+a"(codepage2), "+a"(length)
                : : "cc", "memory" );

   regs.srcaddr = srcaddr;
   regs.len = length;
   regs.desaddr = desaddr;
   regs.tabaddr = codepage2;
   regs.testbyte = test_byte;
   regs.cc = cc;
   return regs;
}

int run_test(void *srcaddr, void *tableaddr, void *desaddr, uint64_t len,
             uint8_t testbyte)
{
   troo_regs regs;
   int i;

   assert(len <= sizeof src);

   if ((testbyte & 0xff) != testbyte)
      printf("testbyte should be 1 byte only\n");

   regs = tr(srcaddr, tableaddr, desaddr, len, testbyte);

   if ((uint64_t)tableaddr != regs.tabaddr)
      printf("translation table address changed\n");
   if ((uint64_t)srcaddr + (len - regs.len) != regs.srcaddr)
      printf("source address/length not updated properly\n");
   if ((uint64_t)desaddr + (len - regs.len) != regs.desaddr)
      printf("destination address/length not updated properly\n");
   if (regs.cc == 0  && regs.len != 0)
      printf("length is not zero but cc is zero\n");
   printf("%u bytes translated\n", (unsigned)(len - regs.len));
   printf("the translated values are");
   for (i = 0; i < len - regs.len; i++) {
      printf(" %x", des[i]);
   }
   printf("\n");

   return regs.cc;
}

int main()
{
   int cc;
 
   assert(sizeof des >= sizeof src);

   /* Test 1 : len == 0 */
   cc = run_test(NULL, NULL, NULL, 0, 0x0);
   if (cc != 0)
      printf("cc not updated properly:%d", cc);

   cc = run_test(&src, &tran_table, &des, 0, 0xca);
   if (cc != 0)
      printf("cc not updated properly:%d",cc);

   /* Test 2 : len > 0, testbyte not matching */
   cc = run_test(&src, &tran_table, &des, 5, 0xee);
   if (cc != 0)
      printf("cc not updated properly:%d",cc);

   cc = run_test(&src, &tran_table, &des, 10, 0x00);
   if (cc != 0)
      printf("cc not updated properly:%d",cc);

   memset((char *)&des, 0, 10);

   /* Test 3 : len > 0, testbyte matching */
   cc = run_test(&src, &tran_table, &des, 5, 0xff);  /* 1st byte matches */
   if (cc != 1)
      printf("cc not updated properly:%d",cc);

   cc = run_test(&src, &tran_table, &des, 5, 0xbb);  /* 2nd byte matches */
   if (cc != 1)
      printf("cc not updated properly:%d",cc);

   cc = run_test(&src, &tran_table, &des, 10, 0xea);
   if (cc != 1)
      printf("cc not updated properly:%d",cc);

   return 0;
}
