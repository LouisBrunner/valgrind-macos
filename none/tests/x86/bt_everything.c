
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>


unsigned int btsl_mem ( unsigned char* base, int bitno )
{
   unsigned char res;
   __asm__ 
   __volatile__("btsl\t%2, %0\n\t"
                "setc\t%1" 
                : "=m" (*base), "=q" (res)
                : "r" (bitno));
   /* Pretty meaningless to dereference base here, but that's what you
      have to do to get a btsl insn which refers to memory starting at
      base. */
   return res;
}

unsigned int btrl_mem ( unsigned char* base, int bitno )
{
   unsigned char res;
   __asm__ 
   __volatile__("btrl\t%2, %0\n\t"
                "setc\t%1" 
                : "=m" (*base), "=q" (res)
                : "r" (bitno));
   return res;
}

unsigned int btcl_mem ( unsigned char* base, int bitno )
{
   unsigned char res;
   __asm__ 
   __volatile__("btcl\t%2, %0\n\t"
                "setc\t%1" 
                : "=m" (*base), "=q" (res)
                : "r" (bitno));
   return res;
}

unsigned int btl_mem ( unsigned char* base, int bitno )
{
   unsigned char res;
   __asm__ 
   __volatile__("btl\t%2, %0\n\t"
                "setc\t%1" 
                : "=m" (*base), "=q" (res)
                : "r" (bitno)
                : "cc", "memory");
   return res;
}




unsigned int btsl_reg ( unsigned int reg_in, int bitno, 
                        unsigned int* reg_out_p )
{
   unsigned char res;
   unsigned int reg_out;
   __asm__ 
   __volatile__("movl\t%3, %%eax\n\t"
                "btsl\t%2, %%eax\n\t"
                "movl\t%%eax, %1\n\t"
                "setc\t%0" 
                : "=q" (res), "=r" (reg_out)
                : "r" (bitno), "r" (reg_in)
                : "cc", "eax");
   *reg_out_p = reg_out;
   return res;
}


unsigned int btrl_reg ( unsigned int reg_in, int bitno, 
                        unsigned int* reg_out_p )
{
   unsigned char res;
   unsigned int reg_out;
   __asm__ 
   __volatile__("movl\t%3, %%eax\n\t"
                "btrl\t%2, %%eax\n\t"
                "movl\t%%eax, %1\n\t"
                "setc\t%0" 
                : "=q" (res), "=r" (reg_out)
                : "r" (bitno), "r" (reg_in)
                : "cc", "eax");
   *reg_out_p = reg_out;
   return res;
}


unsigned int btcl_reg ( unsigned int reg_in, int bitno, 
                        unsigned int* reg_out_p )
{
   unsigned char res;
   unsigned int reg_out;
   __asm__ 
   __volatile__("movl\t%3, %%eax\n\t"
                "btcl\t%2, %%eax\n\t"
                "movl\t%%eax, %1\n\t"
                "setc\t%0" 
                : "=q" (res), "=r" (reg_out)
                : "r" (bitno), "r" (reg_in)
                : "cc", "eax");
   *reg_out_p = reg_out;
   return res;
}


unsigned int btl_reg ( unsigned int reg_in, int bitno, 
                       unsigned int* reg_out_p )
{
   unsigned char res;
   unsigned int reg_out;
   __asm__ 
   __volatile__("movl\t%3, %%eax\n\t"
                "btl\t%2, %%eax\n\t"
                "movl\t%%eax, %1\n\t"
                "setc\t%0" 
                : "=q" (res), "=r" (reg_out)
                : "r" (bitno), "r" (reg_in)
                : "cc", "eax");
   *reg_out_p = reg_out;
   return res;
}







typedef unsigned int UInt;
typedef unsigned char UChar;

UInt rol1 ( UInt x )
{
  return (x << 1) | (x >> 31);
}

int main ( void )
{
   UInt   n, bitoff, op;
   UInt   carrydep, c, res;
   UChar* block;
   UInt   reg;

   /*------------------------ MEM-L -----------------------*/

   carrydep = 0;
   block = calloc(200,1);
   block += 100;
   /* Valid bit offsets are -800 .. 799 inclusive. */

   for (n = 0; n < 10000; n++) {
      bitoff = (random() % 1600) - 800;
      op = random() % 4;
      c = 2;
      switch (op) {
         case 0: c = btsl_mem(block, bitoff); break;
         case 1: c = btrl_mem(block, bitoff); break;
         case 2: c = btcl_mem(block, bitoff); break;
         case 3: c = btl_mem(block, bitoff); break;
      }
      assert(c == 0 || c == 1);
      carrydep = c ? (rol1(carrydep) ^ bitoff) : carrydep;
   }

   /* Compute final result */
   block -= 100;   
   res = 0;
   for (n = 0; n < 200; n++) {
      UChar ch = block[n];
      /* printf("%d ", (int)block[n]); */
      res = rol1(res) ^ (UInt)ch;
   }

   printf("MEM-L: final res 0x%x, carrydep 0x%x\n", res, carrydep);

   /*------------------------ REG-L -----------------------*/

   carrydep = 0;
   reg = 0;

   for (n = 0; n < 1000; n++) {
      bitoff = (random() % 100) - 50;
      op = random() % 4;
      c = 2;
      switch (op) {
         case 0: c = btsl_reg(reg, bitoff, &reg); break;
         case 1: c = btrl_reg(reg, bitoff, &reg); break;
         case 2: c = btcl_reg(reg, bitoff, &reg); break;
         case 3: c = btl_reg(reg, bitoff, &reg); break;
      }
      assert(c == 0 || c == 1);
      carrydep = c ? (rol1(carrydep) ^ bitoff) : carrydep;
   }

   printf("REG-L: final res 0x%x, carrydep 0x%x\n", reg, carrydep);

   block += 100;

   /* Just try one of these at once; more than one can cause a
      confusing merging of error messages. */
   //btsl_mem(block, -800);  /* should not complain */
   //btsl_mem(block, -801);  /* should complain */
   //btsl_mem(block, 799);   /* should not complain */
   //btsl_mem(block, 800);   /* should complain */

   block -= 100;
   free(block);

   return 0;
}

