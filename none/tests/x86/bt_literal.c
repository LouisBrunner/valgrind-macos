
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

typedef unsigned int UInt;

/* Given a word, do bt/bts/btr/btc on bits 0, 1, 2 and 3 of it, and
   also reconstruct the original bits 0, 1, 2, 3 by looking at the
   carry flag.  Returned result has mashed bits 0-3 at the bottom and
   the reconstructed original bits 0-3 as 4-7. */
UInt mash_reg_L ( UInt orig )
{
  UInt reconstructed, mashed;
  __asm__ __volatile__ (
     "movl %2, %%edx\n\t"
     ""
     "movl $0, %%eax\n\t"
     "\n\t"
     "btl  $0, %%edx\n\t"
     "setb %%cl\n\t"
     "movzbl %%cl, %%ecx\n\t"
     "orl %%ecx, %%eax\n\t"
     "\n\t"
     "btsl $1, %%edx\n\t"
     "setb %%cl\n\t"
     "movzbl %%cl, %%ecx\n\t"
     "shll $1, %%ecx\n\t"
     "orl %%ecx, %%eax\n\t"
     "\n\t"
     "btrl $2, %%edx\n\t"
     "setb %%cl\n\t"
     "movzbl %%cl, %%ecx\n\t"
     "shll $2, %%ecx\n\t"
     "orl %%ecx, %%eax\n\t"
     "\n\t"
     "btcl $3, %%edx\n\t"
     "setb %%cl\n\t"
     "movzbl %%cl, %%ecx\n\t"
     "shll $3, %%ecx\n\t"
     "orl %%ecx, %%eax\n\t"
     "\n\t"
     "movl %%eax, %0\n\t"
     "movl %%edx, %1"

     : "=r" (reconstructed), "=r" (mashed)
     : "r" (orig)
     : "eax", "ecx", "edx", "cc");
  return (mashed & 0xF) | ((reconstructed & 0xF) << 4);
}




UInt mash_mem_L ( int* origp )
{
  UInt reconstructed, mashed;
  __asm__ __volatile__ (
     "movl %2, %%edx\n\t"
     ""
     "movl $0, %%eax\n\t"
     "\n\t"
     "btl  $0, (%%edx)\n\t"
     "setb %%cl\n\t"
     "movzbl %%cl, %%ecx\n\t"
     "orl %%ecx, %%eax\n\t"
     "\n\t"
     "btsl $1, (%%edx)\n\t"
     "setb %%cl\n\t"
     "movzbl %%cl, %%ecx\n\t"
     "shll $1, %%ecx\n\t"
     "orl %%ecx, %%eax\n\t"
     "\n\t"
     "btrl $2, (%%edx)\n\t"
     "setb %%cl\n\t"
     "movzbl %%cl, %%ecx\n\t"
     "shll $2, %%ecx\n\t"
     "orl %%ecx, %%eax\n\t"
     "\n\t"
     "btcl $3, (%%edx)\n\t"
     "setb %%cl\n\t"
     "movzbl %%cl, %%ecx\n\t"
     "shll $3, %%ecx\n\t"
     "orl %%ecx, %%eax\n\t"
     "\n\t"
     "movl %%eax, %0\n\t"
     "movl (%%edx), %1"

     : "=r" (reconstructed), "=r" (mashed)
     : "r" (origp)
     : "eax", "ecx", "edx", "cc");
  return (mashed & 0xF) | ((reconstructed & 0xF) << 4);
}



UInt mash_reg_W ( UInt orig )
{
  UInt reconstructed, mashed;
  __asm__ __volatile__ (
     "movl %2, %%edx\n\t"
     ""
     "movl $0, %%eax\n\t"
     "\n\t"
     "btw  $0, %%dx\n\t"
     "setb %%cl\n\t"
     "movzbl %%cl, %%ecx\n\t"
     "orl %%ecx, %%eax\n\t"
     "\n\t"
     "btsw $1, %%dx\n\t"
     "setb %%cl\n\t"
     "movzbl %%cl, %%ecx\n\t"
     "shll $1, %%ecx\n\t"
     "orl %%ecx, %%eax\n\t"
     "\n\t"
     "btrw $2, %%dx\n\t"
     "setb %%cl\n\t"
     "movzbl %%cl, %%ecx\n\t"
     "shll $2, %%ecx\n\t"
     "orl %%ecx, %%eax\n\t"
     "\n\t"
     "btcw $3, %%dx\n\t"
     "setb %%cl\n\t"
     "movzbl %%cl, %%ecx\n\t"
     "shll $3, %%ecx\n\t"
     "orl %%ecx, %%eax\n\t"
     "\n\t"
     "movl %%eax, %0\n\t"
     "movl %%edx, %1"

     : "=r" (reconstructed), "=r" (mashed)
     : "r" (orig)
     : "eax", "ecx", "edx", "cc");
  return (mashed & 0xF) | ((reconstructed & 0xF) << 4);
}




int main ( void )
{
  int i, ii;
  for (i = 0; i < 0x10; i++) {
    ii = i;
    printf("0x%x -> 0x%2x 0x%2x 0x%2x\n", i, 
           mash_reg_L(i), mash_mem_L(&ii), mash_reg_W(i));
  }
  return 1;
}

