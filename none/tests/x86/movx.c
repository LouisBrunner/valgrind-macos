
#include <stdio.h>

static int movzbw_1 ( void )
{
   int res;
   __asm__ __volatile__(
      "movl $0x12345678, %%eax\n\t"
      "movb $0x22, %%al\n\t"
      "movzbw %%al,%%ax\n\t"
      "mov %%eax, %0"
      : "=r"(res) : : "eax"
   );
   return res;
}

static int movzbw_2 ( void )
{
   int res;
   __asm__ __volatile__(
      "movl $0x12345678, %%eax\n\t"
      "movb $0x99, %%al\n\t"
      "movzbw %%al,%%ax\n\t"
      "mov %%eax, %0"
      : "=r"(res) : : "eax"
   );
   return res;
}

static int movzbl_1 ( void )
{
   int res;
   __asm__ __volatile__(
      "movl $0x12345678, %%eax\n\t"
      "movb $0x22, %%al\n\t"
      "movzbl %%al,%%eax\n\t"
      "mov %%eax, %0"
      : "=r"(res) : : "eax"
   );
   return res;
}

static int movzbl_2 ( void )
{
   int res;
   __asm__ __volatile__(
      "movl $0x12345678, %%eax\n\t"
      "movb $0x99, %%al\n\t"
      "movzbl %%al,%%eax\n\t"
      "mov %%eax, %0"
      : "=r"(res) : : "eax"
   );
   return res;
}

static int movzwl_1 ( void )
{
   int res;
   __asm__ __volatile__(
      "movl $0x12345678, %%eax\n\t"
      "movw $0x2222, %%ax\n\t"
      "movzwl %%ax,%%eax\n\t"
      "mov %%eax, %0"
      : "=r"(res) : : "eax"
   );
   return res;
}

static int movzwl_2 ( void )
{
   int res;
   __asm__ __volatile__(
      "movl $0x12345678, %%eax\n\t"
      "movw $0x9999, %%ax\n\t"
      "movzwl %%ax,%%eax\n\t"
      "mov %%eax, %0"
      : "=r"(res) : : "eax"
   );
   return res;
}

static int movsbw_1 ( void )
{
   int res;
   __asm__ __volatile__(
      "movl $0x12345678, %%eax\n\t"
      "movb $0x22, %%al\n\t"
      "movsbw %%al,%%ax\n\t"
      "mov %%eax, %0"
      : "=r"(res) : : "eax"
   );
   return res;
}

static int movsbw_2 ( void )
{
   int res;
   __asm__ __volatile__(
      "movl $0x12345678, %%eax\n\t"
      "movb $0x99, %%al\n\t"
      "movsbw %%al,%%ax\n\t"
      "mov %%eax, %0"
      : "=r"(res) : : "eax"
   );
   return res;
}

static int movsbl_1 ( void )
{
   int res;
   __asm__ __volatile__(
      "movl $0x12345678, %%eax\n\t"
      "movb $0x22, %%al\n\t"
      "movsbl %%al,%%eax\n\t"
      "mov %%eax, %0"
      : "=r"(res) : : "eax"
   );
   return res;
}

static int movsbl_2 ( void )
{
   int res;
   __asm__ __volatile__(
      "movl $0x12345678, %%eax\n\t"
      "movb $0x99, %%al\n\t"
      "movsbl %%al,%%eax\n\t"
      "mov %%eax, %0"
      : "=r"(res) : : "eax"
   );
   return res;
}

static int movswl_1 ( void )
{
   int res;
   __asm__ __volatile__(
      "movl $0x12345678, %%eax\n\t"
      "movw $0x2222, %%ax\n\t"
      "movswl %%ax,%%eax\n\t"
      "mov %%eax, %0"
      : "=r"(res) : : "eax"
   );
   return res;
}

static int movswl_2 ( void )
{
   int res;
   __asm__ __volatile__(
      "movl $0x12345678, %%eax\n\t"
      "movw $0x9999, %%ax\n\t"
      "movswl %%ax,%%eax\n\t"
      "mov %%eax, %0"
      : "=r"(res) : : "eax"
   );
   return res;
}



int main ( void )
{
   printf("%8s 0x%08x\n", "movzbw_1", movzbw_1());
   printf("%8s 0x%08x\n", "movzbw_2", movzbw_2());
   printf("%8s 0x%08x\n", "movzbl_1", movzbl_1());
   printf("%8s 0x%08x\n", "movzbl_2", movzbl_2());
   printf("%8s 0x%08x\n", "movzwl_1", movzwl_1());
   printf("%8s 0x%08x\n", "movzwl_2", movzwl_2());
   printf("%8s 0x%08x\n", "movsbw_1", movsbw_1());
   printf("%8s 0x%08x\n", "movsbw_2", movsbw_2());
   printf("%8s 0x%08x\n", "movsbl_1", movsbl_1());
   printf("%8s 0x%08x\n", "movsbl_2", movsbl_2());
   printf("%8s 0x%08x\n", "movswl_1", movswl_1());
   printf("%8s 0x%08x\n", "movswl_2", movswl_2());
   return 0;
}
