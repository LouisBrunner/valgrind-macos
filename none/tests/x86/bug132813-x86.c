
#include <stdio.h>

void do_pushb_pos ( void )
{
   unsigned int block[3];
   __asm__ __volatile__ (
      "movl  %0, %%edx\n\t"
      "pushl $0x55555555\n\t"
      "movl  %%esp, 0(%%edx)\n\t"
      ".byte 0x6A,0x22\n\t"
      "movl  %%esp, 4(%%edx)\n\t"
      "popl  %%eax\n\t"
      "movl  %%eax,8(%%edx)\n\t"
      "movl  0(%%edx),%%esp\n\t"
      "addl  $4, %%esp"
      : : "r"(&block) : "eax","edx","cc","memory"
    );
    printf("  pushb_pos: delta %d, top32 0x%08x\n",
           block[0] - block[1], block[2]);
}

void do_pushb_neg ( void )
{
   unsigned int block[3];
   __asm__ __volatile__ (
      "movl  %0, %%edx\n\t"
      "pushl $0x55555555\n\t"
      "movl  %%esp, 0(%%edx)\n\t"
      ".byte 0x6A,0xEE\n\t"
      "movl  %%esp, 4(%%edx)\n\t"
      "popl  %%eax\n\t"
      "movl  %%eax,8(%%edx)\n\t"
      "movl  0(%%edx),%%esp\n\t"
      "addl  $4, %%esp"
      : : "r"(&block) : "eax","edx","cc","memory"
    );
    printf("  pushb_neg: delta %d, top32 0x%08x\n",
           block[0] - block[1], block[2]);
}

void do_pushw_pos ( void )
{
   unsigned int block[3];
   __asm__ __volatile__ (
      "movl  %0, %%edx\n\t"
      "pushl $0x55555555\n\t"
      "movl  %%esp, 0(%%edx)\n\t"
      "pushw $0x3344\n\t"
      "movl  %%esp, 4(%%edx)\n\t"
      "popl  %%eax\n\t"
      "movl  %%eax,8(%%edx)\n\t"
      "movl  0(%%edx),%%esp\n\t"
      "addl  $4, %%esp"
      : : "r"(&block) : "eax","edx","cc","memory"
    );
    printf("  pushw_neg: delta %d, top32 0x%08x\n",
           block[0] - block[1], block[2]);
}

void do_pushw_neg ( void )
{
   unsigned int block[3];
   __asm__ __volatile__ (
      "movl  %0, %%edx\n\t"
      "pushl $0x55555555\n\t"
      "movl  %%esp, 0(%%edx)\n\t"
      "pushw $0xDDCC\n\t"
      "movl  %%esp, 4(%%edx)\n\t"
      "popl  %%eax\n\t"
      "movl  %%eax,8(%%edx)\n\t"
      "movl  0(%%edx),%%esp\n\t"
      "addl  $4, %%esp"
      : : "r"(&block) : "eax","edx","cc","memory"
    );
    printf("  pushw_pos: delta %d, top32 0x%08x\n",
           block[0] - block[1], block[2]);
}

void do_pushl_pos ( void )
{
   unsigned int block[3];
   __asm__ __volatile__ (
      "movl  %0, %%edx\n\t"
      "pushl $0x55555555\n\t"
      "movl  %%esp, 0(%%edx)\n\t"
      "pushl $0x67675656\n\t"
      "movl  %%esp, 4(%%edx)\n\t"
      "popl  %%eax\n\t"
      "movl  %%eax,8(%%edx)\n\t"
      "movl  0(%%edx),%%esp\n\t"
      "addl  $4, %%esp"
      : : "r"(&block) : "eax","edx","cc","memory"
    );
    printf("  pushl_pos: delta %d, top32 0x%08x\n",
           block[0] - block[1], block[2]);
}

void do_pushl_neg ( void )
{
   unsigned int block[3];
   __asm__ __volatile__ (
      "movl  %0, %%edx\n\t"
      "pushl $0x55555555\n\t"
      "movl  %%esp, 0(%%edx)\n\t"
      "pushl $0x98988787\n\t"
      "movl  %%esp, 4(%%edx)\n\t"
      "popl  %%eax\n\t"
      "movl  %%eax,8(%%edx)\n\t"
      "movl  0(%%edx),%%esp\n\t"
      "addl  $4, %%esp"
      : : "r"(&block) : "eax","edx","cc","memory"
    );
    printf("  pushl_neg: delta %d, top32 0x%08x\n",
           block[0] - block[1], block[2]);
}


void do_66pushb_pos ( void )
{
   unsigned int block[3];
   __asm__ __volatile__ (
      "movl  %0, %%edx\n\t"
      "pushl $0x55555555\n\t"
      "movl  %%esp, 0(%%edx)\n\t"
      ".byte 0x66,0x6A,0x22\n\t"
      "movl  %%esp, 4(%%edx)\n\t"
      "popl  %%eax\n\t"
      "movl  %%eax,8(%%edx)\n\t"
      "movl  0(%%edx),%%esp\n\t"
      "addl  $4, %%esp"
      : : "r"(&block) : "eax","edx","cc","memory"
    );
    printf("66pushb_pos: delta %d, top32 0x%08x\n",
           block[0] - block[1], block[2]);
}

void do_66pushb_neg ( void )
{
   unsigned int block[3];
   __asm__ __volatile__ (
      "movl  %0, %%edx\n\t"
      "pushl $0x55555555\n\t"
      "movl  %%esp, 0(%%edx)\n\t"
      ".byte 0x66,0x6A,0xEE\n\t"
      "movl  %%esp, 4(%%edx)\n\t"
      "popl  %%eax\n\t"
      "movl  %%eax,8(%%edx)\n\t"
      "movl  0(%%edx),%%esp\n\t"
      "addl  $4, %%esp"
      : : "r"(&block) : "eax","edx","cc","memory"
    );
    printf("66pushb_neg: delta %d, top32 0x%08x\n",
           block[0] - block[1], block[2]);
}


int main ( void )
{
  do_pushb_pos();
  do_pushb_neg();
  do_pushw_pos();
  do_pushw_neg();
  do_pushl_pos();
  do_pushl_neg();

  do_66pushb_pos();
  do_66pushb_neg();
  return 0;
}
