
#include <stdio.h>

void do_pushb_pos ( void )
{
   unsigned long long int block[3];
   __asm__ __volatile__ (
      "movq  %0, %%rdx\n\t"
      "pushq $0x55555555\n\t"
      "movq  %%rsp, 0(%%rdx)\n\t"
      ".byte 0x6A,0x22\n\t"
      "movq  %%rsp, 8(%%rdx)\n\t"
      "popq  %%rax\n\t"
      "movq  %%rax,16(%%rdx)\n\t"
      "movq  0(%%rdx),%%rsp\n\t"
      "addq  $8, %%rsp"
      : : "r"(&block) : "rax","rdx","cc","memory"
    );
    printf("  pushb_pos: delta %lld, top64 0x%016llx\n",
           block[0] - block[1], block[2]);
}

void do_pushb_neg ( void )
{
   unsigned long long int block[3];
   __asm__ __volatile__ (
      "movq  %0, %%rdx\n\t"
      "pushq $0x55555555\n\t"
      "movq  %%rsp, 0(%%rdx)\n\t"
      ".byte 0x6A,0xEE\n\t"
      "movq  %%rsp, 8(%%rdx)\n\t"
      "popq  %%rax\n\t"
      "movq  %%rax,16(%%rdx)\n\t"
      "movq  0(%%rdx),%%rsp\n\t"
      "addq  $8, %%rsp"
      : : "r"(&block) : "rax","rdx","cc","memory"
    );
    printf("  pushb_neg: delta %lld, top64 0x%016llx\n",
           block[0] - block[1], block[2]);
}

void do_pushw_pos ( void )
{
   unsigned long long int block[3];
   __asm__ __volatile__ (
      "movq  %0, %%rdx\n\t"
      "pushq $0x55555555\n\t"
      "movq  %%rsp, 0(%%rdx)\n\t"
      "pushw $0x3344\n\t"
      "movq  %%rsp, 8(%%rdx)\n\t"
      "popq  %%rax\n\t"
      "movq  %%rax,16(%%rdx)\n\t"
      "movq  0(%%rdx),%%rsp\n\t"
      "addq  $8, %%rsp"
      : : "r"(&block) : "rax","rdx","cc","memory"
    );
    printf("  pushw_neg: delta %lld, top64 0x%016llx\n",
           block[0] - block[1], block[2]);
}

void do_pushw_neg ( void )
{
   unsigned long long int block[3];
   __asm__ __volatile__ (
      "movq  %0, %%rdx\n\t"
      "pushq $0x55555555\n\t"
      "movq  %%rsp, 0(%%rdx)\n\t"
      "pushw $0xDDCC\n\t"
      "movq  %%rsp, 8(%%rdx)\n\t"
      "popq  %%rax\n\t"
      "movq  %%rax,16(%%rdx)\n\t"
      "movq  0(%%rdx),%%rsp\n\t"
      "addq  $8, %%rsp"
      : : "r"(&block) : "rax","rdx","cc","memory"
    );
    printf("  pushw_pos: delta %lld, top64 0x%016llx\n",
           block[0] - block[1], block[2]);
}

void do_pushq_pos ( void )
{
   unsigned long long int block[3];
   __asm__ __volatile__ (
      "movq  %0, %%rdx\n\t"
      "pushq $0x55555555\n\t"
      "movq  %%rsp, 0(%%rdx)\n\t"
      "pushq $0x67675656\n\t"
      "movq  %%rsp, 8(%%rdx)\n\t"
      "popq  %%rax\n\t"
      "movq  %%rax,16(%%rdx)\n\t"
      "movq  0(%%rdx),%%rsp\n\t"
      "addq  $8, %%rsp"
      : : "r"(&block) : "rax","rdx","cc","memory"
    );
    printf("  pushq_pos: delta %lld, top64 0x%016llx\n",
           block[0] - block[1], block[2]);
}

void do_pushq_neg ( void )
{
   unsigned long long int block[3];
   __asm__ __volatile__ (
      "movq  %0, %%rdx\n\t"
      "pushq $0x55555555\n\t"
      "movq  %%rsp, 0(%%rdx)\n\t"
      "pushq $0x78988787\n\t"
      "movq  %%rsp, 8(%%rdx)\n\t"
      "popq  %%rax\n\t"
      "movq  %%rax,16(%%rdx)\n\t"
      "movq  0(%%rdx),%%rsp\n\t"
      "addq  $8, %%rsp"
      : : "r"(&block) : "rax","rdx","cc","memory"
    );
    printf("  pushq_neg: delta %lld, top64 0x%016llx\n",
           block[0] - block[1], block[2]);
}


void do_66pushb_pos ( void )
{
   unsigned long long int block[3];
   __asm__ __volatile__ (
      "movq  %0, %%rdx\n\t"
      "pushq $0x55555555\n\t"
      "movq  %%rsp, 0(%%rdx)\n\t"
      ".byte 0x66,0x6A,0x22\n\t"
      "movq  %%rsp, 8(%%rdx)\n\t"
      "popq  %%rax\n\t"
      "movq  %%rax,16(%%rdx)\n\t"
      "movq  0(%%rdx),%%rsp\n\t"
      "addq  $8, %%rsp"
      : : "r"(&block) : "rax","rdx","cc","memory"
    );
    printf("66pushb_pos: delta %lld, top64 0x%016llx\n",
           block[0] - block[1], block[2]);
}

void do_66pushb_neg ( void )
{
   unsigned long long int block[3];
   __asm__ __volatile__ (
      "movq  %0, %%rdx\n\t"
      "pushq $0x55555555\n\t"
      "movq  %%rsp, 0(%%rdx)\n\t"
      ".byte 0x66,0x6A,0xEE\n\t"
      "movq  %%rsp, 8(%%rdx)\n\t"
      "popq  %%rax\n\t"
      "movq  %%rax,16(%%rdx)\n\t"
      "movq  0(%%rdx),%%rsp\n\t"
      "addq  $8, %%rsp"
      : : "r"(&block) : "rax","rdx","cc","memory"
    );
    printf("66pushb_neg: delta %lld, top64 0x%016llx\n",
           block[0] - block[1], block[2]);
}


int main ( void )
{
  do_pushb_pos();
  do_pushb_neg();
  do_pushw_pos();
  do_pushw_neg();
  do_pushq_pos();
  do_pushq_neg();

  do_66pushb_pos();
  do_66pushb_neg();
  return 0;
}
