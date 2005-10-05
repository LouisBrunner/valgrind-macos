
/* A test of the alternative (redundant) encodings for {inc,dec}{w,l}. */

#include <stdio.h>

int r1,r2,r3,r4,r5,r6,r7,r8,a1,a2;

extern void foo ( void );
asm("\n"
"foo:\n"
"\tpushl $0\n"
"\tpopfl\n"
"\tmovl a1,%eax\n"
"\tmovl a2,%edx\n"

/* inc %eax */
"\t.byte 0xFF\n"
"\t.byte 0xC0\n"

"\tmovl %eax,r1\n"
"\tpushfl\n"
"\tpopl r2\n"

/* inc %dx */
"\t.byte 0x66\n"
"\t.byte 0xFF\n"
"\t.byte 0xC2\n"

"\tmovl %edx,r3\n"
"\tpushfl\n"
"\tpopl r4\n"

/* dec %edx */
"\t.byte 0xFF\n"
"\t.byte 0xCA\n"

"\tmovl %edx,r5\n"
"\tpushfl\n"
"\tpopl r6\n"

/* dec %ax */
"\t.byte 0x66\n"
"\t.byte 0xFF\n"
"\t.byte 0xC8\n"

"\tmovl %eax,r7\n"
"\tpushfl\n"
"\tpopl r8\n"

"\tret\n"
);

int main ( void )
{
  a1 = 0x77777777;
  a2 = 0x88888888;
  r1=r2=r3=r4=r5=r6=r7=r8=0;
  foo();
  printf("0x%08x\n",r1);
  printf("0x%08x\n",r2);
  printf("0x%08x\n",r3);
  printf("0x%08x\n",r4);
  printf("0x%08x\n",r5);
  printf("0x%08x\n",r6);
  printf("0x%08x\n",r7);
  printf("0x%08x\n",r8);
  return 0;
}
