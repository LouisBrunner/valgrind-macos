/*
 * Bug 492210 False positive on x86/amd64 with ZF taken directly from addition
 *
 * The problem is that the Z flag wasn't being calculated for addl instructions.
 *
 * The same problem exists for addw and addb.
 */


unsigned char b;
unsigned short w;
unsigned long l;
unsigned long long q;

extern void test(void);

asm("\n"
".text\n"
"test:\n"

"\tmovb b, %al\n"
"\tmovb $1, %ah\n"
"\taddb %al, %ah\n"
"\tje label1\n"
"\tlabel1:\n"

"\taddb %al, %ah\n"
"\tjne label2\n"
"\tlabel2:\n"

"\tsubb %al, %ah\n"
"\tje label3\n"
"\tlabel3:\n"

"\tsubb %al, %ah\n"
"\tjne label4\n"
"\tlabel4:\n"

"\tmov w, %ax\n"
"\tmovw $1, %bx\n"
"\taddw %ax, %bx\n"
"\tje label5\n"
"\tlabel5:\n"

"\taddw %ax, %bx\n"
"\tjne label6\n"
"\tlabel6:\n"

"\tsubw %ax, %bx\n"
"\tje label7\n"
"\tlabel7:\n"

"\tsubw %ax, %bx\n"
"\tjne label8\n"
"\tlabel8:\n"

"\tmov l, %eax\n"
"\tmov $1, %ebx\n"
"\tadd %eax, %ebx\n"
"\tje label9\n"
"\tlabel9:\n"

"\tadd %eax, %ebx\n"
"\tjne label10\n"
"\tlabel10:\n"

"\tsub %eax, %ebx\n"
"\tje label11\n"
"\tlabel11:\n"

"\tsub %eax, %ebx\n"
"\tjne label12\n"
"\tlabel12:\n"

"\tmovq q, %rax\n"
"\tmovq $1, %rbx\n"
"\taddq %rax, %rbx\n"
"\tje label13\n"
"\tlabel13:\n"

"\taddq %rax, %rbx\n"
"\tjne label14\n"
"\tlabel14:\n"

"\tsubq %rax, %rbx\n"
"\tje label15\n"
"\tlabel15:\n"

"\tsubq %rax, %rbx\n"
"\tjne label16\n"
"\tlabel16:\n"

"\tret\n"
".previous\n"
);

int main()
{
   unsigned long long uninit;
   uninit &= 0xfffffffffffffffe;
   b = uninit;
   w = uninit;
   l = uninit;
   q = uninit;
   test();
}
