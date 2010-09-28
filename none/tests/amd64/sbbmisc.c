#include "tests/asm.h"
#include <stdio.h>

char in_b, out_b1, out_b2, in_b2;

short in_w, out_w1, out_w2;

int in_l, out_l1, out_l2;

extern void sbb_ib_al ( void );
asm("\n"
VG_SYM(sbb_ib_al) ":\n"

"\tmovb " VG_SYM(in_b) ", %al\n"
"\tclc\n"
"\tsbbb $5, %al\n"
"\tmovb %al, " VG_SYM(out_b1) "\n"

"\tmovb " VG_SYM(in_b) ", %al\n"
"\tstc\n"
"\tsbbb $5, %al\n"
"\tmovb %al, " VG_SYM(out_b2) "\n"

"\tretq\n"
);


extern void sbb_iw_ax ( void );
asm("\n"
VG_SYM(sbb_iw_ax) ":\n"

"\tmovw " VG_SYM(in_w) ", %ax\n"
"\tclc\n"
"\tsbbw $555, %ax\n"
"\tmovw %ax, " VG_SYM(out_w1) "\n"

"\tmovw " VG_SYM(in_w) ", %ax\n"
"\tstc\n"
"\tsbbw $555, %ax\n"
"\tmovw %ax, " VG_SYM(out_w2) "\n"

"\tretq\n"
);


extern void sbb_il_eax ( void );
asm("\n"
VG_SYM(sbb_il_eax) ":\n"

"\tmovl " VG_SYM(in_l) ", %eax\n"
"\tclc\n"
"\tsbbl $555666, %eax\n"
"\tmovl %eax, " VG_SYM(out_l1) "\n"

"\tmovl " VG_SYM(in_l) ", %eax\n"
"\tstc\n"
"\tsbbl $555666, %eax\n"
"\tmovl %eax, " VG_SYM(out_l2) "\n"

"\tretq\n"
);


extern void sbb_eb_gb ( void );
asm("\n"
VG_SYM(sbb_eb_gb) ":\n"

"\tmovb " VG_SYM(in_b) ", %al\n"
"\tclc\n"
"\tsbbb " VG_SYM(in_b2) ", %al\n"
"\tmovb %al, " VG_SYM(out_b1) "\n"

"\tmovb " VG_SYM(in_b) ", %al\n"
"\tstc\n"
"\tsbbb " VG_SYM(in_b2) ", %al\n"
"\tmovb %al, " VG_SYM(out_b2) "\n"

"\tretq\n"
);


extern void sbb_eb_gb_2 ( void );
asm("\n"
VG_SYM(sbb_eb_gb_2) ":\n"
"\tpushq %rcx\n"

"\tmovb " VG_SYM(in_b) ", %cl\n"
"\tmovb " VG_SYM(in_b2) ", %dh\n"
"\tclc\n"
"\tsbbb %dh,%cl\n"
"\tmovb %cl, " VG_SYM(out_b1) "\n"

"\tmovb " VG_SYM(in_b) ", %cl\n"
"\tmovb " VG_SYM(in_b2) ", %dh\n"
"\tstc\n"
"\tsbbb %dh,%cl\n"
"\tmovb %cl, " VG_SYM(out_b2) "\n"

"\tpopq %rcx\n"
"\tretq\n"
);


extern void adc_eb_gb ( void );
asm("\n"
VG_SYM(adc_eb_gb) ":\n"

"\tmovb " VG_SYM(in_b) ", %al\n"
"\tclc\n"
"\tadcb " VG_SYM(in_b2) ", %al\n"
"\tmovb %al, " VG_SYM(out_b1) "\n"

"\tmovb " VG_SYM(in_b) ", %al\n"
"\tstc\n"
"\tadcb " VG_SYM(in_b2) ", %al\n"
"\tmovb %al, " VG_SYM(out_b2) "\n"

"\tretq\n"
);


extern void adc_eb_gb_2 ( void );
asm("\n"
VG_SYM(adc_eb_gb_2) ":\n"
"\tpushq %rcx\n"

"\tmovb " VG_SYM(in_b) ", %cl\n"
"\tmovb " VG_SYM(in_b2) ", %dh\n"
"\tclc\n"
"\tadcb %dh,%cl\n"
"\tmovb %cl, " VG_SYM(out_b1) "\n"

"\tmovb " VG_SYM(in_b) ", %cl\n"
"\tmovb " VG_SYM(in_b2) ", %dh\n"
"\tstc\n"
"\tadcb %dh,%cl\n"
"\tmovb %cl, " VG_SYM(out_b2) "\n"

"\tpopq %rcx\n"
"\tretq\n"
);

extern void adc_ib_al ( void );
asm("\n"
VG_SYM(adc_ib_al) ":\n"

"\tmovb " VG_SYM(in_b) ", %al\n"
"\tclc\n"
"\tadcb $5, %al\n"
"\tmovb %al, " VG_SYM(out_b1) "\n"

"\tmovb " VG_SYM(in_b) ", %al\n"
"\tstc\n"
"\tadcb $5, %al\n"
"\tmovb %al, " VG_SYM(out_b2) "\n"

"\tretq\n"
);


extern void adc_iw_ax ( void );
asm("\n"
VG_SYM(adc_iw_ax) ":\n"

"\tmovw " VG_SYM(in_w) ", %ax\n"
"\tclc\n"
"\tadcw $555, %ax\n"
"\tmovw %ax, " VG_SYM(out_w1) "\n"

"\tmovw " VG_SYM(in_w) ", %ax\n"
"\tstc\n"
"\tadcw $555, %ax\n"
"\tmovw %ax, " VG_SYM(out_w2) "\n"

"\tretq\n"
);


extern void adc_il_eax ( void );
asm("\n"
VG_SYM(adc_il_eax) ":\n"

"\tmovl " VG_SYM(in_l) ", %eax\n"
"\tclc\n"
"\tadcl $555666, %eax\n"
"\tmovl %eax, " VG_SYM(out_l1) "\n"

"\tmovl " VG_SYM(in_l) ", %eax\n"
"\tstc\n"
"\tadcl $555666, %eax\n"
"\tmovl %eax, " VG_SYM(out_l2) "\n"

"\tretq\n"
);


int main ( void )
{
   in_b = 99;
   sbb_ib_al();
   printf("r1 = %d %d\n", (int)out_b1, (int)out_b2);

   in_w = 49999;
   sbb_iw_ax();
   printf("r2 = %d %d\n", (int)out_w1, (int)out_w2);

   in_l = 0xF0000000;
   sbb_il_eax();
   printf("r3 = %d %d\n", (int)out_l1, (int)out_l2);

   in_b = 99;
   in_b2 = 88;
   sbb_eb_gb();
   printf("r4 = %d %d\n", (int)out_b1, (int)out_b2);

   in_b = 66;
   in_b2 = 77;
   sbb_eb_gb_2();
   printf("r5 = %d %d\n", (int)out_b1, (int)out_b2);

   in_b = 99;
   in_b2 = 88;
   adc_eb_gb();
   printf("r6 = %d %d\n", (int)out_b1, (int)out_b2);

   in_b = 66;
   in_b2 = 77;
   adc_eb_gb_2();
   printf("r7 = %d %d\n", (int)out_b1, (int)out_b2);

   in_b = 99;
   adc_ib_al();
   printf("r8 = %d %d\n", (int)out_b1, (int)out_b2);

   in_w = 49999;
   adc_iw_ax();
   printf("r9 = %d %d\n", (int)out_w1, (int)out_w2);

   in_l = 0xF0000000;
   adc_il_eax();
   printf("r10 = %d %d\n", (int)out_l1, (int)out_l2);

   return 0;
}
