
#include <stdio.h>

char in_b, out_b1, out_b2, in_b2;

short in_w, out_w1, out_w2;

int in_l, out_l1, out_l2;

extern void sbb_ib_al ( void );
asm("\n"
"sbb_ib_al:\n"

"\tmovb in_b, %al\n"
"\tclc\n"
"\tsbbb $5, %al\n"
"\tmovb %al, out_b1\n"

"\tmovb in_b, %al\n"
"\tstc\n"
"\tsbbb $5, %al\n"
"\tmovb %al, out_b2\n"

"\tret\n"
);


extern void sbb_iw_ax ( void );
asm("\n"
"sbb_iw_ax:\n"

"\tmovw in_w, %ax\n"
"\tclc\n"
"\tsbbw $555, %ax\n"
"\tmovw %ax, out_w1\n"

"\tmovw in_w, %ax\n"
"\tstc\n"
"\tsbbw $555, %ax\n"
"\tmovw %ax, out_w2\n"

"\tret\n"
);


extern void sbb_il_eax ( void );
asm("\n"
"sbb_il_eax:\n"

"\tmovl in_l, %eax\n"
"\tclc\n"
"\tsbbl $555666, %eax\n"
"\tmovl %eax, out_l1\n"

"\tmovl in_l, %eax\n"
"\tstc\n"
"\tsbbl $555666, %eax\n"
"\tmovl %eax, out_l2\n"

"\tret\n"
);


extern void sbb_eb_gb ( void );
asm("\n"
"sbb_eb_gb:\n"

"\tmovb in_b, %al\n"
"\tclc\n"
"\tsbbb in_b2, %al\n"
"\tmovb %al, out_b1\n"

"\tmovb in_b, %al\n"
"\tstc\n"
"\tsbbb in_b2, %al\n"
"\tmovb %al, out_b2\n"

"\tret\n"
);


extern void sbb_eb_gb_2 ( void );
asm("\n"
"sbb_eb_gb_2:\n"
"\tpushl %ecx\n"

"\tmovb in_b, %cl\n"
"\tmovb in_b2, %dh\n"
"\tclc\n"
"\tsbbb %dh,%cl\n"
"\tmovb %cl, out_b1\n"

"\tmovb in_b, %cl\n"
"\tmovb in_b2, %dh\n"
"\tstc\n"
"\tsbbb %dh,%cl\n"
"\tmovb %cl, out_b2\n"

"\tpopl %ecx\n"
"\tret\n"
);


extern void adc_eb_gb ( void );
asm("\n"
"adc_eb_gb:\n"

"\tmovb in_b, %al\n"
"\tclc\n"
"\tadcb in_b2, %al\n"
"\tmovb %al, out_b1\n"

"\tmovb in_b, %al\n"
"\tstc\n"
"\tadcb in_b2, %al\n"
"\tmovb %al, out_b2\n"

"\tret\n"
);


extern void adc_eb_gb_2 ( void );
asm("\n"
"adc_eb_gb_2:\n"
"\tpushl %ecx\n"

"\tmovb in_b, %cl\n"
"\tmovb in_b2, %dh\n"
"\tclc\n"
"\tadcb %dh,%cl\n"
"\tmovb %cl, out_b1\n"

"\tmovb in_b, %cl\n"
"\tmovb in_b2, %dh\n"
"\tstc\n"
"\tadcb %dh,%cl\n"
"\tmovb %cl, out_b2\n"

"\tpopl %ecx\n"
"\tret\n"
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

   return 0;
}
