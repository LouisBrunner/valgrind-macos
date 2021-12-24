#include "tests/asm.h"
#include <stdio.h>

char in_b, out_b1, out_b2, in_b2;

short in_w, out_w1, out_w2;

int in_l, out_l1, out_l2;

extern void sbb_ib_al ( void );
asm("\n"
".text\n"
VG_SYM(sbb_ib_al) ":\n"

#ifndef VGP_amd64_darwin
"\tmovb " VG_SYM(in_b) ", %al\n"
#else
"\tmovb " VG_SYM(in_b) "(%rip), %al\n"
#endif

"\tclc\n"
"\tsbbb $5, %al\n"
#ifndef VGP_amd64_darwin
"\tmovb %al, " VG_SYM(out_b1) "\n"

"\tmovb " VG_SYM(in_b) ", %al\n"
#else
"\tmovb %al, " VG_SYM(out_b1) "(%rip)\n"

"\tmovb " VG_SYM(in_b) "(%rip), %al\n"
#endif
"\tstc\n"
"\tsbbb $5, %al\n"
#ifndef VGP_amd64_darwin
"\tmovb %al, " VG_SYM(out_b2) "\n"
#else
"\tmovb %al," VG_SYM(out_b2) "(%rip) \n"
#endif

"\tretq\n"
".previous\n"
);


extern void sbb_iw_ax ( void );
asm("\n"
".text\n"
VG_SYM(sbb_iw_ax) ":\n"

#ifndef VGP_amd64_darwin
"\tmovw " VG_SYM(in_w) ", %ax\n"
#else
"\tmovw " VG_SYM(in_w) "(%rip), %ax\n"
#endif
"\tclc\n"
"\tsbbw $555, %ax\n"
#ifndef VGP_amd64_darwin
"\tmovw %ax, " VG_SYM(out_w1) "\n"

"\tmovw " VG_SYM(in_w) ", %ax\n"
#else
"\tmovw %ax, " VG_SYM(out_w1) "(%rip)\n"

"\tmovw " VG_SYM(in_w) "(%rip), %ax\n"
#endif
"\tstc\n"
"\tsbbw $555, %ax\n"
#ifndef VGP_amd64_darwin
"\tmovw %ax, " VG_SYM(out_w2) "\n"
#else
"\tmovw %ax, " VG_SYM(out_w2) "(%rip)\n"
#endif

"\tretq\n"
".previous\n"
);


extern void sbb_il_eax ( void );
asm("\n"
".text\n"
VG_SYM(sbb_il_eax) ":\n"

#ifndef VGP_amd64_darwin
"\tmovl " VG_SYM(in_l) ", %eax\n"
#else
"\tmovl " VG_SYM(in_l) "(%rip), %eax\n"
#endif
"\tclc\n"
"\tsbbl $555666, %eax\n"
#ifndef VGP_amd64_darwin
"\tmovl %eax, " VG_SYM(out_l1) "\n"

"\tmovl " VG_SYM(in_l) ", %eax\n"
#else
"\tmovl %eax, " VG_SYM(out_l1) "(%rip)\n"

"\tmovl " VG_SYM(in_l) "(%rip), %eax\n"
#endif
"\tstc\n"
"\tsbbl $555666, %eax\n"
#ifndef VGP_amd64_darwin
"\tmovl %eax, " VG_SYM(out_l2) "\n"
#else
"\tmovl %eax, " VG_SYM(out_l2) "(%rip)\n"
#endif

"\tretq\n"
".previous\n"
);


extern void sbb_eb_gb ( void );
asm("\n"
".text\n"
VG_SYM(sbb_eb_gb) ":\n"

#ifndef VGP_amd64_darwin
"\tmovb " VG_SYM(in_b) ", %al\n"
#else
"\tmovb " VG_SYM(in_b) "(%rip), %al\n"
#endif
"\tclc\n"
#ifndef VGP_amd64_darwin
"\tsbbb " VG_SYM(in_b2) ", %al\n"
"\tmovb %al, " VG_SYM(out_b1) "\n"

"\tmovb " VG_SYM(in_b) ", %al\n"
#else
"\tsbbb " VG_SYM(in_b2) "(%rip), %al\n"
"\tmovb %al, " VG_SYM(out_b1) "(%rip)\n"

"\tmovb " VG_SYM(in_b) "(%rip), %al\n"
#endif
"\tstc\n"
#ifndef VGP_amd64_darwin
"\tsbbb " VG_SYM(in_b2) ", %al\n"
"\tmovb %al, " VG_SYM(out_b2) "\n"
#else
"\tsbbb " VG_SYM(in_b2) "(%rip), %al\n"
"\tmovb %al, " VG_SYM(out_b2) "(%rip)\n"
#endif

"\tretq\n"
".previous\n"
);


extern void sbb_eb_gb_2 ( void );
asm("\n"
".text\n"
VG_SYM(sbb_eb_gb_2) ":\n"
"\tpushq %rcx\n"

#ifndef VGP_amd64_darwin
"\tmovb " VG_SYM(in_b) ", %cl\n"
"\tmovb " VG_SYM(in_b2) ", %dh\n"
#else
"\tmovb " VG_SYM(in_b) "(%rip), %cl\n"
"\tmovb " VG_SYM(in_b2) "(%rip), %dh\n"
#endif
"\tclc\n"
"\tsbbb %dh,%cl\n"
#ifndef VGP_amd64_darwin
"\tmovb %cl, " VG_SYM(out_b1) "\n"

"\tmovb " VG_SYM(in_b) ", %cl\n"
"\tmovb " VG_SYM(in_b2) ", %dh\n"
#else
"\tmovb %cl, " VG_SYM(out_b1) "(%rip)\n"

"\tmovb " VG_SYM(in_b) "(%rip), %cl\n"
"\tmovb " VG_SYM(in_b2) "(%rip), %dh\n"
#endif
"\tstc\n"
"\tsbbb %dh,%cl\n"
#ifndef VGP_amd64_darwin
"\tmovb %cl, " VG_SYM(out_b2) "\n"
#else
"\tmovb %cl, " VG_SYM(out_b2) "(%rip)\n"
#endif

"\tpopq %rcx\n"
"\tretq\n"
".previous\n"
);


extern void adc_eb_gb ( void );
asm("\n"
".text\n"
VG_SYM(adc_eb_gb) ":\n"

#ifndef VGP_amd64_darwin
"\tmovb " VG_SYM(in_b) ", %al\n"
#else
"\tmovb " VG_SYM(in_b) "(%rip), %al\n"
#endif
"\tclc\n"
#ifndef VGP_amd64_darwin
"\tadcb " VG_SYM(in_b2) ", %al\n"
"\tmovb %al, " VG_SYM(out_b1) "\n"

"\tmovb " VG_SYM(in_b) ", %al\n"
#else
"\tadcb " VG_SYM(in_b2) "(%rip), %al\n"
"\tmovb %al, " VG_SYM(out_b1) "(%rip)\n"

"\tmovb " VG_SYM(in_b) "(%rip), %al\n"
#endif
"\tstc\n"
#ifndef VGP_amd64_darwin
"\tadcb " VG_SYM(in_b2) ", %al\n"
"\tmovb %al, " VG_SYM(out_b2) "\n"
#else
"\tadcb " VG_SYM(in_b2) "(%rip), %al\n"
"\tmovb %al, " VG_SYM(out_b2) "(%rip)\n"
#endif

"\tretq\n"
".previous\n"
);


extern void adc_eb_gb_2 ( void );
asm("\n"
".text\n"
VG_SYM(adc_eb_gb_2) ":\n"
"\tpushq %rcx\n"

#ifndef VGP_amd64_darwin
"\tmovb " VG_SYM(in_b) ", %cl\n"
"\tmovb " VG_SYM(in_b2) ", %dh\n"
#else
"\tmovb " VG_SYM(in_b) "(%rip), %cl\n"
"\tmovb " VG_SYM(in_b2) "(%rip), %dh\n"
#endif
"\tclc\n"
"\tadcb %dh,%cl\n"
#ifndef VGP_amd64_darwin
"\tmovb %cl, " VG_SYM(out_b1) "\n"

"\tmovb " VG_SYM(in_b) ", %cl\n"
"\tmovb " VG_SYM(in_b2) ", %dh\n"
#else
"\tmovb %cl, " VG_SYM(out_b1) "(%rip)\n"

"\tmovb " VG_SYM(in_b) "(%rip), %cl\n"
"\tmovb " VG_SYM(in_b2) "(%rip), %dh\n"
#endif
"\tstc\n"
"\tadcb %dh,%cl\n"
#ifndef VGP_amd64_darwin
"\tmovb %cl, " VG_SYM(out_b2) "\n"
#else
"\tmovb %cl, " VG_SYM(out_b2) "(%rip)\n"
#endif

"\tpopq %rcx\n"
"\tretq\n"
".previous\n"
);

extern void adc_ib_al ( void );
asm("\n"
".text\n"
VG_SYM(adc_ib_al) ":\n"

#ifndef VGP_amd64_darwin
"\tmovb " VG_SYM(in_b) ", %al\n"
#else
"\tmovb " VG_SYM(in_b) "(%rip), %al\n"
#endif
"\tclc\n"
"\tadcb $5, %al\n"
#ifndef VGP_amd64_darwin
"\tmovb %al, " VG_SYM(out_b1) "\n"

"\tmovb " VG_SYM(in_b) ", %al\n"
#else
"\tmovb %al, " VG_SYM(out_b1) "(%rip)\n"

"\tmovb " VG_SYM(in_b) "(%rip), %al\n"
#endif
"\tstc\n"
"\tadcb $5, %al\n"
#ifndef VGP_amd64_darwin
"\tmovb %al, " VG_SYM(out_b2) "\n"
#else
"\tmovb %al, " VG_SYM(out_b2) "(%rip)\n"
#endif

"\tretq\n"
".previous\n"
);


extern void adc_iw_ax ( void );
asm("\n"
".text\n"
VG_SYM(adc_iw_ax) ":\n"

#ifndef VGP_amd64_darwin
"\tmovw " VG_SYM(in_w) ", %ax\n"
#else
"\tmovw " VG_SYM(in_w) "(%rip), %ax\n"
#endif
"\tclc\n"
"\tadcw $555, %ax\n"
#ifndef VGP_amd64_darwin
"\tmovw %ax, " VG_SYM(out_w1) "\n"

"\tmovw " VG_SYM(in_w) ", %ax\n"
#else
"\tmovw %ax, " VG_SYM(out_w1) "(%rip)\n"

"\tmovw " VG_SYM(in_w) "(%rip), %ax\n"
#endif
"\tstc\n"
"\tadcw $555, %ax\n"
#ifndef VGP_amd64_darwin
"\tmovw %ax, " VG_SYM(out_w2) "\n"
#else
"\tmovw %ax, " VG_SYM(out_w2) "(%rip)\n"
#endif

"\tretq\n"
".previous\n"
);


extern void adc_il_eax ( void );
asm("\n"
".text\n"
VG_SYM(adc_il_eax) ":\n"

#ifndef VGP_amd64_darwin
"\tmovl " VG_SYM(in_l) ", %eax\n"
#else
"\tmovl " VG_SYM(in_l) "(%rip), %eax\n"
#endif
"\tclc\n"
"\tadcl $555666, %eax\n"
#ifndef VGP_amd64_darwin
"\tmovl %eax, " VG_SYM(out_l1) "\n"

"\tmovl " VG_SYM(in_l) ", %eax\n"
#else
"\tmovl %eax, " VG_SYM(out_l1) "(%rip)\n"

"\tmovl " VG_SYM(in_l) "(%rip), %eax\n"
#endif
"\tstc\n"
"\tadcl $555666, %eax\n"
#ifndef VGP_amd64_darwin
"\tmovl %eax, " VG_SYM(out_l2) "\n"
#else
"\tmovl %eax, " VG_SYM(out_l2) "(%rip)\n"
#endif

"\tretq\n"
".previous\n"
);


int main ( void )
{
   in_b = 99;
   sbb_ib_al();
   printf("r1 = %d %d\n", (int)out_b1, (int)out_b2);

   in_w = -15537; /* was 49999 but that causes a warning */
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

   in_w = -15537;
   adc_iw_ax();
   printf("r9 = %d %d\n", (int)out_w1, (int)out_w2);

   in_l = 0xF0000000;
   adc_il_eax();
   printf("r10 = %d %d\n", (int)out_l1, (int)out_l2);

   return 0;
}
