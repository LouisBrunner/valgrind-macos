#include "tests/asm.h"
#include <stdio.h>

/* This test only checks register/register cmpxchg */

typedef unsigned long long int ULong;
typedef unsigned int UInt;

ULong m64;

ULong rax;
ULong rbx;
ULong rcx;
ULong rdx;
ULong rax_out;
ULong rbx_out;
ULong rcx_out;

int main ( void )
{
   
   /* 8-bit */
   
   rdx  = 0x11111111; rax = 0x22222222;
   rcx  = 0x33333333; rbx = 0x44444444;
   
   printf("cmpxchg %%bl,%%cl  (al=%llx bl=%llx cl=%llx)\n",
	  rax&0xff,rbx&0xff,rcx&0xff);
      
   asm("\n"
    "\tpush %rax\n"
    "\tpush %rbx\n"
    "\tpush %rcx\n"
    "\tpush %rdx\n"
    "\txor %rax, %rax\n" // get eflags in a known state
    "\tmov " VG_SYM(rax) ",%rax\n"
    "\tmov " VG_SYM(rbx) ",%rbx\n"
    "\tmov " VG_SYM(rcx) ",%rcx\n"
    "\tmov " VG_SYM(rdx) ",%rdx\n"
    "\tcmpxchg %bl,%cl \n"
    "\tmov %rax," VG_SYM(rax_out) "\n"
    "\tmov %rbx," VG_SYM(rbx_out) "\n"
    "\tmov %rcx," VG_SYM(rcx_out) "\n"
    "\tpop %rdx\n"
    "\tpop %rcx\n"
    "\tpop %rbx\n"
    "\tpop %rax\n"
    );
   
   printf("  al!=cl so al should equal cl (Result al=%llx bl=%llx cl=%llx)\n",
	  rax_out&0xff,rbx_out&0xff,rcx_out&0xff);
   
   
   
   rdx  = 0x99999999; rax = 0x77777777;
   rcx  = 0x55555555; rbx = 0x55555555;
   
   printf("cmpxchg %%bl,%%cl  (al=%llx bl=%llx cl=%llx)\n",
	  rax&0xff,rbx&0xff,rcx&0xff);
      
   asm("\n"
    "\tpush %rax\n"
    "\tpush %rbx\n"
    "\tpush %rcx\n"
    "\tpush %rdx\n"
    "\txor %rax, %rax\n" // get eflags in a known state
    "\tmov " VG_SYM(rax) ",%rax\n"
    "\tmov " VG_SYM(rbx) ",%rbx\n"
    "\tmov " VG_SYM(rcx) ",%rcx\n"
    "\tmov " VG_SYM(rdx) ",%rdx\n"
    "\tcmpxchg %bl,%cl \n"
    "\tmov %rax," VG_SYM(rax_out) "\n"
    "\tmov %rbx," VG_SYM(rbx_out) "\n"
    "\tmov %rcx," VG_SYM(rcx_out) "\n"
    "\tpop %rdx\n"
    "\tpop %rcx\n"
    "\tpop %rbx\n"
    "\tpop %rax\n"
    );
   
   printf("  al==cl so cl should equal bl (Result al=%llx bl=%llx cl=%llx)\n",
	  rax_out&0xff,rbx_out&0xff,rcx_out&0xff);   
   
   /* 16-bit */
   
   rdx  = 0x11111111; rax = 0x22222222;
   rcx  = 0x33333333; rbx = 0x44444444;
   
   printf("cmpxchg %%bx,%%cx  (ax=%llx bx=%llx cx=%llx)\n",
	  rax&0xffff,rbx&0xffff,rcx&0xffff);
      
   asm("\n"
    "\tpush %rax\n"
    "\tpush %rbx\n"
    "\tpush %rcx\n"
    "\tpush %rdx\n"
    "\txor %rax, %rax\n" // get eflags in a known state
    "\tmov " VG_SYM(rax) ",%rax\n"
    "\tmov " VG_SYM(rbx) ",%rbx\n"
    "\tmov " VG_SYM(rcx) ",%rcx\n"
    "\tmov " VG_SYM(rdx) ",%rdx\n"
    "\tcmpxchg %bx,%cx \n"
    "\tmov %rax," VG_SYM(rax_out) "\n"
    "\tmov %rbx," VG_SYM(rbx_out) "\n"
    "\tmov %rcx," VG_SYM(rcx_out) "\n"
    "\tpop %rdx\n"
    "\tpop %rcx\n"
    "\tpop %rbx\n"
    "\tpop %rax\n"
    );
   
   printf("  ax!=cx so ax should equal cx (Result ax=%llx bx=%llx cx=%llx)\n",
	  rax_out&0xffff,rbx_out&0xffff,rcx_out&0xffff);
   
   
   
   rdx  = 0x99999999; rax = 0x77777777;
   rcx  = 0x55555555; rbx = 0x55555555;
   
   printf("cmpxchg %%bx,%%cx  (ax=%llx bx=%llx cx=%llx)\n",
	  rax&0xffff,rbx&0xffff,rcx&0xffff);
      
   asm("\n"
    "\tpush %rax\n"
    "\tpush %rbx\n"
    "\tpush %rcx\n"
    "\tpush %rdx\n"
    "\txor %rax, %rax\n" // get eflags in a known state
    "\tmov " VG_SYM(rax) ",%rax\n"
    "\tmov " VG_SYM(rbx) ",%rbx\n"
    "\tmov " VG_SYM(rcx) ",%rcx\n"
    "\tmov " VG_SYM(rdx) ",%rdx\n"
    "\tcmpxchg %bx,%cx \n"
    "\tmov %rax," VG_SYM(rax_out) "\n"
    "\tmov %rbx," VG_SYM(rbx_out) "\n"
    "\tmov %rcx," VG_SYM(rcx_out) "\n"
    "\tpop %rdx\n"
    "\tpop %rcx\n"
    "\tpop %rbx\n"
    "\tpop %rax\n"
    );
   
   printf("  ax==cx so cx should equal bx (Result ax=%llx bx=%llx cx=%llx)\n",
	  rax_out&0xffff,rbx_out&0xffff,rcx_out&0xffff);   
   
   
   /* 32-bit */
   
   rdx  = 0x11111111; rax = 0x22222222;
   rcx  = 0x33333333; rbx = 0x44444444;
   
   printf("cmpxchg %%ebx,%%ecx  (eax=%llx ebx=%llx ecx=%llx)\n",
	  rax&0xffffffff,rbx&0xffffffff,rcx&0xffffffff);
      
   asm("\n"
    "\tpush %rax\n"
    "\tpush %rbx\n"
    "\tpush %rcx\n"
    "\tpush %rdx\n"
    "\txor %rax, %rax\n" // get eflags in a known state
    "\tmov " VG_SYM(rax) ",%rax\n"
    "\tmov " VG_SYM(rbx) ",%rbx\n"
    "\tmov " VG_SYM(rcx) ",%rcx\n"
    "\tmov " VG_SYM(rdx) ",%rdx\n"
    "\tcmpxchg %ebx,%ecx \n"
    "\tmov %rax," VG_SYM(rax_out) "\n"
    "\tmov %rbx," VG_SYM(rbx_out) "\n"
    "\tmov %rcx," VG_SYM(rcx_out) "\n"
    "\tpop %rdx\n"
    "\tpop %rcx\n"
    "\tpop %rbx\n"
    "\tpop %rax\n"
    );
   
   printf("  eax!=ecx so eax should equal ecx (Result eax=%llx ebx=%llx ecx=%llx)\n",
	  rax_out&0xffffffff,rbx_out&0xffffffff,rcx_out&0xffffffff);
   
   
   
   rdx  = 0x99999999; rax = 0x77777777;
   rcx  = 0x55555555; rbx = 0x55555555;
   
   printf("cmpxchg %%ebx,%%ecx  (eax=%llx ebx=%llx ecx=%llx)\n",
	  rax&0xffffffff,rbx&0xffffffff,rcx&0xffffffff);
      
   asm("\n"
    "\tpush %rax\n"
    "\tpush %rbx\n"
    "\tpush %rcx\n"
    "\tpush %rdx\n"
    "\txor %rax, %rax\n" // get eflags in a known state
    "\tmov " VG_SYM(rax) ",%rax\n"
    "\tmov " VG_SYM(rbx) ",%rbx\n"
    "\tmov " VG_SYM(rcx) ",%rcx\n"
    "\tmov " VG_SYM(rdx) ",%rdx\n"
    "\tcmpxchg %ebx,%ecx \n"
    "\tmov %rax," VG_SYM(rax_out) "\n"
    "\tmov %rbx," VG_SYM(rbx_out) "\n"
    "\tmov %rcx," VG_SYM(rcx_out) "\n"
    "\tpop %rdx\n"
    "\tpop %rcx\n"
    "\tpop %rbx\n"
    "\tpop %rax\n"
    );
   
   printf("  eax==ecx so ecx should equal ebx (Result eax=%llx ebx=%llx ecx=%llx)\n",
	  rax_out&0xffffffff,rbx_out&0xffffffff,rcx_out&0xffffffff);
   
   
   /* 64-bit */
   
   rdx  = 0x111111111; rax = 0x222222222;
   rcx  = 0x333333333; rbx = 0x444444444;
   
   printf("cmpxchg %%rbx,%%rcx  (rax=%llx rbx=%llx rcx=%llx)\n",
	  rax,rbx,rcx);
      
   asm("\n"
    "\tpush %rax\n"
    "\tpush %rbx\n"
    "\tpush %rcx\n"
    "\tpush %rdx\n"
    "\txor %rax, %rax\n" // get eflags in a known state
    "\tmov " VG_SYM(rax) ",%rax\n"
    "\tmov " VG_SYM(rbx) ",%rbx\n"
    "\tmov " VG_SYM(rcx) ",%rcx\n"
    "\tmov " VG_SYM(rdx) ",%rdx\n"
    "\tcmpxchg %rbx,%rcx \n"
    "\tmov %rax," VG_SYM(rax_out) "\n"
    "\tmov %rbx," VG_SYM(rbx_out) "\n"
    "\tmov %rcx," VG_SYM(rcx_out) "\n"
    "\tpop %rdx\n"
    "\tpop %rcx\n"
    "\tpop %rbx\n"
    "\tpop %rax\n"
    );
   
   printf("  rax!=rcx so rax should equal rcx (Result rax=%llx rbx=%llx rcx=%llx)\n",
	  rax_out,rbx_out,rcx_out);
   
   
   
   rdx  = 0x999999999; rax = 0x777777777;
   rcx  = 0x555555555; rbx = 0x555555555;
   
   printf("cmpxchg %%rbx,%%rcx  (rax=%llx rbx=%llx rcx=%llx)\n",
	  rax,rbx,rcx);
      
   asm("\n"
    "\tpush %rax\n"
    "\tpush %rbx\n"
    "\tpush %rcx\n"
    "\tpush %rdx\n"
    "\txor %rax, %rax\n" // get eflags in a known state
    "\tmov " VG_SYM(rax) ",%rax\n"
    "\tmov " VG_SYM(rbx) ",%rbx\n"
    "\tmov " VG_SYM(rcx) ",%rcx\n"
    "\tmov " VG_SYM(rdx) ",%rdx\n"
    "\tcmpxchg %rbx,%rcx \n"
    "\tmov %rax," VG_SYM(rax_out) "\n"
    "\tmov %rbx," VG_SYM(rbx_out) "\n"
    "\tmov %rcx," VG_SYM(rcx_out) "\n"
    "\tpop %rdx\n"
    "\tpop %rcx\n"
    "\tpop %rbx\n"
    "\tpop %rax\n"
    );
   
   printf("  rax==rcx so ecx should equal rbx (Result rax=%llx rbx=%llx rcx=%llx)\n",
	  rax_out,rbx_out,rcx_out);      
   
   return 0;
}
