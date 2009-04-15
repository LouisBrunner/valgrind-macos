#include "tests/asm.h"
#include <stdio.h>

typedef unsigned long long int ULong;
typedef unsigned int UInt;

ULong m64;

UInt eax;
UInt ebx;
UInt ecx;
UInt edx;
UInt zout;

extern void foo ( void );
asm("\n"
    VG_SYM(foo) ":\n"
    "\tpushl %eax\n"
    "\tpushl %ebx\n"
    "\tpushl %ecx\n"
    "\tpushl %edx\n"

    "\txorl %eax, %eax\n" // get eflags in a known state

    "\tmovl " VG_SYM(eax) ",%eax\n"
    "\tmovl " VG_SYM(ebx) ",%ebx\n"
    "\tmovl " VG_SYM(ecx) ",%ecx\n"
    "\tmovl " VG_SYM(edx) ",%edx\n"
    "\tcmpxchg8b " VG_SYM(m64) "\n"
    "\tmovl %eax," VG_SYM(eax) "\n"
    "\tmovl %ebx," VG_SYM(ebx) "\n"
    "\tmovl %ecx," VG_SYM(ecx) "\n"
    "\tmovl %edx," VG_SYM(edx) "\n"
    "\tpushfl\n"
    "\tpopl %eax\n"
    "\tmovl %eax," VG_SYM(zout) "\n"

    "\tpopl %edx\n"
    "\tpopl %ecx\n"
    "\tpopl %ebx\n"
    "\tpopl %eax\n"
    "\tret\n"
    );

int main ( void )
{
   edx  = 0x11111111; eax = 0x22222222;
   ecx  = 0x33333333; ebx = 0x44444444;
   zout = 0x55555555;
   m64  = 0x1111111122222222ULL;
   foo();
   printf("0x%x 0x%x 0x%x 0x%x 0x%x 0x%llx\n",
	  eax, ebx, ecx, edx, zout & 0xFFFF, m64 );

   edx  = 0x11111111; eax = 0x22222222;
   ecx  = 0x33333333; ebx = 0x44444444;
   zout = 0x55555555;
   m64  = 0x1111111122222222ULL;
   m64 += 0x1ULL;
   foo();
   printf("0x%x 0x%x 0x%x 0x%x 0x%x 0x%llx\n",
	  eax, ebx, ecx, edx, zout & 0xFFFF, m64 );

   edx  = 0x11111111; eax = 0x22222222;
   ecx  = 0x33333333; ebx = 0x44444444;
   zout = 0x55555555;
   m64  = 0x1111111122222222ULL;
   m64 += 0x100000000ULL;
   foo();
   printf("0x%x 0x%x 0x%x 0x%x 0x%x 0x%llx\n",
	  eax, ebx, ecx, edx, zout & 0xFFFF, m64 );

   edx  = 0x11111111; eax = 0x22222222;
   ecx  = 0x33333333; ebx = 0x44444444;
   zout = 0x55555555;
   m64  = 0x6666666677777777ULL;
   foo();
   printf("0x%x 0x%x 0x%x 0x%x 0x%x 0x%llx\n",
	  eax, ebx, ecx, edx, zout & 0xFFFF, m64 );

   return 0;
}
