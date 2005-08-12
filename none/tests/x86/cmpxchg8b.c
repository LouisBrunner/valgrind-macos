
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
    "foo:\n"
    "\tpushl %eax\n"
    "\tpushl %ebx\n"
    "\tpushl %ecx\n"
    "\tpushl %edx\n"

    "\txorl %eax, %eax\n" // get eflags in a known state

    "\tmovl eax,%eax\n"
    "\tmovl ebx,%ebx\n"
    "\tmovl ecx,%ecx\n"
    "\tmovl edx,%edx\n"
    "\tcmpxchg8b m64\n"
    "\tmovl %eax,eax\n"
    "\tmovl %ebx,ebx\n"
    "\tmovl %ecx,ecx\n"
    "\tmovl %edx,edx\n"
    "\tpushfl\n"
    "\tpopl %eax\n"
    "\tmovl %eax,zout\n"

    "\tpopl %edx\n"
    "\tpopl %ecx\n"
    "\tpopl %edx\n"
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
