#include "ume_arch.h"

void ume_go(addr_t eip, addr_t esp)
{
   asm volatile ("movl %1, %%esp;"
		 "pushl %%eax;"
		 "xorl	%%eax,%%eax;"
		 "xorl	%%ebx,%%ebx;"
		 "xorl	%%ecx,%%ecx;"
		 "xorl	%%edx,%%edx;"
		 "xorl	%%esi,%%esi;"
		 "xorl	%%edi,%%edi;"
		 "xorl	%%ebp,%%ebp;"

		 "ret"			/* return into entry */
		 : : "a" (eip), "r" (esp));
   /* we should never get here */
   for(;;)
	   ;
} 
