
/*
   This file is part of Valgrind, an extensible x86 protected-mode
   emulator for monitoring program execution on x86-Unixes.

   Copyright (C) 2000-2004 Julian Seward 
      jseward@acm.org

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307, USA.

   The GNU General Public License is contained in the file COPYING.
*/

#include "ume.h"

/* 
   Jump to a particular IP with a particular SP.  This is intended
   to simulate the initial CPU state when the kernel starts an program
   after exec; it therefore also clears all the other registers.
 */
void jmp_with_stack(Addr eip, Addr esp)
{
   // XXX: temporary only
extern int printf (__const char *__restrict __format, ...);
extern void exit (int __status);
   printf("jmp_with_stack: argh\n");
   exit(1);
#if 0
   asm volatile ("movl %1, %%esp;"	/* set esp */
		 "pushl %%eax;"		/* push esp */
		 "xorl	%%eax,%%eax;"	/* clear registers */
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
	   asm volatile("ud2");
#endif
} 
