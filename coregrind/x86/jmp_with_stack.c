
/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2005 Julian Seward 
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


#define ZERO_ALL_INT_REGS \
   "   movl $0, %eax\n"  \
   "   movl $0, %ebx\n"  \
   "   movl $0, %ecx\n"  \
   "   movl $0, %edx\n"  \
   "   movl $0, %esi\n"  \
   "   movl $0, %edi\n"  \
   "   movl $0, %ebp\n"


/* Jump to 'dst', but first set the stack pointer to 'stack'.  Also,
   clear all the integer registers before entering 'dst'.  It's
   important that the stack pointer is set to exactly 'stack' and not
   (eg) stack - apparently_harmless_looking_small_offset.  Basically
   because the code at 'dst' might be wanting to scan the area above
   'stack' (viz, the auxv array), and putting spurious words on the
   stack confuses it.
*/
/*
__attribute__((noreturn))
void jump_and_switch_stacks ( Addr stack, Addr dst );

   4(%esp) == stack
   8(%esp) == f
*/
asm(
".global jump_and_switch_stacks\n"
"jump_and_switch_stacks:\n"
"   movl   %esp, %esi\n"    /* remember old stack pointer */
"   movl   4(%esi), %esp\n" /* set stack */
"   pushl  8(%esi)\n"       /* f to stack*/
    ZERO_ALL_INT_REGS
"   ret\n"                  /* jump to f */
"   ud2\n"                  /* should never get here */
);



/* Call f(arg1), but first switch stacks, using 'stack' as the new
   stack, and use 'retaddr' as f's return-to address.  Also, clear all
   the integer registers before entering f.*/
/*
__attribute__((noreturn))
void call_on_new_stack_0_1 ( Addr stack,
			     Addr retaddr,
			     void (*f)(Word),
                             Word arg1 );
    4(%esp) == stack
    8(%esp) == retaddr
   12(%esp) == f
   16(%esp) == arg1
*/
asm(
".global call_on_new_stack_0_1\n"
"call_on_new_stack_0_1:\n"
"   movl %esp, %esi\n"      /* remember old stack pointer */
"   movl 4(%esi), %esp\n"   /* set stack */
"   pushl 16(%esi)\n"       /* arg1 to stack */
"   pushl  8(%esi)\n"       /* retaddr to stack */
"   pushl 12(%esi)\n"       /* f to stack */
    ZERO_ALL_INT_REGS
"   ret\n"                  /* jump to f */
"   ud2\n"                  /* should never get here */
);


#undef ZERO_ALL_INT_REGS
