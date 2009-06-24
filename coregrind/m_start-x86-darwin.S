
/*--------------------------------------------------------------------*/
/*--- Darwin x86 bootstrap.                   m_start-x86-darwin.S ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2007 Apple Inc.
      Greg Parker  gparker@apple.com

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

#if defined(VGP_x86_darwin)

#include "pub_core_basics_asm.h"

	.text
	.align	2,0x90
Ldyld_stub_binding_helper:
	pushl   $__mh_execute_header
	jmpl    *Ldyld_lazy_symbol_binding_entry_point

	.dyld
	.align	2
Ldyld_lazy_symbol_binding_entry_point:
	.long	0
	.long	0
	.long	0
	.long	0
	.long	0
	.long	Ldyld_stub_binding_helper
	.long	0

		
	// Memory layout established by kernel:
	//
	//        0
	//        executable_name
	//        0
	//        envp[n]
	//        ...
	//        envp[0]
	//        0
	//        argv[argc-1]
	//        ...
	// sp+4-> argv[0]
	// sp  -> argc
	
	.text
	.align 2,0x90
	.globl __start
__start:
	movl	%esp, %eax	// save &argc
	andl	$-16, %esp	// align stack
	pushl	$0		// push NULL "return address" for backtraces
	pushl	$0		// push fake saved ebp
	movl	%esp, %ebp	// save frame pointer
	pushl	$0		// align stack
	pushl	%eax		// start_in_C_darwin(&argc)
	call	__start_in_C_darwin
	
	// should not reach here
	int $3
	int $3

#endif // defined(VGP_x86_darwin)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
