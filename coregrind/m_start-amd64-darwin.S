
/*--------------------------------------------------------------------*/
/*--- Darwin amd64 bootstrap.               m_start-amd64-darwin.S ---*/
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

#if defined(VGP_amd64_darwin)

#include "pub_core_basics_asm.h"

	.text
	.align	3,0x90
Ldyld_stub_binding_helper:
        pushq   %r11
        leaq    ___dso_handle(%rip), %r11
        pushq   %r11
	jmpq    *Ldyld_lazy_symbol_binding_entry_point(%rip)

	.dyld
	.align	3
Ldyld_lazy_symbol_binding_entry_point:
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	Ldyld_stub_binding_helper
	.quad	0

		
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
	// sp+8-> argv[0]
	// sp  -> argc
	
	.text
	.align 3,0x90
	.globl __start
__start:
	movq	%rsp, %rdi	// save &argc
	andq	$-16, %rsp	// align stack
	pushq	$0		// push NULL "return address" for backtraces
	pushq	$0		// push fake saved ebp and align stack
	movq	%rsp, %rbp	// save frame pointer
	call	__start_in_C_darwin  // __start_in_C_darwin(&argc)
	
	// should not reach here
	int $3
	int $3

#endif // defined(VGP_amd64_darwin)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
