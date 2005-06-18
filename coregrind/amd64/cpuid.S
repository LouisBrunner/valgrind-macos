
##--------------------------------------------------------------------##
##--- Support for determining CPU characteristics.   amd64/cpuid.S ---##
##--------------------------------------------------------------------##

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

#include "pub_basics_asm.h"

/*
    Bool VG_(has_cpuid)(void)
 */
.globl VG_(has_cpuid)
VG_(has_cpuid):
        movq    $1, %rax
        ret

/*
    int VG_(cpuid)(UInt eax,
                   UInt *eax_ret, UInt *ebx_ret, UInt *ecx_ret, UInt *edx_ret)
 */
.globl VG_(cpuid)
VG_(cpuid):
        pushq   %rbp
        movq    %rsp, %rbp
        pushq   %rbx
        movl    %edi, %eax
        movq    %rdx, %rdi
        movq    %rcx, %r9
        /*
           eax_ret now in %rsi
           ebx_ret now in %rdi
           ecx_ret now in %r9
           edx_ret now in %r8
         */
        cpuid
        testq   %rsi, %rsi
        jz      1f
        movl    %eax, (%rsi)
1:
        testq   %rdi, %rdi
        jz      2f
        movl    %ebx, (%rdi)
2:
        testq   %r9, %r9
        jz      3f
        movl    %ecx, (%r9)
3:
        testq   %r8, %r8
        jz      4f
        movl    %edx, (%r8)
4:
        popq    %rbx
        movq    %rbp, %rsp
        popq    %rbp
        ret
        
/* Let the linker know we don't need an executable stack */
.section .note.GNU-stack,"",@progbits

##--------------------------------------------------------------------##
##--- end                                    	                   ---##
##--------------------------------------------------------------------##
