
##--------------------------------------------------------------------##
##--- Support for determining CPU characteristics.     x86/cpuid.S ---##
##--------------------------------------------------------------------##

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

#if 0

#include "core_asm.h"

/*
    int VG_(cpuid)(UInt eax,
                   UInt *eax_ret, UInt *ebx_ret, UInt *ecx_ret, UInt *edx_ret)
 */
.globl VG_(cpuid)
VG_(cpuid):
        pushl   %ebp
        movl    %esp, %ebp
        pushl   %eax
        pushl   %ebx
        pushl   %ecx
        pushl   %edx
        pushl   %esi
        movl    8(%ebp), %eax
        cpuid
        movl    12(%ebp), %esi
        testl   %esi, %esi
        jz      1f
        movl    %eax, (%esi)
1:
        movl    16(%ebp), %esi
        testl   %esi, %esi
        jz      2f
        movl    %ebx, (%esi)
2:
        movl    20(%ebp), %esi
        testl   %esi, %esi
        jz      3f
        movl    %ecx, (%esi)
3:
        movl    24(%ebp), %esi
        testl   %esi, %esi
        jz      4f
        movl    %edx, (%esi)
4:
        popl    %esi
        popl    %edx
        popl    %ecx
        popl    %ebx
        popl    %eax
        movl    %ebp, %esp
        popl    %ebp
        ret
        
/* Let the linker know we don't need an executable stack */
.section .note.GNU-stack,"",@progbits

#endif /* 0 */

##--------------------------------------------------------------------##
##--- end                                    	                   ---##
##--------------------------------------------------------------------##
