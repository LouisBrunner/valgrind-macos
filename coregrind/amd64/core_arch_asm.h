/*--------------------------------------------------------------------*/
/*---                                        amd64/core_arch_asm.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2005 Nicholas Nethercote
      njn@valgrind.org

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

#ifndef __AMD64_CORE_ARCH_ASM_H
#define __AMD64_CORE_ARCH_ASM_H

// Print a constant from asm code.
#define OYNK(nnn) push %r8 ; push %r9 ; push %r10; push %r11; \
                  push %rax; push %rbx; push %rcx; push %rdx; \
                  push %rsi; push %rdi; \
                  movl $nnn, %edi; call VG_(oynk); \
                  pop %rdi; pop %rsi; pop %rdx; pop %rcx; \
                  pop %rbx; pop %rax; pop %r11; pop %r10; \
                  pop %r9 ; pop %r8

#endif   // __AMD64_CORE_ARCH_ASM_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
