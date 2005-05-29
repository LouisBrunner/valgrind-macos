
/*--------------------------------------------------------------------*/
/*--- Platform-specific stuff for the core.                        ---*/
/*---                                    x86-linux/core_platform.h ---*/
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

#ifndef __X86_LINUX_CORE_PLATFORM_H
#define __X86_LINUX_CORE_PLATFORM_H

//#include "core_platform_asm.h"    // platform-specific asm  stuff
//#include "platform_arch.h"        // platform-specific tool stuff

/* ---------------------------------------------------------------------
   ucontext stuff
   ------------------------------------------------------------------ */

#define VGP_UCONTEXT_INSTR_PTR(uc)     ((uc)->uc_mcontext.eip)
#define VGP_UCONTEXT_STACK_PTR(uc)     ((uc)->uc_mcontext.esp)
#define VGP_UCONTEXT_FRAME_PTR(uc)     ((uc)->uc_mcontext.ebp)
#define VGP_UCONTEXT_SYSCALL_NUM(uc)   ((uc)->uc_mcontext.eax)
#define VGP_UCONTEXT_SYSCALL_RET(uc)   ((uc)->uc_mcontext.eax)

/* ---------------------------------------------------------------------
   mmap() stuff
   ------------------------------------------------------------------ */

#define VGP_DO_MMAP(ret, start, length, prot, flags, fd, offset) {      \
   UWord __args[6];                                                     \
                                                                        \
   __args[0] = (UWord)(start);                                          \
   __args[1] = (length);                                                \
   __args[2] = (prot);                                                  \
   __args[3] = (flags);                                                 \
   __args[4] = (fd);                                                    \
   __args[5] = (offset);                                                \
                                                                        \
   ret = VG_(do_syscall1)(__NR_mmap, (UWord)(&(__args[0])) );           \
} while (0)

#define VGP_GET_MMAP_ARGS(tst, a1, a2, a3, a4, a5, a6) do {     \
   UInt *arg_block = (UInt*)(tst->arch.vex.VGP_SYSCALL_ARG1);   \
   PRE_MEM_READ( "old_mmap(args)", (Addr)arg_block, 6*sizeof(UWord) );\
   a1 = arg_block[0];                                           \
   a2 = arg_block[1];                                           \
   a3 = arg_block[2];                                           \
   a4 = arg_block[3];                                           \
   a5 = arg_block[4];                                           \
   a6 = arg_block[5];                                           \
} while (0)

#endif   // __X86_LINUX_CORE_PLATFORM_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
