
/*--------------------------------------------------------------------*/
/*--- Platform-specific stuff for the core.                        ---*/
/*---                                  amd64-linux/core_platform.h ---*/
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

#ifndef __AMD64_LINUX_CORE_PLATFORM_H
#define __AMD64_LINUX_CORE_PLATFORM_H

//#include "core_platform_asm.h"    // platform-specific asm  stuff
//#include "platform_arch.h"        // platform-specific tool stuff

/* ---------------------------------------------------------------------
   ucontext stuff
   ------------------------------------------------------------------ */

#define VGP_UCONTEXT_INSTR_PTR(uc)     ((uc)->uc_mcontext.rip)
#define VGP_UCONTEXT_STACK_PTR(uc)     ((uc)->uc_mcontext.rsp)
#define VGP_UCONTEXT_FRAME_PTR(uc)     ((uc)->uc_mcontext.rbp)
#define VGP_UCONTEXT_SYSCALL_NUM(uc)   ((uc)->uc_mcontext.rax)
#define VGP_UCONTEXT_SYSCALL_RET(uc)   ((uc)->uc_mcontext.rax)

/* ---------------------------------------------------------------------
   mmap() stuff
   ------------------------------------------------------------------ */

#define VGP_DO_MMAP(ret, start, length, prot, flags, fd, offset) { \
   ret = VG_(do_syscall6)(__NR_mmap, (UWord)(start), (length),          \
                         (prot), (flags), (fd), (offset));              \
} while (0)

#endif   // __AMD64_LINUX_CORE_PLATFORM_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
