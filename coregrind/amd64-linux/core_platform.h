
/*--------------------------------------------------------------------*/
/*--- Platform-specific stuff for the core.                        ---*/
/*---                                  amd64-linux/core_platform.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2005 Nicholas Nethercote
      njn25@cam.ac.uk

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
   Dealing with registers
   ------------------------------------------------------------------ */

// Accessors for the ThreadArchState
#define PLATFORM_SYSCALL_NUM     guest_RAX
#define PLATFORM_SYSCALL_ARG1    guest_RDI
#define PLATFORM_SYSCALL_ARG2    guest_RSI
#define PLATFORM_SYSCALL_ARG3    guest_RDX
#define PLATFORM_SYSCALL_ARG4    guest_R10
#define PLATFORM_SYSCALL_ARG5    guest_R8
#define PLATFORM_SYSCALL_ARG6    guest_R9
#define PLATFORM_SYSCALL_RET     guest_RAX

// Setting a syscall result
#define PLATFORM_SET_SYSCALL_RESULT(regs, val)     \
   ((regs).vex.guest_RAX = (val))

// Setting thread regs and shadow regs from within the core
#define SET_SYSCALL_RETVAL(zztid, zzval) \
   SET_THREAD_REG(zztid, zzval, SYSCALL_RET, post_reg_write, \
                  Vg_CoreSysCall, zztid, O_SYSCALL_RET, sizeof(UWord))

/* ---------------------------------------------------------------------
   Exports of vg_ldt.c
   ------------------------------------------------------------------ */

// XXX: eventually all these should be x86-private, and not visible to the
// core (except maybe do_useseg()?)

#if 0
/* Simulate the modify_ldt syscall. */
extern Int VG_(sys_modify_ldt) ( ThreadId tid,
                                 Int func, void* ptr, UInt bytecount );

/* Simulate the {get,set}_thread_area syscalls. */
extern Int VG_(sys_set_thread_area) ( ThreadId tid,
                                      vki_modify_ldt_t* info );
extern Int VG_(sys_get_thread_area) ( ThreadId tid,
                                      vki_modify_ldt_t* info );

/* Called from generated code.  Given a segment selector and a virtual
   address, return a linear address, and do limit checks too. */
extern Addr VG_(do_useseg) ( UInt seg_selector, Addr virtual_addr );
#endif

/* ---------------------------------------------------------------------
   ucontext stuff
   ------------------------------------------------------------------ */

#define UCONTEXT_INSTR_PTR(uc)   ((uc)->uc_mcontext.rip)
#define UCONTEXT_STACK_PTR(uc)   ((uc)->uc_mcontext.rsp)
#define UCONTEXT_FRAME_PTR(uc)   ((uc)->uc_mcontext.rbp)
#define UCONTEXT_SYSCALL_NUM(uc) ((uc)->uc_mcontext.rax)

/* ---------------------------------------------------------------------
   mmap() stuff
   ------------------------------------------------------------------ */

#define PLATFORM_DO_MMAP(ret, start, length, prot, flags, fd, offset) { \
   ret = VG_(do_syscall6)(__NR_mmap, (UWord)(start), (length),          \
                         (prot), (flags), (fd), (offset));              \
} while (0)

#define PLATFORM_GET_MMAP_ARGS(tst, a1, a2, a3, a4, a5, a6) do {\
   I_die_here; \
} while (0)


/* Use libc setjmp/longjmp.  longjmp must not restore signal mask
   state, but does need to pass though "val". */
#include <setjmp.h>       /* for jmp_buf         */

#define SETJMP(env)		setjmp(env)
#define LONGJMP(env, val)	longjmp(env, val)

#endif   // __AMD64_LINUX_CORE_PLATFORM_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
