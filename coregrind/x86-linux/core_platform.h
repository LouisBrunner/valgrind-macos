
/*--------------------------------------------------------------------*/
/*--- Platform-specific stuff for the core.                        ---*/
/*---                                    x86-linux/core_platform.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2004 Nicholas Nethercote
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

#ifndef __X86_LINUX_CORE_PLATFORM_H
#define __X86_LINUX_CORE_PLATFORM_H

//#include "core_platform_asm.h"    // platform-specific asm  stuff
//#include "platform_arch.h"        // platform-specific tool stuff

/* ---------------------------------------------------------------------
   Dealing with registers
   ------------------------------------------------------------------ */

// Accessors for the ThreadArchState
#define PLATFORM_SYSCALL_NUM     guest_EAX
#define PLATFORM_SYSCALL_ARG1    guest_EBX
#define PLATFORM_SYSCALL_ARG2    guest_ECX
#define PLATFORM_SYSCALL_ARG3    guest_EDX
#define PLATFORM_SYSCALL_ARG4    guest_ESI
#define PLATFORM_SYSCALL_ARG5    guest_EDI
#define PLATFORM_SYSCALL_ARG6    guest_EBP
#define PLATFORM_SYSCALL_RET     guest_EAX

// Setting a syscall result
#define PLATFORM_SET_SYSCALL_RESULT(regs, val)     \
   ((regs).vex.guest_EAX = (val))

// Setting thread regs and shadow regs from within the core
#define SET_SYSCALL_RETVAL(zztid, zzval) \
   SET_THREAD_REG(zztid, zzval, SYSCALL_RET, post_reg_write, \
                  Vg_CoreSysCall, zztid, O_SYSCALL_RET, sizeof(UWord))

/* ---------------------------------------------------------------------
   Exports of vg_ldt.c
   ------------------------------------------------------------------ */

// XXX: eventually all these should be x86-private, and not visible to the
// core (except maybe do_useseg()?)

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

/* ---------------------------------------------------------------------
   ucontext stuff
   ------------------------------------------------------------------ */

#define UCONTEXT_INSTR_PTR(uc)   ((uc)->uc_mcontext.eip)
#define UCONTEXT_STACK_PTR(uc)   ((uc)->uc_mcontext.esp)
#define UCONTEXT_FRAME_PTR(uc)   ((uc)->uc_mcontext.ebp)
#define UCONTEXT_SYSCALL_NUM(uc) ((uc)->uc_mcontext.eax)

/* ---------------------------------------------------------------------
   mmap() stuff
   ------------------------------------------------------------------ */

#define PLATFORM_DO_MMAP(ret, start, length, prot, flags, fd, offset) { \
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

#define PLATFORM_GET_MMAP_ARGS(tst, a1, a2, a3, a4, a5, a6) do {\
   UInt *arg_block = (UInt*)SYSCALL_ARG1(tst->arch);            \
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
