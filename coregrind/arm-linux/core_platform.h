
/*--------------------------------------------------------------------*/
/*--- Platform-specific stuff for the core.                        ---*/
/*---                                    arm-linux/core_platform.h ---*/
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

#ifndef __ARM_LINUX_CORE_PLATFORM_H
#define __ARM_LINUX_CORE_PLATFORM_H

//#include "core_platform_asm.h"    // platform-specific asm  stuff
//#include "platform_arch.h"        // platform-specific tool stuff

/* ---------------------------------------------------------------------
   Dealing with registers
   ------------------------------------------------------------------ */

// Accessors for the ThreadArchState
#define PLATFORM_SYSCALL_NUM     guest_SYSCALLNO
#define PLATFORM_SYSCALL_ARG1    guest_R0
#define PLATFORM_SYSCALL_ARG2    guest_R1
#define PLATFORM_SYSCALL_ARG3    guest_R2
#define PLATFORM_SYSCALL_ARG4    guest_R3
#define PLATFORM_SYSCALL_ARG5    guest_R4
#define PLATFORM_SYSCALL_ARG6    guest_R5
#define PLATFORM_SYSCALL_RET     guest_R0 // ToDo XXX ????????

#define PLATFORM_SET_SYSCALL_RESULT(regs, val)     \
   ((regs).vex.guest_R0 = (val))    // ToDo XXX ????????

// Setting thread regs and shadow regs from within the core
// XXX ToDo: not sure about this
#define SET_SYSCALL_RETVAL(zztid, zzval) \
   SET_THREAD_REG(zztid, zzval, SYSCALL_RET, post_reg_write, \
                  Vg_CoreSysCall, zztid, O_SYSCALL_RET, sizeof(UWord))

/* ---------------------------------------------------------------------
   ucontext stuff
   ------------------------------------------------------------------ */

#define UCONTEXT_INSTR_PTR(uc)   ((uc)->uc_mcontext.arm_pc)
#define UCONTEXT_STACK_PTR(uc)   ((uc)->uc_mcontext.arm_sp)
#define UCONTEXT_FRAME_PTR(uc)   ((uc)->uc_mcontext.arm_fp)
#define UCONTEXT_SYSCALL_NUM(uc) ((uc)->uc_mcontext.arm_r0)

/* ---------------------------------------------------------------------
   mmap() stuff
   ------------------------------------------------------------------ */

#define PLATFORM_DO_MMAP(ret, start, length, prot, flags, fd, offset) { \
   I_die_here; \
} while (0)

#define PLATFORM_GET_MMAP_ARGS(tst, a1, a2, a3, a4, a5, a6) do { \
   I_die_here; \
} while (0)

#endif   // __ARM_LINUX_CORE_PLATFORM_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
