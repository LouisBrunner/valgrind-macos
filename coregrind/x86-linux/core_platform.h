
/*--------------------------------------------------------------------*/
/*--- x86-Linux-specific stuff for the core.                       ---*/
/*---                                    x86-linux/core_platform.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an extensible x86 protected-mode
   emulator for monitoring program execution on x86-Unixes.

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
   Interesting registers
   ------------------------------------------------------------------ */

// Accessors for the arch_thread_t
#define PLATFORM_SYSCALL_NUM(regs)     ((regs).m_eax)
#define PLATFORM_SYSCALL_RET(regs)     ((regs).m_eax)
#define PLATFORM_SYSCALL_ARG1(regs)    ((regs).m_ebx)
#define PLATFORM_SYSCALL_ARG2(regs)    ((regs).m_ecx)
#define PLATFORM_SYSCALL_ARG3(regs)    ((regs).m_edx)
#define PLATFORM_SYSCALL_ARG4(regs)    ((regs).m_esi)
#define PLATFORM_SYSCALL_ARG5(regs)    ((regs).m_edi)
#define PLATFORM_SYSCALL_ARG6(regs)    ((regs).m_ebp)

#define PLATFORM_PRE_SYSCALL_RESULT(regs, val)     ((regs).m_eax = (val))

// Interesting register numbers
#define R_SYSCALL_NUM                  R_EAX
#define R_SYSCALL_ARG1                 R_EBX
#define R_SYSCALL_RET                  R_EAX

// Setting thread regs and shadow regs from within the core
#define SET_SYSCALL_RETVAL(zztid, zzval) \
   SET_THREAD_REG(zztid, zzval, PLATFORM_SYSCALL_RET, R_SYSCALL_RET, \
                  post_reg_write_syscall_return)

#endif   // __X86_LINUX_CORE_PLATFORM_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
