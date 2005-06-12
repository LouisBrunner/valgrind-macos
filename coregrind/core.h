
/*--------------------------------------------------------------------*/
/*--- A header file for various private parts of Valgrind's core.  ---*/
/*---                                                       core.h ---*/
/*--------------------------------------------------------------------*/
 
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

#ifndef __CORE_H
#define __CORE_H

#include "tool.h"          // tool stuff

#include "libvex.h"
#if defined(VGA_x86)
#  include "libvex_guest_x86.h"
#  define VGA_ELF_ENDIANNESS  ELFDATA2LSB
#  define VGA_ELF_MACHINE     EM_386
#  define VGA_ELF_CLASS       ELFCLASS32
#  define VGA_INSTR_PTR       guest_EIP
#  define VGA_STACK_PTR       guest_ESP
#  define VGA_FRAME_PTR       guest_EBP
#  define VGA_CLREQ_ARGS      guest_EAX
#  define VGA_CLREQ_RET       guest_EDX
#elif defined(VGA_amd64)
#  include "libvex_guest_amd64.h"
#  define VGA_ELF_ENDIANNESS  ELFDATA2LSB
#  define VGA_ELF_MACHINE     EM_X86_64
#  define VGA_ELF_CLASS       ELFCLASS64
#  define VGA_INSTR_PTR       guest_RIP
#  define VGA_STACK_PTR       guest_RSP
#  define VGA_FRAME_PTR       guest_RBP
#  define VGA_CLREQ_ARGS      guest_RAX
#  define VGA_CLREQ_RET       guest_RDX
#elif defined(VGA_arm)
#define VGA_ELF_ENDIANNESS     ELFDATA2LSB
#define VGA_ELF_MACHINE        EM_ARM
#define VGA_ELF_CLASS          ELFCLASS32
   // XXX: Not sure, but I think:
   //   r11 = frame pointer
   //   r12 = "implicit parameter" (neither caller-save, nor callee-save)
   //   r13 = stack pointer
   //   r14 = link register
   //   r15 = program counter
#  define VGA_INSTR_PTR       guest_R15
#  define VGA_STACK_PTR       guest_R13
#  define VGA_FRAME_PTR       guest_R11
#  define VGA_CLREQ_ARGS      guest_R0
#  define VGA_CLREQ_RET       guest_R0
#else
#  error Unknown arch
#endif




#include <setjmp.h>        // for jmp_buf

#include "pub_core_scheduler.h"   // for types 'ThreadArchState'

/* ---------------------------------------------------------------------
   Exports of vg_intercept.c
   ------------------------------------------------------------------ */

/* These are the internal client request codes.  The publically-visible
   request codes are also defined in valgrind.h, and similar headers for
   some tools. */

/* Get the tool's malloc-wrapping functions */
#define VG_USERREQ__GET_MALLOCFUNCS	    0x3030

/* Internal equivalent of VALGRIND_PRINTF . */
#define VG_USERREQ__INTERNAL_PRINTF         0x3103

/* Denote the finish of __libc_freeres_wrapper(). 
   A synonym for exit. */
#define VG_USERREQ__LIBC_FREERES_DONE       0x3029

/* Intercept prefix stuff.  See
   coregrind/m_replace_malloc/vg_replace_malloc.c for details. */
#define VG_INTERCEPT(name)       _vgi_##name
#define VG_INTERCEPT_PREFIX      "_vgi_"
#define VG_INTERCEPT_PREFIX_LEN  5

/* Not sure what these are for.  Todo: clarify */
#define VG_WRAPPER_PREFIX        "_vgw_"
#define VG_WRAPPER_PREFIX_LEN    5
#define VG_WRAPPER(name)         _vgw_##name
#define VG_WRAPPER_ALIAS(name)   "_vgw_" #name

/* ---------------------------------------------------------------------
   Exports of vg_syscall.S
   ------------------------------------------------------------------ */

extern void VG_(sigreturn)(void);

/* ---------------------------------------------------------------------
   Exports of vg_helpers.S
   ------------------------------------------------------------------ */

/* Information about trampoline code (for signal return and syscalls) */
extern const Char VG_(trampoline_code_start);
extern const Int  VG_(trampoline_code_length);
extern const Int  VG_(tramp_sigreturn_offset);
extern const Int  VG_(tramp_rt_sigreturn_offset);
extern const Int  VG_(tramp_syscall_offset);
extern const Int  VG_(tramp_gettimeofday_offset);
extern const Int  VG_(tramp_time_offset);
 
// ---------------------------------------------------------------------
// Architecture-specific things defined in eg. x86/*.c
// ---------------------------------------------------------------------

// Returns the architecture and subarchitecture, or indicates
// that this subarchitecture is unable to run Valgrind
// Returns False to indicate we cannot proceed further.
extern Bool VGA_(getArchAndSubArch)( /*OUT*/VexArch*, 
                                     /*OUT*/VexSubArch* );

// Accessors for the ThreadArchState
#define INSTR_PTR(regs)    ((regs).vex.VGA_INSTR_PTR)
#define STACK_PTR(regs)    ((regs).vex.VGA_STACK_PTR)
#define FRAME_PTR(regs)    ((regs).vex.VGA_FRAME_PTR)
#define CLREQ_ARGS(regs)   ((regs).vex.VGA_CLREQ_ARGS)
#define CLREQ_RET(regs)    ((regs).vex.VGA_CLREQ_RET)
// Offsets for the Vex state
#define O_STACK_PTR        (offsetof(VexGuestArchState, VGA_STACK_PTR))
#define O_CLREQ_RET        (offsetof(VexGuestArchState, VGA_CLREQ_RET))


// Setting up the initial thread (1) state
extern void 
       VGA_(init_thread1state) ( Addr client_eip, 
                                 Addr esp_at_startup,
                                 /*MOD*/ ThreadArchState* arch );

// Run a thread from beginning to end. 
extern VgSchedReturnCode VGO_(thread_wrapper)(Word /*ThreadId*/ tid);

// wait until all other threads are dead
extern void VGA_(reap_threads)(ThreadId self);

// For attaching the debugger
extern Int  VGA_(ptrace_setregs_from_tst) ( Int pid, ThreadArchState* arch );

// Used by leakcheck
extern void VGA_(mark_from_registers)(ThreadId tid, void (*marker)(Addr));

/* ---------------------------------------------------------------------
   Finally - autoconf-generated settings
   ------------------------------------------------------------------ */

#include "config.h"

#endif /* ndef __CORE_H */

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/

