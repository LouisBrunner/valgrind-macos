
/*--------------------------------------------------------------------*/
/*--- Solaris-specific syscalls stuff.      priv_syswrap-solaris.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2011-2017 Petr Pavlu
      setup@dagobah.cz

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

#ifndef __PRIV_SYSWRAP_SOLARIS_H
#define __PRIV_SYSWRAP_SOLARIS_H

#include "pub_core_basics.h"        // VG_ macro
#include "priv_types_n_macros.h"    // DECL_TEMPLATE
#include "pub_core_tooliface.h"     // CorePart

/* Macro to join a syscall name with a syscall variant. */
#define SC2(name, subname) \
   name "_" subname

/* Macro to join a syscall name with its variant and sub-variant. */
#define SC3(name, subname, subsubname) \
   name "_" subname "_" subsubname

extern void ML_(call_on_new_stack_0_1)(Addr stack, Addr retaddr,
                                       void (*f)(Word), Word arg1);
extern Word ML_(start_thread_NORETURN)(void *arg);
extern Addr ML_(allocstack)           (ThreadId tid);
extern void ML_(setup_start_thread_context)(ThreadId tid, vki_ucontext_t *uc);

extern UInt ML_(fletcher32)(UShort *buf, SizeT blocks);
extern ULong ML_(fletcher64)(UInt *buf, SizeT blocks);
extern void ML_(save_machine_context)(ThreadId tid, vki_ucontext_t *uc,
                                      CorePart part);
extern void ML_(restore_machine_context)(ThreadId tid, vki_ucontext_t *uc,
                                         CorePart part, Bool esp_is_thrptr);

#if defined(VGP_x86_solaris)

extern void ML_(setup_gdt)(VexGuestX86State *vex);
extern void ML_(cleanup_gdt)(VexGuestX86State *vex);
extern void ML_(update_gdt_lwpgs)(ThreadId tid);

/* prototypes */
DECL_TEMPLATE(x86_solaris, sys_fstatat64);
DECL_TEMPLATE(x86_solaris, sys_openat64);
DECL_TEMPLATE(x86_solaris, sys_llseek32);
DECL_TEMPLATE(x86_solaris, sys_mmap64);
DECL_TEMPLATE(x86_solaris, sys_stat64);
DECL_TEMPLATE(x86_solaris, sys_lstat64);
DECL_TEMPLATE(x86_solaris, sys_fstat64);
DECL_TEMPLATE(x86_solaris, sys_statvfs64);
DECL_TEMPLATE(x86_solaris, sys_fstatvfs64);
DECL_TEMPLATE(x86_solaris, sys_setrlimit64);
DECL_TEMPLATE(x86_solaris, sys_getrlimit64);
DECL_TEMPLATE(x86_solaris, sys_pread64);
DECL_TEMPLATE(x86_solaris, sys_pwrite64);
DECL_TEMPLATE(x86_solaris, sys_open64);

#elif defined(VGP_amd64_solaris)
/* Nothing yet. */

#else
#  error "Unknown platform"
#endif

#endif   // __PRIV_SYSWRAP_SOLARIS_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
