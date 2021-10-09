
/*--------------------------------------------------------------------*/
/*--- Create/destroy signal delivery frames.                       ---*/
/*---                                          pub_core_sigframe.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2017 Julian Seward
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
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

#ifndef __PUB_CORE_SIGFRAME_H
#define __PUB_CORE_SIGFRAME_H

#include "pub_core_basics.h"     // VG_ macro
#include "pub_core_vki.h"        // vki_sigset_t et al.

//--------------------------------------------------------------------
// PURPOSE: This module creates and destroys signal delivery frames
// for client threads, saving/restoring the thread CPU state in the
// frame appropriately.
//--------------------------------------------------------------------

/* This is an arbitrary si_code that we only use internally for SIGSEGV.
   It corresponds to the value SI_KERNEL on Linux, but that's not really
   of any significance. */
#define VKI_SEGV_MADE_UP_GPF 0x80

/* Create a signal frame for thread 'tid'. */
extern 
void VG_(sigframe_create) ( ThreadId tid, 
                            Bool on_altstack,
                            Addr sp_top_of_frame,
                            const vki_siginfo_t *siginfo,
                            const struct vki_ucontext *uc,
                            void *handler, 
                            UInt flags,
                            const vki_sigset_t *mask,
                            void *restorer );

/* Remove a signal frame from thread 'tid's stack, and 
   restore the CPU state from it. */
#ifdef VGO_freebsd
extern 
void VG_(sigframe_destroy)( ThreadId tid );
#else
extern 
void VG_(sigframe_destroy)( ThreadId tid, Bool isRT );
#endif
#if defined(VGO_solaris)
extern
void VG_(sigframe_return)(ThreadId tid, const vki_ucontext_t *uc);
#endif

#endif   // __PUB_CORE_SIGFRAME_H

/*--------------------------------------------------------------------*/
/*--- end                                      pub_core_sigframe.h ---*/
/*--------------------------------------------------------------------*/
