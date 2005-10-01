
/*--------------------------------------------------------------------*/
/*--- The trampoline code page.              pub_core_trampoline.h ---*/
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

#ifndef __PUB_CORE_TRAMPOLINE_H
#define __PUB_CORE_TRAMPOLINE_H

//--------------------------------------------------------------------
// PURPOSE: This module defines a few replacement functions for Linux
// vsyscalls, which we can't implement directly.  It also contains
// stubs for signal returns.  Note, all the code within runs on the
// simulated CPU.  The vsyscall stubs are gotten to by use of the 
// redirect mechanism.
//--------------------------------------------------------------------

/* These two delimit our handwritten assembly code, so we can tell
   tools which track memory that this area should be regarded as
   readable, at least.  Otherwise Memcheck complains we're jumping to
   invalid addresses. */

extern void VG_(trampoline_stuff_start);
extern void VG_(trampoline_stuff_end);

#if defined(VGP_x86_linux)
extern void VG_(x86_linux_SUBST_FOR_sigreturn);
extern void VG_(x86_linux_SUBST_FOR_rt_sigreturn);
extern void VG_(x86_linux_REDIR_FOR__dl_sysinfo_int80);
#endif

#if defined(VGP_amd64_linux)
extern void VG_(amd64_linux_SUBST_FOR_rt_sigreturn);
extern void VG_(amd64_linux_REDIR_FOR_vgettimeofday);
extern void VG_(amd64_linux_REDIR_FOR_vtime);
#endif

#if defined(VGP_ppc32_linux)
extern UInt VG_(ppc32_linux_REDIR_FOR_strlen)( void* );
extern UInt VG_(ppc32_linux_REDIR_FOR_strcmp)( void*, void* );
#endif
 
#endif   // __PUB_CORE_TRAMPOLINE_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
