
/*--------------------------------------------------------------------*/
/*--- The trampoline code page.              pub_core_trampoline.h ---*/
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

#ifndef __PUB_CORE_TRAMPOLINE_H
#define __PUB_CORE_TRAMPOLINE_H

#include "pub_core_basics.h"   // VG_ macro

//--------------------------------------------------------------------
// PURPOSE: This module defines a few replacement functions for Linux
// vsyscalls, which we can't implement directly.  It also contains
// stubs for signal returns.  Note, all the code within runs on the
// simulated CPU.  The vsyscall stubs are gotten to by use of the 
// redirect mechanism.
//
// Note: generally, putting replacement functions in here is a bad
// idea, since any Dwarf frame-unwind info attached to them will not
// be seen by the unwinder in gcc's runtime support.  This means
// unwinding during exception handling by gcc tends to fail if it
// encounters one of these replacement functions.  A better place to
// put them is in one of the .so's preloaded into the client, since
// the client's ld.so will know about it and so gcc's unwinder
// (somehow) is able to get hold of it.
//--------------------------------------------------------------------

/* These two delimit our handwritten assembly code, so we can tell
   tools which track memory that this area should be regarded as
   readable, at least.  Otherwise Memcheck complains we're jumping to
   invalid addresses. */

extern Addr VG_(trampoline_stuff_start);
extern Addr VG_(trampoline_stuff_end);

#if defined(VGP_x86_freebsd)
extern void VG_(x86_freebsd_SUBST_FOR_sigreturn);
#endif

#if defined(VGP_amd64_freebsd)
extern void VG_(amd64_freebsd_SUBST_FOR_sigreturn);
#endif

#if defined(VGP_x86_linux)
extern Addr VG_(x86_linux_SUBST_FOR_sigreturn);
extern Addr VG_(x86_linux_SUBST_FOR_rt_sigreturn);
extern Char* VG_(x86_linux_REDIR_FOR_index) ( const Char*, Int );
extern UInt VG_(x86_linux_REDIR_FOR_strlen)( void* );
#endif

#if defined(VGP_amd64_linux)
extern Addr VG_(amd64_linux_SUBST_FOR_rt_sigreturn);
extern Addr VG_(amd64_linux_REDIR_FOR_vgettimeofday);
extern Addr VG_(amd64_linux_REDIR_FOR_vtime);
extern Addr VG_(amd64_linux_REDIR_FOR_vgetcpu);
extern UInt VG_(amd64_linux_REDIR_FOR_strlen)( void* );
extern Char* VG_(amd64_linux_REDIR_FOR_index) ( const Char*, Int );
#endif

#if defined(VGP_ppc32_linux)
extern Addr  VG_(ppc32_linux_SUBST_FOR_sigreturn);
extern Addr  VG_(ppc32_linux_SUBST_FOR_rt_sigreturn);
extern UInt  VG_(ppc32_linux_REDIR_FOR_strlen)( void* );
extern UInt  VG_(ppc32_linux_REDIR_FOR_strcmp)( void*, void* );
extern void* VG_(ppc32_linux_REDIR_FOR_strchr)( void*, Int );
#endif

#if defined(VGP_ppc64be_linux) || defined(VGP_ppc64le_linux)
extern Addr  VG_(ppc64_linux_SUBST_FOR_rt_sigreturn);
extern UInt  VG_(ppc64_linux_REDIR_FOR_strlen)( void* );
extern void* VG_(ppc64_linux_REDIR_FOR_strchr)( void*, Int );
/* A label (sans dot) marking the ultra-magical return stub via which
   all redirected and wrapped functions are made to "return" on
   ppc64-linux.  The one insn at this label is never really
   translated.  Instead, m_translate generates IR to restore the
   thread's LR and R2 registers from a small stack in the ppc64 guest
   state structure, and then branch to LR.  Convoluted?  Confusing?
   You betcha.  Could I think of anything simpler?  No. */
extern Addr VG_(ppctoc_magic_redirect_return_stub);
#endif

#if defined(VGP_arm_linux)
extern Addr  VG_(arm_linux_SUBST_FOR_sigreturn);
extern Addr  VG_(arm_linux_SUBST_FOR_rt_sigreturn);
extern UInt  VG_(arm_linux_REDIR_FOR_strlen)( void* );
extern void* VG_(arm_linux_REDIR_FOR_index) ( void*, Int );
extern void* VG_(arm_linux_REDIR_FOR_memcpy)( void*, void*, Int );
extern void* VG_(arm_linux_REDIR_FOR_strcmp)( void*, void* );
#endif

#if defined(VGP_arm64_linux)
extern Addr  VG_(arm64_linux_SUBST_FOR_rt_sigreturn);
extern ULong VG_(arm64_linux_REDIR_FOR_strlen)( void* );
extern void* VG_(arm64_linux_REDIR_FOR_index) ( void*, Long );
extern Long  VG_(arm64_linux_REDIR_FOR_strcmp)( void*, void* );
#endif

#if defined(VGP_x86_darwin)
extern Addr  VG_(x86_darwin_SUBST_FOR_sigreturn);
extern SizeT VG_(x86_darwin_REDIR_FOR_strlen)( void* );
extern SizeT VG_(x86_darwin_REDIR_FOR_strcmp)( void*, void* );
extern void* VG_(x86_darwin_REDIR_FOR_strcat)( void*, void * );
extern char* VG_(x86_darwin_REDIR_FOR_strcpy)( char *s1, char *s2 );
extern SizeT VG_(x86_darwin_REDIR_FOR_strlcat)( char *s1, const char *s2,
                                                SizeT size );
#endif

#if defined(VGP_amd64_darwin)
extern Addr  VG_(amd64_darwin_SUBST_FOR_sigreturn);
extern SizeT VG_(amd64_darwin_REDIR_FOR_strlen)( void* );
extern SizeT VG_(amd64_darwin_REDIR_FOR_strcmp)( void*, void* );
extern void* VG_(amd64_darwin_REDIR_FOR_strcat)( void*, void * );
extern char* VG_(amd64_darwin_REDIR_FOR_strcpy)( char *s1, char *s2 );
extern SizeT VG_(amd64_darwin_REDIR_FOR_strlcat)( char *s1, const char *s2,
                                                  SizeT size );
extern UInt VG_(amd64_darwin_REDIR_FOR_arc4random)( void );
# if DARWIN_VERS == DARWIN_10_9
  extern char* VG_(amd64_darwin_REDIR_FOR_strchr)( const char*, int );
# endif
#endif

#if defined(VGP_s390x_linux)
extern Addr VG_(s390x_linux_SUBST_FOR_sigreturn);
extern Addr VG_(s390x_linux_SUBST_FOR_rt_sigreturn);
// Note: Long for the 2nd parameter because according to z-series ABI,
// section "Parameter Passing" SIMPLE_ARG:
// "Values shorter than 64 bits are sign- or zero-extended
// (as appropriate) to 64 bits."
extern void* VG_(s390x_linux_REDIR_FOR_index) ( void*, Long );
#endif

#if defined(VGP_mips32_linux)
extern Addr  VG_(mips32_linux_SUBST_FOR_sigreturn);
extern Addr  VG_(mips32_linux_SUBST_FOR_rt_sigreturn);
extern Char* VG_(mips32_linux_REDIR_FOR_index)( const Char*, Int );
extern UInt  VG_(mips32_linux_REDIR_FOR_strlen)( void* );
#endif

#if defined(VGP_mips64_linux)
extern Addr  VG_(mips64_linux_SUBST_FOR_rt_sigreturn);
extern Char* VG_(mips64_linux_REDIR_FOR_index)( const Char*, Int );
extern UInt  VG_(mips64_linux_REDIR_FOR_strlen)( void* );
#endif

#if defined(VGP_nanomips_linux)
extern Addr  VG_(nanomips_linux_SUBST_FOR_rt_sigreturn);
extern Char* VG_(nanomips_linux_REDIR_FOR_index)( const Char*, Int );
extern UInt  VG_(nanomips_linux_REDIR_FOR_strlen)( void* );
#endif

#if defined(VGP_x86_solaris)
extern SizeT VG_(x86_solaris_REDIR_FOR_strcmp)(const HChar *, const HChar *);
extern SizeT VG_(x86_solaris_REDIR_FOR_strlen)(const HChar *);
#endif

#if defined(VGP_amd64_solaris)
extern HChar *VG_(amd64_solaris_REDIR_FOR_strcpy)(HChar *, const HChar *);
extern HChar *VG_(amd64_solaris_REDIR_FOR_strncpy)(HChar *, const HChar *,
                                                  SizeT);
extern Int VG_(amd64_solaris_REDIR_FOR_strcmp)(const HChar *, const HChar *);
extern HChar *VG_(amd64_solaris_REDIR_FOR_strcat)(HChar *, const HChar *);
extern SizeT VG_(amd64_solaris_REDIR_FOR_strlen)(const HChar *);
#endif

#endif   // __PUB_CORE_TRAMPOLINE_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
