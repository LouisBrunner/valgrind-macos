
/*--------------------------------------------------------------------*/
/*--- The trampoline code page.              pub_core_trampoline.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2010 Julian Seward
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

#if defined(VGP_x86_linux)
extern Addr VG_(x86_linux_SUBST_FOR_sigreturn);
extern Addr VG_(x86_linux_SUBST_FOR_rt_sigreturn);
extern Char* VG_(x86_linux_REDIR_FOR_index) ( const Char*, Int );
#endif

#if defined(VGP_amd64_linux)
extern Addr VG_(amd64_linux_SUBST_FOR_rt_sigreturn);
extern Addr VG_(amd64_linux_REDIR_FOR_vgettimeofday);
extern Addr VG_(amd64_linux_REDIR_FOR_vtime);
extern UInt VG_(amd64_linux_REDIR_FOR_strlen)( void* );
#endif

#if defined(VGP_ppc32_linux)
extern Addr  VG_(ppc32_linux_SUBST_FOR_sigreturn);
extern Addr  VG_(ppc32_linux_SUBST_FOR_rt_sigreturn);
extern UInt  VG_(ppc32_linux_REDIR_FOR_strlen)( void* );
extern UInt  VG_(ppc32_linux_REDIR_FOR_strcmp)( void*, void* );
extern void* VG_(ppc32_linux_REDIR_FOR_strchr)( void*, Int );
#endif

#if defined(VGP_ppc64_linux)
extern Addr  VG_(ppc64_linux_SUBST_FOR_rt_sigreturn);
extern UInt  VG_(ppc64_linux_REDIR_FOR_strlen)( void* );
extern void* VG_(ppc64_linux_REDIR_FOR_strchr)( void*, Int );
/* A label (sans dot) marking the ultra-magical return stub via which
   all redirected and wrapped functions are made to "return" on
   ppc64-linux/ppc64-aix5/ppc32-aix5.  The one insn at this label is
   never really translated.  Instead, m_translate generates IR to
   restore the thread's LR and R2 registers from a small stack in the
   ppc64 guest state structure, and then branch to LR.  Convoluted?
   Confusing?  You betcha.  Could I think of anything simpler?  No. */
extern Addr VG_(ppctoc_magic_redirect_return_stub);
#endif

#if defined(VGP_arm_linux)
extern UInt  VG_(arm_linux_REDIR_FOR_strlen)( void* );
//extern void* VG_(arm_linux_REDIR_FOR_index) ( void*, Int );
extern void* VG_(arm_linux_REDIR_FOR_memcpy)( void*, void*, Int );
#endif

#if defined(VGP_ppc32_aix5)
/* A label (sans dot) marking the client start point for ppc32_aix5.
   This function is entered with r3 holding a pointer to the
   AIX5PreloadPage struct set up by m_initimg.  It first tries to
   __loadx the _core.so and _tool.so preloads mentioned in the struct;
   then it cleans up the register state to be more what it really
   should be at client startup, and finally it jumps to the client's
   real entry point. */
extern Addr VG_(ppc32_aix5_do_preloads_then_start_client);

/* See comment for VG_(ppctoc_magic_redirect_return_stub) above. */
extern Addr VG_(ppctoc_magic_redirect_return_stub);
#endif

#if defined(VGP_ppc64_aix5)
/* See comment for VG_(ppctoc_magic_redirect_return_stub) above. */
extern Addr VG_(ppctoc_magic_redirect_return_stub);

/* See comment for ppc32_aix5 equivalent above. */
extern Addr VG_(ppc64_aix5_do_preloads_then_start_client);
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
#endif

#endif   // __PUB_CORE_TRAMPOLINE_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
