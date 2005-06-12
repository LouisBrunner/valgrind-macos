
/*--------------------------------------------------------------------*/
/*--- Standalone libc stuff.                   pub_core_libcbase.h ---*/
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

#ifndef __PUB_CORE_LIBCBASE_H
#define __PUB_CORE_LIBCBASE_H

//--------------------------------------------------------------------
// PURPOSE: This module contains all the libc code that is entirely
// standalone (other than the VG_() macro and some types defined 
// elsewhere):  string functions, char functions, and a few other things.
//--------------------------------------------------------------------

#include "pub_tool_libcbase.h"

/* ---------------------------------------------------------------------
   Fundamental functions for doing syscalls on this platform.
   ------------------------------------------------------------------ */

/* Do a syscall on this platform, with 6 args, and return the result
   in canonical format in a SysRes value. */

// We use a full prototype for VG_(do_syscall) rather than "..." to ensure
// that all arguments get converted to a UWord appropriately.  Not doing so
// can cause problems when passing 32-bit integers on 64-bit platforms,
// because the top 32-bits might not be zeroed appropriately, eg. as would
// happen with the 6th arg on AMD64 which is passed on the stack.

extern SysRes VG_(do_syscall) ( UWord sysno, 
                                UWord, UWord, UWord, 
                                UWord, UWord, UWord );

/* Macros make life easier. */

#define vgPlain_do_syscall0(s)             VG_(do_syscall)((s),0,0,0,0,0,0)
#define vgPlain_do_syscall1(s,a)           VG_(do_syscall)((s),(a),0,0,0,0,0)
#define vgPlain_do_syscall2(s,a,b)         VG_(do_syscall)((s),(a),(b),0,0,0,0)
#define vgPlain_do_syscall3(s,a,b,c)       VG_(do_syscall)((s),(a),(b),(c),0,0,0)
#define vgPlain_do_syscall4(s,a,b,c,d)     VG_(do_syscall)((s),(a),(b),\
                                                           (c),(d),0,0)
#define vgPlain_do_syscall5(s,a,b,c,d,e)   VG_(do_syscall)((s),(a),(b),\
                                                           (c),(d),(e),0)
#define vgPlain_do_syscall6(s,a,b,c,d,e,f) VG_(do_syscall)((s),(a),(b),\
                                                           (c),(d),(e),(f))


/* Build SysRes values -- occasionally useful. */

static inline SysRes VG_(mk_SysRes_Error) ( UWord err ) {
   SysRes r = { err, True };
   return r;
}

static inline SysRes VG_(mk_SysRes_Success) ( UWord err ) {
   SysRes r = { err, False };
   return r;
}


/* This is absolutely the wrong place for these, but I can't figure
   out anywhere else for them to go. */

/* Make a SysRes value from an %eax syscall return value on
   x86-linux.

   From:
   http://sources.redhat.com/cgi-bin/cvsweb.cgi/libc/sysdeps/unix/sysv/
   linux/i386/sysdep.h?
   rev=1.28&content-type=text/x-cvsweb-markup&cvsroot=glibc

   Linux uses a negative return value to indicate syscall errors,
   unlike most Unices, which use the condition codes' carry flag.

   Since version 2.1 the return value of a system call might be
   negative even if the call succeeded.  E.g., the 'lseek' system call
   might return a large offset.  Therefore we must not anymore test
   for < 0, but test for a real error by making sure the value in %eax
   is a real error number.  Linus said he will make sure the no
   syscall returns a value in -1 .. -4095 as a valid result so we can
   safely test with -4095.
*/
static inline SysRes VG_(mk_SysRes_x86_linux) ( Int eax ) {
   SysRes res;
   res.isError = eax >= -4095 && eax <= -1;
   res.val     = res.isError ? -eax : eax;
   return res;
}

/* Similarly .. */
static inline SysRes VG_(mk_SysRes_amd64_linux) ( Long rax ) {
   SysRes res;
   res.isError = rax >= -4095 && rax <= -1;
   res.val     = res.isError ? -rax : rax;
   return res;
}



#endif   // __PUB_CORE_LIBCBASE_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
