
/*--------------------------------------------------------------------*/
/*--- Doing syscalls.                                  m_syscall.c ---*/
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

#include "pub_core_basics.h"
#include "pub_core_syscall.h"

/* ---------------------------------------------------------------------
   Building syscall return values.
   ------------------------------------------------------------------ */

/* Make a SysRes value from an syscall return value.  This is
   Linux-specific.

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
SysRes VG_(mk_SysRes_x86_linux) ( UInt val ) {
   SysRes res;
   res.isError = val >= -4095 && val <= -1;
   res.val     = res.isError ? -val : val;
   return res;
}

/* Similarly .. */
SysRes VG_(mk_SysRes_amd64_linux) ( ULong val ) {
   SysRes res;
   res.isError = val >= -4095 && val <= -1;
   res.val     = res.isError ? -val : val;
   return res;
}

/* PPC uses the CR7.SO bit to flag an error (CR0 in IBM-speke) */
/* Note this must be in the bottom bit of the second arg */
SysRes VG_(mk_SysRes_ppc32_linux) ( UInt val, UInt cr0so ) {
   SysRes res;
   res.isError = (cr0so & 1) != 0;
   res.val     = val;
   return res;
}

/* As per ppc32 version, cr0.so must be in l.s.b. of 2nd arg */
SysRes VG_(mk_SysRes_ppc64_linux) ( ULong val, ULong cr0so ) {
   SysRes res;
   res.isError = (cr0so & 1) != 0;
   res.val     = val;
   return res;
}


SysRes VG_(mk_SysRes_Error) ( UWord val ) {
   SysRes r = { val, True };
   return r;
}

SysRes VG_(mk_SysRes_Success) ( UWord val ) {
   SysRes r = { val, False };
   return r;
}


/* ---------------------------------------------------------------------
   A function for doing syscalls.
   ------------------------------------------------------------------ */

#if defined(VGP_x86_linux)
/* Incoming args (syscall number + up to 6 args) come on the stack.
   (ie. the C calling convention).

   The syscall number goes in %eax.  The args are passed to the syscall in
   the regs %ebx, %ecx, %edx, %esi, %edi, %ebp, ie. the kernel's syscall
   calling convention.

   %eax gets the return value.  Not sure which registers the kernel
   clobbers, so we preserve all the callee-save regs (%esi, %edi, %ebx,
   %ebp).
*/
extern UWord do_syscall_WRK (
          UWord syscall_no, 
          UWord a1, UWord a2, UWord a3,
          UWord a4, UWord a5, UWord a6
       );
asm(
".text\n"
"do_syscall_WRK:\n"
"	push	%esi\n"
"	push	%edi\n"
"	push	%ebx\n"
"	push	%ebp\n"
"	movl	16+ 4(%esp),%eax\n"
"	movl	16+ 8(%esp),%ebx\n"
"	movl	16+12(%esp),%ecx\n"
"	movl	16+16(%esp),%edx\n"
"	movl	16+20(%esp),%esi\n"
"	movl	16+24(%esp),%edi\n"
"	movl	16+28(%esp),%ebp\n"
"	int	$0x80\n"
"	popl	%ebp\n"
"	popl	%ebx\n"
"	popl	%edi\n"
"	popl	%esi\n"
"	ret\n"
".previous\n"
);

#elif defined(VGP_amd64_linux)
/* Incoming args (syscall number + up to 6 args) come in %rdi, %rsi,
   %rdx, %rcx, %r8, %r9, and the last one on the stack (ie. the C
   calling convention).

   The syscall number goes in %rax.  The args are passed to the syscall in
   the regs %rdi, %rsi, %rdx, %r10, %r8, %r9 (yes, really %r10, not %rcx),
   ie. the kernel's syscall calling convention.

   %rax gets the return value.  %rcx and %r11 are clobbered by the syscall;
   no matter, they are caller-save (the syscall clobbers no callee-save
   regs, so we don't have to do any register saving/restoring).
*/
extern UWord do_syscall_WRK (
          UWord syscall_no, 
          UWord a1, UWord a2, UWord a3,
          UWord a4, UWord a5, UWord a6
       );
asm(
".text\n"
"do_syscall_WRK:\n"
        /* Convert function calling convention --> syscall calling
           convention */
"	movq	%rdi, %rax\n"
"	movq	%rsi, %rdi\n"
"	movq	%rdx, %rsi\n"
"	movq	%rcx, %rdx\n"
"	movq	%r8,  %r10\n"
"	movq	%r9,  %r8\n"
"	movq    8(%rsp), %r9\n"	 /* last arg from stack */
"	syscall\n"
"	ret\n"
".previous\n"
);

#elif defined(VGP_ppc32_linux)
/* Incoming args (syscall number + up to 6 args) come in %r3:%r9.

   The syscall number goes in %r0.  The args are passed to the syscall in
   the regs %r3:%r8, i.e. the kernel's syscall calling convention.

   The %cr0.so bit flags an error.
   We return the syscall return value in %r3, and the %cr0.so in 
   the lowest bit of %r4.
   We return a ULong, of which %r3 is the high word, and %r4 the low.
   No callee-save regs are clobbered, so no saving/restoring is needed.
*/
extern ULong do_syscall_WRK (
          UWord syscall_no, 
          UWord a1, UWord a2, UWord a3,
          UWord a4, UWord a5, UWord a6
       );
asm(
".text\n"
"do_syscall_WRK:\n"
"        mr      0,3\n"
"        mr      3,4\n"
"        mr      4,5\n"
"        mr      5,6\n"
"        mr      6,7\n"
"        mr      7,8\n"
"        mr      8,9\n"
"        sc\n"                  /* syscall: sets %cr0.so on error         */
"        mfcr    4\n"           /* %cr -> low word of return var          */
"        rlwinm  4,4,4,31,31\n" /* rotate flag bit so to lsb, and mask it */
"        blr\n"                 /* and return                             */
".previous\n"
);

#elif defined(VGP_ppc64_linux)
/* Due to the need to return 65 bits of result, this is completely
   different from the ppc32 case.  The single arg register points to a
   7-word block containing the syscall # and the 6 args.  The syscall
   result proper is put in [0] of the block, and %cr0.so is in the
   bottom but of [1]. */
extern void do_syscall_WRK ( ULong* argblock );
asm(
".align   2\n"
".globl   do_syscall_WRK\n"
".section \".opd\",\"aw\"\n"
".align   3\n"
"do_syscall_WRK:\n"
".quad    .do_syscall_WRK,.TOC.@tocbase,0\n"
".previous\n"
".type    .do_syscall_WRK,@function\n"
".globl   .do_syscall_WRK\n"
".do_syscall_WRK:\n"
"        std  3,-16(1)\n"  /* stash arg */
"        ld   8, 48(3)\n"  /* sc arg 6 */
"        ld   7, 40(3)\n"  /* sc arg 5 */
"        ld   6, 32(3)\n"  /* sc arg 4 */
"        ld   5, 24(3)\n"  /* sc arg 3 */
"        ld   4, 16(3)\n"  /* sc arg 2 */
"        ld   0,  0(3)\n"  /* sc number */
"        ld   3,  8(3)\n"  /* sc arg 1 */
"        sc\n"             /* result in r3 and cr0.so */
"        ld   5,-16(1)\n"  /* reacquire argblock ptr (r5 is caller-save) */
"        std  3,0(5)\n"    /* argblock[0] = r3 */
"        mfcr 3\n"
"        srwi 3,3,28\n"
"        andi. 3,3,1\n"
"        std  3,8(5)\n"    /* argblock[1] = cr0.s0 & 1 */
"        blr\n"
);
#else
#  error Unknown platform
#endif

SysRes VG_(do_syscall) ( UWord sysno, UWord a1, UWord a2, UWord a3,
                                      UWord a4, UWord a5, UWord a6 )
{
#if defined(VGP_x86_linux)
  UWord val = do_syscall_WRK(sysno,a1,a2,a3,a4,a5,a6);
  return VG_(mk_SysRes_x86_linux)( val );
#elif defined(VGP_amd64_linux)
  UWord val = do_syscall_WRK(sysno,a1,a2,a3,a4,a5,a6);
  return VG_(mk_SysRes_amd64_linux)( val );
#elif defined(VGP_ppc32_linux)
  ULong ret     = do_syscall_WRK(sysno,a1,a2,a3,a4,a5,a6);
  UInt  val     = (UInt)(ret>>32);
  UInt  cr0so   = (UInt)(ret);
  return VG_(mk_SysRes_ppc32_linux)( val, cr0so );
#elif defined(VGP_ppc64_linux)
  ULong argblock[7];
  argblock[0] = sysno;
  argblock[1] = a1;
  argblock[2] = a2;
  argblock[3] = a3;
  argblock[4] = a4;
  argblock[5] = a5;
  argblock[6] = a6;
  do_syscall_WRK( &argblock[0] );
  return VG_(mk_SysRes_ppc64_linux)( argblock[0], argblock[1] );
#else
#  error Unknown platform
#endif
}

/* ---------------------------------------------------------------------
   Names of errors.
   ------------------------------------------------------------------ */

/* Return a string which gives the name of an error value.  Note,
   unlike the standard C syserror fn, the returned string is not
   malloc-allocated or writable -- treat it as a constant. 
   TODO: implement this properly. */

const HChar* VG_(strerror) ( UWord errnum )
{
   switch (errnum) {
      case VKI_EPERM:       return "Operation not permitted";
      case VKI_ENOENT:      return "No such file or directory";
      case VKI_ESRCH:       return "No such process";
      case VKI_EINTR:       return "Interrupted system call";
      case VKI_EBADF:       return "Bad file number";
      case VKI_EAGAIN:      return "Try again";
      case VKI_ENOMEM:      return "Out of memory";
      case VKI_EACCES:      return "Permission denied";
      case VKI_EFAULT:      return "Bad address";
      case VKI_EEXIST:      return "File exists";
      case VKI_EINVAL:      return "Invalid argument";
      case VKI_EMFILE:      return "Too many open files";
      case VKI_ENOSYS:      return "Function not implemented";
      case VKI_ERESTARTSYS: return "ERESTARTSYS";
      default:              return "VG_(strerror): unknown error";
   }
}


/*--------------------------------------------------------------------*/
/*--- end                                                        ---*/
/*--------------------------------------------------------------------*/
