
/*--------------------------------------------------------------------*/
/*--- Doing syscalls.                                  m_syscall.c ---*/
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

#include "pub_core_basics.h"
#include "pub_core_libcassert.h"
#include "pub_core_vki.h"
#include "pub_core_vkiscnums.h"
#include "pub_core_syscall.h"

/* ---------------------------------------------------------------------
   Building syscall return values.
   ------------------------------------------------------------------ */

/* Make a SysRes value from a syscall return value.  This is
   platform specific. */

#if defined(VGP_mips32_linux) || defined(VGP_mips64_linux)

SysRes VG_(mk_SysRes_mips32_linux) ( UWord v0, UWord v1, UWord a3 ) {
   /* MIPS uses a3 != 0 to flag an error */
   SysRes res;
   res._isError = (a3 != (UWord)0);
   res._val     = v0;
   res._valEx   = v1;
   return res;
}

SysRes VG_(mk_SysRes_mips64_linux) ( ULong v0, ULong v1, ULong a3 ) {
   /* MIPS uses a3 != 0 to flag an error */
   SysRes res;
   res._isError = (a3 != (ULong)0);
   res._val     = v0;
   res._valEx   = v1;
   return res;
}

/* Generic constructors. */
SysRes VG_(mk_SysRes_Error) ( UWord err ) {
   SysRes r;
   r._isError = True;
   r._val     = err;
   r._valEx   = 0;
   return r;
}

SysRes VG_(mk_SysRes_Success) ( UWord res ) {
   SysRes r;
   r._isError = False;
   r._val     = res;
   r._valEx   = 0;
   return r;
}

SysRes VG_(mk_SysRes_SuccessEx) ( UWord res, UWord resEx ) {
   SysRes r;
   r._isError = False;
   r._val     = res;
   r._valEx   = resEx;
   return r;
}


#elif defined(VGO_linux) \
      && !defined(VGP_mips32_linux) && !defined(VGP_mips64_linux)

/*
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

SysRes VG_(mk_SysRes_nanomips_linux) ( UWord a0 ) {
   SysRes res;
   res._isError = (a0 > 0xFFFFF000ul);
   res._val = a0;
   return res;
}

SysRes VG_(mk_SysRes_x86_linux) ( Int val ) {
   SysRes res;
   res._isError = val >= -4095 && val <= -1;
   if (res._isError) {
      res._val = (UInt)(-val);
   } else {
      res._val = (UInt)val;
   }
   return res;
}

/* Similarly .. */
SysRes VG_(mk_SysRes_amd64_linux) ( Long val ) {
   SysRes res;
   res._isError = val >= -4095 && val <= -1;
   if (res._isError) {
      res._val = (ULong)(-val);
   } else {
      res._val = (ULong)val;
   }
   return res;
}

/* PPC uses the CR7.SO bit to flag an error (CR0 in IBM-speak) */
/* Note this must be in the bottom bit of the second arg */
SysRes VG_(mk_SysRes_ppc32_linux) ( UInt val, UInt cr0so ) {
   SysRes res;
   res._isError = (cr0so & 1) != 0;
   res._val     = val;
   return res;
}

/* As per ppc32 version, for the sc instruction cr0.so must be in
   l.s.b. of 2nd arg.
   For the scv 0 instruction, the return value indicates failure if
   it is -4095..-1 (i.e., it is >= -MAX_ERRNO (-4095) as an unsigned
   comparison), in which case the error value is the negated return value. */
SysRes VG_(mk_SysRes_ppc64_linux) ( ULong val, ULong cr0so, UInt flag ) {
   SysRes res;

   if (flag == SC_FLAG) {
      /* sc instruction */
      res._isError = (cr0so & 1) != 0;
      res._val     = val;
   } else if (flag == SCV_FLAG) {
      /* scv instruction */
      if ( (Long)val >= -4095 && (Long)val <= -1) {
         res._isError = True;
         res._val = (ULong)(-val);
      } else {
         res._isError = False;
         res._val = (ULong)(val);
      }
   } else
      vg_assert(0);
   return res;
}

SysRes VG_(mk_SysRes_s390x_linux) ( Long val ) {
   SysRes res;
   res._isError = val >= -4095 && val <= -1;
   if (res._isError) {
      res._val = -val;
   } else {
      res._val = val;
   }
   return res;
}

SysRes VG_(mk_SysRes_arm_linux) ( Int val ) {
   SysRes res;
   res._isError = val >= -4095 && val <= -1;
   if (res._isError) {
      res._val = (UInt)(-val);
   } else {
      res._val = (UInt)val;
   }
   return res;
}

SysRes VG_(mk_SysRes_arm64_linux) ( Long val ) {
   SysRes res;
   res._isError = val >= -4095 && val <= -1;
   if (res._isError) {
      res._val = (ULong)(-val);
   } else {
      res._val = (ULong)val;
   }
   return res;
}

/* Generic constructors. */
SysRes VG_(mk_SysRes_Success) ( UWord res ) {
   SysRes r;
   r._isError = False;
   r._val     = res;
   return r;
}

#if defined(VGP_nanomips_linux)
SysRes VG_(mk_SysRes_Error) ( UWord err ) {
   SysRes r;
   r._isError = True;
   r._val     = (UWord)(-(Word)err);
   return r;
}
#else
SysRes VG_(mk_SysRes_Error) ( UWord err ) {
   SysRes r;
   r._isError = True;
   r._val     = err;
   return r;
}

#endif


#elif defined(VGO_darwin)

/* Darwin: Some syscalls return a double-word result. */
SysRes VG_(mk_SysRes_x86_darwin) ( UChar scclass, Bool isErr,
                                   UInt wHI, UInt wLO )
{
   SysRes res;
   res._wHI  = 0;
   res._wLO  = 0;
   res._mode = 0; /* invalid */
   vg_assert(isErr == False || isErr == True);
   vg_assert(sizeof(UWord) == sizeof(UInt));
   switch (scclass) {
      case VG_DARWIN_SYSCALL_CLASS_UNIX:
         res._wLO  = wLO;
         res._wHI  = wHI;
         res._mode = isErr ? SysRes_UNIX_ERR : SysRes_UNIX_OK;
         break;
      case VG_DARWIN_SYSCALL_CLASS_MACH:
         vg_assert(!isErr);
         vg_assert(wHI == 0);
         res._wLO  = wLO;
         res._mode = SysRes_MACH;
         break;
      case VG_DARWIN_SYSCALL_CLASS_MDEP:
         vg_assert(!isErr);
         vg_assert(wHI == 0);
         res._wLO  = wLO;
         res._mode = SysRes_MDEP;
         break;
      default:
         vg_assert(0);
   }
   return res;
}

SysRes VG_(mk_SysRes_amd64_darwin) ( UChar scclass, Bool isErr,
                                     ULong wHI, ULong wLO )
{
   SysRes res;
   res._wHI  = 0;
   res._wLO  = 0;
   res._mode = 0; /* invalid */
   vg_assert(isErr == False || isErr == True);
   vg_assert(sizeof(UWord) == sizeof(ULong));
   switch (scclass) {
      case VG_DARWIN_SYSCALL_CLASS_UNIX:
         res._wLO  = wLO;
         res._wHI  = wHI;
         res._mode = isErr ? SysRes_UNIX_ERR : SysRes_UNIX_OK;
         break;
      case VG_DARWIN_SYSCALL_CLASS_MACH:
         vg_assert(!isErr);
         vg_assert(wHI == 0);
         res._wLO  = wLO;
         res._mode = SysRes_MACH;
         break;
      case VG_DARWIN_SYSCALL_CLASS_MDEP:
         vg_assert(!isErr);
         vg_assert(wHI == 0);
         res._wLO  = wLO;
         res._mode = SysRes_MDEP;
         break;
      default:
         vg_assert(0);
   }
   return res;
}

/* Generic constructors.  We assume (without checking if this makes
   any sense, from the caller's point of view) that these are for the
   UNIX style of syscall. */
SysRes VG_(mk_SysRes_Error) ( UWord err ) {
   SysRes r;
   r._wHI  = 0;
   r._wLO  = err;
   r._mode = SysRes_UNIX_ERR;
   return r;
}

SysRes VG_(mk_SysRes_Success) ( UWord res ) {
   SysRes r;
   r._wHI  = 0;
   r._wLO  = res;
   r._mode = SysRes_UNIX_OK;
   return r;
}


#elif defined(VGO_solaris)

/* Generic constructors. */
SysRes VG_(mk_SysRes_Error) ( UWord err ) {
   SysRes r;
   r._val     = err;
   r._val2    = 0;
   r._isError = True;
   return r;
}

SysRes VG_(mk_SysRes_Success) ( UWord res ) {
   SysRes r;
   r._val     = res;
   r._val2    = 0;
   r._isError = False;
   return r;
}

SysRes VG_(mk_SysRes_x86_solaris) ( Bool isErr, UInt val, UInt val2 )
{
   SysRes res;

   // stay sane
   vg_assert(isErr == True || isErr == False);

   res._val  = val;
   res._val2 = val2;
   res._isError = isErr;
   return res;
}

SysRes VG_(mk_SysRes_amd64_solaris) ( Bool isErr, ULong val, ULong val2 )
{
   SysRes res;

   // stay sane
   vg_assert(isErr == True || isErr == False);

   res._val  = val;
   res._val2 = val2;
   res._isError = isErr;
   return res;
}


#elif defined(VGO_freebsd)

SysRes VG_(mk_SysRes_x86_freebsd) ( UInt val, UInt val2, Bool err ) {
   SysRes r;
   r._isError = err;
   r._val = val;
   r._val2 = val2;
   return r;
}

SysRes VG_(mk_SysRes_amd64_freebsd) ( ULong val, ULong val2, Bool err ) {
   SysRes r;
   r._isError = err;
   r._val = val;
   r._val2 = val2;
   return r;
}

/* Generic constructors. */
SysRes VG_(mk_SysRes_Error) ( UWord err ) {
   SysRes r;
   r._val     = err;
   r._val2    = 0;
   r._isError = True;
   return r;
}

SysRes VG_(mk_SysRes_Success) ( UWord res ) {
   SysRes r;
   r._val     = res;
   r._val2    = 0;
   r._isError = False;
   return r;
}

#else
#  error "Unknown OS"
#endif


/* ---------------------------------------------------------------------
   VG_(do_syscall): A function for doing syscalls.
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
".globl do_syscall_WRK\n"
"do_syscall_WRK:\n"
"	.cfi_startproc\n"
"	push	%esi\n"
"	.cfi_adjust_cfa_offset 4\n"
"	.cfi_offset %esi, -8\n"
"	push	%edi\n"
"	.cfi_adjust_cfa_offset 4\n"
"	.cfi_offset %edi, -12\n"
"	push	%ebx\n"
"	.cfi_adjust_cfa_offset 4\n"
"	.cfi_offset %ebx, -16\n"
"	push	%ebp\n"
"	.cfi_adjust_cfa_offset 4\n"
"	.cfi_offset %ebp, -20\n"
"	movl	16+ 4(%esp),%eax\n"
"	movl	16+ 8(%esp),%ebx\n"
"	movl	16+12(%esp),%ecx\n"
"	movl	16+16(%esp),%edx\n"
"	movl	16+20(%esp),%esi\n"
"	movl	16+24(%esp),%edi\n"
"	movl	16+28(%esp),%ebp\n"
"	int	$0x80\n"
"	popl	%ebp\n"
"	.cfi_adjust_cfa_offset -4\n"
"	.cfi_restore %ebp\n"
"	popl	%ebx\n"
"	.cfi_adjust_cfa_offset -4\n"
"	.cfi_restore %ebx\n"
"	popl	%edi\n"
"	.cfi_adjust_cfa_offset -4\n"
"	.cfi_restore %edi\n"
"	popl	%esi\n"
"	.cfi_adjust_cfa_offset -4\n"
"	.cfi_restore %esi\n"
"	ret\n"
"	.cfi_endproc\n"
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
".globl do_syscall_WRK\n"
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
".globl do_syscall_WRK\n"
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

#elif defined(VGP_ppc64be_linux)
/* Due to the need to return 65 bits of result, this is completely
   different from the ppc32 case.  The single arg register points to a
   7-word block containing the syscall # and the 6 args.  The syscall
   result proper is put in [0] of the block, and %cr0.so is in the
   bottom bit of [1]. */
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

#elif defined(VGP_ppc64le_linux)
/* Due to the need to return 65 bits of result, this is completely
   different from the ppc32 case.  The single arg register points to a
   7-word block containing the syscall # and the 6 args.  The syscall
   result proper is put in [0] of the block, and %cr0.so is in the
   bottom bit of [1]. */
extern void do_syscall_WRK ( ULong* argblock );
/* Little Endian supports ELF version 2.  In the future, it may support
 * other versions as well.
 */
asm(
".align   2\n"
".globl   do_syscall_WRK\n"
".type    do_syscall_WRK,@function\n"
"do_syscall_WRK:\n"
"#if  _CALL_ELF == 2"               "\n"
"0:      addis        2,12,.TOC.-0b@ha\n"
"        addi         2,2,.TOC.-0b@l\n"
"        .localentry do_syscall_WRK, .-do_syscall_WRK\n"
"#endif"                            "\n"
/* Check which system call instruction to issue*/
"        ld   8, 56(3)\n"  /* arg 7 holds sc/scv flag */
"        cmpdi 8,1\n"      /* check sc/scv flag not equal to SC_FLAG*/
"        bne  issue_scv\n"

/* setup and issue the sc instruction */
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
"        blr\n"            /* return */

/*  setup to do scv instruction */
"issue_scv: "
/* The scv instruction requires a new stack frame */
"        stdu    1,-80(1)\n"
"        std     27,40(1)\n" /* save r27 to stack frame */
"        mflr    27\n"       /* Get link register */
"        std     27,16(1)\n" /* Save link register */

/* setup and issue the scv instruction */
"        std  3,-16(1)\n"  /* stash arg */
"        ld   8, 48(3)\n"  /* sc arg 6 */
"        ld   7, 40(3)\n"  /* sc arg 5 */
"        ld   6, 32(3)\n"  /* sc arg 4 */
"        ld   5, 24(3)\n"  /* sc arg 3 */
"        ld   4, 16(3)\n"  /* sc arg 2 */
"        ld   0,  0(3)\n"  /* sc number */
"        ld   3,  8(3)\n"  /* sc arg 1 */

"        .machine push\n"
"        .machine \"power9\"\n"
"        scv  0\n"
"        .machine pop\n"
"        ld   5,-16(1)\n"  /* reacquire argblock ptr (r5 is caller-save) */
"        std  3,0(5)\n"    /* argblock[0] = r3 */

/* pop off stack frame */
"        ld      27,16(1)\n"        /* Fetch LR from frame */
"        mtlr    27\n"              /* restore LR */
"        ld      27,40(1)\n"        /* restore r27 from stack frame */
"        addi    1,1,80\n"
"        blr\n"
"        .size do_syscall_WRK, .-do_syscall_WRK\n"
);

#elif defined(VGP_arm_linux)
/* I think the conventions are:
   args  in r0 r1 r2 r3 r4 r5
   sysno in r7
   return value in r0, w/ same conventions as x86-linux, viz r0 in
   -4096 .. -1 is an error value.  All other values are success
   values.
*/
extern UWord do_syscall_WRK (
          UWord a1, UWord a2, UWord a3,
          UWord a4, UWord a5, UWord a6,
          UWord syscall_no
       );
asm(
".text\n"
".globl do_syscall_WRK\n"
"do_syscall_WRK:\n"
"         push    {r4, r5, r7}\n"
"         ldr     r4, [sp, #12]\n"
"         ldr     r5, [sp, #16]\n"
"         ldr     r7, [sp, #20]\n"
"         svc     0x0\n"
"         pop     {r4, r5, r7}\n"
"         bx      lr\n"
".previous\n"
);

#elif defined(VGP_arm64_linux)
/* I think the conventions are:
   args  in r0 r1 r2 r3 r4 r5
   sysno in r8
   return value in r0, w/ same conventions as x86-linux, viz r0 in
   -4096 .. -1 is an error value.  All other values are success
   values.

   r0 to r5 remain unchanged, but syscall_no is in r6 and needs 
   to be moved to r8 (??)
*/
extern UWord do_syscall_WRK (
          UWord a1, UWord a2, UWord a3,
          UWord a4, UWord a5, UWord a6,
          UWord syscall_no
       );
asm(
".text\n"
".globl do_syscall_WRK\n"
"do_syscall_WRK:\n"
"        mov x8, x6\n"
"        mov x6, 0\n"
"        mov x7, 0\n"
"        svc 0\n"
"        ret\n"
".previous\n"
);

#elif defined(VGP_x86_freebsd)
/* Incoming args (syscall number + up to 8 args) are on the stack.
   FreeBSD has a syscall called 'syscall' that takes all args (including
   the syscall number) off the stack.  Since we're called, the return
   address is on the stack as expected, so we can just call syscall(2)
   and it Just Works.  Error is when carry is set.
*/
extern ULong do_syscall_WRK (
          UWord syscall_no, 
          UWord a1, UWord a2, UWord a3,
          UWord a4, UWord a5, UWord a6,
          UWord a7, UWord a8, UInt *flags
       );
asm(
".text\n"
"do_syscall_WRK:\n"
"      movl    $0,%eax\n"      /* syscall number = "syscall" (0) to avoid stack frobbing
*/
"      int     $0x80\n"
"      jb      1f\n"
"      ret\n"
"1:    movl    40(%esp),%ecx\n"        /* store carry in *flags */
"      movl    $1,(%ecx)\n"
"      ret\n"
".previous\n"
);

#elif defined(VGP_amd64_freebsd)
/* Convert function calling convention --> SYSCALL_STD calling
   convention
   PJF - not sure why we don't use SYSCALL0 convention like x86
 */
extern UWord do_syscall_WRK (
          UWord syscall_no,    /* %rdi */
          UWord a1,            /* %rsi */
          UWord a2,            /* %rdx */
          UWord a3,            /* %rcx */
          UWord a4,            /* %r8 */
          UWord a5,            /* %r9 */
          UWord a6,            /* 8(%rsp) */
          UWord a7,            /* 16(%rsp) */
          UWord a8,            /* 24(%rsp) */
          UInt *flags,         /* 32(%rsp) */
          UWord *rv2           /* 40(%rsp) */
       );
asm(
".text\n"
"do_syscall_WRK:\n"
"      pushq   %rbp\n"
"      movq    %rsp, %rbp\n"
"      movq    %rdi, %rax\n"    /* syscall_no */
"      movq    %rsi, %rdi\n"    /* a1 */
"      movq    %rdx, %rsi\n"    /* a2 */
"      movq    %rcx, %rdx\n"    /* a3 */
"      movq    %r8,  %r10\n"    /* a4 */
"      movq    %r9,  %r8\n"     /* a5 */
"      movq    16(%rbp), %r9\n" /* a6 last arg from stack, account for %rbp */
"      movq    24(%rbp), %r11\n" /* a7 from stack */
"      pushq  %r11\n"
"      movq    32(%rbp), %r11\n" /* a8 from stack */
"      pushq  %r11\n"
"      subq    $8,%rsp\n"       /* fake return addr */
"      syscall\n"
"      jb      1f\n"
"      movq    48(%rbp),%rsi\n" /* success */
"      movq    %rdx, (%rsi)\n"  /* second return value */
"      movq    %rbp, %rsp\n"
"      popq    %rbp\n"
"      ret\n"
"1:\n"                          /* error path */
"      movq    40(%rbp), %rsi\n" /* flags */
"      movl    $1,(%rsi)\n"
"      movq    %rbp, %rsp\n"
"      popq    %rbp\n"
"      ret\n"
".previous\n"
);

#elif defined(VGP_x86_darwin)

/* Incoming args (syscall number + up to 8 args) come in on the stack

   The kernel's syscall calling convention is:
   * the syscall number goes in eax
   * the args are passed to the syscall on the stack,
     pushed onto the stack R->L (that is, the usual x86
     calling conventions, with the leftmost arg at the lowest
     address)
   Call instruction:
   * UNIX: sysenter
   * UNIX: int $0x80
   * MACH: int $0x81
   * MDEP: int $0x82
   Note that the call type can be determined from the syscall number;
   there is no need to inspect the actual instruction.  Although obviously
   the instruction must match.
   Return value:
   * MACH,MDEP: the return value comes back in eax
   * UNIX: the return value comes back in edx:eax (hi32:lo32)
   Error:
   * MACH,MDEP: no error is returned
   * UNIX: the carry flag indicates success or failure

   nb here, sizeof(UWord) == sizeof(UInt)
*/

__private_extern__ ULong 
do_syscall_unix_WRK ( UWord a1, UWord a2, UWord a3, /* 4(esp)..12(esp) */
                      UWord a4, UWord a5, UWord a6, /* 16(esp)..24(esp) */
                      UWord a7, UWord a8, /* 28(esp)..32(esp) */
                      UWord syscall_no, /* 36(esp) */
                      /*OUT*/UInt* errflag /* 40(esp) */ );
// Unix syscall: 64-bit return in edx:eax, with LSB in eax
// error indicated by carry flag: clear=good, set=bad
asm(".private_extern _do_syscall_unix_WRK\n"
    "_do_syscall_unix_WRK:\n"
    "        movl    40(%esp), %ecx   \n"  /* assume syscall success */
    "        movl    $0, (%ecx)       \n"
    "        movl    36(%esp), %eax   \n"
    "        int     $0x80            \n"
    "        jnc     1f               \n"  /* jump if success */
    "        movl    40(%esp), %ecx   \n"  /* syscall failed - set *errflag */
    "        movl    $1, (%ecx)       \n"
    "    1:  ret                      \n"
    );

__private_extern__ UInt 
do_syscall_mach_WRK ( UWord a1, UWord a2, UWord a3, /* 4(esp)..12(esp) */
                      UWord a4, UWord a5, UWord a6, /* 16(esp)..24(esp) */
                      UWord a7, UWord a8, /* 28(esp)..32(esp) */
                      UWord syscall_no /* 36(esp) */ );
// Mach trap: 32-bit result in %eax, no error flag
asm(".private_extern _do_syscall_mach_WRK\n"
    "_do_syscall_mach_WRK:\n"
    "        movl    36(%esp), %eax   \n"
    "        int     $0x81            \n"
    "        ret                      \n"
    );

__private_extern__ UInt 
do_syscall_mdep_WRK ( UWord a1, UWord a2, UWord a3, /* 4(esp)..12(esp) */
                      UWord a4, UWord a5, UWord a6, /* 16(esp)..24(esp) */
                      UWord a7, UWord a8, /* 28(esp)..32(esp) */
                      UWord syscall_no /* 36(esp) */ );
// mdep trap: 32-bit result in %eax, no error flag
asm(
    ".private_extern _do_syscall_mdep_WRK\n"
    "_do_syscall_mdep_WRK:\n"
    "        movl    36(%esp), %eax   \n"
    "        int     $0x82            \n"
    "        ret                      \n"
    );


#elif defined(VGP_amd64_darwin)

/* Incoming args (syscall number + up to 8 args) come in registers and stack

   The kernel's syscall calling convention is:
   * the syscall number goes in rax
   * the args are passed to the syscall in registers and the stack
   * the call instruction is 'syscall'
   Return value:
   * MACH,MDEP: the return value comes back in rax
   * UNIX: the return value comes back in rdx:rax (hi64:lo64)
   Error:
   * MACH,MDEP: no error is returned
   * UNIX: the carry flag indicates success or failure

   nb here, sizeof(UWord) == sizeof(ULong)
*/

__private_extern__ UWord 
do_syscall_unix_WRK ( UWord a1, UWord a2, UWord a3, /* rdi, rsi, rdx */
                      UWord a4, UWord a5, UWord a6, /* rcx, r8,  r9 */
                      UWord a7, UWord a8,           /* 8(rsp), 16(rsp) */
                      UWord syscall_no,             /* 24(rsp) */
                      /*OUT*/ULong* errflag,        /* 32(rsp) */
                      /*OUT*/ULong* res2 );         /* 40(rsp) */
// Unix syscall: 128-bit return in rax:rdx, with LSB in rax
// error indicated by carry flag: clear=good, set=bad
asm(".private_extern _do_syscall_unix_WRK\n"
    "_do_syscall_unix_WRK:\n"
    "        movq    %rcx, %r10       \n"  /* pass rcx in r10 instead */
    "        movq    32(%rsp), %rax   \n"  /* assume syscall success */
    "        movq    $0, (%rax)       \n"
    "        movq    24(%rsp), %rax   \n"  /* load syscall_no */
    "        syscall                  \n"
    "        jnc     1f               \n"  /* jump if success */
    "        movq    32(%rsp), %rcx   \n"  /* syscall failed - set *errflag */
    "        movq    $1, (%rcx)       \n"
    "    1:  movq    40(%rsp), %rcx   \n"  /* save 2nd result word */
    "        movq    %rdx, (%rcx)     \n"
    "        retq                     \n"  /* return 1st result word */
    );

__private_extern__ UWord 
do_syscall_mach_WRK ( UWord a1, UWord a2, UWord a3, /* rdi, rsi, rdx */
                      UWord a4, UWord a5, UWord a6, /* rcx, r8,  r9 */
                      UWord a7, UWord a8,           /* 8(rsp), 16(rsp) */
                      UWord syscall_no );           /* 24(rsp) */
// Mach trap: 64-bit result, no error flag
asm(".private_extern _do_syscall_mach_WRK\n"
    "_do_syscall_mach_WRK:\n"
    "        movq    %rcx, %r10       \n"  /* pass rcx in r10 instead */
    "        movq    24(%rsp), %rax   \n"  /* load syscall_no */
    "        syscall                  \n"
    "        retq                     \n"
    );

#elif defined(VGP_s390x_linux)

static UWord do_syscall_WRK (
   UWord syscall_no,
   UWord arg1, UWord arg2, UWord arg3,
   UWord arg4, UWord arg5, UWord arg6
   )
{
   register UWord __arg1 asm("2") = arg1;
   register UWord __arg2 asm("3") = arg2;
   register UWord __arg3 asm("4") = arg3;
   register UWord __arg4 asm("5") = arg4;
   register UWord __arg5 asm("6") = arg5;
   register UWord __arg6 asm("7") = arg6;
   register ULong __svcres asm("2");

   __asm__ __volatile__ (
                 "lgr %%r1,%1\n\t"
                 "svc 0\n\t"
		: "=d" (__svcres)
		: "a" (syscall_no),
		  "0" (__arg1),
		  "d" (__arg2),
		  "d" (__arg3),
		  "d" (__arg4),
		  "d" (__arg5),
		  "d" (__arg6)
		: "1", "cc", "memory");

   return (UWord) (__svcres);
}

#elif defined(VGP_mips32_linux)
/* Incoming args (syscall number + up to 6 args) come in a0 - a3 and stack.

   The syscall number goes in v0.  The args are passed to the syscall in
   the regs a0 - a3 and stack, i.e. the kernel's syscall calling convention.

   (a3 != 0) flags an error.
   We return the syscall return value in v0.
   MIPS version
*/
extern int do_syscall_WRK (
          int a1, int a2, int a3,
          int a4, int a5, int a6, int a7, int syscall_no, UWord *err,
          UWord *valHi, UWord* valLo
       );
asm (
   ".text                                  \n\t"
   ".globl do_syscall_WRK                  \n\t"
   ".type  do_syscall_WRK, @function       \n\t"
   ".set push                              \n\t"
   ".set noreorder                         \n\t"
   "do_syscall_WRK:                        \n\t"
   "	lw $2, 28($29)                       \n\t"
   "	syscall                              \n\t"
   "	lw $8, 32($29)                       \n\t"
   "	sw $7, ($8)                          \n\t"
   "	lw $8, 36($29)                       \n\t"
   "	sw $3, ($8)                          \n\t" /* store valHi */
   "	lw $8, 40($29)                       \n\t"
   "	jr $31                               \n\t"
   "	sw $2, ($8)                          \n\t" /* store valLo */
   ".size do_syscall_WRK, .-do_syscall_WRK \n\t"
   ".set pop                               \n\t"
   ".previous                              \n\t"
);

#elif defined(VGP_mips64_linux)
extern RegWord do_syscall_WRK ( RegWord a1, RegWord a2, RegWord a3, RegWord a4,
                                RegWord a5, RegWord a6, RegWord a7, RegWord syscall_no,
                                RegWord* V1_A3_val );
asm (
   ".text                                  \n\t"
   ".globl do_syscall_WRK                  \n\t"
   ".type  do_syscall_WRK, @function       \n\t"
   ".set push                              \n\t"
   ".set noreorder                         \n\t"
   "do_syscall_WRK:                        \n\t"
   "   move $2, $11                        \n\t"
   "   syscall                             \n\t"
#  if defined(_ABI64)
   "   ld $12, 0($29)                      \n\t"
#  elif defined(_ABIN32)
   "   lw $12, 0($29)                      \n\t"
#  endif
   "   sd $3, 0($12)                       \n\t" /* store v1 in V1_A3_val */
   "   jr $31                              \n\t"
   "   sd $7, 8($12)                       \n\t" /* store a3 in V1_A3_val */
   ".size do_syscall_WRK, .-do_syscall_WRK \n\t"
   ".set pop                               \n\t"
   ".previous                              \n\t"
);

#elif defined(VGP_nanomips_linux)
extern void do_syscall_WRK (
         RegWord a1, RegWord a2, RegWord a3,
         RegWord a4, RegWord a5, RegWord a6,
         RegWord syscall_no, RegWord *res_a0);
asm (
   ".text                                  \n\t"
   ".globl do_syscall_WRK                  \n\t"
   ".type  do_syscall_WRK, @function       \n\t"
   ".set push                              \n\t"
   ".set noreorder                         \n\t"
   "do_syscall_WRK:                        \n\t"
   "   save 32, $a7                        \n\t"
   "   move $t4, $a6                       \n\t"
   "   syscall[32]                         \n\t"
   "   restore 32, $a7                     \n\t"
   "   sw $a0, 0($a7)                      \n\t"
   "   jrc $ra                             \n\t"
   ".size do_syscall_WRK, .-do_syscall_WRK \n\t"
   ".set pop                               \n\t"
   ".previous                              \n\t"
);

#elif defined(VGP_x86_solaris)

extern ULong
do_syscall_WRK(UWord a1, UWord a2, UWord a3,    /* 4(esp)..12(esp) */
               UWord a4, UWord a5, UWord a6,    /* 16(esp)..24(esp) */
               UWord a7, UWord a8,              /* 28(esp)..32(esp) */
               UWord syscall_no,                /* 36(esp) */
               /*OUT*/UInt *errflag);           /* 40(esp) */
/* Classic unix syscall.. parameters on the stack, an unused (by the kernel)
   return address at 0(esp), a sysno in eax, a result in edx:eax, the carry
   flag set on error. */
__asm__ (
".text\n"
".globl do_syscall_WRK\n"
"do_syscall_WRK:\n"
"       movl    40(%esp), %ecx\n"       /* assume syscall success */
"       movl    $0, (%ecx)\n"
"       movl    36(%esp), %eax\n"
"       int     $0x91\n"
"       jnc     1f\n"                   /* jump if success */
"       movl    40(%esp), %ecx\n"       /* syscall failed - set *errflag */
"       movl    $1, (%ecx)\n"
"1:     ret\n"
".previous\n"
);

extern ULong
do_syscall_fast_WRK(UWord syscall_no);          /* 4(esp) */
/* Fasttrap syscall.. no parameters, a sysno in eax, a result in edx:eax,
   never fails (if the sysno is valid). */
__asm__ (
".text\n"
".globl do_syscall_fast_WRK\n"
"do_syscall_fast_WRK:\n"
"       movl    4(%esp), %eax\n"
"       int     $0xD2\n"
"       ret\n"
".previous\n"
);

#elif defined(VGP_amd64_solaris)

extern ULong
do_syscall_WRK(UWord a1, UWord a2, UWord a3,    /* rdi, rsi, rdx */
               UWord a4, UWord a5, UWord a6,    /* rcx, r8, r9 */
               UWord a7, UWord a8,              /* 8(rsp), 16(rsp) */
               UWord syscall_no,                /* 24(rsp) */
               /*OUT*/ULong *errflag,           /* 32(rsp) */
               /*OUT*/ULong *res2);             /* 40(rsp) */
/* First 6 parameters in registers rdi, rsi, rdx, r10, r8, r9, next
   2 parameters on the stack, an unused (by the kernel) return address at
   0(rsp), a sysno in rax, a result in rdx:rax, the carry flag set on
   error. */
__asm__ (
".text\n"
".globl do_syscall_WRK\n"
"do_syscall_WRK:\n"
"       movq    %rcx, %r10\n"           /* pass rcx in r10 instead */
"       movq    32(%rsp), %rcx\n"       /* assume syscall success */
"       movq    $0, (%rcx)\n"
"       movq    24(%rsp), %rax\n"
"       syscall\n"
"       jnc     1f\n"                   /* jump if success */
"       movq    32(%rsp), %rcx\n"       /* syscall failed - set *errflag */
"       movq    $1, (%rcx)\n"
"1:     movq    40(%rsp), %rcx\n"       /* save 2nd result word */
"       movq    %rdx, (%rcx)\n"
"       ret\n"
".previous\n"
);

extern ULong
do_syscall_fast_WRK(UWord syscall_no,           /* rdi */
                    /*OUT*/ULong *res2);        /* rsi */
/* Fasttrap syscall.. no parameters, a sysno in rax, a result in rdx:rax,
   never fails (if the sysno is valid). */
__asm__ (
".text\n"
".globl do_syscall_fast_WRK\n"
"do_syscall_fast_WRK:\n"
"       movq    %rdi, %rax\n"
"       int     $0xD2\n"
"       movq    %rdx, (%rsi)\n"         /* save 2nd result word */
"       ret\n"
".previous\n"
);

#else
#  error Unknown platform
#endif


/* Finally, the generic code.  This sends the call to the right
   helper. */

SysRes VG_(do_syscall) ( UWord sysno, RegWord a1, RegWord a2, RegWord a3,
                                      RegWord a4, RegWord a5, RegWord a6,
                                      RegWord a7, RegWord a8 )
{
#  if defined(VGP_x86_linux)
   UWord val = do_syscall_WRK(sysno,a1,a2,a3,a4,a5,a6);
   return VG_(mk_SysRes_x86_linux)( val );

#  elif defined(VGP_amd64_linux)
   UWord val = do_syscall_WRK(sysno,a1,a2,a3,a4,a5,a6);
   return VG_(mk_SysRes_amd64_linux)( val );

#  elif defined(VGP_x86_freebsd)
   ULong val;
   UInt err = 0;
   val = do_syscall_WRK(sysno, a1, a2, a3, a4, a5,
                        a6, a7, a8, &err);
   return VG_(mk_SysRes_x86_freebsd)( (UInt)val, (UInt)(val>>32), (err & 1) != 0 ? True : False);

#  elif defined(VGP_amd64_freebsd)
   UWord val;
   UWord val2 = 0;
   UInt err = 0;
   val = do_syscall_WRK(sysno, a1, a2, a3, a4, a5,
                        a6, a7, a8, &err, &val2);
   return VG_(mk_SysRes_amd64_freebsd)( val, val2, (err & 1) != 0 ? True : False);

#  elif defined(VGP_ppc32_linux)
   ULong ret     = do_syscall_WRK(sysno,a1,a2,a3,a4,a5,a6);
   UInt  val     = (UInt)(ret>>32);
   UInt  cr0so   = (UInt)(ret);
   return VG_(mk_SysRes_ppc32_linux)( val, cr0so );

#  elif defined(VGP_ppc64be_linux) || defined(VGP_ppc64le_linux)
   ULong argblock[8];
   /* PPC system calls have at most 6 arguments.  The Valgrind infrastructure
      supports 8 system call arguments.  Argument 7 is used on PPC LE to pass
      the flag indicating if the sc or scv instruction should be used for the
      system call.  */
   argblock[0] = sysno;
   argblock[1] = a1;
   argblock[2] = a2;
   argblock[3] = a3;
   argblock[4] = a4;
   argblock[5] = a5;
   argblock[6] = a6;
   argblock[7] = a7;
   do_syscall_WRK( &argblock[0] );
   return VG_(mk_SysRes_ppc64_linux)( argblock[0], argblock[1], a7 );

#  elif defined(VGP_arm_linux)
   UWord val = do_syscall_WRK(a1,a2,a3,a4,a5,a6,sysno);
   return VG_(mk_SysRes_arm_linux)( val );

#  elif defined(VGP_arm64_linux)
   UWord val = do_syscall_WRK(a1,a2,a3,a4,a5,a6,sysno);
   return VG_(mk_SysRes_arm64_linux)( val );

#  elif defined(VGP_x86_darwin)
   UInt  wLO = 0, wHI = 0, err = 0;
   ULong u64;
   UChar scclass = VG_DARWIN_SYSNO_CLASS(sysno);
   switch (scclass) {
      case VG_DARWIN_SYSCALL_CLASS_UNIX:
         u64 = do_syscall_unix_WRK(a1,a2,a3,a4,a5,a6,a7,a8,
                                   VG_DARWIN_SYSNO_FOR_KERNEL(sysno), &err);
         wLO = (UInt)u64;
         wHI = (UInt)(u64 >> 32);
         break;
      case VG_DARWIN_SYSCALL_CLASS_MACH:
         wLO = do_syscall_mach_WRK(a1,a2,a3,a4,a5,a6,a7,a8, 
                                   VG_DARWIN_SYSNO_FOR_KERNEL(sysno));
         err = 0;
         break;
      case VG_DARWIN_SYSCALL_CLASS_MDEP:
         wLO = do_syscall_mdep_WRK(a1,a2,a3,a4,a5,a6,a7,a8, 
                                   VG_DARWIN_SYSNO_FOR_KERNEL(sysno));
         err = 0;
         break;
      default:
         vg_assert(0);
         break;
   }
   return VG_(mk_SysRes_x86_darwin)( scclass, err ? True : False, wHI, wLO );

#  elif defined(VGP_amd64_darwin)
   ULong wLO = 0, wHI = 0, err = 0;
   UChar scclass = VG_DARWIN_SYSNO_CLASS(sysno);
   switch (scclass) {
      case VG_DARWIN_SYSCALL_CLASS_UNIX:
         wLO = do_syscall_unix_WRK(a1,a2,a3,a4,a5,a6,a7,a8,
                                   VG_DARWIN_SYSNO_FOR_KERNEL(sysno), &err, &wHI);
         break;
      case VG_DARWIN_SYSCALL_CLASS_MACH:
      case VG_DARWIN_SYSCALL_CLASS_MDEP:
         wLO = do_syscall_mach_WRK(a1,a2,a3,a4,a5,a6,a7,a8, 
                                   VG_DARWIN_SYSNO_FOR_KERNEL(sysno));
         err = 0;
         break;
      default:
         vg_assert(0);
         break;
   }
   return VG_(mk_SysRes_amd64_darwin)( scclass, err ? True : False, wHI, wLO );
  
#elif defined(VGP_s390x_linux)
   UWord val;

   if (sysno == __NR_mmap) {
     ULong argbuf[6];

     argbuf[0] = a1;
     argbuf[1] = a2;
     argbuf[2] = a3;
     argbuf[3] = a4;
     argbuf[4] = a5;
     argbuf[5] = a6;
     val = do_syscall_WRK(sysno,(UWord)&argbuf[0],0,0,0,0,0);
   } else {
     val = do_syscall_WRK(sysno,a1,a2,a3,a4,a5,a6);
   }

   return VG_(mk_SysRes_s390x_linux)( val );

#elif defined(VGP_mips32_linux)
   UWord err   = 0;
   UWord valHi = 0;
   UWord valLo = 0;
   (void) do_syscall_WRK(a1, a2, a3, a4, a5, a6, a7, sysno, &err, &valHi, &valLo);
   return VG_(mk_SysRes_mips32_linux)( valLo, valHi, (ULong)err );

#elif defined(VGP_mips64_linux)
   RegWord v1_a3[2];
   v1_a3[0] = 0xFF00;
   v1_a3[1] = 0xFF00;
   RegWord V0 = do_syscall_WRK(a1, a2, a3, a4, a5, a6, a7, sysno, v1_a3);
   RegWord V1 = (RegWord)v1_a3[0];
   RegWord A3 = (RegWord)v1_a3[1];
   return VG_(mk_SysRes_mips64_linux)( V0, V1, A3 );

#elif defined(VGP_nanomips_linux)
   RegWord reg_a0 = 0;
   do_syscall_WRK(a1, a2, a3, a4, a5, a6, sysno, &reg_a0);
   return VG_(mk_SysRes_nanomips_linux)(reg_a0);

#  elif defined(VGP_x86_solaris)
   UInt val, val2, err = False;
   Bool restart;
   ULong u64;
   UChar ssclass = VG_SOLARIS_SYSNO_CLASS(sysno);

   switch (ssclass) {
      case VG_SOLARIS_SYSCALL_CLASS_CLASSIC:
         /* The Solaris kernel does not restart syscalls automatically so it
            is done here. */
         do {
            u64 = do_syscall_WRK(a1,a2,a3,a4,a5,a6,a7,a8,
                                 VG_SOLARIS_SYSNO_INDEX(sysno), &err);
            val = (UInt)u64;
            restart = err && (val == VKI_EINTR || val == VKI_ERESTART);
         } while (restart);
         break;
      case VG_SOLARIS_SYSCALL_CLASS_FASTTRAP:
         u64 = do_syscall_fast_WRK(VG_SOLARIS_SYSNO_INDEX(sysno));
         break;
      default:
         vg_assert(0);
         break;
   }

   val = (UInt)u64;
   val2 = (UInt)(u64 >> 32);
   return VG_(mk_SysRes_x86_solaris)(err ? True : False, val,
                                     err ? 0 : val2);

#  elif defined(VGP_amd64_solaris)
   ULong val, val2, err = False;
   Bool restart;
   UChar ssclass = VG_SOLARIS_SYSNO_CLASS(sysno);

   switch (ssclass) {
      case VG_SOLARIS_SYSCALL_CLASS_CLASSIC:
         /* The Solaris kernel does not restart syscalls automatically so it
            is done here. */
         do {
            val = do_syscall_WRK(a1,a2,a3,a4,a5,a6,a7,a8,
                                 VG_SOLARIS_SYSNO_INDEX(sysno), &err, &val2);
            restart = err && (val == VKI_EINTR || val == VKI_ERESTART);
         } while (restart);
         break;
      case VG_SOLARIS_SYSCALL_CLASS_FASTTRAP:
         val = do_syscall_fast_WRK(VG_SOLARIS_SYSNO_INDEX(sysno), &val2);
         break;
      default:
         vg_assert(0);
         break;
   }

   return VG_(mk_SysRes_amd64_solaris)(err ? True : False, val,
                                       err ? 0 : val2);

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
   case VKI_EIO:         return "Input/output error";
   case VKI_ENXIO:       return "No such device or address";
   case VKI_E2BIG:       return "Argument list too long";
   case VKI_ENOEXEC:     return "Exec format error";
   case VKI_EBADF:       return "Bad file descriptor";
   case VKI_ECHILD:      return "No child processes";
   case VKI_EAGAIN:      return "Resource temporarily unavailable";
   case VKI_ENOMEM:      return "Cannot allocate memory";
   case VKI_EACCES:      return "Permission denied";
   case VKI_EFAULT:      return "Bad address";
   case VKI_ENOTBLK:     return "Block device required";
   case VKI_EBUSY:       return "Device or resource busy";
   case VKI_EEXIST:      return "File exists";
   case VKI_EXDEV:       return "Invalid cross-device link";
   case VKI_ENODEV:      return "No such device";
   case VKI_ENOTDIR:     return "Not a directory";
   case VKI_EISDIR:      return "Is a directory";
   case VKI_EINVAL:      return "Invalid argument";
   case VKI_ENFILE:      return "Too many open files in system";
   case VKI_EMFILE:      return "Too many open files";
   case VKI_ENOTTY:      return "Inappropriate ioctl for device";
   case VKI_ETXTBSY:     return "Text file busy";
   case VKI_EFBIG:       return "File too large";
   case VKI_ENOSPC:      return "No space left on device";
   case VKI_ESPIPE:      return "Illegal seek";
   case VKI_EROFS:       return "Read-only file system";
   case VKI_EMLINK:      return "Too many links";
   case VKI_EPIPE:       return "Broken pipe";
   case VKI_EDOM:        return "Numerical argument out of domain";
   case VKI_ERANGE:      return "Numerical result out of range";

   case VKI_ENOSYS:      return "Function not implemented";
   case VKI_EOVERFLOW:   return "Value too large for defined data type";
#     if defined(VKI_ERESTARTSYS)
      case VKI_ERESTARTSYS: return "ERESTARTSYS";
#     endif
#     if defined(VKI_ERESTART)
      case VKI_ERESTART: return "ERESTART";
#     endif
   default:              return "VG_(strerror): unknown error";
   }
}


/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
