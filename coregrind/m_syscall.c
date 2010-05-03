
/*--------------------------------------------------------------------*/
/*--- Doing syscalls.                                  m_syscall.c ---*/
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

#include "pub_core_basics.h"
#include "pub_core_libcassert.h"
#include "pub_core_vki.h"
#include "pub_core_vkiscnums.h"
#include "pub_core_syscall.h"

/* ---------------------------------------------------------------------
   Building syscall return values.
   ------------------------------------------------------------------ */

#if defined(VGO_linux)

/* Make a SysRes value from a syscall return value.  This is
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

/* As per ppc32 version, cr0.so must be in l.s.b. of 2nd arg */
SysRes VG_(mk_SysRes_ppc64_linux) ( ULong val, ULong cr0so ) {
   SysRes res;
   res._isError = (cr0so & 1) != 0;
   res._val     = val;
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

/* Generic constructors. */
SysRes VG_(mk_SysRes_Error) ( UWord err ) {
   SysRes r;
   r._isError = True;
   r._val     = err;
   return r;
}

SysRes VG_(mk_SysRes_Success) ( UWord res ) {
   SysRes r;
   r._isError = False;
   r._val     = res;
   return r;
}


#elif defined(VGO_aix5)

/* AIX scheme: we have to record both 'res' (r3) and 'err' (r4).  If
   'err' is nonzero then the call has failed, but it could still be
   that AIX userspace will ignore 'err' and instead consult 'res' to
   determine if the call failed.  So we have to record both. */
SysRes VG_(mk_SysRes_ppc32_aix5) ( UInt res, UInt err ) {
   SysRes r;
   r.res     = res;
   r.err     = err;
   r.isError = r.err != 0;
   return r;
}

SysRes VG_(mk_SysRes_ppc64_aix5) ( ULong res, ULong err ) {
   SysRes r;
   r.res     = res;
   r.err     = err;
   r.isError = r.err != 0;
   return r;
}

/* Generic constructors. */
SysRes VG_(mk_SysRes_Error) ( UWord err ) {
   SysRes r;
   r._res     = 0;
   r._err     = err;
   r._isError = True;
   return r;
}

SysRes VG_(mk_SysRes_Success) ( UWord res ) {
   SysRes r;
   r._res     = res;
   r._err     = 0;
   r._isError = False;
   return r;
}


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

#elif defined(VGP_ppc32_aix5)
static void do_syscall_WRK ( UWord* res_r3, UWord* res_r4,
                             UWord sysno, 
                             UWord a1, UWord a2, UWord a3,
                             UWord a4, UWord a5, UWord a6,
                             UWord a7, UWord a8 )
{
   /* Syscalls on AIX are very similar to function calls:
      - up to 8 args in r3-r10
      - syscall number in r2
      - kernel resumes at 'lr', so must set it appropriately beforehand
      - r3 holds the result and r4 any applicable error code
      See http://www.cs.utexas.edu/users/cart/publications/tr00-04.ps
      and also 'man truss'.
   */
   /* For some reason gcc-3.3.2 doesn't preserve r31 across the asm
      even though we state it to be trashed.  So use r27 instead. */
   UWord args[9];
   args[0] = sysno;
   args[1] = a1; args[2] = a2;
   args[3] = a3; args[4] = a4;
   args[5] = a5; args[6] = a6;
   args[7] = a7; args[8] = a8;

   __asm__ __volatile__(

      // establish base ptr
      "mr   28,%0\n\t"

      // save r2, lr
      "mr   27,2\n\t" // save r2 in r27
      "mflr 30\n\t"   // save lr in r30

      // set syscall number and args
      "lwz   2,  0(28)\n\t"
      "lwz   3,  4(28)\n\t"
      "lwz   4,  8(28)\n\t"
      "lwz   5, 12(28)\n\t"
      "lwz   6, 16(28)\n\t"
      "lwz   7, 20(28)\n\t"
      "lwz   8, 24(28)\n\t"
      "lwz   9, 28(28)\n\t"
      "lwz  10, 32(28)\n\t"

      // set bit 3 of CR1 otherwise AIX 5.1 returns to the
      // wrong address after the sc instruction
      "crorc 6,6,6\n\t"

      // set up LR to point just after the sc insn
      ".long 0x48000005\n\t" // "bl here+4" -- lr := & next insn
      "mflr 29\n\t"
      "addi 29,29,16\n\t"
      "mtlr 29\n\t"

      // do it!
      "sc\n\t"

      // result is now in r3; save it in args[0]
      "stw  3,0(28)\n\t"
      // error code in r4; save it in args[1]
      "stw  4,4(28)\n\t"

      // restore
      "mr   2,27\n\t"
      "mtlr 30\n\t"

      : /*out*/
      : /*in*/  "b" (&args[0])
      : /*trash*/
           /*temps*/    "r31","r30","r29","r28","r27",
           /*args*/     "r3","r4","r5","r6","r7","r8","r9","r10",
           /*paranoia*/ "memory","cc","r0","r1","r11","r12","r13",
                        "xer","ctr","cr0","cr1","cr2","cr3",
                        "cr4","cr5","cr6","cr7"
   );

   *res_r3 = args[0];
   *res_r4 = args[1];
}

#elif defined(VGP_ppc64_aix5)
static void do_syscall_WRK ( UWord* res_r3, UWord* res_r4,
                             UWord sysno, 
                             UWord a1, UWord a2, UWord a3,
                             UWord a4, UWord a5, UWord a6,
                             UWord a7, UWord a8 )
{
   /* Same scheme as ppc32-aix5. */
   UWord args[9];
   args[0] = sysno;
   args[1] = a1; args[2] = a2;
   args[3] = a3; args[4] = a4;
   args[5] = a5; args[6] = a6;
   args[7] = a7; args[8] = a8;

   __asm__ __volatile__(

      // establish base ptr
      "mr   28,%0\n\t"

      // save r2, lr
      "mr   27,2\n\t" // save r2 in r27
      "mflr 30\n\t"   // save lr in r30

      // set syscall number and args
      "ld    2,  0(28)\n\t"
      "ld    3,  8(28)\n\t"
      "ld    4, 16(28)\n\t"
      "ld    5, 24(28)\n\t"
      "ld    6, 32(28)\n\t"
      "ld    7, 40(28)\n\t"
      "ld    8, 48(28)\n\t"
      "ld    9, 56(28)\n\t"
      "ld   10, 64(28)\n\t"

      // set bit 3 of CR1 otherwise AIX 5.1 returns to the
      // wrong address after the sc instruction
      "crorc 6,6,6\n\t"

      // set up LR to point just after the sc insn
      ".long 0x48000005\n\t" // "bl here+4" -- lr := & next insn
      "mflr 29\n\t"
      "addi 29,29,16\n\t"
      "mtlr 29\n\t"

      // do it!
      "sc\n\t"

      // result is now in r3; save it in args[0]
      "std  3,0(28)\n\t"
      // error code in r4; save it in args[1]
      "std  4,8(28)\n\t"

      // restore
      "mr   2,27\n\t"
      "mtlr 30\n\t"

      : /*out*/
      : /*in*/  "b" (&args[0])
      : /*trash*/
           /*temps*/    "r31","r30","r29","r28","r27",
           /*args*/     "r3","r4","r5","r6","r7","r8","r9","r10",
           /*paranoia*/ "memory","cc","r0","r1","r11","r12","r13",
                        "xer","ctr","cr0","cr1","cr2","cr3",
                        "cr4","cr5","cr6","cr7"
   );

   *res_r3 = args[0];
   *res_r4 = args[1];
}

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

#else
#  error Unknown platform
#endif


/* Finally, the generic code.  This sends the call to the right
   helper. */

SysRes VG_(do_syscall) ( UWord sysno, UWord a1, UWord a2, UWord a3,
                                      UWord a4, UWord a5, UWord a6,
                                      UWord a7, UWord a8 )
{
#  if defined(VGP_x86_linux)
   UWord val = do_syscall_WRK(sysno,a1,a2,a3,a4,a5,a6);
   return VG_(mk_SysRes_x86_linux)( val );

#  elif defined(VGP_amd64_linux)
   UWord val = do_syscall_WRK(sysno,a1,a2,a3,a4,a5,a6);
   return VG_(mk_SysRes_amd64_linux)( val );

#  elif defined(VGP_ppc32_linux)
   ULong ret     = do_syscall_WRK(sysno,a1,a2,a3,a4,a5,a6);
   UInt  val     = (UInt)(ret>>32);
   UInt  cr0so   = (UInt)(ret);
   return VG_(mk_SysRes_ppc32_linux)( val, cr0so );

#  elif defined(VGP_ppc64_linux)
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

#  elif defined(VGP_arm_linux)
   UWord val = do_syscall_WRK(a1,a2,a3,a4,a5,a6,sysno);
   return VG_(mk_SysRes_arm_linux)( val );

#  elif defined(VGP_ppc32_aix5)
   UWord res;
   UWord err;
   do_syscall_WRK( &res, &err, 
		   sysno, a1, a2, a3, a4, a5, a6, a7, a8);
   /* Try to set the error number to zero if the syscall hasn't
      really failed. */
   if (sysno == __NR_AIX5_kread
       || sysno == __NR_AIX5_kwrite) {
      if (res != (UWord)-1L)
         err = 0;
   }
   else if (sysno == __NR_AIX5_sigprocmask
            || sysno == __NR_AIX5__sigpending) {
      if (res == 0)
         err = 0;
   }
   return VG_(mk_SysRes_ppc32_aix5)( res, err );

#  elif defined(VGP_ppc64_aix5)
   UWord res;
   UWord err;
   do_syscall_WRK( &res, &err, 
		   sysno, a1, a2, a3, a4, a5, a6, a7, a8);
   /* Try to set the error number to zero if the syscall hasn't
      really failed. */
   if (sysno == __NR_AIX5_kread
       || sysno == __NR_AIX5_kwrite) {
      if (res != (UWord)-1L)
         err = 0;
   }
   else if (sysno == __NR_AIX5_sigprocmask
            || sysno == __NR_AIX5__sigpending) {
      if (res == 0)
         err = 0;
   }
   return VG_(mk_SysRes_ppc64_aix5)( res, err );

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
      case VKI_EOVERFLOW:   return "Value too large for defined data type";
#     if defined(VKI_ERESTARTSYS)
      case VKI_ERESTARTSYS: return "ERESTARTSYS";
#     endif
      default:              return "VG_(strerror): unknown error";
   }
}


/*--------------------------------------------------------------------*/
/*--- end                                                        ---*/
/*--------------------------------------------------------------------*/
