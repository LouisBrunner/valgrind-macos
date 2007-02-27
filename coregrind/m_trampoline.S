
/*--------------------------------------------------------------------*/
/*--- Trampoline code page stuff.                   m_trampoline.S ---*/
/*--------------------------------------------------------------------*/

/*
  This file is part of Valgrind, a dynamic binary instrumentation
  framework.

  Copyright (C) 2000-2007 Julian Seward 
     jseward@acm.org
  Copyright (C) 2006-2007 OpenWorks LLP
     info@open-works.co.uk
	
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

#include "pub_core_basics_asm.h"

/* We need pub_core_vkiscnums.h, but the AIX5 formulation
   brings in a load of C declarations.  Defining this macro
   makes them invisible.  Yes, a nasty hack. */
#define VG_IN_ASSEMBLY_SOURCE
#  include "pub_core_vkiscnums.h"
#undef VG_IN_ASSEMBLY_SOURCE

/* ------------------ SIMULATED CPU HELPERS ------------------ */
/* 
   Replacements for some functions to do with vsyscalls and signals.
   This code runs on the simulated CPU.
*/
	
/*---------------------- x86-linux ----------------------*/
#if defined(VGP_x86_linux)

#	define UD2_16     ud2 ; ud2 ; ud2 ; ud2 ;ud2 ; ud2 ; ud2 ; ud2
#	define UD2_64     UD2_16   ; UD2_16   ; UD2_16   ; UD2_16
#	define UD2_256    UD2_64   ; UD2_64   ; UD2_64   ; UD2_64
#	define UD2_1024   UD2_256  ; UD2_256  ; UD2_256  ; UD2_256
#	define UD2_PAGE   UD2_1024 ; UD2_1024 ; UD2_1024 ; UD2_1024  

	/* a leading page of unexecutable code */
	UD2_PAGE

.global VG_(trampoline_stuff_start)
VG_(trampoline_stuff_start):

.global VG_(x86_linux_SUBST_FOR_sigreturn)
VG_(x86_linux_SUBST_FOR_sigreturn):
        /* This is a very specific sequence which GDB uses to
           recognize signal handler frames.  Also gcc: see
           x86_fallback_frame_state() in
           gcc-4.1.0/gcc/config/i386/linux-unwind.h */
        popl    %eax
        movl    $__NR_sigreturn, %eax
        int     $0x80
        ud2

.global VG_(x86_linux_SUBST_FOR_rt_sigreturn)
VG_(x86_linux_SUBST_FOR_rt_sigreturn):
        /* Likewise for rt signal frames */
        movl    $__NR_rt_sigreturn, %eax
        int     $0x80
        ud2

/* There's no particular reason that this needs to be handwritten
   assembly, but since that's what this file contains, here's a
   simple index implementation (written in C and compiled by gcc.)

   unsigned char* REDIR_FOR_index ( const char* s, int c ) 
   { 
      unsigned char  ch   = (unsigned char)((unsigned int)c); 
      unsigned char* p    = (unsigned char*)s; 
      unsigned char* last = 0; 
      while (1) { 
         if (*p == ch) last = p; 
         if (*p == 0) return last; 
         p++; 
      } 
   }
*/
.global VG_(x86_linux_REDIR_FOR_index)
.type   VG_(x86_linux_REDIR_FOR_index), @function
VG_(x86_linux_REDIR_FOR_index):
        pushl   %ebp
        movl    %esp, %ebp
        pushl   %ebx
        movb    12(%ebp), %al
        movl    8(%ebp), %ecx
        movl    $0, %ebx
.L2:
        movb    (%ecx), %dl
        cmpb    %dl, %al
        jne     .L3
        movl    %ecx, %ebx
.L3:
        testb   %dl, %dl
        je      .L8
        addl    $1, %ecx
        jmp     .L2
.L8:
        movl    %ebx, %eax
        popl    %ebx
        popl    %ebp
        ret
.size VG_(x86_linux_REDIR_FOR_index), .-VG_(x86_linux_REDIR_FOR_index)

.global VG_(trampoline_stuff_end)
VG_(trampoline_stuff_end):

	/* and a trailing page of unexecutable code */
	UD2_PAGE

#	undef UD2_16
#	undef UD2_64
#	undef UD2_256
#	undef UD2_1024
#	undef UD2_PAGE
	
/*---------------------- amd64-linux ----------------------*/
#else
#if defined(VGP_amd64_linux)

#	define UD2_16     ud2 ; ud2 ; ud2 ; ud2 ;ud2 ; ud2 ; ud2 ; ud2
#	define UD2_64     UD2_16   ; UD2_16   ; UD2_16   ; UD2_16
#	define UD2_256    UD2_64   ; UD2_64   ; UD2_64   ; UD2_64
#	define UD2_1024   UD2_256  ; UD2_256  ; UD2_256  ; UD2_256
#	define UD2_PAGE   UD2_1024 ; UD2_1024 ; UD2_1024 ; UD2_1024  

	/* a leading page of unexecutable code */
	UD2_PAGE

.global VG_(trampoline_stuff_start)
VG_(trampoline_stuff_start):

.global VG_(amd64_linux_SUBST_FOR_rt_sigreturn)
VG_(amd64_linux_SUBST_FOR_rt_sigreturn):
        /* This is a very specific sequence which GDB uses to
           recognize signal handler frames. */
        movq    $__NR_rt_sigreturn, %rax
        syscall
        ud2

.global VG_(amd64_linux_REDIR_FOR_vgettimeofday)
.type   VG_(amd64_linux_REDIR_FOR_vgettimeofday), @function
VG_(amd64_linux_REDIR_FOR_vgettimeofday):
.LfnB2:
        movq    $__NR_gettimeofday, %rax
        syscall
        ret
.LfnE2:
.size VG_(amd64_linux_REDIR_FOR_vgettimeofday), .-LfnB2
	
.global VG_(amd64_linux_REDIR_FOR_vtime)
.type   VG_(amd64_linux_REDIR_FOR_vtime), @function
VG_(amd64_linux_REDIR_FOR_vtime):
.LfnB3:	
        movq    $__NR_time, %rax
        syscall
        ret
.LfnE3:
.size VG_(amd64_linux_REDIR_FOR_vtime), .-LfnB3

/* A CIE for the above two functions, followed by their FDEs */
	.section .eh_frame,"a",@progbits
.Lframe1:
        .long   .LEcie1-.LScie1
.LScie1:
        .long   0x0
        .byte   0x1
        .string "zR"
        .uleb128 0x1
        .sleb128 -8
        .byte   0x10
        .uleb128 0x1
        .byte   0x3
        .byte   0xc
        .uleb128 0x7
        .uleb128 0x8
        .byte   0x90
        .uleb128 0x1
        .align 8
.LEcie1:
.LSfde2:
        .long   .LEfde2-.LASfde2
.LASfde2:
        .long   .LASfde2-.Lframe1
        .long   .LfnB2
        .long   .LfnE2-.LfnB2
        .uleb128 0x0
        .align 8
.LEfde2:
.LSfde3:
        .long   .LEfde3-.LASfde3
.LASfde3:
        .long   .LASfde3-.Lframe1
        .long   .LfnB3
        .long   .LfnE3-.LfnB3
        .uleb128 0x0
        .align 8
.LEfde3:
	.previous

.global VG_(trampoline_stuff_end)
VG_(trampoline_stuff_end):

	/* and a trailing page of unexecutable code */
	UD2_PAGE

#	undef UD2_16
#	undef UD2_64
#	undef UD2_256
#	undef UD2_1024
#	undef UD2_PAGE

/*---------------- ppc32-linux ----------------*/
#else
#if defined(VGP_ppc32_linux)

#	define UD2_16     trap ; trap ; trap; trap
#	define UD2_64     UD2_16   ; UD2_16   ; UD2_16   ; UD2_16
#	define UD2_256    UD2_64   ; UD2_64   ; UD2_64   ; UD2_64
#	define UD2_1024   UD2_256  ; UD2_256  ; UD2_256  ; UD2_256
#	define UD2_PAGE   UD2_1024 ; UD2_1024 ; UD2_1024 ; UD2_1024  

	/* a leading page of unexecutable code */
	UD2_PAGE

.global VG_(trampoline_stuff_start)
VG_(trampoline_stuff_start):

/* There's no particular reason that this needs to be handwritten
   assembly, but since that's what this file contains, here's a
   simple strlen implementation (written in C and compiled by gcc.)
*/
.global VG_(ppc32_linux_REDIR_FOR_strlen)
.type   VG_(ppc32_linux_REDIR_FOR_strlen), @function
VG_(ppc32_linux_REDIR_FOR_strlen):
        lbz 4,0(3)
        li 9,0
        cmpwi 0,4,0
        beq- 0,.L18
.L19:
        lbzu 5,1(3)
        addi 9,9,1
        cmpwi 0,5,0
        bne+ 0,.L19
.L18:
        mr 3,9
        blr
.size VG_(ppc32_linux_REDIR_FOR_strlen), .-VG_(ppc32_linux_REDIR_FOR_strlen)

/* Ditto strcmp */
.global VG_(ppc32_linux_REDIR_FOR_strcmp)
.type   VG_(ppc32_linux_REDIR_FOR_strcmp), @function
VG_(ppc32_linux_REDIR_FOR_strcmp):
.L20:
        lbz 0,0(3)
        cmpwi 7,0,0
        bne- 7,.L21
        lbz 0,0(4)
        li 11,0
        cmpwi 7,0,0
        beq- 7,.L22
.L21:
        lbz 0,0(3)
        li 11,-1
        cmpwi 7,0,0
        beq- 7,.L22
        lbz 0,0(4)
        li 11,1
        cmpwi 7,0,0
        beq- 7,.L22
        lbz 9,0(3)
        lbz 0,0(4)
        li 11,-1
        cmplw 7,9,0
        blt- 7,.L22
        lbz 9,0(3)
        lbz 0,0(4)
        li 11,1
        addi 3,3,1
        addi 4,4,1
        cmplw 7,9,0
        ble+ 7,.L20
.L22:
        mr 3,11
        blr
.size VG_(ppc32_linux_REDIR_FOR_strcmp), .-VG_(ppc32_linux_REDIR_FOR_strcmp)

/* Ditto index/strchr */
.global VG_(ppc32_linux_REDIR_FOR_strchr)
.type   VG_(ppc32_linux_REDIR_FOR_strchr), @function
VG_(ppc32_linux_REDIR_FOR_strchr):
        lbz 0,0(3)
        cmpw 7,4,0
        beqlr 7
        cmpwi 7,0,0
        beq 7,.L307
        .p2align 4,,15
.L310:
        lbzu 0,1(3)
        cmpw 7,0,4
        cmpwi 6,0,0
        beq 7,.L313
        bne 6,.L310
.L307:
        li 3,0
        blr
.L313:
        blr
.size   VG_(ppc32_linux_REDIR_FOR_strchr),.-VG_(ppc32_linux_REDIR_FOR_strchr)
	
.global VG_(trampoline_stuff_end)
VG_(trampoline_stuff_end):

	/* and a trailing page of unexecutable code */
	UD2_PAGE

#	undef UD2_16
#	undef UD2_64
#	undef UD2_256
#	undef UD2_1024
#	undef UD2_PAGE

/*---------------- ppc64-linux ----------------*/
#else
#if defined(VGP_ppc64_linux)

#	define UD2_16     trap ; trap ; trap; trap
#	define UD2_64     UD2_16   ; UD2_16   ; UD2_16   ; UD2_16
#	define UD2_256    UD2_64   ; UD2_64   ; UD2_64   ; UD2_64
#	define UD2_1024   UD2_256  ; UD2_256  ; UD2_256  ; UD2_256
#	define UD2_PAGE   UD2_1024 ; UD2_1024 ; UD2_1024 ; UD2_1024  

	/* a leading page of unexecutable code */
	UD2_PAGE

.global VG_(trampoline_stuff_start)
VG_(trampoline_stuff_start):

	/* See comment in pub_core_trampoline.h for what this is for */
.global VG_(ppctoc_magic_redirect_return_stub)
VG_(ppctoc_magic_redirect_return_stub):
	trap

	/* this function is written using the "dotless" ABI convention */
	.align 2
	.globl VG_(ppc64_linux_REDIR_FOR_strlen)
	.section        ".opd","aw"
	.align 3
VG_(ppc64_linux_REDIR_FOR_strlen):
	.quad   .L.VG_(ppc64_linux_REDIR_FOR_strlen),.TOC.@tocbase,0
	.previous
	.size	VG_(ppc64_linux_REDIR_FOR_strlen), \
			.L0end-.L.VG_(ppc64_linux_REDIR_FOR_strlen)
	.type	VG_(ppc64_linux_REDIR_FOR_strlen), @function

.L.VG_(ppc64_linux_REDIR_FOR_strlen):
        mr 9,3
        lbz 0,0(3)
        li 3,0
        cmpwi 7,0,0
        beqlr 7
        li 3,0
.L01:
        addi 0,3,1
        extsw 3,0
        lbzx 0,9,3
        cmpwi 7,0,0
        bne 7,.L01
        blr
        .long 0
        .byte 0,0,0,0,0,0,0,0
.L0end:

        /* this function is written using the "dotless" ABI convention */
        .align 2
        .globl VG_(ppc64_linux_REDIR_FOR_strchr)
	.section        ".opd","aw"
	.align 3
VG_(ppc64_linux_REDIR_FOR_strchr):
        .quad   .L.VG_(ppc64_linux_REDIR_FOR_strchr),.TOC.@tocbase,0
        .previous
        .size   VG_(ppc64_linux_REDIR_FOR_strchr), \
                        .L1end-.L.VG_(ppc64_linux_REDIR_FOR_strchr)
        .type   VG_(ppc64_linux_REDIR_FOR_strchr),@function
	
.L.VG_(ppc64_linux_REDIR_FOR_strchr):
        rldicl 4,4,0,56
        li 9,0
.L11:
        lbz 0,0(3)
        cmpw 7,0,4
        bne+ 7,.L12
        mr 9,3
.L12:
        lbz 0,0(3)
        addi 3,3,1
        cmpwi 7,0,0
        bne+ 7,.L11
        mr 3,9
        blr
        .long 0
        .byte 0,0,0,0,0,0,0,0
.L1end:

	
.global VG_(trampoline_stuff_end)
VG_(trampoline_stuff_end):

	/* and a trailing page of unexecutable code */
	UD2_PAGE

#	undef UD2_16
#	undef UD2_64
#	undef UD2_256
#	undef UD2_1024
#	undef UD2_PAGE

/*---------------- ppc32-aix5 ----------------*/
#else
#if defined(VGP_ppc32_aix5)

#	define UD2_16     trap ; trap ; trap; trap
#	define UD2_64     UD2_16   ; UD2_16   ; UD2_16   ; UD2_16
#	define UD2_256    UD2_64   ; UD2_64   ; UD2_64   ; UD2_64
#	define UD2_1024   UD2_256  ; UD2_256  ; UD2_256  ; UD2_256
#	define UD2_PAGE   UD2_1024 ; UD2_1024 ; UD2_1024 ; UD2_1024  

	.csect .text[PR]

	/* a leading page of unexecutable code */
	UD2_PAGE

.globl VG_(trampoline_stuff_start)
VG_(trampoline_stuff_start):

/* See pub_core_trampoline.h for an explaination of this.  Also
   see pub_core_initimg.h, struct AIX5PreloadPage.  On entry, r3
   points to an AIX5PreloadPage structure.  Note we can only 
   use r2-r10 as scratch registers here since those are the
   only ones restored from the preload page when finally
   starting the client. */
.globl VG_(ppc32_aix5_do_preloads_then_start_client)
VG_(ppc32_aix5_do_preloads_then_start_client):
	stwu	1,-1024(1)
	stw	3,512(1)	/* stash r3 512 bytes up stack */

	/* Try to load .../vgpreload_core.so */
	lwz	2,0(3)		/* r2 = __NR___loadx */
	lwz	5,20(3)		/* r5 = off_preloadcorename */
	add	6,3,5		/* r6 = preloadcorename */
	addis	1,1,-4
	bl	do___loadx
	addis	1,1,4
	cmpwi	0,3,0
	beq	.Lfailed

	/* Try to load .../vgpreload_tool.so, if it exists */
	lwz	3,512(1)	/* restore r3 */
	lwz	2,0(3)		/* r2 = __NR___loadx */
	lwz	5,24(3)		/* r5 = off_preloadtoolname */
	cmpwi	0,5,0		/* skip tool preload if */
	beq	.Ltry_preload	/* name not present */
	add	6,3,5		/* r6 = preloadtoolname */
	addis	1,1,-4
	bl	do___loadx
	addis	1,1,4
	cmpwi	0,3,0
	beq	.Lfailed

.Ltry_preload:
	/* Try to load the LD_PRELOAD= file, if it exists */
	lwz	3,512(1)	/* restore r3 */
	lwz	2,0(3)		/* r2 = __NR___loadx */
	lwz	5,28(3)		/* r5 = off_ld_preloadname */
	cmpwi	0,5,0		/* skip ld_preload if */
	beq	.Lstart_client	/* name not present */
	add	6,3,5		/* r6 = ld_preloadname */
	addis	1,1,-4
	bl	do___loadx
	addis	1,1,4
	cmpwi	0,3,0
	beq	.Lfailed
	
.Lstart_client:
	/* Success.  Restore r2-r10 from preloadpage-> and start
	the client. */
	lwz	3,512(1)	/* restore r3 */
	addi	1,1,1024
	lwz	2,32+4(3)	/* preloadpage->client_start */
	mtctr	2
	lwz	2,40+4(3)	/* preloadpage->r2 */
	lwz	4,56+4(3)	/* preloadpage->r4 */
	lwz	5,64+4(3)	/* preloadpage->r5 */
	lwz	6,72+4(3)	/* preloadpage->r6 */
	lwz	7,80+4(3)	/* preloadpage->r7 */
	lwz	8,88+4(3)	/* preloadpage->r8 */
	lwz	9,96+4(3)	/* preloadpage->r9 */
	lwz	10,104+4(3)	/* preloadpage->r10 */
	lwz	3,48+4(3)	/* preloadpage->r3 */
	bctr
	/*NOTREACHED*/
	trap

.Lfailed:
	/* __loadx barfed for some reason.  Print the error
	message and get out. */
	/* First the error msg */
	lwz	3,512(1)	/* restore r3 */
	lwz	2,4(3)		/* r2 = __NR_kwrite */
	lwz	4,12(3)		/* r4 = offset of err msg */
	add	4,4,3		/* r4 = err msg */
	lwz	5,16(3)		/* r5 = length err msg */
	li	3,2		/* r3 = stderr */
	bl	do_syscall
	/* now call the diagnosis fn */
	lwz	3,512(1)	/* restore r3 */
	lwz	4,112(3)	/* preloadpage->p_diagnose_load_failure */
	lwz	2,4(4)		/* get its TOC ptr */
	lwz	4,0(4)		/* get its entry point */
	mtlr	4
	blrl
	/* Now do _exit(1) */
	lwz	3,512(1)	/* restore r3 */
	lwz	2,8(3)		/* r2 = __NR_exit */
	li	3,1		/* doing _exit(1) */
	addi	1,1,1024	/* fix stack pointer */
	bl	do_syscall
	/*NOTREACHED*/
	trap
	
do___loadx:
	/* On entry: r2 = __NR___loadx,	r6 = name of module */
	li	3,1
	slwi	3,3,24	/* r3 = 0x1000000 = VKI_DL_LOAD */
	mr	4,1
	lis	5,3
	li	7,0
	li	8,0
	li	9,0
	li	10,0
do_syscall:
	crorc	6,6,6
	sc
	trap
	/* sc continues at 'lr', hence this 
	constitutes an automatic return */


	/* See comment in pub_core_trampoline.h for what this is for */
.globl VG_(ppctoc_magic_redirect_return_stub)
VG_(ppctoc_magic_redirect_return_stub):
	trap
	
.globl VG_(trampoline_stuff_end)
VG_(trampoline_stuff_end):

	/* and a trailing page of unexecutable code */
	UD2_PAGE

#	undef UD2_16
#	undef UD2_64
#	undef UD2_256
#	undef UD2_1024
#	undef UD2_PAGE

/*---------------- ppc64-aix5 ----------------*/
#else
#if defined(VGP_ppc64_aix5)

#	define UD2_16     trap ; trap ; trap; trap
#	define UD2_64     UD2_16   ; UD2_16   ; UD2_16   ; UD2_16
#	define UD2_256    UD2_64   ; UD2_64   ; UD2_64   ; UD2_64
#	define UD2_1024   UD2_256  ; UD2_256  ; UD2_256  ; UD2_256
#	define UD2_PAGE   UD2_1024 ; UD2_1024 ; UD2_1024 ; UD2_1024  

.globl VG_(trampoline_stuff_start)
VG_(trampoline_stuff_start):
/* See pub_core_trampoline.h for an explaination of this.  Also
   see pub_core_initimg.h, struct AIX5PreloadPage.  On entry, r3
   points to an AIX5PreloadPage structure.  Note we can only 
   use r2-r10 as scratch registers here since those are the
   only ones restored from the preload page when finally
   starting the client. */
.globl VG_(ppc64_aix5_do_preloads_then_start_client)
VG_(ppc64_aix5_do_preloads_then_start_client):
	stdu	1,-1024(1)
	std	3,512(1)	/* stash r3 512 bytes up stack */

	/* Try to load .../vgpreload_core.so */
	lwz	2,0(3)		/* r2 = __NR_kload */
	lwz	5,20(3)		/* r5 = off_preloadcorename */
	add	3,3,5		/* r6 = preloadcorename */
	bl	do_kload
	cmpdi	0,3,0
	beq	.Lfailed

	/* Try to load .../vgpreload_tool.so, if it exists */
	ld	3,512(1)	/* restore r3 */
	lwz	2,0(3)		/* r2 = __NR_kload */
	lwz	5,24(3)		/* r5 = off_preloadtoolname */
	cmpwi	0,5,0		/* skip tool preload if */
	beq	.Ltry_preload	/* name not present */
	add	3,3,5		/* r6 = preloadtoolname */
	bl	do_kload
	cmpdi	0,3,0
	beq	.Lfailed

.Ltry_preload:
	/* Try to load the LD_PRELOAD= file, if it exists */
	ld	3,512(1)	/* restore r3 */
	lwz	2,0(3)		/* r2 = __NR_kload */
	lwz	5,28(3)		/* r5 = off_ld_preloadname */
	cmpwi	0,5,0		/* skip ld_preload if */
	beq	.Lstart_client	/* name not present */
	add	3,3,5		/* r6 = ld_preloadname */
	bl	do_kload
	cmpdi	0,3,0
	beq	.Lfailed
	
.Lstart_client:
	/* Success.  Restore r2-r10 from preloadpage-> and start
	the client. */
	ld	3,512(1)	/* restore r3 */
	addi	1,1,1024
	ld	2,32+0(3)	/* preloadpage->client_start */
	mtctr	2
	ld	2,40+0(3)	/* preloadpage->r2 */
	ld	4,56+0(3)	/* preloadpage->r4 */
	ld	5,64+0(3)	/* preloadpage->r5 */
	ld	6,72+0(3)	/* preloadpage->r6 */
	ld	7,80+0(3)	/* preloadpage->r7 */
	ld	8,88+0(3)	/* preloadpage->r8 */
	ld	9,96+0(3)	/* preloadpage->r9 */
	ld	10,104+0(3)	/* preloadpage->r10 */
	ld	3,48+0(3)	/* preloadpage->r3 */
	bctr
	/*NOTREACHED*/
	trap

.Lfailed:
	/* __loadx barfed for some reason.  Print the error
	message and get out. */
	/* First the error msg */
	ld	3,512(1)	/* restore r3 */
	lwz	2,4(3)		/* r2 = __NR_kwrite */
	lwz	4,12(3)		/* r4 = offset of err msg */
	add	4,4,3		/* r4 = err msg */
	lwz	5,16(3)		/* r5 = length err msg */
	li	3,2		/* r3 = stderr */
	bl	do_syscall
	/* now call the diagnosis fn */
	ld	3,512(1)	/* restore r3 */
	ld	4,112(3)	/* preloadpage->p_diagnose_load_failure */
	ld	11,16(4)
	ld	2,8(4)		/* get its TOC ptr */
	ld	4,0(4)		/* get its entry point */
	mtlr	4
	blrl
	/* Now do _exit(1) */
	lwz	3,512(1)	/* restore r3 */
	lwz	2,8(3)		/* r2 = __NR_exit */
	li	3,1		/* doing _exit(1) */
	addi	1,1,1024	/* fix stack pointer */
	bl	do_syscall
	/*NOTREACHED*/
	trap
	
do_kload:
	/* On entry: r2 = __NR_kload,	r3 = name of module */
	li	4,0
	li	5,0
	li	6,0
	li	7,0
	li	8,0
	li	9,0
	li	10,0
do_syscall:
	crorc	6,6,6
	sc
	/* sc continues at 'lr', hence this 
	constitutes an automatic return */

	/* See comment in pub_core_trampoline.h for what this is for */
.globl VG_(ppctoc_magic_redirect_return_stub)
VG_(ppctoc_magic_redirect_return_stub):
	trap
	
.globl VG_(trampoline_stuff_end)
VG_(trampoline_stuff_end):

	/* and a trailing page of unexecutable code */
	UD2_PAGE

#	undef UD2_16
#	undef UD2_64
#	undef UD2_256
#	undef UD2_1024
#	undef UD2_PAGE
	
/*---------------- unknown ----------------*/
#else
#  error Unknown platform

#endif
#endif
#endif
#endif
#endif
#endif

#if defined(VGO_linux)
/* Let the linker know we don't need an executable stack */
.section .note.GNU-stack,"",@progbits
#endif

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
