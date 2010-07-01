
/*--------------------------------------------------------------------*/
/*--- Trampoline code page stuff.                   m_trampoline.S ---*/
/*--------------------------------------------------------------------*/

/*
  This file is part of Valgrind, a dynamic binary instrumentation
  framework.

  Copyright (C) 2000-2010 Julian Seward 
     jseward@acm.org
  Copyright (C) 2006-2010 OpenWorks LLP
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
#include "pub_core_vkiscnums_asm.h"

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
        movl    $ __NR_sigreturn, %eax
        int     $0x80
        ud2

.global VG_(x86_linux_SUBST_FOR_rt_sigreturn)
VG_(x86_linux_SUBST_FOR_rt_sigreturn):
        /* Likewise for rt signal frames */
        movl    $ __NR_rt_sigreturn, %eax
        int     $0x80
        ud2

/* There's no particular reason that this needs to be handwritten
   assembly, but since that's what this file contains, here's a
   simple index implementation (written in C and compiled by gcc.)

   unsigned char* REDIR_FOR_index ( const char* s, int c ) 
   { 
      unsigned char  ch = (unsigned char)((unsigned int)c); 
      unsigned char* p  = (unsigned char*)s; 
      while (1) { 
         if (*p == ch) return p;
         if (*p == 0)  return 0; 
         p++; 
      } 
   }
*/
.global VG_(x86_linux_REDIR_FOR_index)
.type   VG_(x86_linux_REDIR_FOR_index), @function
VG_(x86_linux_REDIR_FOR_index):
        pushl   %ebp
        movl    %esp, %ebp
        movl    8(%ebp), %eax
        movzbl  12(%ebp), %ecx
        movzbl  (%eax), %edx
        cmpb    %dl, %cl
        jne     .L9
        jmp     .L2
.L11:
        addl    $1, %eax
        movzbl  (%eax), %edx
        cmpb    %dl, %cl
        je      .L2
.L9:
        testb   %dl, %dl
        jne     .L11
        xorl    %eax, %eax
.L2:
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
.size VG_(amd64_linux_REDIR_FOR_vgettimeofday), .-.LfnB2
	
.global VG_(amd64_linux_REDIR_FOR_vtime)
.type   VG_(amd64_linux_REDIR_FOR_vtime), @function
VG_(amd64_linux_REDIR_FOR_vtime):
.LfnB3:	
        movq    $__NR_time, %rax
        syscall
        ret
.LfnE3:
.size VG_(amd64_linux_REDIR_FOR_vtime), .-.LfnB3

/* There's no particular reason that this needs to be handwritten
   assembly, but since that's what this file contains, here's a
   simple strlen implementation (written in C and compiled by gcc.)
*/
.global VG_(amd64_linux_REDIR_FOR_strlen)
.type   VG_(amd64_linux_REDIR_FOR_strlen), @function
VG_(amd64_linux_REDIR_FOR_strlen):
.LfnB4:
	xorl	%eax, %eax
	cmpb	$0, (%rdi)
	movq	%rdi, %rdx
	je	.L41
.L40:	addq	$1, %rdx
	cmpb	$0, (%rdx)
	jne	.L40
	movq	%rdx, %rax
	subq	%rdi, %rax
.L41:	ret
.LfnE4:
.size VG_(amd64_linux_REDIR_FOR_strlen), .-VG_(amd64_linux_REDIR_FOR_strlen)


/* A CIE for the above three functions, followed by their FDEs */
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
.LSfde4:
        .long   .LEfde4-.LASfde4
.LASfde4:
        .long   .LASfde4-.Lframe1
        .long   .LfnB4
        .long   .LfnE4-.LfnB4
        .uleb128 0x0
        .align 8
.LEfde4:
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

.global VG_(ppc32_linux_SUBST_FOR_sigreturn)
VG_(ppc32_linux_SUBST_FOR_sigreturn):
	li 0,__NR_sigreturn
        sc
        .long 0	/*illegal insn*/

.global VG_(ppc32_linux_SUBST_FOR_rt_sigreturn)
VG_(ppc32_linux_SUBST_FOR_rt_sigreturn):
	li 0,__NR_rt_sigreturn
        sc
        .long 0	/*illegal insn*/
	
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
        rlwinm 4,4,0,0xff
        cmpw 7,4,0
        beqlr 7
        cmpwi 7,0,0
        bne 7,.L308
        b .L304
.L309:	
        beq 6,.L304
.L308:	
        lbzu 0,1(3)
        cmpw 7,4,0
        cmpwi 6,0,0
        bne 7,.L309
        blr
.L304:	
        li 3,0
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

.global VG_(ppc64_linux_SUBST_FOR_rt_sigreturn)
VG_(ppc64_linux_SUBST_FOR_rt_sigreturn):
	li 0,__NR_rt_sigreturn
        sc
        .long 0	/*illegal insn*/

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
        lbz 0,0(3)
        rldicl 4,4,0,56
        cmpw 7,4,0
        beqlr 7
        cmpdi 7,0,0
        bne 7,.L18
        b .L14
.L19:	
        beq 6,.L14
.L18:	
        lbzu 0,1(3)
        cmpw 7,4,0
        cmpdi 6,0,0
        bne 7,.L19
        blr
.L14:	
        li 3,0
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

/*---------------- ppc32-linux ----------------*/

#elif defined(VGP_arm_linux)

#       define UD2_4      .word 0xFFFFFFFF
#	define UD2_16     UD2_4    ; UD2_4    ; UD2_4    ; UD2_4
#	define UD2_64     UD2_16   ; UD2_16   ; UD2_16   ; UD2_16
#	define UD2_256    UD2_64   ; UD2_64   ; UD2_64   ; UD2_64
#	define UD2_1024   UD2_256  ; UD2_256  ; UD2_256  ; UD2_256
#	define UD2_PAGE   UD2_1024 ; UD2_1024 ; UD2_1024 ; UD2_1024  

	/* a leading page of unexecutable code */
	UD2_PAGE

.global VG_(trampoline_stuff_start)
VG_(trampoline_stuff_start):

.global VG_(arm_linux_REDIR_FOR_strlen)
VG_(arm_linux_REDIR_FOR_strlen):
	mov	r2, r0
	ldrb	r0, [r0, #0]	@ zero_extendqisi2
	@ lr needed for prologue
	cmp	r0, #0
	bxeq	lr
	mov	r0, #0
.L5:
	add	r0, r0, #1
	ldrb	r3, [r0, r2]	@ zero_extendqisi2
	cmp	r3, #0
	bne	.L5
	bx	lr
	UD2_4

//.global VG_(arm_linux_REDIR_FOR_index)
//VG_(arm_linux_REDIR_FOR_index):
//	ldrb	r3, [r0, #0]	@ zero_extendqisi2
//	and	r1, r1, #255
//	cmp	r3, r1
//	@ lr needed for prologue
//	bne	.L9
//	bx	lr
//.L12:
//	ldrb	r3, [r0, #1]!	@ zero_extendqisi2
//	cmp	r3, r1
//	beq	.L11
//.L9:
//	cmp	r3, #0
//	bne	.L12
//	mov	r0, #0
//	bx	lr
//.L11:
//	bx	lr
//	UD2_4

.global VG_(arm_linux_REDIR_FOR_memcpy)
VG_(arm_linux_REDIR_FOR_memcpy):
	stmfd	sp!, {r4, r5, lr}
	subs	lr, r2, #0
	mov	r5, r0
	beq	.L2
	cmp	r0, r1
	bls	.L4
	add	r3, r0, lr
	add	r1, lr, r1
	cmp	lr, #3
	sub	r4, r3, #1
	sub	r0, r1, #1
	ble	.L28
	sub	ip, r3, #5
	sub	r1, r1, #5
.L8:
	ldrb	r3, [r1, #4]	@ zero_extendqisi2
	sub	lr, lr, #4
	strb	r3, [ip, #4]
	ldrb	r2, [r1, #3]	@ zero_extendqisi2
	cmp	lr, #3
	strb	r2, [ip, #3]
	ldrb	r3, [r1, #2]	@ zero_extendqisi2
	mov	r4, ip
	strb	r3, [ip, #2]
	ldrb	r2, [r1, #1]	@ zero_extendqisi2
	mov	r0, r1
	strb	r2, [ip, #1]
	sub	r1, r1, #4
	sub	ip, ip, #4
	bgt	.L8
	cmp	lr, #0
	beq	.L2
.L28:
	sub	r2, lr, #1
.L21:
	sub	r2, r2, #1
	ldrb	r3, [r0], #-1	@ zero_extendqisi2
	cmn	r2, #1
	strb	r3, [r4], #-1
	bne	.L21
.L2:
	mov	r0, r5
	ldmfd	sp!, {r4, r5, pc}
.L4:
	bcs	.L2
	cmp	lr, #3
	mov	ip, r0
	ble	.L29
.L19:
	ldrb	r3, [r1, #0]	@ zero_extendqisi2
	sub	lr, lr, #4
	strb	r3, [ip, #0]
	ldrb	r2, [r1, #1]	@ zero_extendqisi2
	cmp	lr, #3
	strb	r2, [ip, #1]
	ldrb	r3, [r1, #2]	@ zero_extendqisi2
	strb	r3, [ip, #2]
	ldrb	r2, [r1, #3]	@ zero_extendqisi2
	add	r1, r1, #4
	strb	r2, [ip, #3]
	add	ip, ip, #4
	bgt	.L19
	cmp	lr, #0
	beq	.L2
.L29:
	sub	r2, lr, #1
.L20:
	sub	r2, r2, #1
	ldrb	r3, [r1], #1	@ zero_extendqisi2
	cmn	r2, #1
	strb	r3, [ip], #1
	bne	.L20
	mov	r0, r5
	ldmfd	sp!, {r4, r5, pc}
	UD2_4

.global VG_(trampoline_stuff_end)
VG_(trampoline_stuff_end):

	/* and a trailing page of unexecutable code */
	UD2_PAGE

#	undef UD2_4
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

/*---------------- x86-darwin ----------------*/
#else
#if defined(VGP_x86_darwin)

        /* a leading page of unexecutable code */
.fill 2048, 2, 0x0b0f /* `ud2` */

.globl VG_(trampoline_stuff_start)
VG_(trampoline_stuff_start):

.globl VG_(x86_darwin_SUBST_FOR_sigreturn)
VG_(x86_darwin_SUBST_FOR_sigreturn):
        /* XXX does this need to have any special form? (cf x86-linux
	version) */
        movl    $ __NR_DARWIN_FAKE_SIGRETURN, %eax
        int     $0x80
        ud2

.globl VG_(x86_darwin_REDIR_FOR_strlen)
VG_(x86_darwin_REDIR_FOR_strlen):
        movl    4(%esp), %edx
        movl    %edx, %eax
        jmp     1f
0:
        incl    %eax
1:
        cmpb    $0, (%eax)
        jne     0b
        subl    %edx, %eax
        ret

.globl VG_(x86_darwin_REDIR_FOR_strcat)
VG_(x86_darwin_REDIR_FOR_strcat):
        pushl   %esi
        movl    8(%esp), %esi
        movl    12(%esp), %ecx
        movl    %esi, %edx
        jmp     1f
0:
        incl    %edx
1:
        cmpb    $0, (%edx)
        jne     0b
2:
        movzbl  (%ecx), %eax
        incl    %ecx
        movb    %al, (%edx)
        incl    %edx
        testb   %al, %al
        jne     2b
        movl    %esi, %eax
        popl    %esi
        ret


.globl VG_(x86_darwin_REDIR_FOR_strcmp)
VG_(x86_darwin_REDIR_FOR_strcmp):
        movl    4(%esp), %edx
        movl    8(%esp), %ecx
        jmp     1f
0:
        incl    %edx
        incl    %ecx
1:
        movzbl  (%edx), %eax
        testb   %al, %al
        je      2f
        cmpb    (%ecx), %al
        je      0b
2:
        movzbl  (%ecx),%edx
        movzbl  %al,%eax
        subl    %edx, %eax
        ret


.globl VG_(x86_darwin_REDIR_FOR_strcpy)
VG_(x86_darwin_REDIR_FOR_strcpy):
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%esi
	movl	8(%ebp), %esi
	movl	12(%ebp), %ecx
	movl	%esi, %edx
	jmp	1f
0:
	incl	%ecx
	incl	%edx
1:
	movzbl	(%ecx), %eax
	testb	%al, %al
	movb	%al, (%edx)
	jne	0b
	movl	%esi, %eax
	popl	%esi
	leave
	ret

.globl VG_(x86_darwin_REDIR_FOR_strlcat)
VG_(x86_darwin_REDIR_FOR_strlcat):
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%edi
	pushl	%esi
	subl	$16, %esp
	movl	8(%ebp), %esi
	movl	16(%ebp), %ecx
	movl	%esi, %edx
	leal	(%ecx,%esi), %eax
	jmp	1f
0:
	incl	%edx
1:
	cmpl	%edx, %eax
	je	2f
	cmpb	$0, (%edx)
	jne	0b
2:
	movl	%edx, %edi
	subl	%esi, %edi
	movl	%ecx, %esi
	subl	%edi, %esi
	je	3f
	movl	12(%ebp), %eax
	jmp	6f
3:
	movl	12(%ebp), %eax
	movl	%eax, (%esp)
	call	VG_(x86_darwin_REDIR_FOR_strlen)
	jmp	7f
4:
	cmpl	$1, %esi
	je	5f
	movb	%cl, (%edx)
	decl	%esi
	incl	%edx
5:
	incl	%eax
6:
	movzbl	(%eax), %ecx
	testb	%cl, %cl
	jne	4b
	movb	$0, (%edx)
	subl	12(%ebp), %eax
7:
	addl	$16, %esp
	leal	(%edi,%eax), %eax
	popl	%esi
	popl	%edi
	leave
	ret
	
	
.globl VG_(trampoline_stuff_end)
VG_(trampoline_stuff_end):

        /* a trailing page of unexecutable code */
.fill 2048, 2, 0x0b0f /* `ud2` */


/*---------------- amd64-darwin ----------------*/
#else
#if defined(VGP_amd64_darwin)

        /* a leading page of unexecutable code */
.fill 2048, 2, 0x0b0f /* `ud2` */

.globl VG_(trampoline_stuff_start)
VG_(trampoline_stuff_start):

.globl VG_(amd64_darwin_SUBST_FOR_sigreturn)
VG_(amd64_darwin_SUBST_FOR_sigreturn):
        /* XXX does this need to have any special form? (cf x86-linux
	version) */
        movq    $ __NR_DARWIN_FAKE_SIGRETURN, %rax
        syscall
        ud2

.globl VG_(amd64_darwin_REDIR_FOR_strlen)
VG_(amd64_darwin_REDIR_FOR_strlen):
        movq    %rdi, %rax
        jmp     1f
0:
        incq    %rax
1:
        cmpb    $0, (%rax)
        jne     0b
        subq    %rdi, %rax
        ret

.globl VG_(amd64_darwin_REDIR_FOR_strcat)
VG_(amd64_darwin_REDIR_FOR_strcat):
	movq	%rdi, %rdx
	jmp	1f
0:
	incq	%rdx
1:
	cmpb	$0, (%rdx)
	jne	0b
2:
	movzbl	(%rsi), %eax
	incq	%rsi
	movb	%al, (%rdx)
	incq	%rdx
	testb	%al, %al
	jne	2b
	movq	%rdi, %rax
	ret


.globl VG_(amd64_darwin_REDIR_FOR_strcmp)
VG_(amd64_darwin_REDIR_FOR_strcmp):
	jmp	1f
0:
	incq	%rdi
	incq	%rsi
1:
	movzbl	(%rdi), %eax
	testb	%al, %al
	je	2f
	cmpb	(%rsi), %al
	je	0b
2:
	movzbl	(%rsi), %edx
	movzbl	%al, %eax
	subl	%edx, %eax
	ret

.globl VG_(amd64_darwin_REDIR_FOR_strcpy)
VG_(amd64_darwin_REDIR_FOR_strcpy):
	pushq	%rbp
	movq	%rdi, %rdx
	movq	%rsp, %rbp
	jmp	1f
0:
	incq	%rsi
	incq	%rdx
1:
	movzbl	(%rsi), %eax
	testb	%al, %al
	movb	%al, (%rdx)
	jne	0b
	leave
	movq	%rdi, %rax
	ret
	
.globl VG_(amd64_darwin_REDIR_FOR_strlcat)
VG_(amd64_darwin_REDIR_FOR_strlcat):
	pushq	%rbp
	leaq	(%rdx,%rdi), %rax
	movq	%rdi, %rcx
	movq	%rsp, %rbp
	pushq	%rbx
	subq	$8, %rsp
	jmp	1f
0:
	incq	%rcx
1:
	cmpq	%rcx, %rax
	je	2f
	cmpb	$0, (%rcx)
	jne	0b
2:
	movq	%rcx, %rbx
	subq	%rdi, %rbx
	movq	%rdx, %rdi
	subq	%rbx, %rdi
	je	3f
	movq	%rsi, %rax
	jmp	6f
3:
	movq	%rsi, %rdi
	call	VG_(amd64_darwin_REDIR_FOR_strlen)
	jmp	7f
4:
	cmpq	$1, %rdi
	je	5f
	movb	%dl, (%rcx)
	decq	%rdi
	incq	%rcx
5:
	incq	%rax
6:
	movzbl	(%rax), %edx
	testb	%dl, %dl
	jne	4b
	movb	$0, (%rcx)
	subq	%rsi, %rax
7:
	leaq	(%rbx,%rax), %rax
	addq	$8, %rsp
	popq	%rbx
	leave
	ret

.globl VG_(amd64_darwin_REDIR_FOR_arc4random)
VG_(amd64_darwin_REDIR_FOR_arc4random):
	/* not very random, hope dyld won't mind */
	movq	$0x76616c6772696e64, %rax
	ret

.globl VG_(trampoline_stuff_end)
VG_(trampoline_stuff_end):

        /* a trailing page of unexecutable code */
.fill 2048, 2, 0x0b0f /* `ud2` */


/*---------------- unknown ----------------*/
#else
#  error Unknown platform

#endif
#endif
#endif
#endif
#endif
#endif
#endif
#endif

#if defined(VGO_linux)
/* Let the linker know we don't need an executable stack */
#  if defined(VGP_arm_linux)
   .section .note.GNU-stack,"",%progbits
#  else        
   .section .note.GNU-stack,"",@progbits
#  endif
#endif

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
