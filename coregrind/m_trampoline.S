
/*--------------------------------------------------------------------*/
/*--- Trampoline code page stuff.                   m_trampoline.S ---*/
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

#include "pub_core_basics_asm.h"
#include "vki_unistd.h"

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
           recognize signal handler frames. */
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

.global VG_(x86_linux_REDIR_FOR__dl_sysinfo_int80)
VG_(x86_linux_REDIR_FOR__dl_sysinfo_int80):
	/* We can point our sysinfo stuff here */
	int	$0x80
	ret
	
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
VG_(amd64_linux_REDIR_FOR_vgettimeofday):
        movq    $__NR_gettimeofday, %rax
        syscall
        ret

.global VG_(amd64_linux_REDIR_FOR_vtime)
VG_(amd64_linux_REDIR_FOR_vtime):
        movq    $__NR_time, %rax
        syscall
        ret

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


/* Let the linker know we don't need an executable stack */
.section .note.GNU-stack,"",@progbits
		
/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
