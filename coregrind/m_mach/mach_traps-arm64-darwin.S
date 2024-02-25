/*--------------------------------------------------------------------*/
/*--- Basic Mach traps.                  mach_traps-arm64-darwin.S ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2007-2017 Apple Inc.
      Greg Parker  gparker@apple.com

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

#include "pub_core_basics_asm.h"

#if defined(VGP_arm64_darwin)

#include "vki/vki-scnums-darwin.h"

	// mach_port_name_t task_self_trap(void)
	.text
	.align 4
	.globl _task_self_trap
_task_self_trap:
	ldr	x16, =VG_DARWIN_MACH_SYSNO_FOR_KERNEL_ASM(__NR_task_self_trap)
	svc 0x80
	ret

	// mach_port_name_t thread_self_trap(void)
	.text
	.align 4
	.globl _thread_self_trap
_thread_self_trap:
	ldr	x16, =VG_DARWIN_MACH_SYSNO_FOR_KERNEL_ASM(__NR_thread_self_trap)
	svc 0x80
	ret

	// mach_msg_return_t mach_msg_trap(...)
	.text
	.align 4
	.globl _mach_msg_trap
_mach_msg_trap:
	ldr	x16, =VG_DARWIN_MACH_SYSNO_FOR_KERNEL_ASM(__NR_mach_msg_trap)
	svc 0x80
	ret

#if DARWIN_VERS >= DARWIN_13
	// mach_msg_return_t mach_msg2_trap(...)
	.text
	.align 4
	.globl _mach_msg2_trap
_mach_msg2_trap:
	ldr	x16, =VG_DARWIN_MACH_SYSNO_FOR_KERNEL_ASM(__NR_mach_msg2_trap)
	svc 0x80
	ret
#endif

	// mach_port_t mach_reply_port(...)
	.text
	.align 4
	.globl _mach_reply_port
_mach_reply_port:
	ldr	x16, =VG_DARWIN_MACH_SYSNO_FOR_KERNEL_ASM(__NR_mach_reply_port)
	svc 0x80
	ret

	// boolean_t swtch_pri(int)
	.text
	.align 4
	.globl _swtch_pri
_swtch_pri:
	ldr	x16, =VG_DARWIN_MACH_SYSNO_FOR_KERNEL_ASM(__NR_swtch_pri)
	svc 0x80
	ret

	// kern_return_t semaphore_wait(semaphore_t)
	.text
	.align 4
	.globl _semaphore_wait
_semaphore_wait:
	ldr	x16, =VG_DARWIN_MACH_SYSNO_FOR_KERNEL_ASM(__NR_semaphore_wait_trap)
	svc 0x80
	ret

	// kern_return_t semaphore_signal(semaphore_t)
	.text
	.align 4
	.globl _semaphore_signal
_semaphore_signal:
	ldr	x16, =VG_DARWIN_MACH_SYSNO_FOR_KERNEL_ASM(__NR_semaphore_signal_trap)
	svc 0x80
	ret

	// kern_return_t semaphore_signal_thread(semaphore_t, thread_t)
	.text
	.align 4
	.globl _semaphore_signal_thread
_semaphore_signal_thread:
	ldr	x16, =VG_DARWIN_MACH_SYSNO_FOR_KERNEL_ASM(__NR_semaphore_signal_thread_trap)
	svc 0x80
	ret

	// kern_return_t semaphore_wait_signal(semaphore_t, semaphore_t)
	.text
	.align 4
	.globl _semaphore_wait_signal
_semaphore_wait_signal:
	ldr	x16, =VG_DARWIN_MACH_SYSNO_FOR_KERNEL_ASM(__NR_semaphore_wait_signal_trap)
	svc 0x80
	ret

// TODO: most likely unacceptable license-wise
// From https://opensource.apple.com/source/libpthread/libpthread-454.80.2/src/pthread_asm.s.auto.html
#define _PTHREAD_STRUCT_DIRECT_STACKADDR_OFFSET   -48
#define _PTHREAD_STRUCT_DIRECT_STACKBOTTOM_OFFSET -40
.globl ___chkstk_darwin
___chkstk_darwin: // %w9/x9 == alloca size
  stp     x10, x11, [sp, #-16]

  // validate that the frame pointer is on our stack (no alt stack)
  mrs     x10, TPIDRRO_EL0
  and     x10, x10, #0xfffffffffffffff8

  // (%sp - pthread_self()->stackaddr) > 0 ?
  #if defined(__ARM64_ARCH_8_32__)
  ubfx    x9, x9, #0, #32
  ldur    w11, [x10, _PTHREAD_STRUCT_DIRECT_STACKADDR_OFFSET]
  #else
  ldur    x11, [x10, _PTHREAD_STRUCT_DIRECT_STACKADDR_OFFSET]
  #endif
  subs    x11, sp, x11
  b.hs    Lprobe

	// %sp <= pthread_self()->stackbottom ?
#if defined(__ARM64_ARCH_8_32__)
	ldur    w11, [x10, _PTHREAD_STRUCT_DIRECT_STACKBOTTOM_OFFSET]
#else
	ldur    x11, [x10, _PTHREAD_STRUCT_DIRECT_STACKBOTTOM_OFFSET]
#endif
	mov     x10, sp
	cmp     x10, x11
	b.ls    Lprobe

	// %sp - (uintptr_t)%x9 < pthread_self()->stackbottom ?
	subs    x10, x10, x9
	b.lo    Lcrash
	cmp     x10, x11
	b.lo    Lcrash

Lexit:
	ldp     x10, x11, [sp, #-16]
	ret

Lcrash:
	// POSIX mandates that stack overflow crashes with SIGSEGV
	// so load an address in the guard page and dereference it
	//
	// x11 contains pthread_self()->stackbottom already
	ldr     x11, [x11, #-8]
	// if main_thread caused stack growth with setrlimit()
	// fall into Lprobe and eventually cause SIGSEGV.

Lprobe:
	mov     x10, sp
	cmp     x9, #0x1000
	b.lo    Lend
Lloop:
	sub     x10, x10, #0x1000
	ldr     x11, [x10]
	sub     x9, x9, #0x1000
	cmp     x9, #0x1000
	b.hi    Lloop
Lend:
	sub     x10, x10, x9
	ldr     x11, [x10]
	b       Lexit

// TODO: most likely unacceptable license-wise
// From Apple's libplatform & XNU

#define _COMM_PAGE64_BASE_ADDRESS               (0x0000000FFFFFC000ULL) /* In TTBR0 */
#define _COMM_PAGE_START_ADDRESS                _COMM_PAGE64_BASE_ADDRESS
#define _COMM_PAGE_CPUFAMILY                    (_COMM_PAGE_START_ADDRESS+0x080)        // used by memcpy() resolver

#define EXT(x) _ ## x
.macro MOV64
	movk $0, #((($1) >> 48) & 0x000000000000FFFF), lsl #48
	movk $0, #((($1) >> 32) & 0x000000000000FFFF), lsl #32
	movk $0, #((($1) >> 16) & 0x000000000000FFFF), lsl #16
	movk $0, #((($1) >> 00) & 0x000000000000FFFF), lsl #00
.endmacro

#define MMU_I_CLINE	6		// cache line size as 1<<MMU_I_CLINE (64)

/* void sys_icache_invalidate(void *start, size_t length) */
.globl	_sys_icache_invalidate
.p2align	2
_sys_icache_invalidate:
	// see InvalidatePoU_IcacheRegion() in xnu/osfmk/arm64/caches_asm.s
	cbz		x1, 2f							// length > 0 ?

	and		x9, x0, #~((1<<MMU_I_CLINE)-1)	// cacheline align address
	and		x10, x0, #((1<<MMU_I_CLINE)-1)	// extend length by alignment
	add		x10, x1, x10
	sub		x10, x10, #1
	mov		x11, #-1
	eor		x10, x11, x10, lsr #MMU_I_CLINE	// compute cacheline counter
	dsb		ish
	mov		x2, #20
	mov		x3, #0							//0 = do not know, nonzero = we know
1:
	ic		ivau, x9						// invalidate icache line
	add		x9, x9, #1<<MMU_I_CLINE			// next cacheline address
	subs	x2, x2, #1
	b.ne	3f

	//we did some invalidates, time to maybe DSB?
	cbz		x3, 8f
4:											//we need it
	dsb		ish
	mov		x2, #20

3:
	adds	x10, x10, #1					// decrement cacheline counter
	b.ne	1b
	dsb		ish

	isb
2:
	ret

8:
	MOV64	x8, _COMM_PAGE_CPUFAMILY
	ldr		w8, [x8]
	adrp	x2, EXT(cpus_that_need_dsb_for_ic_ivau)@page
	add		x2, x2, EXT(cpus_that_need_dsb_for_ic_ivau)@pageoff
1:
	ldr		w3, [x2], #4
	cbz		w3, 2f
	cmp		w3, w8
	b.eq	4b								//match
	b		1b

2:											//no match
	mov		x2, #0
	mov		x3, #1
	b		3b


_cpus_that_need_dsb_for_ic_ivau:
	.word 0

#endif // defined(VGP_arm64_darwin)

/* Let the linker know we don't need an executable stack */
MARK_STACK_NO_EXEC

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
