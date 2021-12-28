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
	ldr	x16, =__NR_task_self_trap
	svc 0x80
	ret

	// mach_port_name_t thread_self_trap(void)
	.text
	.align 4
	.globl _thread_self_trap
_thread_self_trap:
	ldr	x16, =__NR_thread_self_trap
	svc 0x80
	ret

	// mach_msg_return_t mach_msg_trap(...)
	.text
	.align 4
	.globl _mach_msg_trap
_mach_msg_trap:
	ldr	x16, =__NR_mach_msg_trap
	svc 0x80
	ret

	// mach_port_t mach_reply_port(...)
	.text
	.align 4
	.globl _mach_reply_port
_mach_reply_port:
	ldr	x16, =__NR_mach_reply_port
	svc 0x80
	ret

	// boolean_t swtch_pri(int)
	.text
	.align 4
	.globl _swtch_pri
_swtch_pri:
	ldr	x16, =__NR_swtch_pri
	svc 0x80
	ret

	// kern_return_t semaphore_wait(semaphore_t)
	.text
	.align 4
	.globl _semaphore_wait
_semaphore_wait:
	ldr	x16, =__NR_semaphore_wait_trap
	svc 0x80
	ret

	// kern_return_t semaphore_signal(semaphore_t)
	.text
	.align 4
	.globl _semaphore_signal
_semaphore_signal:
	ldr	x16, =__NR_semaphore_signal_trap
	svc 0x80
	ret

	// kern_return_t semaphore_signal_thread(semaphore_t, thread_t)
	.text
	.align 4
	.globl _semaphore_signal_thread
_semaphore_signal_thread:
	ldr	x16, =__NR_semaphore_signal_thread_trap
	svc 0x80
	ret

	// kern_return_t semaphore_wait_signal(semaphore_t, semaphore_t)
	.text
	.align 4
	.globl _semaphore_wait_signal
_semaphore_wait_signal:
	ldr	x16, =__NR_semaphore_wait_signal_trap
	svc 0x80
	ret

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

#endif // defined(VGP_arm64_darwin)

/* Let the linker know we don't need an executable stack */
MARK_STACK_NO_EXEC

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
