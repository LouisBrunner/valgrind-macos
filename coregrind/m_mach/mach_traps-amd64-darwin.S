/*--------------------------------------------------------------------*/
/*--- Basic Mach traps.                  mach_traps-amd64-darwin.S ---*/
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

#if defined(VGP_amd64_darwin)

#include "vki/vki-scnums-darwin.h"

	// mach_port_name_t task_self_trap(void)
	.text
	.align 4
	.globl _task_self_trap
_task_self_trap:
	movq	$__NR_task_self_trap, %rax
	movq	%rcx, %r10
	syscall
	ret

// DDD: doesn't get used...
//	// mach_port_name_t host_self_trap(void)
//	.text
//	.align 4
//	.globl _host_self_trap
//_host_self_trap:
//	movq	$__NR_host_self_trap, %rax
//	movq	%rcx, %r10
//	syscall
//	ret

	// mach_port_name_t thread_self_trap(void)
	.text
	.align 4
	.globl _thread_self_trap
_thread_self_trap:
	movq	$__NR_thread_self_trap, %rax
	movq	%rcx, %r10
	syscall
	ret

	// mach_msg_return_t mach_msg_trap(...)
	.text
	.align 4
	.globl _mach_msg_trap
_mach_msg_trap:
	movq	$__NR_mach_msg_trap, %rax
	movq	%rcx, %r10
	syscall
	ret

#if DARWIN_VERS >= DARWIN_13_00
	// mach_msg_return_t mach_msg2_trap(...)
	.text
	.align 4
	.globl _mach_msg2_trap
_mach_msg2_trap:
	movq	$__NR_mach_msg2_trap, %rax
	movq	%rcx, %r10
	syscall
	ret
#endif

	// mach_port_t mach_reply_port(...)
	.text
	.align 4
	.globl _mach_reply_port
_mach_reply_port:
	movq	$__NR_mach_reply_port, %rax
	movq	%rcx, %r10
	syscall
	ret

	// boolean_t swtch_pri(int)
	.text
	.align 4
	.globl _swtch_pri
_swtch_pri:
	movq	$__NR_swtch_pri, %rax
	movq	%rcx, %r10
	syscall
	ret

	// kern_return_t semaphore_wait(semaphore_t)
	.text
	.align 4
	.globl _semaphore_wait
_semaphore_wait:
	movq	$__NR_semaphore_wait_trap, %rax
	movq	%rcx, %r10
	syscall
	ret

	// kern_return_t semaphore_signal(semaphore_t)
	.text
	.align 4
	.globl _semaphore_signal
_semaphore_signal:
	movq	$__NR_semaphore_signal_trap, %rax
	movq	%rcx, %r10
	syscall
	ret

	// kern_return_t semaphore_signal_thread(semaphore_t, thread_t)
	.text
	.align 4
	.globl _semaphore_signal_thread
_semaphore_signal_thread:
	movq	$__NR_semaphore_signal_thread_trap, %rax
	movq	%rcx, %r10
	syscall
	ret

	// kern_return_t semaphore_wait_signal(semaphore_t, semaphore_t)
	.text
	.align 4
	.globl _semaphore_wait_signal
_semaphore_wait_signal:
	movq	$__NR_semaphore_wait_signal_trap, %rax
	movq	%rcx, %r10
	syscall
	ret

#endif // defined(VGP_amd64_darwin)

/* Let the linker know we don't need an executable stack */
MARK_STACK_NO_EXEC

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
