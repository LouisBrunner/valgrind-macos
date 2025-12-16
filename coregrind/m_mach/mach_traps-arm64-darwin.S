/*--------------------------------------------------------------------*/
/*--- Basic Mach traps.                  mach_traps-arm64-darwin.S ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (c) 2020-2025 Louis Brunner <louis.brunner.fr@gmail.com>

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 3 of the
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

.globl ___chkstk_darwin
___chkstk_darwin:
  // We don't care about doing stack checking within valgrind
  ret

#endif // defined(VGP_arm64_darwin)

/* Let the linker know we don't need an executable stack */
MARK_STACK_NO_EXEC

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
