
/*--------------------------------------------------------------------*/
/*--- Linux-specific stuff for the core.                           ---*/
/*---                                              linux/core_os.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2005 Nicholas Nethercote
      njn@valgrind.org

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

#ifndef __LINUX_CORE_OS_H
#define __LINUX_CORE_OS_H

// Macros for adding Linux-specific, arch-independent wrappers to a syscall
// table.
#define LINX_(const, name)    SYS_WRAPPER_ENTRY_X_(vgOS_linux, const, name) 
#define LINXY(const, name)    SYS_WRAPPER_ENTRY_XY(vgOS_linux, const, name)

// The following syscall wrappers are Linux-specific, but arch-independent.
#define VGO_LINUX_SYSCALL_WRAPPER(x) \
   extern UInt VGO_(linux_##x##_flags); \
   extern void VGO_(linux_##x##_before)(ThreadId tid, ThreadState *tst); \
   extern void VGO_(linux_##x##_after) (ThreadId tid, ThreadState *tst)

VGO_LINUX_SYSCALL_WRAPPER(sys_exit_group);

VGO_LINUX_SYSCALL_WRAPPER(sys_mount);
VGO_LINUX_SYSCALL_WRAPPER(sys_oldumount);
VGO_LINUX_SYSCALL_WRAPPER(sys_umount);

VGO_LINUX_SYSCALL_WRAPPER(sys_llseek);
VGO_LINUX_SYSCALL_WRAPPER(sys_adjtimex);

VGO_LINUX_SYSCALL_WRAPPER(sys_setfsuid16);
VGO_LINUX_SYSCALL_WRAPPER(sys_setfsgid16);
VGO_LINUX_SYSCALL_WRAPPER(sys_setresuid16);  // man page says "non-standard";
VGO_LINUX_SYSCALL_WRAPPER(sys_getresuid16);
VGO_LINUX_SYSCALL_WRAPPER(sys_setresgid16);  // man page says "non-standard"
VGO_LINUX_SYSCALL_WRAPPER(sys_getresgid16);

VGO_LINUX_SYSCALL_WRAPPER(sys_setfsuid);
VGO_LINUX_SYSCALL_WRAPPER(sys_setfsgid);
VGO_LINUX_SYSCALL_WRAPPER(sys_setresuid);    // man page says "non-standard"
VGO_LINUX_SYSCALL_WRAPPER(sys_getresuid);
VGO_LINUX_SYSCALL_WRAPPER(sys_setresgid);    // man page says "non-standard"
VGO_LINUX_SYSCALL_WRAPPER(sys_getresgid);

VGO_LINUX_SYSCALL_WRAPPER(sys_ioperm);
VGO_LINUX_SYSCALL_WRAPPER(sys_syslog);
VGO_LINUX_SYSCALL_WRAPPER(sys_vhangup);
VGO_LINUX_SYSCALL_WRAPPER(sys_sysinfo);
VGO_LINUX_SYSCALL_WRAPPER(sys_personality);
VGO_LINUX_SYSCALL_WRAPPER(sys_sysctl);
VGO_LINUX_SYSCALL_WRAPPER(sys_prctl);

VGO_LINUX_SYSCALL_WRAPPER(sys_sendfile);
VGO_LINUX_SYSCALL_WRAPPER(sys_sendfile64);
VGO_LINUX_SYSCALL_WRAPPER(sys_futex);

VGO_LINUX_SYSCALL_WRAPPER(sys_epoll_create);
VGO_LINUX_SYSCALL_WRAPPER(sys_epoll_ctl);
VGO_LINUX_SYSCALL_WRAPPER(sys_epoll_wait);

VGO_LINUX_SYSCALL_WRAPPER(sys_gettid);
VGO_LINUX_SYSCALL_WRAPPER(sys_tkill);
VGO_LINUX_SYSCALL_WRAPPER(sys_tgkill);

VGO_LINUX_SYSCALL_WRAPPER(sys_fadvise64);
VGO_LINUX_SYSCALL_WRAPPER(sys_fadvise64_64);

VGO_LINUX_SYSCALL_WRAPPER(sys_io_setup);
VGO_LINUX_SYSCALL_WRAPPER(sys_io_destroy);
VGO_LINUX_SYSCALL_WRAPPER(sys_io_getevents);
VGO_LINUX_SYSCALL_WRAPPER(sys_io_submit);
VGO_LINUX_SYSCALL_WRAPPER(sys_io_cancel);

#define FUTEX_SEMA	0

#if FUTEX_SEMA
/* ---------------------------------------------------------------------
   Definition for a semaphore.  Defined in terms of futex.

   Futex semaphore operations taken from futex-2.2/usersem.h
   ------------------------------------------------------------------ */
typedef struct {
   int count;
} vg_sema_t;

extern Int __futex_down_slow(vg_sema_t *, int, struct vki_timespec *);
extern Int __futex_up_slow(vg_sema_t *);

void VGO_(sema_init)(vg_sema_t *);
static inline void VGO_(sema_deinit)(vg_sema_t *) 
{
}

static inline void VGO_(sema_down)(vg_sema_t *futx)
{
   Int val, woken = 0;

   /* Returns new value */
   while ((val = __futex_down(&futx->count)) != 0) {
      Int ret = __futex_down_slow(futx, val, NULL);
      if (ret < 0) 
	 return; /* error */
      else if (ret == 1)
	 return; /* passed */
      else if (ret == 0)
	 woken = 1; /* slept */
      else
	 /* loop */;
   }
   /* If we were woken, someone else might be sleeping too: set to -1 */
   if (woken) {
      futx->count = -1;
   }
   return;
}

/* If __futex_up increments count from 0 -> 1, noone was waiting.
   Otherwise, set to 1 and tell kernel to wake them up. */
static inline void VGO_(sema_up)(vg_sema_t *futx)
{
   if (!__futex_up(&futx->count))
      __futex_up_slow(futx);
}
#else  /* !FUTEX_SEMA */
/* 
   Not really a semaphore, but use a pipe for a token-passing scheme
 */
typedef struct {
   Int pipe[2];
   Int owner_thread;		/* who currently has it */
} vg_sema_t;

void VGO_(sema_init)(vg_sema_t *);
void VGO_(sema_deinit)(vg_sema_t *);
void VGO_(sema_down)(vg_sema_t *sema);
void VGO_(sema_up)(vg_sema_t *sema);

#endif	/* FUTEX_SEMA */

/* OS-specific thread state */
typedef struct {
   /* who we are */
   Int	lwpid;			/* PID of kernel task */
   Int	threadgroup;		/* thread group id */

   /* how we were started */
   UInt clone_flags;		/* flags passed to clone() to create this thread */
   Int  *parent_tidptr;
   Int  *child_tidptr;

   ThreadId parent;		/* parent tid (if any) */

   /* runtime details */
   Addr  valgrind_stack_base;	/* Valgrind's stack base */
   SizeT valgrind_stack_szB;	/* stack size in bytes */

   /* exit details */
   Int  exitcode;		/* in the case of exitgroup, set by someone else */
   Int  fatalsig;		/* fatal signal */
} os_thread_t;

#endif   // __LINUX_CORE_OS_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
