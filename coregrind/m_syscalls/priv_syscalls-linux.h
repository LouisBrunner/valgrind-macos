
/*--------------------------------------------------------------------*/
/*--- Linux-specific syscalls stuff.         priv_syscalls-linux.h ---*/
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

#ifndef __PRIV_SYSCALLS_LINUX_H
#define __PRIV_SYSCALLS_LINUX_H

// Macros for adding Linux-specific, arch-independent wrappers to a syscall
// table.
#define LINX_(const, name)    SYS_WRAPPER_ENTRY_X_(vgOS_linux, const, name) 
#define LINXY(const, name)    SYS_WRAPPER_ENTRY_XY(vgOS_linux, const, name)

// The following syscall wrappers are Linux-specific, but arch-independent.
#define LINUX_SYSCALL_WRAPPER(x) \
   extern UInt VGO_(linux_##x##_flags); \
   extern void VGO_(linux_##x##_before)(ThreadId tid, ThreadState *tst); \
   extern void VGO_(linux_##x##_after) (ThreadId tid, ThreadState *tst)

LINUX_SYSCALL_WRAPPER(sys_exit_group);

LINUX_SYSCALL_WRAPPER(sys_mount);
LINUX_SYSCALL_WRAPPER(sys_oldumount);
LINUX_SYSCALL_WRAPPER(sys_umount);

LINUX_SYSCALL_WRAPPER(sys_llseek);
LINUX_SYSCALL_WRAPPER(sys_adjtimex);

LINUX_SYSCALL_WRAPPER(sys_setfsuid16);
LINUX_SYSCALL_WRAPPER(sys_setfsgid16);
LINUX_SYSCALL_WRAPPER(sys_setresuid16);  // man page says "non-standard";
LINUX_SYSCALL_WRAPPER(sys_getresuid16);
LINUX_SYSCALL_WRAPPER(sys_setresgid16);  // man page says "non-standard"
LINUX_SYSCALL_WRAPPER(sys_getresgid16);

LINUX_SYSCALL_WRAPPER(sys_setfsuid);
LINUX_SYSCALL_WRAPPER(sys_setfsgid);
LINUX_SYSCALL_WRAPPER(sys_setresuid);    // man page says "non-standard"
LINUX_SYSCALL_WRAPPER(sys_getresuid);
LINUX_SYSCALL_WRAPPER(sys_setresgid);    // man page says "non-standard"
LINUX_SYSCALL_WRAPPER(sys_getresgid);

LINUX_SYSCALL_WRAPPER(sys_ioperm);
LINUX_SYSCALL_WRAPPER(sys_syslog);
LINUX_SYSCALL_WRAPPER(sys_vhangup);
LINUX_SYSCALL_WRAPPER(sys_sysinfo);
LINUX_SYSCALL_WRAPPER(sys_personality);
LINUX_SYSCALL_WRAPPER(sys_sysctl);
LINUX_SYSCALL_WRAPPER(sys_prctl);

LINUX_SYSCALL_WRAPPER(sys_sendfile);
LINUX_SYSCALL_WRAPPER(sys_sendfile64);
LINUX_SYSCALL_WRAPPER(sys_futex);

LINUX_SYSCALL_WRAPPER(sys_epoll_create);
LINUX_SYSCALL_WRAPPER(sys_epoll_ctl);
LINUX_SYSCALL_WRAPPER(sys_epoll_wait);

LINUX_SYSCALL_WRAPPER(sys_gettid);
LINUX_SYSCALL_WRAPPER(sys_tkill);
LINUX_SYSCALL_WRAPPER(sys_tgkill);

LINUX_SYSCALL_WRAPPER(sys_fadvise64);
LINUX_SYSCALL_WRAPPER(sys_fadvise64_64);

LINUX_SYSCALL_WRAPPER(sys_io_setup);
LINUX_SYSCALL_WRAPPER(sys_io_destroy);
LINUX_SYSCALL_WRAPPER(sys_io_getevents);
LINUX_SYSCALL_WRAPPER(sys_io_submit);
LINUX_SYSCALL_WRAPPER(sys_io_cancel);

#endif   // __PRIV_SYSCALLS_LINUX_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
