
/*--------------------------------------------------------------------*/
/*--- Linux-specific stuff for the core.                           ---*/
/*---                                              linux/core_os.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an extensible x86 protected-mode
   emulator for monitoring program execution on x86-Unixes.

   Copyright (C) 2000-2004 Nicholas Nethercote
      njn25@cam.ac.uk

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

#define LINUX_SYSCALL_WRAPPER(x) \
   extern UInt VGA_(linux_##x##_flags); \
   extern void VGA_(linux_##x##_before)(ThreadId tid, ThreadState *tst); \
   extern void VGA_(linux_##x##_after) (ThreadId tid, ThreadState *tst)

// These syscalls are Linux-specific, but architecture-independent.
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

LINUX_SYSCALL_WRAPPER(sys_io_setup);
LINUX_SYSCALL_WRAPPER(sys_io_destroy);
LINUX_SYSCALL_WRAPPER(sys_io_getevents);
LINUX_SYSCALL_WRAPPER(sys_io_submit);
LINUX_SYSCALL_WRAPPER(sys_io_cancel);

#endif   // __LINUX_CORE_OS_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
