
/*--------------------------------------------------------------------*/
/*--- Linux-specific syscalls stuff.          priv_syswrap-linux.h ---*/
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

#ifndef __PRIV_SYSWRAP_LINUX_H
#define __PRIV_SYSWRAP_LINUX_H

/* requires #include "priv_types_n_macros.h" */

// Run a thread from beginning to end. 
extern VgSchedReturnCode ML_(thread_wrapper)(Word /*ThreadId*/ tid);

DECL_TEMPLATE(linux, sys_exit_group);

DECL_TEMPLATE(linux, sys_mount);
DECL_TEMPLATE(linux, sys_oldumount);
DECL_TEMPLATE(linux, sys_umount);

DECL_TEMPLATE(linux, sys_llseek);
DECL_TEMPLATE(linux, sys_adjtimex);

DECL_TEMPLATE(linux, sys_setfsuid16);
DECL_TEMPLATE(linux, sys_setfsgid16);
DECL_TEMPLATE(linux, sys_setresuid16);  // man page says "non-standard";
DECL_TEMPLATE(linux, sys_getresuid16);
DECL_TEMPLATE(linux, sys_setresgid16);  // man page says "non-standard"
DECL_TEMPLATE(linux, sys_getresgid16);

DECL_TEMPLATE(linux, sys_setfsuid);
DECL_TEMPLATE(linux, sys_setfsgid);
DECL_TEMPLATE(linux, sys_setresuid);    // man page says "non-standard"
DECL_TEMPLATE(linux, sys_getresuid);
DECL_TEMPLATE(linux, sys_setresgid);    // man page says "non-standard"
DECL_TEMPLATE(linux, sys_getresgid);

DECL_TEMPLATE(linux, sys_ioperm);
DECL_TEMPLATE(linux, sys_syslog);
DECL_TEMPLATE(linux, sys_vhangup);
DECL_TEMPLATE(linux, sys_sysinfo);
DECL_TEMPLATE(linux, sys_personality);
DECL_TEMPLATE(linux, sys_sysctl);
DECL_TEMPLATE(linux, sys_prctl);

DECL_TEMPLATE(linux, sys_sendfile);
DECL_TEMPLATE(linux, sys_sendfile64);
DECL_TEMPLATE(linux, sys_futex);

DECL_TEMPLATE(linux, sys_epoll_create);
DECL_TEMPLATE(linux, sys_epoll_ctl);
DECL_TEMPLATE(linux, sys_epoll_wait);

DECL_TEMPLATE(linux, sys_gettid);
DECL_TEMPLATE(linux, sys_set_tid_address);
DECL_TEMPLATE(linux, sys_tkill);
DECL_TEMPLATE(linux, sys_tgkill);

DECL_TEMPLATE(linux, sys_fadvise64);
DECL_TEMPLATE(linux, sys_fadvise64_64);

DECL_TEMPLATE(linux, sys_io_setup);
DECL_TEMPLATE(linux, sys_io_destroy);
DECL_TEMPLATE(linux, sys_io_getevents);
DECL_TEMPLATE(linux, sys_io_submit);
DECL_TEMPLATE(linux, sys_io_cancel);

DECL_TEMPLATE(linux, sys_set_mempolicy);
DECL_TEMPLATE(linux, sys_get_mempolicy);

DECL_TEMPLATE(linux, sys_inotify_init);
DECL_TEMPLATE(linux, sys_inotify_add_watch);
DECL_TEMPLATE(linux, sys_inotify_rm_watch);

DECL_TEMPLATE(linux, sys_mq_open);
DECL_TEMPLATE(linux, sys_mq_unlink);
DECL_TEMPLATE(linux, sys_mq_timedsend);
DECL_TEMPLATE(linux, sys_mq_timedreceive);
DECL_TEMPLATE(linux, sys_mq_notify);
DECL_TEMPLATE(linux, sys_mq_getsetattr);

#endif   // __PRIV_SYSWRAP_LINUX_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
