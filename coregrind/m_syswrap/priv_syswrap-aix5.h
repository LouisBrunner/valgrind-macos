
/*--------------------------------------------------------------------*/
/*--- AIX5-specific syscalls stuff.            priv_syswrap-aix5.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

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

   Neither the names of the U.S. Department of Energy nor the
   University of California nor the names of its contributors may be
   used to endorse or promote products derived from this software
   without prior written permission.
*/

#ifndef __PRIV_SYSWRAP_AIX5_H
#define __PRIV_SYSWRAP_AIX5_H

/* requires #include "priv_types_n_macros.h" */


/* Allocate a stack for this thread, if it doesn't already have one.
   They're allocated lazily, and never freed.  Returns the initial stack
   pointer value to use, or 0 if allocation failed. */
extern Addr ML_(allocstack)(ThreadId tid);

/* Re-read /proc/../map and update everything that depends on it. */
extern void ML_(aix5_rescan_procmap_after_load_or_unload) ( void );

/* Mess with the given thread's pc/toc so that it is entering
   pthread_exit() with argument PTHREAD_CANCELED.  Returns True if ok,
   False if it failed to do so, due to not being able to find
   pthread_exit() by searching symbol tables. */
extern Bool ML_(aix5_force_thread_into_pthread_exit)( ThreadId );

/* For various reasons, on AIX we may have to just give up if
   continuing is too difficult (eg, risk of future deadlock).  This
   sets up the process state to exit straight away, but does not
   actually itself exit. */
extern
void ML_(aix5_set_threadstate_for_emergency_exit)(ThreadId tid, HChar* why);

/* Debugging stuff, for making sense of AIX5 threading syscalls. */
extern void   ML_(aix5debugstuff_show_tstate) ( Addr, HChar* who );
extern void   ML_(aix5debugstuff_show_tstate_flags) ( UWord w );
extern HChar* ML_(aix5debugstuff_pc_to_fnname) ( Addr pc );


// Syscalls which can be handled by a common wrapper for
// both ppc32-aix5 and ppc64-aix5

DECL_TEMPLATE(aix5, sys___libc_sbrk);
DECL_TEMPLATE(aix5, sys___msleep);
DECL_TEMPLATE(aix5, sys__clock_settime);
DECL_TEMPLATE(aix5, sys__exit);
DECL_TEMPLATE(aix5, sys__fp_fpscrx_sc);
DECL_TEMPLATE(aix5, sys__getpgrp);
DECL_TEMPLATE(aix5, sys__getpid);
DECL_TEMPLATE(aix5, sys__getppid);
DECL_TEMPLATE(aix5, sys__getpriority);
DECL_TEMPLATE(aix5, sys__nsleep);
DECL_TEMPLATE(aix5, sys__pause);
DECL_TEMPLATE(aix5, sys__poll);
DECL_TEMPLATE(aix5, sys__select);
DECL_TEMPLATE(aix5, sys__sem_wait);
DECL_TEMPLATE(aix5, sys__setpgid);
DECL_TEMPLATE(aix5, sys__setsid);
DECL_TEMPLATE(aix5, sys__sigaction);
DECL_TEMPLATE(aix5, sys__thread_self);
DECL_TEMPLATE(aix5, sys__thread_setsched);
DECL_TEMPLATE(aix5, sys_access);
DECL_TEMPLATE(aix5, sys_accessx);
DECL_TEMPLATE(aix5, sys_appgetrlimit);
DECL_TEMPLATE(aix5, sys_appgetrusage);
DECL_TEMPLATE(aix5, sys_apprestimer);
DECL_TEMPLATE(aix5, sys_appsetrlimit);
DECL_TEMPLATE(aix5, sys_appulimit);
DECL_TEMPLATE(aix5, sys_bind);
DECL_TEMPLATE(aix5, sys_chdir);
DECL_TEMPLATE(aix5, sys_chmod);
DECL_TEMPLATE(aix5, sys_chown);
DECL_TEMPLATE(aix5, sys_close);
DECL_TEMPLATE(aix5, sys_connext);
DECL_TEMPLATE(aix5, sys_execve);
DECL_TEMPLATE(aix5, sys_finfo);
DECL_TEMPLATE(aix5, sys_fstatfs);
DECL_TEMPLATE(aix5, sys_fstatx);
DECL_TEMPLATE(aix5, sys_fsync);
DECL_TEMPLATE(aix5, sys_getdirent);
DECL_TEMPLATE(aix5, sys_getdirent64);
DECL_TEMPLATE(aix5, sys_getdomainname);
DECL_TEMPLATE(aix5, sys_getgidx);
DECL_TEMPLATE(aix5, sys_getgroups);
DECL_TEMPLATE(aix5, sys_gethostname);
DECL_TEMPLATE(aix5, sys_getpriv);
DECL_TEMPLATE(aix5, sys_getprocs);
DECL_TEMPLATE(aix5, sys_getrpid);
DECL_TEMPLATE(aix5, sys_getsockopt);
DECL_TEMPLATE(aix5, sys_gettimerid);
DECL_TEMPLATE(aix5, sys_getuidx);
DECL_TEMPLATE(aix5, sys_incinterval);
DECL_TEMPLATE(aix5, sys_kfcntl);
DECL_TEMPLATE(aix5, sys_kfork);
DECL_TEMPLATE(aix5, sys_kftruncate);
DECL_TEMPLATE(aix5, sys_kgetsidx);
DECL_TEMPLATE(aix5, sys_kill);
DECL_TEMPLATE(aix5, sys_kioctl);
DECL_TEMPLATE(aix5, sys_klseek);
DECL_TEMPLATE(aix5, sys_knlist);
DECL_TEMPLATE(aix5, sys_kpread);
DECL_TEMPLATE(aix5, sys_kread);
DECL_TEMPLATE(aix5, sys_kreadv);
DECL_TEMPLATE(aix5, sys_kthread_ctl);
DECL_TEMPLATE(aix5, sys_ktruncate);
DECL_TEMPLATE(aix5, sys_kwaitpid);
DECL_TEMPLATE(aix5, sys_kwrite);
DECL_TEMPLATE(aix5, sys_kwritev);
DECL_TEMPLATE(aix5, sys_listen);
DECL_TEMPLATE(aix5, sys_loadbind);
DECL_TEMPLATE(aix5, sys_loadquery);
DECL_TEMPLATE(aix5, sys_lseek);
DECL_TEMPLATE(aix5, sys_mkdir);
DECL_TEMPLATE(aix5, sys_mmap);
DECL_TEMPLATE(aix5, sys_mprotect);
DECL_TEMPLATE(aix5, sys_mntctl);
DECL_TEMPLATE(aix5, sys_munmap);
DECL_TEMPLATE(aix5, sys_naccept);
DECL_TEMPLATE(aix5, sys_ngetpeername);
DECL_TEMPLATE(aix5, sys_ngetsockname);
DECL_TEMPLATE(aix5, sys_nrecvfrom);
DECL_TEMPLATE(aix5, sys_nrecvmsg);
DECL_TEMPLATE(aix5, sys_nsendmsg);
DECL_TEMPLATE(aix5, sys_open);
DECL_TEMPLATE(aix5, sys_pipe);
DECL_TEMPLATE(aix5, sys_privcheck);
DECL_TEMPLATE(aix5, sys_readlink);
DECL_TEMPLATE(aix5, sys_recv);
DECL_TEMPLATE(aix5, sys_rename);
DECL_TEMPLATE(aix5, sys_sbrk);
DECL_TEMPLATE(aix5, sys_sched_get_priority_max);
DECL_TEMPLATE(aix5, sys_sem_destroy);
DECL_TEMPLATE(aix5, sys_sem_init);
DECL_TEMPLATE(aix5, sys_sem_post);
DECL_TEMPLATE(aix5, sys_send);
DECL_TEMPLATE(aix5, sys_setgid);
DECL_TEMPLATE(aix5, sys_setsockopt);
DECL_TEMPLATE(aix5, sys_setuid);
DECL_TEMPLATE(aix5, sys_shmat);
DECL_TEMPLATE(aix5, sys_shmctl);
DECL_TEMPLATE(aix5, sys_shmdt);
DECL_TEMPLATE(aix5, sys_shmget);
DECL_TEMPLATE(aix5, sys_shutdown);
DECL_TEMPLATE(aix5, sys_sigcleanup);
DECL_TEMPLATE(aix5, sys_sigprocmask);
DECL_TEMPLATE(aix5, sys_socket);
DECL_TEMPLATE(aix5, sys_statfs);
DECL_TEMPLATE(aix5, sys_statx);
DECL_TEMPLATE(aix5, sys_symlink);
DECL_TEMPLATE(aix5, sys_sys_parm);
DECL_TEMPLATE(aix5, sys_sysconfig);
DECL_TEMPLATE(aix5, sys_thread_create);
DECL_TEMPLATE(aix5, sys_thread_init);
DECL_TEMPLATE(aix5, sys_thread_kill);
/* thread_setmymask_fast is platform specific */
DECL_TEMPLATE(aix5, sys_thread_setmystate);
DECL_TEMPLATE(aix5, sys_thread_setmystate_fast);
/* thread_setstate is platform specific */
DECL_TEMPLATE(aix5, sys_thread_terminate_unlock);
DECL_TEMPLATE(aix5, sys_thread_tsleep);
DECL_TEMPLATE(aix5, sys_thread_tsleep_event);
DECL_TEMPLATE(aix5, sys_thread_twakeup);
DECL_TEMPLATE(aix5, sys_thread_twakeup_event);
DECL_TEMPLATE(aix5, sys_thread_unlock);
DECL_TEMPLATE(aix5, sys_thread_waitlock);
DECL_TEMPLATE(aix5, sys_thread_waitlock_);
DECL_TEMPLATE(aix5, sys_times);
DECL_TEMPLATE(aix5, sys_umask);
DECL_TEMPLATE(aix5, sys_uname);
DECL_TEMPLATE(aix5, sys_unlink);
DECL_TEMPLATE(aix5, sys_utimes);
DECL_TEMPLATE(aix5, sys_vmgetinfo);
DECL_TEMPLATE(aix5, sys_yield);


#endif   // __PRIV_SYSWRAP_AIX5_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
