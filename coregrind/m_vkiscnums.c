
/*--------------------------------------------------------------------*/
/*--- Notional "implementation" for m_vkiscnums.                   ---*/
/*---                                                m_vkiscnums.c ---*/
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
*/

#include "pub_core_basics.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcprint.h"

#include "pub_core_vkiscnums.h"     /* self */

/* We have pub_{core,tool}_vkiscnums.h.  This is the matching implementation
   for that interface.  

   On Linux, the interface exports a bunch of "#define __NR_foo 42" style
   definitions, so there is no implementation.

   On AIX, syscall numbers are not fixed ahead of time; in principle
   each process can have its own assignment of numbers to actual
   syscalls.  As a result we have a bunch of global variables to store
   the number for each syscall, which are assigned to at system
   startup, and a bunch of #defines which map "__NR_foo" names to
   these global variables.  Initially, when we don't know what a
   syscall's number is, it is set to __NR_AIX5_UNKNOWN.

   Therefore, on AIX, this module provides a home for those variables.

   It also provides VG_(aix5_register_syscall) to assign numbers to
   those variables.
*/

//---------------------------------------------------------------------------
#if defined(VGO_linux)
//---------------------------------------------------------------------------

Char* VG_(sysnum_string)(Word sysnum, SizeT n_buf, Char* buf)
{
   VG_(snprintf)(buf, n_buf, "%3ld", sysnum);
   return buf;
}

Char* VG_(sysnum_string_extra)(Word sysnum, SizeT n_buf, Char* buf)
{
   return VG_(sysnum_string)(sysnum, n_buf, buf);
}

//---------------------------------------------------------------------------
#elif defined(VGO_aix5)
//---------------------------------------------------------------------------

/* These ones are for AIX 5.2. */
Int VG_(aix5_NR_utrchook_sc) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_thread_create) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_kfork) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_kra_fork) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_execve) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_ra_execve) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__load) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR___unload) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_loadbind) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR___loadx) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_bindprocessor) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_trcgent) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_trcgen) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_trchk) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_trchkt) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_trchkl) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_trchklt) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_trchkg) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_trchkgt) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_kill) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__addcpucosts) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_mycpu) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_adjtime) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_checkpnt_block) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__checkpnt_kill) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__checkpnt_fail) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__checkpnt_commit) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__checkpnt_register) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__checkpnt) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_setcrid) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_getcrid) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_mkcrid) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_checkpnt_wait) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_checkpnt_deliver) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_gencore) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_thread_terminate) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__exit) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_kwaitpid64) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_kwaitpid) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_yield) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_getprocs64) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_getevars) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_getargs) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_getthrds64) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_getthrds) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_getprocs) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_sigcleanup) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__setpri) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__getpri) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_profil) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_reboot) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_appgetrlimit) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_appsetrlimit) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__setpriority) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__getpriority) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_setrlimit64) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_getrlimit64) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_appgetrusage) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_getrusage64) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_getvtid) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_getrtid) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_getrpid) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_restart_wait) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_restart) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__rmcpucosts) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__clock_getcpuclockid) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__clock_settime) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__clock_gettime) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__clock_getres) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__timer_settime) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__timer_gettime) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__timer_getoverrun) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__timer_delete) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__timer_create) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__sigqueue) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__sigsuspend) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__sigaction) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_sigprocmask) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_siglocalmask) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_count_event_waiters) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_thread_waitact) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_thread_waitlock_local) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_thread_waitlock) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_thread_wait) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_thread_unlock) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_thread_twakeup_unlock) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_thread_twakeup_event) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_thread_twakeup) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_thread_tsleep_event) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_thread_tsleep_chkpnt) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_thread_tsleep) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_thread_post_many) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_thread_post) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_ue_proc_unregister) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_ue_proc_register) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_kthread_ctl) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__thread_setsched) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_threads_runnable) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_thread_getregs) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_thread_terminate_unlock) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_thread_terminate_ack) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_thread_setstate_fast) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_thread_setstate) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_thread_setmymask_fast) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_thread_setmystate_fast) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_thread_setmystate) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_thread_init) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_times) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__nsleep) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_reltimerid) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_appresinc) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_apprestimer) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_appresabs) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_appsettimer) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_appgettimer) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_gettimerid) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_incinterval) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_absinterval) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_getinterval) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_upfget) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__wlm_wait) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__wlm_post) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__wlm_event_init) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__wlm_set_tag) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__wlm_set) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_ptrace64) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_ptracex) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_ptrace) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_ksetcontext_sigreturn) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_ksetcontext) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_kgetcontext) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_sigreturn) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__wlm_get_bio_stats) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_splice) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_rmsock) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_nrecvmsg) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_socket_aio_dequeue) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_getkerninfo) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_getpeereid) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_getpeername) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_ngetpeername) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_getsockname) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_ngetsockname) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_getsockopt) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_setsockopt) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_shutdown) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_recvmsg) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_recv) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_nrecvfrom) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_recvfrom) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_nsendmsg) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_sendmsg) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_send) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_sendto) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_socketpair) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_accept) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_naccept) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_listen) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_bind) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_socket) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_connext) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_setdomainname) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_getdomainname) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_sethostname) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_sethostid) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_gethostid) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_gethostname) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_send_file) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__rmlmbcost) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR___rs_pickmcm) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_rs_getsystem) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_rs_getassociativity) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_rs_setpartition) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_rs_getpartition) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_ra_getrset) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_rs_getinfo) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_rs_getrad) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_rs_numrads) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR___kdb_format_print_rele) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR___kdb_format_print_init) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_close) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_kfsync_range) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_fsync) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_kpwrite) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_kwritev) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_kwrite) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_kpread) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_kreadv) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_kread) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_klseek) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__lseek) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_lseek) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__setsid) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__setpgid) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__setpgrp) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__getpgrpx) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__getpgrp) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__getppid) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__thread_self) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__getpid) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_kgetpgidx) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_setuid) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_setuidx) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_getuidx) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_seteuid) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_setreuid) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_chdir) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_fchdir) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_chroot) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_fchmod) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_chmod) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_chown) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_lchown) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_fchown) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_fchownx) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_chownx) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_kfclear) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_fclear) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_ffinfo) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_finfo) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_fscntl) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_ktruncate) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_kftruncate) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_truncate) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_ftruncate) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_getdirent64) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_getdirent) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_kioctl32) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_kioctl) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_link) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_klockf) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_lockf) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_mkdir) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_mknod) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_mntctl) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_vmount) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_creat) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_openx) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_open) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_quotactl) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_rename) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_rmdir) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_fstatx) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_statx) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_symlink) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_readlink) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_syncvfs) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_sync) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_umask) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_uvmount) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_umount) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_unameu) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_unamex) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_uname) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_unlink) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_ustat) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_utimes) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR___msgxrcv) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR___msgrcv) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR___msgsnd) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_msgctl) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_msgget) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_getgidx) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR___semop) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_semget) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_semctl) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_shmctl) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_shmdt) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_shmat) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_shmget) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_ra_shmgetv) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_ra_shmget) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_privcheck) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_disclaim) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__sem_destroy_unnamed) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__sem_wait) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__sem_close) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__sem_open) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_sem_unlink) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_sem_post) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_sem_init) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_sem_getvalue) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_sem_destroy) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__mq_notify) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__mq_open) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_mq_unlink) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_mq_setattr) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_mq_send) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_mq_receive) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_mq_getattr) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_mq_close) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_shm_unlink) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_shm_open) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__poll) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__select) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_sysconfig) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_sys_parm) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_loadquery) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_knlist) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_brk) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_fjfs_sys_call) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_jfs_sys_call) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_acct) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__dr_unregister) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__dr_notify) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__dr_register) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_getlparload) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_dr_reconfig) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_projctl) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_sbrk) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__sigpending) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__pause) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_thread_kill) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_sigstack) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_sigaltstack) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_appulimit) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_ras_service) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__wlm_class_descr2key) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__wlm_get_procinfo) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__wlm_get_info) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__wlm_getclassname) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__wlm_unload_classes) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__wlm_load) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__wlm_tune) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__wlm_assign) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__wlm_classify) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_fp_cpusync) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__fp_trapstate_ker) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__ewlm_classify_correlator) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__arm_stop_transaction) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__arm_destroy_application) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__arm_stop_application) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__arm_generate_correlator) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__arm_discard_transaction) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__arm_unbind_thread) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__arm_bind_thread) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__arm_unblock_transaction) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__arm_block_transaction) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__arm_update_transaction) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__arm_register_metric) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__arm_report_transaction) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__arm_start_transaction) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__arm_register_transaction) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__arm_start_application) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__arm_register_application) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__lsarm_getinfo) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__ewlm_init) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__ewlm_query) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_ewlm_verify_policy) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_ewlm_abort_policy) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_ewlm_commit_policy) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_ewlm_prepare_policy) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_ewlm_get_completions) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_ewlm_get_activedata) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_ewlm_get_appldata) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_ewlm_collect_samples) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_ewlm_disconnect) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_ewlm_connect) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_auditlog) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_auditproc) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_getgroups) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_setgid) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_setgidx) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_setgroups) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_frevoke) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_revoke) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR___pag_setvalue) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR___pag_getvalue) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR___pag_getid) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR___pag_getname) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR___pag_setname) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_kcap_set_proc) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_kcap_get_proc) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_pipe) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_mwakeup) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR___msleep) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_kmmap) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_msem_remove) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_mincore) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_madvise) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_munmap) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_msync) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_mprotect) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_mmap) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_swapqry) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_swapon) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_swapoff) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_psdanger) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_vmgetinfo) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_rs_admregistername) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_rs_discardname) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_rs_setnameattr) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_rs_registername) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_rs_getnamedrset) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_rs_getnameattr) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_rs_getrsetnames) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_ra_attachrset) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_ra_detachrset) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_dmapi_init) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_kdm_ioctl) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_access) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_accessx) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_kfcntl) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR___pfcntl) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_fstatfs64) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_statfs64) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_fstatfs) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_statfs) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_probe) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_cmp_swap) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__validate_pag) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_kgetsidx) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_kgetsid) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_plock) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_upfput) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_usrinfo) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_audit) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_auditobj) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_auditbin) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_auditevents) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_faccessx) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR___fchaclx) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR___chaclx) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_fchacl) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_chacl) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR___fstataclx) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR___stataclx) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_fstatacl) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_statacl) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_setpriv) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_getpriv) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_fstatpriv) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_statpriv) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_fchpriv) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_chpriv) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_i_int2cpu_pal) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_hd_cfg) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_putpmsg) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_putmsg) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_getpmsg) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_getmsg) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_strinfo) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_strreset) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_dupmsg) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__kgrantpt) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_aixgsc) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_smaccept) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_smconnect) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_smlisten) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_smbind) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_smsocket) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_smdetatt) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_smattach) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_smselect) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_smwait) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_smsetthresh) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_smsendbuff) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_smfreebuff) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_smrcvbuff) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_smgetbuff) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_smversion) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_smtcheckinit) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_aio_nwait_timeout) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_kaio_stats) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_aio_cntl) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_listio) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_acancel) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_iosuspend) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_kaio_rdwr) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_aio_nwait) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__posix_iofsync) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__posix_aio_nwait_timeout) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__posix_kaio_stats) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__posix_listio) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__posix_acancel) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__posix_iosuspend) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__posix_kaio_rdwr) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__posix_aio_cntl) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__posix_aio_nwait) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_nfs_cntl) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_nfssvc) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_nfs_getfh) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_exportfs) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_lm_svc) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_pw_config) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_pw_post) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_pw_wait) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_pw_loadavg) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_pw_debug) = __NR_AIX5_UNKNOWN;

/* Extras for AIX 5.3 */
Int VG_(aix5_NR___libc_sbrk) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_thread_waitlock_) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__fp_fpscrx_sc) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_sched_get_priority_max) = __NR_AIX5_UNKNOWN;

/* Extras for AIX 5.3 64-bit mode. */
Int VG_(aix5_NR_kload) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR__fp_fpscrx64_) = __NR_AIX5_UNKNOWN;
Int VG_(aix5_NR_kunload64) = __NR_AIX5_UNKNOWN;

/* We need an extra fake syscall to denote signal handler returns, as
   used in sigframe-ppc{32,64}-aix5.c.  Since we don't know what
   number we can assign to it, monitor the numbers passed to
   VG_(aix5_register_syscall), and set it to 10000+the largest syscall
   nummber seen. */
Int VG_(aix5_NR_FAKE_SIGRETURN) = __NR_AIX5_UNKNOWN;



/* Also make a record of the registered syscalls, so we can print the
   name in bad_before() (syswrap-main.c) if needed.  The obvious
   approach would be to dump them in an XArray, but that requires
   dynamic memory allocation, and syscall registration is done before
   dynamic memory allocation is available.  So just use a fixed size
   array and hope it doesn't fill up. */
#define N_BINDINGS 2000
static Int    bindings_used = 0;
static Int    bindings_sysno[N_BINDINGS];
static UChar* bindings_sysname[N_BINDINGS];

Char* VG_(sysnum_string)(Word sysnum, SizeT n_buf, Char* buf)
{
   VG_(snprintf)(buf, n_buf, "%3ld", sysnum);
   return buf;
}

Char* VG_(sysnum_string_extra)(Word sysnum, SizeT n_buf, Char* buf)
{
   Int i;
   Char* name = "(unknown name)";
   for (i = 0; i < bindings_used; i++) {
      if (bindings_sysno[i] == sysnum) {
         name = bindings_sysname[i];
         break;
      }
   }
   VG_(snprintf)(buf, n_buf, "%3ld (%s)", sysnum, name);
   return buf;
}

static Bool local_streq ( UChar* s1, UChar* s2 ); /* fwds */

Bool VG_(aix5_register_syscall)( Int sysno, UChar* sysname )
{
   /* Establish the FAKE_SIGRETURN number. */
   if (VG_(aix5_NR_FAKE_SIGRETURN) == __NR_AIX5_UNKNOWN)
      VG_(aix5_NR_FAKE_SIGRETURN) = sysno + 10000;
   else
   if (sysno + 10000 > VG_(aix5_NR_FAKE_SIGRETURN))
      VG_(aix5_NR_FAKE_SIGRETURN) = sysno + 10000;

   /* Note the name, just in case bad_before() needs to complain. */
   if (bindings_used < N_BINDINGS) {
      bindings_sysno[bindings_used] = sysno;
      bindings_sysname[bindings_used] = sysname;
      bindings_used++;
   }

   /* Now do the normal name-to-number binding checks. */
#  define XXX(name)                            \
      if (local_streq(sysname, #name)) {       \
         VG_(aix5_NR_##name) = sysno;          \
         return True;                          \
      }
   /* AIX 5.2 */
   XXX(utrchook_sc)
   XXX(thread_create)
   XXX(kfork)
   XXX(kra_fork)
   XXX(execve)
   XXX(ra_execve)
   XXX(_load)
   XXX(__unload)
   XXX(loadbind)
   XXX(__loadx)
   XXX(bindprocessor)
   XXX(trcgent)
   XXX(trcgen)
   XXX(trchk)
   XXX(trchkt)
   XXX(trchkl)
   XXX(trchklt)
   XXX(trchkg)
   XXX(trchkgt)
   XXX(kill)
   XXX(_addcpucosts)
   XXX(mycpu)
   XXX(adjtime)
   XXX(checkpnt_block)
   XXX(_checkpnt_kill)
   XXX(_checkpnt_fail)
   XXX(_checkpnt_commit)
   XXX(_checkpnt_register)
   XXX(_checkpnt)
   XXX(setcrid)
   XXX(getcrid)
   XXX(mkcrid)
   XXX(checkpnt_wait)
   XXX(checkpnt_deliver)
   XXX(gencore)
   XXX(thread_terminate)
   XXX(_exit)
   XXX(kwaitpid64)
   XXX(kwaitpid)
   XXX(yield)
   XXX(getprocs64)
   XXX(getevars)
   XXX(getargs)
   XXX(getthrds64)
   XXX(getthrds)
   XXX(getprocs)
   XXX(sigcleanup)
   XXX(_setpri)
   XXX(_getpri)
   XXX(profil)
   XXX(reboot)
   XXX(appgetrlimit)
   XXX(appsetrlimit)
   XXX(_setpriority)
   XXX(_getpriority)
   XXX(setrlimit64)
   XXX(getrlimit64)
   XXX(appgetrusage)
   XXX(getrusage64)
   XXX(getvtid)
   XXX(getrtid)
   XXX(getrpid)
   XXX(restart_wait)
   XXX(restart)
   XXX(_rmcpucosts)
   XXX(_clock_getcpuclockid)
   XXX(_clock_settime)
   XXX(_clock_gettime)
   XXX(_clock_getres)
   XXX(_timer_settime)
   XXX(_timer_gettime)
   XXX(_timer_getoverrun)
   XXX(_timer_delete)
   XXX(_timer_create)
   XXX(_sigqueue)
   XXX(_sigsuspend)
   XXX(_sigaction)
   XXX(sigprocmask)
   XXX(siglocalmask)
   XXX(count_event_waiters)
   XXX(thread_waitact)
   XXX(thread_waitlock_local)
   XXX(thread_waitlock)
   XXX(thread_wait)
   XXX(thread_unlock)
   XXX(thread_twakeup_unlock)
   XXX(thread_twakeup_event)
   XXX(thread_twakeup)
   XXX(thread_tsleep_event)
   XXX(thread_tsleep_chkpnt)
   XXX(thread_tsleep)
   XXX(thread_post_many)
   XXX(thread_post)
   XXX(ue_proc_unregister)
   XXX(ue_proc_register)
   XXX(kthread_ctl)
   XXX(_thread_setsched)
   XXX(threads_runnable)
   XXX(thread_getregs)
   XXX(thread_terminate_unlock)
   XXX(thread_terminate_ack)
   XXX(thread_setstate_fast)
   XXX(thread_setstate)
   XXX(thread_setmymask_fast)
   XXX(thread_setmystate_fast)
   XXX(thread_setmystate)
   XXX(thread_init)
   XXX(times)
   XXX(_nsleep)
   XXX(reltimerid)
   XXX(appresinc)
   XXX(apprestimer)
   XXX(appresabs)
   XXX(appsettimer)
   XXX(appgettimer)
   XXX(gettimerid)
   XXX(incinterval)
   XXX(absinterval)
   XXX(getinterval)
   XXX(upfget)
   XXX(_wlm_wait)
   XXX(_wlm_post)
   XXX(_wlm_event_init)
   XXX(_wlm_set_tag)
   XXX(_wlm_set)
   XXX(ptrace64)
   XXX(ptracex)
   XXX(ptrace)
   XXX(ksetcontext_sigreturn)
   XXX(ksetcontext)
   XXX(kgetcontext)
   XXX(sigreturn)
   XXX(_wlm_get_bio_stats)
   XXX(splice)
   XXX(rmsock)
   XXX(nrecvmsg)
   XXX(socket_aio_dequeue)
   XXX(getkerninfo)
   XXX(getpeereid)
   XXX(getpeername)
   XXX(ngetpeername)
   XXX(getsockname)
   XXX(ngetsockname)
   XXX(getsockopt)
   XXX(setsockopt)
   XXX(shutdown)
   XXX(recvmsg)
   XXX(recv)
   XXX(nrecvfrom)
   XXX(recvfrom)
   XXX(nsendmsg)
   XXX(sendmsg)
   XXX(send)
   XXX(sendto)
   XXX(socketpair)
   XXX(accept)
   XXX(naccept)
   XXX(listen)
   XXX(bind)
   XXX(socket)
   XXX(connext)
   XXX(setdomainname)
   XXX(getdomainname)
   XXX(sethostname)
   XXX(sethostid)
   XXX(gethostid)
   XXX(gethostname)
   XXX(send_file)
   XXX(_rmlmbcost)
   XXX(__rs_pickmcm)
   XXX(rs_getsystem)
   XXX(rs_getassociativity)
   XXX(rs_setpartition)
   XXX(rs_getpartition)
   XXX(ra_getrset)
   XXX(rs_getinfo)
   XXX(rs_getrad)
   XXX(rs_numrads)
   XXX(__kdb_format_print_rele)
   XXX(__kdb_format_print_init)
   XXX(close)
   XXX(kfsync_range)
   XXX(fsync)
   XXX(kpwrite)
   XXX(kwritev)
   XXX(kwrite)
   XXX(kpread)
   XXX(kreadv)
   XXX(kread)
   XXX(klseek)
   XXX(_lseek)
   XXX(lseek)
   XXX(_setsid)
   XXX(_setpgid)
   XXX(_setpgrp)
   XXX(_getpgrpx)
   XXX(_getpgrp)
   XXX(_getppid)
   XXX(_thread_self)
   XXX(_getpid)
   XXX(kgetpgidx)
   XXX(setuid)
   XXX(setuidx)
   XXX(getuidx)
   XXX(seteuid)
   XXX(setreuid)
   XXX(chdir)
   XXX(fchdir)
   XXX(chroot)
   XXX(fchmod)
   XXX(chmod)
   XXX(chown)
   XXX(lchown)
   XXX(fchown)
   XXX(fchownx)
   XXX(chownx)
   XXX(kfclear)
   XXX(fclear)
   XXX(ffinfo)
   XXX(finfo)
   XXX(fscntl)
   XXX(ktruncate)
   XXX(kftruncate)
   XXX(truncate)
   XXX(ftruncate)
   XXX(getdirent64)
   XXX(getdirent)
   XXX(kioctl32)
   XXX(kioctl)
   XXX(link)
   XXX(klockf)
   XXX(lockf)
   XXX(mkdir)
   XXX(mknod)
   XXX(mntctl)
   XXX(vmount)
   XXX(creat)
   XXX(openx)
   XXX(open)
   XXX(quotactl)
   XXX(rename)
   XXX(rmdir)
   XXX(fstatx)
   XXX(statx)
   XXX(symlink)
   XXX(readlink)
   XXX(syncvfs)
   XXX(sync)
   XXX(umask)
   XXX(uvmount)
   XXX(umount)
   XXX(unameu)
   XXX(unamex)
   XXX(uname)
   XXX(unlink)
   XXX(ustat)
   XXX(utimes)
   XXX(__msgxrcv)
   XXX(__msgrcv)
   XXX(__msgsnd)
   XXX(msgctl)
   XXX(msgget)
   XXX(getgidx)
   XXX(__semop)
   XXX(semget)
   XXX(semctl)
   XXX(shmctl)
   XXX(shmdt)
   XXX(shmat)
   XXX(shmget)
   XXX(ra_shmgetv)
   XXX(ra_shmget)
   XXX(privcheck)
   XXX(disclaim)
   XXX(_sem_destroy_unnamed)
   XXX(_sem_wait)
   XXX(_sem_close)
   XXX(_sem_open)
   XXX(sem_unlink)
   XXX(sem_post)
   XXX(sem_init)
   XXX(sem_getvalue)
   XXX(sem_destroy)
   XXX(_mq_notify)
   XXX(_mq_open)
   XXX(mq_unlink)
   XXX(mq_setattr)
   XXX(mq_send)
   XXX(mq_receive)
   XXX(mq_getattr)
   XXX(mq_close)
   XXX(shm_unlink)
   XXX(shm_open)
   XXX(_poll)
   XXX(_select)
   XXX(sysconfig)
   XXX(sys_parm)
   XXX(loadquery)
   XXX(knlist)
   XXX(brk)
   XXX(fjfs_sys_call)
   XXX(jfs_sys_call)
   XXX(acct)
   XXX(_dr_unregister)
   XXX(_dr_notify)
   XXX(_dr_register)
   XXX(getlparload)
   XXX(dr_reconfig)
   XXX(projctl)
   XXX(sbrk)
   XXX(_sigpending)
   XXX(_pause)
   XXX(thread_kill)
   XXX(sigstack)
   XXX(sigaltstack)
   XXX(appulimit)
   XXX(ras_service)
   XXX(_wlm_class_descr2key)
   XXX(_wlm_get_procinfo)
   XXX(_wlm_get_info)
   XXX(_wlm_getclassname)
   XXX(_wlm_unload_classes)
   XXX(_wlm_load)
   XXX(_wlm_tune)
   XXX(_wlm_assign)
   XXX(_wlm_classify)
   XXX(fp_cpusync)
   XXX(_fp_trapstate_ker)
   XXX(_ewlm_classify_correlator)
   XXX(_arm_stop_transaction)
   XXX(_arm_destroy_application)
   XXX(_arm_stop_application)
   XXX(_arm_generate_correlator)
   XXX(_arm_discard_transaction)
   XXX(_arm_unbind_thread)
   XXX(_arm_bind_thread)
   XXX(_arm_unblock_transaction)
   XXX(_arm_block_transaction)
   XXX(_arm_update_transaction)
   XXX(_arm_register_metric)
   XXX(_arm_report_transaction)
   XXX(_arm_start_transaction)
   XXX(_arm_register_transaction)
   XXX(_arm_start_application)
   XXX(_arm_register_application)
   XXX(_lsarm_getinfo)
   XXX(_ewlm_init)
   XXX(_ewlm_query)
   XXX(ewlm_verify_policy)
   XXX(ewlm_abort_policy)
   XXX(ewlm_commit_policy)
   XXX(ewlm_prepare_policy)
   XXX(ewlm_get_completions)
   XXX(ewlm_get_activedata)
   XXX(ewlm_get_appldata)
   XXX(ewlm_collect_samples)
   XXX(ewlm_disconnect)
   XXX(ewlm_connect)
   XXX(auditlog)
   XXX(auditproc)
   XXX(getgroups)
   XXX(setgid)
   XXX(setgidx)
   XXX(setgroups)
   XXX(frevoke)
   XXX(revoke)
   XXX(__pag_setvalue)
   XXX(__pag_getvalue)
   XXX(__pag_getid)
   XXX(__pag_getname)
   XXX(__pag_setname)
   XXX(kcap_set_proc)
   XXX(kcap_get_proc)
   XXX(pipe)
   XXX(mwakeup)
   XXX(__msleep)
   XXX(kmmap)
   XXX(msem_remove)
   XXX(mincore)
   XXX(madvise)
   XXX(munmap)
   XXX(msync)
   XXX(mprotect)
   XXX(mmap)
   XXX(swapqry)
   XXX(swapon)
   XXX(swapoff)
   XXX(psdanger)
   XXX(vmgetinfo)
   XXX(rs_admregistername)
   XXX(rs_discardname)
   XXX(rs_setnameattr)
   XXX(rs_registername)
   XXX(rs_getnamedrset)
   XXX(rs_getnameattr)
   XXX(rs_getrsetnames)
   XXX(ra_attachrset)
   XXX(ra_detachrset)
   XXX(dmapi_init)
   XXX(kdm_ioctl)
   XXX(access)
   XXX(accessx)
   XXX(kfcntl)
   XXX(__pfcntl)
   XXX(fstatfs64)
   XXX(statfs64)
   XXX(fstatfs)
   XXX(statfs)
   XXX(probe)
   XXX(cmp_swap)
   XXX(_validate_pag)
   XXX(kgetsidx)
   XXX(kgetsid)
   XXX(plock)
   XXX(upfput)
   XXX(usrinfo)
   XXX(audit)
   XXX(auditobj)
   XXX(auditbin)
   XXX(auditevents)
   XXX(faccessx)
   XXX(__fchaclx)
   XXX(__chaclx)
   XXX(fchacl)
   XXX(chacl)
   XXX(__fstataclx)
   XXX(__stataclx)
   XXX(fstatacl)
   XXX(statacl)
   XXX(setpriv)
   XXX(getpriv)
   XXX(fstatpriv)
   XXX(statpriv)
   XXX(fchpriv)
   XXX(chpriv)
   XXX(i_int2cpu_pal)
   XXX(hd_cfg)
   XXX(putpmsg)
   XXX(putmsg)
   XXX(getpmsg)
   XXX(getmsg)
   XXX(strinfo)
   XXX(strreset)
   XXX(dupmsg)
   XXX(_kgrantpt)
   XXX(aixgsc)
   XXX(smaccept)
   XXX(smconnect)
   XXX(smlisten)
   XXX(smbind)
   XXX(smsocket)
   XXX(smdetatt)
   XXX(smattach)
   XXX(smselect)
   XXX(smwait)
   XXX(smsetthresh)
   XXX(smsendbuff)
   XXX(smfreebuff)
   XXX(smrcvbuff)
   XXX(smgetbuff)
   XXX(smversion)
   XXX(smtcheckinit)
   XXX(aio_nwait_timeout)
   XXX(kaio_stats)
   XXX(aio_cntl)
   XXX(listio)
   XXX(acancel)
   XXX(iosuspend)
   XXX(kaio_rdwr)
   XXX(aio_nwait)
   XXX(_posix_iofsync)
   XXX(_posix_aio_nwait_timeout)
   XXX(_posix_kaio_stats)
   XXX(_posix_listio)
   XXX(_posix_acancel)
   XXX(_posix_iosuspend)
   XXX(_posix_kaio_rdwr)
   XXX(_posix_aio_cntl)
   XXX(_posix_aio_nwait)
   XXX(nfs_cntl)
   XXX(nfssvc)
   XXX(nfs_getfh)
   XXX(exportfs)
   XXX(lm_svc)
   XXX(pw_config)
   XXX(pw_post)
   XXX(pw_wait)
   XXX(pw_loadavg)
   XXX(pw_debug)
   /* Extras for AIX 5.3 */
   XXX(__libc_sbrk)
   XXX(thread_waitlock_)
   XXX(_fp_fpscrx_sc)
   XXX(sched_get_priority_max)
   /* Extras for AIX 5.3 64-bit */
   XXX(kload)
   XXX(_fp_fpscrx64_)
   XXX(kunload64)
#  undef XXX
   return False;
}


static Bool local_streq ( UChar* s1, UChar* s2 )
{
   while (True) {
      if (*s1 == 0 && *s2 == 0) return True;
      if (*s1 == 0) return False;
      if (*s2 == 0) return False;
      if (*s1 != *s2) return False;
      s1++; s2++;
   }
}

//---------------------------------------------------------------------------
#elif defined(VGO_darwin)
//---------------------------------------------------------------------------

Char* VG_(sysnum_string)(Word sysnum, SizeT n_buf, Char* buf)
{
   Char* classname = NULL;
   switch (VG_DARWIN_SYSNO_CLASS(sysnum)) {
      case VG_DARWIN_SYSCALL_CLASS_MACH: classname = "mach"; break;
      case VG_DARWIN_SYSCALL_CLASS_UNIX: classname = "unix"; break;
      case VG_DARWIN_SYSCALL_CLASS_MDEP: classname = "mdep"; break;
      case VG_DARWIN_SYSCALL_CLASS_DIAG: classname = "diag"; break;
      default: classname = "UNKNOWN"; break;
   }
   VG_(snprintf)(buf, n_buf, "%s:%3ld",
                             classname, VG_DARWIN_SYSNO_INDEX(sysnum));
   return buf;
}

Char* VG_(sysnum_string_extra)(Word sysnum, SizeT n_buf, Char* buf)
{
   return VG_(sysnum_string)(sysnum, n_buf, buf);
}

//---------------------------------------------------------------------------
#else
//---------------------------------------------------------------------------
#  error Unknown OS
#endif

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
