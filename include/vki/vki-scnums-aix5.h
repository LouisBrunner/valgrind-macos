
/*--------------------------------------------------------------------*/
/*--- System call numbers for ppc32-aix5 and ppc64-aix5.           ---*/
/*---                                            vki-scnums-aix5.h ---*/
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

#ifndef __VKI_SCNUMS_AIX5_H
#define __VKI_SCNUMS_AIX5_H

#if !defined(VGP_ppc32_aix5) && !defined(VGP_ppc64_aix5)
#  error This file should be included in AIX5 builds only.
#endif

// WARNING: note that this file, unlike other vki-scnums-PLATFORM.h files,
// isn't suitable for inclusion in asm files.

//--------------------------------------------------------------
// Syscalls for AIX 5.2 running on ppc32
//--------------------------------------------------------------

/* This is the initial value for a syscall number, when we don't
   know what it is. */
#define __NR_AIX5_UNKNOWN (-1)

/* Vanilla AIX 5.2 ones */

extern Int VG_(aix5_NR_utrchook_sc);
#define __NR_AIX5_utrchook_sc VG_(aix5_NR_utrchook_sc)

extern Int VG_(aix5_NR_thread_create);
#define __NR_AIX5_thread_create VG_(aix5_NR_thread_create)

extern Int VG_(aix5_NR_kfork);
#define __NR_AIX5_kfork VG_(aix5_NR_kfork)

extern Int VG_(aix5_NR_kra_fork);
#define __NR_AIX5_kra_fork VG_(aix5_NR_kra_fork)

extern Int VG_(aix5_NR_execve);
#define __NR_AIX5_execve VG_(aix5_NR_execve)

extern Int VG_(aix5_NR_ra_execve);
#define __NR_AIX5_ra_execve VG_(aix5_NR_ra_execve)

extern Int VG_(aix5_NR__load);
#define __NR_AIX5__load VG_(aix5_NR__load)

extern Int VG_(aix5_NR___unload);
#define __NR_AIX5___unload VG_(aix5_NR___unload)

extern Int VG_(aix5_NR_loadbind);
#define __NR_AIX5_loadbind VG_(aix5_NR_loadbind)

extern Int VG_(aix5_NR___loadx);
#define __NR_AIX5___loadx VG_(aix5_NR___loadx)

extern Int VG_(aix5_NR_bindprocessor);
#define __NR_AIX5_bindprocessor VG_(aix5_NR_bindprocessor)

extern Int VG_(aix5_NR_trcgent);
#define __NR_AIX5_trcgent VG_(aix5_NR_trcgent)

extern Int VG_(aix5_NR_trcgen);
#define __NR_AIX5_trcgen VG_(aix5_NR_trcgen)

extern Int VG_(aix5_NR_trchk);
#define __NR_AIX5_trchk VG_(aix5_NR_trchk)

extern Int VG_(aix5_NR_trchkt);
#define __NR_AIX5_trchkt VG_(aix5_NR_trchkt)

extern Int VG_(aix5_NR_trchkl);
#define __NR_AIX5_trchkl VG_(aix5_NR_trchkl)

extern Int VG_(aix5_NR_trchklt);
#define __NR_AIX5_trchklt VG_(aix5_NR_trchklt)

extern Int VG_(aix5_NR_trchkg);
#define __NR_AIX5_trchkg VG_(aix5_NR_trchkg)

extern Int VG_(aix5_NR_trchkgt);
#define __NR_AIX5_trchkgt VG_(aix5_NR_trchkgt)

extern Int VG_(aix5_NR_kill);
#define __NR_AIX5_kill VG_(aix5_NR_kill)

extern Int VG_(aix5_NR__addcpucosts);
#define __NR_AIX5__addcpucosts VG_(aix5_NR__addcpucosts)

extern Int VG_(aix5_NR_mycpu);
#define __NR_AIX5_mycpu VG_(aix5_NR_mycpu)

extern Int VG_(aix5_NR_adjtime);
#define __NR_AIX5_adjtime VG_(aix5_NR_adjtime)

extern Int VG_(aix5_NR_checkpnt_block);
#define __NR_AIX5_checkpnt_block VG_(aix5_NR_checkpnt_block)

extern Int VG_(aix5_NR__checkpnt_kill);
#define __NR_AIX5__checkpnt_kill VG_(aix5_NR__checkpnt_kill)

extern Int VG_(aix5_NR__checkpnt_fail);
#define __NR_AIX5__checkpnt_fail VG_(aix5_NR__checkpnt_fail)

extern Int VG_(aix5_NR__checkpnt_commit);
#define __NR_AIX5__checkpnt_commit VG_(aix5_NR__checkpnt_commit)

extern Int VG_(aix5_NR__checkpnt_register);
#define __NR_AIX5__checkpnt_register VG_(aix5_NR__checkpnt_register)

extern Int VG_(aix5_NR__checkpnt);
#define __NR_AIX5__checkpnt VG_(aix5_NR__checkpnt)

extern Int VG_(aix5_NR_setcrid);
#define __NR_AIX5_setcrid VG_(aix5_NR_setcrid)

extern Int VG_(aix5_NR_getcrid);
#define __NR_AIX5_getcrid VG_(aix5_NR_getcrid)

extern Int VG_(aix5_NR_mkcrid);
#define __NR_AIX5_mkcrid VG_(aix5_NR_mkcrid)

extern Int VG_(aix5_NR_checkpnt_wait);
#define __NR_AIX5_checkpnt_wait VG_(aix5_NR_checkpnt_wait)

extern Int VG_(aix5_NR_checkpnt_deliver);
#define __NR_AIX5_checkpnt_deliver VG_(aix5_NR_checkpnt_deliver)

extern Int VG_(aix5_NR_gencore);
#define __NR_AIX5_gencore VG_(aix5_NR_gencore)

extern Int VG_(aix5_NR_thread_terminate);
#define __NR_AIX5_thread_terminate VG_(aix5_NR_thread_terminate)

extern Int VG_(aix5_NR__exit);
#define __NR_AIX5__exit VG_(aix5_NR__exit)

extern Int VG_(aix5_NR_kwaitpid64);
#define __NR_AIX5_kwaitpid64 VG_(aix5_NR_kwaitpid64)

extern Int VG_(aix5_NR_kwaitpid);
#define __NR_AIX5_kwaitpid VG_(aix5_NR_kwaitpid)

extern Int VG_(aix5_NR_yield);
#define __NR_AIX5_yield VG_(aix5_NR_yield)

extern Int VG_(aix5_NR_getprocs64);
#define __NR_AIX5_getprocs64 VG_(aix5_NR_getprocs64)

extern Int VG_(aix5_NR_getevars);
#define __NR_AIX5_getevars VG_(aix5_NR_getevars)

extern Int VG_(aix5_NR_getargs);
#define __NR_AIX5_getargs VG_(aix5_NR_getargs)

extern Int VG_(aix5_NR_getthrds64);
#define __NR_AIX5_getthrds64 VG_(aix5_NR_getthrds64)

extern Int VG_(aix5_NR_getthrds);
#define __NR_AIX5_getthrds VG_(aix5_NR_getthrds)

extern Int VG_(aix5_NR_getprocs);
#define __NR_AIX5_getprocs VG_(aix5_NR_getprocs)

extern Int VG_(aix5_NR_sigcleanup);
#define __NR_AIX5_sigcleanup VG_(aix5_NR_sigcleanup)

extern Int VG_(aix5_NR__setpri);
#define __NR_AIX5__setpri VG_(aix5_NR__setpri)

extern Int VG_(aix5_NR__getpri);
#define __NR_AIX5__getpri VG_(aix5_NR__getpri)

extern Int VG_(aix5_NR_profil);
#define __NR_AIX5_profil VG_(aix5_NR_profil)

extern Int VG_(aix5_NR_reboot);
#define __NR_AIX5_reboot VG_(aix5_NR_reboot)

extern Int VG_(aix5_NR_appgetrlimit);
#define __NR_AIX5_appgetrlimit VG_(aix5_NR_appgetrlimit)

extern Int VG_(aix5_NR_appsetrlimit);
#define __NR_AIX5_appsetrlimit VG_(aix5_NR_appsetrlimit)

extern Int VG_(aix5_NR__setpriority);
#define __NR_AIX5__setpriority VG_(aix5_NR__setpriority)

extern Int VG_(aix5_NR__getpriority);
#define __NR_AIX5__getpriority VG_(aix5_NR__getpriority)

extern Int VG_(aix5_NR_setrlimit64);
#define __NR_AIX5_setrlimit64 VG_(aix5_NR_setrlimit64)

extern Int VG_(aix5_NR_getrlimit64);
#define __NR_AIX5_getrlimit64 VG_(aix5_NR_getrlimit64)

extern Int VG_(aix5_NR_appgetrusage);
#define __NR_AIX5_appgetrusage VG_(aix5_NR_appgetrusage)

extern Int VG_(aix5_NR_getrusage64);
#define __NR_AIX5_getrusage64 VG_(aix5_NR_getrusage64)

extern Int VG_(aix5_NR_getvtid);
#define __NR_AIX5_getvtid VG_(aix5_NR_getvtid)

extern Int VG_(aix5_NR_getrtid);
#define __NR_AIX5_getrtid VG_(aix5_NR_getrtid)

extern Int VG_(aix5_NR_getrpid);
#define __NR_AIX5_getrpid VG_(aix5_NR_getrpid)

extern Int VG_(aix5_NR_restart_wait);
#define __NR_AIX5_restart_wait VG_(aix5_NR_restart_wait)

extern Int VG_(aix5_NR_restart);
#define __NR_AIX5_restart VG_(aix5_NR_restart)

extern Int VG_(aix5_NR__rmcpucosts);
#define __NR_AIX5__rmcpucosts VG_(aix5_NR__rmcpucosts)

extern Int VG_(aix5_NR__clock_getcpuclockid);
#define __NR_AIX5__clock_getcpuclockid VG_(aix5_NR__clock_getcpuclockid)

extern Int VG_(aix5_NR__clock_settime);
#define __NR_AIX5__clock_settime VG_(aix5_NR__clock_settime)

extern Int VG_(aix5_NR__clock_gettime);
#define __NR_AIX5__clock_gettime VG_(aix5_NR__clock_gettime)

extern Int VG_(aix5_NR__clock_getres);
#define __NR_AIX5__clock_getres VG_(aix5_NR__clock_getres)

extern Int VG_(aix5_NR__timer_settime);
#define __NR_AIX5__timer_settime VG_(aix5_NR__timer_settime)

extern Int VG_(aix5_NR__timer_gettime);
#define __NR_AIX5__timer_gettime VG_(aix5_NR__timer_gettime)

extern Int VG_(aix5_NR__timer_getoverrun);
#define __NR_AIX5__timer_getoverrun VG_(aix5_NR__timer_getoverrun)

extern Int VG_(aix5_NR__timer_delete);
#define __NR_AIX5__timer_delete VG_(aix5_NR__timer_delete)

extern Int VG_(aix5_NR__timer_create);
#define __NR_AIX5__timer_create VG_(aix5_NR__timer_create)

extern Int VG_(aix5_NR__sigqueue);
#define __NR_AIX5__sigqueue VG_(aix5_NR__sigqueue)

extern Int VG_(aix5_NR__sigsuspend);
#define __NR_AIX5__sigsuspend VG_(aix5_NR__sigsuspend)

extern Int VG_(aix5_NR__sigaction);
#define __NR_AIX5__sigaction VG_(aix5_NR__sigaction)

extern Int VG_(aix5_NR_sigprocmask);
#define __NR_AIX5_sigprocmask VG_(aix5_NR_sigprocmask)

extern Int VG_(aix5_NR_siglocalmask);
#define __NR_AIX5_siglocalmask VG_(aix5_NR_siglocalmask)

extern Int VG_(aix5_NR_count_event_waiters);
#define __NR_AIX5_count_event_waiters VG_(aix5_NR_count_event_waiters)

extern Int VG_(aix5_NR_thread_waitact);
#define __NR_AIX5_thread_waitact VG_(aix5_NR_thread_waitact)

extern Int VG_(aix5_NR_thread_waitlock_local);
#define __NR_AIX5_thread_waitlock_local VG_(aix5_NR_thread_waitlock_local)

extern Int VG_(aix5_NR_thread_waitlock);
#define __NR_AIX5_thread_waitlock VG_(aix5_NR_thread_waitlock)

extern Int VG_(aix5_NR_thread_wait);
#define __NR_AIX5_thread_wait VG_(aix5_NR_thread_wait)

extern Int VG_(aix5_NR_thread_unlock);
#define __NR_AIX5_thread_unlock VG_(aix5_NR_thread_unlock)

extern Int VG_(aix5_NR_thread_twakeup_unlock);
#define __NR_AIX5_thread_twakeup_unlock VG_(aix5_NR_thread_twakeup_unlock)

extern Int VG_(aix5_NR_thread_twakeup_event);
#define __NR_AIX5_thread_twakeup_event VG_(aix5_NR_thread_twakeup_event)

extern Int VG_(aix5_NR_thread_twakeup);
#define __NR_AIX5_thread_twakeup VG_(aix5_NR_thread_twakeup)

extern Int VG_(aix5_NR_thread_tsleep_event);
#define __NR_AIX5_thread_tsleep_event VG_(aix5_NR_thread_tsleep_event)

extern Int VG_(aix5_NR_thread_tsleep_chkpnt);
#define __NR_AIX5_thread_tsleep_chkpnt VG_(aix5_NR_thread_tsleep_chkpnt)

extern Int VG_(aix5_NR_thread_tsleep);
#define __NR_AIX5_thread_tsleep VG_(aix5_NR_thread_tsleep)

extern Int VG_(aix5_NR_thread_post_many);
#define __NR_AIX5_thread_post_many VG_(aix5_NR_thread_post_many)

extern Int VG_(aix5_NR_thread_post);
#define __NR_AIX5_thread_post VG_(aix5_NR_thread_post)

extern Int VG_(aix5_NR_ue_proc_unregister);
#define __NR_AIX5_ue_proc_unregister VG_(aix5_NR_ue_proc_unregister)

extern Int VG_(aix5_NR_ue_proc_register);
#define __NR_AIX5_ue_proc_register VG_(aix5_NR_ue_proc_register)

extern Int VG_(aix5_NR_kthread_ctl);
#define __NR_AIX5_kthread_ctl VG_(aix5_NR_kthread_ctl)

extern Int VG_(aix5_NR__thread_setsched);
#define __NR_AIX5__thread_setsched VG_(aix5_NR__thread_setsched)

extern Int VG_(aix5_NR_threads_runnable);
#define __NR_AIX5_threads_runnable VG_(aix5_NR_threads_runnable)

extern Int VG_(aix5_NR_thread_getregs);
#define __NR_AIX5_thread_getregs VG_(aix5_NR_thread_getregs)

extern Int VG_(aix5_NR_thread_terminate_unlock);
#define __NR_AIX5_thread_terminate_unlock VG_(aix5_NR_thread_terminate_unlock)

extern Int VG_(aix5_NR_thread_terminate_ack);
#define __NR_AIX5_thread_terminate_ack VG_(aix5_NR_thread_terminate_ack)

extern Int VG_(aix5_NR_thread_setstate_fast);
#define __NR_AIX5_thread_setstate_fast VG_(aix5_NR_thread_setstate_fast)

extern Int VG_(aix5_NR_thread_setstate);
#define __NR_AIX5_thread_setstate VG_(aix5_NR_thread_setstate)

extern Int VG_(aix5_NR_thread_setmymask_fast);
#define __NR_AIX5_thread_setmymask_fast VG_(aix5_NR_thread_setmymask_fast)

extern Int VG_(aix5_NR_thread_setmystate_fast);
#define __NR_AIX5_thread_setmystate_fast VG_(aix5_NR_thread_setmystate_fast)

extern Int VG_(aix5_NR_thread_setmystate);
#define __NR_AIX5_thread_setmystate VG_(aix5_NR_thread_setmystate)

extern Int VG_(aix5_NR_thread_init);
#define __NR_AIX5_thread_init VG_(aix5_NR_thread_init)

extern Int VG_(aix5_NR_times);
#define __NR_AIX5_times VG_(aix5_NR_times)

extern Int VG_(aix5_NR__nsleep);
#define __NR_AIX5__nsleep VG_(aix5_NR__nsleep)

extern Int VG_(aix5_NR_reltimerid);
#define __NR_AIX5_reltimerid VG_(aix5_NR_reltimerid)

extern Int VG_(aix5_NR_appresinc);
#define __NR_AIX5_appresinc VG_(aix5_NR_appresinc)

extern Int VG_(aix5_NR_apprestimer);
#define __NR_AIX5_apprestimer VG_(aix5_NR_apprestimer)

extern Int VG_(aix5_NR_appresabs);
#define __NR_AIX5_appresabs VG_(aix5_NR_appresabs)

extern Int VG_(aix5_NR_appsettimer);
#define __NR_AIX5_appsettimer VG_(aix5_NR_appsettimer)

extern Int VG_(aix5_NR_appgettimer);
#define __NR_AIX5_appgettimer VG_(aix5_NR_appgettimer)

extern Int VG_(aix5_NR_gettimerid);
#define __NR_AIX5_gettimerid VG_(aix5_NR_gettimerid)

extern Int VG_(aix5_NR_incinterval);
#define __NR_AIX5_incinterval VG_(aix5_NR_incinterval)

extern Int VG_(aix5_NR_absinterval);
#define __NR_AIX5_absinterval VG_(aix5_NR_absinterval)

extern Int VG_(aix5_NR_getinterval);
#define __NR_AIX5_getinterval VG_(aix5_NR_getinterval)

extern Int VG_(aix5_NR_upfget);
#define __NR_AIX5_upfget VG_(aix5_NR_upfget)

extern Int VG_(aix5_NR__wlm_wait);
#define __NR_AIX5__wlm_wait VG_(aix5_NR__wlm_wait)

extern Int VG_(aix5_NR__wlm_post);
#define __NR_AIX5__wlm_post VG_(aix5_NR__wlm_post)

extern Int VG_(aix5_NR__wlm_event_init);
#define __NR_AIX5__wlm_event_init VG_(aix5_NR__wlm_event_init)

extern Int VG_(aix5_NR__wlm_set_tag);
#define __NR_AIX5__wlm_set_tag VG_(aix5_NR__wlm_set_tag)

extern Int VG_(aix5_NR__wlm_set);
#define __NR_AIX5__wlm_set VG_(aix5_NR__wlm_set)

extern Int VG_(aix5_NR_ptrace64);
#define __NR_AIX5_ptrace64 VG_(aix5_NR_ptrace64)

extern Int VG_(aix5_NR_ptracex);
#define __NR_AIX5_ptracex VG_(aix5_NR_ptracex)

extern Int VG_(aix5_NR_ptrace);
#define __NR_AIX5_ptrace VG_(aix5_NR_ptrace)

extern Int VG_(aix5_NR_ksetcontext_sigreturn);
#define __NR_AIX5_ksetcontext_sigreturn VG_(aix5_NR_ksetcontext_sigreturn)

extern Int VG_(aix5_NR_ksetcontext);
#define __NR_AIX5_ksetcontext VG_(aix5_NR_ksetcontext)

extern Int VG_(aix5_NR_kgetcontext);
#define __NR_AIX5_kgetcontext VG_(aix5_NR_kgetcontext)

extern Int VG_(aix5_NR_sigreturn);
#define __NR_AIX5_sigreturn VG_(aix5_NR_sigreturn)

extern Int VG_(aix5_NR__wlm_get_bio_stats);
#define __NR_AIX5__wlm_get_bio_stats VG_(aix5_NR__wlm_get_bio_stats)

extern Int VG_(aix5_NR_splice);
#define __NR_AIX5_splice VG_(aix5_NR_splice)

extern Int VG_(aix5_NR_rmsock);
#define __NR_AIX5_rmsock VG_(aix5_NR_rmsock)

extern Int VG_(aix5_NR_nrecvmsg);
#define __NR_AIX5_nrecvmsg VG_(aix5_NR_nrecvmsg)

extern Int VG_(aix5_NR_socket_aio_dequeue);
#define __NR_AIX5_socket_aio_dequeue VG_(aix5_NR_socket_aio_dequeue)

extern Int VG_(aix5_NR_getkerninfo);
#define __NR_AIX5_getkerninfo VG_(aix5_NR_getkerninfo)

extern Int VG_(aix5_NR_getpeereid);
#define __NR_AIX5_getpeereid VG_(aix5_NR_getpeereid)

extern Int VG_(aix5_NR_getpeername);
#define __NR_AIX5_getpeername VG_(aix5_NR_getpeername)

extern Int VG_(aix5_NR_ngetpeername);
#define __NR_AIX5_ngetpeername VG_(aix5_NR_ngetpeername)

extern Int VG_(aix5_NR_getsockname);
#define __NR_AIX5_getsockname VG_(aix5_NR_getsockname)

extern Int VG_(aix5_NR_ngetsockname);
#define __NR_AIX5_ngetsockname VG_(aix5_NR_ngetsockname)

extern Int VG_(aix5_NR_getsockopt);
#define __NR_AIX5_getsockopt VG_(aix5_NR_getsockopt)

extern Int VG_(aix5_NR_setsockopt);
#define __NR_AIX5_setsockopt VG_(aix5_NR_setsockopt)

extern Int VG_(aix5_NR_shutdown);
#define __NR_AIX5_shutdown VG_(aix5_NR_shutdown)

extern Int VG_(aix5_NR_recvmsg);
#define __NR_AIX5_recvmsg VG_(aix5_NR_recvmsg)

extern Int VG_(aix5_NR_recv);
#define __NR_AIX5_recv VG_(aix5_NR_recv)

extern Int VG_(aix5_NR_nrecvfrom);
#define __NR_AIX5_nrecvfrom VG_(aix5_NR_nrecvfrom)

extern Int VG_(aix5_NR_recvfrom);
#define __NR_AIX5_recvfrom VG_(aix5_NR_recvfrom)

extern Int VG_(aix5_NR_nsendmsg);
#define __NR_AIX5_nsendmsg VG_(aix5_NR_nsendmsg)

extern Int VG_(aix5_NR_sendmsg);
#define __NR_AIX5_sendmsg VG_(aix5_NR_sendmsg)

extern Int VG_(aix5_NR_send);
#define __NR_AIX5_send VG_(aix5_NR_send)

extern Int VG_(aix5_NR_sendto);
#define __NR_AIX5_sendto VG_(aix5_NR_sendto)

extern Int VG_(aix5_NR_socketpair);
#define __NR_AIX5_socketpair VG_(aix5_NR_socketpair)

extern Int VG_(aix5_NR_accept);
#define __NR_AIX5_accept VG_(aix5_NR_accept)

extern Int VG_(aix5_NR_naccept);
#define __NR_AIX5_naccept VG_(aix5_NR_naccept)

extern Int VG_(aix5_NR_listen);
#define __NR_AIX5_listen VG_(aix5_NR_listen)

extern Int VG_(aix5_NR_bind);
#define __NR_AIX5_bind VG_(aix5_NR_bind)

extern Int VG_(aix5_NR_socket);
#define __NR_AIX5_socket VG_(aix5_NR_socket)

extern Int VG_(aix5_NR_connext);
#define __NR_AIX5_connext VG_(aix5_NR_connext)

extern Int VG_(aix5_NR_setdomainname);
#define __NR_AIX5_setdomainname VG_(aix5_NR_setdomainname)

extern Int VG_(aix5_NR_getdomainname);
#define __NR_AIX5_getdomainname VG_(aix5_NR_getdomainname)

extern Int VG_(aix5_NR_sethostname);
#define __NR_AIX5_sethostname VG_(aix5_NR_sethostname)

extern Int VG_(aix5_NR_sethostid);
#define __NR_AIX5_sethostid VG_(aix5_NR_sethostid)

extern Int VG_(aix5_NR_gethostid);
#define __NR_AIX5_gethostid VG_(aix5_NR_gethostid)

extern Int VG_(aix5_NR_gethostname);
#define __NR_AIX5_gethostname VG_(aix5_NR_gethostname)

extern Int VG_(aix5_NR_send_file);
#define __NR_AIX5_send_file VG_(aix5_NR_send_file)

extern Int VG_(aix5_NR__rmlmbcost);
#define __NR_AIX5__rmlmbcost VG_(aix5_NR__rmlmbcost)

extern Int VG_(aix5_NR___rs_pickmcm);
#define __NR_AIX5___rs_pickmcm VG_(aix5_NR___rs_pickmcm)

extern Int VG_(aix5_NR_rs_getsystem);
#define __NR_AIX5_rs_getsystem VG_(aix5_NR_rs_getsystem)

extern Int VG_(aix5_NR_rs_getassociativity);
#define __NR_AIX5_rs_getassociativity VG_(aix5_NR_rs_getassociativity)

extern Int VG_(aix5_NR_rs_setpartition);
#define __NR_AIX5_rs_setpartition VG_(aix5_NR_rs_setpartition)

extern Int VG_(aix5_NR_rs_getpartition);
#define __NR_AIX5_rs_getpartition VG_(aix5_NR_rs_getpartition)

extern Int VG_(aix5_NR_ra_getrset);
#define __NR_AIX5_ra_getrset VG_(aix5_NR_ra_getrset)

extern Int VG_(aix5_NR_rs_getinfo);
#define __NR_AIX5_rs_getinfo VG_(aix5_NR_rs_getinfo)

extern Int VG_(aix5_NR_rs_getrad);
#define __NR_AIX5_rs_getrad VG_(aix5_NR_rs_getrad)

extern Int VG_(aix5_NR_rs_numrads);
#define __NR_AIX5_rs_numrads VG_(aix5_NR_rs_numrads)

extern Int VG_(aix5_NR___kdb_format_print_rele);
#define __NR_AIX5___kdb_format_print_rele VG_(aix5_NR___kdb_format_print_rele)

extern Int VG_(aix5_NR___kdb_format_print_init);
#define __NR_AIX5___kdb_format_print_init VG_(aix5_NR___kdb_format_print_init)

extern Int VG_(aix5_NR_close);
#define __NR_AIX5_close VG_(aix5_NR_close)

extern Int VG_(aix5_NR_kfsync_range);
#define __NR_AIX5_kfsync_range VG_(aix5_NR_kfsync_range)

extern Int VG_(aix5_NR_fsync);
#define __NR_AIX5_fsync VG_(aix5_NR_fsync)

extern Int VG_(aix5_NR_kpwrite);
#define __NR_AIX5_kpwrite VG_(aix5_NR_kpwrite)

extern Int VG_(aix5_NR_kwritev);
#define __NR_AIX5_kwritev VG_(aix5_NR_kwritev)

extern Int VG_(aix5_NR_kwrite);
#define __NR_AIX5_kwrite VG_(aix5_NR_kwrite)

extern Int VG_(aix5_NR_kpread);
#define __NR_AIX5_kpread VG_(aix5_NR_kpread)

extern Int VG_(aix5_NR_kreadv);
#define __NR_AIX5_kreadv VG_(aix5_NR_kreadv)

extern Int VG_(aix5_NR_kread);
#define __NR_AIX5_kread VG_(aix5_NR_kread)

extern Int VG_(aix5_NR_klseek);
#define __NR_AIX5_klseek VG_(aix5_NR_klseek)

extern Int VG_(aix5_NR__lseek);
#define __NR_AIX5__lseek VG_(aix5_NR__lseek)

extern Int VG_(aix5_NR_lseek);
#define __NR_AIX5_lseek VG_(aix5_NR_lseek)

extern Int VG_(aix5_NR__setsid);
#define __NR_AIX5__setsid VG_(aix5_NR__setsid)

extern Int VG_(aix5_NR__setpgid);
#define __NR_AIX5__setpgid VG_(aix5_NR__setpgid)

extern Int VG_(aix5_NR__setpgrp);
#define __NR_AIX5__setpgrp VG_(aix5_NR__setpgrp)

extern Int VG_(aix5_NR__getpgrpx);
#define __NR_AIX5__getpgrpx VG_(aix5_NR__getpgrpx)

extern Int VG_(aix5_NR__getpgrp);
#define __NR_AIX5__getpgrp VG_(aix5_NR__getpgrp)

extern Int VG_(aix5_NR__getppid);
#define __NR_AIX5__getppid VG_(aix5_NR__getppid)

extern Int VG_(aix5_NR__thread_self);
#define __NR_AIX5__thread_self VG_(aix5_NR__thread_self)

extern Int VG_(aix5_NR__getpid);
#define __NR_AIX5__getpid VG_(aix5_NR__getpid)

extern Int VG_(aix5_NR_kgetpgidx);
#define __NR_AIX5_kgetpgidx VG_(aix5_NR_kgetpgidx)

extern Int VG_(aix5_NR_setuid);
#define __NR_AIX5_setuid VG_(aix5_NR_setuid)

extern Int VG_(aix5_NR_setuidx);
#define __NR_AIX5_setuidx VG_(aix5_NR_setuidx)

extern Int VG_(aix5_NR_getuidx);
#define __NR_AIX5_getuidx VG_(aix5_NR_getuidx)

extern Int VG_(aix5_NR_seteuid);
#define __NR_AIX5_seteuid VG_(aix5_NR_seteuid)

extern Int VG_(aix5_NR_setreuid);
#define __NR_AIX5_setreuid VG_(aix5_NR_setreuid)

extern Int VG_(aix5_NR_chdir);
#define __NR_AIX5_chdir VG_(aix5_NR_chdir)

extern Int VG_(aix5_NR_fchdir);
#define __NR_AIX5_fchdir VG_(aix5_NR_fchdir)

extern Int VG_(aix5_NR_chroot);
#define __NR_AIX5_chroot VG_(aix5_NR_chroot)

extern Int VG_(aix5_NR_fchmod);
#define __NR_AIX5_fchmod VG_(aix5_NR_fchmod)

extern Int VG_(aix5_NR_chmod);
#define __NR_AIX5_chmod VG_(aix5_NR_chmod)

extern Int VG_(aix5_NR_chown);
#define __NR_AIX5_chown VG_(aix5_NR_chown)

extern Int VG_(aix5_NR_lchown);
#define __NR_AIX5_lchown VG_(aix5_NR_lchown)

extern Int VG_(aix5_NR_fchown);
#define __NR_AIX5_fchown VG_(aix5_NR_fchown)

extern Int VG_(aix5_NR_fchownx);
#define __NR_AIX5_fchownx VG_(aix5_NR_fchownx)

extern Int VG_(aix5_NR_chownx);
#define __NR_AIX5_chownx VG_(aix5_NR_chownx)

extern Int VG_(aix5_NR_kfclear);
#define __NR_AIX5_kfclear VG_(aix5_NR_kfclear)

extern Int VG_(aix5_NR_fclear);
#define __NR_AIX5_fclear VG_(aix5_NR_fclear)

extern Int VG_(aix5_NR_ffinfo);
#define __NR_AIX5_ffinfo VG_(aix5_NR_ffinfo)

extern Int VG_(aix5_NR_finfo);
#define __NR_AIX5_finfo VG_(aix5_NR_finfo)

extern Int VG_(aix5_NR_fscntl);
#define __NR_AIX5_fscntl VG_(aix5_NR_fscntl)

extern Int VG_(aix5_NR_ktruncate);
#define __NR_AIX5_ktruncate VG_(aix5_NR_ktruncate)

extern Int VG_(aix5_NR_kftruncate);
#define __NR_AIX5_kftruncate VG_(aix5_NR_kftruncate)

extern Int VG_(aix5_NR_truncate);
#define __NR_AIX5_truncate VG_(aix5_NR_truncate)

extern Int VG_(aix5_NR_ftruncate);
#define __NR_AIX5_ftruncate VG_(aix5_NR_ftruncate)

extern Int VG_(aix5_NR_getdirent64);
#define __NR_AIX5_getdirent64 VG_(aix5_NR_getdirent64)

extern Int VG_(aix5_NR_getdirent);
#define __NR_AIX5_getdirent VG_(aix5_NR_getdirent)

extern Int VG_(aix5_NR_kioctl32);
#define __NR_AIX5_kioctl32 VG_(aix5_NR_kioctl32)

extern Int VG_(aix5_NR_kioctl);
#define __NR_AIX5_kioctl VG_(aix5_NR_kioctl)

extern Int VG_(aix5_NR_link);
#define __NR_AIX5_link VG_(aix5_NR_link)

extern Int VG_(aix5_NR_klockf);
#define __NR_AIX5_klockf VG_(aix5_NR_klockf)

extern Int VG_(aix5_NR_lockf);
#define __NR_AIX5_lockf VG_(aix5_NR_lockf)

extern Int VG_(aix5_NR_mkdir);
#define __NR_AIX5_mkdir VG_(aix5_NR_mkdir)

extern Int VG_(aix5_NR_mknod);
#define __NR_AIX5_mknod VG_(aix5_NR_mknod)

extern Int VG_(aix5_NR_mntctl);
#define __NR_AIX5_mntctl VG_(aix5_NR_mntctl)

extern Int VG_(aix5_NR_vmount);
#define __NR_AIX5_vmount VG_(aix5_NR_vmount)

extern Int VG_(aix5_NR_creat);
#define __NR_AIX5_creat VG_(aix5_NR_creat)

extern Int VG_(aix5_NR_openx);
#define __NR_AIX5_openx VG_(aix5_NR_openx)

extern Int VG_(aix5_NR_open);
#define __NR_AIX5_open VG_(aix5_NR_open)

extern Int VG_(aix5_NR_quotactl);
#define __NR_AIX5_quotactl VG_(aix5_NR_quotactl)

extern Int VG_(aix5_NR_rename);
#define __NR_AIX5_rename VG_(aix5_NR_rename)

extern Int VG_(aix5_NR_rmdir);
#define __NR_AIX5_rmdir VG_(aix5_NR_rmdir)

extern Int VG_(aix5_NR_fstatx);
#define __NR_AIX5_fstatx VG_(aix5_NR_fstatx)

extern Int VG_(aix5_NR_statx);
#define __NR_AIX5_statx VG_(aix5_NR_statx)

extern Int VG_(aix5_NR_symlink);
#define __NR_AIX5_symlink VG_(aix5_NR_symlink)

extern Int VG_(aix5_NR_readlink);
#define __NR_AIX5_readlink VG_(aix5_NR_readlink)

extern Int VG_(aix5_NR_syncvfs);
#define __NR_AIX5_syncvfs VG_(aix5_NR_syncvfs)

extern Int VG_(aix5_NR_sync);
#define __NR_AIX5_sync VG_(aix5_NR_sync)

extern Int VG_(aix5_NR_umask);
#define __NR_AIX5_umask VG_(aix5_NR_umask)

extern Int VG_(aix5_NR_uvmount);
#define __NR_AIX5_uvmount VG_(aix5_NR_uvmount)

extern Int VG_(aix5_NR_umount);
#define __NR_AIX5_umount VG_(aix5_NR_umount)

extern Int VG_(aix5_NR_unameu);
#define __NR_AIX5_unameu VG_(aix5_NR_unameu)

extern Int VG_(aix5_NR_unamex);
#define __NR_AIX5_unamex VG_(aix5_NR_unamex)

extern Int VG_(aix5_NR_uname);
#define __NR_AIX5_uname VG_(aix5_NR_uname)

extern Int VG_(aix5_NR_unlink);
#define __NR_AIX5_unlink VG_(aix5_NR_unlink)

extern Int VG_(aix5_NR_ustat);
#define __NR_AIX5_ustat VG_(aix5_NR_ustat)

extern Int VG_(aix5_NR_utimes);
#define __NR_AIX5_utimes VG_(aix5_NR_utimes)

extern Int VG_(aix5_NR___msgxrcv);
#define __NR_AIX5___msgxrcv VG_(aix5_NR___msgxrcv)

extern Int VG_(aix5_NR___msgrcv);
#define __NR_AIX5___msgrcv VG_(aix5_NR___msgrcv)

extern Int VG_(aix5_NR___msgsnd);
#define __NR_AIX5___msgsnd VG_(aix5_NR___msgsnd)

extern Int VG_(aix5_NR_msgctl);
#define __NR_AIX5_msgctl VG_(aix5_NR_msgctl)

extern Int VG_(aix5_NR_msgget);
#define __NR_AIX5_msgget VG_(aix5_NR_msgget)

extern Int VG_(aix5_NR_getgidx);
#define __NR_AIX5_getgidx VG_(aix5_NR_getgidx)

extern Int VG_(aix5_NR___semop);
#define __NR_AIX5___semop VG_(aix5_NR___semop)

extern Int VG_(aix5_NR_semget);
#define __NR_AIX5_semget VG_(aix5_NR_semget)

extern Int VG_(aix5_NR_semctl);
#define __NR_AIX5_semctl VG_(aix5_NR_semctl)

extern Int VG_(aix5_NR_shmctl);
#define __NR_AIX5_shmctl VG_(aix5_NR_shmctl)

extern Int VG_(aix5_NR_shmdt);
#define __NR_AIX5_shmdt VG_(aix5_NR_shmdt)

extern Int VG_(aix5_NR_shmat);
#define __NR_AIX5_shmat VG_(aix5_NR_shmat)

extern Int VG_(aix5_NR_shmget);
#define __NR_AIX5_shmget VG_(aix5_NR_shmget)

extern Int VG_(aix5_NR_ra_shmgetv);
#define __NR_AIX5_ra_shmgetv VG_(aix5_NR_ra_shmgetv)

extern Int VG_(aix5_NR_ra_shmget);
#define __NR_AIX5_ra_shmget VG_(aix5_NR_ra_shmget)

extern Int VG_(aix5_NR_privcheck);
#define __NR_AIX5_privcheck VG_(aix5_NR_privcheck)

extern Int VG_(aix5_NR_disclaim);
#define __NR_AIX5_disclaim VG_(aix5_NR_disclaim)

extern Int VG_(aix5_NR__sem_destroy_unnamed);
#define __NR_AIX5__sem_destroy_unnamed VG_(aix5_NR__sem_destroy_unnamed)

extern Int VG_(aix5_NR__sem_wait);
#define __NR_AIX5__sem_wait VG_(aix5_NR__sem_wait)

extern Int VG_(aix5_NR__sem_close);
#define __NR_AIX5__sem_close VG_(aix5_NR__sem_close)

extern Int VG_(aix5_NR__sem_open);
#define __NR_AIX5__sem_open VG_(aix5_NR__sem_open)

extern Int VG_(aix5_NR_sem_unlink);
#define __NR_AIX5_sem_unlink VG_(aix5_NR_sem_unlink)

extern Int VG_(aix5_NR_sem_post);
#define __NR_AIX5_sem_post VG_(aix5_NR_sem_post)

extern Int VG_(aix5_NR_sem_init);
#define __NR_AIX5_sem_init VG_(aix5_NR_sem_init)

extern Int VG_(aix5_NR_sem_getvalue);
#define __NR_AIX5_sem_getvalue VG_(aix5_NR_sem_getvalue)

extern Int VG_(aix5_NR_sem_destroy);
#define __NR_AIX5_sem_destroy VG_(aix5_NR_sem_destroy)

extern Int VG_(aix5_NR__mq_notify);
#define __NR_AIX5__mq_notify VG_(aix5_NR__mq_notify)

extern Int VG_(aix5_NR__mq_open);
#define __NR_AIX5__mq_open VG_(aix5_NR__mq_open)

extern Int VG_(aix5_NR_mq_unlink);
#define __NR_AIX5_mq_unlink VG_(aix5_NR_mq_unlink)

extern Int VG_(aix5_NR_mq_setattr);
#define __NR_AIX5_mq_setattr VG_(aix5_NR_mq_setattr)

extern Int VG_(aix5_NR_mq_send);
#define __NR_AIX5_mq_send VG_(aix5_NR_mq_send)

extern Int VG_(aix5_NR_mq_receive);
#define __NR_AIX5_mq_receive VG_(aix5_NR_mq_receive)

extern Int VG_(aix5_NR_mq_getattr);
#define __NR_AIX5_mq_getattr VG_(aix5_NR_mq_getattr)

extern Int VG_(aix5_NR_mq_close);
#define __NR_AIX5_mq_close VG_(aix5_NR_mq_close)

extern Int VG_(aix5_NR_shm_unlink);
#define __NR_AIX5_shm_unlink VG_(aix5_NR_shm_unlink)

extern Int VG_(aix5_NR_shm_open);
#define __NR_AIX5_shm_open VG_(aix5_NR_shm_open)

extern Int VG_(aix5_NR__poll);
#define __NR_AIX5__poll VG_(aix5_NR__poll)

extern Int VG_(aix5_NR__select);
#define __NR_AIX5__select VG_(aix5_NR__select)

extern Int VG_(aix5_NR_sysconfig);
#define __NR_AIX5_sysconfig VG_(aix5_NR_sysconfig)

extern Int VG_(aix5_NR_sys_parm);
#define __NR_AIX5_sys_parm VG_(aix5_NR_sys_parm)

extern Int VG_(aix5_NR_loadquery);
#define __NR_AIX5_loadquery VG_(aix5_NR_loadquery)

extern Int VG_(aix5_NR_knlist);
#define __NR_AIX5_knlist VG_(aix5_NR_knlist)

extern Int VG_(aix5_NR_brk);
#define __NR_AIX5_brk VG_(aix5_NR_brk)

extern Int VG_(aix5_NR_fjfs_sys_call);
#define __NR_AIX5_fjfs_sys_call VG_(aix5_NR_fjfs_sys_call)

extern Int VG_(aix5_NR_jfs_sys_call);
#define __NR_AIX5_jfs_sys_call VG_(aix5_NR_jfs_sys_call)

extern Int VG_(aix5_NR_acct);
#define __NR_AIX5_acct VG_(aix5_NR_acct)

extern Int VG_(aix5_NR__dr_unregister);
#define __NR_AIX5__dr_unregister VG_(aix5_NR__dr_unregister)

extern Int VG_(aix5_NR__dr_notify);
#define __NR_AIX5__dr_notify VG_(aix5_NR__dr_notify)

extern Int VG_(aix5_NR__dr_register);
#define __NR_AIX5__dr_register VG_(aix5_NR__dr_register)

extern Int VG_(aix5_NR_getlparload);
#define __NR_AIX5_getlparload VG_(aix5_NR_getlparload)

extern Int VG_(aix5_NR_dr_reconfig);
#define __NR_AIX5_dr_reconfig VG_(aix5_NR_dr_reconfig)

extern Int VG_(aix5_NR_projctl);
#define __NR_AIX5_projctl VG_(aix5_NR_projctl)

extern Int VG_(aix5_NR_sbrk);
#define __NR_AIX5_sbrk VG_(aix5_NR_sbrk)

extern Int VG_(aix5_NR__sigpending);
#define __NR_AIX5__sigpending VG_(aix5_NR__sigpending)

extern Int VG_(aix5_NR__pause);
#define __NR_AIX5__pause VG_(aix5_NR__pause)

extern Int VG_(aix5_NR_thread_kill);
#define __NR_AIX5_thread_kill VG_(aix5_NR_thread_kill)

extern Int VG_(aix5_NR_sigstack);
#define __NR_AIX5_sigstack VG_(aix5_NR_sigstack)

extern Int VG_(aix5_NR_sigaltstack);
#define __NR_AIX5_sigaltstack VG_(aix5_NR_sigaltstack)

extern Int VG_(aix5_NR_appulimit);
#define __NR_AIX5_appulimit VG_(aix5_NR_appulimit)

extern Int VG_(aix5_NR_ras_service);
#define __NR_AIX5_ras_service VG_(aix5_NR_ras_service)

extern Int VG_(aix5_NR__wlm_class_descr2key);
#define __NR_AIX5__wlm_class_descr2key VG_(aix5_NR__wlm_class_descr2key)

extern Int VG_(aix5_NR__wlm_get_procinfo);
#define __NR_AIX5__wlm_get_procinfo VG_(aix5_NR__wlm_get_procinfo)

extern Int VG_(aix5_NR__wlm_get_info);
#define __NR_AIX5__wlm_get_info VG_(aix5_NR__wlm_get_info)

extern Int VG_(aix5_NR__wlm_getclassname);
#define __NR_AIX5__wlm_getclassname VG_(aix5_NR__wlm_getclassname)

extern Int VG_(aix5_NR__wlm_unload_classes);
#define __NR_AIX5__wlm_unload_classes VG_(aix5_NR__wlm_unload_classes)

extern Int VG_(aix5_NR__wlm_load);
#define __NR_AIX5__wlm_load VG_(aix5_NR__wlm_load)

extern Int VG_(aix5_NR__wlm_tune);
#define __NR_AIX5__wlm_tune VG_(aix5_NR__wlm_tune)

extern Int VG_(aix5_NR__wlm_assign);
#define __NR_AIX5__wlm_assign VG_(aix5_NR__wlm_assign)

extern Int VG_(aix5_NR__wlm_classify);
#define __NR_AIX5__wlm_classify VG_(aix5_NR__wlm_classify)

extern Int VG_(aix5_NR_fp_cpusync);
#define __NR_AIX5_fp_cpusync VG_(aix5_NR_fp_cpusync)

extern Int VG_(aix5_NR__fp_trapstate_ker);
#define __NR_AIX5__fp_trapstate_ker VG_(aix5_NR__fp_trapstate_ker)

extern Int VG_(aix5_NR__ewlm_classify_correlator);
#define __NR_AIX5__ewlm_classify_correlator VG_(aix5_NR__ewlm_classify_correlator)

extern Int VG_(aix5_NR__arm_stop_transaction);
#define __NR_AIX5__arm_stop_transaction VG_(aix5_NR__arm_stop_transaction)

extern Int VG_(aix5_NR__arm_destroy_application);
#define __NR_AIX5__arm_destroy_application VG_(aix5_NR__arm_destroy_application)

extern Int VG_(aix5_NR__arm_stop_application);
#define __NR_AIX5__arm_stop_application VG_(aix5_NR__arm_stop_application)

extern Int VG_(aix5_NR__arm_generate_correlator);
#define __NR_AIX5__arm_generate_correlator VG_(aix5_NR__arm_generate_correlator)

extern Int VG_(aix5_NR__arm_discard_transaction);
#define __NR_AIX5__arm_discard_transaction VG_(aix5_NR__arm_discard_transaction)

extern Int VG_(aix5_NR__arm_unbind_thread);
#define __NR_AIX5__arm_unbind_thread VG_(aix5_NR__arm_unbind_thread)

extern Int VG_(aix5_NR__arm_bind_thread);
#define __NR_AIX5__arm_bind_thread VG_(aix5_NR__arm_bind_thread)

extern Int VG_(aix5_NR__arm_unblock_transaction);
#define __NR_AIX5__arm_unblock_transaction VG_(aix5_NR__arm_unblock_transaction)

extern Int VG_(aix5_NR__arm_block_transaction);
#define __NR_AIX5__arm_block_transaction VG_(aix5_NR__arm_block_transaction)

extern Int VG_(aix5_NR__arm_update_transaction);
#define __NR_AIX5__arm_update_transaction VG_(aix5_NR__arm_update_transaction)

extern Int VG_(aix5_NR__arm_register_metric);
#define __NR_AIX5__arm_register_metric VG_(aix5_NR__arm_register_metric)

extern Int VG_(aix5_NR__arm_report_transaction);
#define __NR_AIX5__arm_report_transaction VG_(aix5_NR__arm_report_transaction)

extern Int VG_(aix5_NR__arm_start_transaction);
#define __NR_AIX5__arm_start_transaction VG_(aix5_NR__arm_start_transaction)

extern Int VG_(aix5_NR__arm_register_transaction);
#define __NR_AIX5__arm_register_transaction VG_(aix5_NR__arm_register_transaction)

extern Int VG_(aix5_NR__arm_start_application);
#define __NR_AIX5__arm_start_application VG_(aix5_NR__arm_start_application)

extern Int VG_(aix5_NR__arm_register_application);
#define __NR_AIX5__arm_register_application VG_(aix5_NR__arm_register_application)

extern Int VG_(aix5_NR__lsarm_getinfo);
#define __NR_AIX5__lsarm_getinfo VG_(aix5_NR__lsarm_getinfo)

extern Int VG_(aix5_NR__ewlm_init);
#define __NR_AIX5__ewlm_init VG_(aix5_NR__ewlm_init)

extern Int VG_(aix5_NR__ewlm_query);
#define __NR_AIX5__ewlm_query VG_(aix5_NR__ewlm_query)

extern Int VG_(aix5_NR_ewlm_verify_policy);
#define __NR_AIX5_ewlm_verify_policy VG_(aix5_NR_ewlm_verify_policy)

extern Int VG_(aix5_NR_ewlm_abort_policy);
#define __NR_AIX5_ewlm_abort_policy VG_(aix5_NR_ewlm_abort_policy)

extern Int VG_(aix5_NR_ewlm_commit_policy);
#define __NR_AIX5_ewlm_commit_policy VG_(aix5_NR_ewlm_commit_policy)

extern Int VG_(aix5_NR_ewlm_prepare_policy);
#define __NR_AIX5_ewlm_prepare_policy VG_(aix5_NR_ewlm_prepare_policy)

extern Int VG_(aix5_NR_ewlm_get_completions);
#define __NR_AIX5_ewlm_get_completions VG_(aix5_NR_ewlm_get_completions)

extern Int VG_(aix5_NR_ewlm_get_activedata);
#define __NR_AIX5_ewlm_get_activedata VG_(aix5_NR_ewlm_get_activedata)

extern Int VG_(aix5_NR_ewlm_get_appldata);
#define __NR_AIX5_ewlm_get_appldata VG_(aix5_NR_ewlm_get_appldata)

extern Int VG_(aix5_NR_ewlm_collect_samples);
#define __NR_AIX5_ewlm_collect_samples VG_(aix5_NR_ewlm_collect_samples)

extern Int VG_(aix5_NR_ewlm_disconnect);
#define __NR_AIX5_ewlm_disconnect VG_(aix5_NR_ewlm_disconnect)

extern Int VG_(aix5_NR_ewlm_connect);
#define __NR_AIX5_ewlm_connect VG_(aix5_NR_ewlm_connect)

extern Int VG_(aix5_NR_auditlog);
#define __NR_AIX5_auditlog VG_(aix5_NR_auditlog)

extern Int VG_(aix5_NR_auditproc);
#define __NR_AIX5_auditproc VG_(aix5_NR_auditproc)

extern Int VG_(aix5_NR_getgroups);
#define __NR_AIX5_getgroups VG_(aix5_NR_getgroups)

extern Int VG_(aix5_NR_setgid);
#define __NR_AIX5_setgid VG_(aix5_NR_setgid)

extern Int VG_(aix5_NR_setgidx);
#define __NR_AIX5_setgidx VG_(aix5_NR_setgidx)

extern Int VG_(aix5_NR_setgroups);
#define __NR_AIX5_setgroups VG_(aix5_NR_setgroups)

extern Int VG_(aix5_NR_frevoke);
#define __NR_AIX5_frevoke VG_(aix5_NR_frevoke)

extern Int VG_(aix5_NR_revoke);
#define __NR_AIX5_revoke VG_(aix5_NR_revoke)

extern Int VG_(aix5_NR___pag_setvalue);
#define __NR_AIX5___pag_setvalue VG_(aix5_NR___pag_setvalue)

extern Int VG_(aix5_NR___pag_getvalue);
#define __NR_AIX5___pag_getvalue VG_(aix5_NR___pag_getvalue)

extern Int VG_(aix5_NR___pag_getid);
#define __NR_AIX5___pag_getid VG_(aix5_NR___pag_getid)

extern Int VG_(aix5_NR___pag_getname);
#define __NR_AIX5___pag_getname VG_(aix5_NR___pag_getname)

extern Int VG_(aix5_NR___pag_setname);
#define __NR_AIX5___pag_setname VG_(aix5_NR___pag_setname)

extern Int VG_(aix5_NR_kcap_set_proc);
#define __NR_AIX5_kcap_set_proc VG_(aix5_NR_kcap_set_proc)

extern Int VG_(aix5_NR_kcap_get_proc);
#define __NR_AIX5_kcap_get_proc VG_(aix5_NR_kcap_get_proc)

extern Int VG_(aix5_NR_pipe);
#define __NR_AIX5_pipe VG_(aix5_NR_pipe)

extern Int VG_(aix5_NR_mwakeup);
#define __NR_AIX5_mwakeup VG_(aix5_NR_mwakeup)

extern Int VG_(aix5_NR___msleep);
#define __NR_AIX5___msleep VG_(aix5_NR___msleep)

extern Int VG_(aix5_NR_kmmap);
#define __NR_AIX5_kmmap VG_(aix5_NR_kmmap)

extern Int VG_(aix5_NR_msem_remove);
#define __NR_AIX5_msem_remove VG_(aix5_NR_msem_remove)

extern Int VG_(aix5_NR_mincore);
#define __NR_AIX5_mincore VG_(aix5_NR_mincore)

extern Int VG_(aix5_NR_madvise);
#define __NR_AIX5_madvise VG_(aix5_NR_madvise)

extern Int VG_(aix5_NR_munmap);
#define __NR_AIX5_munmap VG_(aix5_NR_munmap)

extern Int VG_(aix5_NR_msync);
#define __NR_AIX5_msync VG_(aix5_NR_msync)

extern Int VG_(aix5_NR_mprotect);
#define __NR_AIX5_mprotect VG_(aix5_NR_mprotect)

extern Int VG_(aix5_NR_mmap);
#define __NR_AIX5_mmap VG_(aix5_NR_mmap)

extern Int VG_(aix5_NR_swapqry);
#define __NR_AIX5_swapqry VG_(aix5_NR_swapqry)

extern Int VG_(aix5_NR_swapon);
#define __NR_AIX5_swapon VG_(aix5_NR_swapon)

extern Int VG_(aix5_NR_swapoff);
#define __NR_AIX5_swapoff VG_(aix5_NR_swapoff)

extern Int VG_(aix5_NR_psdanger);
#define __NR_AIX5_psdanger VG_(aix5_NR_psdanger)

extern Int VG_(aix5_NR_vmgetinfo);
#define __NR_AIX5_vmgetinfo VG_(aix5_NR_vmgetinfo)

extern Int VG_(aix5_NR_rs_admregistername);
#define __NR_AIX5_rs_admregistername VG_(aix5_NR_rs_admregistername)

extern Int VG_(aix5_NR_rs_discardname);
#define __NR_AIX5_rs_discardname VG_(aix5_NR_rs_discardname)

extern Int VG_(aix5_NR_rs_setnameattr);
#define __NR_AIX5_rs_setnameattr VG_(aix5_NR_rs_setnameattr)

extern Int VG_(aix5_NR_rs_registername);
#define __NR_AIX5_rs_registername VG_(aix5_NR_rs_registername)

extern Int VG_(aix5_NR_rs_getnamedrset);
#define __NR_AIX5_rs_getnamedrset VG_(aix5_NR_rs_getnamedrset)

extern Int VG_(aix5_NR_rs_getnameattr);
#define __NR_AIX5_rs_getnameattr VG_(aix5_NR_rs_getnameattr)

extern Int VG_(aix5_NR_rs_getrsetnames);
#define __NR_AIX5_rs_getrsetnames VG_(aix5_NR_rs_getrsetnames)

extern Int VG_(aix5_NR_ra_attachrset);
#define __NR_AIX5_ra_attachrset VG_(aix5_NR_ra_attachrset)

extern Int VG_(aix5_NR_ra_detachrset);
#define __NR_AIX5_ra_detachrset VG_(aix5_NR_ra_detachrset)

extern Int VG_(aix5_NR_dmapi_init);
#define __NR_AIX5_dmapi_init VG_(aix5_NR_dmapi_init)

extern Int VG_(aix5_NR_kdm_ioctl);
#define __NR_AIX5_kdm_ioctl VG_(aix5_NR_kdm_ioctl)

extern Int VG_(aix5_NR_access);
#define __NR_AIX5_access VG_(aix5_NR_access)

extern Int VG_(aix5_NR_accessx);
#define __NR_AIX5_accessx VG_(aix5_NR_accessx)

extern Int VG_(aix5_NR_kfcntl);
#define __NR_AIX5_kfcntl VG_(aix5_NR_kfcntl)

extern Int VG_(aix5_NR___pfcntl);
#define __NR_AIX5___pfcntl VG_(aix5_NR___pfcntl)

extern Int VG_(aix5_NR_fstatfs64);
#define __NR_AIX5_fstatfs64 VG_(aix5_NR_fstatfs64)

extern Int VG_(aix5_NR_statfs64);
#define __NR_AIX5_statfs64 VG_(aix5_NR_statfs64)

extern Int VG_(aix5_NR_fstatfs);
#define __NR_AIX5_fstatfs VG_(aix5_NR_fstatfs)

extern Int VG_(aix5_NR_statfs);
#define __NR_AIX5_statfs VG_(aix5_NR_statfs)

extern Int VG_(aix5_NR_probe);
#define __NR_AIX5_probe VG_(aix5_NR_probe)

extern Int VG_(aix5_NR_cmp_swap);
#define __NR_AIX5_cmp_swap VG_(aix5_NR_cmp_swap)

extern Int VG_(aix5_NR__validate_pag);
#define __NR_AIX5__validate_pag VG_(aix5_NR__validate_pag)

extern Int VG_(aix5_NR_kgetsidx);
#define __NR_AIX5_kgetsidx VG_(aix5_NR_kgetsidx)

extern Int VG_(aix5_NR_kgetsid);
#define __NR_AIX5_kgetsid VG_(aix5_NR_kgetsid)

extern Int VG_(aix5_NR_plock);
#define __NR_AIX5_plock VG_(aix5_NR_plock)

extern Int VG_(aix5_NR_upfput);
#define __NR_AIX5_upfput VG_(aix5_NR_upfput)

extern Int VG_(aix5_NR_usrinfo);
#define __NR_AIX5_usrinfo VG_(aix5_NR_usrinfo)

extern Int VG_(aix5_NR_audit);
#define __NR_AIX5_audit VG_(aix5_NR_audit)

extern Int VG_(aix5_NR_auditobj);
#define __NR_AIX5_auditobj VG_(aix5_NR_auditobj)

extern Int VG_(aix5_NR_auditbin);
#define __NR_AIX5_auditbin VG_(aix5_NR_auditbin)

extern Int VG_(aix5_NR_auditevents);
#define __NR_AIX5_auditevents VG_(aix5_NR_auditevents)

extern Int VG_(aix5_NR_faccessx);
#define __NR_AIX5_faccessx VG_(aix5_NR_faccessx)

extern Int VG_(aix5_NR___fchaclx);
#define __NR_AIX5___fchaclx VG_(aix5_NR___fchaclx)

extern Int VG_(aix5_NR___chaclx);
#define __NR_AIX5___chaclx VG_(aix5_NR___chaclx)

extern Int VG_(aix5_NR_fchacl);
#define __NR_AIX5_fchacl VG_(aix5_NR_fchacl)

extern Int VG_(aix5_NR_chacl);
#define __NR_AIX5_chacl VG_(aix5_NR_chacl)

extern Int VG_(aix5_NR___fstataclx);
#define __NR_AIX5___fstataclx VG_(aix5_NR___fstataclx)

extern Int VG_(aix5_NR___stataclx);
#define __NR_AIX5___stataclx VG_(aix5_NR___stataclx)

extern Int VG_(aix5_NR_fstatacl);
#define __NR_AIX5_fstatacl VG_(aix5_NR_fstatacl)

extern Int VG_(aix5_NR_statacl);
#define __NR_AIX5_statacl VG_(aix5_NR_statacl)

extern Int VG_(aix5_NR_setpriv);
#define __NR_AIX5_setpriv VG_(aix5_NR_setpriv)

extern Int VG_(aix5_NR_getpriv);
#define __NR_AIX5_getpriv VG_(aix5_NR_getpriv)

extern Int VG_(aix5_NR_fstatpriv);
#define __NR_AIX5_fstatpriv VG_(aix5_NR_fstatpriv)

extern Int VG_(aix5_NR_statpriv);
#define __NR_AIX5_statpriv VG_(aix5_NR_statpriv)

extern Int VG_(aix5_NR_fchpriv);
#define __NR_AIX5_fchpriv VG_(aix5_NR_fchpriv)

extern Int VG_(aix5_NR_chpriv);
#define __NR_AIX5_chpriv VG_(aix5_NR_chpriv)

extern Int VG_(aix5_NR_i_int2cpu_pal);
#define __NR_AIX5_i_int2cpu_pal VG_(aix5_NR_i_int2cpu_pal)

extern Int VG_(aix5_NR_hd_cfg);
#define __NR_AIX5_hd_cfg VG_(aix5_NR_hd_cfg)

extern Int VG_(aix5_NR_putpmsg);
#define __NR_AIX5_putpmsg VG_(aix5_NR_putpmsg)

extern Int VG_(aix5_NR_putmsg);
#define __NR_AIX5_putmsg VG_(aix5_NR_putmsg)

extern Int VG_(aix5_NR_getpmsg);
#define __NR_AIX5_getpmsg VG_(aix5_NR_getpmsg)

extern Int VG_(aix5_NR_getmsg);
#define __NR_AIX5_getmsg VG_(aix5_NR_getmsg)

extern Int VG_(aix5_NR_strinfo);
#define __NR_AIX5_strinfo VG_(aix5_NR_strinfo)

extern Int VG_(aix5_NR_strreset);
#define __NR_AIX5_strreset VG_(aix5_NR_strreset)

extern Int VG_(aix5_NR_dupmsg);
#define __NR_AIX5_dupmsg VG_(aix5_NR_dupmsg)

extern Int VG_(aix5_NR__kgrantpt);
#define __NR_AIX5__kgrantpt VG_(aix5_NR__kgrantpt)

extern Int VG_(aix5_NR_aixgsc);
#define __NR_AIX5_aixgsc VG_(aix5_NR_aixgsc)

extern Int VG_(aix5_NR_smaccept);
#define __NR_AIX5_smaccept VG_(aix5_NR_smaccept)

extern Int VG_(aix5_NR_smconnect);
#define __NR_AIX5_smconnect VG_(aix5_NR_smconnect)

extern Int VG_(aix5_NR_smlisten);
#define __NR_AIX5_smlisten VG_(aix5_NR_smlisten)

extern Int VG_(aix5_NR_smbind);
#define __NR_AIX5_smbind VG_(aix5_NR_smbind)

extern Int VG_(aix5_NR_smsocket);
#define __NR_AIX5_smsocket VG_(aix5_NR_smsocket)

extern Int VG_(aix5_NR_smdetatt);
#define __NR_AIX5_smdetatt VG_(aix5_NR_smdetatt)

extern Int VG_(aix5_NR_smattach);
#define __NR_AIX5_smattach VG_(aix5_NR_smattach)

extern Int VG_(aix5_NR_smselect);
#define __NR_AIX5_smselect VG_(aix5_NR_smselect)

extern Int VG_(aix5_NR_smwait);
#define __NR_AIX5_smwait VG_(aix5_NR_smwait)

extern Int VG_(aix5_NR_smsetthresh);
#define __NR_AIX5_smsetthresh VG_(aix5_NR_smsetthresh)

extern Int VG_(aix5_NR_smsendbuff);
#define __NR_AIX5_smsendbuff VG_(aix5_NR_smsendbuff)

extern Int VG_(aix5_NR_smfreebuff);
#define __NR_AIX5_smfreebuff VG_(aix5_NR_smfreebuff)

extern Int VG_(aix5_NR_smrcvbuff);
#define __NR_AIX5_smrcvbuff VG_(aix5_NR_smrcvbuff)

extern Int VG_(aix5_NR_smgetbuff);
#define __NR_AIX5_smgetbuff VG_(aix5_NR_smgetbuff)

extern Int VG_(aix5_NR_smversion);
#define __NR_AIX5_smversion VG_(aix5_NR_smversion)

extern Int VG_(aix5_NR_smtcheckinit);
#define __NR_AIX5_smtcheckinit VG_(aix5_NR_smtcheckinit)

extern Int VG_(aix5_NR_aio_nwait_timeout);
#define __NR_AIX5_aio_nwait_timeout VG_(aix5_NR_aio_nwait_timeout)

extern Int VG_(aix5_NR_kaio_stats);
#define __NR_AIX5_kaio_stats VG_(aix5_NR_kaio_stats)

extern Int VG_(aix5_NR_aio_cntl);
#define __NR_AIX5_aio_cntl VG_(aix5_NR_aio_cntl)

extern Int VG_(aix5_NR_listio);
#define __NR_AIX5_listio VG_(aix5_NR_listio)

extern Int VG_(aix5_NR_acancel);
#define __NR_AIX5_acancel VG_(aix5_NR_acancel)

extern Int VG_(aix5_NR_iosuspend);
#define __NR_AIX5_iosuspend VG_(aix5_NR_iosuspend)

extern Int VG_(aix5_NR_kaio_rdwr);
#define __NR_AIX5_kaio_rdwr VG_(aix5_NR_kaio_rdwr)

extern Int VG_(aix5_NR_aio_nwait);
#define __NR_AIX5_aio_nwait VG_(aix5_NR_aio_nwait)

extern Int VG_(aix5_NR__posix_iofsync);
#define __NR_AIX5__posix_iofsync VG_(aix5_NR__posix_iofsync)

extern Int VG_(aix5_NR__posix_aio_nwait_timeout);
#define __NR_AIX5__posix_aio_nwait_timeout VG_(aix5_NR__posix_aio_nwait_timeout)

extern Int VG_(aix5_NR__posix_kaio_stats);
#define __NR_AIX5__posix_kaio_stats VG_(aix5_NR__posix_kaio_stats)

extern Int VG_(aix5_NR__posix_listio);
#define __NR_AIX5__posix_listio VG_(aix5_NR__posix_listio)

extern Int VG_(aix5_NR__posix_acancel);
#define __NR_AIX5__posix_acancel VG_(aix5_NR__posix_acancel)

extern Int VG_(aix5_NR__posix_iosuspend);
#define __NR_AIX5__posix_iosuspend VG_(aix5_NR__posix_iosuspend)

extern Int VG_(aix5_NR__posix_kaio_rdwr);
#define __NR_AIX5__posix_kaio_rdwr VG_(aix5_NR__posix_kaio_rdwr)

extern Int VG_(aix5_NR__posix_aio_cntl);
#define __NR_AIX5__posix_aio_cntl VG_(aix5_NR__posix_aio_cntl)

extern Int VG_(aix5_NR__posix_aio_nwait);
#define __NR_AIX5__posix_aio_nwait VG_(aix5_NR__posix_aio_nwait)

extern Int VG_(aix5_NR_nfs_cntl);
#define __NR_AIX5_nfs_cntl VG_(aix5_NR_nfs_cntl)

extern Int VG_(aix5_NR_nfssvc);
#define __NR_AIX5_nfssvc VG_(aix5_NR_nfssvc)

extern Int VG_(aix5_NR_nfs_getfh);
#define __NR_AIX5_nfs_getfh VG_(aix5_NR_nfs_getfh)

extern Int VG_(aix5_NR_exportfs);
#define __NR_AIX5_exportfs VG_(aix5_NR_exportfs)

extern Int VG_(aix5_NR_lm_svc);
#define __NR_AIX5_lm_svc VG_(aix5_NR_lm_svc)

extern Int VG_(aix5_NR_pw_config);
#define __NR_AIX5_pw_config VG_(aix5_NR_pw_config)

extern Int VG_(aix5_NR_pw_post);
#define __NR_AIX5_pw_post VG_(aix5_NR_pw_post)

extern Int VG_(aix5_NR_pw_wait);
#define __NR_AIX5_pw_wait VG_(aix5_NR_pw_wait)

extern Int VG_(aix5_NR_pw_loadavg);
#define __NR_AIX5_pw_loadavg VG_(aix5_NR_pw_loadavg)

extern Int VG_(aix5_NR_pw_debug);
#define __NR_AIX5_pw_debug VG_(aix5_NR_pw_debug)

/* Extras for AIX 5.3 */

extern Int VG_(aix5_NR___libc_sbrk);
#define __NR_AIX5___libc_sbrk VG_(aix5_NR___libc_sbrk)

extern Int VG_(aix5_NR_thread_waitlock_);
#define __NR_AIX5_thread_waitlock_ VG_(aix5_NR_thread_waitlock_)

extern Int VG_(aix5_NR__fp_fpscrx_sc);
#define __NR_AIX5__fp_fpscrx_sc VG_(aix5_NR__fp_fpscrx_sc)

extern Int VG_(aix5_NR_sched_get_priority_max);
#define __NR_AIX5_sched_get_priority_max \
        VG_(aix5_NR_sched_get_priority_max)

/* Extras for AIX 5.3 64-bit mode. */

extern Int VG_(aix5_NR_kload);
#define __NR_AIX5_kload VG_(aix5_NR_kload)

extern Int VG_(aix5_NR__fp_fpscrx64_);
#define __NR_AIX5__fp_fpscrx64_ VG_(aix5_NR__fp_fpscrx64_)

extern Int VG_(aix5_NR_kunload64);
#define __NR_AIX5_kunload64 VG_(aix5_NR_kunload64)

/* We need an extra fake syscall to denote signal handler returns, as
   used in sigframe-ppc32/64-aix5.h.  Since we don't know what number we
   can assign to it, monitor the numbers passed to
   VG_(aix5_register_syscall), and set it to 10000+the largest syscall
   nummber seen. */

extern Int VG_(aix5_NR_FAKE_SIGRETURN);
#define __NR_AIX5_FAKE_SIGRETURN VG_(aix5_NR_FAKE_SIGRETURN)


//--------------------------------------------------------------
// "Bindings" to Linux-like names
//--------------------------------------------------------------

#define __NR_getppid         __NR_AIX5__getppid
#define __NR_getpid          __NR_AIX5__getpid
#define __NR_close           __NR_AIX5_close
#define __NR_open            __NR_AIX5_open
#define __NR_mmap            __NR_AIX5_mmap
#define __NR_write           __NR_AIX5_kwrite
#define __NR_exit            __NR_AIX5__exit
#define __NR_read            __NR_AIX5_kread
#define __NR_getrlimit       __NR_AIX5_appgetrlimit
#define __NR_setrlimit       __NR_AIX5_appsetrlimit
#define __NR_rt_sigaction    __NR_AIX5__sigaction
#define __NR_rt_sigprocmask  __NR_AIX5_sigprocmask
#define __NR__sigpending     __NR_AIX5__sigpending
#define __NR__sigsuspend     __NR_AIX5__sigsuspend
#define __NR_fcntl           __NR_AIX5_kfcntl
#define __NR_unlink          __NR_AIX5_unlink
#define __NR_pipe            __NR_AIX5_pipe
#define __NR_mprotect        __NR_AIX5_mprotect
#define __NR_munmap          __NR_AIX5_munmap
#define __NR_fork            __NR_AIX5_kfork
#define __NR_execve          __NR_AIX5_execve
#define __NR_rename          __NR_AIX5_rename
#define __NR_access          __NR_AIX5_access
#define __NR_kill            __NR_AIX5_kill
#define __NR_tkill           __NR_AIX5_thread_kill
#define __NR_getgroups       __NR_AIX5_getgroups
#define __NR_sched_yield     __NR_AIX5_yield

//--------------------------------------------------------------
// BOGUS
//--------------------------------------------------------------

/* XXXXXXXXX BOGUS */
#define __NR_rt_sigreturn    9999
#define __NR_getegid         9999
#define __NR_ptrace          9999
#define __NR_rt_sigtimedwait 9999
#define __NR_lseek           9999
#define __NR_fstat           9999
#define __NR_dup             9999
#define __NR_getcwd          9999
#define __NR_readlink        9999
#define __NR_getdents        9999
#define __NR_gettid          9999
#define __NR_getpgrp         9999
#define __NR_geteuid         9999


#endif /* __VKI_SCNUMS_AIX5_H */

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
