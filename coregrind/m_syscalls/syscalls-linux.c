
/*--------------------------------------------------------------------*/
/*--- Linux-specific syscalls, etc.               syscalls-linux.c ---*/
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

#include "core.h"
#include "pub_core_aspacemgr.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcfile.h"
#include "pub_core_libcprint.h"
#include "pub_core_tooliface.h"
#include "pub_core_options.h"
#include "pub_core_signals.h"

#include "priv_types_n_macros.h"
#include "priv_syscalls-generic.h"
#include "priv_syscalls-linux.h"

/* ---------------------------------------------------------------------
   PRE/POST wrappers for arch-generic, Linux-specific syscalls
   ------------------------------------------------------------------ */

// Nb: See the comment above the generic PRE/POST wrappers in
// coregrind/vg_syscalls.c for notes about how they work.

#define PRE(name)       DEFN_PRE_TEMPLATE(linux, name)
#define POST(name)      DEFN_POST_TEMPLATE(linux, name)

PRE(sys_set_tid_address)
{
   PRINT("sys_set_tid_address ( %p )", ARG1);
   PRE_REG_READ1(long, "set_tid_address", int *, tidptr);
}

PRE(sys_exit_group)
{
   ThreadId     t;
   ThreadState* tst;

   PRINT("exit_group( %d )", ARG1);
   PRE_REG_READ1(void, "exit_group", int, exit_code);

   tst = VG_(get_ThreadState)(tid);

   /* A little complex; find all the threads with the same threadgroup
      as this one (including this one), and mark them to exit */
   for (t = 1; t < VG_N_THREADS; t++) {
      if ( /* not alive */
           VG_(threads)[t].status == VgTs_Empty 
           ||
	   /* not our group */
           VG_(threads)[t].os_state.threadgroup != tst->os_state.threadgroup
         )
         continue;

      VG_(threads)[t].exitreason = VgSrc_ExitSyscall;
      VG_(threads)[t].os_state.exitcode = ARG1;

      if (t != tid)
	 VG_(kill_thread)(t);	/* unblock it, if blocked */
   }

   /* We have to claim the syscall already succeeded. */
   SET_STATUS_Success(0);
}

PRE(sys_mount)
{
   // Nb: depending on 'flags', the 'type' and 'data' args may be ignored.
   // We are conservative and check everything, except the memory pointed to
   // by 'data'.
   *flags |= SfMayBlock;
   PRINT( "sys_mount( %p, %p, %p, %p, %p )" ,ARG1,ARG2,ARG3,ARG4,ARG5);
   PRE_REG_READ5(long, "mount",
                 char *, source, char *, target, char *, type,
                 unsigned long, flags, void *, data);
   PRE_MEM_RASCIIZ( "mount(source)", ARG1);
   PRE_MEM_RASCIIZ( "mount(target)", ARG2);
   PRE_MEM_RASCIIZ( "mount(type)", ARG3);
}

PRE(sys_oldumount)
{
   PRINT("sys_oldumount( %p )", ARG1);
   PRE_REG_READ1(long, "umount", char *, path);
   PRE_MEM_RASCIIZ( "umount(path)", ARG1);
}

PRE(sys_umount)
{
   PRINT("sys_umount( %p )", ARG1);
   PRE_REG_READ2(long, "umount2", char *, path, int, flags);
   PRE_MEM_RASCIIZ( "umount2(path)", ARG1);
}

PRE(sys_llseek)
{
   PRINT("sys_llseek ( %d, 0x%x, 0x%x, %p, %d )", ARG1,ARG2,ARG3,ARG4,ARG5);
   PRE_REG_READ5(long, "llseek",
                 unsigned int, fd, unsigned long, offset_high,
                 unsigned long, offset_low, vki_loff_t *, result,
                 unsigned int, whence);
   PRE_MEM_WRITE( "llseek(result)", ARG4, sizeof(vki_loff_t));
}

POST(sys_llseek)
{
   vg_assert(SUCCESS);
   if (RES == 0)
      POST_MEM_WRITE( ARG4, sizeof(vki_loff_t) );
}

//zz PRE(sys_adjtimex, 0)
//zz {
//zz    struct vki_timex *tx = (struct vki_timex *)ARG1;
//zz    PRINT("sys_adjtimex ( %p )", ARG1);
//zz    PRE_REG_READ1(long, "adjtimex", struct timex *, buf);
//zz    PRE_MEM_READ( "adjtimex(timex->modes)", ARG1, sizeof(tx->modes));
//zz 
//zz #define ADJX(bit,field) 				\
//zz    if (tx->modes & bit)					\
//zz       PRE_MEM_READ( "adjtimex(timex->"#field")",	\
//zz 		    (Addr)&tx->field, sizeof(tx->field))
//zz    ADJX(ADJ_FREQUENCY, freq);
//zz    ADJX(ADJ_MAXERROR, maxerror);
//zz    ADJX(ADJ_ESTERROR, esterror);
//zz    ADJX(ADJ_STATUS, status);
//zz    ADJX(ADJ_TIMECONST, constant);
//zz    ADJX(ADJ_TICK, tick);
//zz #undef ADJX
//zz    
//zz    PRE_MEM_WRITE( "adjtimex(timex)", ARG1, sizeof(struct vki_timex));
//zz }
//zz 
//zz POST(sys_adjtimex)
//zz {
//zz    POST_MEM_WRITE( ARG1, sizeof(struct vki_timex) );
//zz }
//zz 
//zz PRE(sys_setfsuid16, 0)
//zz {
//zz    PRINT("sys_setfsuid16 ( %d )", ARG1);
//zz    PRE_REG_READ1(long, "setfsuid16", vki_old_uid_t, uid);
//zz }
//zz 
//zz PRE(sys_setfsuid, 0)
//zz {
//zz    PRINT("sys_setfsuid ( %d )", ARG1);
//zz    PRE_REG_READ1(long, "setfsuid", vki_uid_t, uid);
//zz }
//zz 
//zz PRE(sys_setfsgid16, 0)
//zz {
//zz    PRINT("sys_setfsgid16 ( %d )", ARG1);
//zz    PRE_REG_READ1(long, "setfsgid16", vki_old_gid_t, gid);
//zz }
//zz 
//zz PRE(sys_setfsgid, 0)
//zz {
//zz    PRINT("sys_setfsgid ( %d )", ARG1);
//zz    PRE_REG_READ1(long, "setfsgid", vki_gid_t, gid);
//zz }
//zz 
//zz PRE(sys_setresuid16, 0)
//zz {
//zz    PRINT("sys_setresuid16 ( %d, %d, %d )", ARG1, ARG2, ARG3);
//zz    PRE_REG_READ3(long, "setresuid16",
//zz                  vki_old_uid_t, ruid, vki_old_uid_t, euid, vki_old_uid_t, suid);
//zz }
//zz 
//zz PRE(sys_setresuid, 0)
//zz {
//zz    PRINT("sys_setresuid ( %d, %d, %d )", ARG1, ARG2, ARG3);
//zz    PRE_REG_READ3(long, "setresuid",
//zz                  vki_uid_t, ruid, vki_uid_t, euid, vki_uid_t, suid);
//zz }
//zz 
//zz PRE(sys_getresuid16, 0)
//zz {
//zz    PRINT("sys_getresuid16 ( %p, %p, %p )", ARG1,ARG2,ARG3);
//zz    PRE_REG_READ3(long, "getresuid16",
//zz                  vki_old_uid_t *, ruid, vki_old_uid_t *, euid,
//zz                  vki_old_uid_t *, suid);
//zz    PRE_MEM_WRITE( "getresuid16(ruid)", ARG1, sizeof(vki_old_uid_t) );
//zz    PRE_MEM_WRITE( "getresuid16(euid)", ARG2, sizeof(vki_old_uid_t) );
//zz    PRE_MEM_WRITE( "getresuid16(suid)", ARG3, sizeof(vki_old_uid_t) );
//zz }
//zz 
//zz POST(sys_getresuid16)
//zz {
//zz    if (RES == 0) {
//zz       POST_MEM_WRITE( ARG1, sizeof(vki_old_uid_t) );
//zz       POST_MEM_WRITE( ARG2, sizeof(vki_old_uid_t) );
//zz       POST_MEM_WRITE( ARG3, sizeof(vki_old_uid_t) );
//zz    }
//zz }
//zz 
//zz PRE(sys_getresuid, 0)
//zz {
//zz    PRINT("sys_getresuid ( %p, %p, %p )", ARG1,ARG2,ARG3);
//zz    PRE_REG_READ3(long, "getresuid", 
//zz                  vki_uid_t *, ruid, vki_uid_t *, euid, vki_uid_t *, suid);
//zz    PRE_MEM_WRITE( "getresuid(ruid)", ARG1, sizeof(vki_uid_t) );
//zz    PRE_MEM_WRITE( "getresuid(euid)", ARG2, sizeof(vki_uid_t) );
//zz    PRE_MEM_WRITE( "getresuid(suid)", ARG3, sizeof(vki_uid_t) );
//zz }
//zz 
//zz POST(sys_getresuid)
//zz {
//zz    if (RES == 0) {
//zz       POST_MEM_WRITE( ARG1, sizeof(vki_uid_t) );
//zz       POST_MEM_WRITE( ARG2, sizeof(vki_uid_t) );
//zz       POST_MEM_WRITE( ARG3, sizeof(vki_uid_t) );
//zz    }
//zz }
//zz 
//zz PRE(sys_setresgid16, 0)
//zz {
//zz    PRINT("sys_setresgid16 ( %d, %d, %d )", ARG1, ARG2, ARG3);
//zz    PRE_REG_READ3(long, "setresgid16",
//zz                  vki_old_gid_t, rgid, vki_old_gid_t, egid, vki_old_gid_t, sgid);
//zz }
//zz 
//zz PRE(sys_setresgid, 0)
//zz {
//zz    PRINT("sys_setresgid ( %d, %d, %d )", ARG1, ARG2, ARG3);
//zz    PRE_REG_READ3(long, "setresgid",
//zz                  vki_gid_t, rgid, vki_gid_t, egid, vki_gid_t, sgid);
//zz }
//zz 
//zz PRE(sys_getresgid16, 0)
//zz {
//zz    PRINT("sys_getresgid16 ( %p, %p, %p )", ARG1,ARG2,ARG3);
//zz    PRE_REG_READ3(long, "getresgid16",
//zz                  vki_old_gid_t *, rgid, vki_old_gid_t *, egid,
//zz                  vki_old_gid_t *, sgid);
//zz    PRE_MEM_WRITE( "getresgid16(rgid)", ARG1, sizeof(vki_old_gid_t) );
//zz    PRE_MEM_WRITE( "getresgid16(egid)", ARG2, sizeof(vki_old_gid_t) );
//zz    PRE_MEM_WRITE( "getresgid16(sgid)", ARG3, sizeof(vki_old_gid_t) );
//zz }
//zz 
//zz POST(sys_getresgid16)
//zz {
//zz    if (RES == 0) {
//zz       POST_MEM_WRITE( ARG1, sizeof(vki_old_gid_t) );
//zz       POST_MEM_WRITE( ARG2, sizeof(vki_old_gid_t) );
//zz       POST_MEM_WRITE( ARG3, sizeof(vki_old_gid_t) );
//zz    }
//zz }
//zz 
//zz PRE(sys_getresgid, 0)
//zz {
//zz    PRINT("sys_getresgid ( %p, %p, %p )", ARG1,ARG2,ARG3);
//zz    PRE_REG_READ3(long, "getresgid", 
//zz                  vki_gid_t *, rgid, vki_gid_t *, egid, vki_gid_t *, sgid);
//zz    PRE_MEM_WRITE( "getresgid(rgid)", ARG1, sizeof(vki_gid_t) );
//zz    PRE_MEM_WRITE( "getresgid(egid)", ARG2, sizeof(vki_gid_t) );
//zz    PRE_MEM_WRITE( "getresgid(sgid)", ARG3, sizeof(vki_gid_t) );
//zz }
//zz 
//zz POST(sys_getresgid)
//zz {
//zz    if (RES == 0) {
//zz       POST_MEM_WRITE( ARG1, sizeof(vki_gid_t) );
//zz       POST_MEM_WRITE( ARG2, sizeof(vki_gid_t) );
//zz       POST_MEM_WRITE( ARG3, sizeof(vki_gid_t) );
//zz    }
//zz }
//zz 
//zz PRE(sys_ioperm, 0)
//zz {
//zz    PRINT("sys_ioperm ( %d, %d, %d )", ARG1, ARG2, ARG3 );
//zz    PRE_REG_READ3(long, "ioperm",
//zz                  unsigned long, from, unsigned long, num, int, turn_on);
//zz }
//zz 
//zz PRE(sys_syslog, MayBlock)
//zz {
//zz    PRINT("sys_syslog (%d, %p, %d)", ARG1,ARG2,ARG3);
//zz    PRE_REG_READ3(long, "syslog", int, type, char *, bufp, int, len);
//zz    switch (ARG1) {
//zz    // The kernel uses magic numbers here, rather than named constants,
//zz    // therefore so do we.
//zz    case 2: case 3: case 4:
//zz       PRE_MEM_WRITE( "syslog(bufp)", ARG2, ARG3);
//zz       break;
//zz    default: 
//zz       break;
//zz    }
//zz }
//zz 
//zz POST(sys_syslog)
//zz {
//zz    switch (ARG1) {
//zz    case 2: case 3: case 4:
//zz       POST_MEM_WRITE( ARG2, ARG3 );
//zz       break;
//zz    default:
//zz       break;
//zz    }
//zz }
//zz 
//zz PRE(sys_vhangup, 0)
//zz {
//zz    PRINT("sys_vhangup ( )");
//zz    PRE_REG_READ0(long, "vhangup");
//zz }
//zz 
//zz PRE(sys_sysinfo, 0)
//zz {
//zz    PRINT("sys_sysinfo ( %p )",ARG1);
//zz    PRE_REG_READ1(long, "sysinfo", struct sysinfo *, info);
//zz    PRE_MEM_WRITE( "sysinfo(info)", ARG1, sizeof(struct vki_sysinfo) );
//zz }
//zz 
//zz POST(sys_sysinfo)
//zz {
//zz    POST_MEM_WRITE( ARG1, sizeof(struct vki_sysinfo) );
//zz }
//zz 
//zz PRE(sys_personality, 0)
//zz {
//zz    PRINT("sys_personality ( %llu )", (ULong)ARG1);
//zz    PRE_REG_READ1(long, "personality", vki_u_long, persona);
//zz }

PRE(sys_sysctl)
{
   struct __vki_sysctl_args *argz;
   PRINT("sys_sysctl ( %p )", ARG1 );
   argz = (struct __vki_sysctl_args *)ARG1;
   PRE_REG_READ1(long, "sysctl", struct __sysctl_args *, argz);
   PRE_MEM_WRITE( "sysctl(args)", ARG1, sizeof(struct __vki_sysctl_args) );
   if (!VG_(is_addressable)(ARG1, sizeof(struct __vki_sysctl_args), VKI_PROT_READ)) {
      SET_STATUS_Failure( VKI_EFAULT );
      return;
   }

   PRE_MEM_READ("sysctl(name)", (Addr)argz->name, argz->nlen * sizeof(*argz->name));
   if (argz->newval != NULL)
      PRE_MEM_READ("sysctl(newval)", (Addr)argz->newval, argz->newlen);
   if (argz->oldlenp != NULL) {
      PRE_MEM_READ("sysctl(oldlenp)", (Addr)argz->oldlenp, sizeof(*argz->oldlenp));
      PRE_MEM_WRITE("sysctl(oldval)", (Addr)argz->oldval, *argz->oldlenp);
   }
}

POST(sys_sysctl)
{
   struct __vki_sysctl_args *argz;
   argz = (struct __vki_sysctl_args *)ARG1;
   if (argz->oldlenp != NULL) {
      POST_MEM_WRITE((Addr)argz->oldlenp, sizeof(*argz->oldlenp));
      POST_MEM_WRITE((Addr)argz->oldval, 1 + *argz->oldlenp);
   }
}

//zz PRE(sys_prctl, MayBlock)
//zz {
//zz    PRINT( "prctl ( %d, %d, %d, %d, %d )", ARG1, ARG2, ARG3, ARG4, ARG5 );
//zz    // XXX: too simplistic, often not all args are used
//zz    // Nb: can't use "ARG2".."ARG5" here because that's our own macro...
//zz    PRE_REG_READ5(long, "prctl",
//zz                  int, option, unsigned long, arg2, unsigned long, arg3,
//zz                  unsigned long, arg4, unsigned long, arg5);
//zz    // XXX: totally wrong... we need to look at the 'option' arg, and do
//zz    // PRE_MEM_READs/PRE_MEM_WRITEs as necessary...
//zz }
//zz 
//zz PRE(sys_sendfile, MayBlock)
//zz {
//zz    PRINT("sys_sendfile ( %d, %d, %p, %llu )", ARG1,ARG2,ARG3,(ULong)ARG4);
//zz    PRE_REG_READ4(ssize_t, "sendfile",
//zz                  int, out_fd, int, in_fd, vki_off_t *, offset,
//zz                  vki_size_t, count);
//zz    if (ARG3 != 0)
//zz       PRE_MEM_WRITE( "sendfile(offset)", ARG3, sizeof(vki_off_t) );
//zz }
//zz 
//zz POST(sys_sendfile)
//zz {
//zz    POST_MEM_WRITE( ARG3, sizeof( vki_off_t ) );
//zz }
//zz 
//zz PRE(sys_sendfile64, MayBlock)
//zz {
//zz    PRINT("sendfile64 ( %d, %d, %p, %llu )",ARG1,ARG2,ARG3,(ULong)ARG4);
//zz    PRE_REG_READ4(ssize_t, "sendfile64",
//zz                  int, out_fd, int, in_fd, vki_loff_t *, offset,
//zz                  vki_size_t, count);
//zz    if (ARG3 != 0)
//zz       PRE_MEM_WRITE( "sendfile64(offset)", ARG3, sizeof(vki_loff_t) );
//zz }
//zz 
//zz POST(sys_sendfile64)
//zz {
//zz    if (ARG3 != 0 ) {
//zz       POST_MEM_WRITE( ARG3, sizeof(vki_loff_t) );
//zz    }
//zz }

PRE(sys_futex)
{
   /* 
      arg    param                              used by ops

      ARG1 - u32 *futex				all
      ARG2 - int op
      ARG3 - int val				WAIT,WAKE,FD,REQUEUE,CMP_REQUEUE
      ARG4 - struct timespec *utime		WAIT:time*	REQUEUE,CMP_REQUEUE:val2
      ARG5 - u32 *uaddr2			REQUEUE,CMP_REQUEUE
      ARG6 - int val3				CMP_REQUEUE
    */
   PRINT("sys_futex ( %p, %d, %d, %p, %p )", ARG1,ARG2,ARG3,ARG4,ARG5);
   PRE_REG_READ6(long, "futex", 
                 vki_u32 *, futex, int, op, int, val,
                 struct timespec *, utime, vki_u32 *, uaddr2, int, val3);

   PRE_MEM_READ( "futex(futex)", ARG1, sizeof(Int) );

   *flags |= SfMayBlock;

   switch(ARG2) {
   case VKI_FUTEX_WAIT:
      if (ARG4 != 0)
	 PRE_MEM_READ( "futex(timeout)", ARG4, sizeof(struct vki_timespec) );
      break;

   case VKI_FUTEX_REQUEUE:
   case VKI_FUTEX_CMP_REQUEUE:
      PRE_MEM_READ( "futex(futex2)", ARG5, sizeof(Int) );
      break;

   case VKI_FUTEX_WAKE:
   case VKI_FUTEX_FD:
      /* no additional pointers */
      break;

   default:
      SET_STATUS_Failure( VKI_ENOSYS );   // some futex function we don't understand
      break;
   }
}

POST(sys_futex)
{
   vg_assert(SUCCESS);
   POST_MEM_WRITE( ARG1, sizeof(int) );
   if (ARG2 == VKI_FUTEX_FD) {
      if (!VG_(fd_allowed)(RES, "futex", tid, True)) {
         VG_(close)(RES);
         SET_STATUS_Failure( VKI_EMFILE );
      } else {
         if (VG_(clo_track_fds))
            VG_(record_fd_open)(tid, RES, VG_(arena_strdup)(VG_AR_CORE, (Char*)ARG1));
      }
   }
}

//zz PRE(sys_epoll_create, 0)
//zz {
//zz    PRINT("sys_epoll_create ( %d )", ARG1);
//zz    PRE_REG_READ1(long, "epoll_create", int, size);
//zz }
//zz 
//zz POST(sys_epoll_create)
//zz {
//zz    if (!VG_(fd_allowed)(RES, "epoll_create", tid, True)) {
//zz       VG_(close)(RES);
//zz       SET_STATUS_( -VKI_EMFILE );
//zz    } else {
//zz       if (VG_(clo_track_fds))
//zz          VG_(record_fd_open) (tid, RES, NULL);
//zz    }
//zz }
//zz 
//zz PRE(sys_epoll_ctl, 0)
//zz {
//zz    static const char* epoll_ctl_s[3] = {
//zz       "EPOLL_CTL_ADD",
//zz       "EPOLL_CTL_DEL",
//zz       "EPOLL_CTL_MOD"
//zz    };
//zz    PRINT("sys_epoll_ctl ( %d, %s, %d, %p )", 
//zz          ARG1, ( ARG2<3 ? epoll_ctl_s[ARG2] : "?" ), ARG3, ARG4);
//zz    PRE_REG_READ4(long, "epoll_ctl",
//zz                  int, epfd, int, op, int, fd, struct epoll_event *, event);
//zz    PRE_MEM_READ( "epoll_ctl(event)", ARG4, sizeof(struct epoll_event) );
//zz }
//zz 
//zz PRE(sys_epoll_wait, MayBlock)
//zz {
//zz    PRINT("sys_epoll_wait ( %d, %p, %d, %d )", ARG1, ARG2, ARG3, ARG4);
//zz    PRE_REG_READ4(long, "epoll_wait",
//zz                  int, epfd, struct epoll_event *, events,
//zz                  int, maxevents, int, timeout);
//zz    PRE_MEM_WRITE( "epoll_wait(events)", ARG2, sizeof(struct epoll_event)*ARG3);
//zz }
//zz 
//zz POST(sys_epoll_wait)
//zz {
//zz    if (RES > 0)
//zz       POST_MEM_WRITE( ARG2, sizeof(struct epoll_event)*RES ) ;
//zz }

PRE(sys_gettid)
{
   PRINT("sys_gettid ()");
   PRE_REG_READ0(long, "gettid");
}

//zz PRE(sys_tkill, Special)
//zz {
//zz    /* int tkill(pid_t tid, int sig); */
//zz    PRINT("sys_tkill ( %d, %d )", ARG1,ARG2);
//zz    PRE_REG_READ2(long, "tkill", int, tid, int, sig);
//zz    if (!VG_(client_signal_OK)(ARG2)) {
//zz       SET_STATUS_( -VKI_EINVAL );
//zz       return;
//zz    }
//zz 
//zz    /* If we're sending SIGKILL, check to see if the target is one of
//zz       our threads and handle it specially. */
//zz    if (ARG2 == VKI_SIGKILL && VG_(do_sigkill)(ARG1, -1))
//zz       SET_STATUS_(0);
//zz    else
//zz       SET_STATUS_(VG_(do_syscall2)(SYSNO, ARG1, ARG2));
//zz 
//zz    if (VG_(clo_trace_signals))
//zz       VG_(message)(Vg_DebugMsg, "tkill: sent signal %d to pid %d",
//zz 		   ARG2, ARG1);
//zz    // Check to see if this kill gave us a pending signal
//zz    XXX FIXME VG_(poll_signals)(tid);
//zz }

PRE(sys_tgkill)
{
   /* int tgkill(pid_t tgid, pid_t tid, int sig); */
   PRINT("sys_tgkill ( %d, %d, %d )", ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "tgkill", int, tgid, int, tid, int, sig);
   if (!VG_(client_signal_OK)(ARG3)) {
      SET_STATUS_Failure( VKI_EINVAL );
      return;
   }
   
   /* If we're sending SIGKILL, check to see if the target is one of
      our threads and handle it specially. */
   if (ARG3 == VKI_SIGKILL && VG_(do_sigkill)(ARG2, ARG1))
      SET_STATUS_Success(0);
   else
      SET_STATUS_from_SysRes(VG_(do_syscall3)(SYSNO, ARG1, ARG2, ARG3));

   if (VG_(clo_trace_signals))
      VG_(message)(Vg_DebugMsg, "tgkill: sent signal %d to pid %d/%d",
		   ARG3, ARG1, ARG2);
   /* Check to see if this kill gave us a pending signal */
   *flags |= SfPollAfter;
}

POST(sys_tgkill)
{
   if (VG_(clo_trace_signals))
      VG_(message)(Vg_DebugMsg, "tgkill: sent signal %d to pid %d/%d",
                   ARG3, ARG1, ARG2);
}

//zz PRE(sys_fadvise64, 0)
//zz {
//zz    PRINT("sys_fadvise64 ( %d, %lld, %lu, %d )", ARG1,ARG2,ARG3);
//zz    PRE_REG_READ4(long, "fadvise64",
//zz                  int, fd, vki_loff_t, offset, vki_size_t, len, int, advice)
//zz }
//zz 
//zz PRE(sys_fadvise64_64, 0)
//zz {
//zz    PRINT("sys_fadvise64_64 ( %d, %lld, %lld, %d )", ARG1,ARG2,ARG3);
//zz    PRE_REG_READ4(long, "fadvise64_64",
//zz                  int, fd, vki_loff_t, offset, vki_loff_t, len, int, advice)
//zz }
//zz 
//zz // Nb: this wrapper is "Special" because we have to pad/unpad memory around
//zz // the syscall itself, and this allows us to control exactly the code that
//zz // gets run while the padding is in place.
//zz PRE(sys_io_setup, Special)
//zz {
//zz    SizeT size;
//zz    Addr addr;
//zz 
//zz    PRINT("sys_io_setup ( %u, %p )", ARG1,ARG2);
//zz    PRE_REG_READ2(long, "io_setup",
//zz                  unsigned, nr_events, vki_aio_context_t *, ctxp);
//zz    PRE_MEM_WRITE( "io_setup(ctxp)", ARG2, sizeof(vki_aio_context_t) );
//zz    
//zz    size = VG_PGROUNDUP(sizeof(struct vki_aio_ring) +
//zz                        ARG1*sizeof(struct vki_io_event));
//zz    addr = VG_(find_map_space)(0, size, True);
//zz    
//zz    if (addr == 0) {
//zz       SET_STATUS_( -VKI_ENOMEM );
//zz       return;
//zz    }
//zz 
//zz    VG_(map_segment)(addr, size, VKI_PROT_READ|VKI_PROT_WRITE, SF_FIXED);
//zz    
//zz    VG_(pad_address_space)(0);
//zz    SET_STATUS_( VG_(do_syscall2)(SYSNO, ARG1, ARG2) );
//zz    VG_(unpad_address_space)(0);
//zz 
//zz    if (RES == 0) {
//zz       struct vki_aio_ring *r = *(struct vki_aio_ring **)ARG2;
//zz         
//zz       vg_assert(addr == (Addr)r);
//zz       vg_assert(VG_(valid_client_addr)(addr, size, tid, "io_setup"));
//zz                 
//zz       VG_TRACK( new_mem_mmap, addr, size, True, True, False );
//zz       POST_MEM_WRITE( ARG2, sizeof(vki_aio_context_t) );
//zz    }
//zz    else {
//zz       VG_(unmap_range)(addr, size);
//zz    }
//zz }
//zz 
//zz // Nb: This wrapper is "Special" because we need 'size' to do the unmap
//zz // after the syscall.  We must get 'size' from the aio_ring structure,
//zz // before the syscall, while the aio_ring structure still exists.  (And we
//zz // know that we must look at the aio_ring structure because Tom inspected the
//zz // kernel and glibc sources to see what they do, yuk.)
//zz //
//zz // XXX This segment can be implicitly unmapped when aio
//zz // file-descriptors are closed...
//zz PRE(sys_io_destroy, Special)
//zz {     
//zz    Segment *s = VG_(find_segment)(ARG1);
//zz    struct vki_aio_ring *r;
//zz    SizeT size;
//zz       
//zz    PRINT("sys_io_destroy ( %llu )", (ULong)ARG1);
//zz    PRE_REG_READ1(long, "io_destroy", vki_aio_context_t, ctx);
//zz 
//zz    // If we are going to seg fault (due to a bogus ARG1) do it as late as
//zz    // possible...
//zz    r = *(struct vki_aio_ring **)ARG1;
//zz    size = VG_PGROUNDUP(sizeof(struct vki_aio_ring) + 
//zz                        r->nr*sizeof(struct vki_io_event));
//zz 
//zz    SET_STATUS_( VG_(do_syscall1)(SYSNO, ARG1) );
//zz 
//zz    if (RES == 0 && s != NULL) { 
//zz       VG_TRACK( die_mem_munmap, ARG1, size );
//zz       VG_(unmap_range)(ARG1, size);
//zz    }  
//zz }  
//zz 
//zz PRE(sys_io_getevents, MayBlock)
//zz {
//zz    PRINT("sys_io_getevents ( %llu, %lld, %lld, %p, %p )",
//zz          (ULong)ARG1,(Long)ARG2,(Long)ARG3,ARG4,ARG5);
//zz    PRE_REG_READ5(long, "io_getevents",
//zz                  vki_aio_context_t, ctx_id, long, min_nr, long, nr,
//zz                  struct io_event *, events,
//zz                  struct timespec *, timeout);
//zz    if (ARG3 > 0)
//zz       PRE_MEM_WRITE( "io_getevents(events)",
//zz                      ARG4, sizeof(struct vki_io_event)*ARG3 );
//zz    if (ARG5 != 0)
//zz       PRE_MEM_READ( "io_getevents(timeout)",
//zz                     ARG5, sizeof(struct vki_timespec));
//zz }
//zz 
//zz POST(sys_io_getevents)
//zz {
//zz    int i;
//zz 
//zz    if (RES > 0) {
//zz       POST_MEM_WRITE( ARG4, sizeof(struct vki_io_event)*RES );
//zz       for (i = 0; i < RES; i++) {
//zz          const struct vki_io_event *vev = ((struct vki_io_event *)ARG4) + i;
//zz          const struct vki_iocb *cb = (struct vki_iocb *)(Addr)vev->obj;
//zz 
//zz          switch (cb->aio_lio_opcode) {
//zz          case VKI_IOCB_CMD_PREAD:
//zz             if (vev->result > 0)
//zz                POST_MEM_WRITE( cb->aio_buf, vev->result );
//zz             break;
//zz             
//zz          case VKI_IOCB_CMD_PWRITE:
//zz             break;
//zz            
//zz          default:
//zz             VG_(message)(Vg_DebugMsg,"Warning: unhandled io_getevents opcode: %u\n",cb->aio_lio_opcode);
//zz             break;
//zz          }
//zz       }
//zz    }
//zz }
//zz 
//zz PRE(sys_io_submit, 0)
//zz {
//zz    int i;
//zz 
//zz    PRINT("sys_io_submit( %llu, %lld, %p )", (ULong)ARG1,(Long)ARG2,ARG3);
//zz    PRE_REG_READ3(long, "io_submit",
//zz                  vki_aio_context_t, ctx_id, long, nr,
//zz                  struct iocb **, iocbpp);
//zz    PRE_MEM_READ( "io_submit(iocbpp)", ARG3, ARG2*sizeof(struct vki_iocb *) );
//zz    if (ARG3 != 0) {
//zz       for (i = 0; i < ARG2; i++) {
//zz          struct vki_iocb *cb = ((struct vki_iocb **)ARG3)[i];
//zz          PRE_MEM_READ( "io_submit(iocb)", (Addr)cb, sizeof(struct vki_iocb) );
//zz          switch (cb->aio_lio_opcode) {
//zz          case VKI_IOCB_CMD_PREAD:
//zz             PRE_MEM_WRITE( "io_submit(PREAD)", cb->aio_buf, cb->aio_nbytes );
//zz             break;
//zz 
//zz          case VKI_IOCB_CMD_PWRITE:
//zz             PRE_MEM_READ( "io_submit(PWRITE)", cb->aio_buf, cb->aio_nbytes );
//zz             break;
//zz            
//zz          default:
//zz             VG_(message)(Vg_DebugMsg,"Warning: unhandled io_submit opcode: %u\n",
//zz                          cb->aio_lio_opcode);
//zz             break;
//zz          }
//zz       }
//zz    }
//zz }
//zz 
//zz PRE(sys_io_cancel, 0)
//zz {
//zz    PRINT("sys_io_cancel( %llu, %p, %p )", (ULong)ARG1,ARG2,ARG3);
//zz    PRE_REG_READ3(long, "io_cancel",
//zz                  vki_aio_context_t, ctx_id, struct iocb *, iocb,
//zz                  struct io_event *, result);
//zz    PRE_MEM_READ( "io_cancel(iocb)", ARG2, sizeof(struct vki_iocb) );
//zz    PRE_MEM_WRITE( "io_cancel(result)", ARG3, sizeof(struct vki_io_event) );
//zz }
//zz 
//zz POST(sys_io_cancel)
//zz {
//zz    POST_MEM_WRITE( ARG3, sizeof(struct vki_io_event) );
//zz }
//zz 
//zz #undef PRE
//zz #undef POST

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
