
/*--------------------------------------------------------------------*/
/*--- Linux-specific syscalls, etc.               linux/syscalls.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2005 Nicholas Nethercote
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

#include "core.h"

/* ---------------------------------------------------------------------
   PRE/POST wrappers for arch-generic, Linux-specific syscalls
   ------------------------------------------------------------------ */

// Nb: See the comment above the generic PRE/POST wrappers in
// coregrind/vg_syscalls.c for notes about how they work.

#define PRE(name, f)     PRE_TEMPLATE( , vgArch_linux, name, f)
#define POST(name)      POST_TEMPLATE( , vgArch_linux, name)

PRE(sys_exit_group, Special)
{
   ThreadId t;

   PRINT("exit_group( %d )", ARG1);
   PRE_REG_READ1(void, "exit_group", int, exit_code);

   /* A little complex; find all the threads with the same threadgroup
      as this one (including this one), and mark them to exit */
   for (t = 1; t < VG_N_THREADS; t++) {
      if (VG_(threads)[t].status == VgTs_Empty ||				/* not alive */
	  VG_(threads)[t].os_state.threadgroup != tst->os_state.threadgroup)	/* not our group */
	 continue;

      VG_(threads)[t].exitreason = VgSrc_ExitSyscall;
      VG_(threads)[t].os_state.exitcode = ARG1;

      if (t != tid)
	 VG_(kill_thread)(t);	/* unblock it, if blocked */
   }

   /* exit_group doesn't return anything (perhaps it doesn't return?)
      Nevertheless, if we don't do this, the result-not-assigned-
      yet-you-said-you-were-Special assertion in the main syscall
      handling logic will fire.  Hence ..
   */
   SET_RESULT(0);
}

PRE(sys_mount, MayBlock)
{
   // Nb: depending on 'flags', the 'type' and 'data' args may be ignored.
   // We are conservative and check everything, except the memory pointed to
   // by 'data'.
   PRINT( "sys_mount( %p, %p, %p, %p, %p )" ,ARG1,ARG2,ARG3,ARG4,ARG5);
   PRE_REG_READ5(long, "mount",
                 char *, source, char *, target, char *, type,
                 unsigned long, flags, void *, data);
   PRE_MEM_RASCIIZ( "mount(source)", ARG1);
   PRE_MEM_RASCIIZ( "mount(target)", ARG2);
   PRE_MEM_RASCIIZ( "mount(type)", ARG3);
}

PRE(sys_oldumount, 0)
{
   PRINT("sys_oldumount( %p )", ARG1);
   PRE_REG_READ1(long, "umount", char *, path);
   PRE_MEM_RASCIIZ( "umount(path)", ARG1);
}

PRE(sys_umount, 0)
{
   PRINT("sys_umount( %p )", ARG1);
   PRE_REG_READ2(long, "umount2", char *, path, int, flags);
   PRE_MEM_RASCIIZ( "umount2(path)", ARG1);
}

PRE(sys_llseek, 0)
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
   if (RES == 0)
      POST_MEM_WRITE( ARG4, sizeof(vki_loff_t) );
}

PRE(sys_adjtimex, 0)
{
   struct vki_timex *tx = (struct vki_timex *)ARG1;
   PRINT("sys_adjtimex ( %p )", ARG1);
   PRE_REG_READ1(long, "adjtimex", struct timex *, buf);
   PRE_MEM_READ( "adjtimex(timex->modes)", ARG1, sizeof(tx->modes));

#define ADJX(bit,field) 				\
   if (tx->modes & bit)					\
      PRE_MEM_READ( "adjtimex(timex->"#field")",	\
		    (Addr)&tx->field, sizeof(tx->field))
   ADJX(ADJ_FREQUENCY, freq);
   ADJX(ADJ_MAXERROR, maxerror);
   ADJX(ADJ_ESTERROR, esterror);
   ADJX(ADJ_STATUS, status);
   ADJX(ADJ_TIMECONST, constant);
   ADJX(ADJ_TICK, tick);
#undef ADJX
   
   PRE_MEM_WRITE( "adjtimex(timex)", ARG1, sizeof(struct vki_timex));
}

POST(sys_adjtimex)
{
   POST_MEM_WRITE( ARG1, sizeof(struct vki_timex) );
}

PRE(sys_setfsuid16, 0)
{
   PRINT("sys_setfsuid16 ( %d )", ARG1);
   PRE_REG_READ1(long, "setfsuid16", vki_old_uid_t, uid);
}

PRE(sys_setfsuid, 0)
{
   PRINT("sys_setfsuid ( %d )", ARG1);
   PRE_REG_READ1(long, "setfsuid", vki_uid_t, uid);
}

PRE(sys_setfsgid16, 0)
{
   PRINT("sys_setfsgid16 ( %d )", ARG1);
   PRE_REG_READ1(long, "setfsgid16", vki_old_gid_t, gid);
}

PRE(sys_setfsgid, 0)
{
   PRINT("sys_setfsgid ( %d )", ARG1);
   PRE_REG_READ1(long, "setfsgid", vki_gid_t, gid);
}

PRE(sys_setresuid16, 0)
{
   PRINT("sys_setresuid16 ( %d, %d, %d )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "setresuid16",
                 vki_old_uid_t, ruid, vki_old_uid_t, euid, vki_old_uid_t, suid);
}

PRE(sys_setresuid, 0)
{
   PRINT("sys_setresuid ( %d, %d, %d )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "setresuid",
                 vki_uid_t, ruid, vki_uid_t, euid, vki_uid_t, suid);
}

PRE(sys_getresuid16, 0)
{
   PRINT("sys_getresuid16 ( %p, %p, %p )", ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "getresuid16",
                 vki_old_uid_t *, ruid, vki_old_uid_t *, euid,
                 vki_old_uid_t *, suid);
   PRE_MEM_WRITE( "getresuid16(ruid)", ARG1, sizeof(vki_old_uid_t) );
   PRE_MEM_WRITE( "getresuid16(euid)", ARG2, sizeof(vki_old_uid_t) );
   PRE_MEM_WRITE( "getresuid16(suid)", ARG3, sizeof(vki_old_uid_t) );
}

POST(sys_getresuid16)
{
   if (RES == 0) {
      POST_MEM_WRITE( ARG1, sizeof(vki_old_uid_t) );
      POST_MEM_WRITE( ARG2, sizeof(vki_old_uid_t) );
      POST_MEM_WRITE( ARG3, sizeof(vki_old_uid_t) );
   }
}

PRE(sys_getresuid, 0)
{
   PRINT("sys_getresuid ( %p, %p, %p )", ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "getresuid", 
                 vki_uid_t *, ruid, vki_uid_t *, euid, vki_uid_t *, suid);
   PRE_MEM_WRITE( "getresuid(ruid)", ARG1, sizeof(vki_uid_t) );
   PRE_MEM_WRITE( "getresuid(euid)", ARG2, sizeof(vki_uid_t) );
   PRE_MEM_WRITE( "getresuid(suid)", ARG3, sizeof(vki_uid_t) );
}

POST(sys_getresuid)
{
   if (RES == 0) {
      POST_MEM_WRITE( ARG1, sizeof(vki_uid_t) );
      POST_MEM_WRITE( ARG2, sizeof(vki_uid_t) );
      POST_MEM_WRITE( ARG3, sizeof(vki_uid_t) );
   }
}

PRE(sys_setresgid16, 0)
{
   PRINT("sys_setresgid16 ( %d, %d, %d )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "setresgid16",
                 vki_old_gid_t, rgid, vki_old_gid_t, egid, vki_old_gid_t, sgid);
}

PRE(sys_setresgid, 0)
{
   PRINT("sys_setresgid ( %d, %d, %d )", ARG1, ARG2, ARG3);
   PRE_REG_READ3(long, "setresgid",
                 vki_gid_t, rgid, vki_gid_t, egid, vki_gid_t, sgid);
}

PRE(sys_getresgid16, 0)
{
   PRINT("sys_getresgid16 ( %p, %p, %p )", ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "getresgid16",
                 vki_old_gid_t *, rgid, vki_old_gid_t *, egid,
                 vki_old_gid_t *, sgid);
   PRE_MEM_WRITE( "getresgid16(rgid)", ARG1, sizeof(vki_old_gid_t) );
   PRE_MEM_WRITE( "getresgid16(egid)", ARG2, sizeof(vki_old_gid_t) );
   PRE_MEM_WRITE( "getresgid16(sgid)", ARG3, sizeof(vki_old_gid_t) );
}

POST(sys_getresgid16)
{
   if (RES == 0) {
      POST_MEM_WRITE( ARG1, sizeof(vki_old_gid_t) );
      POST_MEM_WRITE( ARG2, sizeof(vki_old_gid_t) );
      POST_MEM_WRITE( ARG3, sizeof(vki_old_gid_t) );
   }
}

PRE(sys_getresgid, 0)
{
   PRINT("sys_getresgid ( %p, %p, %p )", ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "getresgid", 
                 vki_gid_t *, rgid, vki_gid_t *, egid, vki_gid_t *, sgid);
   PRE_MEM_WRITE( "getresgid(rgid)", ARG1, sizeof(vki_gid_t) );
   PRE_MEM_WRITE( "getresgid(egid)", ARG2, sizeof(vki_gid_t) );
   PRE_MEM_WRITE( "getresgid(sgid)", ARG3, sizeof(vki_gid_t) );
}

POST(sys_getresgid)
{
   if (RES == 0) {
      POST_MEM_WRITE( ARG1, sizeof(vki_gid_t) );
      POST_MEM_WRITE( ARG2, sizeof(vki_gid_t) );
      POST_MEM_WRITE( ARG3, sizeof(vki_gid_t) );
   }
}

PRE(sys_ioperm, 0)
{
   PRINT("sys_ioperm ( %d, %d, %d )", ARG1, ARG2, ARG3 );
   PRE_REG_READ3(long, "ioperm",
                 unsigned long, from, unsigned long, num, int, turn_on);
}

PRE(sys_syslog, MayBlock)
{
   PRINT("sys_syslog (%d, %p, %d)", ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "syslog", int, type, char *, bufp, int, len);
   switch (ARG1) {
   // The kernel uses magic numbers here, rather than named constants,
   // therefore so do we.
   case 2: case 3: case 4:
      PRE_MEM_WRITE( "syslog(bufp)", ARG2, ARG3);
      break;
   default: 
      break;
   }
}

POST(sys_syslog)
{
   switch (ARG1) {
   case 2: case 3: case 4:
      POST_MEM_WRITE( ARG2, ARG3 );
      break;
   default:
      break;
   }
}

PRE(sys_vhangup, 0)
{
   PRINT("sys_vhangup ( )");
   PRE_REG_READ0(long, "vhangup");
}

PRE(sys_sysinfo, 0)
{
   PRINT("sys_sysinfo ( %p )",ARG1);
   PRE_REG_READ1(long, "sysinfo", struct sysinfo *, info);
   PRE_MEM_WRITE( "sysinfo(info)", ARG1, sizeof(struct vki_sysinfo) );
}

POST(sys_sysinfo)
{
   POST_MEM_WRITE( ARG1, sizeof(struct vki_sysinfo) );
}

PRE(sys_personality, 0)
{
   PRINT("sys_personality ( %llu )", (ULong)ARG1);
   PRE_REG_READ1(long, "personality", vki_u_long, persona);
}

PRE(sys_sysctl, 0)
{
   PRINT("sys_sysctl ( %p )", ARG1 );
   struct __vki_sysctl_args *args;
   args = (struct __vki_sysctl_args *)ARG1;
   PRE_REG_READ1(long, "sysctl", struct __sysctl_args *, args);
   PRE_MEM_WRITE( "sysctl(args)", ARG1, sizeof(struct __vki_sysctl_args) );
   if (!VG_(is_addressable)(ARG1, sizeof(struct __vki_sysctl_args), VKI_PROT_READ)) {
      SET_RESULT( -VKI_EFAULT );
      return;
   }

   PRE_MEM_READ("sysctl(name)", (Addr)args->name, args->nlen * sizeof(*args->name));
   if (args->newval != NULL)
      PRE_MEM_READ("sysctl(newval)", (Addr)args->newval, args->newlen);
   if (args->oldlenp != NULL) {
      PRE_MEM_READ("sysctl(oldlenp)", (Addr)args->oldlenp, sizeof(*args->oldlenp));
      PRE_MEM_WRITE("sysctl(oldval)", (Addr)args->oldval, *args->oldlenp);
   }
}

POST(sys_sysctl)
{
   struct __vki_sysctl_args *args;
   args = (struct __vki_sysctl_args *)ARG1;
   if (args->oldlenp != NULL) {
      POST_MEM_WRITE((Addr)args->oldlenp, sizeof(*args->oldlenp));
      POST_MEM_WRITE((Addr)args->oldval, *args->oldlenp);
   }
}

PRE(sys_prctl, MayBlock)
{
   PRINT( "prctl ( %d, %d, %d, %d, %d )", ARG1, ARG2, ARG3, ARG4, ARG5 );
   // XXX: too simplistic, often not all args are used
   // Nb: can't use "ARG2".."ARG5" here because that's our own macro...
   PRE_REG_READ5(long, "prctl",
                 int, option, unsigned long, arg2, unsigned long, arg3,
                 unsigned long, arg4, unsigned long, arg5);
   // XXX: totally wrong... we need to look at the 'option' arg, and do
   // SYS_PRE_MEM_READs/SYS_PRE_MEM_WRITEs as necessary...
}

PRE(sys_sendfile, MayBlock)
{
   PRINT("sys_sendfile ( %d, %d, %p, %llu )", ARG1,ARG2,ARG3,(ULong)ARG4);
   PRE_REG_READ4(ssize_t, "sendfile",
                 int, out_fd, int, in_fd, vki_off_t *, offset,
                 vki_size_t, count);
   if (ARG3 != 0)
      PRE_MEM_WRITE( "sendfile(offset)", ARG3, sizeof(vki_off_t) );
}

POST(sys_sendfile)
{
   POST_MEM_WRITE( ARG3, sizeof( vki_off_t ) );
}

PRE(sys_sendfile64, MayBlock)
{
   PRINT("sendfile64 ( %d, %d, %p, %llu )",ARG1,ARG2,ARG3,(ULong)ARG4);
   PRE_REG_READ4(ssize_t, "sendfile64",
                 int, out_fd, int, in_fd, vki_loff_t *, offset,
                 vki_size_t, count);
   if (ARG3 != 0)
      PRE_MEM_WRITE( "sendfile64(offset)", ARG3, sizeof(vki_loff_t) );
}

POST(sys_sendfile64)
{
   if (ARG3 != 0 ) {
      POST_MEM_WRITE( ARG3, sizeof(vki_loff_t) );
   }
}

PRE(sys_futex, MayBlock)
{
   PRINT("sys_futex ( %p, %d, %d, %p, %p )", ARG1,ARG2,ARG3,ARG4,ARG5);
   PRE_REG_READ6(long, "futex", 
                 vki_u32 *, futex, int, op, int, val,
                 struct timespec *, utime, vki_u32 *, uaddr2, int, val3);
   PRE_MEM_READ( "futex(futex)", ARG1, sizeof(int) );
   if (ARG2 == VKI_FUTEX_WAIT && ARG4 != 0)
      PRE_MEM_READ( "futex(timeout)", ARG4, sizeof(struct vki_timespec) );
   if (ARG2 == VKI_FUTEX_REQUEUE)
      PRE_MEM_READ( "futex(futex2)", ARG4, sizeof(int) );
}

POST(sys_futex)
{
   POST_MEM_WRITE( ARG1, sizeof(int) );
   if (ARG2 == VKI_FUTEX_FD) {
      if (!VG_(fd_allowed)(RES, "futex", tid, True)) {
         VG_(close)(RES);
         SET_RESULT( -VKI_EMFILE );
      } else {
         if (VG_(clo_track_fds))
            VG_(record_fd_open)(tid, RES, VG_(arena_strdup)(VG_AR_CORE, (Char*)ARG1));
      }
   }
}

PRE(sys_epoll_create, 0)
{
   PRINT("sys_epoll_create ( %d )", ARG1);
   PRE_REG_READ1(long, "epoll_create", int, size);
}

POST(sys_epoll_create)
{
   if (!VG_(fd_allowed)(RES, "epoll_create", tid, True)) {
      VG_(close)(RES);
      SET_RESULT( -VKI_EMFILE );
   } else {
      if (VG_(clo_track_fds))
         VG_(record_fd_open) (tid, RES, NULL);
   }
}

PRE(sys_epoll_ctl, 0)
{
   static const char* epoll_ctl_s[3] = {
      "EPOLL_CTL_ADD",
      "EPOLL_CTL_DEL",
      "EPOLL_CTL_MOD"
   };
   PRINT("sys_epoll_ctl ( %d, %s, %d, %p )", 
         ARG1, ( ARG2<3 ? epoll_ctl_s[ARG2] : "?" ), ARG3, ARG4);
   PRE_REG_READ4(long, "epoll_ctl",
                 int, epfd, int, op, int, fd, struct epoll_event *, event);
   PRE_MEM_READ( "epoll_ctl(event)", ARG4, sizeof(struct epoll_event) );
}

PRE(sys_epoll_wait, MayBlock)
{
   PRINT("sys_epoll_wait ( %d, %p, %d, %d )", ARG1, ARG2, ARG3, ARG4);
   PRE_REG_READ4(long, "epoll_wait",
                 int, epfd, struct epoll_event *, events,
                 int, maxevents, int, timeout);
   PRE_MEM_WRITE( "epoll_wait(events)", ARG2, sizeof(struct epoll_event)*ARG3);
}

POST(sys_epoll_wait)
{
   if (RES > 0)
      POST_MEM_WRITE( ARG2, sizeof(struct epoll_event)*RES ) ;
}

// Nb: this wrapper is "Special" because we have to pad/unpad memory around
// the syscall itself, and this allows us to control exactly the code that
// gets run while the padding is in place.
PRE(sys_io_setup, Special)
{
   SizeT size;
   Addr addr;

   PRINT("sys_io_setup ( %u, %p )", ARG1,ARG2);
   PRE_REG_READ2(long, "io_setup",
                 unsigned, nr_events, vki_aio_context_t *, ctxp);
   PRE_MEM_WRITE( "io_setup(ctxp)", ARG2, sizeof(vki_aio_context_t) );
   
   size = PGROUNDUP(sizeof(struct vki_aio_ring) +
                    ARG1*sizeof(struct vki_io_event));
   addr = VG_(find_map_space)(0, size, True);
   
   if (addr == 0) {
      SET_RESULT( -VKI_ENOMEM );
      return;
   }

   VG_(map_segment)(addr, size, VKI_PROT_READ|VKI_PROT_WRITE, SF_FIXED);
   
   VG_(pad_address_space)(0);
   VG_(unpad_address_space)(0);

   if (RES == 0) {
      struct vki_aio_ring *r = *(struct vki_aio_ring **)ARG2;
        
      vg_assert(addr == (Addr)r);
      vg_assert(VG_(valid_client_addr)(addr, size, tid, "io_setup"));
                
      VG_TRACK( new_mem_mmap, addr, size, True, True, False );
      POST_MEM_WRITE( ARG2, sizeof(vki_aio_context_t) );
   }
   else {
      VG_(unmap_range)(addr, size);
   }
}

// Nb: This wrapper is "Special" because we need 'size' to do the unmap
// after the syscall.  We must get 'size' from the aio_ring structure,
// before the syscall, while the aio_ring structure still exists.  (And we
// know that we must look at the aio_ring structure because Tom inspected the
// kernel and glibc sources to see what they do, yuk.)
//
// XXX This segment can be implicitly unmapped when aio
// file-descriptors are closed...
PRE(sys_io_destroy, Special)
{     
   Segment *s = VG_(find_segment)(ARG1);
   struct vki_aio_ring *r;
   SizeT size;
      
   PRINT("sys_io_destroy ( %llu )", (ULong)ARG1);
   PRE_REG_READ1(long, "io_destroy", vki_aio_context_t, ctx);

   // If we are going to seg fault (due to a bogus ARG1) do it as late as
   // possible...
   r = *(struct vki_aio_ring **)ARG1;
   size = PGROUNDUP(sizeof(struct vki_aio_ring) + 
                    r->nr*sizeof(struct vki_io_event));

   SET_RESULT( VG_(do_syscall1)(SYSNO, ARG1) );

   if (RES == 0 && s != NULL) { 
      VG_TRACK( die_mem_munmap, ARG1, size );
      VG_(unmap_range)(ARG1, size);
   }  
}  

PRE(sys_io_getevents, MayBlock)
{
   PRINT("sys_io_getevents ( %llu, %lld, %lld, %p, %p )",
         (ULong)ARG1,(Long)ARG2,(Long)ARG3,ARG4,ARG5);
   PRE_REG_READ5(long, "io_getevents",
                 vki_aio_context_t, ctx_id, long, min_nr, long, nr,
                 struct io_event *, events,
                 struct timespec *, timeout);
   if (ARG3 > 0)
      PRE_MEM_WRITE( "io_getevents(events)",
                     ARG4, sizeof(struct vki_io_event)*ARG3 );
   if (ARG5 != 0)
      PRE_MEM_READ( "io_getevents(timeout)",
                    ARG5, sizeof(struct vki_timespec));
}

POST(sys_io_getevents)
{
   int i;

   if (RES > 0) {
      POST_MEM_WRITE( ARG4, sizeof(struct vki_io_event)*RES );
      for (i = 0; i < RES; i++) {
         const struct vki_io_event *vev = ((struct vki_io_event *)ARG4) + i;
         const struct vki_iocb *cb = (struct vki_iocb *)(Addr)vev->obj;

         switch (cb->aio_lio_opcode) {
         case VKI_IOCB_CMD_PREAD:
            if (vev->result > 0)
               POST_MEM_WRITE( cb->aio_buf, vev->result );
            break;
            
         case VKI_IOCB_CMD_PWRITE:
            break;
           
         default:
            VG_(message)(Vg_DebugMsg,"Warning: unhandled io_getevents opcode: %u\n",cb->aio_lio_opcode);
            break;
         }
      }
   }
}

PRE(sys_io_submit, 0)
{
   int i;

   PRINT("sys_io_submit( %llu, %lld, %p )", (ULong)ARG1,(Long)ARG2,ARG3);
   PRE_REG_READ3(long, "io_submit",
                 vki_aio_context_t, ctx_id, long, nr,
                 struct iocb **, iocbpp);
   PRE_MEM_READ( "io_submit(iocbpp)", ARG3, ARG2*sizeof(struct vki_iocb *) );
   if (ARG3 != 0) {
      for (i = 0; i < ARG2; i++) {
         struct vki_iocb *cb = ((struct vki_iocb **)ARG3)[i];
         PRE_MEM_READ( "io_submit(iocb)", (Addr)cb, sizeof(struct vki_iocb) );
         switch (cb->aio_lio_opcode) {
         case VKI_IOCB_CMD_PREAD:
            PRE_MEM_WRITE( "io_submit(PREAD)", cb->aio_buf, cb->aio_nbytes );
            break;

         case VKI_IOCB_CMD_PWRITE:
            PRE_MEM_READ( "io_submit(PWRITE)", cb->aio_buf, cb->aio_nbytes );
            break;
           
         default:
            VG_(message)(Vg_DebugMsg,"Warning: unhandled io_submit opcode: %u\n",
                         cb->aio_lio_opcode);
            break;
         }
      }
   }
}

PRE(sys_io_cancel, 0)
{
   PRINT("sys_io_cancel( %llu, %p, %p )", (ULong)ARG1,ARG2,ARG3);
   PRE_REG_READ3(long, "io_cancel",
                 vki_aio_context_t, ctx_id, struct iocb *, iocb,
                 struct io_event *, result);
   PRE_MEM_READ( "io_cancel(iocb)", ARG2, sizeof(struct vki_iocb) );
   PRE_MEM_WRITE( "io_cancel(result)", ARG3, sizeof(struct vki_io_event) );
}

POST(sys_io_cancel)
{
   POST_MEM_WRITE( ARG3, sizeof(struct vki_io_event) );
}

#undef PRE
#undef POST

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
