
/*--------------------------------------------------------------------*/
/*--- Linux-specific syscalls, etc.               linux/syscalls.c ---*/
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

#include "core.h"
#include "syscall_wrappers.h"

/* ---------------------------------------------------------------------
   PRE/POST wrappers for arch-generic, Linux-specific syscalls
   ------------------------------------------------------------------ */

// Nb: See the comment above the generic PRE/POST wrappers in
// coregrind/vg_syscalls.c for notes about how they work.

#define PRE(x,f) \
   UInt VGA_(linux_##x##_flags) = f; \
   void VGA_(linux_##x##_before)(ThreadId tid, ThreadState *tst)
#define POST(x) \
   void VGA_(linux_##x##_after) (ThreadId tid, ThreadState *tst)

#define SYSNO	PLATFORM_SYSCALL_NUM(tst->arch)    // in PRE(x)
#define res	PLATFORM_SYSCALL_RET(tst->arch)	   // in POST(x)
#define arg1	PLATFORM_SYSCALL_ARG1(tst->arch)
#define arg2	PLATFORM_SYSCALL_ARG2(tst->arch)
#define arg3	PLATFORM_SYSCALL_ARG3(tst->arch)
#define arg4	PLATFORM_SYSCALL_ARG4(tst->arch)
#define arg5	PLATFORM_SYSCALL_ARG5(tst->arch)
#define arg6	PLATFORM_SYSCALL_ARG6(tst->arch)

#define set_result(val) PLATFORM_SET_SYSCALL_RESULT(tst->arch, (val))

#define PRINT(format, args...)  \
   if (VG_(clo_trace_syscalls))        \
      VG_(printf)(format, ## args)


PRE(sys_mount, MayBlock)
{
   // Nb: depending on 'flags', the 'type' and 'data' args may be ignored.
   // We are conservative and check everything, except the memory pointed to
   // by 'data'.
   PRINT( "sys_mount( %p, %p, %p, %p, %p )" ,arg1,arg2,arg3);
   PRE_REG_READ5(long, "mount",
                 char *, source, char *, target, char *, type,
                 unsigned long, flags, void *, data);
   PRE_MEM_RASCIIZ( "mount(source)", arg1);
   PRE_MEM_RASCIIZ( "mount(target)", arg2);
   PRE_MEM_RASCIIZ( "mount(type)", arg3);
}

PRE(sys_oldumount, 0)
{
   PRINT("sys_oldumount( %p )", arg1);
   PRE_REG_READ1(long, "umount", char *, path);
   PRE_MEM_RASCIIZ( "umount(path)", arg1);
}

PRE(sys_umount, 0)
{
   PRINT("sys_umount( %p )", arg1);
   PRE_REG_READ2(long, "umount2", char *, path, int, flags);
   PRE_MEM_RASCIIZ( "umount2(path)", arg1);
}

PRE(sys_llseek, 0)
{
   PRINT("sys_llseek ( %d, 0x%x, 0x%x, %p, %d )", arg1,arg2,arg3,arg4,arg5);
   PRE_REG_READ5(long, "llseek",
                 unsigned int, fd, unsigned long, offset_high,
                 unsigned long, offset_low, vki_loff_t *, result,
                 unsigned int, whence);
   PRE_MEM_WRITE( "llseek(result)", arg4, sizeof(vki_loff_t));
}

POST(sys_llseek)
{
   if (res == 0)
      POST_MEM_WRITE( arg4, sizeof(vki_loff_t) );
}

PRE(sys_adjtimex, 0)
{
   struct vki_timex *tx = (struct vki_timex *)arg1;
   PRINT("sys_adjtimex ( %p )", arg1);
   PRE_REG_READ1(long, "adjtimex", struct timex *, buf);
   PRE_MEM_READ( "adjtimex(timex->modes)", arg1, sizeof(tx->modes));

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
   
   PRE_MEM_WRITE( "adjtimex(timex)", arg1, sizeof(struct vki_timex));
}

POST(sys_adjtimex)
{
   VG_TRACK(post_mem_write, arg1, sizeof(struct vki_timex));
}

PRE(sys_setfsuid16, 0)
{
   PRINT("sys_setfsuid16 ( %d )", arg1);
   PRE_REG_READ1(long, "setfsuid16", vki_old_uid_t, uid);
}

PRE(sys_setfsuid, 0)
{
   PRINT("sys_setfsuid ( %d )", arg1);
   PRE_REG_READ1(long, "setfsuid", vki_uid_t, uid);
}

PRE(sys_setfsgid16, 0)
{
   PRINT("sys_setfsgid16 ( %d )", arg1);
   PRE_REG_READ1(long, "setfsgid16", vki_old_gid_t, gid);
}

PRE(sys_setfsgid, 0)
{
   PRINT("sys_setfsgid ( %d )", arg1);
   PRE_REG_READ1(long, "setfsgid", vki_gid_t, gid);
}

PRE(sys_setresuid16, 0)
{
   PRINT("sys_setresuid16 ( %d, %d, %d )", arg1, arg2, arg3);
   PRE_REG_READ3(long, "setresuid16",
                 vki_old_uid_t, ruid, vki_old_uid_t, euid, vki_old_uid_t, suid);
}

PRE(sys_setresuid, 0)
{
   PRINT("sys_setresuid ( %d, %d, %d )", arg1, arg2, arg3);
   PRE_REG_READ3(long, "setresuid",
                 vki_uid_t, ruid, vki_uid_t, euid, vki_uid_t, suid);
}

PRE(sys_getresuid16, 0)
{
   PRINT("sys_getresuid16 ( %p, %p, %p )", arg1,arg2,arg3);
   PRE_REG_READ3(long, "getresuid16",
                 vki_old_uid_t *, ruid, vki_old_uid_t *, euid,
                 vki_old_uid_t *, suid);
   PRE_MEM_WRITE( "getresuid16(ruid)", arg1, sizeof(vki_old_uid_t) );
   PRE_MEM_WRITE( "getresuid16(euid)", arg2, sizeof(vki_old_uid_t) );
   PRE_MEM_WRITE( "getresuid16(suid)", arg3, sizeof(vki_old_uid_t) );
}

POST(sys_getresuid16)
{
   if (res == 0) {
      POST_MEM_WRITE( arg1, sizeof(vki_old_uid_t) );
      POST_MEM_WRITE( arg2, sizeof(vki_old_uid_t) );
      POST_MEM_WRITE( arg3, sizeof(vki_old_uid_t) );
   }
}

PRE(sys_getresuid, 0)
{
   PRINT("sys_getresuid ( %p, %p, %p )", arg1,arg2,arg3);
   PRE_REG_READ3(long, "getresuid", 
                 vki_uid_t *, ruid, vki_uid_t *, euid, vki_uid_t *, suid);
   PRE_MEM_WRITE( "getresuid(ruid)", arg1, sizeof(vki_uid_t) );
   PRE_MEM_WRITE( "getresuid(euid)", arg2, sizeof(vki_uid_t) );
   PRE_MEM_WRITE( "getresuid(suid)", arg3, sizeof(vki_uid_t) );
}

POST(sys_getresuid)
{
   if (res == 0) {
      POST_MEM_WRITE( arg1, sizeof(vki_uid_t) );
      POST_MEM_WRITE( arg2, sizeof(vki_uid_t) );
      POST_MEM_WRITE( arg3, sizeof(vki_uid_t) );
   }
}

PRE(sys_setresgid16, 0)
{
   PRINT("sys_setresgid16 ( %d, %d, %d )", arg1, arg2, arg3);
   PRE_REG_READ3(long, "setresgid16",
                 vki_old_gid_t, rgid, vki_old_gid_t, egid, vki_old_gid_t, sgid);
}

PRE(sys_setresgid, 0)
{
   PRINT("sys_setresgid ( %d, %d, %d )", arg1, arg2, arg3);
   PRE_REG_READ3(long, "setresgid",
                 vki_gid_t, rgid, vki_gid_t, egid, vki_gid_t, sgid);
}

PRE(sys_getresgid16, 0)
{
   PRINT("sys_getresgid16 ( %p, %p, %p )", arg1,arg2,arg3);
   PRE_REG_READ3(long, "getresgid16",
                 vki_old_gid_t *, rgid, vki_old_gid_t *, egid,
                 vki_old_gid_t *, sgid);
   PRE_MEM_WRITE( "getresgid16(rgid)", arg1, sizeof(vki_old_gid_t) );
   PRE_MEM_WRITE( "getresgid16(egid)", arg2, sizeof(vki_old_gid_t) );
   PRE_MEM_WRITE( "getresgid16(sgid)", arg3, sizeof(vki_old_gid_t) );
}

POST(sys_getresgid16)
{
   if (res == 0) {
      POST_MEM_WRITE( arg1, sizeof(vki_old_gid_t) );
      POST_MEM_WRITE( arg2, sizeof(vki_old_gid_t) );
      POST_MEM_WRITE( arg3, sizeof(vki_old_gid_t) );
   }
}

PRE(sys_getresgid, 0)
{
   PRINT("sys_getresgid ( %p, %p, %p )", arg1,arg2,arg3);
   PRE_REG_READ3(long, "getresgid", 
                 vki_gid_t *, rgid, vki_gid_t *, egid, vki_gid_t *, sgid);
   PRE_MEM_WRITE( "getresgid(rgid)", arg1, sizeof(vki_gid_t) );
   PRE_MEM_WRITE( "getresgid(egid)", arg2, sizeof(vki_gid_t) );
   PRE_MEM_WRITE( "getresgid(sgid)", arg3, sizeof(vki_gid_t) );
}

POST(sys_getresgid)
{
   if (res == 0) {
      POST_MEM_WRITE( arg1, sizeof(vki_gid_t) );
      POST_MEM_WRITE( arg2, sizeof(vki_gid_t) );
      POST_MEM_WRITE( arg3, sizeof(vki_gid_t) );
   }
}

PRE(sys_ioperm, 0)
{
   PRINT("sys_ioperm ( %d, %d, %d )", arg1, arg2, arg3 );
   PRE_REG_READ3(long, "ioperm",
                 unsigned long, from, unsigned long, num, int, turn_on);
}

PRE(sys_syslog, MayBlock)
{
   PRINT("sys_syslog (%d, %p, %d)", arg1,arg2,arg3);
   PRE_REG_READ3(long, "syslog", int, type, char *, bufp, int, len);
   switch (arg1) {
   case 2: case 3: case 4:
      PRE_MEM_WRITE( "syslog(bufp)", arg2, arg3);
      break;
   default: 
      break;
   }
}

POST(sys_syslog)
{
   switch (arg1) {
   case 2: case 3: case 4:
      POST_MEM_WRITE( arg2, arg3 );
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
   PRINT("sys_sysinfo ( %p )",arg1);
   PRE_REG_READ1(long, "sysinfo", struct sysinfo *, info);
   PRE_MEM_WRITE( "sysinfo(info)", arg1, sizeof(struct vki_sysinfo) );
}

POST(sys_sysinfo)
{
   POST_MEM_WRITE( arg1, sizeof(struct vki_sysinfo) );
}

PRE(sys_personality, 0)
{
   PRINT("sys_personality ( %llu )", (ULong)arg1);
   PRE_REG_READ1(long, "personality", vki_u_long, persona);
}

PRE(sys_sysctl, 0)
{
   PRINT("sys_sysctl ( %p )", arg1 );
   PRE_REG_READ1(long, "sysctl", struct __sysctl_args *, args);
   PRE_MEM_WRITE( "sysctl(args)", arg1, sizeof(struct __vki_sysctl_args) );
}

POST(sys_sysctl)
{
   POST_MEM_WRITE( arg1, sizeof(struct __vki_sysctl_args) );
}

PRE(sys_prctl, MayBlock)
{
   PRINT( "prctl ( %d, %d, %d, %d, %d )", arg1, arg2, arg3, arg4, arg5 );
   // XXX: too simplistic, often not all args are used
   // Nb: can't use "arg2".."arg5" here because that's our own macro...
   PRE_REG_READ5(long, "prctl",
                 int, option, unsigned long, parg2, unsigned long, parg3,
                 unsigned long, parg4, unsigned long, parg5);
   // XXX: totally wrong... we need to look at the 'option' arg, and do
   // PRE_MEM_READs/PRE_MEM_WRITEs as necessary...
}

// Nb: this wrapper is "Special" because we have to pad/unpad memory around
// the syscall itself, and this allows us to control exactly the code that
// gets run while the padding is in place.
PRE(sys_io_setup, Special)
{
   SizeT size;
   Addr addr;

   PRINT("sys_io_setup ( %u, %p )", arg1,arg2);
   PRE_REG_READ2(long, "io_setup",
                 unsigned, nr_events, vki_aio_context_t *, ctxp);
   PRE_MEM_WRITE( "io_setup(ctxp)", arg2, sizeof(vki_aio_context_t) );
   
   size = PGROUNDUP(sizeof(struct vki_aio_ring) +
                    arg1*sizeof(struct vki_io_event));
   addr = VG_(find_map_space)(0, size, True);
   VG_(map_segment)(addr, size, VKI_PROT_READ|VKI_PROT_EXEC, SF_FIXED);
   
   VG_(pad_address_space)();
   set_result( VG_(do_syscall)(SYSNO, arg1, arg2) );
   VG_(unpad_address_space)();

   if (res == 0) {
      struct vki_aio_ring *r = *(struct vki_aio_ring **)arg2;
        
      vg_assert(addr == (Addr)r);
      vg_assert(VG_(valid_client_addr)(addr, size, tid, "io_setup"));
                
      VG_TRACK( new_mem_mmap, addr, size, True, True, False );
      POST_MEM_WRITE( arg2, sizeof(vki_aio_context_t) );
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
PRE(sys_io_destroy, Special)
{     
   Segment *s = VG_(find_segment)(arg1);
   struct vki_aio_ring *r;
   SizeT size;
      
   PRINT("sys_io_destroy ( %llu )", (ULong)arg1);
   PRE_REG_READ1(long, "io_destroy", vki_aio_context_t, ctx);

   // If we are going to seg fault (due to a bogus arg1) do it as late as
   // possible...
   r = *(struct vki_aio_ring **)arg1;
   size = PGROUNDUP(sizeof(struct vki_aio_ring) + 
                    r->nr*sizeof(struct vki_io_event));

   set_result( VG_(do_syscall)(SYSNO, arg1) );

   if (res == 0 && s != NULL && VG_(seg_contains)(s, arg1, size)) { 
      VG_TRACK( die_mem_munmap, arg1, size );
      VG_(unmap_range)(arg1, size);
   }  
}  

PRE(sys_io_getevents, MayBlock)
{
   PRINT("sys_io_getevents ( %llu, %lld, %lld, %p, %p )",
         (ULong)arg1,(Long)arg2,(Long)arg3,arg4,arg5);
   PRE_REG_READ5(long, "io_getevents",
                 vki_aio_context_t, ctx_id, long, min_nr, long, nr,
                 struct io_event *, events,
                 struct timespec *, timeout);
   if (arg3 > 0)
      PRE_MEM_WRITE( "io_getevents(events)",
                     arg4, sizeof(struct vki_io_event)*arg3 );
   if (arg5 != (UWord)NULL)
      PRE_MEM_READ( "io_getevents(timeout)",
                     arg5, sizeof(struct vki_timespec));
}

POST(sys_io_getevents)
{
   int i;

   if (res > 0) {
      POST_MEM_WRITE( arg4, sizeof(struct vki_io_event)*res );
      for (i = 0; i < res; i++) {
         const struct vki_io_event *vev = ((struct vki_io_event *)arg4) + i;
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

   PRINT("sys_io_submit( %llu, %lld, %p )", (ULong)arg1,(Long)arg2,arg3);
   PRE_REG_READ3(long, "io_submit",
                 vki_aio_context_t, ctx_id, long, nr,
                 struct iocb **, iocbpp);
   PRE_MEM_READ( "io_submit(iocbpp)", arg3, arg2*sizeof(struct vki_iocb *) );
   if (arg3 != (UWord)NULL) {
      for (i = 0; i < arg2; i++) {
         struct vki_iocb *cb = ((struct vki_iocb **)arg3)[i];
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
   PRINT("sys_io_cancel( %llu, %p, %p )", (ULong)arg1,arg2,arg3);
   PRE_REG_READ3(long, "io_cancel",
                 vki_aio_context_t, ctx_id, struct iocb *, iocb,
                 struct io_event *, result);
   PRE_MEM_READ( "io_cancel(iocb)", arg2, sizeof(struct vki_iocb) );
   PRE_MEM_WRITE( "io_cancel(result)", arg3, sizeof(struct vki_io_event) );
}

POST(sys_io_cancel)
{
   POST_MEM_WRITE( arg3, sizeof(struct vki_io_event) );
}

#undef PRE
#undef POST

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
