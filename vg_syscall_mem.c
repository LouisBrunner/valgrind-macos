
/*--------------------------------------------------------------------*/
/*--- Update the byte permission maps following a system call.     ---*/
/*---                                             vg_syscall_mem.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an x86 protected-mode emulator 
   designed for debugging and profiling binaries on x86-Unixes.

   Copyright (C) 2000-2002 Julian Seward 
      jseward@acm.org
      Julian_Seward@muraroa.demon.co.uk

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

   The GNU General Public License is contained in the file LICENSE.
*/

#include "vg_include.h"

/* vg_unsafe.h should NOT be included into any file except this
   one. */
#include "vg_unsafe.h"


/* All system calls are channelled through vg_wrap_syscall.  It does
   three things:

   * optionally, checks the permissions for the args to the call

   * perform the syscall, usually by passing it along to the kernel
     unmodified.  However, because we simulate signals ourselves,
     signal-related syscalls are routed to vg_signal.c, and are not
     delivered to the kernel.

   * Update the permission maps following the syscall.

   A magical piece of assembly code, vg_do_syscall(), in vg_syscall.S
   does the tricky bit of passing a syscall to the kernel, whilst
   having the simulator retain control.
*/

static void make_noaccess ( Addr a, UInt len )
{
   if (VG_(clo_instrument))
      VGM_(make_noaccess) ( a, len );
}

static void make_writable ( Addr a, UInt len )
{
   if (VG_(clo_instrument))
      VGM_(make_writable) ( a, len );
}

static void make_readable ( Addr a, UInt len )
{
   if (VG_(clo_instrument))
      VGM_(make_readable) ( a, len );
}

static void make_readwritable ( Addr a, UInt len )
{
   if (VG_(clo_instrument))
      VGM_(make_readwritable) ( a, len );
}

static
void must_be_writable ( Char* syscall_name, UInt base, UInt size )
{
   Bool ok;
   Addr bad_addr;
   /* VG_(message)(Vg_DebugMsg,"must be writable: %x .. %x",
                               base,base+size-1); */
   if (!VG_(clo_instrument)) 
      return;
   ok = VGM_(check_writable) ( base, size, &bad_addr );
   if (!ok)
      VG_(record_param_err) ( bad_addr, True, syscall_name );
}

static
void must_be_readable ( Char* syscall_name, UInt base, UInt size )
{
   Bool ok;
   Addr bad_addr;
   /* VG_(message)(Vg_DebugMsg,"must be readable: %x .. %x",
                               base,base+size-1); */
   if (!VG_(clo_instrument)) 
      return;
   ok = VGM_(check_readable) ( base, size, &bad_addr );
   if (!ok)
      VG_(record_param_err) ( bad_addr, False, syscall_name );
}

static
void must_be_readable_asciiz ( Char* syscall_name, UInt str )
{
   Bool ok = True;
   Addr bad_addr;
   /* VG_(message)(Vg_DebugMsg,"must be readable asciiz: 0x%x",str); */
   if (!VG_(clo_instrument)) 
      return;
   ok = VGM_(check_readable_asciiz) ( (Addr)str, &bad_addr );
   if (!ok)
      VG_(record_param_err) ( bad_addr, False, syscall_name );
}


/* Set memory permissions, based on PROT_* values for mmap/mprotect,
   into the permissions our scheme understands.  Dunno if this is
   really correct.  */

static void approximate_mmap_permissions ( Addr a, UInt len, UInt prot )
{
   /* PROT_READ and PROT_WRITE --> readable
      PROT_READ only           --> readable
      PROT_WRITE only          --> writable
      NEITHER                  --> noaccess
   */
   if (prot & PROT_READ)
      make_readable(a,len);
   else
   if (prot & PROT_WRITE)
      make_writable(a,len);
   else
      make_noaccess(a,len);
}


/* Dereference a pointer, but only after checking that it's
   safe to do so.  If not, return the default.
*/
static
UInt safe_dereference ( Addr aa, UInt defawlt )
{
   if (!VG_(clo_instrument)) 
      return * (UInt*)aa;
   if (VGM_(check_readable)(aa,4,NULL))
      return * (UInt*)aa;
   else
      return defawlt;
}


/* Is this a Linux kernel error return value? */
/* From:
   http://sources.redhat.com/cgi-bin/cvsweb.cgi/libc/sysdeps/unix/sysv/
   linux/i386/sysdep.h?
   rev=1.28&content-type=text/x-cvsweb-markup&cvsroot=glibc

   QUOTE:

   Linux uses a negative return value to indicate syscall errors,
   unlike most Unices, which use the condition codes' carry flag.

   Since version 2.1 the return value of a system call might be
   negative even if the call succeeded.  E.g., the `lseek' system call
   might return a large offset.  Therefore we must not anymore test
   for < 0, but test for a real error by making sure the value in %eax
   is a real error number.  Linus said he will make sure the no syscall
   returns a value in -1 .. -4095 as a valid result so we can savely
   test with -4095.  

   END QUOTE
*/
Bool VG_(is_kerror) ( Int res )
{
   if (res >= -4095 && res <= -1)
      return True;
   else
      return False;
}

static
UInt get_shm_size ( Int shmid )
{
   struct shmid_ds buf;
   long __res;
    __asm__ volatile ( "int $0x80"
                       : "=a" (__res)
                       : "0" (__NR_ipc),
                         "b" ((long)(24) /*IPCOP_shmctl*/),
                         "c" ((long)(shmid)),
                         "d" ((long)(IPC_STAT)),
                         "S" ((long)(0)),
                         "D" ((long)(&buf)) );
    if ( VG_(is_kerror) ( __res ) )
       return 0;
 
   return buf.shm_segsz;
}
 
static
Char *strdupcat( const Char *s1, const Char *s2, ArenaId aid )
{
   UInt len = VG_(strlen) ( s1 ) + VG_(strlen) ( s2 ) + 1;
   Char *result = VG_(malloc) ( aid, len );
   VG_(strcpy) ( result, s1 );
   VG_(strcat) ( result, s2 );
   return result;
}

static 
void must_be_readable_sendmsg( Char *msg, UInt base, UInt size )
{
   Char *outmsg = strdupcat ( "socketcall.sendmsg", msg, VG_AR_TRANSIENT );
   must_be_readable ( outmsg, base, size );
   VG_(free) ( VG_AR_TRANSIENT, outmsg );
}

static 
void must_be_writable_recvmsg( Char *msg, UInt base, UInt size )
{
   Char *outmsg = strdupcat ( "socketcall.recvmsg", msg, VG_AR_TRANSIENT );
   must_be_writable ( outmsg, base, size );
   VG_(free) ( VG_AR_TRANSIENT, outmsg );
}

static
void make_readable_recvmsg( Char *fieldName, UInt base, UInt size )
{
   make_readable( base, size );
}
 
static
void msghdr_foreachfield ( struct msghdr *msg, 
                           void (*foreach_func)( Char *, UInt, UInt ) )
{
   if ( !msg )
      return;

   foreach_func ( "(msg)", (Addr)msg, sizeof( struct msghdr ) );

   if ( msg->msg_name )
      foreach_func ( "(msg.msg_name)", 
                     (Addr)msg->msg_name, msg->msg_namelen );

   if ( msg->msg_iov ) {
      struct iovec *iov = msg->msg_iov;
      UInt i;

      foreach_func ( "(msg.msg_iov)", 
                     (Addr)iov, msg->msg_iovlen * sizeof( struct iovec ) );

      for ( i = 0; i < msg->msg_iovlen; ++i, ++iov )
         foreach_func ( "(msg.msg_iov[i]", 
                        (Addr)iov->iov_base, iov->iov_len );
   }

   if ( msg->msg_control )
      foreach_func ( "(msg.msg_control)", 
                     (Addr)msg->msg_control, msg->msg_controllen );
}


/* Records the current end of the data segment so we can make sense of
   calls to brk().  Initial value set by hdm_init_memory_audit(). */
Addr VGM_(curr_dataseg_end);


/* The Main Entertainment ... */

void VG_(wrap_syscall) ( void )
{
   Bool sane_before_call = True;
   Bool sane_after_call  = True;

   UInt syscallno = VG_(baseBlock)[VGOFF_(m_eax)];
   UInt arg1      = VG_(baseBlock)[VGOFF_(m_ebx)];
   UInt arg2      = VG_(baseBlock)[VGOFF_(m_ecx)];
   UInt arg3      = VG_(baseBlock)[VGOFF_(m_edx)];
   UInt arg4      = VG_(baseBlock)[VGOFF_(m_esi)];
   UInt arg5      = VG_(baseBlock)[VGOFF_(m_edi)];

   /* Do not make this unsigned! */
   Int res;

   /* Keep track of nested syscalls, and do some sanity checks. */
   Int syscall_depth_saved = VG_(syscall_depth);
   if (VG_(syscall_depth) > 1)
     VG_(unimplemented)
        ("recursion between blocked syscalls and signal handlers");
   vg_assert( VG_(syscall_depth) == 0 || VG_(syscall_depth) == 1 );
   VG_(syscall_depth) ++;

   VGP_PUSHCC(VgpSyscall);

   /* Since buggy syscall wrappers sometimes break this, we may as well 
      check ourselves. */
   if (! VG_(first_and_last_secondaries_look_plausible))
      sane_before_call = False;

   /* the syscall no is in %eax.  For syscalls with <= 5 args,
      args 1 .. 5 to the syscall are in %ebx %ecx %edx %esi %edi.
      For calls with > 5 args, %ebx points to a lump of memory
      containing the args.

      The result is returned in %eax.  If this value >= 0, the call
      succeeded, and this is the return value.  If < 0, it failed, and
      the negation of this value is errno.  To be more specific, 
      if res is in the range -EMEDIUMTYPE (-124) .. -EPERM (-1)
      (kernel 2.4.9 sources, include/asm-i386/errno.h)
      then it indicates an error.  Otherwise it doesn't.

      Dirk Mueller (mueller@kde.org) says that values -4095 .. -1
      (inclusive?) indicate error returns.  Not sure where the -4095
      comes from.
   */

   if (VG_(clo_trace_syscalls))
      VG_(printf)("SYSCALL[%d, %d](%3d): ", 
                  VG_(syscall_depth), VG_(getpid)(), syscallno);

   switch (syscallno) {

      case __NR_sigaltstack:
         VG_(unimplemented)
            ("client signals on alternative stack (SA_ONSTACK)");
         break;

      case __NR_clone:
         VG_(unimplemented)
            ("clone(): Valgrind doesn't support threads; sorry.");
         break;

#     if defined(__NR_modify_ldt)
      case __NR_modify_ldt:
         VG_(unimplemented)
            ("modify_ldt(): I (JRS) haven't investigated this yet; sorry.");
         break;
#     endif

      /* !!!!!!!!!! New, untested syscalls, 14 Mar 02 !!!!!!!!!! */

#     if defined(__NR_setresgid32)
      case __NR_setresgid32: /* syscall 210 */
         /* int setresgid(gid_t rgid, gid_t egid, gid_t sgid); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("setresgid32 ( %d, %d, %d )\n", arg1, arg2, arg3);
         KERNEL_DO_SYSCALL(res);
         break;
#     endif

#     if defined(__NR_setfsuid32)
      case __NR_setfsuid32: /* syscall 215 */
         /* int setfsuid(uid_t fsuid); */
          if (VG_(clo_trace_syscalls))
             VG_(printf)("setfsuid ( %d )\n", arg1);
          KERNEL_DO_SYSCALL(res);
          break;
#     endif

#     if defined(__NR__sysctl)
      case __NR__sysctl:
      /* int _sysctl(struct __sysctl_args *args); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("_sysctl ( %p )\n", arg1 );
         must_be_writable ( "_sysctl(args)", arg1, 
                            sizeof(struct __sysctl_args) );
         KERNEL_DO_SYSCALL(res);
         if (!VG_(is_kerror)(res))
            make_readable ( arg1, sizeof(struct __sysctl_args) );
         break;
#     endif

#     if defined(__NR_sched_getscheduler)
      case __NR_sched_getscheduler:
         /* int sched_getscheduler(pid_t pid); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("sched_getscheduler ( %d )\n", arg1);
         KERNEL_DO_SYSCALL(res);
         break;
#     endif

#     if defined(__NR_sched_setscheduler)
      case __NR_sched_setscheduler:
         /* int sched_setscheduler(pid_t pid, int policy, 
                const struct sched_param *p); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("sched_setscheduler ( %d, %d, %p )\n",arg1,arg2,arg3);
         if (arg3 != (UInt)NULL)
            must_be_readable( "sched_setscheduler(struct sched_param *p)", 
                              arg3, sizeof(struct sched_param));
         KERNEL_DO_SYSCALL(res);
         break;
#     endif

#     if defined(__NR_mlockall)
      case __NR_mlockall:
         /* int mlockall(int flags); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("mlockall ( %x )\n", arg1);
         KERNEL_DO_SYSCALL(res);
         break;
#     endif

#     if defined(__NR_munlockall)
      case __NR_munlockall:
         /* int munlockall(void); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("munlockall ( )\n");
         KERNEL_DO_SYSCALL(res);
         break;
#     endif

#if   defined(__NR_sched_get_priority_max)
      case __NR_sched_get_priority_max:
         /* int sched_get_priority_max(int policy); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("sched_get_priority_max ( %d )\n", arg1);
         KERNEL_DO_SYSCALL(res);
         break;
#     endif

#     if defined(__NR_setfsgid)
      case __NR_setfsgid: /* syscall 139 */
         /* int setfsgid(gid_t gid); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("setfsgid ( %d )\n", arg1);
         KERNEL_DO_SYSCALL(res);
         break;
#     endif

#     if defined(__NR_setregid)
      case __NR_setregid: /* syscall 71 */
         /* int setregid(gid_t rgid, gid_t egid); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("setregid ( %d, %d )\n", arg1, arg2);
         KERNEL_DO_SYSCALL(res);
         break;
#     endif

#     if defined(__NR_setresuid)
      case __NR_setresuid: /* syscall 164 */
         /* int setresuid(uid_t ruid, uid_t euid, uid_t suid); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("setresuid ( %d, %d, %d )\n", arg1, arg2, arg3);
         KERNEL_DO_SYSCALL(res);
         break;
#     endif

#     if defined(__NR_setfsuid)
      case __NR_setfsuid: /* syscall 138 */
         /* int setfsuid(uid_t uid); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("setfsuid ( %d )\n", arg1);
         KERNEL_DO_SYSCALL(res);
         break;
#     endif

      /* !!!!!!!!!! New, untested syscalls, 8 Mar 02 !!!!!!!!!!! */

#     if defined(__NR_sendfile)
      case __NR_sendfile: /* syscall 187 */
         /* ssize_t sendfile(int out_fd, int in_fd, off_t *offset, 
                             size_t count) */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("sendfile ( %d, %d, %p, %d )\n",arg1,arg2,arg3,arg4);
         must_be_writable( "sendfile(offset)", arg3, sizeof(off_t) );
         KERNEL_DO_SYSCALL(res);
         if (!VG_(is_kerror)(res)) {
            make_readable( arg3, sizeof( off_t ) );
         }
         break;
#     endif

      /* !!!!!!!!!! New, untested syscalls, 7 Mar 02 !!!!!!!!!!! */

#     if defined(__NR_pwrite)
      case __NR_pwrite: /* syscall 181 */
         /* ssize_t pwrite (int fd, const void *buf, size_t nbytes,
                            off_t offset); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("pwrite ( %d, %p, %d, %d )\n", arg1, arg2, arg3, arg4);
         must_be_readable( "pwrite(buf)", arg2, arg3 );
         KERNEL_DO_SYSCALL(res);
         break;
#     endif

      /* !!!!!!!!!! New, untested syscalls, 6 Mar 02 !!!!!!!!!!! */

      case __NR_sync: /* syscall 36 */
         /* int sync(); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("sync ( )\n");
         KERNEL_DO_SYSCALL(res);
         break; 
 
      case __NR_fstatfs: /* syscall 100 */
         /* int fstatfs(int fd, struct statfs *buf); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("fstatfs ( %d, %p )\n",arg1,arg2);
         must_be_writable( "stat(buf)", arg2, sizeof(struct statfs) );
         KERNEL_DO_SYSCALL(res);
         if (!VG_(is_kerror)(res))
            make_readable( arg2, sizeof(struct statfs) );
         break;

      /* !!!!!!!!!! New, untested syscalls, 4 Mar 02 !!!!!!!!!!! */

      case __NR_pause: /* syscall 29 */
         /* int pause(void); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("pause ( )\n");
         KERNEL_DO_SYSCALL(res);
         break;

      case __NR_getsid: /* syscall 147 */
         /* pid_t getsid(pid_t pid); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("getsid ( %d )\n", arg1);
         KERNEL_DO_SYSCALL(res);
         break;

#     if defined(__NR_pread)
      case __NR_pread: /* syscall 180 */
         /* ssize_t pread(int fd, void *buf, size_t count, off_t offset); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("pread ( %d, %p, %d, %d ) ...\n",arg1,arg2,arg3,arg4);
         must_be_writable( "pread(buf)", arg2, arg3 );
         KERNEL_DO_SYSCALL(res);
         if (VG_(clo_trace_syscalls))
            VG_(printf)("SYSCALL[%d, %d]       pread ( %d, %p, %d, %d ) --> %d\n",
                        VG_(syscall_depth), VG_(getpid)(),
                        arg1, arg2, arg3, arg4, res);
         if (!VG_(is_kerror)(res) && res > 0) {
            make_readable( arg2, res );
         }
         break;
#     endif

      /* !!!!!!!!!! New, untested syscalls, 27 Feb 02 !!!!!!!!!! */

      case __NR_mknod: /* syscall 14 */
         /* int mknod(const char *pathname, mode_t mode, dev_t dev); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("mknod ( %p, 0x%x, 0x%x )\n", arg1, arg2, arg3 );
         must_be_readable_asciiz( "mknod(pathname)", arg1 );
         KERNEL_DO_SYSCALL(res);
         break;

      case __NR_flock: /* syscall 143 */
         /* int flock(int fd, int operation); */
         if (VG_(clo_trace_syscalls)) 
            VG_(printf)("flock ( %d, %d )\n", arg1, arg2 );
         KERNEL_DO_SYSCALL(res);
         break;

#     if defined(__NR_rt_sigsuspend)
      /* Viewed with great suspicion by me, but, hey, let's do it
         anyway ... */
      case __NR_rt_sigsuspend: /* syscall 179 */
         /* int sigsuspend(const sigset_t *mask); */
         if (VG_(clo_trace_syscalls)) 
            VG_(printf)("sigsuspend ( %p )\n", arg1 );
         if (arg1 != (Addr)NULL) {
            /* above NULL test is paranoia */
            must_be_readable( "sigsuspend(mask)", arg1, 
                              sizeof(vki_ksigset_t) );
         }
         KERNEL_DO_SYSCALL(res);
         break;
#     endif

      case __NR_init_module: /* syscall 128 */
         /* int init_module(const char *name, struct module *image); */
         if (VG_(clo_trace_syscalls)) 
            VG_(printf)("init_module ( %p, %p )\n", arg1, arg2 );
         must_be_readable_asciiz( "init_module(name)", arg1 );
         must_be_readable( "init_module(image)", arg2, 
                           sizeof(struct module) );
         KERNEL_DO_SYSCALL(res);
         break;

      case __NR_ioperm: /* syscall 101 */
         /* int ioperm(unsigned long from, unsigned long num, int turn_on); */
         if (VG_(clo_trace_syscalls)) 
            VG_(printf)("ioperm ( %d, %d, %d )\n", arg1, arg2, arg3 );
         KERNEL_DO_SYSCALL(res);
         break;

      case __NR_capget: /* syscall 184 */
         /* int capget(cap_user_header_t header, cap_user_data_t data); */
         if (VG_(clo_trace_syscalls)) 
            VG_(printf)("capget ( %p, %p )\n", arg1, arg2 );
         must_be_readable( "capget(header)", arg1, 
                                             sizeof(vki_cap_user_header_t) );
         must_be_writable( "capget(data)", arg2, 
                                           sizeof( vki_cap_user_data_t) );
         KERNEL_DO_SYSCALL(res);
         if (!VG_(is_kerror)(res) && arg2 != (Addr)NULL)
            make_readable ( arg2, sizeof( vki_cap_user_data_t) );
         break;

      /* !!!!!!!!!!!!!!!!!!!!! mutant ones !!!!!!!!!!!!!!!!!!!!! */

      case __NR_execve:
         /* int execve (const char *filename, 
                        char *const argv [], 
                        char *const envp[]); */
         if (VG_(clo_trace_syscalls)) 
            VG_(printf)("execve ( %p(%s), %p, %p ) --- NOT CHECKED\n", 
                        arg1, arg1, arg2, arg3);
         /* Make any binding for LD_PRELOAD disappear, so that child
            processes don't get traced into. */
         if (!VG_(clo_trace_children)) {
            Int i;
            Char** envp = (Char**)arg3;
            for (i = 0; envp[i] != NULL; i++) {
               if (VG_(strncmp)(envp[i], "LD_PRELOAD=", 11) == 0) {
                  VG_(mash_LD_PRELOAD_string)(&envp[i][11]);
               }
            }
         }
         KERNEL_DO_SYSCALL(res);
         /* Should we still be alive here?  Don't think so. */
         /* Actually, above comment is wrong.  execve can fail, just
            like any other syscall -- typically the file to exec does
            not exist.  Hence: */
         vg_assert(VG_(is_kerror)(res));
         break;

      case __NR_exit: /* syscall 1 */
         /* void _exit(int status); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("exit ( %d )\n", arg1);
         VG_(message)(Vg_UserMsg, 
            "Warning: client exiting by calling exit(%d).  Bye!",
            arg1);

         KERNEL_DO_SYSCALL(res);
         /* Definitely should not be alive here :) */
         break;

      /* !!!!!!!!!!!!!!!!!!!!!     end     !!!!!!!!!!!!!!!!!!!!! */

      case __NR_access: /* syscall 33 */
         /* int access(const char *pathname, int mode); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("access ( %p, %d )\n", arg1,arg2);
         must_be_readable_asciiz( "access(pathname)", arg1 );
         KERNEL_DO_SYSCALL(res);
         break;

      case __NR_alarm: /* syscall 27 */
         /* unsigned int alarm(unsigned int seconds); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("alarm ( %d )\n", arg1);
         KERNEL_DO_SYSCALL(res);
         break;

      case __NR_brk: /* syscall 45 */
         /* Haven't a clue if this is really right. */
         /* int brk(void *end_data_segment); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("brk ( %p ) --> ",arg1);
         KERNEL_DO_SYSCALL(res);
         if (VG_(clo_trace_syscalls)) 
            VG_(printf)("0x%x\n", res);

         if (!VG_(is_kerror)(res)) {
            if (arg1 == 0) {
               /* Just asking where the current end is. (???) */
               VGM_(curr_dataseg_end) = res;
            } else
            if (arg1 < VGM_(curr_dataseg_end)) {
               /* shrinking the data segment. */
               make_noaccess( (Addr)arg1, 
                              VGM_(curr_dataseg_end)-arg1 );
               VGM_(curr_dataseg_end) = arg1;
            } else
            if (arg1 > VGM_(curr_dataseg_end) && res != 0) {
               /* asked for more memory, and got it */
               /* 
               VG_(printf)("BRK: new area %x .. %x\n", 
                           VGM_(curr_dataseg_end, arg1-1 );
               */
               make_writable ( (Addr)VGM_(curr_dataseg_end), 
                               arg1-VGM_(curr_dataseg_end) );
               VGM_(curr_dataseg_end) = arg1;         
            }
         }
         break;

      case __NR_chdir: /* syscall 12 */
         /* int chdir(const char *path); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("chdir ( %p )\n", arg1);
         must_be_readable_asciiz( "chdir(path)", arg1 );
         KERNEL_DO_SYSCALL(res);
         break;

      case __NR_chmod: /* syscall 15 */
         /* int chmod(const char *path, mode_t mode); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("chmod ( %p, %d )\n", arg1,arg2);
         must_be_readable_asciiz( "chmod(path)", arg1 );
         KERNEL_DO_SYSCALL(res);
         break;

#     if defined(__NR_chown32)
      case __NR_chown32: /* syscall 212 */
#     endif
#     if defined(__NR_lchown32)
      case __NR_lchown32: /* syscall 198 */
#     endif
      case __NR_chown: /* syscall 16 */
         /* int chown(const char *path, uid_t owner, gid_t group); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("chown ( %p, 0x%x, 0x%x )\n", arg1,arg2,arg3);
         must_be_readable_asciiz( "chown(path)", arg1 );
         KERNEL_DO_SYSCALL(res);
         break;

      case __NR_close: /* syscall 6 */
         /* int close(int fd); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("close ( %d )\n",arg1);
         /* Detect and negate attempts by the client to close Valgrind's
            logfile fd ... */
         if (arg1 == VG_(clo_logfile_fd)) {
            VG_(message)(Vg_UserMsg, 
              "Warning: client attempted to close "
               "Valgrind's logfile fd (%d).", 
               VG_(clo_logfile_fd));
            VG_(message)(Vg_UserMsg, 
              "   Use --logfile-fd=<number> to select an "
              "alternative logfile fd." );
         } else {
            KERNEL_DO_SYSCALL(res);
         }
         break;

      case __NR_dup: /* syscall 41 */
         /* int dup(int oldfd); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("dup ( %d ) --> ", arg1);
         KERNEL_DO_SYSCALL(res);
         if (VG_(clo_trace_syscalls))
            VG_(printf)("%d\n", res);
         break;

      case __NR_dup2: /* syscall 63 */
         /* int dup2(int oldfd, int newfd); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("dup2 ( %d, %d ) ...\n", arg1,arg2);
         KERNEL_DO_SYSCALL(res);
         if (VG_(clo_trace_syscalls))
            VG_(printf)("SYSCALL[%d, %d]       dup2 ( %d, %d ) = %d\n", 
                        VG_(syscall_depth), VG_(getpid)(), 
                        arg1, arg2, res);
         break;

      case __NR_fcntl: /* syscall 55 */
         /* int fcntl(int fd, int cmd); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("fcntl ( %d, %d )\n",arg1,arg2);
         KERNEL_DO_SYSCALL(res);
         break;

      case __NR_fchdir: /* syscall 133 */
         /* int fchdir(int fd); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("fchdir ( %d )\n", arg1);
         KERNEL_DO_SYSCALL(res);
         break;

#     if defined(__NR_fchown32)
      case __NR_fchown32: /* syscall 207 */
#     endif
      case __NR_fchown: /* syscall 95 */
         /* int fchown(int filedes, uid_t owner, gid_t group); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("fchown ( %d, %d, %d )\n", arg1,arg2,arg3);
         KERNEL_DO_SYSCALL(res);
         break;

      case __NR_fchmod: /* syscall 94 */
         /* int fchmod(int fildes, mode_t mode); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("fchmod ( %d, %d )\n", arg1,arg2);
         KERNEL_DO_SYSCALL(res);
         break;

#     if defined(__NR_fcntl64)
      case __NR_fcntl64: /* syscall 221 */
         /* I don't know what the prototype for this is supposed to be. */
         /* ??? int fcntl(int fd, int cmd); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("fcntl64 (?!) ( %d, %d )\n", arg1,arg2);
         KERNEL_DO_SYSCALL(res);
         break;
#     endif

      case __NR_fstat: /* syscall 108 */
         /* int fstat(int filedes, struct stat *buf); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("fstat ( %d, %p )\n",arg1,arg2);
         must_be_writable( "fstat", arg2, sizeof(struct stat) );
         KERNEL_DO_SYSCALL(res);
         if (!VG_(is_kerror)(res))
            make_readable( arg2, sizeof(struct stat) );
         break;

      case __NR_vfork: /* syscall 190 */
         /* pid_t vfork(void); */
         if (VG_(clo_trace_syscalls)) 
            VG_(printf)("vfork ( ) ... becomes ... ");
         /* KLUDGE: we prefer to do a fork rather than vfork. 
            vfork gives a SIGSEGV, and the stated semantics looks
            pretty much impossible for us. */
         VG_(baseBlock)[VGOFF_(m_eax)] = __NR_fork;
         /* fall through ... */
      case __NR_fork: /* syscall 2 */
         /* pid_t fork(void); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("fork ()\n");
         KERNEL_DO_SYSCALL(res);
         break;

      case __NR_fsync: /* syscall 118 */
         /* int fsync(int fd); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("fsync ( %d )\n", arg1);
         KERNEL_DO_SYSCALL(res);
         break;

      case __NR_ftruncate: /* syscall 93 */
         /* int ftruncate(int fd, size_t length); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("ftruncate ( %d, %d )\n", arg1,arg2);
         KERNEL_DO_SYSCALL(res);
         break;

#     if defined(__NR_ftruncate64)
      case __NR_ftruncate64: /* syscall 194 */
         /* int ftruncate64(int fd, off64_t length); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("ftruncate64 ( %d, %lld )\n", 
                        arg1,arg2|((long long) arg3 << 32));
         KERNEL_DO_SYSCALL(res);
         break;
#     endif

      case __NR_getdents: /* syscall 141 */
         /* int getdents(unsigned int fd, struct dirent *dirp, 
                         unsigned int count); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("getdents ( %d, %p, %d )\n",arg1,arg2,arg3);
         must_be_writable( "getdents(dirp)", arg2, arg3 );
         KERNEL_DO_SYSCALL(res);
         if (!VG_(is_kerror)(res) && res > 0)
            make_readable( arg2, res );
         break;

#     if defined(__NR_getdents64)
      case __NR_getdents64: /* syscall 220 */
         /* int getdents(unsigned int fd, struct dirent64 *dirp, 
                         unsigned int count); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("getdents64 ( %d, %p, %d )\n",arg1,arg2,arg3);
         must_be_writable( "getdents64(dirp)", arg2, arg3 );
         KERNEL_DO_SYSCALL(res);
         if (!VG_(is_kerror)(res) && res > 0)
            make_readable( arg2, res );
         break;
#     endif

#     if defined(__NR_getgroups32)
      case __NR_getgroups32: /* syscall 205 */
#     endif
      case __NR_getgroups: /* syscall 80 */
         /* int getgroups(int size, gid_t list[]); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("getgroups ( %d, %p )\n", arg1, arg2);
         if (arg1 > 0)
            must_be_writable ( "getgroups(list)", arg2, 
                               arg1 * sizeof(gid_t) );
         KERNEL_DO_SYSCALL(res);
         if (arg1 > 0 && !VG_(is_kerror)(res) && res > 0)
            make_readable ( arg2, res * sizeof(gid_t) );
         break;

      case __NR_getcwd: /* syscall 183 */
         /* char *getcwd(char *buf, size_t size); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("getcwd ( %p, %d )\n",arg1,arg2);
         must_be_writable( "getcwd(buf)", arg1, arg2 );
         KERNEL_DO_SYSCALL(res);
         if (!VG_(is_kerror)(res) && res != (Addr)NULL)
            make_readable ( arg1, arg2 );
         /* Not really right -- really we should have the asciiz
            string starting at arg1 readable, or up to arg2 bytes,
            whichever finishes first. */
         break;

      case __NR_geteuid: /* syscall 49 */
         /* uid_t geteuid(void); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("geteuid ( )\n");
         KERNEL_DO_SYSCALL(res);
         break;

#     if defined(__NR_geteuid32)
      case __NR_geteuid32: /* syscall 201 */
         /* ?? uid_t geteuid32(void); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("geteuid32(?) ( )\n");
         KERNEL_DO_SYSCALL(res);
         break;
#     endif

      case __NR_getegid: /* syscall 50 */
         /* gid_t getegid(void); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("getegid ()\n");
         KERNEL_DO_SYSCALL(res);
         break;

#     if defined(__NR_getegid32)
      case __NR_getegid32: /* syscall 202 */
         /* gid_t getegid32(void); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("getegid32 ()\n");
         KERNEL_DO_SYSCALL(res);
         break;
#     endif

      case __NR_getgid: /* syscall 47 */
         /* gid_t getgid(void); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("getgid ()\n");
         KERNEL_DO_SYSCALL(res);
         break;

#     if defined(__NR_getgid32)
      case __NR_getgid32: /* syscall 200 */
         /* gid_t getgid32(void); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("getgid32 ()\n");
         KERNEL_DO_SYSCALL(res);
         break;
#     endif

      case __NR_getpid: /* syscall 20 */
         /* pid_t getpid(void); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("getpid ()\n");
         KERNEL_DO_SYSCALL(res);
         break;

      case __NR_getpgid: /* syscall 132 */
         /* pid_t getpgid(pid_t pid); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("getpgid ( %d )\n", arg1);
         KERNEL_DO_SYSCALL(res);
         break;

      case __NR_getpgrp: /* syscall 65 */
         /* pid_t getpprp(void); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("getpgrp ()\n");
         KERNEL_DO_SYSCALL(res);
         break;

      case __NR_getppid: /* syscall 64 */
         /* pid_t getppid(void); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("getppid ()\n");
         KERNEL_DO_SYSCALL(res);
         break;

      case __NR_getresgid: /* syscall 171 */
         /* int getresgid(gid_t *rgid, gid_t *egid, gid_t *sgid); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("getresgid ( %p, %p, %p )\n", arg1,arg2,arg3);
         must_be_writable ( "getresgid(rgid)", arg1, sizeof(gid_t) );
         must_be_writable ( "getresgid(egid)", arg2, sizeof(gid_t) );
         must_be_writable ( "getresgid(sgid)", arg3, sizeof(gid_t) );
         KERNEL_DO_SYSCALL(res);
         if (!VG_(is_kerror)(res) && res == 0) {
            make_readable ( arg1, sizeof(gid_t) );
            make_readable ( arg2, sizeof(gid_t) );
            make_readable ( arg3, sizeof(gid_t) );
         }
         break;

#     if defined(__NR_getresgid32)
      case __NR_getresgid32: /* syscall 211 */
         /* int getresgid(gid_t *rgid, gid_t *egid, gid_t *sgid); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("getresgid32 ( %p, %p, %p )\n", arg1,arg2,arg3);
         must_be_writable ( "getresgid32(rgid)", arg1, sizeof(gid_t) );
         must_be_writable ( "getresgid32(egid)", arg2, sizeof(gid_t) );
         must_be_writable ( "getresgid32(sgid)", arg3, sizeof(gid_t) );
         KERNEL_DO_SYSCALL(res);
         if (!VG_(is_kerror)(res) && res == 0) {
            make_readable ( arg1, sizeof(gid_t) );
            make_readable ( arg2, sizeof(gid_t) );
            make_readable ( arg3, sizeof(gid_t) );
         }
         break;
#     endif

      case __NR_getresuid: /* syscall 165 */
         /* int getresuid(uid_t *ruid, uid_t *euid, uid_t *suid); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("getresuid ( %p, %p, %p )\n", arg1,arg2,arg3);
         must_be_writable ( "getresuid(ruid)", arg1, sizeof(uid_t) );
         must_be_writable ( "getresuid(euid)", arg2, sizeof(uid_t) );
         must_be_writable ( "getresuid(suid)", arg3, sizeof(uid_t) );
         KERNEL_DO_SYSCALL(res);
         if (!VG_(is_kerror)(res) && res == 0) {
            make_readable ( arg1, sizeof(uid_t) );
            make_readable ( arg2, sizeof(uid_t) );
            make_readable ( arg3, sizeof(uid_t) );
         }
         break;

#     if defined(__NR_getresuid32)
      case __NR_getresuid32: /* syscall 209 */
         /* int getresuid(uid_t *ruid, uid_t *euid, uid_t *suid); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("getresuid32 ( %p, %p, %p )\n", arg1,arg2,arg3);
         must_be_writable ( "getresuid32(ruid)", arg1, sizeof(uid_t) );
         must_be_writable ( "getresuid32(euid)", arg2, sizeof(uid_t) );
         must_be_writable ( "getresuid32(suid)", arg3, sizeof(uid_t) );
         KERNEL_DO_SYSCALL(res);
         if (!VG_(is_kerror)(res) && res == 0) {
            make_readable ( arg1, sizeof(uid_t) );
            make_readable ( arg2, sizeof(uid_t) );
            make_readable ( arg3, sizeof(uid_t) );
         }
         break;
#     endif

#     if defined(__NR_ugetrlimit)
      case __NR_ugetrlimit: /* syscall 191 */
#     endif
      case __NR_getrlimit: /* syscall 76 */
         /* int getrlimit (int resource, struct rlimit *rlim); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("getrlimit ( %d, %p )\n", arg1,arg2);
         must_be_writable( "getrlimit(rlim)", arg2, sizeof(struct rlimit) );
         KERNEL_DO_SYSCALL(res);
         if (!VG_(is_kerror)(res) && res == 0)
            make_readable( arg2, sizeof(struct rlimit) );
         break;

      case __NR_getrusage: /* syscall 77 */
         /* int getrusage (int who, struct rusage *usage); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("getrusage ( %d, %p )\n", arg1,arg2);
         must_be_writable( "getrusage(usage)", arg2, sizeof(struct rusage) );
         KERNEL_DO_SYSCALL(res);
         if (!VG_(is_kerror)(res) && res == 0)
            make_readable(arg2, sizeof(struct rusage) );
         break;

      case __NR_gettimeofday: /* syscall 78 */
         /* int gettimeofday(struct timeval *tv, struct timezone *tz); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("gettimeofday ( %p, %p )\n",arg1,arg2);
         must_be_writable( "gettimeofday(tv)", arg1, sizeof(struct timeval) );
         if (arg2 != 0)
            must_be_writable( "gettimeofday(tz)", arg2, 
                              sizeof(struct timezone) );
         KERNEL_DO_SYSCALL(res);
         if (!VG_(is_kerror)(res) && res == 0) {
            make_readable( arg1, sizeof(struct timeval) );
            if (arg2 != 0)
               make_readable( arg2, sizeof(struct timezone) );
         }
         break;

      case __NR_getuid: /* syscall 24 */
         /* uid_t getuid(void); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("getuid ( )\n");
         KERNEL_DO_SYSCALL(res);
         break;

#     if defined(__NR_getuid32)
      case __NR_getuid32: /* syscall 199 */
         /* ???uid_t getuid32(void); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("getuid32 ( )\n");
         KERNEL_DO_SYSCALL(res);
         break;
#     endif

      case __NR_ipc: /* syscall 117 */
         /* int ipc ( unsigned int call, int first, int second, 
                      int third, void *ptr, long fifth); */
         {
         UInt arg6 = VG_(baseBlock)[VGOFF_(m_ebp)];

         if (VG_(clo_trace_syscalls))
            VG_(printf)("ipc ( %d, %d, %d, %d, %p, %d )\n",
                        arg1,arg2,arg3,arg4,arg5,arg6);
         switch (arg1 /* call */) {
            case 1: /* IPCOP_semop */
               must_be_readable ( "semop(sops)", arg5, 
                                  arg3 * sizeof(struct sembuf) );
               KERNEL_DO_SYSCALL(res);
               break;
            case 2: /* IPCOP_semget */
            case 3: /* IPCOP_semctl */
               KERNEL_DO_SYSCALL(res);
               break;
            case 11: /* IPCOP_msgsnd */
               {
                  struct msgbuf *msgp = (struct msgbuf *)arg5;
                  Int msgsz = arg3;

                  must_be_readable ( "msgsnd(msgp->mtype)", 
                                     (UInt)&msgp->mtype, sizeof(msgp->mtype) );
                  must_be_readable ( "msgsnd(msgp->mtext)", 
                                     (UInt)msgp->mtext, msgsz );

                  KERNEL_DO_SYSCALL(res);
                  break;
               }
            case 12: /* IPCOP_msgrcv */
               {
                  struct msgbuf *msgp = ((struct ipc_kludge *)arg5)->msgp;
                  Int msgsz = arg3;

                  must_be_writable ( "msgsnd(msgp->mtype)", 
                                     (UInt)&msgp->mtype, sizeof(msgp->mtype) );
                  must_be_writable ( "msgsnd(msgp->mtext)", 
                                     (UInt)msgp->mtext, msgsz );

                  KERNEL_DO_SYSCALL(res);

                  if ( !VG_(is_kerror)(res) && res > 0 ) {
                     make_readable ( (UInt)&msgp->mtype, sizeof(msgp->mtype) );
                     make_readable ( (UInt)msgp->mtext, res );
                  }
                  break;
               }
            case 13: /* IPCOP_msgget */
               KERNEL_DO_SYSCALL(res);
               break;
            case 14: /* IPCOP_msgctl */
               {
                  switch (arg3 /* cmd */) {
                     case IPC_STAT:
                        must_be_writable ( "msgctl(buf)", arg5, 
                                           sizeof(struct msqid_ds) );
                        KERNEL_DO_SYSCALL(res);
                        if ( !VG_(is_kerror)(res) && res > 0 ) {
                           make_readable ( arg5, sizeof(struct msqid_ds) );
                        }
                        break;
                     case IPC_SET:
                        must_be_readable ( "msgctl(buf)", arg5, 
                                           sizeof(struct msqid_ds) );
                        KERNEL_DO_SYSCALL(res);
                        break;
                     case IPC_STAT|IPC_64:
                        must_be_writable ( "msgctl(buf)", arg5, 
                                           sizeof(struct msqid64_ds) );
                        KERNEL_DO_SYSCALL(res);
                        if ( !VG_(is_kerror)(res) && res > 0 ) {
                           make_readable ( arg5, sizeof(struct msqid64_ds) );
                        }
                        break;
                     case IPC_SET|IPC_64:
                        must_be_readable ( "msgctl(buf)", arg5, 
                                           sizeof(struct msqid64_ds) );
                        KERNEL_DO_SYSCALL(res);
                        break;
                     default:
                        KERNEL_DO_SYSCALL(res);
                        break;
                  }
                  break;
               }
            case 21: /* IPCOP_shmat */
               {
                  Int shmid = arg2;
                  Int shmflag = arg3;
                  UInt addr;

                  KERNEL_DO_SYSCALL(res);

                  if ( VG_(is_kerror) ( res ) )
                     break;
                  
                  /* force readability. before the syscall it is
                   * indeed uninitialized, as can be seen in
                   * glibc/sysdeps/unix/sysv/linux/shmat.c */
                  make_readable ( arg4, sizeof( ULong ) );

                  addr = safe_dereference ( arg4, 0 );
                  if ( addr > 0 ) { 
                     UInt segmentSize = get_shm_size ( shmid );
                     if ( segmentSize > 0 ) {
                        if ( shmflag & SHM_RDONLY )
                           make_readable ( addr, segmentSize );
                        else
                           make_readwritable ( addr, segmentSize );
                     }
                  }
                  break;
               }
            case 22: /* IPCOP_shmdt */
                  KERNEL_DO_SYSCALL(res);
                  /* ### FIXME: this should call make_noaccess on the
                   * area passed to shmdt. But there's no way to
                   * figure out the size of the shared memory segment
                   * just from the address...  Maybe we want to keep a
                   * copy of the exiting mappings inside valgrind? */
                  break;
            case 23: /* IPCOP_shmget */
                KERNEL_DO_SYSCALL(res);
                break;
            case 24: /* IPCOP_shmctl */
               {
                  if ( arg3 > 0 ) {
                     must_be_readable ( "shmctl(buf)", arg3, 
                                        sizeof( struct shmid_ds ) ); 

                     if ( arg2 == SHM_STAT )
                        must_be_writable( "shmctl(IPC_STAT,buf)", arg3, 
                                          sizeof( struct shmid_ds ) );
                  }

                  KERNEL_DO_SYSCALL(res);
                  break;
               }
            default:
               VG_(message)(Vg_DebugMsg,
                            "FATAL: unhandled syscall(ipc) %d",
                            arg1 );
               VG_(panic)("... bye!\n");
               break; /*NOTREACHED*/
         }
         }
         break;

      case __NR_ioctl: /* syscall 54 */
         /* int ioctl(int d, int request, ...)
            [The  "third"  argument  is traditionally char *argp, 
             and will be so named for this discussion.]
         */
         /*
         VG_(message)(
            Vg_DebugMsg, 
            "is an IOCTL,  request = 0x%x,   d = %d,   argp = 0x%x", 
            arg2,arg1,arg3);
         */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("ioctl ( %d, 0x%x, %p )\n",arg1,arg2,arg3);
         switch (arg2 /* request */) {
            case TCSETS:
            case TCSETSW:
            case TCSETSF:
               must_be_readable( "ioctl(TCSETSW)", arg3, 
                                 VKI_SIZEOF_STRUCT_TERMIOS );
               KERNEL_DO_SYSCALL(res);
               break; 
            case TCGETS:
               must_be_writable( "ioctl(TCGETS)", arg3, 
                                 VKI_SIZEOF_STRUCT_TERMIOS );
               KERNEL_DO_SYSCALL(res);
               if (!VG_(is_kerror)(res) && res == 0)
                  make_readable ( arg3, VKI_SIZEOF_STRUCT_TERMIOS );
               break;
            case TCSETA:
               must_be_readable( "ioctl(TCSETA)", arg3,
                                 VKI_SIZEOF_STRUCT_TERMIO );
               KERNEL_DO_SYSCALL(res);
               break;
            case TCGETA:
               must_be_writable( "ioctl(TCGETA)", arg3,
                                 VKI_SIZEOF_STRUCT_TERMIO );
               KERNEL_DO_SYSCALL(res);
               if (!VG_(is_kerror)(res) && res == 0)
                  make_readable ( arg3, VKI_SIZEOF_STRUCT_TERMIO );
               break;
            case TCSBRK:
            case TCSBRKP:
            case TCFLSH:
               /* These just take an int by value */
               KERNEL_DO_SYSCALL(res);
               break;
            case TIOCGWINSZ:
               must_be_writable( "ioctl(TIOCGWINSZ)", arg3, 
                                 sizeof(struct winsize) );
               KERNEL_DO_SYSCALL(res);
               if (!VG_(is_kerror)(res) && res == 0)
                  make_readable ( arg3, sizeof(struct winsize) );
               break;
            case TIOCSWINSZ:
               must_be_readable( "ioctl(TIOCSWINSZ)", arg3, 
                                 sizeof(struct winsize) );
               KERNEL_DO_SYSCALL(res);
               break;
            case TIOCGPGRP:
               /* Get process group ID for foreground processing group. */
               must_be_writable( "ioctl(TIOCGPGRP)", arg3,
                                 sizeof(pid_t) );
               KERNEL_DO_SYSCALL(res);
               if (!VG_(is_kerror)(res) && res == 0)
                  make_readable ( arg3, sizeof(pid_t) );
            case TIOCGPTN: /* Get Pty Number (of pty-mux device) */
               must_be_writable("ioctl(TIOCGPTN)", arg3, sizeof(int) );
               KERNEL_DO_SYSCALL(res);
               if (!VG_(is_kerror)(res) && res == 0)
                   make_readable ( arg3, sizeof(int));
               break;
            case TIOCSPTLCK: /* Lock/unlock Pty */
               must_be_readable( "ioctl(TIOCSPTLCK)", arg3, sizeof(int) );
               KERNEL_DO_SYSCALL(res);
               break;
            case FIONBIO:
               must_be_readable( "ioctl(FIONBIO)", arg3, sizeof(int) );
               KERNEL_DO_SYSCALL(res);
               break;
            case FIOASYNC:
               must_be_readable( "ioctl(FIOASYNC)", arg3, sizeof(int) );
               KERNEL_DO_SYSCALL(res);
               break;
            case FIONREAD:
               must_be_writable( "ioctl(FIONREAD)", arg3, sizeof(int) );
               KERNEL_DO_SYSCALL(res);
               if (!VG_(is_kerror)(res) && res == 0)
                  make_readable( arg3, sizeof(int) );
               break;

            /* If you get compilation problems here, change the #if
               1 to #if 0 and get rid of <scsi/sg.h> in
               vg_unsafe.h. */
#       if 1
            case SG_SET_COMMAND_Q:
               must_be_readable( "ioctl(SG_SET_COMMAND_Q)", arg3, sizeof(int) );
               KERNEL_DO_SYSCALL(res);
               break;
#           if defined(SG_IO)
            case SG_IO:
               must_be_writable( "ioctl(SG_IO)", arg3, 
                                 sizeof(struct sg_io_hdr) );
               KERNEL_DO_SYSCALL(res);
               if (!VG_(is_kerror)(res) && res == 0)
                  make_readable (arg3, sizeof(struct sg_io_hdr));
               break;
#           endif /* SG_IO */
            case SG_GET_SCSI_ID:
               /* Note: sometimes sg_scsi_id is called sg_scsi_id_t */
               must_be_writable( "ioctl(SG_GET_SCSI_ID)", arg3, 
                                 sizeof(struct sg_scsi_id) );
               KERNEL_DO_SYSCALL(res);
               if (!VG_(is_kerror)(res) && res == 0)
                  make_readable (arg3, sizeof(struct sg_scsi_id));
               break;
            case SG_SET_RESERVED_SIZE:
               must_be_readable( "ioctl(SG_SET_RESERVED_SIZE)", 
                                 arg3, sizeof(int) );
               KERNEL_DO_SYSCALL(res);
               break;
            case SG_SET_TIMEOUT:
               must_be_readable( "ioctl(SG_SET_TIMEOUT)", arg3, sizeof(int) );
               KERNEL_DO_SYSCALL(res);
               break;
            case SG_GET_RESERVED_SIZE:
               must_be_writable( "ioctl(SG_GET_RESERVED_SIZE)", arg3, 
                                 sizeof(int) );
               KERNEL_DO_SYSCALL(res);
               if (!VG_(is_kerror)(res) && res == 0)
                  make_readable (arg3, sizeof(int));
               break;
            case SG_GET_TIMEOUT:
               must_be_writable( "ioctl(SG_GET_TIMEOUT)", arg3, sizeof(int) );
               KERNEL_DO_SYSCALL(res);
               if (!VG_(is_kerror)(res) && res == 0)
                  make_readable (arg3, sizeof(int));
               break;
            case SG_GET_VERSION_NUM:
               must_be_readable( "ioctl(SG_GET_VERSION_NUM)", 
                                 arg3, sizeof(int) );
               KERNEL_DO_SYSCALL(res);
               break;
#       endif

            case IIOCGETCPS:
               /* In early 2.4 kernels, ISDN_MAX_CHANNELS was only defined
                * when KERNEL was. I never saw a larger value than 64 though */
#              ifndef ISDN_MAX_CHANNELS
#              define ISDN_MAX_CHANNELS 64
#              endif
               must_be_writable( "ioctl(IIOCGETCPS)", arg3,
                                 ISDN_MAX_CHANNELS 
                                 * 2 * sizeof(unsigned long) );
               KERNEL_DO_SYSCALL(res);
               if (!VG_(is_kerror)(res) && res == 0)
                  make_readable ( arg3, ISDN_MAX_CHANNELS 
                                        * 2 * sizeof(unsigned long) );
               break;
            case IIOCNETGPN:
               must_be_readable( "ioctl(IIOCNETGPN)",
                                 (UInt)&((isdn_net_ioctl_phone *)arg3)->name,
                                 sizeof(((isdn_net_ioctl_phone *)arg3)->name) );
               must_be_writable( "ioctl(IIOCNETGPN)", arg3,
                                 sizeof(isdn_net_ioctl_phone) );
               KERNEL_DO_SYSCALL(res);
               if (!VG_(is_kerror)(res) && res == 0)
                  make_readable ( arg3, sizeof(isdn_net_ioctl_phone) );
               break;

            /* These all use struct ifreq AFAIK */
            case SIOCGIFINDEX:
            case SIOCGIFFLAGS:        /* get flags                    */
            case SIOCGIFHWADDR:       /* Get hardware address         */
            case SIOCGIFMTU:          /* get MTU size                 */
            case SIOCGIFADDR:         /* get PA address               */
            case SIOCGIFNETMASK:      /* get network PA mask          */
            case SIOCGIFMETRIC:       /* get metric                   */
            case SIOCGIFMAP:          /* Get device parameters        */
            case SIOCGIFTXQLEN:       /* Get the tx queue length      */
            case SIOCGIFDSTADDR:      /* get remote PA address        */
            case SIOCGIFBRDADDR:      /* get broadcast PA address     */
            case SIOCGIFNAME:         /* get iface name               */
               must_be_writable("ioctl(SIOCGIFINDEX)", arg3, 
                                sizeof(struct ifreq));
               KERNEL_DO_SYSCALL(res);
               if (!VG_(is_kerror)(res) && res == 0)
                  make_readable (arg3, sizeof(struct ifreq));
               break;
            case SIOCGIFCONF:         /* get iface list               */
               /* WAS:
               must_be_writable("ioctl(SIOCGIFCONF)", arg3, 
                                sizeof(struct ifconf));
               KERNEL_DO_SYSCALL(res);
               if (!VG_(is_kerror)(res) && res == 0)
                  make_readable (arg3, sizeof(struct ifconf));
               */
               must_be_readable("ioctl(SIOCGIFCONF)", arg3, 
                                sizeof(struct ifconf));
               if ( arg3 ) {
                  // TODO len must be readable and writable
                  // buf pointer only needs to be readable
                  struct ifconf *ifc = (struct ifconf *) arg3;
                  must_be_writable("ioctl(SIOCGIFCONF).ifc_buf",
                                   (Addr)(ifc->ifc_buf), (UInt)(ifc->ifc_len) );
               }
               KERNEL_DO_SYSCALL(res);
               if (!VG_(is_kerror)(res) && res == 0 && arg3 ) {
                  struct ifconf *ifc = (struct ifconf *) arg3;
                  make_readable ( (Addr)(ifc->ifc_buf), (UInt)(ifc->ifc_len) );
               }
               break;
            case SIOCGSTAMP:
               must_be_writable("ioctl(SIOCGSTAMP)", arg3, 
                                sizeof(struct timeval));
               KERNEL_DO_SYSCALL(res);
               if (!VG_(is_kerror)(res) && res == 0)
                  make_readable (arg3, sizeof(struct timeval));
               break;
            case SIOCGRARP:           /* get RARP table entry         */
            case SIOCGARP:            /* get ARP table entry          */
               must_be_writable("ioctl(SIOCGARP)", arg3, 
                                sizeof(struct arpreq));
               KERNEL_DO_SYSCALL(res);
               if (!VG_(is_kerror)(res) && res == 0)
                  make_readable (arg3, sizeof(struct arpreq));
               break;
                    
            case SIOCSIFFLAGS:        /* set flags                    */
            case SIOCSIFMAP:          /* Set device parameters        */
            case SIOCSIFTXQLEN:       /* Set the tx queue length      */
            case SIOCSIFDSTADDR:      /* set remote PA address        */
            case SIOCSIFBRDADDR:      /* set broadcast PA address     */
            case SIOCSIFNETMASK:      /* set network PA mask          */
            case SIOCSIFMETRIC:       /* set metric                   */
            case SIOCSIFADDR:         /* set PA address               */
            case SIOCSIFMTU:          /* set MTU size                 */
            case SIOCSIFHWADDR:       /* set hardware address         */
               must_be_readable("ioctl(SIOCSIFFLAGS)", arg3, 
                                sizeof(struct ifreq));
               KERNEL_DO_SYSCALL(res);
               break;
            /* Routing table calls.  */
            case SIOCADDRT:           /* add routing table entry      */
            case SIOCDELRT:           /* delete routing table entry   */
               must_be_readable("ioctl(SIOCADDRT/DELRT)", arg3, 
                                sizeof(struct rtentry));
               KERNEL_DO_SYSCALL(res);
               break;

            /* RARP cache control calls. */
            case SIOCDRARP:           /* delete RARP table entry      */
            case SIOCSRARP:           /* set RARP table entry         */
            /* ARP cache control calls. */
            case SIOCSARP:            /* set ARP table entry          */
            case SIOCDARP:            /* delete ARP table entry       */
               must_be_readable("ioctl(SIOCSIFFLAGS)", arg3, 
                                sizeof(struct ifreq));
               KERNEL_DO_SYSCALL(res);
               break;

            case SIOCSPGRP:
               must_be_readable( "ioctl(SIOCSPGRP)", arg3, sizeof(int) );
               KERNEL_DO_SYSCALL(res);
               break;

            /* linux/soundcard interface (OSS) */
            case SNDCTL_SEQ_GETOUTCOUNT:
            case SNDCTL_SEQ_GETINCOUNT:
            case SNDCTL_SEQ_PERCMODE:
            case SNDCTL_SEQ_TESTMIDI:
            case SNDCTL_SEQ_RESETSAMPLES:
            case SNDCTL_SEQ_NRSYNTHS:
            case SNDCTL_SEQ_NRMIDIS:
            case SNDCTL_SEQ_GETTIME:
            case SNDCTL_DSP_GETFMTS:
            case SNDCTL_DSP_GETTRIGGER:
            case SNDCTL_DSP_GETODELAY:
#           if defined(SNDCTL_DSP_GETSPDIF)
            case SNDCTL_DSP_GETSPDIF:
#           endif
            case SNDCTL_DSP_GETCAPS:
            case SOUND_PCM_READ_RATE:
            case SOUND_PCM_READ_CHANNELS:
            case SOUND_PCM_READ_BITS:
            case (SOUND_PCM_READ_BITS|0x40000000): /* what the fuck ? */
            case SOUND_PCM_READ_FILTER:
               must_be_writable("ioctl(SNDCTL_XXX|SOUND_XXX (SIOR, int))", arg3,
                                sizeof(int));
               KERNEL_DO_SYSCALL(res);
               if (!VG_(is_kerror)(res) && res == 0)
                  make_readable (arg3, sizeof(int));
               break;
            case SNDCTL_SEQ_CTRLRATE:
            case SNDCTL_DSP_SPEED:
            case SNDCTL_DSP_STEREO:
            case SNDCTL_DSP_GETBLKSIZE: 
            case SNDCTL_DSP_CHANNELS:
            case SOUND_PCM_WRITE_FILTER:
            case SNDCTL_DSP_SUBDIVIDE:
            case SNDCTL_DSP_SETFRAGMENT:
#           if defined(SNDCTL_DSP_GETCHANNELMASK)
            case SNDCTL_DSP_GETCHANNELMASK:
#           endif
#           if defined(SNDCTL_DSP_BIND_CHANNEL)
            case SNDCTL_DSP_BIND_CHANNEL:
#           endif
            case SNDCTL_TMR_TIMEBASE:
            case SNDCTL_TMR_TEMPO:
            case SNDCTL_TMR_SOURCE:
            case SNDCTL_MIDI_PRETIME:
            case SNDCTL_MIDI_MPUMODE:
               must_be_readable("ioctl(SNDCTL_XXX|SOUND_XXX (SIOWR, int))", 
                                arg3, sizeof(int));
               must_be_writable("ioctl(SNDCTL_XXX|SOUND_XXX (SIOWR, int))", 
                                arg3, sizeof(int));
               KERNEL_DO_SYSCALL(res);
               break;
            case SNDCTL_DSP_GETOSPACE:
            case SNDCTL_DSP_GETISPACE:
               must_be_writable("ioctl(SNDCTL_XXX|SOUND_XXX "
                                "(SIOR, audio_buf_info))", arg3,
                                sizeof(audio_buf_info));
               KERNEL_DO_SYSCALL(res);
               if (!VG_(is_kerror)(res) && res == 0)
                  make_readable (arg3, sizeof(audio_buf_info));
               break;
            case SNDCTL_DSP_SETTRIGGER:
               must_be_readable("ioctl(SNDCTL_XXX|SOUND_XXX (SIOW, int))", 
                                arg3, sizeof(int));
               KERNEL_DO_SYSCALL(res);
               break;
          
            /* We don't have any specific information on it, so
               try to do something reasonable based on direction and
               size bits.  The encoding scheme is described in
               /usr/include/asm/ioctl.h. */
            default: {
               UInt dir  = _IOC_DIR(arg2);
               UInt size = _IOC_SIZE(arg2);
               if (/* size == 0 || */ dir == _IOC_NONE) {
                  VG_(message)(Vg_UserMsg, 
                     "Warning: noted but unhandled ioctl 0x%x"
                     " with no size/direction hints",
                     arg2); 
                  VG_(message)(Vg_UserMsg, 
                     "   This could cause spurious value errors"
                     " to appear.");
                  VG_(message)(Vg_UserMsg, 
                     "   See README_MISSING_SYSCALL_OR_IOCTL for guidance on"
                     " writing a proper wrapper." );
               } else {
                  if ((dir & _IOC_READ) && size > 0)
                     must_be_readable("ioctl(generic)", arg3, size);
                  if ((dir & _IOC_WRITE) && size > 0)
                     must_be_writable("ioctl(generic)", arg3, size);
               }
               KERNEL_DO_SYSCALL(res);
               if (size > 0 && (dir & _IOC_WRITE)
                   && !VG_(is_kerror)(res) && res == 0)
                  make_readable (arg3, size);
               break;
            }
         }
         break;

      case __NR_kill: /* syscall 37 */
         /* int kill(pid_t pid, int sig); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("kill ( %d, %d )\n", arg1,arg2);
         KERNEL_DO_SYSCALL(res);
         break;

      case __NR_link: /* syscall 9 */
         /* int link(const char *oldpath, const char *newpath); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("link ( %p, %p)\n", arg1, arg2);
         must_be_readable_asciiz( "link(oldpath)", arg1);
         must_be_readable_asciiz( "link(newpath)", arg2);
         KERNEL_DO_SYSCALL(res);
         break;

      case __NR_lseek: /* syscall 19 */
         /* off_t lseek(int fildes, off_t offset, int whence); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("lseek ( %d, %d, %d )\n",arg1,arg2,arg3);
         KERNEL_DO_SYSCALL(res);
         break;

      case __NR__llseek: /* syscall 140 */
         /* int _llseek(unsigned int fd, unsigned long offset_high,       
                        unsigned long  offset_low, 
                        loff_t * result, unsigned int whence); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("llseek ( %d, 0x%x, 0x%x, %p, %d )\n",
                        arg1,arg2,arg3,arg4,arg5);
         must_be_writable( "llseek(result)", arg4, sizeof(loff_t));
         KERNEL_DO_SYSCALL(res);
         if (!VG_(is_kerror)(res) && res == 0)
            make_readable( arg4, sizeof(loff_t) );
         break;

      case __NR_lstat: /* syscall 107 */
         /* int lstat(const char *file_name, struct stat *buf); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("lstat ( %p, %p )\n",arg1,arg2);
         must_be_readable_asciiz( "lstat(file_name)", arg1 );
         must_be_writable( "lstat(buf)", arg2, sizeof(struct stat) );
         KERNEL_DO_SYSCALL(res);
         if (!VG_(is_kerror)(res) && res == 0) {
            make_readable( arg2, sizeof(struct stat) );
         }
         break;

#     if defined(__NR_lstat64)
      case __NR_lstat64: /* syscall 196 */
         /* int lstat64(const char *file_name, struct stat64 *buf); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("lstat64 ( %p, %p )\n",arg1,arg2);
         must_be_readable_asciiz( "lstat64(file_name)", arg1 );
         must_be_writable( "lstat64(buf)", arg2, sizeof(struct stat64) );
         KERNEL_DO_SYSCALL(res);
         if (!VG_(is_kerror)(res) && res == 0) {
            make_readable( arg2, sizeof(struct stat64) );
         }
         break;
#     endif

      case __NR_mkdir: /* syscall 39 */
         /* int mkdir(const char *pathname, mode_t mode); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("mkdir ( %p, %d )\n", arg1,arg2);
         must_be_readable_asciiz( "mkdir(pathname)", arg1 );
         KERNEL_DO_SYSCALL(res);
         break;

#     if defined(__NR_mmap2)
      case __NR_mmap2: /* syscall 192 */
         /* My impression is that this is exactly like __NR_mmap 
            except that all 6 args are passed in regs, rather than in 
            a memory-block. */
         /* void* mmap(void *start, size_t length, int prot, 
                       int flags, int fd, off_t offset); 
         */
         {
         UInt arg6 = VG_(baseBlock)[VGOFF_(m_ebp)];
         if (VG_(clo_trace_syscalls))
            VG_(printf)("mmap2 ( %p, %d, %d, %d, %d, %d )\n",
                        arg1, arg2, arg3, arg4, arg5, arg6 );
         KERNEL_DO_SYSCALL(res);
         /* !!! shouldn't we also be doing the symtab loading stuff as
            in __NR_mmap ? */
         if (!VG_(is_kerror)(res))
            approximate_mmap_permissions( (Addr)res, arg2, arg3 );
         }
         break;
#     endif

      case __NR_mmap: /* syscall 90 */
         /* void* mmap(void *start, size_t length, int prot, 
                       int flags, int fd, off_t offset); 
         */
         {
         Bool arg_block_readable
                 = VG_(clo_instrument)
                 ? VGM_(check_readable)(arg1, 6*sizeof(UInt), NULL)
                 : True;
         must_be_readable( "mmap(args)", arg1, 6*sizeof(UInt) );
         if (arg_block_readable) {
            UInt* arg_block = (UInt*)arg1;
            UInt arg6;
            arg1 = arg_block[0];
            arg2 = arg_block[1];
            arg3 = arg_block[2];
            arg4 = arg_block[3];
            arg5 = arg_block[4];
            arg6 = arg_block[5];
            if (VG_(clo_trace_syscalls))
               VG_(printf)("mmap ( %p, %d, %d, %d, %d, %d )\n",
                           arg1, arg2, arg3, arg4, arg5, arg6 );
         }
         KERNEL_DO_SYSCALL(res);
         if (arg_block_readable && !VG_(is_kerror)(res))
            approximate_mmap_permissions( (Addr)res, arg2, arg3 );
         if (arg_block_readable && !VG_(is_kerror)(res)
             && (arg3 & PROT_EXEC)) {
            /* The client mmap'ed a segment with executable
               permissions.  Tell the symbol-table loader, so that it
               has an opportunity to pick up more symbols if this mmap
               was caused by the client loading a new .so via
               dlopen().  This is important for debugging KDE. */
            VG_(read_symbols)();
         }
         }
         
         break;

      case __NR_mprotect: /* syscall 125 */
         /* int mprotect(const void *addr, size_t len, int prot); */
         /* should addr .. addr+len-1 be checked before the call? */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("mprotect ( %p, %d, %d )\n", arg1,arg2,arg3);
         KERNEL_DO_SYSCALL(res);
         if (!VG_(is_kerror)(res))
            approximate_mmap_permissions ( arg1, arg2, arg3 );
         break;

      case __NR_munmap: /* syscall 91 */
         /* int munmap(void *start, size_t length); */
         /* should start .. start+length-1 be checked before the call? */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("munmap ( %p, %d )\n", arg1,arg2);
         KERNEL_DO_SYSCALL(res);
         if (!VG_(is_kerror)(res)) {
            /* Mash around start and length so that the area passed to
               make_noaccess() exactly covers an integral number of
               pages.  If we don't do that, our idea of addressible
               memory diverges from that of the kernel's, which causes
               the leak detector to crash. */
            Addr start = arg1;
            Addr length = arg2;
            while ((start % VKI_BYTES_PER_PAGE) > 0) { start--; length++; }
            while (((start+length) % VKI_BYTES_PER_PAGE) > 0) { length++; }
            /*
            VG_(printf)("MUNMAP: correct (%p for %d) to (%p for %d) %s\n", 
               arg1, arg2, start, length, (arg1!=start || arg2!=length) 
                                             ? "CHANGE" : "");
            */
            make_noaccess( start, length );
            /* Tell our symbol table machinery about this, so that if
               this happens to be a .so being unloaded, the relevant
               symbols are removed too. */
            VG_(symtab_notify_munmap) ( start, length );
         }
         break;

      case __NR_nanosleep: /* syscall 162 */
         /* int nanosleep(const struct timespec *req, struct timespec *rem); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("nanosleep ( %p, %p )\n", arg1,arg2);
         must_be_readable ( "nanosleep(req)", arg1, 
                                              sizeof(struct timespec) );
         if (arg2 != (UInt)NULL)
            must_be_writable ( "nanosleep(rem)", arg2, 
                               sizeof(struct timespec) );
         KERNEL_DO_SYSCALL(res);
         /* Somewhat bogus ... is only written by the kernel if
            res == -1 && errno == EINTR. */
         if (!VG_(is_kerror)(res) && arg2 != (UInt)NULL)
            make_readable ( arg2, sizeof(struct timespec) );
         break;

      case __NR__newselect: /* syscall 142 */
         /* int select(int n,  
                       fd_set *readfds, fd_set *writefds, fd_set *exceptfds, 
                       struct timeval *timeout);
         */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("newselect ( %d, %p, %p, %p, %p )\n",
                        arg1,arg2,arg3,arg4,arg5);
         if (arg2 != 0)
            must_be_readable( "newselect(readfds)",   
                              arg2, arg1/8 /* __FD_SETSIZE/8 */ );
         if (arg3 != 0)
            must_be_readable( "newselect(writefds)",  
                              arg3, arg1/8 /* __FD_SETSIZE/8 */ );
         if (arg4 != 0)
            must_be_readable( "newselect(exceptfds)", 
                              arg4, arg1/8 /* __FD_SETSIZE/8 */ );
         if (arg5 != 0)
            must_be_readable( "newselect(timeout)", arg5, 
                              sizeof(struct timeval) );
         KERNEL_DO_SYSCALL(res);
         break;
         
      case __NR_open: /* syscall 5 */
         /* int open(const char *pathname, int flags); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("open ( %p(%s), %d ) --> ",arg1,arg1,arg2);
         must_be_readable_asciiz( "open(pathname)", arg1 );
         KERNEL_DO_SYSCALL(res);
         if (VG_(clo_trace_syscalls))
            VG_(printf)("%d\n",res);
         break;

      case __NR_pipe: /* syscall 42 */
         /* int pipe(int filedes[2]); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("pipe ( %p ) ...\n", arg1);
         must_be_writable( "pipe(filedes)", arg1, 2*sizeof(int) );
         KERNEL_DO_SYSCALL(res);
         if (!VG_(is_kerror)(res))
            make_readable ( arg1, 2*sizeof(int) );
         if (VG_(clo_trace_syscalls) && !VG_(is_kerror)(res))
            VG_(printf)("SYSCALL[%d, %d]       pipe --> (rd %d, wr %d)\n", 
                        VG_(syscall_depth), VG_(getpid)(), 
                        ((UInt*)arg1)[0], ((UInt*)arg1)[1] );
         break;

      case __NR_poll: /* syscall 168 */
         /* struct pollfd {
               int fd;           -- file descriptor
               short events;     -- requested events
               short revents;    -- returned events
            };
           int poll(struct pollfd *ufds, unsigned int nfds, 
                                         int timeout) 
         */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("poll ( %d, %d, %d )\n",arg1,arg2,arg3);
         /* In fact some parts of this struct should be readable too.
            This should be fixed properly. */
         must_be_writable( "poll(ufds)", arg1, arg2 * sizeof(struct pollfd) );
         KERNEL_DO_SYSCALL(res);
         if (!VG_(is_kerror)(res) && res > 0) {
            Int i;
            struct pollfd * arr = (struct pollfd *)arg1;
            for (i = 0; i < arg2; i++)
               make_readable( (Addr)(&arr[i].revents), sizeof(Short) );
         }
         break;
 
      case __NR_read: /* syscall 3 */
         /* size_t read(int fd, void *buf, size_t count); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("read ( %d, %p, %d ) ...\n",arg1,arg2,arg3);
         must_be_writable( "read(buf)", arg2, arg3 );
         KERNEL_DO_SYSCALL(res);
         if (VG_(clo_trace_syscalls))
            VG_(printf)("SYSCALL[%d, %d]       read ( %d, %p, %d ) --> %d\n", 
                        VG_(syscall_depth), VG_(getpid)(), 
                        arg1, arg2, arg3, res);
         if (!VG_(is_kerror)(res) && res > 0) {
            make_readable( arg2, res );
         }
         break;

      case __NR_readlink: /* syscall 85 */
         /* int readlink(const char *path, char *buf, size_t bufsiz); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("readlink ( %p, %p, %d )\n", arg1,arg2,arg3);
         must_be_readable_asciiz( "readlink(path)", arg1 );
         must_be_writable ( "readlink(buf)", arg2,arg3 );
         KERNEL_DO_SYSCALL(res);
         if (!VG_(is_kerror)(res) && res > 0) {
            make_readable ( arg2, res );
         }
         break;

      case __NR_readv: { /* syscall 145 */
         /* int readv(int fd, const struct iovec * vector, size_t count); */
         UInt i;
         struct iovec * vec;
         if (VG_(clo_trace_syscalls))
            VG_(printf)("readv ( %d, %p, %d )\n",arg1,arg2,arg3);
         must_be_readable( "readv(vector)", 
                           arg2, arg3 * sizeof(struct iovec) );
         /* ToDo: don't do any of the following if the vector is invalid */
         vec = (struct iovec *)arg2;
         for (i = 0; i < arg3; i++)
            must_be_writable( "readv(vector[...])",
                              (UInt)vec[i].iov_base,vec[i].iov_len );
         KERNEL_DO_SYSCALL(res);
         if (!VG_(is_kerror)(res) && res > 0) {
            /* res holds the number of bytes read. */
            for (i = 0; i < arg3; i++) {
               Int nReadThisBuf = vec[i].iov_len;
               if (nReadThisBuf > res) nReadThisBuf = res;
               make_readable( (UInt)vec[i].iov_base, nReadThisBuf );
               res -= nReadThisBuf;
               if (res < 0) VG_(panic)("vg_wrap_syscall: readv: res < 0");
            }
         }
         break;
      }

      case __NR_rename: /* syscall 38 */
         /* int rename(const char *oldpath, const char *newpath); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("rename ( %p, %p )\n", arg1, arg2 );
         must_be_readable_asciiz( "rename(oldpath)", arg1 );
         must_be_readable_asciiz( "rename(newpath)", arg2 );
         KERNEL_DO_SYSCALL(res);
         break;

      case __NR_rmdir: /* syscall 40 */
         /* int rmdir(const char *pathname); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("rmdir ( %p )\n", arg1);
         must_be_readable_asciiz( "rmdir(pathname)", arg1 );
         KERNEL_DO_SYSCALL(res);
         break;

      case __NR_sched_setparam:
      case __NR_sched_getparam:
      case __NR_sched_yield:
      case __NR_sched_get_priority_min:
         if (VG_(clo_instrument)) {
            VG_(message)(Vg_UserMsg, 
               "Warning: noted but unhandled __NR_sched_* syscall (%d).", 
               syscallno);
            VG_(message)(Vg_UserMsg, 
               "   This could cause spurious value errors"
               " to appear.");
         }
         KERNEL_DO_SYSCALL(res);
         break;

      case __NR_select: /* syscall 82 */
         /* struct sel_arg_struct {
              unsigned long n;
              fd_set *inp, *outp, *exp;
              struct timeval *tvp;
            };
            int old_select(struct sel_arg_struct *arg);
         */
         {
         Bool arg_block_readable
                 = VG_(clo_instrument)
                 ? VGM_(check_readable)(arg1, 5*sizeof(UInt), NULL)
                 : True;
         must_be_readable ( "select(args)", arg1, 5*sizeof(UInt) );
         if (arg_block_readable) {
            UInt* arg_struct = (UInt*)arg1;
            arg1 = arg_struct[0];
            arg2 = arg_struct[1];
            arg3 = arg_struct[2];
            arg4 = arg_struct[3];
            arg5 = arg_struct[4];

            if (VG_(clo_trace_syscalls)) 
               VG_(printf)("select ( %d, %p, %p, %p, %p )\n", 
                           arg1,arg2,arg3,arg4,arg5);
            if (arg2 != (Addr)NULL)
               must_be_readable("select(readfds)", arg2, 
                                arg1/8 /* __FD_SETSIZE/8 */ );
            if (arg3 != (Addr)NULL)
               must_be_readable("select(writefds)", arg3, 
                                arg1/8 /* __FD_SETSIZE/8 */ );
            if (arg4 != (Addr)NULL)
               must_be_readable("select(exceptfds)", arg4, 
                                arg1/8 /* __FD_SETSIZE/8 */ );
            if (arg5 != (Addr)NULL)
               must_be_readable("select(timeout)", arg5, 
                                sizeof(struct timeval) );
         }
         }
         KERNEL_DO_SYSCALL(res);
         break;

      case __NR_setitimer: /* syscall 104 */
         /* setitimer(int which, const struct itimerval *value,
                                 struct itimerval *ovalue); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("setitimer ( %d, %p, %p )\n", arg1,arg2,arg3);
         must_be_readable("setitimer(value)", 
                          arg2, sizeof(struct itimerval) );
         if (arg3 != (Addr)NULL)
            must_be_writable("setitimer(ovalue)", 
                             arg3, sizeof(struct itimerval));
         KERNEL_DO_SYSCALL(res);
         if (!VG_(is_kerror)(res) && arg3 != (Addr)NULL) {
            make_readable(arg3, sizeof(struct itimerval));
         }
         break;

#     if defined(__NR_setfsgid32)
      case __NR_setfsgid32: /* syscall 216 */
         /* int setfsgid(uid_t fsgid); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("setfsgid ( %d )\n", arg1);
         KERNEL_DO_SYSCALL(res);
         break;
#     endif

#     if defined(__NR_setgid32)
      case __NR_setgid32: /* syscall 214 */
#     endif
      case __NR_setgid: /* syscall 46 */
         /* int setgid(gid_t gid); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("setgid ( %d )\n", arg1);
         KERNEL_DO_SYSCALL(res);
         break;

      case __NR_setsid: /* syscall 66 */
         /* pid_t setsid(void); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("setsid ()\n");
         KERNEL_DO_SYSCALL(res);
         break;

#     if defined(__NR_setgroups32)
      case __NR_setgroups32: /* syscall 206 */
#     endif
      case __NR_setgroups: /* syscall 81 */
         /* int setgroups(size_t size, const gid_t *list); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("setgroups ( %d, %p )\n", arg1, arg2);
         if (arg1 > 0)
            must_be_readable ( "setgroups(list)", arg2, 
                               arg1 * sizeof(gid_t) );
         KERNEL_DO_SYSCALL(res);
         break;

      case __NR_setpgid: /* syscall 57 */
         /* int setpgid(pid_t pid, pid_t pgid); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("setpgid ( %d, %d )\n", arg1, arg2);
         KERNEL_DO_SYSCALL(res);
         break;

#     if defined(__NR_setregid32)
      case __NR_setregid32: /* syscall 204 */
         /* int setregid(gid_t rgid, gid_t egid); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("setregid32(?) ( %d, %d )\n", arg1, arg2);
         KERNEL_DO_SYSCALL(res);
         break;
#     endif

#     if defined(__NR_setresuid32)
      case __NR_setresuid32: /* syscall 208 */
         /* int setresuid(uid_t ruid, uid_t euid, uid_t suid); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("setresuid32(?) ( %d, %d, %d )\n", arg1, arg2, arg3);
         KERNEL_DO_SYSCALL(res);
         break;
#     endif

#     if defined(__NR_setreuid32)
      case __NR_setreuid32: /* syscall 203 */
#     endif
      case __NR_setreuid: /* syscall 70 */
         /* int setreuid(uid_t ruid, uid_t euid); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("setreuid ( 0x%x, 0x%x )\n", arg1, arg2);
         KERNEL_DO_SYSCALL(res);
         break;

      case __NR_setrlimit: /* syscall 75 */
         /* int setrlimit (int resource, const struct rlimit *rlim); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("setrlimit ( %d, %p )\n", arg1,arg2);
         must_be_readable( "setrlimit(rlim)", arg2, sizeof(struct rlimit) );
         KERNEL_DO_SYSCALL(res);
         break;

#     if defined(__NR_setuid32)
      case __NR_setuid32: /* syscall 213 */
#     endif
      case __NR_setuid: /* syscall 23 */
         /* int setuid(uid_t uid); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("setuid ( %d )\n", arg1);
         KERNEL_DO_SYSCALL(res);
         break;

      case __NR_socketcall: /* syscall 102 */
         /* int socketcall(int call, unsigned long *args); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("socketcall ( %d, %p )\n",arg1,arg2);
         switch (arg1 /* request */) {

            case SYS_SOCKETPAIR:
               /* int socketpair(int d, int type, int protocol, int sv[2]); */
               must_be_readable( "socketcall.socketpair(args)", 
                                 arg2, 4*sizeof(Addr) );
               must_be_writable( "socketcall.socketpair(sv)", 
                                 ((UInt*)arg2)[3], 2*sizeof(int) );
               KERNEL_DO_SYSCALL(res);
               if (!VG_(is_kerror)(res))
                  make_readable ( ((UInt*)arg2)[3], 2*sizeof(int) );
               break;

            case SYS_SOCKET:
               /* int socket(int domain, int type, int protocol); */
               must_be_readable( "socketcall.socket(args)", 
                                 arg2, 3*sizeof(Addr) );
               KERNEL_DO_SYSCALL(res);
               break;

            case SYS_BIND:
               /* int bind(int sockfd, struct sockaddr *my_addr, 
                           int addrlen); */
               must_be_readable( "socketcall.bind(args)", 
                                 arg2, 3*sizeof(Addr) );
               must_be_readable( "socketcall.bind(my_addr)", 
                                 ((UInt*)arg2)[1], ((UInt*)arg2)[2] );
               KERNEL_DO_SYSCALL(res);
               break;

            case SYS_LISTEN:
               /* int listen(int s, int backlog); */
               must_be_readable( "socketcall.listen(args)", 
                                 arg2, 2*sizeof(Addr) );
               KERNEL_DO_SYSCALL(res);
               break;

            case SYS_ACCEPT: {
               /* int accept(int s, struct sockaddr *addr, int *p_addrlen); */
               Addr addr;
               Addr p_addrlen;
               UInt addrlen_in, addrlen_out;
               must_be_readable( "socketcall.accept(args)", 
                                 arg2, 3*sizeof(Addr) );
               addr      = ((UInt*)arg2)[1];
               p_addrlen = ((UInt*)arg2)[2];
               if (p_addrlen != (Addr)NULL) {
                  must_be_readable ( "socketcall.accept(addrlen)", 
                                     p_addrlen, sizeof(int) );
                  addrlen_in = safe_dereference( p_addrlen, 0 );
                  must_be_writable ( "socketcall.accept(addr)", 
                                     addr, addrlen_in );
               }
               KERNEL_DO_SYSCALL(res);
               if (!VG_(is_kerror)(res) && res >= 0 && p_addrlen != (Addr)NULL) {
                  addrlen_out = safe_dereference( p_addrlen, 0 );
                  if (addrlen_out > 0)
                     make_readable( addr, addrlen_out );
               }
               break;
            }

            case SYS_SENDTO:
               /* int sendto(int s, const void *msg, int len, 
                             unsigned int flags, 
                             const struct sockaddr *to, int tolen); */
               must_be_readable( "socketcall.sendto(args)", arg2, 
                                 6*sizeof(Addr) );
               must_be_readable( "socketcall.sendto(msg)",
                                 ((UInt*)arg2)[1], /* msg */
                                 ((UInt*)arg2)[2]  /* len */ );
               must_be_readable( "socketcall.sendto(to)",
                                 ((UInt*)arg2)[4], /* to */
                                 ((UInt*)arg2)[5]  /* tolen */ );
               KERNEL_DO_SYSCALL(res);
               break;

            case SYS_SEND:
               /* int send(int s, const void *msg, size_t len, int flags); */
               must_be_readable( "socketcall.send(args)", arg2,
                                 4*sizeof(Addr) );
               must_be_readable( "socketcall.send(msg)",
                                 ((UInt*)arg2)[1], /* msg */
                                  ((UInt*)arg2)[2]  /* len */ );
               KERNEL_DO_SYSCALL(res);
               break;

            case SYS_RECVFROM:
               /* int recvfrom(int s, void *buf, int len, unsigned int flags,
                               struct sockaddr *from, int *fromlen); */
               must_be_readable( "socketcall.recvfrom(args)", 
                                 arg2, 6*sizeof(Addr) );
               if ( ((UInt*)arg2)[4] /* from */ != 0) {
                  must_be_readable( "socketcall.recvfrom(fromlen)",
                                    ((UInt*)arg2)[5] /* fromlen */, 
                                    sizeof(int) );
                  must_be_writable( "socketcall.recvfrom(from)",
                                    ((UInt*)arg2)[4], /*from*/
                                    safe_dereference( (Addr)
                                                      ((UInt*)arg2)[5], 0 ) );
               }
               must_be_writable( "socketcall.recvfrom(buf)", 
                                 ((UInt*)arg2)[1], /* buf */
                                 ((UInt*)arg2)[2]  /* len */ );
               KERNEL_DO_SYSCALL(res);
               if (!VG_(is_kerror)(res) && res >= 0) {
                  make_readable( ((UInt*)arg2)[1], /* buf */
                                 ((UInt*)arg2)[2]  /* len */ );
                  if ( ((UInt*)arg2)[4] /* from */ != 0) {
                     make_readable( 
                        ((UInt*)arg2)[4], /*from*/
                        safe_dereference( (Addr) ((UInt*)arg2)[5], 0 ) );
                  }
               }
               /* phew! */
               break;

            case SYS_RECV:
               /* int recv(int s, void *buf, int len, unsigned int flags); */
               /* man 2 recv says:
               The  recv call is normally used only on a connected socket
               (see connect(2)) and is identical to recvfrom with a  NULL
               from parameter.
               */
               must_be_readable( "socketcall.recv(args)", 
                                 arg2, 4*sizeof(Addr) );
               must_be_writable( "socketcall.recv(buf)", 
                                 ((UInt*)arg2)[1], /* buf */
                                 ((UInt*)arg2)[2]  /* len */ );
               KERNEL_DO_SYSCALL(res);
               if (!VG_(is_kerror)(res) && res >= 0 
                                   && ((UInt*)arg2)[1] != (UInt)NULL) {
                  make_readable( ((UInt*)arg2)[1], /* buf */
                                 ((UInt*)arg2)[2]  /* len */ );
               }
               break;

            case SYS_CONNECT: {
               struct sockaddr *sa;
               /* int connect(int sockfd, 
                              struct sockaddr *serv_addr, int addrlen ); */
               must_be_readable( "socketcall.connect(args)", 
                                 arg2, 3*sizeof(Addr) );
               must_be_readable( "socketcall.connect(serv_addr.sa_family)",
                                 ((UInt*)arg2)[1], /* serv_addr */
                                 sizeof (sa_family_t));
               sa = (struct sockaddr *) (((UInt*)arg2)[1]);
               if (sa->sa_family == AF_UNIX)
                  must_be_readable_asciiz( 
                     "socketcall.connect(serv_addr.sun_path)",
                     (UInt) ((struct sockaddr_un *) sa)->sun_path);
               /* XXX There probably should be more cases here since not
                  all of the struct sockaddr_XXX must be initialized.  But
                  wait until something pops up.  */
               else
                  must_be_readable( "socketcall.connect(serv_addr)",
                                    ((UInt*)arg2)[1], /* serv_addr */
                                    ((UInt*)arg2)[2]  /* addrlen */ );
               KERNEL_DO_SYSCALL(res);
               break;
           }

            case SYS_SETSOCKOPT:
               /* int setsockopt(int s, int level, int optname, 
                                 const void *optval, int optlen); */
               must_be_readable( "socketcall.setsockopt(args)", 
                                 arg2, 5*sizeof(Addr) );
               must_be_readable( "socketcall.setsockopt(optval)",
                                 ((UInt*)arg2)[3], /* optval */
                                 ((UInt*)arg2)[4]  /* optlen */ );
               KERNEL_DO_SYSCALL(res);
               break;

            case SYS_GETSOCKOPT:
               /* int setsockopt(int s, int level, int optname, 
                                 void *optval, socklen_t *optlen); */
               must_be_readable( "socketcall.getsockopt(args)", 
                                 arg2, 5*sizeof(Addr) );
               {
               Addr optval_p = ((UInt*)arg2)[3];
               Addr optlen_p = ((UInt*)arg2)[4];
               //vg_assert(sizeof(socklen_t) == sizeof(UInt));
               UInt optlen_after;
               UInt optlen = safe_dereference ( optlen_p, 0 );
               if (optlen > 0) 
                  must_be_writable( "socketcall.getsockopt(optval)", 
                                    optval_p, optlen );
               KERNEL_DO_SYSCALL(res);
               optlen_after = safe_dereference ( optlen_p, 0 );
               if (!VG_(is_kerror)(res) && optlen > 0 && optlen_after > 0) 
                  make_readable( optval_p, optlen_after );
               }
               break;

            case SYS_GETSOCKNAME:
               /* int getsockname(int s, struct sockaddr* name, 
                                  int* namelen) */
               must_be_readable( "socketcall.getsockname(args)", 
                                 arg2, 3*sizeof(Addr) );
               {
               UInt namelen = safe_dereference( (Addr) ((UInt*)arg2)[2], 0);
               if (namelen > 0)
                  must_be_writable( "socketcall.getsockname(name)", 
                                    ((UInt*)arg2)[1], namelen );
               KERNEL_DO_SYSCALL(res);
               if (!VG_(is_kerror)(res)) {
                  namelen = safe_dereference( (Addr) ((UInt*)arg2)[2], 0);
                  if (namelen > 0 
                      && ((UInt*)arg2)[1] != (UInt)NULL)
                     make_readable( ((UInt*)arg2)[1], namelen );
               }
               }
               break;

            case SYS_GETPEERNAME:
               /* int getpeername(int s, struct sockaddr* name, 
                                  int* namelen) */
               must_be_readable( "socketcall.getpeername(args)", 
                                 arg2, 3*sizeof(Addr) );
               {
               UInt namelen = safe_dereference( (Addr) ((UInt*)arg2)[2], 0);
               if (namelen > 0)
                  must_be_writable( "socketcall.getpeername(name)", 
                                    ((UInt*)arg2)[1], namelen );
               KERNEL_DO_SYSCALL(res);
               if (!VG_(is_kerror)(res)) {
                  namelen = safe_dereference( (Addr) ((UInt*)arg2)[2], 0);
                  if (namelen > 0 
                      && ((UInt*)arg2)[1] != (UInt)NULL)
                     make_readable( ((UInt*)arg2)[1], namelen );
               }
               }
               break;

            case SYS_SHUTDOWN:
               /* int shutdown(int s, int how); */
               must_be_readable( "socketcall.shutdown(args)", 
                                  arg2, 2*sizeof(Addr) );
               KERNEL_DO_SYSCALL(res);
               break;

            case SYS_SENDMSG:
               {
                  /* int sendmsg(int s, const struct msghdr *msg, int flags); */

                  /* this causes warnings, and I don't get why. glibc bug?
                   * (after all it's glibc providing the arguments array)
                  must_be_readable( "socketcall.sendmsg(args)", 
                                     arg2, 3*sizeof(Addr) );
                  */

                  struct msghdr *msg = (struct msghdr *)((UInt *)arg2)[ 1 ];
                  msghdr_foreachfield ( msg, must_be_readable_sendmsg );

                  KERNEL_DO_SYSCALL(res);
                  break;
               }

            case SYS_RECVMSG:
               {
                  /* int recvmsg(int s, struct msghdr *msg, int flags); */

                  /* this causes warnings, and I don't get why. glibc bug?
                   * (after all it's glibc providing the arguments array)
                  must_be_readable( "socketcall.recvmsg(args)", 
                                     arg2, 3*sizeof(Addr) );
                  */

                  struct msghdr *msg = (struct msghdr *)((UInt *)arg2)[ 1 ];
                  msghdr_foreachfield ( msg, must_be_writable_recvmsg );

                  KERNEL_DO_SYSCALL(res);

                  if ( !VG_(is_kerror)( res ) )
                     msghdr_foreachfield( msg, make_readable_recvmsg );

                  break;
               }

            default:
               VG_(message)(Vg_DebugMsg,"FATAL: unhandled socketcall 0x%x",arg1);
               VG_(panic)("... bye!\n");
               break; /*NOTREACHED*/
         }
         break;

      case __NR_stat: /* syscall 106 */
         /* int stat(const char *file_name, struct stat *buf); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("stat ( %p, %p )\n",arg1,arg2);
         must_be_readable_asciiz( "stat(file_name)", arg1 );
         must_be_writable( "stat(buf)", arg2, sizeof(struct stat) );
         KERNEL_DO_SYSCALL(res);
         if (!VG_(is_kerror)(res))
            make_readable( arg2, sizeof(struct stat) );
         break;

      case __NR_statfs: /* syscall 99 */
         /* int statfs(const char *path, struct statfs *buf); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("statfs ( %p, %p )\n",arg1,arg2);
         must_be_readable_asciiz( "statfs(path)", arg1 );
         must_be_writable( "stat(buf)", arg2, sizeof(struct statfs) );
         KERNEL_DO_SYSCALL(res);
         if (!VG_(is_kerror)(res))
            make_readable( arg2, sizeof(struct statfs) );
         break;

      case __NR_symlink: /* syscall 83 */
         /* int symlink(const char *oldpath, const char *newpath); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("symlink ( %p, %p )\n",arg1,arg2);
         must_be_readable_asciiz( "symlink(oldpath)", arg1 );
         must_be_readable_asciiz( "symlink(newpath)", arg2 );
         KERNEL_DO_SYSCALL(res);
         break; 

#     if defined(__NR_stat64)
      case __NR_stat64: /* syscall 195 */
         /* int stat64(const char *file_name, struct stat64 *buf); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("stat64 ( %p, %p )\n",arg1,arg2);
         must_be_readable_asciiz( "stat64(file_name)", arg1 );
         must_be_writable( "stat64(buf)", arg2, sizeof(struct stat64) );
         KERNEL_DO_SYSCALL(res);
         if (!VG_(is_kerror)(res))
            make_readable( arg2, sizeof(struct stat64) );
         break;
#     endif

#     if defined(__NR_fstat64)
      case __NR_fstat64: /* syscall 197 */
         /* int fstat64(int filedes, struct stat64 *buf); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("fstat64 ( %d, %p )\n",arg1,arg2);
         must_be_writable( "fstat64(buf)", arg2, sizeof(struct stat64) );
         KERNEL_DO_SYSCALL(res);
         if (!VG_(is_kerror)(res))
            make_readable( arg2, sizeof(struct stat64) );
         break;
#     endif

      case __NR_sysinfo: /* syscall 116 */
         /* int sysinfo(struct sysinfo *info); */
         if (VG_(clo_trace_syscalls)) 
            VG_(printf)("sysinfo ( %p )\n",arg1);
         must_be_writable( "sysinfo(info)", arg1, sizeof(struct sysinfo) );
         KERNEL_DO_SYSCALL(res);
         if (!VG_(is_kerror)(res))
            make_readable( arg1, sizeof(struct sysinfo) );
         break;

      case __NR_time: /* syscall 13 */
         /* time_t time(time_t *t); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("time ( %p )\n",arg1);
         if (arg1 != (UInt)NULL) {
            must_be_writable( "time", arg1, sizeof(time_t) );
         }
         KERNEL_DO_SYSCALL(res);
         if (!VG_(is_kerror)(res) && arg1 != (UInt)NULL) {
            make_readable( arg1, sizeof(time_t) );
         }
         break;

      case __NR_times: /* syscall 43 */
         /* clock_t times(struct tms *buf); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("times ( %p )\n",arg1);
         must_be_writable( "times(buf)", arg1, sizeof(struct tms) );
         KERNEL_DO_SYSCALL(res);
         if (!VG_(is_kerror)(res) && arg1 != (UInt)NULL) {
            make_readable( arg1, sizeof(struct tms) );
         }
         break;

      case __NR_truncate: /* syscall 92 */
         /* int truncate(const char *path, size_t length); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("truncate ( %p, %d )\n", arg1,arg2);
         must_be_readable_asciiz( "truncate(path)", arg1 );
         KERNEL_DO_SYSCALL(res);
         break;

      case __NR_umask: /* syscall 60 */
         /* mode_t umask(mode_t mask); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("umask ( %d )\n", arg1);
         KERNEL_DO_SYSCALL(res);
         break;

      case __NR_unlink: /* syscall 10 */
         /* int unlink(const char *pathname) */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("ulink ( %p )\n",arg1);
         must_be_readable_asciiz( "unlink(pathname)", arg1 );
         KERNEL_DO_SYSCALL(res);
         break;

      case __NR_uname: /* syscall 122 */
         /* int uname(struct utsname *buf); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("uname ( %p )\n",arg1);
         must_be_writable( "uname(buf)", arg1, sizeof(struct utsname) );
         KERNEL_DO_SYSCALL(res);
         if (!VG_(is_kerror)(res) && arg1 != (UInt)NULL) {
            make_readable( arg1, sizeof(struct utsname) );
         }
         break;

      case __NR_utime: /* syscall 30 */
         /* int utime(const char *filename, struct utimbuf *buf); */
         if (VG_(clo_trace_syscalls)) 
            VG_(printf)("utime ( %p, %p )\n", arg1,arg2);
         must_be_readable_asciiz( "utime(filename)", arg1 );
         if (arg2 != (UInt)NULL)
            must_be_readable( "utime(buf)", arg2, 
                                            sizeof(struct utimbuf) );
         KERNEL_DO_SYSCALL(res);
         break;

      case __NR_wait4: /* syscall 114 */
         /* pid_t wait4(pid_t pid, int *status, int options,
                        struct rusage *rusage) */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("wait4 ( %d, %p, %d, %p )\n",
                      arg1,arg2,arg3,arg4);
         if (arg2 != (Addr)NULL)
            must_be_writable( "wait4(status)", arg2, sizeof(int) );
         if (arg4 != (Addr)NULL)
            must_be_writable( "wait4(rusage)", arg4, sizeof(struct rusage) );
         KERNEL_DO_SYSCALL(res);
         if (!VG_(is_kerror)(res)) {
            if (arg2 != (Addr)NULL)
               make_readable( arg2, sizeof(int) );
            if (arg4 != (Addr)NULL)
               make_readable( arg4, sizeof(struct rusage) );
         }
         break;

      case __NR_write: /* syscall 4 */
         /* size_t write(int fd, const void *buf, size_t count); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("write ( %d, %p, %d )\n",arg1,arg2,arg3);
         must_be_readable( "write(buf)", arg2, arg3 );
         KERNEL_DO_SYSCALL(res);
         break;

      case __NR_writev: { /* syscall 146 */
         /* int writev(int fd, const struct iovec * vector, size_t count); */
         UInt i;
         struct iovec * vec;
         if (VG_(clo_trace_syscalls))
            VG_(printf)("writev ( %d, %p, %d )\n",arg1,arg2,arg3);
         must_be_readable( "writev(vector)", 
                           arg2, arg3 * sizeof(struct iovec) );
         /* ToDo: don't do any of the following if the vector is invalid */
         vec = (struct iovec *)arg2;
         for (i = 0; i < arg3; i++)
            must_be_readable( "writev(vector[...])",
                              (UInt)vec[i].iov_base,vec[i].iov_len );
         KERNEL_DO_SYSCALL(res);
         break;
      }

      /*-------------------------- SIGNALS --------------------------*/

      /* Normally set to 1, so that Valgrind's signal-simulation machinery
         is engaged.  Sometimes useful to disable (set to 0), for
         debugging purposes, to make clients more deterministic. */
#     define SIGNAL_SIMULATION 1

      case __NR_rt_sigaction:
      case __NR_sigaction:
         /* int sigaction(int signum, struct k_sigaction *act, 
                                      struct k_sigaction *oldact); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("sigaction ( %d, %p, %p )\n",arg1,arg2,arg3);
         if (arg2 != (UInt)NULL)
            must_be_readable( "sigaction(act)", 
                              arg2, sizeof(vki_ksigaction));
         if (arg3 != (UInt)NULL)
            must_be_writable( "sigaction(oldact)", 
                              arg3, sizeof(vki_ksigaction));
         /* We do this one ourselves! */
#        if SIGNAL_SIMULATION
         VG_(do__NR_sigaction)();
         res = VG_(baseBlock)[VGOFF_(m_eax)];
#        else
         /* debugging signals; when we don't handle them. */
         KERNEL_DO_SYSCALL(res);
#        endif
         if (!VG_(is_kerror)(res) && res == 0 && arg3 != (UInt)NULL)
            make_readable( arg3, sizeof(vki_ksigaction));
         break;

      case __NR_rt_sigprocmask:
      case __NR_sigprocmask:
         /* int sigprocmask(int how, k_sigset_t *set, 
                                     k_sigset_t *oldset); */
         if (VG_(clo_trace_syscalls))
            VG_(printf)("sigprocmask ( %d, %p, %p )\n",arg1,arg2,arg3);
         if (arg2 != (UInt)NULL)
            must_be_readable( "sigprocmask(set)", 
                              arg2, sizeof(vki_ksigset_t));
         if (arg3 != (UInt)NULL)
            must_be_writable( "sigprocmask(oldset)", 
                              arg3, sizeof(vki_ksigset_t));
         KERNEL_DO_SYSCALL(res);
         if (!VG_(is_kerror)(res) && res == 0 && arg3 != (UInt)NULL)
            make_readable( arg3, sizeof(vki_ksigset_t));
#        if SIGNAL_SIMULATION
         /* For the reason why both the kernel and Valgrind process
            sigprocmask, see the detailed comment at
            vg_do__NR_sigprocmask(). */
         VG_(do__NR_sigprocmask) ( arg1 /*how*/, (vki_ksigset_t*) arg2 );
#        endif
         break;

      default:
         VG_(message)
            (Vg_DebugMsg,"FATAL: unhandled syscall: %d",syscallno);
         VG_(message)
            (Vg_DebugMsg,"Do not panic.  You may be able to fix this easily.");
         VG_(message)
            (Vg_DebugMsg,"Read the file README_MISSING_SYSCALL_OR_IOCTL.");
         VG_(unimplemented)("no wrapper for the above system call");
         vg_assert(3+3 == 7);
         break; /*NOTREACHED*/
   }

   /* Tell the signal handler machinery that we've finished the
      syscall.  */
   VG_(syscall_depth) --;
      
   /* { void zzzmemscan(void); zzzmemscan(); } */

   /* Finish off with some sanity checks.  */
   vg_assert( VG_(syscall_depth) == syscall_depth_saved );

   if (! VG_(first_and_last_secondaries_look_plausible))
      sane_before_call = False;

   if (sane_before_call && (!sane_after_call)) {
      VG_(message)(Vg_DebugMsg, "valgrind syscall handler: ");
      VG_(message)(Vg_DebugMsg,
                   "probable sanity check failure for syscall number %d\n", 
                   syscallno );
      VG_(panic)("aborting due to the above ... bye!"); 
   }

   VGP_POPCC;
}


/*--------------------------------------------------------------------*/
/*--- end                                         vg_syscall_mem.c ---*/
/*--------------------------------------------------------------------*/
