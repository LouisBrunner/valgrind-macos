
/*--------------------------------------------------------------------*/
/*--- Intercepts for various libc functions we want to capture     ---*/
/*--- (mostly for threading purposes).              vg_intercept.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an extensible x86 protected-mode
   emulator for monitoring program execution on x86-Unixes.

   Copyright (C) 2000-2003 Julian Seward 
      jseward@acm.org

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


/* ---------------------------------------------------------------------
   ALL THE CODE IN THIS FILE RUNS ON THE SIMULATED CPU.  It is
   intended for various reasons as drop-in replacements for libc
   functions.  These functions have global visibility (obviously) and
   have no prototypes in vg_include.h, since they are not intended to
   be called from within Valgrind.
   ------------------------------------------------------------------ */

/* General idea (2003-Apr-26) is that master implementations of
   selected functions are done as VGR_(fnname).  Then we route
   all calls to the master, both here and in vg_libpthread.c.
   This means we no longer have to rely on the semantics of weak
   symbols, which seems to have changed in glibc >= 2.3.2 in such
   a way as to make the previous interception scheme stop working.
*/

/* Sidestep the normal check which disallows using valgrind.h
   directly. */
#define __VALGRIND_SOMESKIN_H
#include "valgrind.h"

#include "vg_include.h"
#include "vg_kerneliface.h"

/* This has some nasty duplication of stuff from vg_libpthread.c */

#include <errno.h>
#include <sys/types.h>
#include <stdio.h>
#include <sys/ipc.h>
#include <sys/msg.h>
#include <asm/ipc.h>		/* for ipc_kludge */
#include <sys/poll.h>
#include <sys/socket.h>
#include <sys/uio.h>
#ifdef GLIBC_2_1
#include <sys/time.h>
#endif

/* --------------------------------------------------------------- */

# define strong_alias(name, aliasname) \
  extern __typeof (name) aliasname __attribute__ ((alias (#name)));

#define WEAK	__attribute__((weak))

/* --------------------------------------------------------------- */

static __inline__
int my_do_syscall1 ( int syscallno, int arg1 )
{ 
   int __res;
   __asm__ volatile ("pushl %%ebx; movl %%edx,%%ebx ; int $0x80 ; popl %%ebx"
                     : "=a" (__res)
                     : "0" (syscallno),
                       "d" (arg1) );
   return __res;
}

static __inline__
int my_do_syscall2 ( int syscallno, 
                     int arg1, int arg2 )
{ 
   int __res;
   __asm__ volatile ("pushl %%ebx; movl %%edx,%%ebx ; int $0x80 ; popl %%ebx"
                     : "=a" (__res)
                     : "0" (syscallno),
                       "d" (arg1),
                       "c" (arg2) );
   return __res;
}

static __inline__
int my_do_syscall3 ( int syscallno, 
                     int arg1, int arg2, int arg3 )
{ 
   int __res;
   __asm__ volatile ("pushl %%ebx; movl %%esi,%%ebx ; int $0x80 ; popl %%ebx"
                     : "=a" (__res)
                     : "0" (syscallno),
                       "S" (arg1),
                       "c" (arg2),
                       "d" (arg3) );
   return __res;
}

static __inline__
int my_do_syscall5 ( int syscallno, 
                     int arg1, int arg2, int arg3, int arg4, int arg5 )
{ 
   int __res;
   __asm__ volatile ("int $0x80"
                     : "=a" (__res)
                     : "0" (syscallno),
                       "b" (arg1),
                       "c" (arg2),
                       "d" (arg3),
                       "S" (arg4),
                       "D" (arg5));
   return __res;
}

static __inline__
int do_syscall_select( int n, 
                       vki_fd_set* readfds, 
                       vki_fd_set* writefds, 
                       vki_fd_set* exceptfds, 
                       struct vki_timeval * timeout )
{
   int res;
   int args[5];
   args[0] = n;
   args[1] = (int)readfds;
   args[2] = (int)writefds;
   args[3] = (int)exceptfds;
   args[4] = (int)timeout;
   res = my_do_syscall1(__NR_select, (int)(&(args[0])) );
   return res;
}


static __inline__
int do_syscall_ipc( unsigned call, 
                    int first, int second, int third, 
                    void *ptr)
{
   return my_do_syscall5(__NR_ipc, call, first, second, third, (int)ptr);
}


/* --------------------------------------------------------------- */

/* Just start up Valgrind if it's not already going.  VG_(startup)()
   detects and ignores second and subsequent calls. */

/* We need this guy -- it's in valgrind.so. */
extern void VG_(startup) ( void );

static __inline__
void ensure_valgrind ( char* caller )
{
   VG_(startup)();
}

static __inline__
int is_kerror ( int res )
{
   if (res >= -4095 && res <= -1)
      return 1;
   else
      return 0;
}

/* --------------------------------------------------------------- */

/* Extract from Valgrind the value of VG_(clo_trace_pthread_level).
   Returns 0 (none) if not running on Valgrind. */
static
int get_pt_trace_level ( void )
{
   int res;
   VALGRIND_MAGIC_SEQUENCE(res, 0 /* default */,
                           VG_USERREQ__GET_PTHREAD_TRACE_LEVEL,
                           0, 0, 0, 0);
   return res;
}

static 
void cat_n_send ( char* pre, char* msg )
{
   char  buf[1000];
   if (get_pt_trace_level() >= 0) {
      snprintf(buf, sizeof(buf), "%s%s", pre, msg );
      buf[sizeof(buf)-1] = '\0';
      VALGRIND_NON_SIMD_CALL2(VG_(message), Vg_UserMsg, buf);
   }
}

static
void my_exit ( int arg )
{
   my_do_syscall1(__NR_exit, arg);
   /*NOTREACHED*/
}

static
void my_assert_fail ( const Char* expr, const Char* file, Int line, const Char* fn )
{
   char buf[1000];
   static Bool entered = False;
   if (entered) 
      my_exit(2);
   entered = True;
   sprintf(buf, "\n%s: %s:%d (%s): Assertion `%s' failed.\n",
                "valgrind", file, line, fn, expr );
   cat_n_send ( "", buf );
   sprintf(buf, "Please report this bug to me at: %s\n\n", 
                 VG_EMAIL_ADDR);
   cat_n_send ( "", buf );
   my_exit(1);
}

#define MY__STRING(__str)  #__str

#define my_assert(expr)                                               \
  ((void) ((expr) ? 0 :						      \
	   (my_assert_fail  (MY__STRING(expr),			      \
			      __FILE__, __LINE__,                     \
                              __PRETTY_FUNCTION__), 0)))

/* --------------------------------------------------------------- */

static __inline__
void __my_pthread_testcancel(void)
{
   int res;
   ensure_valgrind("__my_pthread_testcancel");
   VALGRIND_MAGIC_SEQUENCE(res, (-1) /* default */,
                           VG_USERREQ__TESTCANCEL,
                           0, 0, 0, 0);
   my_assert(res == 0);
}

/* ================================ poll ================================ */

/* This is the master implementation of poll().  It blocks only the
   calling thread.  All roads lead here.
*/

#ifndef HAVE_NFDS_T
typedef unsigned long int nfds_t;
#endif


int VGR_(poll) (struct pollfd *__fds, nfds_t __nfds, int __timeout)
{
   unsigned int        ms_now, ms_end, i;
   int                 res;
   struct vki_timespec nanosleep_interval;

   __my_pthread_testcancel();
   ensure_valgrind("poll");

   /* Detect the current time and simultaneously find out if we are
      running on Valgrind. */
   VALGRIND_MAGIC_SEQUENCE(ms_now, 0xFFFFFFFF /* default */,
                           VG_USERREQ__READ_MILLISECOND_TIMER,
                           0, 0, 0, 0);


   /* CHECK SIZES FOR struct pollfd */
   my_assert(sizeof(struct timeval) == sizeof(struct vki_timeval));

   /* dummy initialisation to keep gcc -Wall happy */
   ms_end = 0;

   /* If a zero timeout specified, this call is harmless.  Also do
      this if not running on Valgrind. */
   if (__timeout == 0 || ms_now == 0xFFFFFFFF) {
      res = my_do_syscall3(__NR_poll, (int)__fds, __nfds, __timeout);
      if (is_kerror(res)) {
         * (__errno_location()) = -res;
         return -1;
      } else {
         return res;
      }
   }

   /* If a timeout was specified, set ms_end to be the end wallclock
      time.  Easy considering that __timeout is in milliseconds. */
   if (__timeout > 0) {
      ms_end = ms_now + (unsigned int)__timeout;
   }

   /* fprintf(stderr, "MY_POLL: before loop\n"); */

   /* Either timeout < 0, meaning wait indefinitely, or timeout > 0,
      in which case t_end holds the end time. */

   my_assert(__timeout != 0);

   while (1) {

      /* Do a return-immediately poll. */

      res = my_do_syscall3(__NR_poll, (int)__fds, __nfds, 0 );
      if (is_kerror(res)) {
         /* Some kind of error.  Set errno and return.  */
         * (__errno_location()) = -res;
         return -1;
      }
      if (res > 0) {
         /* One or more fds is ready.  Return now. */
         return res;
      }

      /* Nothing interesting happened, so we go to sleep for a
         while. */

      /* fprintf(stderr, "MY_POLL: nanosleep\n"); */
      /* nanosleep and go round again */
      nanosleep_interval.tv_sec  = 0;
      nanosleep_interval.tv_nsec = 13 * 1000 * 1000; /* 13 milliseconds */
      /* It's critical here that valgrind's nanosleep implementation
         is nonblocking. */
      res = my_do_syscall2(__NR_nanosleep, 
                           (int)(&nanosleep_interval), (int)NULL);
      if (res == -VKI_EINTR) {
         /* The nanosleep was interrupted by a signal.  So we do the
            same. */
         * (__errno_location()) = EINTR;
         return -1;
      }

      /* Sleeping finished.  If a finite timeout, check to see if it
         has expired yet. */
      if (__timeout > 0) {
         VALGRIND_MAGIC_SEQUENCE(ms_now, 0xFFFFFFFF /* default */,
                                 VG_USERREQ__READ_MILLISECOND_TIMER,
                                 0, 0, 0, 0);
         my_assert(ms_now != 0xFFFFFFFF);
         if (ms_now >= ms_end) {
            /* timeout; nothing interesting happened. */
            for (i = 0; i < __nfds; i++) 
               __fds[i].revents = 0;
            return 0;
         }
      }
   }
}

int poll(struct pollfd *__fds, nfds_t __nfds, int __timeout)
{
   return VGR_(poll)(__fds, __nfds, __timeout);
}

strong_alias(poll, __poll)

/* ================================ msgsnd ================================ */

/* Turn a blocking msgsnd() into a polling non-blocking one, so that
   other threads make progress */
int VGR_(msgsnd)(int msgid, 
                 const void *msgp, 
                 /*size_t*/ unsigned int msgsz, 
                 int msgflg)
{
   struct vki_timespec nanosleep_interval;
   int err;

   ensure_valgrind("msgsnd");

   nanosleep_interval.tv_sec  = 0;
   nanosleep_interval.tv_nsec = 13 * 1000 * 1000; /* 13 milliseconds */

   if (msgflg & IPC_NOWAIT) {
      /* If we aren't blocking anyway, just do it */
      err = do_syscall_ipc(11, msgid, msgsz, msgflg, (void *)msgp);
   } else {
      /* Otherwise poll on the queue to let other things run */
      for(;;) {
	 err = do_syscall_ipc(11, msgid, msgsz, msgflg | IPC_NOWAIT, 
                                  (void *)msgp);

	 if (err != -EAGAIN)
	    break;

	 (void)my_do_syscall2(__NR_nanosleep, 
			      (int)(&nanosleep_interval), (int)NULL);
      }  
   }

   if (is_kerror(err)) {
      *(__errno_location()) = -err;
      return -1;
   }
   return 0;
}

#ifdef GLIBC_2_1
int msgsnd(int msgid,       void *msgp, size_t msgsz, int msgflg)
#else
int msgsnd(int msgid, const void *msgp, size_t msgsz, int msgflg)
#endif
{
   return VGR_(msgsnd)(msgid, msgp, msgsz, msgflg);
}

/* ================================ msgrcv ================================ */

/* Turn a blocking msgrcv() into a polling non-blocking one, so that
   other threads make progress */
int VGR_(msgrcv)( int msqid, 
                  void* msgp,  
                  /*size_t*/ unsigned int msgsz, 
                  long msgtyp, 
                  int msgflg )
{
   struct vki_timespec nanosleep_interval;
   int err;
   struct ipc_kludge tmp;

   ensure_valgrind("msgrcv");

   nanosleep_interval.tv_sec  = 0;
   nanosleep_interval.tv_nsec = 13 * 1000 * 1000; /* 13 milliseconds */

   tmp.msgp = msgp;
   tmp.msgtyp = msgtyp;

   if (msgflg & IPC_NOWAIT) {
      /* If we aren't blocking anyway, just do it */
      err = do_syscall_ipc(12, msqid, msgsz, msgflg, &tmp );
   } else {
      /* Otherwise poll on the queue to let other things run */
      for(;;) {
	 err = do_syscall_ipc(12, msqid, msgsz, msgflg | IPC_NOWAIT, &tmp );

	 if (err != -ENOMSG)
	    break;

	 (void)my_do_syscall2(__NR_nanosleep, 
			      (int)(&nanosleep_interval), (int)NULL);
      }  
   }
   
   if (is_kerror(err)) {
      *(__errno_location()) = -err;
      return -1;
   }

   return err;
}

int msgrcv( int msqid, void  *msgp,  size_t msgsz, long msgtyp, int msgflg )
{
   return VGR_(msgrcv)( msqid, msgp,  msgsz, msgtyp, msgflg );
}

/* ================================ accept ================================ */

extern
int __libc_accept(int s, struct sockaddr *addr, socklen_t *addrlen);

int VGR_(accept)(int s, /*struct sockaddr*/ void *addr, 
                        /*socklen_t*/ void *addrlen)
{
   __my_pthread_testcancel();
   VGR_(wait_for_fd_to_be_readable_or_erring)(s);
   __my_pthread_testcancel();
   return __libc_accept(s, addr, addrlen);
}

int accept(int s, struct sockaddr *addr, socklen_t *addrlen)
{
   return VGR_(accept)(s, addr, addrlen);
}

/* ================================ recv ================================ */

extern
int __libc_recv(int s, void *buf, size_t len, int flags);

int VGR_(recv)(int s, void *buf, size_t len, int flags)
{
   __my_pthread_testcancel();
   VGR_(wait_for_fd_to_be_readable_or_erring)(s);
   __my_pthread_testcancel();
   return __libc_recv(s, buf, len, flags);
}

int recv(int s, void *buf, size_t len, int flags)
{
   return VGR_(recv)(s, buf, len, flags);
}

strong_alias(recv, __recv)

/* ================================ select ================================ */

/* This is a wrapper round select(), which makes it thread-safe,
   meaning that only this thread will block, rather than the entire
   process.  This wrapper in turn depends on nanosleep() not to block
   the entire process, but I think (hope? suspect?) that POSIX
   pthreads guarantees that to be the case.

   Basic idea is: modify the timeout parameter to select so that it
   returns immediately.  Poll like this until select returns non-zero,
   indicating something interesting happened, or until our time is up.
   Space out the polls with nanosleeps of say 11 milliseconds, which
   is required to be nonblocking; this allows other threads to run.  

   Assumes:
   * (checked via my_assert) types fd_set and vki_fd_set are identical.
   * (checked via my_assert) types timeval and vki_timeval are identical.
   * (unchecked) libc error numbers (EINTR etc) are the negation of the
     kernel's error numbers (VKI_EINTR etc).
*/

int VGR_(select) ( int n, 
                   /*fd_set*/ void *rfdsV, 
                   /*fd_set*/ void *wfdsV, 
                   /*fd_set*/ void *xfdsV, 
                   /*struct timeval*/ void *timeoutV )
{
   unsigned int ms_now, ms_end;
   int    res;
   fd_set rfds_copy;
   fd_set wfds_copy;
   fd_set xfds_copy;
   struct vki_timeval  t_now;
   struct vki_timeval  zero_timeout;
   struct vki_timespec nanosleep_interval;

   struct timeval* timeout = (struct timeval*)timeoutV;
   fd_set* rfds = (fd_set*)rfdsV;
   fd_set* wfds = (fd_set*)wfdsV;
   fd_set* xfds = (fd_set*)xfdsV;

   __my_pthread_testcancel();

   /* gcc's complains about ms_end being used uninitialised -- classic
      case it can't understand, where ms_end is both defined and used
      only if timeout != NULL.  Hence ... */
   ms_end = 0;

   /* We assume that the kernel and libc data layouts are identical
      for the following types.  These asserts provide a crude
      check. */
   my_assert(sizeof(struct timeval) == sizeof(struct vki_timeval));

   /* Detect the current time and simultaneously find out if we are
      running on Valgrind. */
   VALGRIND_MAGIC_SEQUENCE(ms_now, 0xFFFFFFFF /* default */,
                           VG_USERREQ__READ_MILLISECOND_TIMER,
                           0, 0, 0, 0);

   /* If a zero timeout specified, this call is harmless.  Also go
      this route if we're not running on Valgrind, for whatever
      reason. */
   if ( (timeout && timeout->tv_sec == 0 && timeout->tv_usec == 0)
        || (ms_now == 0xFFFFFFFF) ) {
      res = do_syscall_select( n, (vki_fd_set*)rfds, 
                                   (vki_fd_set*)wfds, 
                                   (vki_fd_set*)xfds, 
                                   (struct vki_timeval*)timeout);
      if (is_kerror(res)) {
         * (__errno_location()) = -res;
         return -1;
      } else {
         return res;
      }
   }

   /* If a timeout was specified, set ms_end to be the end millisecond
      counter [wallclock] time. */
   if (timeout) {
      res = my_do_syscall2(__NR_gettimeofday, (int)&t_now, (int)NULL);
      my_assert(res == 0);
      ms_end = ms_now;
      ms_end += (timeout->tv_usec / 1000);
      ms_end += (timeout->tv_sec * 1000);
      /* Stay sane ... */
      if (ms_end < ms_now)
         ms_end = ms_now;
   }

   /* fprintf(stderr, "MY_SELECT: before loop\n"); */

   /* Either timeout == NULL, meaning wait indefinitely, or timeout !=
      NULL, in which case ms_end holds the end time. */

   while (1) {

      /* First, do a return-immediately select(). */

      /* These could be trashed each time round the loop, so restore
         them each time. */
      if (rfds) rfds_copy = *rfds;
      if (wfds) wfds_copy = *wfds;
      if (xfds) xfds_copy = *xfds;

      zero_timeout.tv_sec = zero_timeout.tv_usec = 0;

      res = do_syscall_select( n, 
                               rfds ? (vki_fd_set*)(&rfds_copy) : NULL,
                               wfds ? (vki_fd_set*)(&wfds_copy) : NULL,
                               xfds ? (vki_fd_set*)(&xfds_copy) : NULL,
                               & zero_timeout );
      if (is_kerror(res)) {
         /* Some kind of error (including EINTR).  Set errno and
            return.  The sets are unspecified in this case. */
         * (__errno_location()) = -res;
         return -1;
      }
      if (res > 0) {
         /* one or more fds is ready.  Copy out resulting sets and
            return. */
         if (rfds) *rfds = rfds_copy;
         if (wfds) *wfds = wfds_copy;
         if (xfds) *xfds = xfds_copy;
         return res;
      }

      /* Nothing interesting happened, so we go to sleep for a
         while. */

      /* fprintf(stderr, "MY_SELECT: nanosleep\n"); */
      /* nanosleep and go round again */
      nanosleep_interval.tv_sec  = 0;
      nanosleep_interval.tv_nsec = 11 * 1000 * 1000; /* 11 milliseconds */
      /* It's critical here that valgrind's nanosleep implementation
         is nonblocking. */
      res = my_do_syscall2(__NR_nanosleep, 
                           (int)(&nanosleep_interval), (int)NULL);
      if (res == -VKI_EINTR) {
         /* The nanosleep was interrupted by a signal.  So we do the
            same. */
         * (__errno_location()) = EINTR;
         return -1;
      }

      /* Sleeping finished.  If a finite timeout, check to see if it
         has expired yet. */
      if (timeout) {
         VALGRIND_MAGIC_SEQUENCE(ms_now, 0xFFFFFFFF /* default */,
                                 VG_USERREQ__READ_MILLISECOND_TIMER,
                                 0, 0, 0, 0);
         my_assert(ms_now != 0xFFFFFFFF);
         if (ms_now >= ms_end) {
            /* timeout; nothing interesting happened. */
            if (rfds) FD_ZERO(rfds);
            if (wfds) FD_ZERO(wfds);
            if (xfds) FD_ZERO(xfds);
            return 0;
         }
      }

   }
}

int select ( int n, 
             fd_set *rfds, 
             fd_set *wfds, 
             fd_set *xfds, 
             struct timeval *timeout )
{
   return VGR_(select)(n, rfds, wfds, xfds, timeout);
}

strong_alias(select, __select)

/* ================================ readv ================================ */

int VGR_(readv)(int fd, const /*struct iovec*/ void *iovV, int count)
{
   int res;
   const struct iovec* iov = (const struct iovec*)iovV;

   __my_pthread_testcancel();
   VGR_(wait_for_fd_to_be_readable_or_erring)(fd);
   __my_pthread_testcancel();
   res = my_do_syscall3(__NR_readv, fd, (unsigned)iov, count);

   if (is_kerror(res)) {
      *(__errno_location()) = -res;
      return -1;
   }
   return res;
}

int readv (int fd, const struct iovec *iov, int count)
{
   return VGR_(readv)(fd, iov, count);
}

strong_alias(readv, __readv)

/* ================================ writev ================================ */

int VGR_(writev)(int fd, const /*struct iovec*/ void *iovV, int count)
{
   int res;
   struct iovec* iov = (struct iovec*)iovV;

   __my_pthread_testcancel();
   VGR_(wait_for_fd_to_be_writable_or_erring)(fd);
   __my_pthread_testcancel();
   res = my_do_syscall3(__NR_writev, fd, (unsigned)iov, count);

   if (is_kerror(res)) {
      *(__errno_location()) = -res;
      return -1;
   }
   return res;
}

int writev (int fd, const struct iovec *iov, int count)
{
   return VGR_(writev)(fd, iov, count);
}

strong_alias(writev, __writev)

/* ================================ sigsuspend ============================ */

/* ---------------------------------------------------------------------
   Horrible hack to make sigsuspend() sort-of work OK.  Same trick as
   for pause() in vg_libpthread.so.
   ------------------------------------------------------------------ */

/* Horrible because

   -- uses VG_(ksigprocmask), VG_(nanosleep) and vg_assert, which are 
      valgrind-native (not intended for client use).

   -- This is here so single-threaded progs (not linking libpthread.so)
      can see it.  But pause() should also be here.  ???
*/

/* Either libc supplies this (weak) or our libpthread.so supplies it
   (strong) in a threaded setting. 
*/
extern int* __errno_location ( void );


int sigsuspend ( /* const sigset_t * */ void* mask)
{
   unsigned int n_orig, n_now;
   struct vki_timespec nanosleep_interval;

   VALGRIND_MAGIC_SEQUENCE(n_orig, 0xFFFFFFFF /* default */,
                           VG_USERREQ__GET_N_SIGS_RETURNED, 
                           0, 0, 0, 0);
   vg_assert(n_orig != 0xFFFFFFFF);

   VG_(ksigprocmask)(VKI_SIG_SETMASK, mask, NULL);

   while (1) {
      VALGRIND_MAGIC_SEQUENCE(n_now, 0xFFFFFFFF /* default */,
                              VG_USERREQ__GET_N_SIGS_RETURNED, 
                              0, 0, 0, 0);
      vg_assert(n_now != 0xFFFFFFFF);
      vg_assert(n_now >= n_orig);
      if (n_now != n_orig) break;

      nanosleep_interval.tv_sec  = 0;
      nanosleep_interval.tv_nsec = 53 * 1000 * 1000; /* 53 milliseconds */
      /* It's critical here that valgrind's nanosleep implementation
         is nonblocking. */
      VG_(nanosleep)( &nanosleep_interval, NULL);
   }

   /* Maybe this is OK both in single and multithreaded setting. */
   * (__errno_location()) = -VKI_EINTR; /* == EINTR; */ 
   return -1;
}


/* ================================ waitpid ============================ */

#undef WNOHANG
#define WNOHANG         0x00000001

extern pid_t __libc_waitpid(pid_t pid, int *status, int options);

pid_t waitpid(pid_t pid, int *status, int options)
{
   pid_t res;
   struct vki_timespec nanosleep_interval;

   if (options & WNOHANG)
      return __libc_waitpid(pid,status,options);

   options |= WNOHANG;
   while (1) {
      res = __libc_waitpid(pid,status,options);
      if (res != 0)
         return res;

      nanosleep_interval.tv_sec  = 0;
      nanosleep_interval.tv_nsec = 54 * 1000 * 1000; /* 54 milliseconds */
      /* It's critical here that valgrind's nanosleep implementation
         is nonblocking. */
      VG_(nanosleep)( &nanosleep_interval, NULL);
   }
}

#undef WNOHANG

/* ---------------------------------------------------------------------
   Hook for running __libc_freeres once the program exits.
   ------------------------------------------------------------------ */

void VG_(__libc_freeres_wrapper)( void )
{
   int res;
   extern void __libc_freeres(void);
   __libc_freeres();
   VALGRIND_MAGIC_SEQUENCE(res, 0 /* default */,
                           VG_USERREQ__LIBC_FREERES_DONE, 0, 0, 0, 0);
   /*NOTREACHED*/
   vg_assert(12345+54321 == 999999);
}

/* ---------------------------------------------------------------------
   Useful for skins that want to replace certain functions
   ------------------------------------------------------------------ */

Bool VG_(is_running_on_simd_CPU)(void)
{
   return VG_(running_on_simd_CPU);
}


/* ---------------------------------------------------------------------
   Helpers for safely (nonblockingly) detecting when a file descriptor
   is safe to use.
   ------------------------------------------------------------------ */

/* Helper function used to make accept() non-blocking.  Idea is to use
   the above nonblocking poll() to make this thread ONLY wait for the
   specified fd to become ready, and then return. */

/* Sigh -- a hack.  We're not supposed to include this file directly;
   should do it via /usr/include/fcntl.h, but that introduces a
   varargs prototype for fcntl itself, which we can't mimic. */
#define _FCNTL_H
#include <bits/fcntl.h>

extern int __libc_fcntl(int fd, int cmd, long arg);

void VGR_(wait_for_fd_to_be_readable_or_erring) ( int fd )
{
   struct pollfd pfd;
   int           res;

   /* fprintf(stderr, "wait_for_fd_to_be_readable_or_erring %d\n", fd); */

   /* First check to see if the fd is nonblocking, and/or invalid.  In
      either case return immediately. */
   res = __libc_fcntl(fd, F_GETFL, 0);
   if (res == -1) return; /* fd is invalid somehow */
   if (res & O_NONBLOCK) return; /* fd is nonblocking */

   /* Ok, we'd better wait with poll. */
   pfd.fd = fd;
   pfd.events = POLLIN | POLLPRI | POLLERR | POLLHUP | POLLNVAL;
   /* ... but not POLLOUT, you may notice. */
   pfd.revents = 0;
   (void)poll(&pfd, 1, -1 /* forever */);
}

void VGR_(wait_for_fd_to_be_writable_or_erring) ( int fd )
{
   struct pollfd pfd;
   int           res;

   /* fprintf(stderr, "wait_for_fd_to_be_readable_or_erring %d\n", fd); */

   /* First check to see if the fd is nonblocking, and/or invalid.  In
      either case return immediately. */
   res = __libc_fcntl(fd, F_GETFL, 0);
   if (res == -1) return; /* fd is invalid somehow */
   if (res & O_NONBLOCK) return; /* fd is nonblocking */

   /* Ok, we'd better wait with poll. */
   pfd.fd = fd;
   pfd.events = POLLOUT | POLLERR | POLLHUP | POLLNVAL;
   pfd.revents = 0;
   (void)poll(&pfd, 1, -1 /* forever */);
}

/*--------------------------------------------------------------------*/
/*--- end                                           vg_intercept.c ---*/
/*--------------------------------------------------------------------*/

