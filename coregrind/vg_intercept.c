
/*--------------------------------------------------------------------*/
/*--- Intercepts for various libc functions we want to capture     ---*/
/*--- (mostly for threading purposes).              vg_intercept.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an extensible x86 protected-mode
   emulator for monitoring program execution on x86-Unixes.

   Copyright (C) 2000-2002 Julian Seward 
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
   All the code in this file runs on the SIMULATED CPU.  It is
   intended for various reasons as drop-in replacements for libc
   functions.  These functions have global visibility (obviously) and
   have no prototypes in vg_include.h, since they are not intended to
   be called from within Valgrind.
   ------------------------------------------------------------------ */

/* This has some nasty duplication of stuff from vg_libpthread.c */

#include <errno.h>
#include <sys/types.h>

/* Sidestep the normal check which disallows using valgrind.h
   directly. */
#define __VALGRIND_SOMESKIN_H
#include "valgrind.h"

#include "vg_include.h"

# define strong_alias(name, aliasname) \
  extern __typeof (name) aliasname __attribute__ ((alias (#name)));

# define weak_alias(name, aliasname) \
  extern __typeof (name) aliasname __attribute__ ((weak, alias (#name)));

#define WEAK	__attribute__((weak))

#include "vg_kerneliface.h"

static
__inline__
int is_kerror ( int res )
{
   if (res >= -4095 && res <= -1)
      return 1;
   else
      return 0;
}


static inline
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

/* -------------------------------- msgsnd -------------------------------- */
#include <asm/ipc.h>		/* for ipc_kludge */

static inline int sys_ipc(unsigned call, int first, int second, int third, void *ptr)
{
   return my_do_syscall5(__NR_ipc, call, first, second, third, (int)ptr);
}

WEAK int VGL_(msgsnd)(int msgid, const void *msgp, size_t msgsz, int msgflg)
{
   int err = sys_ipc(11, msgid, msgsz, msgflg, (void *)msgp);
   if (is_kerror(err)) {
      *(__errno_location()) = -err;
      return -1;
   }
   return 0;
}

int msgsnd(int msgid, const void *msgp, size_t msgsz, int msgflg)
{
   return VGL_(msgsnd)(msgid, msgp, msgsz, msgflg);
}

/* -------------------------------- msgrcv -------------------------------- */

WEAK int VGL_(msgrcv)( int msqid, void  *msgp,  size_t msgsz, long msgtyp, int msgflg )
{
   struct ipc_kludge tmp;
   int err;

   tmp.msgp = msgp;
   tmp.msgtyp = msgtyp;

   err = sys_ipc(12, msqid, msgsz, msgflg, &tmp );

   if (is_kerror(err)) {
      *(__errno_location()) = -err;
      return -1;
   }
   return err;
}

int msgrcv( int msqid, void  *msgp,  size_t msgsz, long msgtyp, int msgflg )
{
   return VGL_(msgrcv)( msqid, msgp,  msgsz, msgtyp, msgflg );
}

/* -------------------------------- accept -------------------------------- */

#include <sys/socket.h>

extern
int __libc_accept(int s, struct sockaddr *addr, socklen_t *addrlen);
WEAK int VGL_(accept)(int s, struct sockaddr *addr, socklen_t *addrlen)
{
   return __libc_accept(s, addr, addrlen);
}

int accept(int s, struct sockaddr *addr, socklen_t *addrlen)
{
   return VGL_(accept)(s, addr, addrlen);
}

/* -------------------------------- recv -------------------------------- */

extern
int __libc_recv(int s, void *buf, size_t len, int flags);

WEAK int VGL_(recv)(int s, void *buf, size_t len, int flags)
{
   return __libc_recv(s, buf, len, flags);
}

int recv(int s, void *buf, size_t len, int flags)
{
   return VGL_(recv)(s, buf, len, flags);
}

strong_alias(recv, __recv)

/* -------------------------------- poll -------------------------------- */

static inline
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

#include <sys/poll.h>

#ifndef HAVE_NFDS_T
typedef unsigned long int nfds_t;
#endif


WEAK int VGL_(poll)(struct pollfd *__fds, nfds_t __nfds, int __timeout)
{
   int res = my_do_syscall3(__NR_poll, (int)__fds, __nfds, __timeout);

   if (is_kerror(res)) {
      * (__errno_location()) = -res;
      return -1;
   }
   return res;
}

int poll(struct pollfd *__fds, nfds_t __nfds, int __timeout)
{
   return VGL_(poll)(__fds, __nfds, __timeout);
}

strong_alias(poll, __poll)


/* -------------------------------- select -------------------------------- */


static inline
int my_do_syscall1 ( int syscallno, int arg1 )
{ 
   int __res;
   __asm__ volatile ("pushl %%ebx; movl %%edx,%%ebx ; int $0x80 ; popl %%ebx"
                     : "=a" (__res)
                     : "0" (syscallno),
                       "d" (arg1) );
   return __res;
}


WEAK int VGL_(select)( int n, 
		       fd_set* readfds, 
		       fd_set* writefds, 
		       fd_set* exceptfds, 
		       struct timeval * timeout )
{
   int res;
   int args[5];
   args[0] = n;
   args[1] = (int)readfds;
   args[2] = (int)writefds;
   args[3] = (int)exceptfds;
   args[4] = (int)timeout;
   res = my_do_syscall1(__NR_select, (int)(&(args[0])) );

   if (is_kerror(res)) {
      *(__errno_location()) = -res;
      return -1;
   }
   return res;
}

int select ( int n, 
             fd_set *rfds, 
             fd_set *wfds, 
             fd_set *xfds, 
             struct timeval *timeout )
{
   return VGL_(select)(n, rfds, wfds, xfds, timeout);
}

strong_alias(select, __select)

/* -------------------------------- readv -------------------------------- */

#include <sys/uio.h>

WEAK int VGL_(readv)(int fd, const struct iovec *iov, int count)
{
   int res = my_do_syscall3(__NR_readv, fd, (unsigned)iov, count);

   if (is_kerror(res)) {
      *(__errno_location()) = -res;
      return -1;
   }

   return res;
}

int readv (int fd, const struct iovec *iov, int count)
{
   return VGL_(readv)(fd, iov, count);
}

strong_alias(readv, __readv)

/* -------------------------------- writev -------------------------------- */

WEAK int VGL_(writev)(int fd, const struct iovec *iov, int count)
{
   int res = my_do_syscall3(__NR_writev, fd, (unsigned)iov, count);

   if (is_kerror(res)) {
      *(__errno_location()) = -res;
      return -1;
   }

   return res;
}

int writev (int fd, const struct iovec *iov, int count)
{
   return VGL_(writev)(fd, iov, count);
}

strong_alias(writev, __writev)

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

/*--------------------------------------------------------------------*/
/*--- end                                           vg_intercept.c ---*/
/*--------------------------------------------------------------------*/

