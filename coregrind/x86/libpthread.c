
/*--------------------------------------------------------------------*/
/*--- x86-specific libpthread code.               x86/libpthread.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an extensible x86 protected-mode
   emulator for monitoring program execution on x86-Unixes.

   Copyright (C) 2000-2004 Julian Seward 
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

/* ALL THIS CODE RUNS ON THE SIMULATED CPU.

   See the comments at the top of coregrind/vg_libpthread.c for some
   caveats.
*/

//#include "valgrind.h"    /* For the request-passing mechanism */
//#include "core.h"        /* For the VG_USERREQ__* constants */

//#define __USE_UNIX98
//#include <sys/types.h>
//#include <pthread.h>
//#undef __USE_UNIX98

//#define __USE_GNU
//#include <dlfcn.h>
//#undef __USE_GNU

#include <errno.h>

/* POSIX spinlocks, taken from glibc linuxthreads/sysdeps/i386 */

typedef volatile int pthread_spinlock_t; /* Huh?  Guarded by __USE_XOPEN2K */

int pthread_spin_init(pthread_spinlock_t *lock, int pshared)
{
  /* We can ignore the `pshared' parameter.  Since we are busy-waiting
     all processes which can access the memory location `lock' points
     to can use the spinlock.  */
  *lock = 1;
  return 0;
}

int pthread_spin_lock(pthread_spinlock_t *lock)
{
  asm volatile
    ("\n"
     "1:\n\t"
     "lock; decl %0\n\t"
     "js 2f\n\t"
     ".section .text.spinlock,\"ax\"\n"
     "2:\n\t"
     "cmpl $0,%0\n\t"
     "rep; nop\n\t"
     "jle 2b\n\t"
     "jmp 1b\n\t"
     ".previous"
     : "=m" (*lock));
  return 0;
}

int pthread_spin_unlock(pthread_spinlock_t *lock)
{
  asm volatile
    ("movl $1,%0"
     : "=m" (*lock));
  return 0;
}

int pthread_spin_destroy(pthread_spinlock_t *lock)
{
  /* Nothing to do.  */
  return 0;
}

int pthread_spin_trylock(pthread_spinlock_t *lock)
{
  int oldval;

  asm volatile
    ("xchgl %0,%1"
     : "=r" (oldval), "=m" (*lock)
     : "0" (0));
  return oldval > 0 ? 0 : EBUSY;
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
