
/*--------------------------------------------------------------------*/
/*--- Arch-specific libpthread code.              x86/libpthread.c ---*/
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

#include "core.h"        /* For the VG_USERREQ__* constants */

#define __USE_UNIX98
#include <pthread.h>
#undef __USE_UNIX98

#define __USE_GNU
#include <dlfcn.h>
#undef __USE_GNU

#include <errno.h>

// Struct used to describe a TDB header, copied from glibc.
typedef
   struct {
      void *tcb;
      void *dtv;
      void *self;                
      int multiple_threads;
      unsigned long sysinfo;
   }
   tcbhead_t;

/* --------------------------------------------------- 
   Helper functions for running a thread 
   and for clearing up afterwards.
   ------------------------------------------------ */

typedef void *(*__attribute__ ((stdcall)) REGPARM(3) allocate_tls_t) (void *result);
typedef void (*__attribute__ ((stdcall)) REGPARM(3) deallocate_tls_t) (void *tcb, int dealloc_tcb);

static allocate_tls_t allocate_tls = NULL;
static deallocate_tls_t deallocate_tls = NULL;

static int get_gs()
{
   int gs;
   asm volatile ("movw %%gs, %w0" : "=q" (gs));
   return gs & 0xffff;
}

static void set_gs( int gs )
{
   asm volatile ("movw %w0, %%gs" :: "q" (gs));
}

static void *get_tcb()
{
   void *tcb;
   asm volatile ("movl %%gs:0, %0" : "=r" (tcb));
   return tcb;
}


Bool VGA_(has_tls)(void)
{
   return (get_gs() & 7) == 3;
}  


void VGA_(thread_create)(ThreadArchAux *aux)
{
   if (VGA_(has_tls)()) {
      tcbhead_t *tcb = get_tcb();

      if (allocate_tls == NULL || deallocate_tls == NULL) {
         allocate_tls = (allocate_tls_t)dlsym(RTLD_DEFAULT, "_dl_allocate_tls");
         deallocate_tls = (deallocate_tls_t)dlsym(RTLD_DEFAULT, "_dl_deallocate_tls");
      }

      my_assert(allocate_tls != NULL);
      
      aux->tls_data = allocate_tls(NULL);
      aux->tls_segment = get_gs() >> 3;
      aux->sysinfo = tcb->sysinfo;

      tcb->multiple_threads = 1;
   } else {
      aux->tls_data = NULL;
      aux->tls_segment = -1;
      aux->sysinfo = 0;
   }
}
   
void VGA_(thread_wrapper)(ThreadArchAux *aux)
{
   void*         tls_data;
   int           tls_segment;
   unsigned long sysinfo;
      
   tls_data    = aux->tls_data;
   tls_segment = aux->tls_segment;
   sysinfo     = aux->sysinfo;

   if (tls_data) {
      tcbhead_t *tcb = tls_data;
      vki_modify_ldt_t ldt_info;

      /* Fill in the TCB header */
      tcb->tcb = tcb;
      tcb->self = tcb;
      tcb->multiple_threads = 1;
      tcb->sysinfo = sysinfo;
      
      /* Fill in an LDT descriptor */
      ldt_info.entry_number = tls_segment;
      ldt_info.base_addr = (unsigned long)tls_data;
      ldt_info.limit = 0xfffff;
      ldt_info.seg_32bit = 1;
      ldt_info.contents = 0;
      ldt_info.read_exec_only = 0;
      ldt_info.limit_in_pages = 1;
      ldt_info.seg_not_present = 0;
      ldt_info.useable = 1;
      ldt_info.reserved = 0;
      
      /* Install the thread area */
      VG_(do_syscall)(__NR_set_thread_area, &ldt_info);
      
      /* Setup the GS segment register */
      set_gs(ldt_info.entry_number * 8 + 3);
   }
}
   
void VGA_(thread_exit)(void)
{
   /* Free up any TLS data */
   if ((get_gs() & 7) == 3 && pthread_self() > 1) {
      my_assert(deallocate_tls != NULL);
      deallocate_tls(get_tcb(), 1);
   }
}   

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
