
/*--------------------------------------------------------------------*/
/*--- Give dummy bindings for everything the real libpthread.so    ---*/
/*--- binds.                                 vg_libpthread_unimp.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an x86 protected-mode emulator 
   designed for debugging and profiling binaries on x86-Unixes.

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

   The GNU General Public License is contained in the file LICENSE.
*/

/* ---------------------------------------------------------------------
   ALL THIS CODE RUNS ON THE SIMULATED CPU.
   Give a binding for everything the real libpthread.so binds.
   ------------------------------------------------------------------ */

extern void vgPlain_unimp ( char* );
#define unimp(str) vgPlain_unimp(str)

//void _IO_flockfile ( void )  { unimp("_IO_flockfile"); }
void _IO_ftrylockfile ( void )  { unimp("_IO_ftrylockfile"); }
//void _IO_funlockfile ( void )  { unimp("_IO_funlockfile"); }
//void __close ( void )  { unimp("__close"); }
//void __connect ( void )  { unimp("__connect"); }
//void __errno_location ( void )  { unimp("__errno_location"); }
//void __fcntl ( void )  { unimp("__fcntl"); }
//void __fork ( void )  { unimp("__fork"); }
//void __h_errno_location ( void )  { unimp("__h_errno_location"); }
void __libc_allocate_rtsig ( void )  { unimp("__libc_allocate_rtsig"); }
void __libc_current_sigrtmax ( void )  { unimp("__libc_current_sigrtmax"); }
void __libc_current_sigrtmin ( void )  { unimp("__libc_current_sigrtmin"); }
//void __lseek ( void )  { unimp("__lseek"); }
//void __open ( void )  { unimp("__open"); }
//void __open64 ( void )  { unimp("__open64"); }
//void __pread64 ( void )  { unimp("__pread64"); }
//void __pthread_atfork ( void )  { unimp("__pthread_atfork"); }
//void __pthread_getspecific ( void )  { unimp("__pthread_getspecific"); }
//void __pthread_key_create ( void )  { unimp("__pthread_key_create"); }
//void __pthread_kill_other_threads_np ( void )  { unimp("__pthread_kill_other_threads_np"); }
//void __pthread_mutex_destroy ( void )  { unimp("__pthread_mutex_destroy"); }
//void __pthread_mutex_init ( void )  { unimp("__pthread_mutex_init"); }
//void __pthread_mutex_lock ( void )  { unimp("__pthread_mutex_lock"); }
//void __pthread_mutex_trylock ( void )  { unimp("__pthread_mutex_trylock"); }
//void __pthread_mutex_unlock ( void )  { unimp("__pthread_mutex_unlock"); }
//void __pthread_mutexattr_destroy ( void )  { unimp("__pthread_mutexattr_destroy"); }
//void __pthread_mutexattr_init ( void )  { unimp("__pthread_mutexattr_init"); }
//void __pthread_mutexattr_settype ( void )  { unimp("__pthread_mutexattr_settype"); }
//void __pthread_once ( void )  { unimp("__pthread_once"); }
//void __pthread_setspecific ( void )  { unimp("__pthread_setspecific"); }
//void __pwrite64 ( void )  { unimp("__pwrite64"); }
//void __read ( void )  { unimp("__read"); }
//void __res_state ( void )  { unimp("__res_state"); }
//void __send ( void )  { unimp("__send"); }
//void __sigaction ( void )  { unimp("__sigaction"); }
void __vfork ( void )  { unimp("__vfork"); }
//void __wait ( void )  { unimp("__wait"); }
//void __write ( void )  { unimp("__write"); }
//void _pthread_cleanup_pop ( void )  { unimp("_pthread_cleanup_pop"); }
//void _pthread_cleanup_pop_restore ( void )  { unimp("_pthread_cleanup_pop_restore"); }
//void _pthread_cleanup_push ( void )  { unimp("_pthread_cleanup_push"); }
//void _pthread_cleanup_push_defer ( void )  { unimp("_pthread_cleanup_push_defer"); }
//void longjmp ( void )  { unimp("longjmp"); }
//void pthread_atfork ( void )  { unimp("pthread_atfork"); }
//void pthread_attr_destroy ( void )  { unimp("pthread_attr_destroy"); }
void pthread_attr_getdetachstate ( void )  { unimp("pthread_attr_getdetachstate"); }
void pthread_attr_getinheritsched ( void )  { unimp("pthread_attr_getinheritsched"); }
//void pthread_attr_getschedparam ( void )  { unimp("pthread_attr_getschedparam"); }
void pthread_attr_getschedpolicy ( void )  { unimp("pthread_attr_getschedpolicy"); }
void pthread_attr_getscope ( void )  { unimp("pthread_attr_getscope"); }

//void pthread_attr_setdetachstate ( void )  { unimp("pthread_attr_setdetachstate"); }
//void pthread_attr_setinheritsched ( void )  { unimp("pthread_attr_setinheritsched"); }
//void pthread_attr_setschedparam ( void )  { unimp("pthread_attr_setschedparam"); }
void pthread_attr_setschedpolicy ( void )  { unimp("pthread_attr_setschedpolicy"); }
void pthread_attr_setscope ( void )  { unimp("pthread_attr_setscope"); }
void pthread_barrier_destroy ( void )  { unimp("pthread_barrier_destroy"); }
void pthread_barrier_init ( void )  { unimp("pthread_barrier_init"); }
void pthread_barrier_wait ( void )  { unimp("pthread_barrier_wait"); }
void pthread_barrierattr_destroy ( void )  { unimp("pthread_barrierattr_destroy"); }
void pthread_barrierattr_init ( void )  { unimp("pthread_barrierattr_init"); }
void pthread_barrierattr_setpshared ( void )  { unimp("pthread_barrierattr_setpshared"); }
//void pthread_cancel ( void )  { unimp("pthread_cancel"); }
//void pthread_cond_broadcast ( void )  { unimp("pthread_cond_broadcast"); }
//void pthread_cond_destroy ( void )  { unimp("pthread_cond_destroy"); }
//void pthread_cond_init ( void )  { unimp("pthread_cond_init"); }
//void pthread_cond_signal ( void )  { unimp("pthread_cond_signal"); }
//void pthread_cond_timedwait ( void )  { unimp("pthread_cond_timedwait"); }
//void pthread_cond_wait ( void )  { unimp("pthread_cond_wait"); }
//void pthread_condattr_destroy ( void )  { unimp("pthread_condattr_destroy"); }
void pthread_condattr_getpshared ( void )  { unimp("pthread_condattr_getpshared"); }
//void pthread_condattr_init ( void )  { unimp("pthread_condattr_init"); }
void pthread_condattr_setpshared ( void )  { unimp("pthread_condattr_setpshared"); }
//void pthread_detach ( void )  { unimp("pthread_detach"); }
//void pthread_equal ( void )  { unimp("pthread_equal"); }
//void pthread_exit ( void )  { unimp("pthread_exit"); }
void pthread_getattr_np ( void )  { unimp("pthread_getattr_np"); }
void pthread_getcpuclockid ( void )  { unimp("pthread_getcpuclockid"); }
//void pthread_getschedparam ( void )  { unimp("pthread_getschedparam"); }
//void pthread_getspecific ( void )  { unimp("pthread_getspecific"); }
//void pthread_join ( void )  { unimp("pthread_join"); }
//void pthread_key_create ( void )  { unimp("pthread_key_create"); }
//void pthread_key_delete ( void )  { unimp("pthread_key_delete"); }
//void pthread_kill ( void )  { unimp("pthread_kill"); }
//void pthread_mutex_destroy ( void )  { unimp("pthread_mutex_destroy"); }
//void pthread_mutex_init ( void )  { unimp("pthread_mutex_init"); }
//void pthread_mutex_lock ( void )  { unimp("pthread_mutex_lock"); }
void pthread_mutex_timedlock ( void )  { unimp("pthread_mutex_timedlock"); }
//void pthread_mutex_trylock ( void )  { unimp("pthread_mutex_trylock"); }
//void pthread_mutex_unlock ( void )  { unimp("pthread_mutex_unlock"); }
//void pthread_mutexattr_destroy ( void )  { unimp("pthread_mutexattr_destroy"); }
//void pthread_mutexattr_init ( void )  { unimp("pthread_mutexattr_init"); }
//void pthread_once ( void )  { unimp("pthread_once"); }
void pthread_rwlock_destroy ( void )  { unimp("pthread_rwlock_destroy"); }
void pthread_rwlock_init ( void )  { unimp("pthread_rwlock_init"); }
//void pthread_rwlock_rdlock ( void )  { unimp("pthread_rwlock_rdlock"); }
void pthread_rwlock_timedrdlock ( void )  { unimp("pthread_rwlock_timedrdlock"); }
void pthread_rwlock_timedwrlock ( void )  { unimp("pthread_rwlock_timedwrlock"); }
void pthread_rwlock_tryrdlock ( void )  { unimp("pthread_rwlock_tryrdlock"); }
void pthread_rwlock_trywrlock ( void )  { unimp("pthread_rwlock_trywrlock"); }
//void pthread_rwlock_unlock ( void )  { unimp("pthread_rwlock_unlock"); }
//void pthread_rwlock_wrlock ( void )  { unimp("pthread_rwlock_wrlock"); }
void pthread_rwlockattr_destroy ( void )  { unimp("pthread_rwlockattr_destroy"); }
void pthread_rwlockattr_getkind_np ( void )  { unimp("pthread_rwlockattr_getkind_np"); }
void pthread_rwlockattr_getpshared ( void )  { unimp("pthread_rwlockattr_getpshared"); }
void pthread_rwlockattr_init ( void )  { unimp("pthread_rwlockattr_init"); }
void pthread_rwlockattr_setkind_np ( void )  { unimp("pthread_rwlockattr_setkind_np"); }
void pthread_rwlockattr_setpshared ( void )  { unimp("pthread_rwlockattr_setpshared"); }
//void pthread_self ( void )  { unimp("pthread_self"); }
//void pthread_setcancelstate ( void )  { unimp("pthread_setcancelstate"); }
//void pthread_setcanceltype ( void )  { unimp("pthread_setcanceltype"); }
//void pthread_setschedparam ( void )  { unimp("pthread_setschedparam"); }
//void pthread_setspecific ( void )  { unimp("pthread_setspecific"); }
//void pthread_sigmask ( void )  { unimp("pthread_sigmask"); }
//void pthread_testcancel ( void )  { unimp("pthread_testcancel"); }
//void raise ( void )  { unimp("raise"); }
void sem_close ( void )  { unimp("sem_close"); }
void sem_open ( void )  { unimp("sem_open"); }
void sem_timedwait ( void )  { unimp("sem_timedwait"); }
void sem_unlink ( void )  { unimp("sem_unlink"); }
//void sigaction ( void )  { unimp("sigaction"); }
void siglongjmp ( void )  { unimp("siglongjmp"); }
//void sigwait ( void )  { unimp("sigwait"); }

#if 0
void pthread_create@@GLIBC_2.1 ( void )  { unimp("pthread_create@@GLIBC_2.1"); }
void pthread_create@GLIBC_2.0 ( void )  { unimp("pthread_create@GLIBC_2.0"); }

void sem_wait@@GLIBC_2.1 ( void )  { unimp("sem_wait@@GLIBC_2.1"); }
void sem_wait@GLIBC_2.0 ( void )  { unimp("sem_wait@GLIBC_2.0"); }

void sem_trywait@@GLIBC_2.1 ( void )  { unimp("sem_trywait@@GLIBC_2.1"); }
void sem_trywait@GLIBC_2.0 ( void )  { unimp("sem_trywait@GLIBC_2.0"); }

void sem_post@@GLIBC_2.1 ( void )  { unimp("sem_post@@GLIBC_2.1"); }
void sem_post@GLIBC_2.0 ( void )  { unimp("sem_post@GLIBC_2.0"); }

void sem_destroy@@GLIBC_2.1 ( void )  { unimp("sem_destroy@@GLIBC_2.1"); }
void sem_destroy@GLIBC_2.0 ( void )  { unimp("sem_destroy@GLIBC_2.0"); }
void sem_getvalue@@GLIBC_2.1 ( void )  { unimp("sem_getvalue@@GLIBC_2.1"); }
void sem_getvalue@GLIBC_2.0 ( void )  { unimp("sem_getvalue@GLIBC_2.0"); }
void sem_init@@GLIBC_2.1 ( void )  { unimp("sem_init@@GLIBC_2.1"); }
void sem_init@GLIBC_2.0 ( void )  { unimp("sem_init@GLIBC_2.0"); }

void pthread_attr_init@@GLIBC_2.1 ( void )  { unimp("pthread_attr_init@@GLIBC_2.1"); }
void pthread_attr_init@GLIBC_2.0 ( void )  { unimp("pthread_attr_init@GLIBC_2.0"); }
#endif



# define strong_alias(name, aliasname) \
  extern __typeof (name) aliasname __attribute__ ((alias (#name)));

# define weak_alias(name, aliasname) \
  extern __typeof (name) aliasname __attribute__ ((weak, alias (#name)));

weak_alias(pthread_rwlock_destroy, __pthread_rwlock_destroy)
weak_alias(pthread_rwlock_init, __pthread_rwlock_init)
weak_alias(pthread_rwlock_tryrdlock, __pthread_rwlock_tryrdlock)
weak_alias(pthread_rwlock_trywrlock, __pthread_rwlock_trywrlock)
//weak_alias(pthread_rwlock_wrlock, __pthread_rwlock_wrlock)
weak_alias(_IO_ftrylockfile, ftrylockfile)

__attribute__((weak)) void pread ( void ) { vgPlain_unimp("pread"); }
__attribute__((weak)) void pwrite ( void ) { vgPlain_unimp("pwrite"); }
__attribute__((weak)) void msync ( void ) { vgPlain_unimp("msync"); }
__attribute__((weak)) void pause ( void ) { vgPlain_unimp("pause"); }
__attribute__((weak)) void recvfrom ( void ) { vgPlain_unimp("recvfrom"); }
__attribute__((weak)) void recvmsg ( void ) { vgPlain_unimp("recvmsg"); }
//__attribute__((weak)) void sendmsg ( void ) { vgPlain_unimp("sendmsg"); }
__attribute__((weak)) void tcdrain ( void ) { vgPlain_unimp("tcdrain"); }
__attribute__((weak)) void vfork ( void ) { vgPlain_unimp("vfork"); }

__attribute__((weak)) void pthread_attr_getguardsize ( void )
                      { vgPlain_unimp("pthread_attr_getguardsize"); }
__attribute__((weak)) void pthread_attr_getstack ( void )
                      { vgPlain_unimp("pthread_attr_getstack"); }
__attribute__((weak)) void pthread_attr_getstackaddr ( void )
                      { vgPlain_unimp("pthread_attr_getstackaddr"); }
__attribute__((weak)) void pthread_attr_getstacksize ( void )
                      { vgPlain_unimp("pthread_attr_getstacksize"); }
__attribute__((weak)) void pthread_attr_setguardsize ( void )
                      { vgPlain_unimp("pthread_attr_setguardsize"); }
__attribute__((weak)) void pthread_attr_setstack ( void )
                      { vgPlain_unimp("pthread_attr_setstack"); }
__attribute__((weak)) void pthread_attr_setstackaddr ( void )
                      { vgPlain_unimp("pthread_attr_setstackaddr"); }
//__attribute__((weak)) void pthread_attr_setstacksize ( void )
//                      { vgPlain_unimp("pthread_attr_setstacksize"); }
__attribute__((weak)) void pthread_getconcurrency ( void )
                      { vgPlain_unimp("pthread_getconcurrency"); }
__attribute__((weak)) void pthread_kill_other_threads_np ( void )
                      { vgPlain_unimp("pthread_kill_other_threads_np"); }
__attribute__((weak)) void pthread_mutexattr_getkind_np ( void )
                      { vgPlain_unimp("pthread_mutexattr_getkind_np"); }
__attribute__((weak)) void pthread_mutexattr_getpshared ( void )
                      { vgPlain_unimp("pthread_mutexattr_getpshared"); }
__attribute__((weak)) void pthread_mutexattr_gettype ( void )
                      { vgPlain_unimp("pthread_mutexattr_gettype"); }
__attribute__((weak)) void pthread_mutexattr_setkind_np ( void )
                      { vgPlain_unimp("pthread_mutexattr_setkind_np"); }
__attribute__((weak)) void pthread_mutexattr_setpshared ( void )
                      { vgPlain_unimp("pthread_mutexattr_setpshared"); }
__attribute__((weak)) void pthread_setconcurrency ( void )
                      { vgPlain_unimp("pthread_setconcurrency"); }
__attribute__((weak)) void pthread_spin_destroy ( void )
                      { vgPlain_unimp("pthread_spin_destroy"); }
__attribute__((weak)) void pthread_spin_init ( void )
                      { vgPlain_unimp("pthread_spin_init"); }
__attribute__((weak)) void pthread_spin_lock ( void )
                      { vgPlain_unimp("pthread_spin_lock"); }
__attribute__((weak)) void pthread_spin_trylock ( void )
                      { vgPlain_unimp("pthread_spin_trylock"); }
__attribute__((weak)) void pthread_spin_unlock ( void )
                      { vgPlain_unimp("pthread_spin_unlock"); }
__attribute__((weak)) void pthread_yield ( void )
                      { vgPlain_unimp("pthread_yield"); }


/*--------------------------------------------------------------------*/
/*--- end                                    vg_libpthread_unimp.c ---*/
/*--------------------------------------------------------------------*/
