
/*--------------------------------------------------------------------*/
/*--- A replacement for the standard libpthread.so.                ---*/
/*---                                              vg_libpthread.c ---*/
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

/* ALL THIS CODE RUNS ON THE SIMULATED CPU.

   This is a replacement for the standard libpthread.so.  It is loaded
   as part of the client's image (if required) and directs pthread
   calls through to Valgrind's request mechanism. 

   A couple of caveats.
 
   1.  Since it's a binary-compatible replacement for an existing library, 
       we must take care to used exactly the same data layouts, etc, as 
       the standard pthread.so does.  

   2.  Since this runs as part of the client, there are no specific
       restrictions on what headers etc we can include, so long as
       this libpthread.so does not end up having dependencies on .so's
       which the real one doesn't.

   Later ... it appears we cannot call file-related stuff in libc here,
   perhaps fair enough.  Be careful what you call from here.  Even exit()
   doesn't work (gives infinite recursion and then stack overflow); hence
   myexit().  Also fprintf doesn't seem safe.
*/

#include "valgrind.h"    /* For the request-passing mechanism */
#include "vg_include.h"  /* For the VG_USERREQ__* constants */

#define __USE_UNIX98
#include <sys/types.h>
#include <pthread.h>
#undef __USE_UNIX98

#include <unistd.h>
#include <string.h>
#ifdef GLIBC_2_1
#include <sys/time.h>
#endif

#include <stdio.h>


/* ---------------------------------------------------------------------
   Forwardses.
   ------------------------------------------------------------------ */

static void wait_for_fd_to_be_readable_or_erring ( int fd );

static
int my_do_syscall2 ( int syscallno, 
                     int arg1, int arg2 );


/* ---------------------------------------------------------------------
   Helpers.  We have to be pretty self-sufficient.
   ------------------------------------------------------------------ */

/* Number of times any given error message is printed. */
#define N_MOANS 3

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
void my_exit ( int arg )
{
   int __res;
   __asm__ volatile ("movl %%ecx, %%ebx ; int $0x80"
                     : "=a" (__res)
                     : "0" (__NR_exit),
                       "c" (arg) );
   /* We don't bother to mention the fact that this asm trashes %ebx,
      since it won't return.  If you ever do let it return ... fix
      this! */
}


/* We need this guy -- it's in valgrind.so. */
extern void VG_(startup) ( void );


/* Just start up Valgrind if it's not already going.  VG_(startup)()
   detects and ignores second and subsequent calls. */
static __inline__
void ensure_valgrind ( char* caller )
{
   VG_(startup)();
}

/* While we're at it ... hook our own startup function into this
   game. */
__asm__ (
   ".section .init\n"
   "\tcall vgPlain_startup"
);


static
__attribute__((noreturn))
void barf ( char* str )
{
   char buf[100];
   buf[0] = 0;
   strcat(buf, "\nvalgrind's libpthread.so: ");
   strcat(buf, str);
   strcat(buf, "\n\n");
   write(2, buf, strlen(buf));
   my_exit(1);
   /* We have to persuade gcc into believing this doesn't return. */
   while (1) { };
}


static void ignored ( char* msg )
{
   if (get_pt_trace_level() >= 0) {
      char* ig = "valgrind's libpthread.so: IGNORED call to: ";
      write(2, ig, strlen(ig));
      write(2, msg, strlen(msg));
      ig = "\n";
      write(2, ig, strlen(ig));
   }
}

static void kludged ( char* msg )
{
   if (get_pt_trace_level() >= 0) {
      char* ig = "valgrind's libpthread.so: KLUDGED call to: ";
      write(2, ig, strlen(ig));
      write(2, msg, strlen(msg));
      ig = "\n";
      write(2, ig, strlen(ig));
   }
}

static void not_inside ( char* msg )
{
   VG_(startup)();
}

__attribute__((noreturn))
void vgPlain_unimp ( char* what )
{
   char* ig = "valgrind's libpthread.so: UNIMPLEMENTED FUNCTION: ";
   write(2, ig, strlen(ig));
   write(2, what, strlen(what));
   ig = "\n";
   write(2, ig, strlen(ig));
   barf("Please report this bug to me at: jseward@acm.org");
}


static
void my_assert_fail ( Char* expr, Char* file, Int line, Char* fn )
{
   static Bool entered = False;
   if (entered) 
      my_exit(2);
   entered = True;
   fprintf(stderr, "\n%s: %s:%d (%s): Assertion `%s' failed.\n",
                   "valgrind", file, line, fn, expr );
   fprintf(stderr, "Please report this bug to me at: %s\n\n", 
                   VG_EMAIL_ADDR);
   my_exit(1);
}

#define MY__STRING(__str)  #__str

#define my_assert(expr)                                               \
  ((void) ((expr) ? 0 :						      \
	   (my_assert_fail  (MY__STRING(expr),			      \
			      __FILE__, __LINE__,                     \
                              __PRETTY_FUNCTION__), 0)))


/* ---------------------------------------------------------------------
   Pass pthread_ calls to Valgrind's request mechanism.
   ------------------------------------------------------------------ */

#include <errno.h>
#include <sys/time.h> /* gettimeofday */


/* ---------------------------------------------------
   Ummm ..
   ------------------------------------------------ */

static
void pthread_error ( const char* msg )
{
   int res;
   VALGRIND_MAGIC_SEQUENCE(res, 0,
                           VG_USERREQ__PTHREAD_ERROR, 
                           msg, 0, 0, 0);
}


/* ---------------------------------------------------
   THREAD ATTRIBUTES
   ------------------------------------------------ */

int pthread_attr_init(pthread_attr_t *attr)
{
   /* Just initialise the fields which we might look at. */
   attr->__detachstate = PTHREAD_CREATE_JOINABLE;
   return 0;
}

int pthread_attr_setdetachstate(pthread_attr_t *attr, int detachstate)
{
   if (detachstate != PTHREAD_CREATE_JOINABLE 
       && detachstate != PTHREAD_CREATE_DETACHED) {
      pthread_error("pthread_attr_setdetachstate: "
                    "detachstate is invalid");
      return EINVAL;
   }
   attr->__detachstate = detachstate;
   return 0;
}

int pthread_attr_setinheritsched(pthread_attr_t *attr, int inherit)
{
   static int moans = N_MOANS;
   if (moans-- > 0) 
      ignored("pthread_attr_setinheritsched");
   return 0;
}

__attribute__((weak))
int pthread_attr_setstacksize (pthread_attr_t *__attr,
                               size_t __stacksize)
{
   size_t limit;
   char buf[1024];
   ensure_valgrind("pthread_attr_setstacksize");
   limit = VG_PTHREAD_STACK_SIZE - VG_AR_CLIENT_STACKBASE_REDZONE_SZB 
                                 - 1000; /* paranoia */
   if (__stacksize < limit)
      return 0;
   snprintf(buf, sizeof(buf), "pthread_attr_setstacksize: "
            "requested size %d >= VG_PTHREAD_STACK_SIZE\n   "
            "edit vg_include.h and rebuild.", __stacksize);
   buf[sizeof(buf)-1] = '\0'; /* Make sure it is zero terminated */
   barf(buf);
}


/* This is completely bogus. */
int  pthread_attr_getschedparam(const  pthread_attr_t  *attr,  
                                struct sched_param *param)
{
   static int moans = N_MOANS;
   if (moans-- > 0) 
      kludged("pthread_attr_getschedparam");
#  ifdef HAVE_SCHED_PRIORITY
   if (param) param->sched_priority = 0; /* who knows */
#  else
   if (param) param->__sched_priority = 0; /* who knows */
#  endif
   return 0;
}

int  pthread_attr_setschedparam(pthread_attr_t  *attr,
                                const  struct sched_param *param)
{
   static int moans = N_MOANS;
   if (moans-- > 0) 
      ignored("pthread_attr_setschedparam");
   return 0;
}

int pthread_attr_destroy(pthread_attr_t *attr)
{
   static int moans = N_MOANS;
   if (moans-- > 0) 
      ignored("pthread_attr_destroy");
   return 0;
}

/* These are no-ops, as with LinuxThreads. */
int pthread_attr_setscope ( pthread_attr_t *attr, int scope )
{
   ensure_valgrind("pthread_attr_setscope");
   if (scope == PTHREAD_SCOPE_SYSTEM)
      return 0;
   pthread_error("pthread_attr_setscope: "
                 "invalid or unsupported scope");
   if (scope == PTHREAD_SCOPE_PROCESS)
      return ENOTSUP;
   return EINVAL;
}

int pthread_attr_getscope ( const pthread_attr_t *attr, int *scope )
{
   ensure_valgrind("pthread_attr_setscope");
   if (scope)
      *scope = PTHREAD_SCOPE_SYSTEM;
   return 0;
}


/* Pretty bogus.  Avoid if possible. */
int pthread_getattr_np (pthread_t thread, pthread_attr_t *attr)
{
   int    detached;
   size_t limit;
   ensure_valgrind("pthread_getattr_np");
   kludged("pthread_getattr_np");
   limit = VG_PTHREAD_STACK_SIZE - VG_AR_CLIENT_STACKBASE_REDZONE_SZB 
                                 - 1000; /* paranoia */
   attr->__detachstate = PTHREAD_CREATE_JOINABLE;
   attr->__schedpolicy = SCHED_OTHER;
   attr->__schedparam.sched_priority = 0;
   attr->__inheritsched = PTHREAD_EXPLICIT_SCHED;
   attr->__scope = PTHREAD_SCOPE_SYSTEM;
   attr->__guardsize = VKI_BYTES_PER_PAGE;
   attr->__stackaddr = NULL;
   attr->__stackaddr_set = 0;
   attr->__stacksize = limit;
   VALGRIND_MAGIC_SEQUENCE(detached, (-1) /* default */,
                           VG_USERREQ__SET_OR_GET_DETACH, 
                           2 /* get */, thread, 0, 0);
   my_assert(detached == 0 || detached == 1);
   if (detached)
      attr->__detachstate = PTHREAD_CREATE_DETACHED;
   return 0;
}


/* Bogus ... */
int pthread_attr_getstackaddr ( const pthread_attr_t * attr,
                                void ** stackaddr )
{
   ensure_valgrind("pthread_attr_getstackaddr");
   kludged("pthread_attr_getstackaddr");
   if (stackaddr)
      *stackaddr = NULL;
   return 0;
}

/* Not bogus (!) */
int pthread_attr_getstacksize ( const pthread_attr_t * _attr, 
                                size_t * __stacksize )
{
   size_t limit;
   ensure_valgrind("pthread_attr_getstacksize");
   limit = VG_PTHREAD_STACK_SIZE - VG_AR_CLIENT_STACKBASE_REDZONE_SZB 
                                 - 1000; /* paranoia */
   if (__stacksize)
      *__stacksize = limit;
   return 0;
}

int pthread_attr_setschedpolicy(pthread_attr_t *attr, int policy)
{
  if (policy != SCHED_OTHER && policy != SCHED_FIFO && policy != SCHED_RR)
    return EINVAL;
  attr->__schedpolicy = policy;
  return 0;
}

int pthread_attr_getschedpolicy(const pthread_attr_t *attr, int *policy)
{
  *policy = attr->__schedpolicy;
  return 0;
}


/* --------------------------------------------------- 
   Helper functions for running a thread 
   and for clearing up afterwards.
   ------------------------------------------------ */

/* All exiting threads eventually pass through here, bearing the
   return value, or PTHREAD_CANCELED, in ret_val. */
static
__attribute__((noreturn))
void thread_exit_wrapper ( void* ret_val )
{
   int           detached, res;
   CleanupEntry  cu;
   pthread_key_t key;

   /* Run this thread's cleanup handlers. */
   while (1) {
      VALGRIND_MAGIC_SEQUENCE(res, (-1) /* default */,
                              VG_USERREQ__CLEANUP_POP,
                              &cu, 0, 0, 0);
      if (res == -1) break; /* stack empty */
      my_assert(res == 0);
      if (0) printf("running exit cleanup handler");
      cu.fn ( cu.arg );
   }

   /* Run this thread's key finalizers.  Really this should be run
      PTHREAD_DESTRUCTOR_ITERATIONS times. */
   for (key = 0; key < VG_N_THREAD_KEYS; key++) {
      VALGRIND_MAGIC_SEQUENCE(res, (-2) /* default */,
                              VG_USERREQ__GET_KEY_D_AND_S,
                              key, &cu, 0, 0 );
      if (res == 0) {
         /* valid key */
         if (cu.fn && cu.arg)
            cu.fn /* destructor for key */ 
                  ( cu.arg /* specific for key for this thread */ );
         continue;
      }
      my_assert(res == -1);
   }

   /* Decide on my final disposition. */
   VALGRIND_MAGIC_SEQUENCE(detached, (-1) /* default */,
                           VG_USERREQ__SET_OR_GET_DETACH, 
                           2 /* get */, pthread_self(), 0, 0);
   my_assert(detached == 0 || detached == 1);

   if (detached) {
      /* Detached; I just quit right now. */
      VALGRIND_MAGIC_SEQUENCE(res, 0 /* default */,
                              VG_USERREQ__QUIT, 0, 0, 0, 0);
   } else {
      /* Not detached; so I wait for a joiner. */
      VALGRIND_MAGIC_SEQUENCE(res, 0 /* default */,
                              VG_USERREQ__WAIT_JOINER, ret_val, 0, 0, 0);
   }
   /* NOTREACHED */
   barf("thread_exit_wrapper: still alive?!");
}


/* This function is a wrapper function for running a thread.  It runs
   the root function specified in pthread_create, and then, should the
   root function return a value, it arranges to run the thread's
   cleanup handlers and exit correctly. */

/* Struct used to convey info from pthread_create to thread_wrapper.
   Must be careful not to pass to the child thread any pointers to
   objects which might be on the parent's stack.  */
typedef
   struct {
      int   attr__detachstate;
      void* (*root_fn) ( void* );
      void* arg;
   }
   NewThreadInfo;


/* This is passed to the VG_USERREQ__APPLY_IN_NEW_THREAD and so must
   not return.  Note that this runs in the new thread, not the
   parent. */
static
__attribute__((noreturn))
void thread_wrapper ( NewThreadInfo* info )
{
   int   res;
   int   attr__detachstate;
   void* (*root_fn) ( void* );
   void* arg;
   void* ret_val;

   attr__detachstate = info->attr__detachstate;
   root_fn           = info->root_fn;
   arg               = info->arg;

   /* Free up the arg block that pthread_create malloced. */
   VALGRIND_MAGIC_SEQUENCE(res, (-1) /* default */,
                           VG_USERREQ__FREE, info, 0, 0, 0);
   my_assert(res == 0);

   /* Minimally observe the attributes supplied. */
   if (attr__detachstate != PTHREAD_CREATE_DETACHED
       && attr__detachstate != PTHREAD_CREATE_JOINABLE)
      pthread_error("thread_wrapper: invalid attr->__detachstate");
   if (attr__detachstate == PTHREAD_CREATE_DETACHED)
      pthread_detach(pthread_self());

   /* The root function might not return.  But if it does we simply
      move along to thread_exit_wrapper.  All other ways out for the
      thread (cancellation, or calling pthread_exit) lead there
      too. */
   ret_val = root_fn(arg);
   thread_exit_wrapper(ret_val);
   /* NOTREACHED */
}


/* ---------------------------------------------------
   THREADs
   ------------------------------------------------ */

__attribute__((weak))
int pthread_yield ( void )
{
   int res;
   ensure_valgrind("pthread_yield");
   VALGRIND_MAGIC_SEQUENCE(res, 0 /* default */,
                           VG_USERREQ__PTHREAD_YIELD, 0, 0, 0, 0);
   return 0;
}


int pthread_equal(pthread_t thread1, pthread_t thread2)
{
   return thread1 == thread2 ? 1 : 0;
}


/* Bundle up the args into a malloc'd block and create a new thread
   consisting of thread_wrapper() applied to said malloc'd block. */
int
pthread_create (pthread_t *__restrict __thread,
                __const pthread_attr_t *__restrict __attr,
                void *(*__start_routine) (void *),
                void *__restrict __arg)
{
   int            tid_child;
   NewThreadInfo* info;

   ensure_valgrind("pthread_create");

   /* Allocate space for the arg block.  thread_wrapper will free
      it. */
   VALGRIND_MAGIC_SEQUENCE(info, NULL /* default */,
                           VG_USERREQ__MALLOC, 
                           sizeof(NewThreadInfo), 0, 0, 0);
   my_assert(info != NULL);

   if (__attr)
      info->attr__detachstate = __attr->__detachstate;
   else 
      info->attr__detachstate = PTHREAD_CREATE_JOINABLE;

   info->root_fn = __start_routine;
   info->arg     = __arg;
   VALGRIND_MAGIC_SEQUENCE(tid_child, VG_INVALID_THREADID /* default */,
                           VG_USERREQ__APPLY_IN_NEW_THREAD,
                           &thread_wrapper, info, 0, 0);
   my_assert(tid_child != VG_INVALID_THREADID);

   if (__thread)
      *__thread = tid_child;
   return 0; /* success */
}


int 
pthread_join (pthread_t __th, void **__thread_return)
{
   int res;
   ensure_valgrind("pthread_join");
   VALGRIND_MAGIC_SEQUENCE(res, 0 /* default */,
                           VG_USERREQ__PTHREAD_JOIN,
                           __th, __thread_return, 0, 0);
   return res;
}


void pthread_exit(void *retval)
{
   ensure_valgrind("pthread_exit");
   /* Simple! */
   thread_exit_wrapper(retval);
}


pthread_t pthread_self(void)
{
   int tid;
   ensure_valgrind("pthread_self");
   VALGRIND_MAGIC_SEQUENCE(tid, 1 /* default */,
                           VG_USERREQ__PTHREAD_GET_THREADID,
                           0, 0, 0, 0);
   if (tid < 1 || tid >= VG_N_THREADS)
      barf("pthread_self: invalid ThreadId");
   return tid;
}


int pthread_detach(pthread_t th)
{
   int res;
   ensure_valgrind("pthread_detach");
   /* First we enquire as to the current detach state. */
   VALGRIND_MAGIC_SEQUENCE(res, (-2) /* default */,
                           VG_USERREQ__SET_OR_GET_DETACH,
                           2 /* get */, th, 0, 0);
   if (res == -1) {
      /* not found */ 
      pthread_error("pthread_detach: "
                    "invalid target thread");
      return ESRCH;
   }
   if (res == 1) { 
      /* already detached */
      pthread_error("pthread_detach: "
                    "target thread is already detached");
      return EINVAL;
   }
   if (res == 0) {
      VALGRIND_MAGIC_SEQUENCE(res, (-2) /* default */,
                              VG_USERREQ__SET_OR_GET_DETACH,
                              1 /* set */, th, 0, 0);
      my_assert(res == 0);
      return 0;
   }
   barf("pthread_detach");
}


/* ---------------------------------------------------
   CLEANUP STACKS
   ------------------------------------------------ */

void _pthread_cleanup_push (struct _pthread_cleanup_buffer *__buffer,
                            void (*__routine) (void *),
                            void *__arg)
{
   int          res;
   CleanupEntry cu;
   ensure_valgrind("_pthread_cleanup_push");
   cu.fn  = __routine;
   cu.arg = __arg;
   VALGRIND_MAGIC_SEQUENCE(res, (-1) /* default */,
                           VG_USERREQ__CLEANUP_PUSH,
                           &cu, 0, 0, 0);
   my_assert(res == 0);
}


void _pthread_cleanup_push_defer (struct _pthread_cleanup_buffer *__buffer,
                                  void (*__routine) (void *),
                                  void *__arg)
{
   /* As _pthread_cleanup_push, but first save the thread's original
      cancellation type in __buffer and set it to Deferred. */
   int orig_ctype;
   ensure_valgrind("_pthread_cleanup_push_defer");
   /* Set to Deferred, and put the old cancellation type in res. */
   my_assert(-1 != PTHREAD_CANCEL_DEFERRED);
   my_assert(-1 != PTHREAD_CANCEL_ASYNCHRONOUS);
   my_assert(sizeof(struct _pthread_cleanup_buffer) >= sizeof(int));
   VALGRIND_MAGIC_SEQUENCE(orig_ctype, (-1) /* default */,
                           VG_USERREQ__SET_CANCELTYPE,
                           PTHREAD_CANCEL_DEFERRED, 0, 0, 0);   
   my_assert(orig_ctype != -1);
   *((int*)(__buffer)) = orig_ctype;
   /* Now push the cleanup. */
   _pthread_cleanup_push(NULL, __routine, __arg);
}


void _pthread_cleanup_pop (struct _pthread_cleanup_buffer *__buffer,
                           int __execute)
{
   int          res;
   CleanupEntry cu;
   ensure_valgrind("_pthread_cleanup_push");
   cu.fn = cu.arg = NULL; /* paranoia */
   VALGRIND_MAGIC_SEQUENCE(res, (-1) /* default */,
                           VG_USERREQ__CLEANUP_POP,
                           &cu, 0, 0, 0);
   if (res == 0) {
      /* pop succeeded */
     if (__execute) {
        cu.fn ( cu.arg );
     }
     return;
   }   
   if (res == -1) {
      /* stack underflow */
      return;
   }
   barf("_pthread_cleanup_pop");
}


void _pthread_cleanup_pop_restore (struct _pthread_cleanup_buffer *__buffer,
                                   int __execute)
{
   int orig_ctype, fake_ctype;
   /* As _pthread_cleanup_pop, but after popping/running the handler,
      restore the thread's original cancellation type from the first
      word of __buffer. */
   _pthread_cleanup_pop(NULL, __execute);
   orig_ctype = *((int*)(__buffer));
   my_assert(orig_ctype == PTHREAD_CANCEL_DEFERRED
          || orig_ctype == PTHREAD_CANCEL_ASYNCHRONOUS);
   my_assert(-1 != PTHREAD_CANCEL_DEFERRED);
   my_assert(-1 != PTHREAD_CANCEL_ASYNCHRONOUS);
   my_assert(sizeof(struct _pthread_cleanup_buffer) >= sizeof(int));
   VALGRIND_MAGIC_SEQUENCE(fake_ctype, (-1) /* default */,
                           VG_USERREQ__SET_CANCELTYPE,
                           orig_ctype, 0, 0, 0); 
   my_assert(fake_ctype == PTHREAD_CANCEL_DEFERRED);
}


/* ---------------------------------------------------
   MUTEX ATTRIBUTES
   ------------------------------------------------ */

int __pthread_mutexattr_init(pthread_mutexattr_t *attr)
{
   attr->__mutexkind = PTHREAD_MUTEX_ERRORCHECK_NP;
   return 0;
}

int __pthread_mutexattr_settype(pthread_mutexattr_t *attr, int type)
{
   switch (type) {
#     ifndef GLIBC_2_1    
      case PTHREAD_MUTEX_TIMED_NP:
      case PTHREAD_MUTEX_ADAPTIVE_NP:
#     endif
#     ifdef GLIBC_2_1    
      case PTHREAD_MUTEX_FAST_NP:
#     endif
      case PTHREAD_MUTEX_RECURSIVE_NP:
      case PTHREAD_MUTEX_ERRORCHECK_NP:
         attr->__mutexkind = type;
         return 0;
      default:
         pthread_error("pthread_mutexattr_settype: "
                       "invalid type");
         return EINVAL;
   }
}

int __pthread_mutexattr_destroy(pthread_mutexattr_t *attr)
{
   return 0;
}


/* ---------------------------------------------------
   MUTEXes
   ------------------------------------------------ */

int __pthread_mutex_init(pthread_mutex_t *mutex, 
                         const  pthread_mutexattr_t *mutexattr)
{
   mutex->__m_count = 0;
   mutex->__m_owner = (_pthread_descr)VG_INVALID_THREADID;
   mutex->__m_kind  = PTHREAD_MUTEX_ERRORCHECK_NP;
   if (mutexattr)
      mutex->__m_kind = mutexattr->__mutexkind;
   return 0;
}


int __pthread_mutex_lock(pthread_mutex_t *mutex)
{
   int res;
   static int moans = N_MOANS;
   if (RUNNING_ON_VALGRIND) {
      VALGRIND_MAGIC_SEQUENCE(res, 0 /* default */,
                              VG_USERREQ__PTHREAD_MUTEX_LOCK,
                              mutex, 0, 0, 0);
      return res;
   } else {
      if (moans-- > 0)
         not_inside("pthread_mutex_lock");
      return 0; /* success */
   }
}


int __pthread_mutex_trylock(pthread_mutex_t *mutex)
{
   int res;
   static int moans = N_MOANS;
   if (RUNNING_ON_VALGRIND) {
      VALGRIND_MAGIC_SEQUENCE(res, 0 /* default */,
                              VG_USERREQ__PTHREAD_MUTEX_TRYLOCK,
                              mutex, 0, 0, 0);
      return res;
   } else {
      if (moans-- > 0)
         not_inside("pthread_mutex_trylock");
      return 0;
   }
}


int __pthread_mutex_unlock(pthread_mutex_t *mutex)
{
   int res;
   static int moans = N_MOANS;
   if (RUNNING_ON_VALGRIND) {
      VALGRIND_MAGIC_SEQUENCE(res, 0 /* default */,
                              VG_USERREQ__PTHREAD_MUTEX_UNLOCK,
                              mutex, 0, 0, 0);
      return res;
   } else {
      if (moans-- > 0)
         not_inside("pthread_mutex_unlock");
      return 0;
   }
}


int __pthread_mutex_destroy(pthread_mutex_t *mutex)
{
   /* Valgrind doesn't hold any resources on behalf of the mutex, so no
      need to involve it. */
   if (mutex->__m_count > 0) {
       pthread_error("pthread_mutex_destroy: "
                     "mutex is still in use");
       return EBUSY;
   }
   mutex->__m_count = 0;
   mutex->__m_owner = (_pthread_descr)VG_INVALID_THREADID;
   mutex->__m_kind  = PTHREAD_MUTEX_ERRORCHECK_NP;
   return 0;
}


/* ---------------------------------------------------
   CONDITION VARIABLES
   ------------------------------------------------ */

/* LinuxThreads supports no attributes for conditions.  Hence ... */

int pthread_condattr_init(pthread_condattr_t *attr)
{
   return 0;
}

int pthread_condattr_destroy(pthread_condattr_t *attr)
{
   return 0;
}

int pthread_cond_init( pthread_cond_t *cond,
                       const pthread_condattr_t *cond_attr)
{
   cond->__c_waiting = (_pthread_descr)VG_INVALID_THREADID;
   return 0;
}

int pthread_cond_destroy(pthread_cond_t *cond)
{
   /* should check that no threads are waiting on this CV */
   static int moans = N_MOANS;
   if (moans-- > 0) 
      kludged("pthread_cond_destroy");
   return 0;
}

/* ---------------------------------------------------
   SCHEDULING
   ------------------------------------------------ */

/* This is completely bogus. */
int   pthread_getschedparam(pthread_t  target_thread,  
                            int  *policy,
                            struct sched_param *param)
{
   static int moans = N_MOANS;
   if (moans-- > 0) 
      kludged("pthread_getschedparam");
   if (policy) *policy = SCHED_OTHER;
#  ifdef HAVE_SCHED_PRIORITY
   if (param) param->sched_priority = 0; /* who knows */
#  else
   if (param) param->__sched_priority = 0; /* who knows */
#  endif
   return 0;
}

int pthread_setschedparam(pthread_t target_thread, 
                          int policy, 
                          const struct sched_param *param)
{
   static int moans = N_MOANS;
   if (moans-- > 0) 
      ignored("pthread_setschedparam");
   return 0;
}

int pthread_cond_wait(pthread_cond_t *cond, pthread_mutex_t *mutex)
{
   int res;
   ensure_valgrind("pthread_cond_wait");
   VALGRIND_MAGIC_SEQUENCE(res, 0 /* default */,
                           VG_USERREQ__PTHREAD_COND_WAIT,
			   cond, mutex, 0, 0);
   return res;
}

int pthread_cond_timedwait ( pthread_cond_t *cond, 
                             pthread_mutex_t *mutex, 
                             const struct  timespec *abstime )
{
   int res;
   unsigned int ms_now, ms_end;
   struct  timeval timeval_now;
   unsigned long long int ull_ms_now_after_1970;
   unsigned long long int ull_ms_end_after_1970;

   ensure_valgrind("pthread_cond_timedwait");
   VALGRIND_MAGIC_SEQUENCE(ms_now, 0xFFFFFFFF /* default */,
                           VG_USERREQ__READ_MILLISECOND_TIMER,
                           0, 0, 0, 0);
   my_assert(ms_now != 0xFFFFFFFF);
   res = gettimeofday(&timeval_now, NULL);
   my_assert(res == 0);

   ull_ms_now_after_1970 
      = 1000ULL * ((unsigned long long int)(timeval_now.tv_sec))
        + ((unsigned long long int)(timeval_now.tv_usec / 1000000));
   ull_ms_end_after_1970
      = 1000ULL * ((unsigned long long int)(abstime->tv_sec))
        + ((unsigned long long int)(abstime->tv_nsec / 1000000));
   if (ull_ms_end_after_1970 < ull_ms_now_after_1970)
      ull_ms_end_after_1970 = ull_ms_now_after_1970;
   ms_end 
      = ms_now + (unsigned int)(ull_ms_end_after_1970 - ull_ms_now_after_1970);
   VALGRIND_MAGIC_SEQUENCE(res, 0 /* default */,
                           VG_USERREQ__PTHREAD_COND_TIMEDWAIT,
			   cond, mutex, ms_end, 0);
   return res;
}


int pthread_cond_signal(pthread_cond_t *cond)
{
   int res;
   ensure_valgrind("pthread_cond_signal");
   VALGRIND_MAGIC_SEQUENCE(res, 0 /* default */,
                           VG_USERREQ__PTHREAD_COND_SIGNAL,
			   cond, 0, 0, 0);
   return res;
}

int pthread_cond_broadcast(pthread_cond_t *cond)
{
   int res;
   ensure_valgrind("pthread_cond_broadcast");
   VALGRIND_MAGIC_SEQUENCE(res, 0 /* default */,
                           VG_USERREQ__PTHREAD_COND_BROADCAST,
			   cond, 0, 0, 0);
   return res;
}


/* ---------------------------------------------------
   CANCELLATION
   ------------------------------------------------ */

int pthread_setcancelstate(int state, int *oldstate)
{
   int res;
   ensure_valgrind("pthread_setcancelstate");
   if (state != PTHREAD_CANCEL_ENABLE
       && state != PTHREAD_CANCEL_DISABLE) {
      pthread_error("pthread_setcancelstate: "
                    "invalid state");
      return EINVAL;
   }
   my_assert(-1 != PTHREAD_CANCEL_ENABLE);
   my_assert(-1 != PTHREAD_CANCEL_DISABLE);
   VALGRIND_MAGIC_SEQUENCE(res, (-1) /* default */,
                           VG_USERREQ__SET_CANCELSTATE,
                           state, 0, 0, 0);
   my_assert(res != -1);
   if (oldstate) 
      *oldstate = res;
   return 0;
}

int pthread_setcanceltype(int type, int *oldtype)
{
   int res;
   ensure_valgrind("pthread_setcanceltype");
   if (type != PTHREAD_CANCEL_DEFERRED
       && type != PTHREAD_CANCEL_ASYNCHRONOUS) {
      pthread_error("pthread_setcanceltype: "
                    "invalid type");
      return EINVAL;
   }
   my_assert(-1 != PTHREAD_CANCEL_DEFERRED);
   my_assert(-1 != PTHREAD_CANCEL_ASYNCHRONOUS);
   VALGRIND_MAGIC_SEQUENCE(res, (-1) /* default */,
                           VG_USERREQ__SET_CANCELTYPE,
                           type, 0, 0, 0);
   my_assert(res != -1);
   if (oldtype) 
      *oldtype = res;
   return 0;
}

int pthread_cancel(pthread_t thread)
{
   int res;
   ensure_valgrind("pthread_cancel");
   VALGRIND_MAGIC_SEQUENCE(res, (-1) /* default */,
                           VG_USERREQ__SET_CANCELPEND,
                           thread, &thread_exit_wrapper, 0, 0);
   my_assert(res != -1);
   return res;
}

static __inline__
void __my_pthread_testcancel(void)
{
   int res;
   VALGRIND_MAGIC_SEQUENCE(res, (-1) /* default */,
                           VG_USERREQ__TESTCANCEL,
                           0, 0, 0, 0);
   my_assert(res == 0);
}

void pthread_testcancel ( void )
{
   __my_pthread_testcancel();
}


/* Not really sure what this is for.  I suspect for doing the POSIX
   requirements for fork() and exec().  We do this internally anyway
   whenever those syscalls are observed, so this could be superfluous,
   but hey ... 
*/
void __pthread_kill_other_threads_np ( void )
{
   int res;
   ensure_valgrind("__pthread_kill_other_threads_np");
   VALGRIND_MAGIC_SEQUENCE(res, (-1) /* default */,
                           VG_USERREQ__NUKE_OTHER_THREADS,
                           0, 0, 0, 0);
   my_assert(res == 0);
}


/* ---------------------------------------------------
   SIGNALS
   ------------------------------------------------ */

#include <signal.h>

int pthread_sigmask(int how, const sigset_t *newmask, 
                             sigset_t *oldmask)
{
   int res;

   /* A bit subtle, because the scheduler expects newmask and oldmask
      to be vki_sigset_t* rather than sigset_t*, and the two are
      different.  Fortunately the first 64 bits of a sigset_t are
      exactly a vki_sigset_t, so we just pass the pointers through
      unmodified.  Haaaack! 

      Also mash the how value so that the SIG_ constants from glibc
      constants to VKI_ constants, so that the former do not have to
      be included into vg_scheduler.c. */

   ensure_valgrind("pthread_sigmask");

   switch (how) {
      case SIG_SETMASK: how = VKI_SIG_SETMASK; break;
      case SIG_BLOCK:   how = VKI_SIG_BLOCK; break;
      case SIG_UNBLOCK: how = VKI_SIG_UNBLOCK; break;
      default: pthread_error("pthread_sigmask: invalid how");
               return EINVAL;
   }

   /* Crude check */
   if (newmask == NULL)
      return EFAULT;

   VALGRIND_MAGIC_SEQUENCE(res, 0 /* default */,
                           VG_USERREQ__PTHREAD_SIGMASK,
                           how, newmask, oldmask, 0);

   /* The scheduler tells us of any memory violations. */
   return res == 0 ? 0 : EFAULT;
}


int sigwait ( const sigset_t* set, int* sig )
{
   int res;
   ensure_valgrind("sigwait");
   /* As with pthread_sigmask we deliberately confuse sigset_t with
      vki_ksigset_t. */
   VALGRIND_MAGIC_SEQUENCE(res, 0 /* default */,
                           VG_USERREQ__SIGWAIT,
                           set, sig, 0, 0);
   return res;
}


int pthread_kill(pthread_t thread, int signo)
{
   int res;
   ensure_valgrind("pthread_kill");
   VALGRIND_MAGIC_SEQUENCE(res, 0 /* default */,
                           VG_USERREQ__PTHREAD_KILL, 
                           thread, signo, 0, 0);
   return res;
}


/* Copied verbatim from Linuxthreads */
/* Redefine raise() to send signal to calling thread only,
   as per POSIX 1003.1c */
int raise (int sig)
{
  int retcode = pthread_kill(pthread_self(), sig);
  if (retcode == 0) {
    return 0;
  } else {
    errno = retcode;
    return -1;
  }
}


int pause ( void )
{
   unsigned int n_orig, n_now;
   struct vki_timespec nanosleep_interval;
   ensure_valgrind("pause");

   /* This is surely a cancellation point. */
   __my_pthread_testcancel();

   VALGRIND_MAGIC_SEQUENCE(n_orig, 0xFFFFFFFF /* default */,
                           VG_USERREQ__GET_N_SIGS_RETURNED, 
                           0, 0, 0, 0);
   my_assert(n_orig != 0xFFFFFFFF);

   while (1) {
      VALGRIND_MAGIC_SEQUENCE(n_now, 0xFFFFFFFF /* default */,
                              VG_USERREQ__GET_N_SIGS_RETURNED, 
                              0, 0, 0, 0);
      my_assert(n_now != 0xFFFFFFFF);
      my_assert(n_now >= n_orig);
      if (n_now != n_orig) break;

      nanosleep_interval.tv_sec  = 0;
      nanosleep_interval.tv_nsec = 52 * 1000 * 1000; /* 52 milliseconds */
      /* It's critical here that valgrind's nanosleep implementation
         is nonblocking. */
      (void)my_do_syscall2(__NR_nanosleep, 
                           (int)(&nanosleep_interval), (int)NULL);
   }

   * (__errno_location()) = EINTR;
   return -1;
}


/* ---------------------------------------------------
   THREAD-SPECIFICs
   ------------------------------------------------ */

int __pthread_key_create(pthread_key_t *key,  
                         void  (*destr_function)  (void *))
{
   int res;
   ensure_valgrind("pthread_key_create");
   VALGRIND_MAGIC_SEQUENCE(res, 0 /* default */,
                           VG_USERREQ__PTHREAD_KEY_CREATE,
                           key, destr_function, 0, 0);
   return res;
}

int pthread_key_delete(pthread_key_t key)
{
   static int moans = N_MOANS;
   if (moans-- > 0) 
      ignored("pthread_key_delete");
   return 0;
}

int __pthread_setspecific(pthread_key_t key, const void *pointer)
{
   int res;
   ensure_valgrind("pthread_setspecific");
   VALGRIND_MAGIC_SEQUENCE(res, 0 /* default */,
                           VG_USERREQ__PTHREAD_SETSPECIFIC,
                           key, pointer, 0, 0);
   return res;
}

void * __pthread_getspecific(pthread_key_t key)
{
   int res;
   ensure_valgrind("pthread_getspecific");
   VALGRIND_MAGIC_SEQUENCE(res, 0 /* default */,
                           VG_USERREQ__PTHREAD_GETSPECIFIC,
                           key, 0 , 0, 0);
   return (void*)res;
}


/* ---------------------------------------------------
   ONCEry
   ------------------------------------------------ */

static pthread_mutex_t once_masterlock = PTHREAD_MUTEX_INITIALIZER;


int __pthread_once ( pthread_once_t *once_control, 
                     void (*init_routine) (void) )
{
   int res;
   ensure_valgrind("pthread_once");

   res = __pthread_mutex_lock(&once_masterlock);

   if (res != 0) {
      barf("pthread_once: Looks like your program's "
           "init routine calls back to pthread_once() ?!");
   }

   if (*once_control == 0) {
      *once_control = 1;
      init_routine();
   }

   __pthread_mutex_unlock(&once_masterlock);

   return 0;
}


/* ---------------------------------------------------
   MISC
   ------------------------------------------------ */

int __pthread_atfork ( void (*prepare)(void),
                       void (*parent)(void),
                       void (*child)(void) )
{
   /* We have to do this properly or not at all; faking it isn't an
      option. */
   vgPlain_unimp("__pthread_atfork");
}


__attribute__((weak)) 
void __pthread_initialize ( void )
{
   ensure_valgrind("__pthread_initialize");
}


/* ---------------------------------------------------
   LIBRARY-PRIVATE THREAD SPECIFIC STATE
   ------------------------------------------------ */

#include <resolv.h>
static int thread_specific_errno[VG_N_THREADS];
static int thread_specific_h_errno[VG_N_THREADS];
static struct __res_state
           thread_specific_res_state[VG_N_THREADS];

int* __errno_location ( void )
{
   int tid;
   /* ensure_valgrind("__errno_location"); */
   VALGRIND_MAGIC_SEQUENCE(tid, 1 /* default */,
                           VG_USERREQ__PTHREAD_GET_THREADID,
                           0, 0, 0, 0);
   /* 'cos I'm paranoid ... */
   if (tid < 1 || tid >= VG_N_THREADS)
      barf("__errno_location: invalid ThreadId");
   return & thread_specific_errno[tid];
}

int* __h_errno_location ( void )
{
   int tid;
   /* ensure_valgrind("__h_errno_location"); */
   VALGRIND_MAGIC_SEQUENCE(tid, 1 /* default */,
                           VG_USERREQ__PTHREAD_GET_THREADID,
                           0, 0, 0, 0);
   /* 'cos I'm paranoid ... */
   if (tid < 1 || tid >= VG_N_THREADS)
      barf("__h_errno_location: invalid ThreadId");
   return & thread_specific_h_errno[tid];
}

struct __res_state* __res_state ( void )
{
   int tid;
   /* ensure_valgrind("__res_state"); */
   VALGRIND_MAGIC_SEQUENCE(tid, 1 /* default */,
                           VG_USERREQ__PTHREAD_GET_THREADID,
                           0, 0, 0, 0);
   /* 'cos I'm paranoid ... */
   if (tid < 1 || tid >= VG_N_THREADS)
      barf("__res_state: invalid ThreadId");
   return & thread_specific_res_state[tid];
}


/* ---------------------------------------------------
   LIBC-PRIVATE SPECIFIC DATA
   ------------------------------------------------ */

/* Relies on assumption that initial private data is NULL.  This
   should be fixed somehow. */

/* The allowable keys (indices) (all 2 of them). 
   From sysdeps/pthread/bits/libc-tsd.h
*/
#define N_LIBC_TSD_EXTRA_KEYS 1

enum __libc_tsd_key_t { _LIBC_TSD_KEY_MALLOC = 0,
                        _LIBC_TSD_KEY_DL_ERROR,
                        _LIBC_TSD_KEY_N };

/* Auto-initialising subsystem.  libc_specifics_inited is set 
   after initialisation.  libc_specifics_inited_mx guards it. */
static int             libc_specifics_inited    = 0;
static pthread_mutex_t libc_specifics_inited_mx = PTHREAD_MUTEX_INITIALIZER;

/* These are the keys we must initialise the first time. */
static pthread_key_t libc_specifics_keys[_LIBC_TSD_KEY_N
                                         + N_LIBC_TSD_EXTRA_KEYS];

/* Initialise the keys, if they are not already initialise. */
static
void init_libc_tsd_keys ( void )
{
   int res, i;
   pthread_key_t k;

   res = pthread_mutex_lock(&libc_specifics_inited_mx);
   if (res != 0) barf("init_libc_tsd_keys: lock");

   if (libc_specifics_inited == 0) {
      /* printf("INIT libc specifics\n"); */
      libc_specifics_inited = 1;
      for (i = 0; i < _LIBC_TSD_KEY_N + N_LIBC_TSD_EXTRA_KEYS; i++) {
         res = pthread_key_create(&k, NULL);
	 if (res != 0) barf("init_libc_tsd_keys: create");
         libc_specifics_keys[i] = k;
      }
   }

   res = pthread_mutex_unlock(&libc_specifics_inited_mx);
   if (res != 0) barf("init_libc_tsd_keys: unlock");
}


static int
libc_internal_tsd_set ( enum __libc_tsd_key_t key, 
                        const void * pointer )
{
   int        res;
   static int moans = N_MOANS;
   /* printf("SET SET SET key %d ptr %p\n", key, pointer); */
   if (key < _LIBC_TSD_KEY_MALLOC 
       || key >= _LIBC_TSD_KEY_N + N_LIBC_TSD_EXTRA_KEYS)
      barf("libc_internal_tsd_set: invalid key");
   if (key >= _LIBC_TSD_KEY_N && moans-- > 0)
      fprintf(stderr, 
         "valgrind's libpthread.so: libc_internal_tsd_set: "
         "dubious key %d\n", key);
   init_libc_tsd_keys();
   res = pthread_setspecific(libc_specifics_keys[key], pointer);
   if (res != 0) barf("libc_internal_tsd_set: setspecific failed");
   return 0;
}

static void *
libc_internal_tsd_get ( enum __libc_tsd_key_t key )
{
   void*      v;
   static int moans = N_MOANS;
   /* printf("GET GET GET key %d\n", key); */
   if (key < _LIBC_TSD_KEY_MALLOC 
       || key >= _LIBC_TSD_KEY_N + N_LIBC_TSD_EXTRA_KEYS)
      barf("libc_internal_tsd_get: invalid key");
   if (key >= _LIBC_TSD_KEY_N && moans-- > 0)
      fprintf(stderr, 
         "valgrind's libpthread.so: libc_internal_tsd_get: "
         "dubious key %d\n", key);
   init_libc_tsd_keys();
   v = pthread_getspecific(libc_specifics_keys[key]);
   /* if (v == NULL) barf("libc_internal_tsd_set: getspecific failed"); */
   return v;
}




int (*__libc_internal_tsd_set)
    (enum __libc_tsd_key_t key, const void * pointer)
   = libc_internal_tsd_set;

void* (*__libc_internal_tsd_get)
      (enum __libc_tsd_key_t key)
   = libc_internal_tsd_get;


/* ---------------------------------------------------------------------
   These are here (I think) because they are deemed cancellation
   points by POSIX.  For the moment we'll simply pass the call along
   to the corresponding thread-unaware (?) libc routine.
   ------------------------------------------------------------------ */

#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>

#ifdef GLIBC_2_1
extern
int __sigaction
             (int signum, 
              const struct sigaction *act,  
              struct  sigaction *oldact);
#else
extern
int __libc_sigaction
             (int signum, 
              const struct sigaction *act,  
              struct  sigaction *oldact);
#endif
int sigaction(int signum, 
              const struct sigaction *act,  
              struct  sigaction *oldact)
{
   __my_pthread_testcancel();
#  ifdef GLIBC_2_1
   return __sigaction(signum, act, oldact);
#  else
   return __libc_sigaction(signum, act, oldact);
#  endif
}


extern
int  __libc_connect(int  sockfd,  
                    const  struct  sockaddr  *serv_addr, 
                    socklen_t addrlen);
__attribute__((weak))
int  connect(int  sockfd,  
             const  struct  sockaddr  *serv_addr, 
             socklen_t addrlen)
{
   __my_pthread_testcancel();
   return __libc_connect(sockfd, serv_addr, addrlen);
}


extern
int __libc_fcntl(int fd, int cmd, long arg);
__attribute__((weak))
int fcntl(int fd, int cmd, long arg)
{
   __my_pthread_testcancel();
   return __libc_fcntl(fd, cmd, arg);
}


extern 
ssize_t __libc_write(int fd, const void *buf, size_t count);
__attribute__((weak))
ssize_t write(int fd, const void *buf, size_t count)
{
   __my_pthread_testcancel();
   return __libc_write(fd, buf, count);
}


extern 
ssize_t __libc_read(int fd, void *buf, size_t count);
__attribute__((weak))
ssize_t read(int fd, void *buf, size_t count)
{
   __my_pthread_testcancel();
   return __libc_read(fd, buf, count);
}

 
extern
int __libc_open64(const char *pathname, int flags, mode_t mode);
__attribute__((weak))
int open64(const char *pathname, int flags, mode_t mode)
{
   __my_pthread_testcancel();
   return __libc_open64(pathname, flags, mode);
}


extern
int __libc_open(const char *pathname, int flags, mode_t mode);
__attribute__((weak))
int open(const char *pathname, int flags, mode_t mode)
{
   __my_pthread_testcancel();
   return __libc_open(pathname, flags, mode);
}


extern
int __libc_close(int fd);
__attribute__((weak))
int close(int fd)
{
   __my_pthread_testcancel();
   return __libc_close(fd);
}


extern
int __libc_accept(int s, struct sockaddr *addr, socklen_t *addrlen);
__attribute__((weak))
int accept(int s, struct sockaddr *addr, socklen_t *addrlen)
{
   __my_pthread_testcancel();
   wait_for_fd_to_be_readable_or_erring(s);
   __my_pthread_testcancel();
   return __libc_accept(s, addr, addrlen);
}


extern
pid_t __libc_fork(void);
pid_t __fork(void)
{
   __my_pthread_testcancel();
   return __libc_fork();
}


extern
pid_t __libc_waitpid(pid_t pid, int *status, int options);
__attribute__((weak))
pid_t waitpid(pid_t pid, int *status, int options)
{
   __my_pthread_testcancel();
   return __libc_waitpid(pid, status, options);
}


extern
int __libc_nanosleep(const struct timespec *req, struct timespec *rem);
__attribute__((weak))
int nanosleep(const struct timespec *req, struct timespec *rem)
{
   __my_pthread_testcancel();
   return __libc_nanosleep(req, rem);
}


extern
int __libc_fsync(int fd);
__attribute__((weak))
int fsync(int fd)
{
   __my_pthread_testcancel();
   return __libc_fsync(fd);
}


extern
off_t __libc_lseek(int fildes, off_t offset, int whence);
__attribute__((weak))
off_t lseek(int fildes, off_t offset, int whence)
{
   __my_pthread_testcancel();
   return __libc_lseek(fildes, offset, whence);
}


extern
__off64_t __libc_lseek64(int fildes, __off64_t offset, int whence);
__attribute__((weak))
__off64_t lseek64(int fildes, __off64_t offset, int whence)
{
   __my_pthread_testcancel();
   return __libc_lseek64(fildes, offset, whence);
}


extern 
ssize_t __libc_pread64 (int __fd, void *__buf, size_t __nbytes,
                        __off64_t __offset);
ssize_t __pread64 (int __fd, void *__buf, size_t __nbytes,
                   __off64_t __offset)
{
   __my_pthread_testcancel();
   return __libc_pread64(__fd, __buf, __nbytes, __offset);
}


extern
ssize_t __libc_pwrite64 (int __fd, const void *__buf, size_t __nbytes,
                        __off64_t __offset);
ssize_t __pwrite64 (int __fd, const void *__buf, size_t __nbytes,
                   __off64_t __offset)
{
   __my_pthread_testcancel();
   return __libc_pwrite64(__fd, __buf, __nbytes, __offset);
}


extern 
ssize_t __libc_pwrite(int fd, const void *buf, size_t count, off_t offset);
__attribute__((weak))
ssize_t pwrite(int fd, const void *buf, size_t count, off_t offset)
{
   __my_pthread_testcancel();
   return __libc_pwrite(fd, buf, count, offset);
}


extern 
ssize_t __libc_pread(int fd, void *buf, size_t count, off_t offset);
__attribute__((weak))
ssize_t pread(int fd, void *buf, size_t count, off_t offset)
{
   __my_pthread_testcancel();
   return __libc_pread(fd, buf, count, offset);
}


extern  
void __libc_longjmp(jmp_buf env, int val) __attribute((noreturn));
/* not weak: __attribute__((weak)) */
void longjmp(jmp_buf env, int val)
{
   __libc_longjmp(env, val);
}


extern void __libc_siglongjmp (sigjmp_buf env, int val)
                               __attribute__ ((noreturn));
void siglongjmp(sigjmp_buf env, int val)
{
   kludged("siglongjmp (cleanup handlers are ignored)");
   __libc_siglongjmp(env, val);
}


extern
int __libc_send(int s, const void *msg, size_t len, int flags);
__attribute__((weak))
int send(int s, const void *msg, size_t len, int flags)
{
   __my_pthread_testcancel();
   return __libc_send(s, msg, len, flags);
}


extern
int __libc_recv(int s, void *buf, size_t len, int flags);
__attribute__((weak))
int recv(int s, void *buf, size_t len, int flags)
{
   __my_pthread_testcancel();
   wait_for_fd_to_be_readable_or_erring(s);
   __my_pthread_testcancel();
   return __libc_recv(s, buf, len, flags);
}


extern 
int __libc_sendmsg(int s, const struct msghdr *msg, int flags);
__attribute__((weak))
int sendmsg(int s, const struct msghdr *msg, int flags)
{
   __my_pthread_testcancel();
   return __libc_sendmsg(s, msg, flags);
}


extern
int __libc_recvmsg(int s, struct msghdr *msg, int flags);
__attribute__((weak))
int recvmsg(int s, struct msghdr *msg, int flags)
{
   __my_pthread_testcancel();
   return __libc_recvmsg(s, msg, flags);
}


extern
int __libc_recvfrom(int s, void *buf, size_t len, int flags,
                    struct sockaddr *from, socklen_t *fromlen);
__attribute__((weak))
int recvfrom(int s, void *buf, size_t len, int flags,
             struct sockaddr *from, socklen_t *fromlen)
{
   __my_pthread_testcancel();
   wait_for_fd_to_be_readable_or_erring(s);
   __my_pthread_testcancel();
   return __libc_recvfrom(s, buf, len, flags, from, fromlen);
}


extern
int __libc_sendto(int s, const void *msg, size_t len, int flags, 
                  const struct sockaddr *to, socklen_t tolen);
__attribute__((weak))
int sendto(int s, const void *msg, size_t len, int flags, 
           const struct sockaddr *to, socklen_t tolen)
{
   __my_pthread_testcancel();
   return __libc_sendto(s, msg, len, flags, to, tolen);
}


extern 
int __libc_system(const char* str);
__attribute__((weak))
int system(const char* str)
{
   __my_pthread_testcancel();
   return __libc_system(str);
}


extern
pid_t __libc_wait(int *status);
__attribute__((weak))
pid_t wait(int *status)
{
   __my_pthread_testcancel();
   return __libc_wait(status);
}


extern
int __libc_msync(const void *start, size_t length, int flags);
__attribute__((weak))
int msync(const void *start, size_t length, int flags)
{
   __my_pthread_testcancel();
   return __libc_msync(start, length, flags);
}


/* ---------------------------------------------------------------------
   Nonblocking implementations of select() and poll().  This stuff will
   surely rot your mind.
   ------------------------------------------------------------------ */

/*--------------------------------------------------*/

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


static
int my_do_syscall1 ( int syscallno, int arg1 )
{ 
   int __res;
   __asm__ volatile ("pushl %%ebx; movl %%edx,%%ebx ; int $0x80 ; popl %%ebx"
                     : "=a" (__res)
                     : "0" (syscallno),
                       "d" (arg1) );
   return __res;
}

static
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

static
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

static
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


/* This is a wrapper round select(), which makes it thread-safe,
   meaning that only this thread will block, rather than the entire
   process.  This wrapper in turn depends on nanosleep() not to block
   the entire process, but I think (hope? suspect?) that POSIX
   pthreads guarantees that to be the case.

   Basic idea is: modify the timeout parameter to select so that it
   returns immediately.  Poll like this until select returns non-zero,
   indicating something interesting happened, or until our time is up.
   Space out the polls with nanosleeps of say 20 milliseconds, which
   is required to be nonblocking; this allows other threads to run.  

   Assumes:
   * (checked via my_assert) types fd_set and vki_fd_set are identical.
   * (checked via my_assert) types timeval and vki_timeval are identical.
   * (unchecked) libc error numbers (EINTR etc) are the negation of the
     kernel's error numbers (VKI_EINTR etc).
*/

/* __attribute__((weak)) */
int select ( int n, 
             fd_set *rfds, 
             fd_set *wfds, 
             fd_set *xfds, 
             struct timeval *timeout )
{
   unsigned int ms_now, ms_end;
   int    res;
   fd_set rfds_copy;
   fd_set wfds_copy;
   fd_set xfds_copy;
   struct vki_timeval  t_now;
   struct vki_timeval  zero_timeout;
   struct vki_timespec nanosleep_interval;

   __my_pthread_testcancel();

   /* gcc's complains about ms_end being used uninitialised -- classic
      case it can't understand, where ms_end is both defined and used
      only if timeout != NULL.  Hence ... */
   ms_end = 0;

   /* We assume that the kernel and libc data layouts are identical
      for the following types.  These asserts provide a crude
      check. */
   if (sizeof(fd_set) != sizeof(vki_fd_set)
       || sizeof(struct timeval) != sizeof(struct vki_timeval))
      barf("valgrind's hacky non-blocking select(): data sizes error");

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
      my_assert (ms_end >= ms_now);
   }

   /* fprintf(stderr, "MY_SELECT: before loop\n"); */

   /* Either timeout == NULL, meaning wait indefinitely, or timeout !=
      NULL, in which case ms_end holds the end time. */
   while (1) {
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
      /* fprintf(stderr, "MY_SELECT: nanosleep\n"); */
      /* nanosleep and go round again */
      nanosleep_interval.tv_sec  = 0;
      nanosleep_interval.tv_nsec = 50 * 1000 * 1000; /* 50 milliseconds */
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
   }
}




#include <sys/poll.h>

#ifndef HAVE_NFDS_T
typedef unsigned long int nfds_t;
#endif


/* __attribute__((weak)) */
int poll (struct pollfd *__fds, nfds_t __nfds, int __timeout)
{
   unsigned int        ms_now, ms_end;
   int                 res, i;
   struct vki_timespec nanosleep_interval;

   __my_pthread_testcancel();
   ensure_valgrind("poll");

   /* Detect the current time and simultaneously find out if we are
      running on Valgrind. */
   VALGRIND_MAGIC_SEQUENCE(ms_now, 0xFFFFFFFF /* default */,
                           VG_USERREQ__READ_MILLISECOND_TIMER,
                           0, 0, 0, 0);

   if (/* CHECK SIZES FOR struct pollfd */
       sizeof(struct timeval) != sizeof(struct vki_timeval))
      barf("valgrind's hacky non-blocking poll(): data sizes error");

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
      /* fprintf(stderr, "MY_POLL: nanosleep\n"); */
      /* nanosleep and go round again */
      nanosleep_interval.tv_sec  = 0;
      nanosleep_interval.tv_nsec = 51 * 1000 * 1000; /* 51 milliseconds */
      /* It's critical here that valgrind's nanosleep implementation
         is nonblocking. */
      (void)my_do_syscall2(__NR_nanosleep, 
                           (int)(&nanosleep_interval), (int)NULL);
   }
}


/* Helper function used to make accept() non-blocking.  Idea is to use
   the above nonblocking poll() to make this thread ONLY wait for the
   specified fd to become ready, and then return. */

/* Sigh -- a hack.  We're not supposed to include this file directly;
   should do it via /usr/include/fcntl.h, but that introduces a
   varargs prototype for fcntl itself, which we can't mimic. */
#define _FCNTL_H
#include <bits/fcntl.h>

static void wait_for_fd_to_be_readable_or_erring ( int fd )
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


/* ---------------------------------------------------------------------
   Hacky implementation of semaphores.
   ------------------------------------------------------------------ */

#include <semaphore.h>

/* This is a terrible way to do the remapping.  Plan is to import an
   AVL tree at some point. */

typedef
   struct {
      pthread_mutex_t se_mx;
      pthread_cond_t se_cv;
      int count;
   }
   vg_sem_t;

static pthread_mutex_t se_remap_mx = PTHREAD_MUTEX_INITIALIZER;

static int      se_remap_used = 0;
static sem_t*   se_remap_orig[VG_N_SEMAPHORES];
static vg_sem_t se_remap_new[VG_N_SEMAPHORES];

static vg_sem_t* se_remap ( sem_t* orig )
{
   int res, i;
   res = __pthread_mutex_lock(&se_remap_mx);
   my_assert(res == 0);

   for (i = 0; i < se_remap_used; i++) {
      if (se_remap_orig[i] == orig)
         break;
   }
   if (i == se_remap_used) {
      if (se_remap_used == VG_N_SEMAPHORES) {
         res = pthread_mutex_unlock(&se_remap_mx);
         my_assert(res == 0);
         barf("VG_N_SEMAPHORES is too low.  Increase and recompile.");
      }
      se_remap_used++;
      se_remap_orig[i] = orig;
      /* printf("allocated semaphore %d\n", i); */
   }
   res = __pthread_mutex_unlock(&se_remap_mx);
   my_assert(res == 0);
   return &se_remap_new[i];
}


int sem_init(sem_t *sem, int pshared, unsigned int value)
{
   int       res;
   vg_sem_t* vg_sem;
   ensure_valgrind("sem_init");
   if (pshared != 0) {
      pthread_error("sem_init: unsupported pshared value");
      errno = ENOSYS;
      return -1;
   }
   vg_sem = se_remap(sem);
   res = pthread_mutex_init(&vg_sem->se_mx, NULL);
   my_assert(res == 0);
   res = pthread_cond_init(&vg_sem->se_cv, NULL);
   my_assert(res == 0);
   vg_sem->count = value;
   return 0;
}


int sem_wait ( sem_t* sem ) 
{
   int       res;
   vg_sem_t* vg_sem;
   ensure_valgrind("sem_wait");
   vg_sem = se_remap(sem);
   res = __pthread_mutex_lock(&vg_sem->se_mx);
   my_assert(res == 0);
   while (vg_sem->count == 0) {
      res = pthread_cond_wait(&vg_sem->se_cv, &vg_sem->se_mx);
      my_assert(res == 0);
   }
   vg_sem->count--;
   res = __pthread_mutex_unlock(&vg_sem->se_mx);
   my_assert(res == 0);
   return 0;
}

int sem_post ( sem_t* sem ) 
{
   int       res;
   vg_sem_t* vg_sem; 
   ensure_valgrind("sem_post");
   vg_sem = se_remap(sem);
   res = __pthread_mutex_lock(&vg_sem->se_mx);
   my_assert(res == 0);
   if (vg_sem->count == 0) {
      vg_sem->count++;
      res = pthread_cond_broadcast(&vg_sem->se_cv);
      my_assert(res == 0);
   } else {
      vg_sem->count++;
   }
   res = __pthread_mutex_unlock(&vg_sem->se_mx);
   my_assert(res == 0);
   return 0;
}


int sem_trywait ( sem_t* sem ) 
{
   int       ret, res;
   vg_sem_t* vg_sem; 
   ensure_valgrind("sem_trywait");
   vg_sem = se_remap(sem);
   res = __pthread_mutex_lock(&vg_sem->se_mx);
   my_assert(res == 0);
   if (vg_sem->count > 0) { 
      vg_sem->count--; 
      ret = 0; 
   } else { 
      ret = -1; 
      errno = EAGAIN; 
   }
   res = __pthread_mutex_unlock(&vg_sem->se_mx);
   my_assert(res == 0);
   return ret;
}


int sem_getvalue(sem_t* sem, int * sval)
{
   vg_sem_t* vg_sem; 
   ensure_valgrind("sem_trywait");
   vg_sem = se_remap(sem);
   *sval = vg_sem->count;
   return 0;
}


int sem_destroy(sem_t * sem)
{
   kludged("sem_destroy");
   /* if someone waiting on this semaphore, errno = EBUSY, return -1 */
   return 0;
}


/* ---------------------------------------------------------------------
   Reader-writer locks.
   ------------------------------------------------------------------ */

typedef 
   struct {
      int             initted;  /* != 0 --> in use; sanity check only */
      int             prefer_w; /* != 0 --> prefer writer */
      int             nwait_r;  /* # of waiting readers */
      int             nwait_w;  /* # of waiting writers */
      pthread_cond_t  cv_r;     /* for signalling readers */
      pthread_cond_t  cv_w;     /* for signalling writers */
      pthread_mutex_t mx;
      int             status;
      /* allowed range for status: >= -1.  -1 means 1 writer currently
         active, >= 0 means N readers currently active. */
   } 
   vg_rwlock_t;


static pthread_mutex_t rw_remap_mx = PTHREAD_MUTEX_INITIALIZER;

static int                 rw_remap_used = 0;
static pthread_rwlock_t*   rw_remap_orig[VG_N_RWLOCKS];
static vg_rwlock_t         rw_remap_new[VG_N_RWLOCKS];


static 
void init_vg_rwlock ( vg_rwlock_t* vg_rwl )
{
   int res = 0;
   vg_rwl->initted = 1;
   vg_rwl->prefer_w = 1;
   vg_rwl->nwait_r = 0;
   vg_rwl->nwait_w = 0;
   vg_rwl->status = 0;
   res = pthread_mutex_init(&vg_rwl->mx, NULL);
   res |= pthread_cond_init(&vg_rwl->cv_r, NULL);
   res |= pthread_cond_init(&vg_rwl->cv_w, NULL);
   my_assert(res == 0);
}


/* Take the address of a LinuxThreads rwlock_t and return the shadow
   address of our version.  Further, if the LinuxThreads version
   appears to have been statically initialised, do the same to the one
   we allocate here.  The pthread_rwlock_t.__rw_readers field is set
   to zero by PTHREAD_RWLOCK_INITIALIZER, so we take zero as meaning
   uninitialised and non-zero meaning initialised. 
*/
static vg_rwlock_t* rw_remap ( pthread_rwlock_t* orig )
{
   int          res, i;
   vg_rwlock_t* vg_rwl;
   res = __pthread_mutex_lock(&rw_remap_mx);
   my_assert(res == 0);

   for (i = 0; i < rw_remap_used; i++) {
      if (rw_remap_orig[i] == orig)
         break;
   }
   if (i == rw_remap_used) {
      if (rw_remap_used == VG_N_RWLOCKS) {
         res = __pthread_mutex_unlock(&rw_remap_mx);
         my_assert(res == 0);
         barf("VG_N_RWLOCKS is too low.  Increase and recompile.");
      }
      rw_remap_used++;
      rw_remap_orig[i] = orig;
      rw_remap_new[i].initted = 0;
      if (0) printf("allocated rwlock %d\n", i);
   }
   res = __pthread_mutex_unlock(&rw_remap_mx);
   my_assert(res == 0);
   vg_rwl = &rw_remap_new[i];

   /* Initialise the shadow, if required. */
   if (orig->__rw_readers == 0) {
      orig->__rw_readers = 1;
      init_vg_rwlock(vg_rwl);
      if (orig->__rw_kind == PTHREAD_RWLOCK_PREFER_READER_NP)
         vg_rwl->prefer_w = 0;
   }

   return vg_rwl;
}


int pthread_rwlock_init ( pthread_rwlock_t* orig,
                          const pthread_rwlockattr_t* attr )
{
   vg_rwlock_t* rwl;
   if (0) printf ("pthread_rwlock_init\n");
   /* Force the remapper to initialise the shadow. */
   orig->__rw_readers = 0;
   /* Install the lock preference; the remapper needs to know it. */
   orig->__rw_kind = PTHREAD_RWLOCK_DEFAULT_NP;
   if (attr)
      orig->__rw_kind = attr->__lockkind;
   rwl = rw_remap ( orig );
   return 0;
}


static 
void pthread_rwlock_rdlock_CANCEL_HDLR ( void* rwl_v )
{
   vg_rwlock_t* rwl = (vg_rwlock_t*)rwl_v;
   rwl->nwait_r--;
   pthread_mutex_unlock (&rwl->mx);
}


int pthread_rwlock_rdlock ( pthread_rwlock_t* orig )
{
   int res;
   vg_rwlock_t* rwl;
   if (0) printf ("pthread_rwlock_rdlock\n");
   rwl = rw_remap ( orig );
   res = __pthread_mutex_lock(&rwl->mx);
   my_assert(res == 0);
   if (!rwl->initted) {
      res = __pthread_mutex_unlock(&rwl->mx);
      my_assert(res == 0);
      return EINVAL;
   }
   if (rwl->status < 0) {
      my_assert(rwl->status == -1);
      rwl->nwait_r++;
      pthread_cleanup_push( pthread_rwlock_rdlock_CANCEL_HDLR, rwl );
      while (1) {
         if (rwl->status == 0) break;
         res = pthread_cond_wait(&rwl->cv_r, &rwl->mx);
         my_assert(res == 0);
      }
      pthread_cleanup_pop(0);
      rwl->nwait_r--;
   }
   my_assert(rwl->status >= 0);
   rwl->status++;
   res = __pthread_mutex_unlock(&rwl->mx);
   my_assert(res == 0);
   return 0;
}


int pthread_rwlock_tryrdlock ( pthread_rwlock_t* orig )
{
   int res;
   vg_rwlock_t* rwl;
   if (0) printf ("pthread_rwlock_tryrdlock\n");
   rwl = rw_remap ( orig );
   res = __pthread_mutex_lock(&rwl->mx);
   my_assert(res == 0);
   if (!rwl->initted) {
      res = __pthread_mutex_unlock(&rwl->mx);
      my_assert(res == 0);
      return EINVAL;
   }
   if (rwl->status == -1) {
      /* Writer active; we have to give up. */
      res = __pthread_mutex_unlock(&rwl->mx);
      my_assert(res == 0);
      return EBUSY;
   }
   /* Success */
   my_assert(rwl->status >= 0);
   rwl->status++;
   res = __pthread_mutex_unlock(&rwl->mx);
   my_assert(res == 0);
   return 0;
}


static 
void pthread_rwlock_wrlock_CANCEL_HDLR ( void* rwl_v )
{
   vg_rwlock_t* rwl = (vg_rwlock_t*)rwl_v;
   rwl->nwait_w--;
   pthread_mutex_unlock (&rwl->mx);
}


int pthread_rwlock_wrlock ( pthread_rwlock_t* orig )
{
   int res;
   vg_rwlock_t* rwl;
   if (0) printf ("pthread_rwlock_wrlock\n");
   rwl = rw_remap ( orig );
   res = __pthread_mutex_lock(&rwl->mx);
   my_assert(res == 0);
   if (!rwl->initted) {
      res = __pthread_mutex_unlock(&rwl->mx);
      my_assert(res == 0);
      return EINVAL;
   }
   if (rwl->status != 0) {
      rwl->nwait_w++;
      pthread_cleanup_push( pthread_rwlock_wrlock_CANCEL_HDLR, rwl );
      while (1) {
         if (rwl->status == 0) break;
         res = pthread_cond_wait(&rwl->cv_w, &rwl->mx);
         my_assert(res == 0);
      }
      pthread_cleanup_pop(0);
      rwl->nwait_w--;
   }
   my_assert(rwl->status == 0);
   rwl->status = -1;
   res = __pthread_mutex_unlock(&rwl->mx);
   my_assert(res == 0);
   return 0;
}


int pthread_rwlock_trywrlock ( pthread_rwlock_t* orig )
{
   int res;
   vg_rwlock_t* rwl;
   if (0) printf ("pthread_wrlock_trywrlock\n");
   rwl = rw_remap ( orig );
   res = __pthread_mutex_lock(&rwl->mx);
   my_assert(res == 0);
   if (!rwl->initted) {
      res = __pthread_mutex_unlock(&rwl->mx);
      my_assert(res == 0);
      return EINVAL;
   }
   if (rwl->status != 0) {
      /* Reader(s) or a writer active; we have to give up. */
      res = __pthread_mutex_unlock(&rwl->mx);
      my_assert(res == 0);
      return EBUSY;
   }
   /* Success */
   my_assert(rwl->status == 0);
   rwl->status = -1;
   res = __pthread_mutex_unlock(&rwl->mx);
   my_assert(res == 0);
   return 0;
}


int pthread_rwlock_unlock ( pthread_rwlock_t* orig )
{
   int res;
   vg_rwlock_t* rwl;
   if (0) printf ("pthread_rwlock_unlock\n");
   rwl = rw_remap ( orig );
   rwl = rw_remap ( orig );
   res = __pthread_mutex_lock(&rwl->mx);
   my_assert(res == 0);
   if (!rwl->initted) {
      res = __pthread_mutex_unlock(&rwl->mx);
      my_assert(res == 0);
      return EINVAL;
   }
   if (rwl->status == 0) {
      res = __pthread_mutex_unlock(&rwl->mx);
      my_assert(res == 0);
      return EPERM;
   }
   my_assert(rwl->status != 0);
   if (rwl->status == -1) {
     rwl->status = 0;
   } else {
     my_assert(rwl->status > 0);
     rwl->status--;
   }

   my_assert(rwl->status >= 0);

   if (rwl->prefer_w) {

      /* Favour waiting writers, if any. */
      if (rwl->nwait_w > 0) {
         /* Writer(s) are waiting. */
         if (rwl->status == 0) {
            /* We can let a writer in. */
            res = pthread_cond_signal(&rwl->cv_w);
            my_assert(res == 0);
         } else {
            /* There are still readers active.  Do nothing; eventually
               they will disappear, at which point a writer will be
               admitted. */
         }
      } 
      else
      /* No waiting writers. */
      if (rwl->nwait_r > 0) {
         /* Let in a waiting reader. */
         res = pthread_cond_signal(&rwl->cv_r);
         my_assert(res == 0);
      }

   } else {

      /* Favour waiting readers, if any. */
      if (rwl->nwait_r > 0) {
         /* Reader(s) are waiting; let one in. */
         res = pthread_cond_signal(&rwl->cv_r);
         my_assert(res == 0);
      } 
      else
      /* No waiting readers. */
      if (rwl->nwait_w > 0 && rwl->status == 0) {
         /* We have waiting writers and no active readers; let a
            writer in. */
         res = pthread_cond_signal(&rwl->cv_w);
         my_assert(res == 0);
      }
   }

   res = __pthread_mutex_unlock(&rwl->mx);
   my_assert(res == 0);
   return 0;   
}


int pthread_rwlock_destroy ( pthread_rwlock_t *orig )
{
   int res;
   vg_rwlock_t* rwl;
   if (0) printf ("pthread_rwlock_destroy\n");
   rwl = rw_remap ( orig );
   res = __pthread_mutex_lock(&rwl->mx);
   my_assert(res == 0);
   if (!rwl->initted) {
      res = __pthread_mutex_unlock(&rwl->mx);
      my_assert(res == 0);
      return EINVAL;
   }
   if (rwl->status != 0 || rwl->nwait_r > 0 || rwl->nwait_w > 0) {
      res = __pthread_mutex_unlock(&rwl->mx);
      my_assert(res == 0);
      return EBUSY;
   }
   rwl->initted = 0;
   res = __pthread_mutex_unlock(&rwl->mx);
   my_assert(res == 0);
   return 0;
}


/* Copied directly from LinuxThreads. */
int
pthread_rwlockattr_init (pthread_rwlockattr_t *attr)
{
  attr->__lockkind = 0;
  attr->__pshared = PTHREAD_PROCESS_PRIVATE;

  return 0;
}


/* ---------------------------------------------------------------------
   B'stard.
   ------------------------------------------------------------------ */

# define strong_alias(name, aliasname) \
  extern __typeof (name) aliasname __attribute__ ((alias (#name)));

# define weak_alias(name, aliasname) \
  extern __typeof (name) aliasname __attribute__ ((weak, alias (#name)));

strong_alias(__pthread_mutex_lock, pthread_mutex_lock)
strong_alias(__pthread_mutex_trylock, pthread_mutex_trylock)
strong_alias(__pthread_mutex_unlock, pthread_mutex_unlock)
strong_alias(__pthread_mutexattr_init, pthread_mutexattr_init)
  weak_alias(__pthread_mutexattr_settype, pthread_mutexattr_settype)
strong_alias(__pthread_mutex_init, pthread_mutex_init)
strong_alias(__pthread_mutexattr_destroy, pthread_mutexattr_destroy)
strong_alias(__pthread_mutex_destroy, pthread_mutex_destroy)
strong_alias(__pthread_once, pthread_once)
strong_alias(__pthread_atfork, pthread_atfork)
strong_alias(__pthread_key_create, pthread_key_create)
strong_alias(__pthread_getspecific, pthread_getspecific)
strong_alias(__pthread_setspecific, pthread_setspecific)

#ifndef GLIBC_2_1
strong_alias(sigaction, __sigaction)
#endif
     
strong_alias(close, __close)
strong_alias(fcntl, __fcntl)
strong_alias(lseek, __lseek)
strong_alias(open, __open)
strong_alias(open64, __open64)
strong_alias(read, __read)
strong_alias(wait, __wait)
strong_alias(write, __write)
strong_alias(connect, __connect)
strong_alias(send, __send)

weak_alias (__pread64, pread64)
weak_alias (__pwrite64, pwrite64)
weak_alias(__fork, fork)

weak_alias (__pthread_kill_other_threads_np, pthread_kill_other_threads_np)

/*--------------------------------------------------*/

weak_alias(pthread_rwlock_rdlock, __pthread_rwlock_rdlock)
weak_alias(pthread_rwlock_unlock, __pthread_rwlock_unlock)
weak_alias(pthread_rwlock_wrlock, __pthread_rwlock_wrlock)

weak_alias(pthread_rwlock_destroy, __pthread_rwlock_destroy)
weak_alias(pthread_rwlock_init, __pthread_rwlock_init)
weak_alias(pthread_rwlock_tryrdlock, __pthread_rwlock_tryrdlock)
weak_alias(pthread_rwlock_trywrlock, __pthread_rwlock_trywrlock)


/* I've no idea what these are, but they get called quite a lot.
   Anybody know? */

#undef _IO_flockfile
void _IO_flockfile ( _IO_FILE * file )
{
   pthread_mutex_lock(file->_lock);
}
weak_alias(_IO_flockfile, flockfile);


#undef _IO_funlockfile
void _IO_funlockfile ( _IO_FILE * file )
{
   pthread_mutex_unlock(file->_lock);
}
weak_alias(_IO_funlockfile, funlockfile);


/* This doesn't seem to be needed to simulate libpthread.so's external
   interface, but many people complain about its absence. */

strong_alias(__pthread_mutexattr_settype, __pthread_mutexattr_setkind_np)
weak_alias(__pthread_mutexattr_setkind_np, pthread_mutexattr_setkind_np)


/*--------------------------------------------------------------------*/
/*--- end                                          vg_libpthread.c ---*/
/*--------------------------------------------------------------------*/
