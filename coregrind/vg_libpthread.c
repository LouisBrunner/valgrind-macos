
/*--------------------------------------------------------------------*/
/*--- A replacement for the standard libpthread.so.                ---*/
/*---                                              vg_libpthread.c ---*/
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
#include "core.h"        /* For the VG_USERREQ__* constants */

#define __USE_UNIX98
#include <sys/types.h>
#include <pthread.h>
#undef __USE_UNIX98

#define __USE_GNU
#include <dlfcn.h>
#undef __USE_GNU

#include <unistd.h>
#include <string.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <sys/poll.h>
#include <stdio.h>
#include <errno.h>
#include <signal.h>

#include <stdlib.h>

# define strong_alias(name, aliasname) \
  extern __typeof (name) aliasname __attribute__ ((alias (#name)));

# define weak_alias(name, aliasname) \
  extern __typeof (name) aliasname __attribute__ ((weak, alias (#name)));


/* ---------------------------------------------------------------------
   Our own definition of types that vary between LinuxThreads and NPTL.
   ------------------------------------------------------------------ */

/* Moving from LinuxThreads to NPTL, several crucial types (eg.
   pthread_mutex_t, pthread_mutexattr_t, etc) in pthreadtypes.h were changed
   in binary-compatible, but source-incompatible, ways.  We can similarly
   use any layout we want, so long as it's binary-compatible.  However, we
   can no longer use the LinuxThreads types, because they won't work on NPTL
   systems.  Thus, we have to introduce a layer of indirection, and define
   our own versions of these types (vg_pthread_mutex_t, etc).  NPTL does
   pretty much the same thing, and it keeps many of its internal types
   secret.

   We can layout our types however we want, as long as we put the small
   number of fields in the right place for binary compatibility (eg.
   mutex->kind).  To make life easy, our versions have the exact same layout
   as the LinuxThreads ones;  only the type names and field names are
   different (they differ only by include "vg" at the start).

   In our implementation of the pthread operations (pthread_mutex_lock(),
   pthread_mutexattr_settype(), etc) we always cast the standard pthread
   types to our own types, (eg. pthread_mutex_t --> vg_pthread_mutex_t),
   before working with them.
    
   Note that we have various mutexes (and condvars) in this file that have the
   type pthread_mutex_t (and pthread_cond_t).  That is fine, because they
   are always only handled by calling the standard pthread functions (eg.
   pthread_mutex_lock()) on them.  Phew.

   WARNING: as a result of all this, we should *never* access these standard
   pthread types as is;  they *must* be converted to the vg_pthread_foo_t
   equivalent.   It would be nice if this was enforced...  (but compilation
   on NPTL-only systems should fail if this rule isn't followed...?)
*/

#include <sched.h>   // for 'struct __sched_param'

typedef struct __vg_pthread_attr_s
{
   int __vg_detachstate;
   int __vg_schedpolicy;
   struct __sched_param __vg_schedparam;
   int __vg_inheritsched;
   int __vg_scope;
   size_t __vg_guardsize;
   int __vg_stackaddr_set;
   void *__vg_stackaddr;
   size_t __vg_stacksize;
} vg_pthread_attr_t;

typedef struct
{
   int __vg_mutexkind;
} vg_pthread_mutexattr_t;

typedef struct
{
   int __vg_pshared;
} vg_pthread_condattr_t;

typedef struct _vg_pthread_rwlock_t
{
   struct _vg_pthread_fastlock __vg_rw_lock; /* Lock to guarantee mutual exclusion */
   int __vg_rw_readers;                   /* Number of readers */
   /*_pthread_descr*/ void* __vg_rw_writer;         /* Identity of writer, or NULL if none */
   /*_pthread_descr*/ void* __vg_rw_read_waiting;   /* Threads waiting for reading */
   /*_pthread_descr*/ void* __vg_rw_write_waiting;  /* Threads waiting for writing */
   int __vg_rw_kind;                      /* Reader/Writer preference selection */
   int __vg_rw_pshared;                   /* Shared between processes or not */
} vg_pthread_rwlock_t;

typedef struct
{
   int __vg_lockkind;
   int __vg_pshared;
} vg_pthread_rwlockattr_t;

/* Converting pthread types to vg_pthread types.  We always check that the
   passed-in type is as big as ours, for safety.  We also zero the pointer
   to the original struct, to ensure we don't accidentally use it again. */

#define CONVERT(foo, x, vg_x) \
   my_assert(sizeof(*x) >= sizeof(vg_pthread_##foo##_t)); \
   vg_x = (vg_pthread_##foo##_t*)x; \
   x = 0;  // ensure we don't accidentally use x again!


/* ---------------------------------------------------------------------
   Our own definition of types that only exist in NPTL.
   ------------------------------------------------------------------ */

#ifndef HAVE___PTHREAD_UNWIND_BUF_T

typedef struct
{
   struct
   {
      jmp_buf __cancel_jmp_buf;
      int __mask_was_saved;
   } __cancel_jmp_buf[1];
   void *__pad[4];
} __pthread_unwind_buf_t __attribute__ ((__aligned__));

#endif

/* ---------------------------------------------------------------------
   Forwardses.
   ------------------------------------------------------------------ */

#define WEAK	__attribute__((weak))

static
__inline__
int is_kerror ( int res )
{
   if (res >= -4095 && res <= -1)
      return 1;
   else
      return 0;
}


#ifdef GLIBC_2_3
   /* kludge by JRS (not from glibc) ... */
   typedef void* __locale_t;

   /* Copied from locale/locale.h in glibc-2.2.93 sources */
   /* This value can be passed to `uselocale' and may be returned by
      it.  Passing this value to any other function has undefined
      behavior.  */
#  define LC_GLOBAL_LOCALE       ((__locale_t) -1L)
   extern __locale_t __uselocale ( __locale_t );
#endif

static void
init_global_thread_specific_state ( void );

static void
init_thread_specific_state ( void );

static void
set_ret_val ( void* );
static void *
get_ret_val ( void );

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

/* Don't do anything if we're not under Valgrind */
/* Optimisation (?) 28 Nov 04: assume that once we establish
   yes/no, the situation does not change, and so only one 
   client request is ever needed.
*/
static __inline__
void ensure_valgrind ( char* caller )
{
   /* 0: unknown   1: running on V   2: not running on V */
   static Int status = 0; 

  again:
   /* common case */
   if (status == 1) 
      return;

   if (status == 2) {
      const char msg[] = "Error: this libpthread.so should "
                         "only be run with Valgrind\n";
      VG_(do_syscall3)(__NR_write, 2, (UWord)msg, sizeof(msg)-1);
      VG_(do_syscall1)(__NR_exit, 1);
   }
   
   status = (RUNNING_ON_VALGRIND) ? 1 : 2;
   goto again;
}

/* While we're at it ... hook our own startup function into this
   game. */

static
__attribute__((noreturn))
void barf ( const char* str )
{
   char buf[1000];
   strcpy(buf, "\nvalgrind's libpthread.so: ");
   strcat(buf, str);
   strcat(buf, "\nPlease report this bug at: ");
   strcat(buf, VG_BUGS_TO);
   strcat(buf, "\n\n");
   VALGRIND_INTERNAL_PRINTF(buf);
   _exit(1);
   /* We have to persuade gcc into believing this doesn't return. */
   while (1) { };
}


static void cat_n_send ( char* s1, char* s2, char* s3 )
{
   char  buf[1000];
   if (get_pt_trace_level() >= 0) {
      snprintf(buf, sizeof(buf), "%s%s%s", s1, s2, s3);
      buf[sizeof(buf)-1] = '\0';
      VALGRIND_INTERNAL_PRINTF(buf);
   }
}

static void oh_dear ( char* fn, char* aux, char* s )
{
   cat_n_send    ( "warning: Valgrind's ", fn, s );
   if (NULL != aux)
      cat_n_send ( "         ", aux, "" );
   cat_n_send    ( "         your program may misbehave as a result", "", "" );
}

static void ignored ( char* fn, char* aux )
{
   oh_dear ( fn, aux, " does nothing" );
}

static void kludged ( char* fn, char* aux )
{
   oh_dear ( fn, aux, " is incomplete" );
}


__attribute__((noreturn))
void vgPlain_unimp ( char* fn )
{
   cat_n_send ( "valgrind's libpthread.so: UNIMPLEMENTED FUNCTION: ", fn, "" );
   barf("unimplemented function");
}


void VG_(user_assert_fail) ( const Char* expr, const Char* file, Int line, const Char* fn )
{
   char buf[1000];
   static Bool entered = False;
   if (entered) 
      _exit(2);
   entered = True;
   sprintf(buf, "\n%s: %s:%d (%s): Assertion `%s' failed.\n",
                "valgrind", file, line, fn, expr );
   cat_n_send ( "", buf, "" );
   sprintf(buf, "Please report this bug at: %s\n\n", VG_BUGS_TO);
   cat_n_send ( "", buf, "" );
   _exit(1);
}

static
void my_free ( void* ptr )
{
#if 0
   int res;
   VALGRIND_MAGIC_SEQUENCE(res, (-1) /* default */,
                           VG_USERREQ__FREE, ptr, 0, 0, 0);
   my_assert(res == 0);
#else
   free(ptr);
#endif
}


static
void* my_malloc ( int nbytes )
{
   void* res;
#if 0
   VALGRIND_MAGIC_SEQUENCE(res, 0 /* default */,
                           VG_USERREQ__MALLOC, nbytes, 0, 0, 0);
#else
   res = malloc(nbytes);
#endif
   my_assert(res != (void*)0);
   return res;
}



/* ---------------------------------------------------------------------
   Pass pthread_ calls to Valgrind's request mechanism.
   ------------------------------------------------------------------ */


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
   Here so it can be inlined without complaint.
   ------------------------------------------------ */

__inline__
pthread_t pthread_self(void)
{
   int tid;
   ensure_valgrind("pthread_self");
   VALGRIND_MAGIC_SEQUENCE(tid, 0 /* default */,
                           VG_USERREQ__PTHREAD_GET_THREADID,
                           0, 0, 0, 0);
   if (tid < 1 || tid >= VG_N_THREADS)
      barf("pthread_self: invalid ThreadId");
   return tid;
}


/* ---------------------------------------------------
   THREAD ATTRIBUTES
   ------------------------------------------------ */

int pthread_attr_init(pthread_attr_t *attr)
{
   vg_pthread_attr_t* vg_attr;
   CONVERT(attr, attr, vg_attr);
   
   /* Just initialise the fields which we might look at. */
   vg_attr->__vg_detachstate = PTHREAD_CREATE_JOINABLE;
   /* Linuxthreads sets this field to the value __getpagesize(), so I
      guess the following is OK. */
   vg_attr->__vg_guardsize = VKI_PAGE_SIZE;
   /* No special stack yet. */
   vg_attr->__vg_stackaddr = 0;
   vg_attr->__vg_stacksize = VG_PTHREAD_STACK_SIZE;
   return 0;
}

int pthread_attr_setdetachstate(pthread_attr_t *attr, int detachstate)
{
   vg_pthread_attr_t* vg_attr;
   CONVERT(attr, attr, vg_attr);

   if (detachstate != PTHREAD_CREATE_JOINABLE 
       && detachstate != PTHREAD_CREATE_DETACHED) {
      pthread_error("pthread_attr_setdetachstate: "
                    "detachstate is invalid");
      return EINVAL;
   }
   vg_attr->__vg_detachstate = detachstate;
   return 0;
}

int pthread_attr_getdetachstate(const pthread_attr_t *attr, int *detachstate)
{
   vg_pthread_attr_t* vg_attr;
   CONVERT(attr, attr, vg_attr);
   *detachstate = vg_attr->__vg_detachstate;
   return 0;
}

int pthread_attr_getinheritsched(const pthread_attr_t *attr, int *inherit)
{
   static int moans = N_MOANS;
   if (moans-- > 0) 
      kludged("pthread_attr_getinheritsched", NULL);
   *inherit = PTHREAD_EXPLICIT_SCHED;
   return 0;
}

int pthread_attr_setinheritsched(pthread_attr_t *attr, int inherit)
{
   static int moans = N_MOANS;
   if (moans-- > 0) 
      ignored("pthread_attr_setinheritsched", NULL);
   return 0;
}

WEAK
int pthread_attr_setstacksize (pthread_attr_t *attr,
                               size_t stacksize)
{
   vg_pthread_attr_t* vg_attr;
   CONVERT(attr, attr, vg_attr);
   vg_attr->__vg_stacksize = stacksize;
   return 0;
}


/* This is completely bogus. */
int  pthread_attr_getschedparam(const  pthread_attr_t  *attr,  
                                struct sched_param *param)
{
   static int moans = N_MOANS;
   if (moans-- > 0) 
      kludged("pthread_attr_getschedparam", NULL);
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
      ignored("pthread_attr_setschedparam", "(scheduling not changeable)");
   return 0;
}

int pthread_attr_destroy(pthread_attr_t *attr)
{
   static int moans = N_MOANS;
   if (moans-- > 0) 
      ignored("pthread_attr_destroy", NULL);
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
   StackInfo si;
   int res;
   int    detached;
   vg_pthread_attr_t* vg_attr;
   CONVERT(attr, attr, vg_attr);

   ensure_valgrind("pthread_getattr_np");
   kludged("pthread_getattr_np", NULL);
   vg_attr->__vg_detachstate = PTHREAD_CREATE_JOINABLE;
   vg_attr->__vg_schedpolicy = SCHED_OTHER;
   vg_attr->__vg_schedparam.sched_priority = 0;
   vg_attr->__vg_inheritsched = PTHREAD_EXPLICIT_SCHED;
   vg_attr->__vg_scope = PTHREAD_SCOPE_SYSTEM;
   VALGRIND_MAGIC_SEQUENCE(res, (-1) /* default */,
                           VG_USERREQ__GET_STACK_INFO,
                           thread, &si, 0, 0 );
   vg_attr->__vg_guardsize = si.guardsize;
   vg_attr->__vg_stackaddr = (void *)si.base;
   vg_attr->__vg_stackaddr_set = 0;
   vg_attr->__vg_stacksize = si.size;
   VALGRIND_MAGIC_SEQUENCE(detached, (-1) /* default */,
                           VG_USERREQ__SET_OR_GET_DETACH, 
                           2 /* get */, thread, 0, 0);
   my_assert(detached == 0 || detached == 1);
   if (detached)
      vg_attr->__vg_detachstate = PTHREAD_CREATE_DETACHED;
   return 0;
}


WEAK
int pthread_attr_getstack ( const pthread_attr_t * attr,
                            void ** stackaddr,
                            size_t *stacksize )
{
   vg_pthread_attr_t* vg_attr;
   CONVERT(attr, attr, vg_attr);
   ensure_valgrind("pthread_attr_getstack");
   if (stackaddr)
      *stackaddr = vg_attr->__vg_stackaddr;
   if (stacksize)
      *stacksize = vg_attr->__vg_stacksize;
   return 0;
}

WEAK
int pthread_attr_getstackaddr ( const pthread_attr_t * attr,
                                void ** stackaddr )
{
   vg_pthread_attr_t* vg_attr;
   CONVERT(attr, attr, vg_attr);
   ensure_valgrind("pthread_attr_getstackaddr");
   if (stackaddr)
      *stackaddr = vg_attr->__vg_stackaddr;
   return 0;
}

WEAK
int pthread_attr_getstacksize ( const pthread_attr_t * attr, 
                                size_t * stacksize )
{
   vg_pthread_attr_t* vg_attr;
   CONVERT(attr, attr, vg_attr);
   ensure_valgrind("pthread_attr_getstacksize");
   if (stacksize)
      *stacksize = vg_attr->__vg_stacksize;
   return 0;
}

int pthread_attr_setschedpolicy(pthread_attr_t *attr, int policy)
{
   vg_pthread_attr_t* vg_attr;
   CONVERT(attr, attr, vg_attr);
   if (policy != SCHED_OTHER && policy != SCHED_FIFO && policy != SCHED_RR)
      return EINVAL;
   vg_attr->__vg_schedpolicy = policy;
   return 0;
}

int pthread_attr_getschedpolicy(const pthread_attr_t *attr, int *policy)
{
   vg_pthread_attr_t* vg_attr;
   CONVERT(attr, attr, vg_attr);
   *policy = vg_attr->__vg_schedpolicy;
   return 0;
}


WEAK 
int pthread_attr_setguardsize(pthread_attr_t *attr, size_t guardsize)
{
   vg_pthread_attr_t* vg_attr;
   CONVERT(attr, attr, vg_attr);
   vg_attr->__vg_guardsize = guardsize;
   return 0;
}

WEAK 
int pthread_attr_getguardsize(const pthread_attr_t *attr, size_t *guardsize)
{
   vg_pthread_attr_t* vg_attr;
   CONVERT(attr, attr, vg_attr);
   *guardsize = vg_attr->__vg_guardsize;
   return 0;
}  

/* Again, like LinuxThreads. */

static int concurrency_current_level = 0;

WEAK 
int pthread_setconcurrency(int new_level)
{
   if (new_level < 0)
      return EINVAL;
   else {
      concurrency_current_level = new_level;
      return 0;
   }
}

WEAK 
int pthread_getconcurrency(void)
{
   return concurrency_current_level;
}


/* All exiting threads eventually pass through here, bearing the
   return value, or PTHREAD_CANCELED, in ret_val. */
static
__attribute__((noreturn))
void thread_exit_wrapper ( void* ret_val )
{
   int           detached, res;
   CleanupEntry  cu;
   pthread_key_t key;
   void**        specifics_ptr;

   /* Run this thread's key finalizers.  Really this should be run
      PTHREAD_DESTRUCTOR_ITERATIONS times. */
   for (key = 0; key < VG_N_THREAD_KEYS; key++) {
      VALGRIND_MAGIC_SEQUENCE(res, (-2) /* default */,
                              VG_USERREQ__GET_KEY_D_AND_S,
                              key, &cu, 0, 0 );
      if (res == 0) {
         /* valid key */
         my_assert(cu.type == VgCt_Function);
         if (cu.data.function.fn && cu.data.function.arg)
            cu.data.function.fn /* destructor for key */ 
                  ( cu.data.function.arg /* specific for key for this thread */ );
         continue;
      }
      my_assert(res == -1);
   }

   /* Free up my specifics space, if any. */
   VALGRIND_MAGIC_SEQUENCE(specifics_ptr, 3 /* default */,
                           VG_USERREQ__PTHREAD_GETSPECIFIC_PTR,
                           pthread_self(), 0, 0, 0);
   my_assert(specifics_ptr != (void**)3);
   my_assert(specifics_ptr != (void**)1); /* 1 means invalid thread */
   if (specifics_ptr != NULL)
      my_free(specifics_ptr);
   
   VGA_(thread_exit)();
   
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
      int           attr__detachstate;
      void*         (*root_fn) ( void* );
      void*         arg;
      sigset_t	    sigmask;
      ThreadArchAux aux;
   }
   NewThreadInfo;

/* Struct used to describe a TDB header, copied from glibc. */
typedef
   struct {
      void *tcb;
      void *dtv;
      void *self;
      int multiple_threads;
      unsigned long sysinfo;
   }
   tcbhead_t;

/* This is passed to the VG_USERREQ__APPLY_IN_NEW_THREAD and so must
   not return.  Note that this runs in the new thread, not the
   parent. */
static
__attribute__((noreturn))
void thread_wrapper ( NewThreadInfo* info )
{
   int                    attr__detachstate;
   void*                  (*root_fn) ( void* );
   void*                  arg;
   void*                  ret_val;
   __pthread_unwind_buf_t ub;

   attr__detachstate = info->attr__detachstate;
   root_fn           = info->root_fn;
   arg               = info->arg;

   VGA_(thread_wrapper)(&info->aux);

   /* Minimally observe the attributes supplied. */
   if (attr__detachstate != PTHREAD_CREATE_DETACHED
       && attr__detachstate != PTHREAD_CREATE_JOINABLE)
      pthread_error("thread_wrapper: invalid attr->__detachstate");
   if (attr__detachstate == PTHREAD_CREATE_DETACHED)
      pthread_detach(pthread_self());

   /* Initialise thread specific state */
   init_thread_specific_state();

   /* Now that everything is set up, restore our signal mask (we're
      ready to accept signals) */
   sigprocmask(SIG_SETMASK, &info->sigmask, NULL);

   /* Free up the arg block that pthread_create malloced. */
   my_free(info);


   if (setjmp(ub.__cancel_jmp_buf[0].__cancel_jmp_buf) == 0) {
      CleanupEntry cu;
      int          res;
      
      cu.type = VgCt_Longjmp;
      cu.data.longjmp.ub = &ub;
      VALGRIND_MAGIC_SEQUENCE(res, (-1) /* default */,
                              VG_USERREQ__CLEANUP_PUSH,
                              &cu, 0, 0, 0);

      /* The root function might not return.  But if it does we simply
         move along to thread_exit_wrapper.  All other ways out for the
         thread (cancellation, or calling pthread_exit) lead there
         too. */
      ret_val = root_fn(arg);
   }
   else {
      ret_val = get_ret_val();
   }
   
   thread_exit_wrapper(ret_val);
   /* NOTREACHED */
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
   cu.type = VgCt_Function;
   cu.data.function.fn = __routine;
   cu.data.function.arg = __arg;
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
   cu.type = VgCt_None; /* paranoia */
   VALGRIND_MAGIC_SEQUENCE(res, (-1) /* default */,
                           VG_USERREQ__CLEANUP_POP,
                           &cu, 0, 0, 0);
   my_assert(cu.type == VgCt_Function);
   if (res == 0) {
      /* pop succeeded */
     if (__execute) {
        cu.data.function.fn ( cu.data.function.arg );
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


REGPARM(1)
void __pthread_register_cancel (__pthread_unwind_buf_t *__buf)
{
   int          res;
   CleanupEntry cu;
   ensure_valgrind("__pthread_register_cancel");
   cu.type = VgCt_Longjmp;
   cu.data.longjmp.ub = __buf;
   VALGRIND_MAGIC_SEQUENCE(res, (-1) /* default */,
                           VG_USERREQ__CLEANUP_PUSH,
                           &cu, 0, 0, 0);
   my_assert(res == 0);
}


REGPARM(1)
void __pthread_register_cancel_defer (__pthread_unwind_buf_t *__buf)
{
   /* As __pthread_register cancel, but save the thread's original
      cancellation type and set it to Deferred. */
   int          res;
   CleanupEntry cu;
   ensure_valgrind("__pthread_register_cancel_defer");
   cu.type = VgCt_Longjmp;
   cu.data.longjmp.ub = __buf;
   /* Set to Deferred, and save the old cancellation type. */
   my_assert(-1 != PTHREAD_CANCEL_DEFERRED);
   my_assert(-1 != PTHREAD_CANCEL_ASYNCHRONOUS);
   my_assert(sizeof(struct _pthread_cleanup_buffer) >= sizeof(int));
   VALGRIND_MAGIC_SEQUENCE(cu.data.longjmp.ctype, (-1) /* default */,
                           VG_USERREQ__SET_CANCELTYPE,
                           PTHREAD_CANCEL_DEFERRED, 0, 0, 0);   
   my_assert(cu.data.longjmp.ctype != -1);
   /* Now push the cleanup. */
   VALGRIND_MAGIC_SEQUENCE(res, (-1) /* default */,
                           VG_USERREQ__CLEANUP_PUSH,
                           &cu, 0, 0, 0);
   my_assert(res == 0);
}


REGPARM(1)
void __pthread_unregister_cancel (__pthread_unwind_buf_t *__buf)
{
   int          res;
   CleanupEntry cu;
   ensure_valgrind("__pthread_unregister_cancel");
   cu.type = VgCt_None; /* paranoia */
   VALGRIND_MAGIC_SEQUENCE(res, (-1) /* default */,
                           VG_USERREQ__CLEANUP_POP,
                           &cu, 0, 0, 0);
   my_assert(cu.type == VgCt_Longjmp);
   my_assert(cu.data.longjmp.ub == __buf);
   return;
}


REGPARM(1)
void __pthread_unregister_restore (__pthread_unwind_buf_t *__buf)
{
   int          res;
   CleanupEntry cu;
   int          fake_ctype;
   /* As __pthread_unregister_cancel, but after popping/running the
      handler, restore the thread's original cancellation type. */
   ensure_valgrind("__pthread_unregister_cancel_restore");
   cu.type = VgCt_None; /* paranoia */
   VALGRIND_MAGIC_SEQUENCE(res, (-1) /* default */,
                           VG_USERREQ__CLEANUP_POP,
                           &cu, 0, 0, 0);
   my_assert(cu.type == VgCt_Longjmp);
   my_assert(cu.data.longjmp.ub == __buf);
   /* Restore the original cancellation type. */
   my_assert(cu.data.longjmp.ctype == PTHREAD_CANCEL_DEFERRED
          || cu.data.longjmp.ctype == PTHREAD_CANCEL_ASYNCHRONOUS);
   my_assert(-1 != PTHREAD_CANCEL_DEFERRED);
   my_assert(-1 != PTHREAD_CANCEL_ASYNCHRONOUS);
   VALGRIND_MAGIC_SEQUENCE(fake_ctype, (-1) /* default */,
                           VG_USERREQ__SET_CANCELTYPE,
                           cu.data.longjmp.ctype, 0, 0, 0); 
   my_assert(fake_ctype == PTHREAD_CANCEL_DEFERRED);
   return;
}

REGPARM(1)
__attribute ((__noreturn__))
void __pthread_unwind (__pthread_unwind_buf_t *__buf)
{
   int           res;
   CleanupEntry  cu;
   while (1) {
      VALGRIND_MAGIC_SEQUENCE(res, (-1) /* default */,
                              VG_USERREQ__CLEANUP_POP,
                              &cu, 0, 0, 0);
      my_assert(res == 0);
      if (cu.type == VgCt_Longjmp) break;
      if (0) printf("running cleanup handler");
      my_assert(cu.type == VgCt_Function);
      cu.data.function.fn ( cu.data.function.arg );
   }
   my_assert(cu.type == VgCt_Longjmp);
   my_assert(__buf == NULL || __buf == cu.data.longjmp.ub);
   __buf = cu.data.longjmp.ub;
   longjmp(__buf->__cancel_jmp_buf[0].__cancel_jmp_buf, 1);
   /* NOTREACHED */
}


REGPARM(1)
__attribute ((__noreturn__))
void __pthread_unwind_next (__pthread_unwind_buf_t *__buf)
{
   __pthread_unwind(NULL);
   /* NOTREACHED */
}


/* ---------------------------------------------------
   THREADs
   ------------------------------------------------ */

static void __valgrind_pthread_yield ( void )
{
   int res;
   ensure_valgrind("pthread_yield");
   VALGRIND_MAGIC_SEQUENCE(res, 0 /* default */,
                           VG_USERREQ__PTHREAD_YIELD, 0, 0, 0, 0);
}

WEAK
int pthread_yield ( void )
{
   __valgrind_pthread_yield();
   return 0;
}


int pthread_equal(pthread_t thread1, pthread_t thread2)
{
   return thread1 == thread2 ? 1 : 0;
}


/* Bundle up the args into a malloc'd block and create a new thread
   consisting of thread_wrapper() applied to said malloc'd block. */
int
pthread_create (pthread_t *__restrict __thredd,
                __const pthread_attr_t *__restrict __attr,
                void *(*__start_routine) (void *),
                void *__restrict __arg)
{
   int            tid_child;
   NewThreadInfo* info;
   StackInfo      si;
   vg_pthread_attr_t* __vg_attr;
   CONVERT(attr, __attr, __vg_attr);

   ensure_valgrind("pthread_create");

   /* make sure the tsd keys, and hence locale info, for the root
      thread are initialised before we get into complications making
      new threads. */
   init_global_thread_specific_state();

   /* Allocate space for the arg block.  thread_wrapper will free
      it. */
   info = my_malloc(sizeof(NewThreadInfo));
   my_assert(info != NULL);

   if (__vg_attr)
      info->attr__detachstate = __vg_attr->__vg_detachstate;
   else 
      info->attr__detachstate = PTHREAD_CREATE_JOINABLE;

   VGA_(thread_create)(&info->aux);

   info->root_fn = __start_routine;
   info->arg     = __arg;
   sigprocmask(SIG_SETMASK, NULL, &info->sigmask);

   if (__attr) {
      si.base = (Addr)__vg_attr->__vg_stackaddr;
      si.size = __vg_attr->__vg_stacksize;
      si.guardsize = __vg_attr->__vg_guardsize;
   } else {
      si.base = (Addr)NULL;
      si.size = VG_PTHREAD_STACK_SIZE;
      si.guardsize = VKI_PAGE_SIZE;
   }
   
   VALGRIND_MAGIC_SEQUENCE(tid_child, VG_INVALID_THREADID /* default */,
                           VG_USERREQ__APPLY_IN_NEW_THREAD,
                           &thread_wrapper, info, &si, 0);
   my_assert(tid_child != VG_INVALID_THREADID);

   if (__thredd)
      *__thredd = tid_child;

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
   set_ret_val(retval);
   __pthread_unwind(NULL);
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
   MUTEX ATTRIBUTES
   ------------------------------------------------ */

int __pthread_mutexattr_init(pthread_mutexattr_t *attr)
{
   vg_pthread_mutexattr_t* vg_attr;
   CONVERT(mutexattr, attr, vg_attr);
   vg_attr->__vg_mutexkind = PTHREAD_MUTEX_ERRORCHECK_NP;
   return 0;
}

int __pthread_mutexattr_settype(pthread_mutexattr_t *attr, int type)
{
   vg_pthread_mutexattr_t* vg_attr;
   CONVERT(mutexattr, attr, vg_attr);

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
         vg_attr->__vg_mutexkind = type;
         return 0;
      default:
         pthread_error("pthread_mutexattr_settype: "
                       "invalid type");
         return EINVAL;
   }
}

int __pthread_mutexattr_gettype(const pthread_mutexattr_t *attr, int *type)
{
   vg_pthread_mutexattr_t* vg_attr;
   CONVERT(mutexattr, attr, vg_attr);

   *type = vg_attr->__vg_mutexkind;

   return 0;
}

int __pthread_mutexattr_destroy(pthread_mutexattr_t *attr)
{
   return 0;
}

int __pthread_mutexattr_setpshared ( pthread_mutexattr_t* attr, int pshared)
{
  if (pshared != PTHREAD_PROCESS_PRIVATE && pshared != PTHREAD_PROCESS_SHARED)
    return EINVAL;

  /* For now it is not possible to shared a conditional variable.  */
  if (pshared != PTHREAD_PROCESS_PRIVATE)
    return ENOSYS;

  return 0;
}


/* ---------------------------------------------------
   MUTEXes
   ------------------------------------------------ */

int __pthread_mutex_init(pthread_mutex_t *mutex, 
                         const  pthread_mutexattr_t *mutexattr)
{
   vg_pthread_mutex_t* vg_mutex;
   vg_pthread_mutexattr_t* vg_mutexattr;
   CONVERT(mutex, mutex, vg_mutex); 
   CONVERT(mutexattr, mutexattr, vg_mutexattr);
   
   vg_mutex->__vg_m_count = 0;
   vg_mutex->__vg_m_owner = (/*_pthread_descr*/void*)VG_INVALID_THREADID;
   vg_mutex->__vg_m_kind  = PTHREAD_MUTEX_ERRORCHECK_NP;
   if (vg_mutexattr)
      vg_mutex->__vg_m_kind = vg_mutexattr->__vg_mutexkind;
   return 0;
}


int __pthread_mutex_lock(pthread_mutex_t *mutex)
{
   int res;
   vg_pthread_mutex_t* vg_mutex;
   CONVERT(mutex, mutex, vg_mutex);
   
   VALGRIND_MAGIC_SEQUENCE(res, 0 /* default */,
                           VG_USERREQ__PTHREAD_MUTEX_LOCK,
                           vg_mutex, 0, 0, 0);
   return res;
}


int __pthread_mutex_timedlock(pthread_mutex_t *mutex,
                              const struct  timespec *abstime )
{
   int res;
   unsigned int ms_now, ms_end;
   struct  timeval timeval_now;
   unsigned long long int ull_ms_now_after_1970;
   unsigned long long int ull_ms_end_after_1970;
   unsigned long long int ull_ms_now;
   unsigned long long int ull_ms_end;
   vg_pthread_mutex_t* vg_mutex;
   CONVERT(mutex, mutex, vg_mutex);
   
   VALGRIND_MAGIC_SEQUENCE(ms_now, 0xFFFFFFFF /* default */,
                           VG_USERREQ__READ_MILLISECOND_TIMER,
                           0, 0, 0, 0);
   my_assert(ms_now != 0xFFFFFFFF);
   res = gettimeofday(&timeval_now, NULL);
   my_assert(res == 0);

   ull_ms_now_after_1970 
      = 1000ULL * ((unsigned long long int)(timeval_now.tv_sec))
        + ((unsigned long long int)(timeval_now.tv_usec / 1000));
   ull_ms_end_after_1970
      = 1000ULL * ((unsigned long long int)(abstime->tv_sec))
        + ((unsigned long long int)(abstime->tv_nsec / 1000000));
   if (ull_ms_end_after_1970 < ull_ms_now_after_1970)
      ull_ms_end_after_1970 = ull_ms_now_after_1970;
   ull_ms_now = ((unsigned long long int)(ms_now));
   ull_ms_end = ull_ms_now + (ull_ms_end_after_1970 - ull_ms_now_after_1970);
   if (ull_ms_end >= (unsigned long long int)(0xFFFFFFFFUL)) {
      /* use 0xFFFFFFFEUL because 0xFFFFFFFFUL is reserved for no timeout
         (the fine difference between a long wait and a possible abort
         due to a detected deadlock). 
      */
      ms_end = 0xFFFFFFFEUL; 
   } else {
      ms_end = (unsigned int)(ull_ms_end);
   }
   VALGRIND_MAGIC_SEQUENCE(res, 0 /* default */,
                           VG_USERREQ__PTHREAD_MUTEX_TIMEDLOCK,
                           vg_mutex, ms_end, 0, 0);
   return res;
}


int __pthread_mutex_trylock(pthread_mutex_t *mutex)
{
   int res;
   vg_pthread_mutex_t* vg_mutex;
   CONVERT(mutex, mutex, vg_mutex);
   
   VALGRIND_MAGIC_SEQUENCE(res, 0 /* default */,
                           VG_USERREQ__PTHREAD_MUTEX_TRYLOCK,
                           vg_mutex, 0, 0, 0);
   return res;
}


int __pthread_mutex_unlock(pthread_mutex_t *mutex)
{
   int res;
   vg_pthread_mutex_t* vg_mutex;
   CONVERT(mutex, mutex, vg_mutex);
   
   VALGRIND_MAGIC_SEQUENCE(res, 0 /* default */,
                           VG_USERREQ__PTHREAD_MUTEX_UNLOCK,
                           vg_mutex, 0, 0, 0);
   return res;
}


int __pthread_mutex_destroy(pthread_mutex_t *mutex)
{
   vg_pthread_mutex_t* vg_mutex;
   CONVERT(mutex, mutex, vg_mutex);

   /* Valgrind doesn't hold any resources on behalf of the mutex, so no
      need to involve it. */
   if (vg_mutex->__vg_m_count > 0) {
      /* Oh, the horror.  glibc's internal use of pthreads "knows"
	 that destroying a lock does an implicit unlock.  Make it
	 explicit. */
      __pthread_mutex_unlock( (pthread_mutex_t*)vg_mutex );
      pthread_error("pthread_mutex_destroy: mutex is still in use");
      return EBUSY;
   }
   vg_mutex->__vg_m_count = 0;
   vg_mutex->__vg_m_owner = (/*_pthread_descr*/void*)VG_INVALID_THREADID;
   vg_mutex->__vg_m_kind  = PTHREAD_MUTEX_ERRORCHECK_NP;
   return 0;
}


/* ---------------------------------------------------
   CONDITION VARIABLES
   ------------------------------------------------ */

/* LinuxThreads supports no attributes for conditions.  Hence ... */

int pthread_condattr_init(pthread_condattr_t *attr)
{
   vg_pthread_condattr_t* vg_attr;
   CONVERT(condattr, attr, vg_attr);

   vg_attr->__vg_pshared = 0;
   return 0;
}

int pthread_condattr_destroy(pthread_condattr_t *attr)
{
   return 0;
}
 
int pthread_condattr_setpshared(pthread_condattr_t *attr, int pshared)
{ 
   static int moans = N_MOANS;
   vg_pthread_condattr_t* vg_attr;
   CONVERT(condattr, attr, vg_attr);

   if (pshared != PTHREAD_PROCESS_PRIVATE && 
       pshared != PTHREAD_PROCESS_SHARED)
      return EINVAL;

   if (pshared == PTHREAD_PROCESS_SHARED && moans-- > 0) 
      kludged("pthread_setschedparam", "(process shared condition variables not supported)");

   vg_attr->__vg_pshared = pshared;
   return 0;
}

int pthread_condattr_getpshared (const pthread_condattr_t *attr, int *pshared)
{
   vg_pthread_condattr_t* vg_attr;
   CONVERT(condattr, attr, vg_attr);

   *pshared = vg_attr->__vg_pshared;
   return 0;
}

int pthread_cond_init( pthread_cond_t *cond,
		       const pthread_condattr_t *cond_attr)
{
   vg_pthread_cond_t* vg_cond;
   CONVERT(cond, cond, vg_cond);
   vg_cond->__vg_c_waiting = (/*_pthread_descr*/void*)VG_INVALID_THREADID;
   return 0;
}

int pthread_cond_destroy(pthread_cond_t *cond)
{
   /* should check that no threads are waiting on this CV */
   static int moans = N_MOANS;
   if (moans-- > 0) 
      kludged("pthread_cond_destroy", 
              "(it doesn't check if the cond is waited on)" );
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
      kludged("pthread_getschedparam", NULL);
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
      ignored("pthread_setschedparam", "(scheduling not changeable)");
   return 0;
}

int pthread_cond_wait(pthread_cond_t *cond, pthread_mutex_t *mutex)
{
   int res;
   vg_pthread_mutex_t* vg_mutex;
   CONVERT(mutex, mutex, vg_mutex);

   ensure_valgrind("pthread_cond_wait");
   VALGRIND_MAGIC_SEQUENCE(res, 0 /* default */,
                           VG_USERREQ__PTHREAD_COND_WAIT,
			   cond, vg_mutex, 0, 0);
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
   unsigned long long int ull_ms_now;
   unsigned long long int ull_ms_end;
   vg_pthread_mutex_t* vg_mutex;
   CONVERT(mutex, mutex, vg_mutex);

   ensure_valgrind("pthread_cond_timedwait");
   VALGRIND_MAGIC_SEQUENCE(ms_now, 0xFFFFFFFF /* default */,
                           VG_USERREQ__READ_MILLISECOND_TIMER,
                           0, 0, 0, 0);
   my_assert(ms_now != 0xFFFFFFFF);
   res = gettimeofday(&timeval_now, NULL);
   my_assert(res == 0);

   ull_ms_now_after_1970 
      = 1000ULL * ((unsigned long long int)(timeval_now.tv_sec))
        + ((unsigned long long int)(timeval_now.tv_usec / 1000));
   ull_ms_end_after_1970
      = 1000ULL * ((unsigned long long int)(abstime->tv_sec))
        + ((unsigned long long int)(abstime->tv_nsec / 1000000));
   if (ull_ms_end_after_1970 < ull_ms_now_after_1970)
      ull_ms_end_after_1970 = ull_ms_now_after_1970;
   ull_ms_now = ((unsigned long long int)(ms_now));
   ull_ms_end = ull_ms_now + (ull_ms_end_after_1970 - ull_ms_now_after_1970);
   if (ull_ms_end >= (unsigned long long int)(0xFFFFFFFFUL)) {
      /* use 0xFFFFFFFEUL because 0xFFFFFFFFUL is reserved for no timeout
         (the fine difference between a long wait and a possible abort
         due to a detected deadlock). 
      */
      ms_end = 0xFFFFFFFEUL; 
   } else {
      ms_end = (unsigned int)(ull_ms_end);
   }
   VALGRIND_MAGIC_SEQUENCE(res, 0 /* default */,
                           VG_USERREQ__PTHREAD_COND_TIMEDWAIT,
			   cond, vg_mutex, ms_end, 0);
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
                           thread, &pthread_exit, 0, 0);
   my_assert(res != -1);
   return res;
}

static
void __my_pthread_testcancel(void)
{
   int res;
   ensure_valgrind("__my_pthread_testcancel");
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

   VALGRIND_MAGIC_SEQUENCE(res, 0 /* default */,
                           VG_USERREQ__PTHREAD_SIGMASK,
                           how, newmask, oldmask, 0);

   /* The scheduler tells us of any memory violations. */
   return res == 0 ? 0 : EFAULT;
}

int sigwait ( const sigset_t* set, int* sig )
{
   int res;
   siginfo_t si;
   
   __my_pthread_testcancel();

   si.si_signo = 0;
   res = sigtimedwait(set, &si, NULL);
   *sig = si.si_signo;

   return 0;			/* always returns 0 */
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
    *(__errno_location()) = retcode;
    return -1;
  }
}



/* ---------------------------------------------------
   THREAD-SPECIFICs
   ------------------------------------------------ */

static
int key_is_valid (pthread_key_t key)
{
   int res;
   VALGRIND_MAGIC_SEQUENCE(res, 2 /* default */,
                           VG_USERREQ__PTHREAD_KEY_VALIDATE,
                           key, 0, 0, 0);
   my_assert(res != 2);
   return res;
}


/* Returns NULL if thread is invalid.  Otherwise, if the thread
   already has a specifics area, return that.  Otherwise allocate it
   one. */
static
void** get_or_allocate_specifics_ptr ( pthread_t thread )
{
   int    res, i;
   void** specifics_ptr;
   ensure_valgrind("get_or_allocate_specifics_ptr");

   /* Returns zero if the thread has no specific_ptr.  One if thread
      is invalid.  Otherwise, the specific_ptr value.  This is
      allocated with my_malloc and so is aligned and cannot be
      confused with 1 or 3. */
   VALGRIND_MAGIC_SEQUENCE(specifics_ptr, 3 /* default */,
                           VG_USERREQ__PTHREAD_GETSPECIFIC_PTR,
                           thread, 0, 0, 0);
   my_assert(specifics_ptr != (void**)3);

   if (specifics_ptr == (void**)1) 
      return NULL; /* invalid thread */

   if (specifics_ptr != NULL)
      return specifics_ptr; /* already has a specifics ptr. */

   /* None yet ... allocate a new one.  Should never fail. */
   specifics_ptr = my_malloc( VG_N_THREAD_KEYS * sizeof(void*) );
   my_assert(specifics_ptr != NULL);

   VALGRIND_MAGIC_SEQUENCE(res, -1 /* default */,
                           VG_USERREQ__PTHREAD_SETSPECIFIC_PTR,
                           specifics_ptr, 0, 0, 0);
   my_assert(res == 0);

   /* POSIX sez: "Upon thread creation, the value NULL shall be
      associated with all defined keys in the new thread."  This
      allocation is in effect a delayed allocation of the specific
      data for a thread, at its first-use.  Hence we initialise it
      here. */
   for (i = 0; i < VG_N_THREAD_KEYS; i++) {
      specifics_ptr[i] = NULL;
   }

   return specifics_ptr;   
}


int __pthread_key_create(pthread_key_t *key,  
                         void  (*destr_function)  (void *))
{
   void** specifics_ptr;
   int    res, i;
   ensure_valgrind("pthread_key_create");

   /* This writes *key if successful.  It should never fail. */
   VALGRIND_MAGIC_SEQUENCE(res, 1 /* default */,
                           VG_USERREQ__PTHREAD_KEY_CREATE,
                           key, destr_function, 0, 0);
   
   if (res == 0) {
      /* POSIX sez: "Upon key creation, the value NULL shall be
	 associated with the new key in all active threads." */
      for (i = 0; i < VG_N_THREADS; i++) {
	 specifics_ptr = get_or_allocate_specifics_ptr(i);
	 /* we get NULL if i is an invalid thread. */
	 if (specifics_ptr != NULL)
	    specifics_ptr[*key] = NULL;
      }
   }

   return res;
}

int pthread_key_delete(pthread_key_t key)
{
   int res;
   ensure_valgrind("pthread_key_delete");
   if (!key_is_valid(key))
      return EINVAL;
   VALGRIND_MAGIC_SEQUENCE(res, 0 /* default */,
                           VG_USERREQ__PTHREAD_KEY_DELETE,
                           key, 0, 0, 0);
   my_assert(res == 0);
   return 0;
}

int __pthread_setspecific(pthread_key_t key, const void *pointer)
{
   void** specifics_ptr;
   ensure_valgrind("pthread_setspecific");
   
   if (!key_is_valid(key))
      return EINVAL;

   specifics_ptr = get_or_allocate_specifics_ptr(pthread_self());
   specifics_ptr[key] = (void*)pointer;
   return 0;
}

void * __pthread_getspecific(pthread_key_t key)
{
   void** specifics_ptr;
   ensure_valgrind("pthread_getspecific");

   if (!key_is_valid(key))
      return NULL;

   specifics_ptr = get_or_allocate_specifics_ptr(pthread_self());
   return specifics_ptr[key];
}


/* ---------------------------------------------------
   ONCEry
   ------------------------------------------------ */

/* This protects reads and writes of the once_control variable
   supplied.  It is never held whilst any particular initialiser is
   running. */
static pthread_mutex_t once_masterlock = PTHREAD_MUTEX_INITIALIZER;

/* Initialiser needs to be run. */
#define P_ONCE_NOT_DONE  ((PTHREAD_ONCE_INIT) + 0)

/* Initialiser currently running. */
#define P_ONCE_RUNNING   ((PTHREAD_ONCE_INIT) + 1)

/* Initialiser has completed. */
#define P_ONCE_COMPLETED ((PTHREAD_ONCE_INIT) + 2)

int __pthread_once ( pthread_once_t *once_control, 
                     void (*init_routine) (void) )
{
   int res;
   int done;

#  define TAKE_LOCK                                   \
      res = __pthread_mutex_lock(&once_masterlock);   \
      my_assert(res == 0);

#  define RELEASE_LOCK                                \
      res = __pthread_mutex_unlock(&once_masterlock); \
      my_assert(res == 0);

   void cleanup(void *v) {
      TAKE_LOCK;
      *once_control = P_ONCE_NOT_DONE;
      RELEASE_LOCK;
   }

   ensure_valgrind("pthread_once");

   /* Grab the lock transiently, so we can safely see what state this
      once_control is in. */

   TAKE_LOCK;

   switch (*once_control) {

      case P_ONCE_NOT_DONE:
 	 /* Not started.  Change state to indicate running, drop the
	    lock and run.  */
         *once_control = P_ONCE_RUNNING;
	 _pthread_cleanup_push(NULL, cleanup, NULL);
	 RELEASE_LOCK;
         init_routine();
         /* re-take the lock, and set state to indicate done. */
	 TAKE_LOCK;
	 _pthread_cleanup_pop(NULL, False);
         *once_control = P_ONCE_COMPLETED;
	 RELEASE_LOCK;
	 break;

      case P_ONCE_RUNNING:
	 /* This is the tricky case.  The initialiser is running in
            some other thread, but we have to delay this thread till
            the other one completes.  So we sort-of busy wait.  In
            fact it makes sense to yield now, because what we want to
            happen is for the thread running the initialiser to
            complete ASAP. */
	 RELEASE_LOCK;
         done = 0;
         while (1) {
            /* Let others run for a while. */
	    __valgrind_pthread_yield();
	    /* Grab the lock and see if we're done waiting. */
	    TAKE_LOCK;
            if (*once_control == P_ONCE_COMPLETED)
               done = 1;
	    RELEASE_LOCK;
	    if (done)
               break;
	 }
	 break;

      case P_ONCE_COMPLETED:
      default: 
 	 /* Easy.  It's already done.  Just drop the lock. */
         RELEASE_LOCK;
	 break;
   }

   return 0;

#  undef TAKE_LOCK
#  undef RELEASE_LOCK
}

#undef P_ONCE_NOT_DONE
#undef P_ONCE_RUNNING
#undef P_ONCE_COMPLETED


/* ---------------------------------------------------
   MISC
   ------------------------------------------------ */

static pthread_mutex_t pthread_atfork_lock 
   = PTHREAD_MUTEX_INITIALIZER;

int __pthread_atfork ( void (*prepare)(void),
                       void (*parent)(void),
                       void (*child)(void) )
{
   int n, res;
   ForkHandlerEntry entry;

   ensure_valgrind("pthread_atfork");
   __pthread_mutex_lock(&pthread_atfork_lock);

   /* Fetch old counter */
   VALGRIND_MAGIC_SEQUENCE(n, -2 /* default */,
                           VG_USERREQ__GET_FHSTACK_USED,
                           0, 0, 0, 0);
   my_assert(n >= 0 && n < VG_N_FORKHANDLERSTACK);
   if (n == VG_N_FORKHANDLERSTACK-1)
      barf("pthread_atfork: VG_N_FORKHANDLERSTACK is too low; "
           "increase and recompile");

   /* Add entry */
   entry.prepare = *prepare;
   entry.parent  = *parent;
   entry.child   = *child;   
   VALGRIND_MAGIC_SEQUENCE(res, -2 /* default */,
                           VG_USERREQ__SET_FHSTACK_ENTRY,
                           n, &entry, 0, 0);
   my_assert(res == 0);

   /* Bump counter */
   VALGRIND_MAGIC_SEQUENCE(res, -2 /* default */,
                           VG_USERREQ__SET_FHSTACK_USED,
                           n+1, 0, 0, 0);
   my_assert(res == 0);

   __pthread_mutex_unlock(&pthread_atfork_lock);
   return 0;
}


#ifdef GLIBC_2_3
/* This seems to be a hook which appeared in glibc-2.3.2. */
int __register_atfork ( void (*prepare)(void),
                        void (*parent)(void),
                        void (*child)(void) )
{
   return __pthread_atfork(prepare,parent,child);
}
#endif

WEAK 
void __pthread_initialize ( void )
{
   ensure_valgrind("__pthread_initialize");
}


/* ---------------------------------------------------
   LIBRARY-PRIVATE THREAD SPECIFIC STATE
   ------------------------------------------------ */

#include <resolv.h>

/* The allowable libc TSD keys (indices) from glibc source. */
enum __libc_tsd_key_t { _LIBC_TSD_KEY_MALLOC = 0,
                        _LIBC_TSD_KEY_DL_ERROR,
                        _LIBC_TSD_KEY_RPC_VARS,
                        _LIBC_TSD_KEY_LOCALE,
                        _LIBC_TSD_KEY_CTYPE_B,
                        _LIBC_TSD_KEY_CTYPE_TOLOWER,
                        _LIBC_TSD_KEY_CTYPE_TOUPPER,
                        _LIBC_TSD_KEY_N };

typedef
   struct {
     void               *ret_val;
     int                *errno_ptr;
     int                *h_errno_ptr;
     struct __res_state *res_state_ptr;
     int                errno_data;
     int                h_errno_data;
     struct __res_state res_state_data;
     void               *libc_specifics[_LIBC_TSD_KEY_N];
   }
   ThreadSpecificState;

static ThreadSpecificState thread_specific_state[VG_N_THREADS];

/* Auto-initialising subsystem.  global_init_done is set 
   after initialisation.  global_init_done_mx guards it. */
static int             global_init_done    = 0;
static pthread_mutex_t global_init_done_mx = PTHREAD_MUTEX_INITIALIZER;

static
void cleanup_root(void *arg)
{
   thread_exit_wrapper(get_ret_val());
   /* NOTREACHED */
}

static void __attribute__((constructor))
init_global_thread_specific_state ( void )
{
   int res;

   /* Don't fall into deadlock if we get called again whilst we still
      hold the lock, via the __uselocale() call herein. */
   if (global_init_done != 0)
      return;

   /* Take the lock. */
   res = __pthread_mutex_lock(&global_init_done_mx);
   if (res != 0) barf("init_global_thread_specific_state: lock");

   /* Now test again, to be sure there is no mistake. */
   if (global_init_done != 0) {
      res = __pthread_mutex_unlock(&global_init_done_mx);
      if (res != 0) barf("init_global_thread_specific_state: unlock(1)");
      return;
   }
   
   /* assert that we are the root thread. */
   my_assert(pthread_self() == 1);

   /* Signify init done - we shouldn't really do this until after
      the call to init_thread_specific_state() but that routine makes
      a call to __uselocale() that may bring us back here as that
      routine will call __libc_tsd_set() which will call us.

      We can get away with marking the init as done now because
      the important bits of init_thread_specific_state() are done
      before the call to __uselocale() is made. */
   global_init_done = 1;

   /* Initialise thread specific data for the root thread. */
   init_thread_specific_state();

   /* Install a cleanup routine to handle the root thread exiting */
   _pthread_cleanup_push(NULL, cleanup_root, NULL);

   /* Unlock and return. */
   res = __pthread_mutex_unlock(&global_init_done_mx);
   if (res != 0) barf("init_global_thread_specific_state: unlock");
}

static void
init_thread_specific_state ( void )
{
   int tid = pthread_self();
   int i;

   /* No return value yet */
   thread_specific_state[tid].ret_val = NULL;

   /* Initialise the errno and resolver state pointers. */
   thread_specific_state[tid].errno_ptr = NULL;
   thread_specific_state[tid].h_errno_ptr = NULL;
   thread_specific_state[tid].res_state_ptr = NULL;

   /* Initialise the per-thread libc data. */
   for (i = 0; i < _LIBC_TSD_KEY_N; i++) {
      thread_specific_state[tid].libc_specifics[i] = NULL;
   }

#  ifdef GLIBC_2_3
   /* Set this thread's locale to the global (default) locale.  A hack
      in support of glibc-2.3.
   */
   __uselocale(LC_GLOBAL_LOCALE);
#  endif
}

static void
set_ret_val ( void* ret_val )
{
   int tid;
   VALGRIND_MAGIC_SEQUENCE(tid, 1 /* default */,
                           VG_USERREQ__PTHREAD_GET_THREADID,
                           0, 0, 0, 0);
   thread_specific_state[tid].ret_val = ret_val;
}

static void *
get_ret_val ( void )
{
   int tid;
   VALGRIND_MAGIC_SEQUENCE(tid, 1 /* default */,
                           VG_USERREQ__PTHREAD_GET_THREADID,
                           0, 0, 0, 0);
   return thread_specific_state[tid].ret_val;
}

int* __errno_location ( void )
{
   int tid;

   ensure_valgrind("__errno_location");
   VALGRIND_MAGIC_SEQUENCE(tid, 1 /* default */,
                           VG_USERREQ__PTHREAD_GET_THREADID,
                           0, 0, 0, 0);
   /* 'cos I'm paranoid ... */
   if (tid < 1 || tid >= VG_N_THREADS)
      barf("__errno_location: invalid ThreadId");
   if (thread_specific_state[tid].errno_ptr == NULL) {
      if (VGA_(has_tls)())
         thread_specific_state[tid].errno_ptr = dlsym(RTLD_DEFAULT, "errno");
      else if (tid == 1)
         thread_specific_state[tid].errno_ptr = dlvsym(RTLD_DEFAULT, "errno", "GLIBC_2.0");
      else
         thread_specific_state[tid].errno_ptr = &thread_specific_state[tid].errno_data;
   }
   return thread_specific_state[tid].errno_ptr;
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
   if (thread_specific_state[tid].h_errno_ptr == NULL) {
      if (VGA_(has_tls)())
         thread_specific_state[tid].h_errno_ptr = dlsym(RTLD_DEFAULT, "h_errno");
      else if (tid == 1)
         thread_specific_state[tid].h_errno_ptr = dlvsym(RTLD_DEFAULT, "h_errno", "GLIBC_2.0");
      else
         thread_specific_state[tid].h_errno_ptr = &thread_specific_state[tid].h_errno_data;
   }
   return thread_specific_state[tid].h_errno_ptr;
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
   if (thread_specific_state[tid].res_state_ptr == NULL) {
      if (VGA_(has_tls)()) {
         struct __res_state **resp = dlsym(RTLD_DEFAULT, "__resp");
         
         thread_specific_state[tid].res_state_ptr = *resp;
      } else if (tid == 1) {
         thread_specific_state[tid].res_state_ptr = dlvsym(RTLD_DEFAULT, "_res", "GLIBC_2.0");
      } else {
         thread_specific_state[tid].res_state_ptr = &thread_specific_state[tid].res_state_data;
      }
   }
   return thread_specific_state[tid].res_state_ptr;
}


/* ---------------------------------------------------
   LIBC-PRIVATE SPECIFIC DATA
   ------------------------------------------------ */

static int
libc_internal_tsd_set ( enum __libc_tsd_key_t key, 
                        const void * pointer )
{
   int tid = pthread_self();
   /* printf("SET SET SET key %d ptr %p\n", key, pointer); */
   if (key < _LIBC_TSD_KEY_MALLOC || key >= _LIBC_TSD_KEY_N)
      barf("libc_internal_tsd_set: invalid key");
   init_global_thread_specific_state();
   thread_specific_state[tid].libc_specifics[key] = (void *)pointer;
   return 0;
}

static void *
libc_internal_tsd_get ( enum __libc_tsd_key_t key )
{
   int tid = pthread_self();
   /* printf("GET GET GET key %d\n", key); */
   if (key < _LIBC_TSD_KEY_MALLOC || key >= _LIBC_TSD_KEY_N)
      barf("libc_internal_tsd_get: invalid key");
   init_global_thread_specific_state();
   return thread_specific_state[tid].libc_specifics[key];
}


int (*__libc_internal_tsd_set)
    (enum __libc_tsd_key_t key, const void * pointer)
   = libc_internal_tsd_set;

void* (*__libc_internal_tsd_get)
      (enum __libc_tsd_key_t key)
   = libc_internal_tsd_get;


#ifdef GLIBC_2_3
/* This one was first spotted be me in the glibc-2.2.93 sources. */
static void**
libc_internal_tsd_address ( enum __libc_tsd_key_t key )
{
   int tid = pthread_self();
   /* printf("ADDR ADDR ADDR key %d\n", key); */
   if (key < _LIBC_TSD_KEY_MALLOC || key >= _LIBC_TSD_KEY_N)
      barf("libc_internal_tsd_address: invalid key");
   init_global_thread_specific_state();
   return &thread_specific_state[tid].libc_specifics[key];
}

void ** (*__libc_internal_tsd_address) 
        (enum __libc_tsd_key_t key)
   = libc_internal_tsd_address;
#endif


/* ---------------------------------------------------------------------
   These are here (I think) because they are deemed cancellation
   points by POSIX.  For the moment we'll simply pass the call along
   to the corresponding thread-unaware (?) libc routine.
   ------------------------------------------------------------------ */

static void *libpthread_handle;

#define FORWARD(name, altname, args...) \
  ({ \
    static name##_t name##_ptr = NULL; \
    if (libpthread_handle == NULL) { \
      libpthread_handle = dlopen("libpthread.so.0", RTLD_LAZY); \
      my_assert(libpthread_handle != NULL); \
    } \
    if (name##_ptr == NULL) { \
      if ((name##_ptr = (name##_t)dlsym(RTLD_NEXT, #name)) == NULL) \
        name##_ptr = (name##_t)dlsym(RTLD_DEFAULT, #altname); \
      my_assert(name##_ptr != NULL && name##_ptr != dlsym(libpthread_handle, #name)); \
    } \
    name##_ptr(args); \
  })

typedef
int (*sigaction_t)
             (int signum, 
              const struct sigaction *act,  
              struct  sigaction *oldact);
int sigaction(int signum, 
              const struct sigaction *act,  
              struct  sigaction *oldact)
{
   __my_pthread_testcancel();
#ifdef GLIBC_2_1
   return FORWARD(sigaction, __sigaction, signum, act, oldact);
#else
   return FORWARD(sigaction, __libc_sigaction, signum, act, oldact);
#endif
}

typedef 
int (*accept_t)(int fd, struct sockaddr *addr, socklen_t *len);

WEAK
int accept(int fd, struct sockaddr *addr, socklen_t *len)
{
   __my_pthread_testcancel();
   return FORWARD(accept, __libc_accept, fd, addr, len);
}

typedef
int (*connect_t)(int sockfd,  
                 const struct sockaddr *serv_addr, 
                 socklen_t addrlen);
WEAK
int connect(int sockfd,  
            const struct sockaddr *serv_addr, 
            socklen_t addrlen)
{
   __my_pthread_testcancel();
   return FORWARD(connect, __libc_connect, sockfd, serv_addr, addrlen);
}


typedef
int (*fcntl_t)(int fd, int cmd, long arg);
WEAK
int fcntl(int fd, int cmd, long arg)
{
   __my_pthread_testcancel();
   return FORWARD(fcntl, __libc_fcntl, fd, cmd, arg);
}


typedef 
ssize_t (*write_t)(int fd, const void *buf, size_t count);
WEAK
ssize_t write(int fd, const void *buf, size_t count)
{
   __my_pthread_testcancel();
   return FORWARD(write, __libc_write, fd, buf, count);
}


typedef
ssize_t (*read_t)(int fd, void *buf, size_t count);
WEAK
ssize_t read(int fd, void *buf, size_t count)
{
   __my_pthread_testcancel();
   return FORWARD(read, __libc_read, fd, buf, count);
}

typedef
int (*open64_t)(const char *pathname, int flags, mode_t mode);
/* WEAK */
int open64(const char *pathname, int flags, mode_t mode)
{
   return FORWARD(open64, __libc_open64, pathname, flags, mode);
}

typedef
int (*open_t)(const char *pathname, int flags, mode_t mode);
/* WEAK */
int open(const char *pathname, int flags, mode_t mode)
{
   return FORWARD(open, __libc_open, pathname, flags, mode);
}

typedef
int (*close_t)(int fd);
WEAK
int close(int fd)
{
   __my_pthread_testcancel();
   return FORWARD(close, __libc_close, fd);
}


typedef
pid_t (*waitpid_t)(pid_t pid, int *status, int options);
WEAK
pid_t waitpid(pid_t pid, int *status, int options)
{
   __my_pthread_testcancel();
   return FORWARD(waitpid, __libc_waitpid, pid, status, options);
}


typedef
int (*__nanosleep_t)(const struct timespec *req, struct timespec *rem);
WEAK
int __nanosleep(const struct timespec *req, struct timespec *rem)
{
   __my_pthread_testcancel();
   return FORWARD(__nanosleep, __libc_nanosleep, req, rem);
}

typedef
int (*pause_t)(void);
WEAK
int pause(void)
{
   __my_pthread_testcancel();
   return FORWARD(pause, __libc_pause);
}


typedef
int (*__tcdrain_t)(int fd);
WEAK
int __tcdrain(int fd)
{
   __my_pthread_testcancel();
   return FORWARD(__tcdrain, __libc_tcdrain, fd);
}


typedef
int (*fsync_t)(int fd);
WEAK
int fsync(int fd)
{
   __my_pthread_testcancel();
   return FORWARD(fsync, __libc_fsync, fd);
}


typedef
off_t (*lseek_t)(int fildes, off_t offset, int whence);
WEAK
off_t lseek(int fildes, off_t offset, int whence)
{
   __my_pthread_testcancel();
   return FORWARD(lseek, __libc_lseek, fildes, offset, whence);
}


typedef
__off64_t (*lseek64_t)(int fildes, __off64_t offset, int whence);
WEAK
__off64_t lseek64(int fildes, __off64_t offset, int whence)
{
   __my_pthread_testcancel();
   return FORWARD(lseek64, __libc_lseek64, fildes, offset, whence);
}


typedef 
ssize_t (*__pread64_t) (int __fd, void *__buf, size_t __nbytes,
                        __off64_t __offset);
ssize_t __pread64 (int __fd, void *__buf, size_t __nbytes,
                   __off64_t __offset)
{
   __my_pthread_testcancel();
   return FORWARD(__pread64, __libc_pread64, __fd, __buf, __nbytes, __offset);
}


typedef
ssize_t (*__pwrite64_t) (int __fd, const void *__buf, size_t __nbytes,
                         __off64_t __offset);
ssize_t __pwrite64 (int __fd, const void *__buf, size_t __nbytes,
                   __off64_t __offset)
{
   __my_pthread_testcancel();
   return FORWARD(__pwrite64, __libc_pwrite64, __fd, __buf, __nbytes, __offset);
}


typedef 
ssize_t (*pwrite_t)(int fd, const void *buf, size_t count, off_t offset);
WEAK
ssize_t pwrite(int fd, const void *buf, size_t count, off_t offset)
{
   __my_pthread_testcancel();
   return FORWARD(pwrite, __libc_pwrite, fd, buf, count, offset);
}


typedef 
ssize_t (*pread_t)(int fd, void *buf, size_t count, off_t offset);
WEAK
ssize_t pread(int fd, void *buf, size_t count, off_t offset)
{
   __my_pthread_testcancel();
   return FORWARD(pread, __libc_pread, fd, buf, count, offset);
}

typedef
ssize_t (*recv_t)(int s, void *msg, size_t len, int flags);
WEAK
ssize_t recv(int s, void *msg, size_t len, int flags)
{
   __my_pthread_testcancel();
   return FORWARD(recv, __libc_recv, s, msg, len, flags);
}

typedef
ssize_t (*send_t)(int s, const void *msg, size_t len, int flags);
WEAK
ssize_t send(int s, const void *msg, size_t len, int flags)
{
   __my_pthread_testcancel();
   return FORWARD(send, __libc_send, s, msg, len, flags);
}


typedef 
ssize_t (*sendmsg_t)(int s, const struct msghdr *msg, int flags);
WEAK
ssize_t sendmsg(int s, const struct msghdr *msg, int flags)
{
   __my_pthread_testcancel();
   return FORWARD(sendmsg, __libc_sendmsg, s, msg, flags);
}


typedef
ssize_t (*recvmsg_t)(int s, struct msghdr *msg, int flags);
WEAK
ssize_t recvmsg(int s, struct msghdr *msg, int flags)
{
   __my_pthread_testcancel();
   return FORWARD(recvmsg, __libc_recvmsg, s, msg, flags);
}


typedef
ssize_t (*recvfrom_t)(int s, void *buf, size_t len, int flags,
                  struct sockaddr *from, socklen_t *fromlen);
WEAK
ssize_t recvfrom(int s, void *buf, size_t len, int flags,
                 struct sockaddr *from, socklen_t *fromlen)
{
   __my_pthread_testcancel();
   return FORWARD(recvfrom, __libc_recfrom, s, buf, len, flags, from, fromlen);
}


typedef
ssize_t (*sendto_t)(int s, const void *msg, size_t len, int flags, 
                    const struct sockaddr *to, socklen_t tolen);
WEAK
ssize_t sendto(int s, const void *msg, size_t len, int flags, 
               const struct sockaddr *to, socklen_t tolen)
{
   __my_pthread_testcancel();
   return FORWARD(sendto, __libc_sendto, s, msg, len, flags, to, tolen);
}


typedef 
int (*system_t)(const char* str);
WEAK
int system(const char* str)
{
   __my_pthread_testcancel();
   return FORWARD(system, __libc_system, str);
}


typedef
pid_t (*wait_t)(int *status);
WEAK
pid_t wait(int *status)
{
   __my_pthread_testcancel();
   return FORWARD(wait, __libc_wait, status);
}


typedef
int (*msync_t)(const void *start, size_t length, int flags);
WEAK
int msync(const void *start, size_t length, int flags)
{
   __my_pthread_testcancel();
   return FORWARD(msync, __libc_msync, start, length, flags);
}

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
strong_alias(pause, __pause)

weak_alias (__pread64, pread64)
weak_alias (__pwrite64, pwrite64)
weak_alias(__nanosleep, nanosleep)
weak_alias(__tcdrain, tcdrain)


typedef  
void (*longjmp_t)(jmp_buf env, int val) __attribute((noreturn));
/* not weak: WEAK */
void longjmp(jmp_buf env, int val)
{
   FORWARD(longjmp, __libc_longjmp, env, val);
}


typedef void (*siglongjmp_t) (sigjmp_buf env, int val)
                              __attribute__ ((noreturn));
void siglongjmp(sigjmp_buf env, int val)
{
   kludged("siglongjmp", "(it ignores cleanup handlers)");
   FORWARD(siglongjmp, __libc_siglongjmp, env, val);
}


/*--- fork and its helper ---*/

static
void run_fork_handlers ( int what )
{
   ForkHandlerEntry entry;
   int n_h, n_handlers, i, res;

   my_assert(what == 0 || what == 1 || what == 2);

   /* Fetch old counter */
   VALGRIND_MAGIC_SEQUENCE(n_handlers, -2 /* default */,
                           VG_USERREQ__GET_FHSTACK_USED,
                           0, 0, 0, 0);
   my_assert(n_handlers >= 0 && n_handlers < VG_N_FORKHANDLERSTACK);

   /* Prepare handlers (what == 0) are called in opposite order of
      calls to pthread_atfork.  Parent and child handlers are called
      in the same order as calls to pthread_atfork. */
   if (what == 0)
      n_h = n_handlers - 1;
   else
      n_h = 0;

   for (i = 0; i < n_handlers; i++) {
      VALGRIND_MAGIC_SEQUENCE(res, -2 /* default */,
                              VG_USERREQ__GET_FHSTACK_ENTRY,
                              n_h, &entry, 0, 0);
      my_assert(res == 0);
      switch (what) {
         case 0:  if (entry.prepare) entry.prepare(); 
                  n_h--; break;
         case 1:  if (entry.parent) entry.parent(); 
                  n_h++; break;
         case 2:  if (entry.child) entry.child(); 
                  n_h++; break;
         default: barf("run_fork_handlers: invalid what");
      }
   }

   if (what != 0 /* prepare */) {
      /* Empty out the stack. */
      VALGRIND_MAGIC_SEQUENCE(res, -2 /* default */,
                              VG_USERREQ__SET_FHSTACK_USED,
                              0, 0, 0, 0);
      my_assert(res == 0);
   }
}

typedef
pid_t (*__fork_t)(void);
pid_t __fork(void)
{
   pid_t pid;
   __my_pthread_testcancel();
   __pthread_mutex_lock(&pthread_atfork_lock);

   run_fork_handlers(0 /* prepare */);
   pid = FORWARD(__fork, __libc_fork);
   if (pid == 0) {
      /* I am the child */
      run_fork_handlers(2 /* child */);
      __pthread_mutex_unlock(&pthread_atfork_lock);
      __pthread_mutex_init(&pthread_atfork_lock, NULL);
   } else {
      /* I am the parent */
      run_fork_handlers(1 /* parent */);
      __pthread_mutex_unlock(&pthread_atfork_lock);
   }
   return pid;
}


pid_t __vfork(void)
{
   return __fork();
}



/* ---------------------------------------------------------------------
   Hacky implementation of semaphores.
   ------------------------------------------------------------------ */

#include <semaphore.h>

typedef
   struct {
      pthread_mutex_t se_mx;
      pthread_cond_t se_cv;
      int count;
      int waiters;
   }
   vg_sem_t;

#define SEM_CHECK_MAGIC 0x5b1d0772

typedef
   struct {
      union {
         vg_sem_t* p;
         int i;
      } shadow;
      int err_check;
   }
   user_sem_t;


static vg_sem_t* se_new ( sem_t* orig )
{
   user_sem_t* u_sem = (user_sem_t*)orig;
   vg_sem_t* vg_sem;

   vg_sem = my_malloc(sizeof(vg_sem_t));

   u_sem->shadow.p = vg_sem;
   u_sem->err_check = u_sem->shadow.i ^ SEM_CHECK_MAGIC;

   return vg_sem;
}

static vg_sem_t* se_lookup ( sem_t* orig )
{
   user_sem_t* u_sem = (user_sem_t*) orig;

   if(!u_sem->shadow.p || ((u_sem->shadow.i ^ SEM_CHECK_MAGIC) != u_sem->err_check))
      return NULL;
   
   return u_sem->shadow.p;
}
   
static void se_free( sem_t* orig )
{
   user_sem_t* u_sem = (user_sem_t*) orig;

   my_free(u_sem->shadow.p);

   u_sem->shadow.p = NULL;
   u_sem->err_check = 0;

   return;
}

int sem_init(sem_t *sem, int pshared, unsigned int value)
{
   int       res;
   vg_sem_t* vg_sem;
   ensure_valgrind("sem_init");
   if (pshared != 0) {
      pthread_error("sem_init: unsupported pshared value");
      *(__errno_location()) = ENOSYS;
      return -1;
   }
   vg_sem = se_new(sem);

   res = pthread_mutex_init(&vg_sem->se_mx, NULL);
   my_assert(res == 0);
   res = pthread_cond_init(&vg_sem->se_cv, NULL);
   my_assert(res == 0);
   vg_sem->count = value;
   vg_sem->waiters = 0;
   return 0;
}

int sem_wait ( sem_t* sem ) 
{
   int       res;
   vg_sem_t* vg_sem;
   ensure_valgrind("sem_wait");
   vg_sem = se_lookup(sem);
   if(!vg_sem) {
      pthread_error("sem_wait: semaphore overwritten or not initialized");
      *(__errno_location()) = EINVAL;
      return -1;
   }
   res = __pthread_mutex_lock(&vg_sem->se_mx);
   my_assert(res == 0);
   while (vg_sem->count == 0) {
      ++vg_sem->waiters;
      res = pthread_cond_wait(&vg_sem->se_cv, &vg_sem->se_mx);
      --vg_sem->waiters;
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
   vg_sem = se_lookup(sem);
   if(!vg_sem) {
      pthread_error("sem_post: semaphore overwritten or not initialized");
      *(__errno_location()) = EINVAL;
      return -1;
   }
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
   vg_sem = se_lookup(sem);
   if(!vg_sem) {
      pthread_error("sem_trywait: semaphore overwritten or not initialized");
      *(__errno_location()) = EINVAL;
      return -1;
   }
   res = __pthread_mutex_lock(&vg_sem->se_mx);
   my_assert(res == 0);
   if (vg_sem->count > 0) { 
      vg_sem->count--; 
      ret = 0; 
   } else { 
      ret = -1; 
      *(__errno_location()) = EAGAIN; 
   }
   res = __pthread_mutex_unlock(&vg_sem->se_mx);
   my_assert(res == 0);
   return ret;
}


int sem_getvalue(sem_t* sem, int * sval)
{
   int res;
   vg_sem_t* vg_sem; 
   ensure_valgrind("sem_getvalue");
   vg_sem = se_lookup(sem);
   if(!vg_sem) {
      pthread_error("sem_getvalue: semaphore overwritten or not initialized");
      *(__errno_location()) = EINVAL;
      return -1;
   }
   res = __pthread_mutex_lock(&vg_sem->se_mx);
   my_assert(res == 0);
   *sval = vg_sem->count;
   res = __pthread_mutex_unlock(&vg_sem->se_mx);
   my_assert(res == 0);
   return 0;
}


int sem_destroy(sem_t * sem)
{
   /* if someone waiting on this semaphore, errno = EBUSY, return -1 */
   vg_sem_t* vg_sem;
   int res;
   ensure_valgrind("sem_destroy");
   vg_sem = se_lookup(sem);
   if(!vg_sem) {
      pthread_error("sem_destroy: semaphore overwritten or not initialized");
      *(__errno_location()) = EINVAL;
      return -1;
   }
   res = __pthread_mutex_lock(&vg_sem->se_mx);
   my_assert(res == 0);
   if (vg_sem->waiters > 0)
   {
      *(__errno_location()) = EBUSY;
      res = __pthread_mutex_unlock(&vg_sem->se_mx);
      my_assert(res == 0);
      return -1;
   }
   res = pthread_cond_destroy(&vg_sem->se_cv);
   my_assert(res == 0);
   res = __pthread_mutex_unlock(&vg_sem->se_mx);
   my_assert(res == 0);
   res = pthread_mutex_destroy(&vg_sem->se_mx);
   my_assert(res == 0);
   se_free(sem);
   return 0;
}

  
int sem_timedwait(sem_t* sem, const struct timespec *abstime) 
{ 
   int       res; 
   vg_sem_t* vg_sem; 
   ensure_valgrind("sem_timedwait"); 
   vg_sem = se_lookup(sem); 
   if(!vg_sem) {
      pthread_error("sem_timedwait: semaphore overwritten or not initialized");
      *(__errno_location()) = EINVAL;
      return -1;
   }
   res = __pthread_mutex_lock(&vg_sem->se_mx); 
   my_assert(res == 0); 
   while ( vg_sem->count == 0 && res != ETIMEDOUT ) { 
      ++vg_sem->waiters;
      res = pthread_cond_timedwait(&vg_sem->se_cv, &vg_sem->se_mx, abstime); 
      --vg_sem->waiters;
   } 
   if ( vg_sem->count > 0 ) { 
      vg_sem->count--; 
      res = __pthread_mutex_unlock(&vg_sem->se_mx); 
      my_assert(res == 0 ); 
      return 0; 
   } else { 
      res = __pthread_mutex_unlock(&vg_sem->se_mx); 
      my_assert(res == 0 ); 
      *(__errno_location()) = ETIMEDOUT; 
      return -1; 
   } 
} 
 

/* ---------------------------------------------------------------------
   Reader-writer locks.
   ------------------------------------------------------------------ */

typedef 
   struct {
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


static pthread_mutex_t rw_new_mx = PTHREAD_MUTEX_INITIALIZER;

#define RWLOCK_CHECK_MAGIC 0xb5d17027


static void init_vg_rwlock ( vg_rwlock_t* vg_rwl )
{
   int res = 0;
   vg_rwl->prefer_w = 1;
   vg_rwl->nwait_r = 0;
   vg_rwl->nwait_w = 0;
   vg_rwl->status = 0;
   res = pthread_mutex_init(&vg_rwl->mx, NULL);
   res |= pthread_cond_init(&vg_rwl->cv_r, NULL);
   res |= pthread_cond_init(&vg_rwl->cv_w, NULL);
   my_assert(res == 0);
}

static vg_rwlock_t* rw_new ( pthread_rwlock_t* orig )
{
   int res;
   vg_rwlock_t* rwl;
   vg_pthread_rwlock_t* vg_orig;
   CONVERT(rwlock, orig, vg_orig);

   res = __pthread_mutex_lock(&rw_new_mx);
   my_assert(res == 0);

   rwl = my_malloc(sizeof(vg_rwlock_t));

   vg_orig->__vg_rw_writer = rwl;
   vg_orig->__vg_rw_read_waiting = (void *)((Addr)rwl ^ RWLOCK_CHECK_MAGIC);

   init_vg_rwlock(rwl);
   if (vg_orig->__vg_rw_kind == PTHREAD_RWLOCK_PREFER_READER_NP)
      rwl->prefer_w = 0;

   res = __pthread_mutex_unlock(&rw_new_mx);
   my_assert(res == 0);
  
   return rwl;
}

static vg_rwlock_t* rw_lookup ( pthread_rwlock_t* orig )
{
   vg_rwlock_t* rwl;
   vg_pthread_rwlock_t* vg_orig;
   CONVERT(rwlock, orig, vg_orig);

   if (vg_orig->__vg_rw_writer == NULL)
      rwl = rw_new ((pthread_rwlock_t*)vg_orig);
   else if (((Addr)vg_orig->__vg_rw_writer ^ RWLOCK_CHECK_MAGIC) == (Addr)vg_orig->__vg_rw_read_waiting)
      rwl = vg_orig->__vg_rw_writer;
   else
      rwl = NULL;

   return rwl;
}

static void rw_free ( pthread_rwlock_t* orig )
{
   int res;
   vg_rwlock_t* rwl;
   vg_pthread_rwlock_t* vg_orig;
   CONVERT(rwlock, orig, vg_orig);

   rwl = vg_orig->__vg_rw_writer;

   vg_orig->__vg_rw_writer = NULL;
   vg_orig->__vg_rw_read_waiting = NULL;

   res = __pthread_mutex_unlock(&rwl->mx);
   my_assert(res == 0);

   res = pthread_cond_destroy(&rwl->cv_w);
   res |= pthread_cond_destroy(&rwl->cv_r);
   res |= pthread_mutex_destroy(&rwl->mx);
   my_assert(res == 0);

   my_free(rwl);

   return;
}

int pthread_rwlock_init ( pthread_rwlock_t* orig,
                          const pthread_rwlockattr_t* attr )
{
   vg_rwlock_t* rwl;
   vg_pthread_rwlock_t* vg_orig;
   vg_pthread_rwlockattr_t* vg_attr;
   CONVERT(rwlock, orig, vg_orig);
   CONVERT(rwlockattr, attr, vg_attr);

   if (0) printf ("pthread_rwlock_init\n");
   /* Install the lock preference; the remapper needs to know it. */
   if (vg_attr)
      vg_orig->__vg_rw_kind = vg_attr->__vg_lockkind;
   else
      vg_orig->__vg_rw_kind = PTHREAD_RWLOCK_DEFAULT_NP;
   /* Allocate the shadow */
   rwl = rw_new ((pthread_rwlock_t *)vg_orig);
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
   rwl = rw_lookup (orig);
   if(!rwl) {
      pthread_error("pthread_rwlock_rdlock: lock overwritten or not initialized");
      return EINVAL;
   }
   res = __pthread_mutex_lock(&rwl->mx);
   my_assert(res == 0);
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
   rwl = rw_lookup (orig);
   if(!rwl) {
      pthread_error("pthread_rwlock_tryrdlock: lock overwritten or not initialized");
      return EINVAL;
   }
   res = __pthread_mutex_lock(&rwl->mx);
   my_assert(res == 0);
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
   rwl = rw_lookup (orig);
   if(!rwl) {
      pthread_error("pthread_rwlock_wrlock: lock overwritten or not initialized");
      return EINVAL;
   }
   res = __pthread_mutex_lock(&rwl->mx);
   my_assert(res == 0);
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
   rwl = rw_lookup (orig);
   if(!rwl) {
      pthread_error("pthread_rwlock_trywrlock: lock overwritten or not initialized");
      return EINVAL;
   }
   res = __pthread_mutex_lock(&rwl->mx);
   my_assert(res == 0);
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
   rwl = rw_lookup (orig);
   if(!rwl) {
      pthread_error("pthread_rwlock_unlock: lock overwritten or not initialized");
      return EINVAL;
   }
   res = __pthread_mutex_lock(&rwl->mx);
   my_assert(res == 0);
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
   rwl = rw_lookup (orig);
   if(!rwl) {
      pthread_error("pthread_rwlock_destroy: lock overwritten or not initialized");
      return EINVAL;
   }
   res = __pthread_mutex_lock(&rwl->mx);
   my_assert(res == 0);
   if (rwl->status != 0 || rwl->nwait_r > 0 || rwl->nwait_w > 0) {
      res = __pthread_mutex_unlock(&rwl->mx);
      my_assert(res == 0);
      return EBUSY;
   }
   rw_free (orig);
   return 0;
}


/* Copied directly from LinuxThreads. */
int
pthread_rwlockattr_init (pthread_rwlockattr_t *attr)
{
   vg_pthread_rwlockattr_t* vg_attr;
   CONVERT(rwlockattr, attr, vg_attr);
   vg_attr->__vg_lockkind = 0;
   vg_attr->__vg_pshared = PTHREAD_PROCESS_PRIVATE;
   return 0;
}

/* Copied directly from LinuxThreads. */
int
pthread_rwlockattr_destroy (pthread_rwlockattr_t *attr)
{
  return 0;
}

/* Copied directly from LinuxThreads. */
int
pthread_rwlockattr_setpshared (pthread_rwlockattr_t *attr, int pshared)
{
   vg_pthread_rwlockattr_t* vg_attr;
   CONVERT(rwlockattr, attr, vg_attr);

   if (pshared != PTHREAD_PROCESS_PRIVATE && pshared != PTHREAD_PROCESS_SHARED)
      return EINVAL;

   /* For now it is not possible to shared a conditional variable.  */
   if (pshared != PTHREAD_PROCESS_PRIVATE)
      return ENOSYS;

   vg_attr->__vg_pshared = pshared;

   return 0;
}



/* ---------------------------------------------------------------------
   Manage the allocation and use of RT signals.  The Valgrind core
   uses one.  glibc needs us to implement this to make RT signals
   work; things just seem to crash if we don't.
   ------------------------------------------------------------------ */
int __libc_current_sigrtmin (void)
{
   int res;

   VALGRIND_MAGIC_SEQUENCE(res, 0, 
			   VG_USERREQ__GET_SIGRT_MIN,
			   0, 0, 0, 0);

   return res;
}

int __libc_current_sigrtmax (void)
{
   int res;

   VALGRIND_MAGIC_SEQUENCE(res, 0, 
			   VG_USERREQ__GET_SIGRT_MAX,
			   0, 0, 0, 0);

   return res;
}

int __libc_allocate_rtsig (int high)
{
   int res;

   VALGRIND_MAGIC_SEQUENCE(res, 0, 
			   VG_USERREQ__ALLOC_RTSIG,
			   high, 0, 0, 0);

   return res;
}

/* ---------------------------------------------------------------------
   B'stard.
   ------------------------------------------------------------------ */
strong_alias(__pthread_mutex_lock, pthread_mutex_lock)
strong_alias(__pthread_mutex_timedlock, pthread_mutex_timedlock)
strong_alias(__pthread_mutex_trylock, pthread_mutex_trylock)
strong_alias(__pthread_mutex_unlock, pthread_mutex_unlock)
strong_alias(__pthread_mutexattr_init, pthread_mutexattr_init)
  weak_alias(__pthread_mutexattr_settype, pthread_mutexattr_settype)
  weak_alias(__pthread_mutexattr_gettype, pthread_mutexattr_gettype)
  weak_alias(__pthread_mutexattr_setpshared, pthread_mutexattr_setpshared)
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
     
weak_alias(__fork, fork)
weak_alias(__vfork, vfork)
weak_alias (__pthread_kill_other_threads_np, pthread_kill_other_threads_np)

/*--------------------------------------------------*/

weak_alias(pthread_rwlock_rdlock, __pthread_rwlock_rdlock)
weak_alias(pthread_rwlock_unlock, __pthread_rwlock_unlock)
weak_alias(pthread_rwlock_wrlock, __pthread_rwlock_wrlock)

weak_alias(pthread_rwlock_destroy, __pthread_rwlock_destroy)
weak_alias(pthread_rwlock_init, __pthread_rwlock_init)
weak_alias(pthread_rwlock_tryrdlock, __pthread_rwlock_tryrdlock)
weak_alias(pthread_rwlock_trywrlock, __pthread_rwlock_trywrlock)


#ifndef __UCLIBC__
/* These are called as part of stdio to lock the FILE structure for MT
   programs.  Unfortunately, the lock is not always a pthreads lock -
   the NPTL version uses a lighter-weight lock which uses futex
   directly (and uses a structure which is smaller than
   pthread_mutex).  So basically, this is completely broken on recent
   glibcs. */

#undef _IO_flockfile
void _IO_flockfile ( _IO_FILE * file )
{
   pthread_mutex_lock(file->_lock);
}
strong_alias(_IO_flockfile, __flockfile);
weak_alias(_IO_flockfile, flockfile);

#undef _IO_funlockfile
void _IO_funlockfile ( _IO_FILE * file )
{
   pthread_mutex_unlock(file->_lock);
}
strong_alias(_IO_funlockfile, __funlockfile);
weak_alias(_IO_funlockfile, funlockfile);
#endif


/* This doesn't seem to be needed to simulate libpthread.so's external
   interface, but many people complain about its absence. */

strong_alias(__pthread_mutexattr_settype, __pthread_mutexattr_setkind_np)
weak_alias(__pthread_mutexattr_setkind_np, pthread_mutexattr_setkind_np)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
