
/* This is a replacement for the standard libpthread.so.  It is loaded
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

#include <unistd.h>
#include <string.h>
#ifdef GLIBC_2_1
#include <sys/time.h>
#endif

/* ---------------------------------------------------------------------
   Helpers.  We have to be pretty self-sufficient.
   ------------------------------------------------------------------ */

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
void myexit ( int arg )
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


/* Give up without using printf etc, since they seem to give
   segfaults. */
static __inline__
void ensure_valgrind ( char* caller )
{
   char* str;
   int is_valgrind = RUNNING_ON_VALGRIND;
   if (!is_valgrind) {
      str = "\nvalgrind-ed process: vg_libpthread.so: "
            "pthread call when\n";
      write(2, str, strlen(str));
      str = "not running on valgrind; aborting!  "
            "This is probably a bug in\n";
      write(2, str, strlen(str));
      str = "valgrind.  Please report it to me at: "
            "jseward@acm.org.  Thanks.\n";
      write(2, str, strlen(str));
      str = "unexpectedly called function is: ";
      write(2, str, strlen(str));
      write(2, caller, strlen(caller));
      str = "\n\n";
      write(2, str, strlen(str));
      myexit(1);
   }
}


static
__attribute__((noreturn))
void barf ( char* str )
{
   char buf[100];
   buf[0] = 0;
   strcat(buf, "\nvg_libpthread.so: ");
   strcat(buf, str);
   strcat(buf, "\n\n");
   write(2, buf, strlen(buf));
   myexit(1);
   /* We have to persuade gcc into believing this doesn't return. */
   while (1) { };
}


static void ignored ( char* msg )
{
   if (get_pt_trace_level() >= 1) {
      char* ig = "vg_libpthread.so: IGNORED call to: ";
      write(2, ig, strlen(ig));
      write(2, msg, strlen(msg));
      ig = "\n";
      write(2, ig, strlen(ig));
   }
}

static void kludged ( char* msg )
{
   if (get_pt_trace_level() >= 1) {
      char* ig = "vg_libpthread.so: KLUDGED call to: ";
      write(2, ig, strlen(ig));
      write(2, msg, strlen(msg));
      ig = "\n";
      write(2, ig, strlen(ig));
   }
}

void vgPlain_unimp ( char* what )
{
   char* ig = "vg_libpthread.so: UNIMPLEMENTED FUNCTION: ";
   write(2, ig, strlen(ig));
   write(2, what, strlen(what));
   ig = "\n";
   write(2, ig, strlen(ig));
   barf("Please report this bug to me at: jseward@acm.org");
}


/* ---------------------------------------------------------------------
   Pass pthread_ calls to Valgrind's request mechanism.
   ------------------------------------------------------------------ */

#include <pthread.h>
#include <stdio.h>
#include <errno.h>
#include <assert.h>
#include <sys/time.h> /* gettimeofday */

/* ---------------------------------------------------
   THREAD ATTRIBUTES
   ------------------------------------------------ */

int pthread_attr_init(pthread_attr_t *attr)
{
   ignored("pthread_attr_init");
   return 0;
}

int pthread_attr_setdetachstate(pthread_attr_t *attr, int detachstate)
{
   ignored("pthread_attr_setdetachstate");
   return 0;
}

int pthread_attr_setinheritsched(pthread_attr_t *attr, int inherit)
{
   ignored("pthread_attr_setinheritsched");
   return 0;
}

/* This is completely bogus. */
int  pthread_attr_getschedparam(const  pthread_attr_t  *attr,  
                                struct sched_param *param)
{
   kludged("pthread_attr_getschedparam");
#  ifdef GLIBC_2_1
   if (param) param->sched_priority = 0; /* who knows */
#  else
   if (param) param->__sched_priority = 0; /* who knows */
#  endif
   return 0;
}

int  pthread_attr_setschedparam(pthread_attr_t  *attr,
                                const  struct sched_param *param)
{
   ignored("pthread_attr_setschedparam");
   return 0;
}

int pthread_attr_destroy(pthread_attr_t *attr)
{
   ignored("pthread_attr_destroy");
   return 0;
}

/* ---------------------------------------------------
   THREADs
   ------------------------------------------------ */

int pthread_equal(pthread_t thread1, pthread_t thread2)
{
   return thread1 == thread2 ? 1 : 0;
}


int
pthread_create (pthread_t *__restrict __thread,
                __const pthread_attr_t *__restrict __attr,
                void *(*__start_routine) (void *),
                void *__restrict __arg)
{
   int res;
   ensure_valgrind("pthread_create");
   VALGRIND_MAGIC_SEQUENCE(res, 0 /* default */,
                           VG_USERREQ__PTHREAD_CREATE,
                           __thread, __attr, __start_routine, __arg);
   return res;
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
   int res;
   ensure_valgrind("pthread_exit");
   VALGRIND_MAGIC_SEQUENCE(res, 0 /* default */,
                           VG_USERREQ__PTHREAD_EXIT,
                           retval, 0, 0, 0);
   /* Doesn't return! */
   /* However, we have to fool gcc into knowing that. */
   barf("pthread_exit: still alive after request?!");
}


pthread_t pthread_self(void)
{
   int tid;
   ensure_valgrind("pthread_self");
   VALGRIND_MAGIC_SEQUENCE(tid, 0 /* default */,
                           VG_USERREQ__PTHREAD_GET_THREADID,
                           0, 0, 0, 0);
   if (tid < 0 || tid >= VG_N_THREADS)
      barf("pthread_self: invalid ThreadId");
   return tid;
}


int pthread_detach(pthread_t th)
{
   ignored("pthread_detach");
   return 0;
}


/* ---------------------------------------------------
   MUTEX ATTRIBUTES
   ------------------------------------------------ */

int pthread_mutexattr_init(pthread_mutexattr_t *attr)
{
   attr->__mutexkind = PTHREAD_MUTEX_ERRORCHECK_NP;
   return 0;
}

int pthread_mutexattr_settype(pthread_mutexattr_t *attr, int type)
{
   switch (type) {
#     ifndef GLIBC_2_1    
      case PTHREAD_MUTEX_TIMED_NP:
      case PTHREAD_MUTEX_ADAPTIVE_NP:
#     endif
      case PTHREAD_MUTEX_RECURSIVE_NP:
      case PTHREAD_MUTEX_ERRORCHECK_NP:
         attr->__mutexkind = type;
         return 0;
      default:
         return EINVAL;
   }
}

int pthread_mutexattr_destroy(pthread_mutexattr_t *attr)
{
   return 0;
}


/* ---------------------------------------------------
   MUTEXes
   ------------------------------------------------ */

int pthread_mutex_init(pthread_mutex_t *mutex, 
                       const  pthread_mutexattr_t *mutexattr)
{
   mutex->__m_count = 0;
   mutex->__m_owner = (_pthread_descr)VG_INVALID_THREADID;
   mutex->__m_kind  = PTHREAD_MUTEX_ERRORCHECK_NP;
   if (mutexattr)
      mutex->__m_kind = mutexattr->__mutexkind;
   return 0;
}

int pthread_mutex_lock(pthread_mutex_t *mutex)
{
   int res;
   static int moans = 3;
   if (!(RUNNING_ON_VALGRIND) && moans-- > 0) {
      char* str = "pthread_mutex_lock-NOT-INSIDE-VALGRIND\n";
      write(2, str, strlen(str));
      return 0;
   } else {
      VALGRIND_MAGIC_SEQUENCE(res, 0 /* default */,
                              VG_USERREQ__PTHREAD_MUTEX_LOCK,
                              mutex, 0, 0, 0);
      return res;
   }
}

int pthread_mutex_trylock(pthread_mutex_t *mutex)
{
   int res;
   static int moans = 3;
   if (!(RUNNING_ON_VALGRIND) && moans-- > 0) {
      char* str = "pthread_mutex_trylock-NOT-INSIDE-VALGRIND\n";
      write(2, str, strlen(str));
      return 0;
   } else {
      VALGRIND_MAGIC_SEQUENCE(res, 0 /* default */,
                              VG_USERREQ__PTHREAD_MUTEX_TRYLOCK,
                              mutex, 0, 0, 0);
      return res;
   }
}

int pthread_mutex_unlock(pthread_mutex_t *mutex)
{
   int res;
   static int moans = 3;
   if (!(RUNNING_ON_VALGRIND) && moans-- > 0) {
      char* str = "pthread_mutex_unlock-NOT-INSIDE-VALGRIND\n";
      write(2, str, strlen(str));
      return 0;
   } else {
      VALGRIND_MAGIC_SEQUENCE(res, 0 /* default */,
                              VG_USERREQ__PTHREAD_MUTEX_UNLOCK,
                              mutex, 0, 0, 0);
      return res;
   }
}

int pthread_mutex_destroy(pthread_mutex_t *mutex)
{
   /* Valgrind doesn't hold any resources on behalf of the mutex, so no
      need to involve it. */
    if (mutex->__m_count > 0)
       return EBUSY;
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
   kludged("pthread_getschedparam");
   if (policy) *policy = SCHED_OTHER;
#  ifdef GLIBC_2_1
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
   assert(ms_now != 0xFFFFFFFF);
   res = gettimeofday(&timeval_now, NULL);
   assert(res == 0);

   ull_ms_now_after_1970 
      = 1000ULL * ((unsigned long long int)(timeval_now.tv_sec))
        + ((unsigned long long int)(timeval_now.tv_usec / 1000000));
   ull_ms_end_after_1970
      = 1000ULL * ((unsigned long long int)(abstime->tv_sec))
        + ((unsigned long long int)(abstime->tv_nsec / 1000000));
   assert(ull_ms_end_after_1970 >= ull_ms_now_after_1970);
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
   ignored("pthread_setcancelstate");
   return 0;
}

int pthread_setcanceltype(int type, int *oldtype)
{
   ignored("pthread_setcanceltype");
   return 0;
}

int pthread_cancel(pthread_t thread)
{
   int res;
   ensure_valgrind("pthread_cancel");
   VALGRIND_MAGIC_SEQUENCE(res, 0 /* default */,
                           VG_USERREQ__PTHREAD_CANCEL,
                           thread, 0, 0, 0);
   return res;
}

void pthread_testcancel(void)
{
}

/*-------------------*/
static pthread_mutex_t massacre_mx = PTHREAD_MUTEX_INITIALIZER;

void __pthread_kill_other_threads_np ( void )
{
   int i, res, me;
   pthread_mutex_lock(&massacre_mx);
   me = pthread_self();
   for (i = 1; i < VG_N_THREADS; i++) {
      if (i == me) continue;
      res = pthread_cancel(i);
      if (res == 0)
         printf("----------- NUKED %d\n", i);
   }
   pthread_mutex_unlock(&massacre_mx);
}


/* ---------------------------------------------------
   THREAD-SPECIFICs
   ------------------------------------------------ */

int pthread_key_create(pthread_key_t *key,  
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
   ignored("pthread_key_delete");
   return 0;
}

int pthread_setspecific(pthread_key_t key, const void *pointer)
{
   int res;
   ensure_valgrind("pthread_setspecific");
   VALGRIND_MAGIC_SEQUENCE(res, 0 /* default */,
                           VG_USERREQ__PTHREAD_SETSPECIFIC,
                           key, pointer, 0, 0);
   return res;
}

void * pthread_getspecific(pthread_key_t key)
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


int pthread_once ( pthread_once_t *once_control, 
                   void (*init_routine) (void) )
{
   int res;
   ensure_valgrind("pthread_once");

   res = pthread_mutex_lock(&once_masterlock);

   if (res != 0)
      barf("pthread_once: Looks like your program's "
           "init routine calls back to pthread_once() ?!");

   if (*once_control == 0) {
      *once_control = 1;
      init_routine();
   }

   pthread_mutex_unlock(&once_masterlock);

   return 0;
}


/* ---------------------------------------------------
   MISC
   ------------------------------------------------ */

int pthread_atfork ( void (*prepare)(void),
                     void (*parent)(void),
                     void (*child)(void) )
{
   ignored("pthread_atfork");
   return 0;
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
   if (tid < 0 || tid >= VG_N_THREADS)
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
   if (tid < 0 || tid >= VG_N_THREADS)
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
   if (tid < 0 || tid >= VG_N_THREADS)
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
enum __libc_tsd_key_t { _LIBC_TSD_KEY_MALLOC = 0,
                        _LIBC_TSD_KEY_DL_ERROR,
                        _LIBC_TSD_KEY_N };

/* Auto-initialising subsystem.  libc_specifics_inited is set 
   after initialisation.  libc_specifics_inited_mx guards it. */
static int             libc_specifics_inited    = 0;
static pthread_mutex_t libc_specifics_inited_mx = PTHREAD_MUTEX_INITIALIZER;

/* These are the keys we must initialise the first time. */
static pthread_key_t libc_specifics_keys[_LIBC_TSD_KEY_N];

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
      for (i = 0; i < _LIBC_TSD_KEY_N; i++) {
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
   int res;
   /* printf("SET SET SET key %d ptr %p\n", key, pointer); */
   if (key < _LIBC_TSD_KEY_MALLOC || key >= _LIBC_TSD_KEY_N)
      barf("libc_internal_tsd_set: invalid key");
   init_libc_tsd_keys();
   res = pthread_setspecific(libc_specifics_keys[key], pointer);
   if (res != 0) barf("libc_internal_tsd_set: setspecific failed");
   return 0;
}

static void *
libc_internal_tsd_get ( enum __libc_tsd_key_t key )
{
   void* v;
   /* printf("GET GET GET key %d\n", key); */
   if (key < _LIBC_TSD_KEY_MALLOC || key >= _LIBC_TSD_KEY_N)
      barf("libc_internal_tsd_get: invalid key");
   init_libc_tsd_keys();
   v = pthread_getspecific(libc_specifics_keys[key]);
   /* if (v == NULL) barf("libc_internal_tsd_set: getspecific failed"); */
   return v;
}




int (*__libc_internal_tsd_set)(enum __libc_tsd_key_t key, const void * pointer)
     = libc_internal_tsd_set;

void* (*__libc_internal_tsd_get)(enum __libc_tsd_key_t key)
     = libc_internal_tsd_get;


/* ---------------------------------------------------------------------
   These are here (I think) because they are deemed cancellation
   points by POSIX.  For the moment we'll simply pass the call along
   to the corresponding thread-unaware (?) libc routine.
   ------------------------------------------------------------------ */

#include <stdlib.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/socket.h>

extern
int __libc_sigaction
             (int signum, 
              const struct sigaction *act,  
              struct  sigaction *oldact);
int sigaction(int signum, 
              const struct sigaction *act,  
              struct  sigaction *oldact)
{
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
int  connect(int  sockfd,  
             const  struct  sockaddr  *serv_addr, 
             socklen_t addrlen)
{
   return __libc_connect(sockfd, serv_addr, addrlen);
}


extern
int __libc_fcntl(int fd, int cmd, long arg);
int fcntl(int fd, int cmd, long arg)
{
   return __libc_fcntl(fd, cmd, arg);
}


extern 
ssize_t __libc_write(int fd, const void *buf, size_t count);
ssize_t write(int fd, const void *buf, size_t count)
{
   return __libc_write(fd, buf, count);
}


extern 
ssize_t __libc_read(int fd, void *buf, size_t count);
ssize_t read(int fd, void *buf, size_t count)
{
   return __libc_read(fd, buf, count);
}

 
extern
int __libc_open64(const char *pathname, int flags, mode_t mode);
int open64(const char *pathname, int flags, mode_t mode)
{
   return __libc_open64(pathname, flags, mode);
}


extern
int __libc_open(const char *pathname, int flags, mode_t mode);
int open(const char *pathname, int flags, mode_t mode)
{
   return __libc_open(pathname, flags, mode);
}


extern
int __libc_close(int fd);
int close(int fd)
{
   return __libc_close(fd);
}


extern
int __libc_accept(int s, struct sockaddr *addr, socklen_t *addrlen);
int accept(int s, struct sockaddr *addr, socklen_t *addrlen)
{
   return __libc_accept(s, addr, addrlen);
}


extern
pid_t __libc_fork(void);
pid_t fork(void)
{
   return __libc_fork();
}


extern
pid_t __libc_waitpid(pid_t pid, int *status, int options);
pid_t waitpid(pid_t pid, int *status, int options)
{
   return __libc_waitpid(pid, status, options);
}


extern
int __libc_nanosleep(const struct timespec *req, struct timespec *rem);
int nanosleep(const struct timespec *req, struct timespec *rem)
{
   return __libc_nanosleep(req, rem);
}


extern
int __libc_fsync(int fd);
int fsync(int fd)
{
   return __libc_fsync(fd);
}


extern
off_t __libc_lseek(int fildes, off_t offset, int whence);
off_t lseek(int fildes, off_t offset, int whence)
{
   return __libc_lseek(fildes, offset, whence);
}


extern
__off64_t __libc_lseek64(int fildes, __off64_t offset, int whence);
__off64_t lseek64(int fildes, __off64_t offset, int whence)
{
   return __libc_lseek64(fildes, offset, whence);
}


extern  
void __libc_longjmp(jmp_buf env, int val) __attribute((noreturn));
void longjmp(jmp_buf env, int val)
{
   __libc_longjmp(env, val);
}


extern
int __libc_send(int s, const void *msg, size_t len, int flags);
int send(int s, const void *msg, size_t len, int flags)
{
   return __libc_send(s, msg, len, flags);
}


extern
int __libc_recv(int s, void *buf, size_t len, int flags);
int recv(int s, void *buf, size_t len, int flags)
{
   return __libc_recv(s, buf, len, flags);
}


extern
int __libc_sendto(int s, const void *msg, size_t len, int flags, 
                  const struct sockaddr *to, socklen_t tolen);
int sendto(int s, const void *msg, size_t len, int flags, 
           const struct sockaddr *to, socklen_t tolen)
{
   return __libc_sendto(s, msg, len, flags, to, tolen);
}


extern 
int __libc_system(const char* str);
int system(const char* str)
{
   return __libc_system(str);
}


extern
pid_t __libc_wait(int *status);
pid_t wait(int *status)
{
   return __libc_wait(status);
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
   * (checked via assert) types fd_set and vki_fd_set are identical.
   * (checked via assert) types timeval and vki_timeval are identical.
   * (unchecked) libc error numbers (EINTR etc) are the negation of the
     kernel's error numbers (VKI_EINTR etc).
*/


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
      assert(res == 0);
      ms_end = ms_now;
      ms_end += (timeout->tv_usec / 1000);
      ms_end += (timeout->tv_sec * 1000);
      /* Stay sane ... */
      assert (ms_end >= ms_now);
   }

   /* fprintf(stderr, "MY_SELECT: before loop\n"); */

   /* Either timeout == NULL, meaning wait indefinitely, or timeout !=
      NULL, in which case ms_end holds the end time. */
   while (1) {
      if (timeout) {
         VALGRIND_MAGIC_SEQUENCE(ms_now, 0xFFFFFFFF /* default */,
                                 VG_USERREQ__READ_MILLISECOND_TIMER,
                                 0, 0, 0, 0);
         assert(ms_now != 0xFFFFFFFF);
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
      (void)my_do_syscall2(__NR_nanosleep, 
                           (int)(&nanosleep_interval), (int)NULL);
   }
}




#include <sys/poll.h>

#ifdef GLIBC_2_1
typedef unsigned long int nfds_t;
#endif

int poll (struct pollfd *__fds, nfds_t __nfds, int __timeout)
{
   unsigned int        ms_now, ms_end;
   int                 res, i;
   struct vki_timespec nanosleep_interval;

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
      ms_end += (unsigned int)__timeout;
   }

   /* fprintf(stderr, "MY_POLL: before loop\n"); */

   /* Either timeout < 0, meaning wait indefinitely, or timeout > 0,
      in which case t_end holds the end time. */
   assert(__timeout != 0);

   while (1) {
      if (__timeout > 0) {
         VALGRIND_MAGIC_SEQUENCE(ms_now, 0xFFFFFFFF /* default */,
                                 VG_USERREQ__READ_MILLISECOND_TIMER,
                                 0, 0, 0, 0);
         assert(ms_now != 0xFFFFFFFF);
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


/* ---------------------------------------------------------------------
   B'stard.
   ------------------------------------------------------------------ */

# define strong_alias(name, aliasname) \
  extern __typeof (name) aliasname __attribute__ ((alias (#name)));

strong_alias(pthread_mutex_lock, __pthread_mutex_lock)
strong_alias(pthread_mutex_trylock, __pthread_mutex_trylock)
strong_alias(pthread_mutex_unlock, __pthread_mutex_unlock)
strong_alias(pthread_mutexattr_init, __pthread_mutexattr_init)
strong_alias(pthread_mutexattr_settype, __pthread_mutexattr_settype)
strong_alias(pthread_mutex_init, __pthread_mutex_init)
strong_alias(pthread_mutexattr_destroy, __pthread_mutexattr_destroy)
strong_alias(pthread_mutex_destroy, __pthread_mutex_destroy)
strong_alias(pthread_once, __pthread_once)
strong_alias(pthread_atfork, __pthread_atfork)

strong_alias(fork, __fork)
strong_alias(close, __close)
strong_alias(write, __write)
strong_alias(read, __read)
strong_alias(open, __open)
strong_alias(sigaction, __sigaction)

strong_alias(pthread_key_create, __pthread_key_create)
strong_alias(pthread_getspecific, __pthread_getspecific)
strong_alias(pthread_setspecific, __pthread_setspecific)
strong_alias(open64, __open64)
strong_alias(fcntl, __fcntl)
strong_alias(connect, __connect)

/*--------------------------------------------------*/

int
pthread_rwlock_rdlock (void* /* pthread_rwlock_t* */ rwlock)
{
   kludged("pthread_rwlock_rdlock");
   return 0;
}

strong_alias(pthread_rwlock_rdlock, __pthread_rwlock_rdlock)


int
pthread_rwlock_unlock (void* /* pthread_rwlock_t* */ rwlock)
{
   kludged("pthread_rwlock_unlock");
   return 0;
}

strong_alias(pthread_rwlock_unlock, __pthread_rwlock_unlock)


/* I've no idea what these are, but they get called quite a lot.
   Anybody know? */

#undef _IO_flockfile
void _IO_flockfile ( _IO_FILE * file )
{
   //char* str = "_IO_flockfile\n";
   //write(2, str, strlen(str));
   pthread_mutex_lock(file->_lock);
   //  barf("_IO_flockfile");
}

#undef _IO_funlockfile
void _IO_funlockfile ( _IO_FILE * file )
{
   //char* str = "_IO_funlockfile\n";
   //write(2, str, strlen(str));
   pthread_mutex_unlock(file->_lock);
   //  barf("_IO_funlockfile");
}

void _pthread_cleanup_push_defer ( void )
{
   //    char* str = "_pthread_cleanup_push_defer\n";
   //    write(2, str, strlen(str));
}

void _pthread_cleanup_pop_restore ( void )
{
   //    char* str = "_pthread_cleanup_pop_restore\n";
   //    write(2, str, strlen(str));
}
