
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

#include <pthread.h>
#include <unistd.h>
#include <string.h>


/* ---------------------------------------------------------------------
   Helpers.  We have to be pretty self-sufficient.
   ------------------------------------------------------------------ */

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
static
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
void barf ( char* str )
{
   char buf[100];
   buf[0] = 0;
   strcat(buf, "\nvg_libpthread.so: ");
   strcat(buf, str);
   strcat(buf, "\n\n");
   write(2, buf, strlen(buf));
   myexit(1);
}



/* ---------------------------------------------------------------------
   Pass pthread_ calls to Valgrind's request mechanism.
   ------------------------------------------------------------------ */

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


/* What are these?  Anybody know?  I don't. */

void _pthread_cleanup_push_defer ( void )
{
  //  char* str = "_pthread_cleanup_push_defer\n";
  //  write(2, str, strlen(str));
}

void _pthread_cleanup_pop_restore ( void )
{
  //  char* str = "_pthread_cleanup_pop_restore\n";
  //  write(2, str, strlen(str));
}


static int thread_specific_errno[VG_N_THREADS];

int* __errno_location ( void )
{
   int tid;
   ensure_valgrind("__errno_location");
   VALGRIND_MAGIC_SEQUENCE(tid, 0 /* default */,
                           VG_USERREQ__PTHREAD_GET_THREADID,
                           0, 0, 0, 0);
   /* 'cos I'm paranoid ... */
   if (tid < 0 || tid >= VG_N_THREADS)
      barf("__errno_location: invalid ThreadId");
   return & thread_specific_errno[tid];
}


int pthread_mutexattr_init(pthread_mutexattr_t *attr)
{
   char* str = "IGNORED pthread_mutexattr_init\n";
   write(2, str, strlen(str));
   return 0;
}

int pthread_mutex_init(pthread_mutex_t *mutex, 
                       const  pthread_mutexattr_t *mutexattr)
{
   int res;
   //  char* str = "pthread_mutex_init\n";
   //  write(2, str, strlen(str));
   ensure_valgrind("pthread_mutex_init");
   VALGRIND_MAGIC_SEQUENCE(res, 0 /* default */,
                           VG_USERREQ__PTHREAD_MUTEX_INIT,
                           mutex, mutexattr, 0, 0);
   return res;
}

int pthread_mutexattr_destroy(pthread_mutexattr_t *attr)
{
  char* str = "IGNORED pthread_mutexattr_destroy\n";
  write(2, str, strlen(str));
  return 0;
}

int pthread_mutex_lock(pthread_mutex_t *mutex)
{
   int res;
   if (!(RUNNING_ON_VALGRIND)) {
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

int pthread_mutex_unlock(pthread_mutex_t *mutex)
{
   int res;
   if (!(RUNNING_ON_VALGRIND)) {
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

int pthread_mutex_destroy(pthread_mutex_t *mutex)
{
   int res;
   if (!(RUNNING_ON_VALGRIND)) {
      char* str = "pthread_mutex_destroy-NOT-INSIDE-VALGRIND\n";
      write(2, str, strlen(str));
      return 0;
   } else {
      VALGRIND_MAGIC_SEQUENCE(res, 0 /* default */,
                              VG_USERREQ__PTHREAD_MUTEX_DESTROY,
                              mutex, 0, 0, 0);
   }
   return res;
}


int pthread_setcanceltype(int type, int *oldtype)
{
   char* str = "IGNORED pthread_setcanceltype\n";
   write(2, str, strlen(str));
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


/* ---------------------------------------------------------------------
   These are here (I think) because they are deemed cancellation
   points by POSIX.  For the moment we'll simply pass the call along
   to the corresponding thread-unaware (?) libc routine.
   ------------------------------------------------------------------ */

#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <errno.h>
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
  //  char* str = "sigaction\n";
  //  write(2, str, strlen(str));
  return __libc_sigaction(signum, act, oldact);
}


extern
int  __libc_connect(int  sockfd,  
                    const  struct  sockaddr  *serv_addr, 
                    socklen_t addrlen);
int  connect(int  sockfd,  
             const  struct  sockaddr  *serv_addr, 
             socklen_t addrlen)
{
  //  char* str = "connect\n";
  //  write(2, str, strlen(str));
  return __libc_connect(sockfd, serv_addr, addrlen);
}


extern
int __libc_fcntl(int fd, int cmd, long arg);
int fcntl(int fd, int cmd, long arg)
{
  //  char* str = "fcntl\n";
  //  write(2, str, strlen(str));
  return __libc_fcntl(fd, cmd, arg);
}


extern 
ssize_t __libc_write(int fd, const void *buf, size_t count);
ssize_t write(int fd, const void *buf, size_t count)
{
  //  char* str = "write\n";
  //  write(2, str, strlen(str));
  return __libc_write(fd, buf, count);
}


extern 
ssize_t __libc_read(int fd, void *buf, size_t count);
ssize_t read(int fd, void *buf, size_t count)
{
  //  char* str = "read\n";
  //  write(2, str, strlen(str));
  return __libc_read(fd, buf, count);
}


extern
int __libc_open(const char *pathname, int flags);
int open(const char *pathname, int flags)
{
  //  char* str = "open\n";
  //  write(2, str, strlen(str));
  return __libc_open(pathname, flags);
}


extern
int __libc_close(int fd);
int close(int fd)
{
  //  char* str = "open\n";
  //  write(2, str, strlen(str));
  return __libc_close(fd);
}


extern
int __libc_accept(int s, struct sockaddr *addr, socklen_t *addrlen);
int accept(int s, struct sockaddr *addr, socklen_t *addrlen)
{
  //  char* str = "accept\n";
  //  write(2, str, strlen(str));
  return __libc_accept(s, addr, addrlen);
}


extern
pid_t __libc_fork(void);
pid_t fork(void)
{
  //  char* str = "fork\n";
  //  write(2, str, strlen(str));
  return __libc_fork();
}


extern
pid_t __libc_waitpid(pid_t pid, int *status, int options);
pid_t waitpid(pid_t pid, int *status, int options)
{
  //  char* str = "waitpid\n";
  //  write(2, str, strlen(str));
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

/* I've no idea what these are, but they get called quite a lot.
   Anybody know? */

#undef _IO_flockfile
void _IO_flockfile ( _IO_FILE * file )
{
  //  char* str = "_IO_flockfile\n";
  //  write(2, str, strlen(str));
  //  barf("_IO_flockfile");
}

#undef _IO_funlockfile
void _IO_funlockfile ( _IO_FILE * file )
{
  //  char* str = "_IO_funlockfile\n";
  //  write(2, str, strlen(str));
  //barf("_IO_funlockfile");
}

