
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


/* ---------------------------------------------------------------------
   Pass pthread_ calls to Valgrind's request mechanism.
   ------------------------------------------------------------------ */

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
   ignored("pthread_mutexattr_init");
   return 0;
}

int pthread_mutex_init(pthread_mutex_t *mutex, 
                       const  pthread_mutexattr_t *mutexattr)
{
   int res;
   ensure_valgrind("pthread_mutex_init");
   VALGRIND_MAGIC_SEQUENCE(res, 0 /* default */,
                           VG_USERREQ__PTHREAD_MUTEX_INIT,
                           mutex, mutexattr, 0, 0);
   return res;
}

int pthread_mutexattr_destroy(pthread_mutexattr_t *attr)
{
   ignored("pthread_mutexattr_destroy");
   return 0;
}

int pthread_mutexattr_settype(pthread_mutexattr_t *attr, int type)
{
   ignored("pthread_mutexattr_settype");
   return 0;
}

int pthread_mutex_lock(pthread_mutex_t *mutex)
{
   int res;
   static int moans = 5;
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

int pthread_mutex_unlock(pthread_mutex_t *mutex)
{
   int res;
   static int moans = 5;
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
   static int moans = 5;
   if (!(RUNNING_ON_VALGRIND) && moans-- > 0) {
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



int pthread_key_create(pthread_key_t *key,  
                       void  (*destr_function)  (void *))
{
   ignored("pthread_key_create");
   return 0;
}

int pthread_key_delete(pthread_key_t key)
{
   ignored("pthread_key_delete");
   return 0;
}

int pthread_setspecific(pthread_key_t key, const void *pointer)
{
   ignored("pthread_setspecific");
   return 0;
}

void * pthread_getspecific(pthread_key_t key)
{
   ignored("pthread_setspecific");
   return NULL;
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
int __libc_open(const char *pathname, int flags);
int open(const char *pathname, int flags)
{
   return __libc_open(pathname, flags);
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


/*--------------------------------------------------*/

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
   if (is_kerror(res)) {
      * (__errno_location()) = -res;
      return -1;
   } else {
      return res;
   }
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
*/
#include <assert.h>


int select ( int n, 
             fd_set *rfds, 
             fd_set *wfds, 
             fd_set *xfds, 
             struct timeval *timeout )
{
   int    res;
   fd_set rfds_copy;
   fd_set wfds_copy;
   fd_set xfds_copy;
   struct vki_timeval  t_now;
   struct vki_timeval  t_end;
   struct vki_timeval  zero_timeout;
   struct vki_timespec nanosleep_interval;

   ensure_valgrind("select");

   /* We assume that the kernel and libc data layouts are identical
      for the following types.  These asserts provide a crude
      check. */
   if (sizeof(fd_set) != sizeof(vki_fd_set)
       || sizeof(struct timeval) != sizeof(struct vki_timeval))
      barf("valgrind's hacky non-blocking select(): data sizes error");

   /* If a zero timeout specified, this call is harmless. */
   if (timeout && timeout->tv_sec == 0 && timeout->tv_usec == 0)
      return do_syscall_select( n, (vki_fd_set*)rfds, 
                                   (vki_fd_set*)wfds, 
                                   (vki_fd_set*)xfds, 
                                   (struct vki_timeval*)timeout);

   /* If a timeout was specified, set t_end to be the end wallclock
      time. */
   if (timeout) {
      res = my_do_syscall2(__NR_gettimeofday, (int)&t_now, (int)NULL);
      assert(res == 0);
      t_end = t_now;
      t_end.tv_usec += timeout->tv_usec;
      t_end.tv_sec  += timeout->tv_sec;
      if (t_end.tv_usec >= 1000000) {
         t_end.tv_usec -= 1000000;
         t_end.tv_sec += 1;
      }
      /* Stay sane ... */
      assert (t_end.tv_sec > t_now.tv_sec
              || (t_end.tv_sec == t_now.tv_sec 
                  && t_end.tv_usec >= t_now.tv_usec));
   }

   /* fprintf(stderr, "MY_SELECT: before loop\n"); */

   /* Either timeout == NULL, meaning wait indefinitely, or timeout !=
      NULL, in which case t_end holds the end time. */
   while (1) {
      if (timeout) {
         res = my_do_syscall2(__NR_gettimeofday, (int)&t_now, (int)NULL);
         assert(res == 0);
         if (t_now.tv_sec > t_end.tv_sec
             || (t_now.tv_sec == t_end.tv_sec 
                 && t_now.tv_usec > t_end.tv_usec)) {
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
      if (res < 0) {
         /* some kind of error (including EINTR); errno is set, so just
            return.  The sets are unspecified in this case. */
         return res;
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
      nanosleep_interval.tv_sec = 0;
      nanosleep_interval.tv_nsec = 40 * 1000 * 1000; /* 40 milliseconds */
      /* It's critical here that valgrind's nanosleep implementation
         is nonblocking. */
      (void)my_do_syscall2(__NR_nanosleep, 
                           (int)(&nanosleep_interval), (int)NULL);
   }
}
