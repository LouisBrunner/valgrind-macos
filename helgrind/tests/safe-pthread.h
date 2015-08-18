#include <pthread.h>
#include <signal.h>
#include <setjmp.h>
#include <errno.h>
#include <assert.h>

static jmp_buf env;

/*
 * Starting with glibc 2.20 some pthread calls may execute
 * an xend instruction unconditionally when a lock is used in
 * a way that is invalid so defined a sigill handler that can
 * convert these invalid instructions to a normal error.
 */
static void sigill_handler( int signum, siginfo_t *siginfo, void *sigcontext ) {
   unsigned char *pc = siginfo->si_addr;
   assert( pc[0] == 0x0f && pc[1] == 0x01 && pc[2] == 0xd5 );
   longjmp( env, EPERM );
}

/*
 * Wrapper for pthread_rwlock_unlock which may execute xend
 * unconditionally when used on a lock that is not locked.
 *
 * Note that we return 0 instead of EPERM because that is what
 * glibc normally does - error reporting is optional.
 */
static int safe_pthread_rwlock_unlock( pthread_rwlock_t *rwlock ) {
#if __GLIBC_PREREQ(2,20) && ( defined(__i386__) || defined(__x86_64__) )
   struct sigaction sa;
   struct sigaction oldsa;
   int r;

   sa.sa_handler = NULL;
   sa.sa_sigaction = sigill_handler;
   sigemptyset( &sa.sa_mask );
   sa.sa_flags = SA_SIGINFO;
   sa.sa_restorer = NULL;
   
   sigaction( SIGILL, &sa, &oldsa );

   if ( ( r = setjmp( env ) ) == 0 ) {
     r = pthread_rwlock_unlock( rwlock );
   } else {
     r = 0;
   }

   sigaction( SIGILL, &oldsa, NULL );

   return r;
#else
   return pthread_rwlock_unlock( rwlock );
#endif
}

#define pthread_rwlock_unlock safe_pthread_rwlock_unlock
