#include <pthread.h>
#include <signal.h>
#include <setjmp.h>
#include <errno.h>
#include <assert.h>

static sigjmp_buf env;

/*
 * Starting with glibc 2.20 some pthread calls may execute
 * an xend instruction unconditionally when a lock is used in
 * a way that is invalid so defined a sigill handler that can
 * convert these invalid instructions to a normal error.
 */
static void sigill_handler( int signum, siginfo_t *siginfo, void *sigcontext ) {
   unsigned char *pc = siginfo->si_addr;
   assert( pc[0] == 0x0f && pc[1] == 0x01 && pc[2] == 0xd5 );
   siglongjmp( env, EPERM );
}

/*
 * Same as above, but in case we do recognize the xend,
 * but detect it is invalid (used outside a transaction)
 * and generate a segv.  Unfortunately then si_addr is,
 * just zero, so we cannot add an assert/sanity check.
 */
static void segv_handler( int signum, siginfo_t *siginfo, void *sigcontext ) {
   siglongjmp( env, EPERM );
}

static int safe_pthread_rwlock_unlock( pthread_rwlock_t *rwlock ) __attribute__((unused));
/*
 * Wrapper for pthread_rwlock_unlock which may execute xend
 * unconditionally when used on a lock that is not locked.
 *
 * Note that we return 0 instead of EPERM because that is what
 * glibc normally does - error reporting is optional.
 */
static int safe_pthread_rwlock_unlock( pthread_rwlock_t *rwlock ) {
   struct sigaction sa_ill, sa_segv;
   struct sigaction oldsa_ill, oldsa_segv;
   int r;

   sa_ill.sa_handler = NULL;
   sa_ill.sa_sigaction = sigill_handler;
   sigemptyset( &sa_ill.sa_mask );
   sa_ill.sa_flags = SA_SIGINFO;
   
   sigaction( SIGILL, &sa_ill, &oldsa_ill );

   sa_segv.sa_handler = NULL;
   sa_segv.sa_sigaction = segv_handler;
   sigemptyset( &sa_segv.sa_mask );
   sa_segv.sa_flags = SA_SIGINFO;

   sigaction( SIGSEGV, &sa_segv, &oldsa_segv );

   if ( ( r = sigsetjmp( env, 1 ) ) == 0 ) {
     r = pthread_rwlock_unlock( rwlock );
   } else {
     r = 0;
   }

   sigaction( SIGILL, &oldsa_ill, NULL );
   sigaction( SIGSEGV, &oldsa_segv, NULL );

   return r;
}

#define pthread_rwlock_unlock safe_pthread_rwlock_unlock
