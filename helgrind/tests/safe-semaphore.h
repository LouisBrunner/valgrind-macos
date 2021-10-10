#include <semaphore.h>
#include <signal.h>
#include <setjmp.h>
#include <errno.h>
#include <assert.h>

static sigjmp_buf env;

/*
 * Newer glibc crashes on really bogus semaphors.
 * Catch a SIGABRT and turn it into a EINVAL.
 */
static void abrt_handler( int signum, siginfo_t *siginfo, void *sigcontext ) {
   siglongjmp( env, EINVAL );
}

static int safe_sem_post( sem_t *sem ) __attribute__((unused));
static int safe_sem_post( sem_t *sem ) {
   struct sigaction sa;
   struct sigaction oldsa;
   int r, e;

   sa.sa_handler = NULL;
   sa.sa_sigaction = abrt_handler;
   sigemptyset( &sa.sa_mask );
   sa.sa_flags = SA_SIGINFO;

   sigaction( SIGABRT, &sa, &oldsa );

   if ( ( e = sigsetjmp( env, 1 ) ) == 0 ) {
     r = sem_post( sem );
   } else {
     r = -1;
   }
   errno = e;

   sigaction( SIGABRT, &oldsa, NULL );

   return r;
}

#define sem_post safe_sem_post
