/* JSGF: no idea what this is actually doing, but it really gives the
   signals/sigaltstack/threads machinery a working out */
/**
 * Compile with: 
 * gcc -g -Wall -lpthread -o susphello susphello.c
 *
 * Author Magnus Ihse, ihse at bea.com 
 */

#include <signal.h>


#include <errno.h>
#include <stddef.h>
#include <pthread.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <sys/resource.h>
#include <unistd.h>
#include <sys/syscall.h>
#include <dlfcn.h>


#include <pthread.h>
#include <unistd.h>
#include <string.h>
#include <netdb.h>
#include <stdio.h>
#include <stdlib.h>

#define THREAD_COUNT 10
#define ITER_COUNT 200

static volatile int finishedArray[THREAD_COUNT];
static int pKey;

static sigset_t srSigset;

pthread_t main_thread;

int srSignal = SIGUSR1;


void
ptiSrSigHandler(int sig, siginfo_t *sip, void *arg)
{
   //ucontext_t *ucontext = (ucontext_t *)arg;
   
   int mypos = (int) pthread_getspecific(pKey);
   
   
//   int threadPos = (int)pthread_getspecific(srThreadKey);

//   thread->os_context = (OSContextP)&(ucontext->uc_mcontext);

   // Notify suspender that we have been suspended
   if (pthread_kill(main_thread, srSignal) == -1) {
      perror("pthread_kill");
      exit(1);
   }
   
   finishedArray[mypos]++;

//   printf("this is thread %d: i'm now suspended!\n", mypos);

   // Wait until we are resumed
   while (sigwaitinfo(&srSigset, NULL) == -1) {
      // Interrupted by SIGSTOP in gdb
      if(errno != EINTR) {
      	perror("sigwaitinfo");
        exit(1);
      }
   }

//   printf("this is thread %d: i'm now resumed!\n", mypos);

   //thread->os_context = NULL; // just for the sake of it...

   // Notify resumer that we have been resumed
   if (pthread_kill(main_thread, srSignal) == -1) {
      perror("pthread_kill");
      exit(1);
   }
//   printf("this is thread %d: and I've told Master!!\n", mypos);

}


void
suspendOrResume(pthread_t thread, int i)
{
   sigset_t oss;

   // Mask out suspend/resume signal until we explicitly wait for it
   sigprocmask(SIG_BLOCK, &srSigset, &oss);

   // Send signal to suspend or resume the thread
   if (pthread_kill(thread, srSignal) == -1) {
      perror("pthread_kill");
      exit(1);
   }

//   printf("sent signal to %d...", i);
   // Wait for notification from thread being suspended/resumed
   while (sigwaitinfo(&srSigset, NULL) == -1) {
      // Interrupted by SIGSTOP in gdb
      if(errno != EINTR) {
      	perror("sigwaitinfo");
        exit(1);
      }
   }

   // Restore original signal mask
   sigprocmask(SIG_SETMASK, &oss, NULL);

//   printf("... okay, %d suspended\n", i);
}



void
initSignalling(void)
{
   struct sigaction sa;

   // Signal mask for suspend/resume
   sigemptyset(&srSigset);
   sigaddset(&srSigset, srSignal);

   // Set up signal handler for suspend/resume
   sa.sa_flags = SA_RESTART | SA_SIGINFO | SA_ONSTACK;
   sa.sa_sigaction = ptiSrSigHandler;
   sigfillset(&sa.sa_mask);
   sigdelset(&sa.sa_mask, (__SIGRTMIN+1));
   if (sigaction(srSignal, &sa, 0) == -1) {
      perror("sigaction");
      exit(1);
   }

   // Unblock suspend signal
   sigprocmask(SIG_UNBLOCK, &srSigset, 0);

   main_thread = pthread_self();
}


void* setup_altstack(void) {
   stack_t ss;

       ss.ss_sp = malloc(20*1024);
       if (ss.ss_sp == 0) {
	   return NULL;
       }
       ss.ss_size = 20*1024;
       ss.ss_flags = 0;
       
       if (sigaltstack(&ss, NULL) == -1) {
	   perror("sigaltstack");
           return NULL;
       }
       return ss.ss_sp;
}

void takedown_altstack(void* stack) {
   struct sigaltstack ss;
   int result;
   
   ss.ss_flags = SS_DISABLE;
   ss.ss_sp = (void*)47;  // This value should be ignored when ss_flags is SS_DISABLE
   ss.ss_size = 29;       // This value should be ignored when ss_flags is SS_DISABLE
   
   {
       result = sigaltstack(&ss, NULL);
       free(stack);
   }
}

void *threadfunc(void *arg) {
   int mypos = (int)arg;
   int i;
   long square = 1;
   void* altstack = setup_altstack();
   
   pthread_setspecific(pKey, arg);
   for (i=0; i < 1000; i++) {
      square = i*i + square*mypos;
   }

// wait for signal   
      while (finishedArray[mypos] == 0) {
   struct timespec req, rem;

   req.tv_sec = 0;
   req.tv_nsec = 5 * 1000 * 1000;

   nanosleep(&req, &rem);
         
   };
   
   finishedArray[mypos]++;
   
   takedown_altstack(altstack);
   
   return NULL;
}


int main(int argc, char ** argv) {
  pthread_t threads[THREAD_COUNT];
  pthread_attr_t attr;
  int result;
  int i;
  int iteration;
  int finished;

  initSignalling();
  
  pthread_attr_init(&attr);
  pthread_attr_setstacksize(&attr, 128*1024);

  pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
  
  pthread_key_create(&pKey, NULL);

  for (iteration = 0; iteration < ITER_COUNT; iteration++) {
#if 0
     if ((iteration % 100) == 0) {
	printf("\nStarting run series %i: ", iteration);
     }

     if ((iteration % 10) == 0) {
	printf(".");
        fflush(stdout);
     }
#endif

      // Clear array
      for (i = 0; i< THREAD_COUNT; i++) {
	 finishedArray[i] = 0;
      }

      // Start threads
      for (i = 0; i< THREAD_COUNT; i++) {
	 result = pthread_create(&threads[i], &attr, threadfunc, (void*)i);
	 if (result != 0) {
	    perror("pthread_create");
	    exit(1);
	 }
    }
    
//    printf("all threads started\n");
    // suspend threads
    for (i = 0; i< THREAD_COUNT; i++) {
    	suspendOrResume(threads[i], i);
    }
    
//    printf("now all threads are suspended\n");
    
    // resume threads
    for (i = 0; i< THREAD_COUNT; i++) {
    	suspendOrResume(threads[i], i);
    }
    

      // Join threads
/*      
      printf("about to join...");
      for (i = 0; i< THREAD_COUNT; i++) {
	 result = pthread_join(threads[i], NULL);
	 if (result != 0) {
	    perror("pthread_join");
	    exit(1);
	 }
      }
      
      printf("...joined");
*/      
//      printf("Spin waiting for results\n");
      finished = 1;
      do {
   struct timespec req, rem;

   req.tv_sec = 0;
   req.tv_nsec = 5 * 1000 * 1000;
   finished = 1;

   nanosleep(&req, &rem);
         
//         sleep(1);
         for (i = 0; i< THREAD_COUNT; i++) {
            if (finishedArray[i] < 2) {
               finished = 0;
//               printf("no result at: %d, value: %d\n", i, finishedArray[i]);
               break;
            }
	 }
//         sleep(1);
      } while (!finished);

  }

  printf("PASSED\n");
  return 0;
}
