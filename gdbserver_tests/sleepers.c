#define _GNU_SOURCE
#include <string.h>
#include <pthread.h>
#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/syscall.h>
#include <sched.h>
#include <signal.h>
static int loops = 15; // each thread+main will do this amount of loop
static int sleepms = 1000; // in each loop, will sleep "sleepms" milliseconds
static int burn = 0; // after each sleep, will burn cpu in a tight 'burn' loop 
static void setup_sigusr_handler(void); // sigusr1 and 2 sigaction setup.

static pid_t gettid()
{
#ifdef __NR_gettid
   return syscall(__NR_gettid);
#else
   return getpid();
#endif
}
// will be invoked from gdb.
static void whoami(char *msg) __attribute__((unused));
static void whoami(char *msg)
{
   fprintf(stderr, "pid %d Thread %d %s\n", getpid(), gettid(), msg);
   fflush(stderr);
}


static void do_burn ()
{
   int i;
   int loopnr = 0;
   // one single line for the below, to ensure interrupt on this line.
   for (i = 0; i < burn; i++) loopnr++;
}

static int thread_ready = 0;
static pthread_cond_t ready = PTHREAD_COND_INITIALIZER;
static pthread_mutex_t ready_mutex = PTHREAD_MUTEX_INITIALIZER;
static void signal_ready (void)
{
   int rc;
   rc = pthread_mutex_lock(&ready_mutex);
   if (rc != 0)
      fprintf(stderr, "signal_ready lock error %d_n", rc);
   thread_ready = 1;
   rc = pthread_cond_signal(&ready);
   if (rc != 0)
      fprintf(stderr, "signal_ready signal error %d_n", rc);
   rc = pthread_mutex_unlock(&ready_mutex);
   if (rc != 0)
      fprintf(stderr, "signal_ready unlock error %d_n", rc);
}

struct spec {
   char *name;
   int sleep;
   int burn;
   int t;
};
static struct timeval t[4];
static int nr_sleeper_or_burner = 0;
static volatile int report_finished = 1; 
// set to 0 to have no finish msg (as order is non-deterministic)
static void *sleeper_or_burner(void *v)
{
   int i = 0;
   struct spec* s = (struct spec*)v;

   fprintf(stderr, "%s ready to sleep and/or burn\n", s->name);
   fflush (stderr);
   signal_ready();
   nr_sleeper_or_burner++;

   for (i = 0; i < loops; i++) {
      if (sleepms > 0 && s->sleep) {
         t[s->t].tv_sec = sleepms / 1000;
         t[s->t].tv_usec = (sleepms % 1000) * 1000;
         select (0, NULL, NULL, NULL, &t[s->t]);
      }
      if (burn > 0 && s->burn)
         do_burn();
   }
   if (report_finished) {
      fprintf(stderr, "%s finished to sleep and/or burn\n", s->name);
      fflush (stderr);
   }
   return NULL;
}

// wait till a thread signals it is ready
static void wait_ready(void)
{
   int rc;
   rc = pthread_mutex_lock(&ready_mutex);
   if (rc != 0)
      fprintf(stderr, "wait_ready lock error %d_n", rc);
   while (! thread_ready && rc == 0) {
      rc = pthread_cond_wait(&ready, &ready_mutex);
      if (rc != 0)
         fprintf(stderr, "wait_ready wait error %d_n", rc);
   }
   thread_ready = 0;
   rc = pthread_mutex_unlock(&ready_mutex);
   if (rc != 0)
      fprintf(stderr, "wait_ready unlock error %d_n", rc);
}

// We will lock ourselves on one single cpu.
// This bypasses the unfairness of the Valgrind scheduler
// when a multi-cpu machine has enough cpu to run all the
// threads wanting to burn cpu.
static void setaffinity(void)
{
#ifdef VGO_linux
   cpu_set_t single_cpu;
   CPU_ZERO(&single_cpu);
   CPU_SET(1, &single_cpu);
   (void) sched_setaffinity(0, sizeof(single_cpu), &single_cpu);
#endif
   // GDBTD: equivalent for Darwin ?
}

int main (int argc, char *argv[])
{
  char *threads_spec;
  pthread_t ebbr, egll, zzzz;
  struct spec b, l, p, m;
  char *some_mem __attribute__((unused)) = malloc(100);
  setaffinity();
  setup_sigusr_handler();
  if (argc > 1)
     loops = atoi(argv[1]);

  if (argc > 2)
     sleepms = atoi(argv[2]);

  if (argc > 3)
     burn = atoll(argv[3]);

  if (argc > 4)
     threads_spec = argv[4];
  else
     threads_spec = "BSBSBSBS";
  
  fprintf(stderr, "loops/sleep_ms/burn/threads_spec:  %d %d %d %s\n",
          loops, sleepms, burn, threads_spec);
  fflush(stderr);

  b.name = "Brussels";
  b.burn = *threads_spec++ == 'B';
  b.sleep = *threads_spec++ == 'S';
  b.t = -1;
  if (b.burn || b.sleep) {
     b.t = 1;
     pthread_create(&ebbr, NULL, sleeper_or_burner, &b);
     wait_ready();
  }
  
  l.name = "London";
  l.burn = *threads_spec++ == 'B';
  l.sleep = *threads_spec++ == 'S';
  l.t = -1;
  if (l.burn || l.sleep) {
     l.t = 2;
     pthread_create(&egll, NULL, sleeper_or_burner, &l);
     wait_ready();
  }

  p.name = "Petaouchnok";
  p.burn = *threads_spec++ == 'B';
  p.sleep = *threads_spec++ == 'S';
  p.t = -1;
  if (p.burn || p.sleep) {
     p.t = 3;
     pthread_create(&zzzz, NULL, sleeper_or_burner, &p);
     wait_ready();
  }

  m.name = "main";
  m.burn = *threads_spec++ == 'B';
  m.sleep = *threads_spec++ == 'S';
  m.t = 0;
  sleeper_or_burner(&m);

  if (b.t != -1) pthread_join(ebbr, NULL);
  if (l.t != -1) pthread_join(egll, NULL);
  if (p.t != -1) pthread_join(zzzz, NULL);

  return 0;
}

static int sigusr1_received = 0;
static void sigusr1_handler(int signr)
{
   sigusr1_received++;
}
static void setup_sigusr_handler(void)
{
   struct sigaction sa;
   sa.sa_handler = sigusr1_handler;
   sigemptyset(&sa.sa_mask);
   sa.sa_flags = 0;

   if (sigaction (SIGUSR1, &sa, NULL) != 0)
      perror("sigaction SIGUSR1");

   sa.sa_handler = SIG_IGN;
   if (sigaction (SIGUSR2, &sa, NULL) != 0)
      perror("sigaction SIGUSR2");
}

