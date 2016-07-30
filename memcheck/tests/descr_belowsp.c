#include "../../config.h"

#define _GNU_SOURCE
#include <inttypes.h>
#include <stdio.h>
#include <pthread.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include <assert.h>
#include <setjmp.h>
#include <signal.h>
#ifdef HAVE_GETPAGESIZE
#include <unistd.h>
#endif
#include "../../include/valgrind.h"
#include "../memcheck.h"

typedef  unsigned long   UWord;
typedef  UWord           Addr;
#define VG_ROUNDDN(p, a)   ((Addr)(p) & ~((Addr)(a)-1))
#define VG_ROUNDUP(p, a)   VG_ROUNDDN((p)+(a)-1, (a))

static pthread_t children;

// If != 0, will test addr description does not explode with
// wrong stack registration.
static int shake_with_wrong_registration = 0;

/* Do whatever to have the stack grown enough that
   we can access below sp relatively safely */
static void grow_the_stack(void)
{
   int i;
   char m[5000];
   for (i = 0; i < sizeof(m); i++)
      m[i] = i;
   sprintf(m, "do whatever %d", i);
   if (strlen(m) > 1000)
      fprintf(stderr, "something went wrong with %s\n", m);
}

static char s[1000];
static void describe (char* what, void* a)
{
   fprintf(stderr, "describing %#" PRIxPTR " %s\n", (uintptr_t) a, what);
   sprintf(s, "v.info location %#" PRIxPTR, (uintptr_t) a);
   VALGRIND_MONITOR_COMMAND(s);
}

static void bad_things_below_sp (void)
{
   int i;
   char *p = (char*)&i;
   describe ("1500 bytes below a local var", p-1500);
}


static volatile char *lowest_j;
static jmp_buf goback;

static void sigsegv_handler(int signr)
{
   longjmp(goback, 1);
}

static void bad_things_till_guard_page(void)
{
   char j = 0;
   char *p = &j;

   for (;;) {
      j = j + *p;
      p = p - 400;
      lowest_j = p;
   }
}

static int guess_pagesize(void)
{
#ifdef HAVE_GETPAGESIZE
   const int pagesize = getpagesize();
#else
   const int pagesize = 4096; // let's say ?
#endif
   return pagesize;
}

static void describe_many(void)
{
   const int pagesize = guess_pagesize();
   describe ("discovered address giving SEGV in thread stack",
             (void*)lowest_j);
   describe ("byte just above highest guardpage byte",
             (void*) VG_ROUNDUP(lowest_j, pagesize));
   describe ("highest guardpage byte",
             (void*) VG_ROUNDUP(lowest_j, pagesize)-1);
   describe ("lowest guardpage byte",
             (void*) VG_ROUNDDN(lowest_j, pagesize));
   /* Cannot test the next byte, as we cannot predict how
      this byte will be described. */
}

static void* child_fn_0 ( void* arg )
{
   grow_the_stack();
   bad_things_below_sp();

   if (setjmp(goback)) {
      describe_many();
   } else
      bad_things_till_guard_page();

   if (shake_with_wrong_registration) {
      // Do whatever stupid things we could imagine
      // with stack registration and see no explosion happens
      // Note: this is executed only if an arg is given to the program.
      // 
      
      const int pgsz = guess_pagesize();
      int stackid;

      fprintf(stderr, "\n\nShaking after unregistering stack\n");
      // Assuming our first stack was automatically registered as nr 1
      VALGRIND_STACK_DEREGISTER(1);
      // Test with no stack registered
      describe_many();

      fprintf(stderr, "\n\nShaking with small stack\n");
      stackid = VALGRIND_STACK_REGISTER((void*) VG_ROUNDDN(&stackid, pgsz),
                                        (void*) VG_ROUNDUP(&stackid, pgsz));
      describe_many();
      VALGRIND_STACK_DEREGISTER(stackid);

      fprintf(stderr, "\n\nShaking with huge stack\n");
      stackid = VALGRIND_STACK_REGISTER((void*) 0x0,
                                        (void*) VG_ROUNDUP(&stackid, 2<<20));
      describe_many();
      VALGRIND_STACK_DEREGISTER(stackid);


   }

   return NULL;
}

int main(int argc, const char** argv)
{
   struct sigaction sa;
   int r;

   shake_with_wrong_registration = argc > 1;

   /* We will discover the thread guard page using SEGV.
      So, prepare an handler. */
   sa.sa_handler = sigsegv_handler;
   sigemptyset(&sa.sa_mask);
   sa.sa_flags = 0;

   if (sigaction (SIGSEGV, &sa, NULL) != 0)
      perror("sigaction");

   grow_the_stack();
   bad_things_below_sp();

   pthread_attr_t attrs;
   r = pthread_attr_init(&attrs);
   assert(!r);

#  if defined(VGO_solaris)
   /* Solaris needs to configure at least two page sizes to have
      a visible stack guard page. One page size is deducted for
      an implicit mmap red zone. */
   r = pthread_attr_setguardsize(&attrs, 2 * guess_pagesize());
   assert(!r);
#  endif /* VGO_solaris */
   
   r = pthread_create(&children, &attrs, child_fn_0, NULL);
   assert(!r);

   r = pthread_attr_destroy(&attrs);
   assert(!r);

   r = pthread_join(children, NULL);
   assert(!r);
 

   return 0;
}

