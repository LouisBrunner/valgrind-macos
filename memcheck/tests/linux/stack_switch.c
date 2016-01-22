#define _XOPEN_SOURCE 600

/* Legacy feature macro.  */
#define _BSD_SOURCE
/* New feature macro, always define to squash warning about _BSD_SOURCE
   with glibc 2.20+.  */
#define _DEFAULT_SOURCE 1

#define _GNU_SOURCE

#include <stdio.h>

#include <sched.h>
#include <stdlib.h>
#include <string.h>
#include "tests/sys_mman.h"
#include <sys/syscall.h>
#include <sys/wait.h>
#include <unistd.h>

#include "valgrind.h"

#define STACK_SIZE 8192

#ifndef CLONE_THREAD
#define CLONE_THREAD	0x00010000	/* Same thread group? */
#endif

static int thread_main(void *arg)
{
   char buffer[1024];

   memset( buffer, 1, sizeof( buffer ) );

   sleep(2); /* ppc64-linux hack */
   return memchr( buffer, 1, sizeof( buffer ) ) == NULL;
}

int main(int argc, char **argv)
{
   void *stack;
   int stackid __attribute__((unused));
   pid_t pid;

   /* "2*" is a ppc64-linux hack */
   if ( ( stack = mmap( NULL, 2* STACK_SIZE, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0 ) ) == MAP_FAILED )
   {
      perror( "mmap" );
      exit( 1 );
   }

   stackid = VALGRIND_STACK_REGISTER( stack, stack + STACK_SIZE );

   if ( ( pid = clone( thread_main, stack + STACK_SIZE, CLONE_VM|CLONE_FS|CLONE_FILES|CLONE_SIGHAND|CLONE_THREAD|SIGCHLD, NULL ) ) == -1 )
   {
      perror( "clone" );
      exit( 1 );
   }

   sleep( 1 );

   exit( 0 );
}
