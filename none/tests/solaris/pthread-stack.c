/* This test causes an error in 3.10.1 and earlier versions like so:

==8336== Can't extend stack to 0x4033f98 during signal delivery for thread 2:
==8336==   no stack segment

  The reason was that only AnonC segments were considered as stack
  segments. */

#include <assert.h>
#include <fcntl.h>
#include <pthread.h>
#include <setjmp.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/mman.h>
#include <sys/stat.h>

static volatile char *lowest_j;
static jmp_buf goback;

static void sigsegv_handler(int signr)
{
   longjmp(goback, 1);
}

static void bad_things_till_guard_page(void)
{
   fprintf(stderr, "... doing bad things till guard page\n");
   char j = 0;
   char *p = &j;

   for (;;) {
      j = j + *p;
      p = p - 400;
      lowest_j = p;
   }
}

static void say_something(void)
{
  fprintf(stderr, "plugh\n");
}

static void *child_func(void *arg)
{
   if (setjmp(goback))
      say_something();
   else
      bad_things_till_guard_page();

   return NULL;
}

int main(int argc, const char *argv[])
{
   /* We will discover the thread guard page using SEGV.
      So, prepare a handler. */
   struct sigaction sa;
   sa.sa_handler = sigsegv_handler;
   sigemptyset(&sa.sa_mask);
   sa.sa_flags = 0;
   if (sigaction(SIGSEGV, &sa, NULL) != 0)
      perror("sigaction");

   /* Create a file that will be used as stack for a pthread.  */
   size_t file_size = 1024 * 1024;
   const char file_name[] = "FILE";
   int fd = open(file_name, O_CREAT|O_WRONLY,
                 S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH|S_IWOTH);
   assert(fd >= 0);
   void *p = calloc(file_size, 1);
   assert(p != 0);
   int written = write(fd, p, file_size);
   assert(written == file_size);
   close(fd);

   /* Create a file-based stack for the child. */
   fd = open(file_name, O_CREAT|O_RDWR,
             S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH|S_IWOTH);
   assert(fd >= 0);
   const size_t stack_size = 256 * 1024;
   assert(stack_size < file_size);
   void *stack = mmap(NULL, stack_size, PROT_READ|PROT_WRITE,
                      MAP_PRIVATE, fd, 0);
   assert(stack != (void *)-1);
   pthread_attr_t attr;
   pthread_attr_init(&attr);
   int r = pthread_attr_setstack(&attr, stack, stack_size);
   assert(r == 0);

   /* Create child run. */
   pthread_t child;
   r = pthread_create(&child, &attr, child_func, NULL);
   assert(r == 0);
   r = pthread_join(child, NULL);
   assert(r == 0);

   /* Remove file */
   unlink(file_name);
   return 0;
}

