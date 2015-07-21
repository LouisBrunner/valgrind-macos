/* Test for the brk syscall wrapper. */

#include <assert.h>
#include <errno.h>
#include <stddef.h>
#include <sys/syscall.h>

/* Data segment end. */
extern int _end;
static char *begin = (char *)&_end;

__attribute__((noinline))
static int test_begin(void)
{
   int res = 0;
   int tmp;

   /* Check that a value at the break is inaccessible. */
   if (*begin)
      res++;

   /* Allocate one byte and check that the last byte is accessible and
      initialized. */
   tmp = syscall(SYS_brk, begin + 1);
   assert(tmp != -1);
   if (*begin)
      res++;

   /* Deallocate one byte and check that the last byte is now inaccessible. */
   tmp = syscall(SYS_brk, begin);
   assert(tmp != -1);
   if (*begin)
      res++;

   return res;
}

__attribute__((noinline))
static void test_updown(void)
{
   int tmp;
   size_t i;

#define MAX_SIZE 8192
   /* Run up phase. */
   for (i = 0; i < MAX_SIZE; i++) {
      tmp = syscall(SYS_brk, begin + i);
      assert(tmp != -1);
   }

   /* Run down phase. */
   for (i = 0; i < MAX_SIZE; i++) {
      tmp = syscall(SYS_brk, begin + MAX_SIZE - 1 - i);
      assert(tmp != -1);
   }
#undef MAX_SIZE
}

__attribute__((noinline))
static void test_range(void)
{
   int tmp;

   tmp = syscall(SYS_brk, begin - 1);
   assert(tmp == -1);
   assert(errno == ENOMEM);

   /* Unified limit for 64-bit and 32-bit version. */
   unsigned long long impossible_limit = 0xffffff4fffffffULL;
   tmp = syscall(SYS_brk, impossible_limit);
   assert(tmp == -1);
   assert(errno == ENOMEM);
}

int main(void)
{
   int res;
   res = test_begin();
   test_updown();
   test_range();
   return res;
}

