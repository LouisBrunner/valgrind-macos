#define _XOPEN_SOURCE 600 // to enable posix_memalign()
#include <assert.h>
#include <stdlib.h>
#include <malloc.h> // for memalign()
static __attribute__((noinline)) void bar ( int ); /* fwds */
int main(void) {
   int  sum = 0;
   int* x1 = (int*)malloc(sizeof(int));
   int* x2 = new int;
   int* x3 = new int[10];
   int* x4 = (int*)calloc(1, sizeof(int));
   int* x5 = (int*)memalign(8, sizeof(int));
   int* x6;  void* v6;
   int res = posix_memalign(&v6, 8, sizeof(int)); x6 = (int*)v6;
   assert(NULL != x1 && NULL != x2 && NULL != x3 && NULL != x4 &&
          NULL != x5 && 0 == res);

   __asm__ __volatile__("":::"memory");
   // all underruns
   sum += x1[-1]; __asm__ __volatile__("":::"memory"); bar(1);
   sum += x2[-1]; __asm__ __volatile__("":::"memory"); bar(2);
   sum += x3[-1]; __asm__ __volatile__("":::"memory"); bar(3);
   sum += x4[-1]; __asm__ __volatile__("":::"memory"); bar(4);
   sum += x5[-1]; __asm__ __volatile__("":::"memory"); bar(5);
   sum += x6[-1]; __asm__ __volatile__("":::"memory"); bar(6);
   __asm__ __volatile__("":::"memory");
   return sum;
}

/* What's with all this __asm__ __volatile__ stuff?  Well, it's an
   attempt to get gcc-4.1.2 not to claim the memory references that
   we're interested in -- x1[-1] through x6[-1] -- appear on different
   lines than they really do.  By its own rules, gcc can't move code
   across an __asm__ __volatile__, and the "memory" item says each one
   clobbers memory in some way which gcc can't know, so that probably
   (!)  persuades it not to carry memory CSEs around either. */

static __attribute__((noinline)) void bar ( int x )
{
   __asm__ __volatile__("":::"memory");
}
