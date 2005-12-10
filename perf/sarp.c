// This artificial program allocates and deallocates a lot of large objects
// on the stack.  It is a stress test for Memcheck's set_address_range_perms
// (sarp) function.  Pretty much all Valgrind versions up to 3.1.X do very
// badly on it, ie. a slowdown of at least 100x.
//
// It is representative of tsim_arch, the simulator for the University of
// Texas's TRIPS processor, whose performance under Valgrind is dominated by
// the handling of one frequently-called function that allocates 8348 bytes
// on the stack.

#include <assert.h>
#include <time.h>

#define REPS   1000*1000

int f(int i)
{
   // This nonsense is just to ensure that the compiler does not optimise
   // away the stack allocation.
   char big_array[8348];
   big_array[0]    = 12;
   big_array[2333] = 34;
   big_array[5678] = 56;
   big_array[8347] = 78;
   assert( 8000 == (&big_array[8100] - &big_array[100]) );
   return big_array[i];
}

int main(void)
{
   int i, sum = 0;

   struct timespec req;
   req.tv_sec  = 0;
   req.tv_nsec = 100*1000*1000;   // 0.1s

   // Pause for a bit so that the native run-time is not 0.00, which leads
   // to ridiculous slow-down figures.
   nanosleep(&req, NULL);
   
   for (i = 0; i < REPS; i++) {
      sum += f(i & 0xff);
   }
   return sum % 256;
}

