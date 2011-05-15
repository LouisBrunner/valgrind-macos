// This program is a thorough test of the LOADVn/STOREVn shadow memory
// operations.

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "tests/sys_mman.h"
#include "memcheck/memcheck.h"

// All the sizes here are in *bytes*, not bits.

typedef unsigned char        U1;
typedef unsigned short       U2;
typedef unsigned int         U4;
typedef unsigned long long   U8;

typedef float                F4;
typedef double               F8;

typedef unsigned long        UWord;

#define PAGE_SIZE 4096ULL


// XXX: should check the error cases for SET/GET_VBITS also

// For the byte 'x', build a value of 'size' bytes from that byte, eg:
//   size 1 --> x
//   size 2 --> xx
//   size 4 --> xxxx
//   size 8 --> xxxxxxxx
// where the 0 bits are seen by Memcheck as defined, and the 1 bits are
// seen as undefined (ie. the value of each bit matches its V bit, ie. the
// resulting value is the same as its metavalue).
//
U8 build(int size, U1 byte)
{
   int i;
   U8 mask = 0;
   U8 shres;
   U8 res = 0xffffffffffffffffULL, res2;
   VALGRIND_MAKE_MEM_UNDEFINED(&res, 8);
   assert(1 == size || 2 == size || 4 == size || 8 == size);

   for (i = 0; i < size; i++) {
      mask <<= 8;
      mask |= (U8)byte;
   }

   res &= mask;      
   
   // res is now considered partially defined, but we know exactly what its
   // value is (it happens to be the same as its metavalue).
   
   (void)VALGRIND_GET_VBITS(&res, &shres, 8);
   res2 = res;
   (void)VALGRIND_MAKE_MEM_DEFINED(&res2, 8);  // avoid the 'undefined' warning
   assert(res2 == shres);
   return res;
}

U1 make_defined ( U1 x )
{
   volatile U1 xx = x;
   VALGRIND_MAKE_MEM_DEFINED(&xx, 1);
   return xx;
}

void check(U1* arr, int n, char* who)
{
   int i;
   U1* shadow = malloc(n);
   U1 arr_i;
   U8 sum = 0;
   (void)VALGRIND_GET_VBITS(arr, shadow, n);
   for (i = 0; i < n; i++) {
      arr_i = make_defined(arr[i]);
      if (arr_i != shadow[i]) {
          fprintf(stderr, "\n\nFAILURE: %s, byte %d -- "
                          "is 0x%x, should be 0x%x\n\n",
                          who, i, shadow[i], arr[i]);
          exit(1);
      }
      sum += (U8)arr_i;
   }
   free(shadow);
   printf("test passed, sum = %llu (%9.5f per byte)\n", 
	  sum, (F8)sum / (F8)n);
}

static inline U4 randomU4 ( void )
{
   static U4 n = 0;
   /* From "Numerical Recipes in C" 2nd Edition */
   n = 1664525UL * n + 1013904223UL;
   return n;
}

static inline U1 randomU1 ( void )
{
   return 0xFF & (randomU4() >> 13);
}

#define N_BYTES  300000
#define N_EVENTS (5 * N_BYTES)


void do_test_at ( U1* arr )
{
   int i;

   U4 mv1 = 0, mv2 = 0, mv4 = 0, mv8 = 0, mv4f = 0, mv8f = 0;

   /* Fill arr with random bytes whose shadows match them. */
   if (0) printf("-------- arr = %p\n", arr);

   printf("initialising\n");
   for (i = 0; i < N_BYTES; i++)
      arr[i] = (U1)build(1, randomU1());

   printf("post-initialisation check\n");
   check(arr, N_BYTES, "after initialisation");

   /* Now do huge numbers of memory copies. */
   printf("doing copies\n");
   for (i = 0; i < N_EVENTS; i++) {
      U4 ty, src, dst;
      ty  = (randomU4() >> 13) % 5;
     tryagain:
      src = (randomU4() >>  1) % N_BYTES;
      dst = (randomU4() >>  3) % N_BYTES;
      switch (ty) {
         case 0: { // U1
            *(U1*)(arr+dst) = *(U1*)(arr+src);
	    mv1++;
            break;
         }
         case 1: { // U2
            if (src+2 >= N_BYTES || dst+2 >= N_BYTES) 
               goto tryagain;
            *(U2*)(arr+dst) = *(U2*)(arr+src);
	    mv2++;
            break;
         }
         case 2: { // U4
            if (src+4 >= N_BYTES || dst+4 >= N_BYTES) 
               goto tryagain;
            *(U4*)(arr+dst) = *(U4*)(arr+src);
	    mv4++;
            break;
         }
         case 3: { // U8
            if (src+8 >= N_BYTES || dst+8 >= N_BYTES) 
               goto tryagain;
            *(U8*)(arr+dst) = *(U8*)(arr+src);
	    mv8++;
            break;
         }
         /* Don't bother with 32-bit floats.  These cause
            horrible complications, as discussed in sh-mem.c. */
         /*
         case 4: { // F4
            if (src+4 >= N_BYTES || dst+4 >= N_BYTES) 
               goto tryagain;
            *(F4*)(arr+dst) = *(F4*)(arr+src);
	    mv4f++;
            break;
         }
         */
         case 4: { // F8
            if (src+8 >= N_BYTES || dst+8 >= N_BYTES) 
               goto tryagain;
#if defined(__i386__)
	    /* Copying via an x87 register causes the test to fail,
               because (I think) some obscure values that are FP
               denormals get changed during the copy due to the FPU
               normalising, or rounding, or whatever, them.  This
               causes them to no longer bit-for-bit match the
               accompanying metadata.  Yet we still need to do a
               genuine 8-byte load/store to test the relevant memcheck
               {LOADV8,STOREV8} routines.  Hence use the MMX registers
               instead, as copying through them should be
               straightforward.. */
            __asm__ __volatile__(
               "movq (%1), %%mm2\n\t"
               "movq %%mm2, (%0)\n\t"
               "emms"
               : : "r"(arr+dst), "r"(arr+src) : "memory"
            );
#else
            /* Straightforward.  On amd64, this gives a load/store of
               the bottom half of an xmm register.  On ppc32/64 this
               is a straighforward load/store of an FP register. */
            *(F8*)(arr+dst) = *(F8*)(arr+src);
#endif
	    mv8f++;
            break;
         }
         default:
	   fprintf(stderr, "sh-mem-random: bad size\n");
	   exit(0);
      }
   }

   printf("final check\n");
   check(arr, N_BYTES, "final check");

   printf("counts 1/2/4/8/F4/F8: %d %d %d %d %d %d\n", 
          mv1, mv2, mv4, mv8, mv4f, mv8f);
}



int main(void)
{
   U1* arr;

   if (0 == RUNNING_ON_VALGRIND) {
      fprintf(stderr, "error: this program only works when run under Valgrind\n");
      exit(1);
   }

   printf("-------- testing non-auxmap range --------\n");

   arr = malloc(N_BYTES);
   assert(arr);
   do_test_at(arr);
   free(arr);

   if (sizeof(void*) == 8) {
      // 64-bit platform.
      int tries;
      int nbytes_p;
      // (U1*)(UWord)constULL funny casting to keep gcc quiet on
      // 32-bit platforms
      U1* huge_addr = (U1*)(UWord)0x6600000000ULL;  // 408GB
      // Note, kernel 2.6.? on Athlon64 refuses fixed mmap requests
      // at above 512GB.

      printf("-------- testing auxmap range --------\n");

      nbytes_p = (N_BYTES + PAGE_SIZE) & ~(PAGE_SIZE-1);

      for (tries = 0; tries < 10; tries++) {
         arr = mmap(huge_addr, nbytes_p, PROT_READ|PROT_WRITE, 
                    MAP_FIXED|MAP_PRIVATE|MAP_ANONYMOUS, -1,0);
	 if (arr != MAP_FAILED)
            break;
	 // hmm. fudge the address and try again.
         huge_addr += (randomU4() & ~(PAGE_SIZE-1));
      }

      if (tries >= 10) {
	   fprintf(stderr, "sh-mem-random: can't mmap hi-mem\n");
	   exit(0);
      }
      assert(arr != MAP_FAILED);

      do_test_at(arr);
   }

   return 0;

}
