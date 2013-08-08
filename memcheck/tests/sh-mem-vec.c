
// Tests shadow memory correctness for 16-byte vector loads/stores

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "tests/malloc.h"
#include "memcheck/memcheck.h"

// What we're actually testing
static __attribute__((noinline))
void vector16_copy ( void* dst, void* src )
{
  __asm__ __volatile__(
     "movups (%1), %%xmm7 ; movups %%xmm7, (%0)"
     : /*OUT*/ : /*IN*/ "r"(dst), "r"(src) : "memory","xmm7" 
  );
}

// All the sizes here are in *bytes*, not bits.

typedef unsigned char        U1;
typedef unsigned short       U2;
typedef unsigned int         U4;
typedef unsigned long long   U8;
typedef unsigned long int    UWord;

typedef unsigned char        Bool;
#define  True   ((Bool)1)
#define  False  ((Bool)0)

#define CFENCE __asm__ __volatile__("":::"cc","memory")


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

#define N_BYTES  80000
#define N_EVENTS (N_BYTES * 2)

// Return x, but with its definedness bits set to be its own value bits
static inline U1 self_shadow ( U1 x )
{
   U1 res = 0xFF;
   VALGRIND_MAKE_MEM_UNDEFINED(&res, 1);
   res &= x;
   return res;
}

static inline U1 get_shadow ( U1 x )
{
   U1 res = 0;
   U4 r = VALGRIND_GET_VBITS(&x, &res, 1);
   assert(r == 1 || r == 0);
   return res;
}

static inline U1 make_def ( U1 x )
{
   U1 y = x;
   VALGRIND_MAKE_MEM_DEFINED(&y, 1);
   return y;
}

static inline U1 make_undef ( U1 x )
{
   U1 y = x;
   VALGRIND_MAKE_MEM_UNDEFINED(&y, 1);
   return y;
}

static void make_noaccess ( U1* dst )
{
   VALGRIND_MAKE_MEM_NOACCESS(dst, 1);
}

static void apply ( void(*fn)(U4,Bool), U4 arg1, Bool arg2 )
{
   switch (arg1 & (32-1)) {
      case 0: CFENCE; fn(arg1, arg2); CFENCE; break;
      case 1: CFENCE; fn(arg1, arg2); CFENCE; break;
      case 2: CFENCE; fn(arg1, arg2); CFENCE; break;
      case 3: CFENCE; fn(arg1, arg2); CFENCE; break;
      case 4: CFENCE; fn(arg1, arg2); CFENCE; break;
      case 5: CFENCE; fn(arg1, arg2); CFENCE; break;
      case 6: CFENCE; fn(arg1, arg2); CFENCE; break;
      case 7: CFENCE; fn(arg1, arg2); CFENCE; break;
      case 8: CFENCE; fn(arg1, arg2); CFENCE; break;
      case 9: CFENCE; fn(arg1, arg2); CFENCE; break;
      case 10: CFENCE; fn(arg1, arg2); CFENCE; break;
      case 11: CFENCE; fn(arg1, arg2); CFENCE; break;
      case 12: CFENCE; fn(arg1, arg2); CFENCE; break;
      case 13: CFENCE; fn(arg1, arg2); CFENCE; break;
      case 14: CFENCE; fn(arg1, arg2); CFENCE; break;
      case 15: CFENCE; fn(arg1, arg2); CFENCE; break;
      case 16: CFENCE; fn(arg1, arg2); CFENCE; break;
      case 17: CFENCE; fn(arg1, arg2); CFENCE; break;
      case 18: CFENCE; fn(arg1, arg2); CFENCE; break;
      case 19: CFENCE; fn(arg1, arg2); CFENCE; break;
      case 20: CFENCE; fn(arg1, arg2); CFENCE; break;
      case 21: CFENCE; fn(arg1, arg2); CFENCE; break;
      case 22: CFENCE; fn(arg1, arg2); CFENCE; break;
      case 23: CFENCE; fn(arg1, arg2); CFENCE; break;
      case 24: CFENCE; fn(arg1, arg2); CFENCE; break;
      case 25: CFENCE; fn(arg1, arg2); CFENCE; break;
      case 26: CFENCE; fn(arg1, arg2); CFENCE; break;
      case 27: CFENCE; fn(arg1, arg2); CFENCE; break;
      case 28: CFENCE; fn(arg1, arg2); CFENCE; break;
      case 29: CFENCE; fn(arg1, arg2); CFENCE; break;
      case 30: CFENCE; fn(arg1, arg2); CFENCE; break;
      case 31: CFENCE; fn(arg1, arg2); CFENCE; break;
      default: CFENCE; fn(arg1, arg2); CFENCE; break;
   }
}

  // Try doing some partial-loads-ok/not-ok testing.
  /* Test cases:
     - load, 16-aligned, all no-access 
         ==> addr err
     - load, 16-aligned, 1 to 15 initial bytes accessible,
             then at least one unaccessible byte,
             then remaining bytes in any state.
         ==> if PLO then no error, but returned V bits are undefined
                for unaccessible bytes
             else
                error; and V bits are defined for unaccessible bytes

     All of the above, but non-16-aligned:
        -- all return an addressing error
  */

static void do_partial_load_case ( U4 nInitialValid, Bool aligned )
{
     fprintf(stderr,
       "------ PL %s case with %u leading acc+def bytes ------\n\n",
             aligned ? "Aligned" : "Unaligned", nInitialValid);

     U1* block = memalign16(64);
     U4 j;
     for (j = 0; j < 64; j++) block[j] = 0;

     if (!aligned) block++;

     // Make the block have this pattern:
     // block[0 .. i-1]  accessible and defined
     // block[i .. 15]   repeating NOACCESS, UNDEF, DEF
     // hence block[i], at the very least, is always NOACCESS
     U4 i = nInitialValid;
     for (j = i; j < 16; j++) {
        switch ((j-i) % 3) {
           case 0: make_noaccess(&block[j]); break;
           case 1: block[j] = make_undef(block[j]); break;
           case 2: /* already acc and def */ break;
        }
     }

     // Do the access, possibly generating an error, and show the
     // resulting V bits
     U1 dst[16];
     vector16_copy(&dst[0], block);

     U1 dst_vbits[16];
     U4 r = VALGRIND_GET_VBITS(&dst[0], &dst_vbits[0], 16);
     assert(r == 1 || r == 0);

     fprintf(stderr, "\n");
     for (j = 0; j < 16; j++) {
        fprintf(stderr, "%c", dst_vbits[j] == 0 ? 'd'
                              : dst_vbits[j] == 0xFF ? 'U' : '?');
     }
     fprintf(stderr, "\n\n");

     // Also let's use the resulting value, to check we get an undef
     // error
     U1 sum = 0;
     for (j = 0; j < 16; j++)
        sum ^= dst[j];

     if (sum == 42) {
        CFENCE; fprintf(stderr, "%s", ""); CFENCE;
     } else {
        CFENCE; fprintf(stderr, "%s", ""); CFENCE;
     }

     fprintf(stderr, "\n");

     if (!aligned) block--;
     free(block);
}

int main ( void )
{
  U4 i;
  U1* buf = memalign16(N_BYTES);

  // Fill |buf| with bytes, so that zero bits have a zero shadow
  // (are defined) and one bits have a one shadow (are undefined)
  for (i = 0; i < N_BYTES/2; i++) {
     buf[i] = self_shadow( (i & (1<<5)) ? 0x00 : 0xFF );
  }
  for (     ;  i < N_BYTES; i++) {
     buf[i] = self_shadow( randomU1() );
  }

  // Randomly copy the data around.  Once every 8 srcs/dsts, force
  // the src or dst to be aligned.  Once every 64, force both to be
  // aligned.  So as to give the fast (aligned) paths some checking.
  const U4 n_copies = N_EVENTS;
  U4 n_d_aligned = 0;
  U4 n_s_aligned = 0;
  U4 n_both_aligned = 0;
  U4 n_fails = 0;

  for (i = 0; i < n_copies; i++) {
     U4 si = randomU4() % (N_BYTES-16);
     U4 di = randomU4() % (N_BYTES-16);
     if (0 == (randomU1() & 7)) si &= ~(16-1);
     if (0 == (randomU1() & 7)) di &= ~(16-1);
     if (0 == (randomU1() & 63)) { di &= ~(16-1); si &= ~(16-1); }

     void* dst = &buf[di];
     void* src = &buf[si];

     if (0 == (((UWord)src) & (16-1))) n_s_aligned++;
     if (0 == (((UWord)dst) & (16-1))) n_d_aligned++;
     if (0 == (((UWord)src) & (16-1)) && 0 == (((UWord)dst) & (16-1)))
       n_both_aligned++;

     vector16_copy(dst, src);
  }

  U4 freq[256];
  for (i = 0; i < 256; i++)
     freq[i] = 0;

  for (i = 0; i < N_BYTES; i++) {
     //if (i > 0 && 0 == (i & 0x0F)) fprintf(stderr, "\n");
     U1 v_actual = make_def(buf[i]);
     U1 v_shadow = get_shadow(buf[i]);
     if (v_actual != v_shadow) n_fails++;
     //fprintf(stderr, "%02x:%02x ", (U4)v_actual, (U4)v_shadow);
     freq[(U4)v_actual]++;
  }

  fprintf(stderr, "\n");
  U4 totFreq = 0;
  for (i = 0; i < 256; i++) {
     totFreq += freq[i];
     if (i > 0 && (0 == (i % 16))) fprintf(stderr, "\n");
     fprintf(stderr, "%5u ", freq[i]);
  }
  assert(totFreq == N_BYTES);

  fprintf(stderr, "\n\n");
  fprintf(stderr, "%u copies, %u d_aligned, %u s_aligned, %u both_aligned\n",
         n_copies, n_d_aligned, n_s_aligned, n_both_aligned);
  fprintf(stderr, "%u %s\n", n_fails, n_fails == 0 ? "failures" : "FAILURES");

  // Check that we can detect underruns of the block.
  fprintf(stderr, "\nExpect 2 x no error\n" );
  vector16_copy( &buf[100], &buf[0] );
  vector16_copy( &buf[0],   &buf[100] );

  fprintf(stderr, "\nExpect 2 x error\n\n" );
  vector16_copy( &buf[100], &buf[-1]  ); // invalid rd
  vector16_copy( &buf[-1],  &buf[100] ); // invalid wr

  // and overruns ..
  fprintf(stderr, "\nExpect 2 x no error\n" );
  vector16_copy( &buf[200],            &buf[N_BYTES-16 + 0] );
  vector16_copy( &buf[N_BYTES-16 + 0], &buf[200]            );

  fprintf(stderr, "\nExpect 2 x error\n\n" );
  vector16_copy( &buf[200],            &buf[N_BYTES-16 + 1] );
  vector16_copy( &buf[N_BYTES-16 + 1], &buf[200]            );

  free(buf);
  fprintf(stderr, "\n");

  for (i = 0; i < 16; i++)
     apply( do_partial_load_case, i, True/*aligned*/ );

  for (i = 0; i < 16; i++)
     apply( do_partial_load_case, i, False/*not aligned*/ );

  return 0;
}

// This is a version of sh-mem.c that works with vector types.
// It's too difficult to roll that into the macro magic in 
// sh-mem.c, hence the separate program.

// This program is a thorough test of the LOADVn/STOREVn shadow memory
// operations.

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "memcheck/memcheck.h"

#if defined(__APPLE__) && defined(__i386__)
#  define PLAT_x86_darwin 1
#elif defined(__APPLE__) && defined(__x86_64__)
#  define PLAT_amd64_darwin 1
#elif defined(__MINGW32__) || defined(__CYGWIN32__) \
      || (defined(_WIN32) && defined(_M_IX86))
#  define PLAT_x86_win32 1
#elif defined(__linux__) && defined(__i386__)
#  define PLAT_x86_linux 1
#elif defined(__linux__) && defined(__x86_64__)
#  define PLAT_amd64_linux 1
#elif defined(__linux__) && defined(__powerpc__) && !defined(__powerpc64__)
#  define PLAT_ppc32_linux 1
#elif defined(__linux__) && defined(__powerpc__) && defined(__powerpc64__)
#  define PLAT_ppc64_linux 1
#elif defined(__linux__) && defined(__arm__)
#  define PLAT_arm_linux 1
#elif defined(__linux__) && defined(__s390__) && defined(__s390x__)
#  define PLAT_s390x_linux 1
#elif defined(__linux__) && defined(__mips__) && (__mips == 64)
#  define PLAT_mips64_linux 1
#elif defined(__linux__) && defined(__mips__) && !(__mips == 64)
#  define PLAT_mips32_linux 1
#endif

// All the sizes here are in *bytes*, not bits.

typedef unsigned char        U1;
typedef unsigned short       U2;
typedef unsigned int         U4;
typedef unsigned long long   U8;

typedef float                F4;
typedef double               F8;

typedef unsigned char        V16 __attribute__((vector_size (16)));

typedef unsigned char Bool;
#define False ((Bool)0)
#define True  ((Bool)1)

#define SZB_OF_a   128

// a[] is the array in which we do our loads and stores.  
// b[] is another one in which we do some copying.
U8 a [SZB_OF_a / 8];    // Type is U8 to ensure it's 8-aligned
U8 b [SZB_OF_a / 8];    // same size as a[]

// XXX: should check the error cases for SET/GET_VBITS also

static Bool eq_V16 ( V16 x, V16 y ) {
   return 0 == memcmp(&x, &y, sizeof(x));
}

__attribute__((noinline))
static void copy_V16 ( volatile V16* dst, volatile V16* src ) 
{
#  if defined(PLAT_amd64_linux) || defined(PLAT_amd64_darwin)
   __asm__ __volatile__( "movupd (%0),%%xmm0 ; movupd %%xmm0,(%1)"
                         : : "r"(src), "r"(dst) : "memory","xmm0");
#  else
   *dst = *src;
#  endif
}

// For the byte 'x', build a value of 'size' bytes from that byte, eg:
//   size  1 --> x
//   size  2 --> xx
//   size  4 --> xxxx
//   size  8 --> xxxxxxxx
//   size 16 --> xxxxxxxx'xxxxxxxx
// where the 0 bits are seen by Memcheck as defined, and the 1 bits are
// seen as undefined (ie. the value of each bit matches its V bit, ie. the
// resulting value is the same as its metavalue).
//
V16 build(int size, U1 byte)
{
   int i;
   V16 mask; memset(&mask, 0, sizeof(mask));
   V16 shres;
   V16 res; memset(&res, 0xFF, sizeof(res));
   V16 res2;
   VALGRIND_MAKE_MEM_UNDEFINED(&res, sizeof(res));
   assert(16 == size);

   for (i = 0; i < size; i++) {
      mask <<= 8;
      mask |= (U8)byte;
   }

   res &= mask;      
   
   // res is now considered partially defined, but we know exactly what its
   // value is (it happens to be the same as its metavalue).
   
   (void)VALGRIND_GET_VBITS(&res, &shres, sizeof(res));
   res2 = res;
   // avoid the 'undefined' warning
   (void)VALGRIND_MAKE_MEM_DEFINED(&res2, sizeof(res2));
   assert(eq_V16(res2, shres));

   return res;
}

// Check that all the bytes in a[x..y-1] have their V byte equal 
// to 'expected_byte''.
// 'str' and 'offset' are only used for printing an error message if
// something goes wrong.
void check_all(U4 x, U4 y, U1 expected_byte,
                           char* str, int offset)
{
   U1 sh[SZB_OF_a];     // Used for getting a[]'s V bits
   int i;
   assert(x < y);
   assert(y <= SZB_OF_a);
   assert(x < SZB_OF_a);

   (void)VALGRIND_GET_VBITS(a, sh, sizeof(a));
   for (i = x; i < y; i++) {
      if ( expected_byte != sh[i] ) {
         fprintf(stderr, "\n\nFAILURE: %s, offset %d, byte %d -- "
                         "is 0x%x, should be 0x%x\n\n",
                         str, offset, i, sh[i], expected_byte);
         exit(1);
      }
   }
}

int main(void)
{
   int h, i, j;
   U1 *undefA, expected_byte;

   if (0 == RUNNING_ON_VALGRIND) {
      fprintf(stderr,
              "error: this program only works when run under Valgrind\n");
      exit(1);
   }

   // Check a[] has the expected alignment, and that it's not too high in
   // the address space (which would trigger the slow cases in
   // LOADVn/STOREVn) on 64-bit platforms).
   assert( 0 == (long)a % 8);
   if (sizeof(void*) == 8) {
      assert( ((U1*)(&a[0])) < ((U1*)(32ULL * 1024*1024*1024)/*32G*/) );
   }

   // Check basic types have the expected sizes.
   assert(1 == sizeof(U1));
   assert(2 == sizeof(U2));
   assert(4 == sizeof(U4));
   assert(8 == sizeof(U8));
   assert(16 == sizeof(V16));

   // Create an array of values that has all the possible V bit metavalues.
   // Because 0 represents a defined bit, and because undefA[] is initially
   // zeroed, we have the nice property that:
   //
   //    i == undefA[i] == V_bits_of(undefA[i])
   //
   // which is useful for testing below.
   undefA = calloc(1, 256);         // one for each possible undefinedness value
   VALGRIND_MAKE_MEM_UNDEFINED(undefA, 256);
   for (i = 0; i < 256; i++) {
      undefA[i] &= i; 
   }

   // This code does a whole lot of reads and writes of a particular size
   // (NNN = 16 or 32), with varying alignments, of values with
   // different not/partially/fully defined metavalues, and checks that the
   // V bits are set in a[] as expected using GET_VBITS.
   //
   // 'Ty' is the type of the thing we are copying.  It can be an integer
   // type or an FP type.  'ITy' is the same-sized integer type (and thus
   // will be the same as 'Ty' if 'ITy' is an integer type).  'ITy' is used
   // when doing shifting/masking and stuff like that.

#define Ty V16
#define NNN 16
#define VEC_EQ eq_V16

   fprintf(stderr, "-- NNN: %d %s ------------------------\n", 
           NNN, "V16"); 
   /* For all of the alignments from (0..NNN-1), eg. if NNN==16, we do */ 
   /* alignments of 0, 1, 2 .. 15. */ 
   for (h = 0; h < NNN; h++) { 
      
      size_t n  = sizeof(a); 
      size_t nN = n / sizeof(Ty); 
      volatile Ty* aN    = (Ty*)a; 
      volatile Ty* bN    = (Ty*)b; 
      volatile Ty* aNb   = (Ty*)(((U1*)aN) + h); /* set offset from a[] */ 
      volatile Ty* bNb   = (Ty*)(((U1*)bN) + h); /* set offset from b[] */ 
      
      fprintf(stderr, "h = %d (checking %d..%d)   ", h, h, (int)(n-NNN+h)); 
      
      /* For each of the 256 possible V byte values... */ 
      for (j = 0; j < 256; j++) { 
         /* build the value for i (one of: i, ii, iiii, iiiiiiii) */ 
         V16 tmp        = build(NNN, j); 
         Ty  undefN_ITy = (Ty)tmp; 
         { /* This just checks that no overflow occurred when squeezing */ 
           /* the output of build() into a variable of type 'Ty'. */ 
            V16 tmpDef     = tmp; 
            Ty undefN_ITyDef = undefN_ITy; 
            VALGRIND_MAKE_MEM_DEFINED(&tmpDef,        16 ); 
            VALGRIND_MAKE_MEM_DEFINED(&undefN_ITyDef, NNN); 
            assert(VEC_EQ(tmpDef, undefN_ITyDef));          
         } 
         
         /* We have to use an array for undefN_Ty -- because if we try to
          * convert an integer type from build into an FP type with a
          * straight cast -- eg "float f = (float)i" -- the value gets
          * converted.  With this pointer/array nonsense the exact bit
          * pattern gets used as an FP value unchanged (that FP value is
          * undoubtedly nonsense, but that's not a problem here). */ 
         Ty* undefN_Ty = (Ty*)&undefN_ITy; 
         if (0 == j % 32) fprintf(stderr, "%d...", j); /* progress meter */ 
         fflush(stderr); fflush(stdout);
         
         expected_byte = j; 
         
         /* STOREVn.  Note that we use the first element of the undefN_Ty
          * array, as explained above. */ 
         for (i = 0; i < nN-1; i++) { copy_V16(&aNb[i], &undefN_Ty[0]); } 
         check_all(h, n-NNN+h, expected_byte, 
                   "STOREVn", h); 
         
         /* LOADVn -- by copying the values to one place and then back, 
          * we ensure that LOADVn gets exercised. */ 
         for (i = 0; i < nN-1; i++) { copy_V16(&bNb[i], &aNb[i]); } 
         for (i = 0; i < nN-1; i++) { copy_V16(&aNb[i], &bNb[i]); } 
         check_all(h, n-NNN+h, expected_byte, "LOADVn", h); 
      } 
      fprintf(stderr, "\n"); 
   }

   return 0;
}
