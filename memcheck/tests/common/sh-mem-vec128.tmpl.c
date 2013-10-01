
// Tests shadow memory correctness for 16-byte/32-byte/etc. vector
// loads/stores. Requires vector_copy() and VECTOR_BYTES to be
// specified somehow.

#ifndef VECTOR_BYTES
#error "VECTOR_BYTES must be defined"
#endif

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "tests/malloc.h"
#include "memcheck/memcheck.h"

// What we're actually testing
// .. is vector_copy, which should be defined before this point

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

static __attribute__((noinline)) const char* get_endianness ( void )
{
   volatile U4 w32 = 0x88776655;
   volatile U1* p = (U1*)&w32;
   if (p[0] == 0x55) {
      assert(p[3] == 0x88);
      return "little";
   }
   if (p[0] == 0x88) {
      assert(p[3] == 0x55);
      return "big";
   }
   assert(0);
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

#define N_BYTES  80000
#define N_EVENTS (N_BYTES * 2)

// Return x, but with its definedness bits set to be its own value bits
static inline U1 self_shadow ( U1 x )
{
   U1 res = 0xFF;
   (void) VALGRIND_MAKE_MEM_UNDEFINED(&res, 1);
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
   (void) VALGRIND_MAKE_MEM_DEFINED(&y, 1);
   return y;
}

static inline U1 make_undef ( U1 x )
{
   U1 y = x;
   (void) VALGRIND_MAKE_MEM_UNDEFINED(&y, 1);
   return y;
}

static void make_noaccess ( U1* dst )
{
  (void) VALGRIND_MAKE_MEM_NOACCESS(dst, 1);
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
     - load, aligned, all no-access
         ==> addr err
     - load, aligned, 1 to VECTOR_BYTES-1 initial bytes accessible,
             then at least one unaccessible byte,
             then remaining bytes in any state.
         ==> if PLO then no error, but returned V bits are undefined
                for unaccessible bytes
             else
                error; and V bits are defined for unaccessible bytes

     All of the above, but non-aligned:
        -- all return an addressing error
  */

static void do_partial_load_case ( U4 nInitialValid, Bool aligned )
{
     fprintf(stderr,
       "------ PL %s case with %u leading acc+def bytes ------\n\n",
             aligned ? "Aligned" : "Unaligned", nInitialValid);

     void *temp;
     if (posix_memalign(&temp, VECTOR_BYTES, 64) != 0)
         abort();
     U1* block = temp;
     U4 j;
     for (j = 0; j < 64; j++) block[j] = 0;

     if (!aligned) block++;

     // Make the block have this pattern:
     // block[0 .. i-1]  accessible and defined
     // block[i .. VECTOR_BYTES-1]   repeating NOACCESS, UNDEF, DEF
     // hence block[i], at the very least, is always NOACCESS
     U4 i = nInitialValid;
     for (j = i; j < VECTOR_BYTES; j++) {
        switch ((j-i) % 3) {
           case 0: make_noaccess(&block[j]); break;
           case 1: block[j] = make_undef(block[j]); break;
           case 2: /* already acc and def */ break;
        }
     }

     // Do the access, possibly generating an error, and show the
     // resulting V bits
     U1 dst[VECTOR_BYTES];
     vector_copy(&dst[0], block);

     U1 dst_vbits[VECTOR_BYTES];
     U4 r = VALGRIND_GET_VBITS(&dst[0], &dst_vbits[0], VECTOR_BYTES);
     assert(r == 1 || r == 0);

     fprintf(stderr, "\n");
     for (j = 0; j < VECTOR_BYTES; j++) {
        fprintf(stderr, "%c", dst_vbits[j] == 0 ? 'd'
                              : dst_vbits[j] == 0xFF ? 'U' : '?');
     }
     fprintf(stderr, "\n\n");

     // Also let's use the resulting value, to check we get an undef
     // error
     U1 sum = 0;
     for (j = 0; j < VECTOR_BYTES; j++)
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
  fprintf(stderr, "sh-mem-vec%d: config: %s-endian, %d-bit word size\n",
          VECTOR_BYTES * 8, get_endianness(), (int)(8 * sizeof(void*)));

  U4 i;
  void *temp;
  if (posix_memalign(&temp, VECTOR_BYTES, N_BYTES) != 0)
      abort();
  U1* buf = temp;

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
     U4 si = randomU4() % (N_BYTES-VECTOR_BYTES);
     U4 di = randomU4() % (N_BYTES-VECTOR_BYTES);
     if (0 == (randomU1() & 7)) si &= ~(VECTOR_BYTES-1);
     if (0 == (randomU1() & 7)) di &= ~(VECTOR_BYTES-1);
     if (0 == (randomU1() & 63)) { di &= ~(VECTOR_BYTES-1); si &= ~(VECTOR_BYTES-1); }

     void* dst = &buf[di];
     void* src = &buf[si];

     if (0 == (((UWord)src) & (VECTOR_BYTES-1))) n_s_aligned++;
     if (0 == (((UWord)dst) & (VECTOR_BYTES-1))) n_d_aligned++;
     if (0 == (((UWord)src) & (VECTOR_BYTES-1)) && 0 == (((UWord)dst) & (VECTOR_BYTES-1)))
       n_both_aligned++;

     vector_copy(dst, src);
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
  vector_copy( &buf[100], &buf[0] );
  vector_copy( &buf[0],   &buf[100] );

  fprintf(stderr, "\nExpect 2 x error\n\n" );
  vector_copy( &buf[100], &buf[-1]  ); // invalid rd
  vector_copy( &buf[-1],  &buf[100] ); // invalid wr

  // and overruns ..
  fprintf(stderr, "\nExpect 2 x no error\n" );
  vector_copy( &buf[200],            &buf[N_BYTES-VECTOR_BYTES + 0] );
  vector_copy( &buf[N_BYTES-VECTOR_BYTES + 0], &buf[200]            );

  fprintf(stderr, "\nExpect 2 x error\n\n" );
  vector_copy( &buf[200],            &buf[N_BYTES-VECTOR_BYTES + 1] );
  vector_copy( &buf[N_BYTES-VECTOR_BYTES + 1], &buf[200]            );

  free(buf);
  fprintf(stderr, "\n");

  for (i = 0; i < VECTOR_BYTES; i++)
     apply( do_partial_load_case, i, True/*aligned*/ );

  for (i = 0; i < VECTOR_BYTES; i++)
     apply( do_partial_load_case, i, False/*not aligned*/ );

  return 0;
}
