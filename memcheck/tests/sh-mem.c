// This program is a thorough test of the LOADVn/STOREVn shadow memory
// operations.

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "memcheck/memcheck.h"

// All the sizes here are in *bytes*, not bits.

typedef unsigned char        U1;
typedef unsigned short       U2;
typedef unsigned int         U4;
typedef unsigned long long   U8;

typedef float                F4;
typedef double               F8;

#define SZB_OF_a    64

// a[] is the array in which we do our loads and stores.  
// b[] is another one in which we do some copying.
U8 a [SZB_OF_a / 8];    // Type is U8 to ensure it's 8-aligned
U8 b [SZB_OF_a / 8];    // same size as a[]

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
   (void)VALGRIND_MAKE_MEM_UNDEFINED(&res, 8);
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

// Check that all the bytes in a[x..y-1] have their V byte equal 
// to either 'expected_byte' or 'expected_byte_alt'.
// 'str' and 'offset' are only used for printing an error message if
// something goes wrong.
void check_all(U4 x, U4 y, U1 expected_byte, U1 expected_byte_alt, 
                           char* str, int offset)
{
   U1 sh[SZB_OF_a];     // Used for getting a[]'s V bits
   int i;

   (void)VALGRIND_GET_VBITS(a, sh, sizeof(a));
   for (i = x; i < y; i++) {
      if ( expected_byte != sh[i] && expected_byte_alt != sh[i] ) {
         fprintf(stderr, "\n\nFAILURE: %s, offset %d, byte %d -- "
                         "is 0x%x, should be 0x%x or 0x%x\n\n",
                         str, offset, i, sh[i], expected_byte, 
                         expected_byte_alt);
         exit(1);
      }
   }
}

int main(void)
{
   int h, i, j;
   U1 *undefA, expected_byte, expected_byte_alt;

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

   // Create an array of values that has all the possible V bit metavalues.
   // Because 0 represents a defined bit, and because undefA[] is initially
   // zeroed, we have the nice property that:
   //
   //    i == undefA[i] == V_bits_of(undefA[i])
   //
   // which is useful for testing below.
   undefA = calloc(1, 256);         // one for each possible undefinedness value
   (void)VALGRIND_MAKE_MEM_UNDEFINED(undefA, 256);
   for (i = 0; i < 256; i++) {
      undefA[i] &= i; 
   }

   // This code does a whole lot of reads and writes of a particular size
   // (NNN = 1, 2, 4 or 8), with varying alignments, of values with
   // different not/partially/fully defined metavalues, and checks that the
   // V bits are set in a[] as expected using GET_VBITS.
   //
   // 'Ty' is the type of the thing we are copying.  It can be an integer
   // type or an FP type.  'ITy' is the same-sized integer type (and thus
   // will be the same as 'Ty' if 'ITy' is an integer type).  'ITy' is used
   // when doing shifting/masking and stuff like that.

#define DO(NNN, Ty, ITy, isF4) \
   fprintf(stderr, "-- NNN: %d %s %s ------------------------\n", \
           NNN, #Ty, #ITy); \
   /* For all of the alignments from (0..NNN-1), eg. if NNN==4, we do */ \
   /* alignments of 0, 1, 2, 3. */ \
   for (h = 0; h < NNN; h++) { \
      \
      size_t n  = sizeof(a); \
      size_t nN = n / sizeof(Ty); \
      Ty* aN    = (Ty*)a; \
      Ty* bN    = (Ty*)b; \
      Ty* aNb   = (Ty*)(((U1*)aN) + h); /* set offset from a[] */ \
      Ty* bNb   = (Ty*)(((U1*)bN) + h); /* set offset from b[] */ \
      \
      fprintf(stderr, "h = %d (checking %d..%d)   ", h, h, (int)(n-NNN+h)); \
      \
      /* For each of the 256 possible V byte values... */ \
      for (j = 0; j < 256; j++) { \
         /* build the value for i (one of: i, ii, iiii, iiiiiiii) */ \
         U8  tmp        = build(NNN, j); \
         ITy undefN_ITy = (ITy)tmp; \
         Ty* undefN_Ty; \
         { /* This just checks that no overflow occurred when squeezing */ \
           /* the output of build() into a variable of type 'Ty'. */ \
            U8  tmpDef     = tmp; \
            ITy undefN_ITyDef = undefN_ITy; \
            (void)VALGRIND_MAKE_MEM_DEFINED(&tmpDef,        8  );       \
            (void)VALGRIND_MAKE_MEM_DEFINED(&undefN_ITyDef, NNN);       \
            assert(tmpDef == (U8)undefN_ITyDef); \
         } \
         \
         /* We have to use an array for undefN_Ty -- because if we try to
          * convert an integer type from build into an FP type with a
          * straight cast -- eg "float f = (float)i" -- the value gets
          * converted.  With this pointer/array nonsense the exact bit
          * pattern gets used as an FP value unchanged (that FP value is
          * undoubtedly nonsense, but that's not a problem here). */ \
         undefN_Ty = (Ty*)&undefN_ITy; \
         if (0 == j % 32) fprintf(stderr, "%d...", j); /* progress meter */ \
         \
         /* A nasty exception: most machines so far (x86/PPC32/PPC64)
          * don't have 32-bit floats.  So 32-bit floats get cast to 64-bit
          * floats.  Memcheck does a PCast in this case, which means that if
          * any V bits for the 32-bit float are undefined (ie. 0 != j), all
          * the V bits in the 64-bit float are undefined.  So account for
          * this when checking.  AMD64 typically does FP arithmetic on
          * SSE, effectively giving it access to 32-bit FP registers.  So
          * in short, for floats, we have to allow either 'j' or 0xFF
          * as an acceptable result.  Sigh. */ \
         if (isF4) { \
            expected_byte = j; \
            expected_byte_alt = 0 != j ? 0xFF : j; \
         } else { \
            expected_byte = j; \
            expected_byte_alt = j; \
         } \
         \
         /* STOREVn.  Note that we use the first element of the undefN_Ty
          * array, as explained above. */ \
         for (i = 0; i < nN-1; i++) { aNb[i] = undefN_Ty[0]; } \
         check_all(h, n-NNN+h, expected_byte, expected_byte_alt, \
                   "STOREVn", h); \
         \
         /* LOADVn -- by copying the values to one place and then back, 
          * we ensure that LOADVn gets exercised. */ \
         for (i = 0; i < nN-1; i++) { bNb[i] = aNb[i]; } \
         for (i = 0; i < nN-1; i++) { aNb[i] = bNb[i]; } \
         check_all(h, n-NNN+h, expected_byte, expected_byte_alt, "LOADVn", h); \
      } \
      fprintf(stderr, "\n"); \
   }

   // For sizes 4 and 8 we do both integer and floating-point types.  The
   // reason being that on 32-bit machines just using integer types never
   // exercises LOADV8/STOREV8 -- for integer types these loads/stores get
   // broken into two 32-bit loads/stores.
   DO(1, U1, U1, /*isF4*/0);
   DO(2, U2, U2, /*isF4*/0);
   DO(4, U4, U4, /*isF4*/0);
   DO(4, F4, U4, /*isF4*/1);
   DO(8, U8, U8, /*isF4*/0);
   DO(8, F8, U8, /*isF4*/0);
   
   return 0;
}
