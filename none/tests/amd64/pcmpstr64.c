
/* Tests in detail the core arithmetic for pcmp{e,i}str{i,m} using
   pcmpistri to drive it.  Does not check the e-vs-i or i-vs-m
   aspect. */

#include <string.h>
#include <stdio.h>
#include <assert.h>

typedef  unsigned int   UInt;
typedef  signed int     Int;
typedef  unsigned char  UChar;
typedef  unsigned long long int ULong;
typedef  UChar          Bool;
#define False ((Bool)0)
#define True  ((Bool)1)

//typedef  unsigned char  V128[16];
typedef
   union {
      UChar uChar[16];
      UInt  uInt[4];
   }
   V128;

#define SHIFT_O   11
#define SHIFT_S   7
#define SHIFT_Z   6
#define SHIFT_A   4
#define SHIFT_C   0
#define SHIFT_P   2

#define MASK_O    (1ULL << SHIFT_O)
#define MASK_S    (1ULL << SHIFT_S)
#define MASK_Z    (1ULL << SHIFT_Z)
#define MASK_A    (1ULL << SHIFT_A)
#define MASK_C    (1ULL << SHIFT_C)
#define MASK_P    (1ULL << SHIFT_P)


UInt clz32 ( UInt x )
{
   Int y, m, n;
   y = -(x >> 16);
   m = (y >> 16) & 16;
   n = 16 - m;
   x = x >> m;
   y = x - 0x100;
   m = (y >> 16) & 8;
   n = n + m;
   x = x << m;
   y = x - 0x1000;
   m = (y >> 16) & 4;
   n = n + m;
   x = x << m;
   y = x - 0x4000;
   m = (y >> 16) & 2;
   n = n + m;
   x = x << m;
   y = x >> 14;
   m = y & ~(y >> 1);
   return n + 2 - m;
}

UInt ctz32 ( UInt x )
{
   return 32 - clz32((~x) & (x-1));
}

void expand ( V128* dst, char* summary )
{
   Int i;
   assert( strlen(summary) == 16 );
   for (i = 0; i < 16; i++) {
      UChar xx = 0;
      UChar x = summary[15-i];
      if      (x >= '0' && x <= '9') { xx = x - '0'; }
      else if (x >= 'A' && x <= 'F') { xx = x - 'A' + 10; }
      else if (x >= 'a' && x <= 'f') { xx = x - 'a' + 10; }
      else assert(0);

      assert(xx < 16);
      xx = (xx << 4) | xx;
      assert(xx < 256);
      dst->uChar[i] = xx;
   }
}

void try_istri ( char* which,
                 UInt(*h_fn)(V128*,V128*),
                 UInt(*s_fn)(V128*,V128*),
                 char* summL, char* summR )
{
   assert(strlen(which) == 2);
   V128 argL, argR;
   expand(&argL, summL);
   expand(&argR, summR);
   UInt h_res = h_fn(&argL, &argR);
   UInt s_res = s_fn(&argL, &argR);
   printf("istri %s  %s %s -> %08x %08x %s\n",
          which, summL, summR, h_res, s_res, h_res == s_res ? "" : "!!!!");
}

UInt zmask_from_V128 ( V128* arg )
{
   UInt i, res = 0;
   for (i = 0; i < 16; i++) {
      res |=  ((arg->uChar[i] == 0) ? 1 : 0) << i;
   }
   return res;
}

//////////////////////////////////////////////////////////
//                                                      //
//                       GENERAL                        //
//                                                      //
//////////////////////////////////////////////////////////


/* Given partial results from a pcmpXstrX operation (intRes1,
   basically), generate an I format (index value for ECX) output, and
   also the new OSZACP flags.
*/
static
void pcmpXstrX_WRK_gen_output_fmt_I(/*OUT*/V128* resV,
                                    /*OUT*/UInt* resOSZACP,
                                    UInt intRes1,
                                    UInt zmaskL, UInt zmaskR,
                                    UInt validL,
                                    UInt pol, UInt idx )
{
   assert((pol >> 2) == 0);
   assert((idx >> 1) == 0);

   UInt intRes2 = 0;
   switch (pol) {
      case 0: intRes2 = intRes1;          break; // pol +
      case 1: intRes2 = ~intRes1;         break; // pol -
      case 2: intRes2 = intRes1;          break; // pol m+
      case 3: intRes2 = intRes1 ^ validL; break; // pol m-
   }
   intRes2 &= 0xFFFF;

   // generate ecx value
   UInt newECX = 0;
   if (idx) {
     // index of ms-1-bit
     newECX = intRes2 == 0 ? 16 : (31 - clz32(intRes2));
   } else {
     // index of ls-1-bit
     newECX = intRes2 == 0 ? 16 : ctz32(intRes2);
   }

   *(UInt*)(&resV[0]) = newECX;

   // generate new flags, common to all ISTRI and ISTRM cases
   *resOSZACP    // A, P are zero
     = ((intRes2 == 0) ? 0 : MASK_C) // C == 0 iff intRes2 == 0
     | ((zmaskL == 0)  ? 0 : MASK_Z) // Z == 1 iff any in argL is 0
     | ((zmaskR == 0)  ? 0 : MASK_S) // S == 1 iff any in argR is 0
     | ((intRes2 & 1) << SHIFT_O);   // O == IntRes2[0]
}


/* Compute result and new OSZACP flags for all PCMP{E,I}STR{I,M}
   variants.

   For xSTRI variants, the new ECX value is placed in the 32 bits
   pointed to by *resV.  For xSTRM variants, the result is a 128 bit
   value and is placed at *resV in the obvious way.

   For all variants, the new OSZACP value is placed at *resOSZACP.

   argLV and argRV are the vector args.  The caller must prepare a
   16-bit mask for each, zmaskL and zmaskR.  For ISTRx variants this
   must be 1 for each zero byte of of the respective arg.  For ESTRx
   variants this is derived from the explicit length indication, and
   must be 0 in all places except at the bit index corresponding to
   the valid length (0 .. 16).  If the valid length is 16 then the
   mask must be all zeroes.  In all cases, bits 31:16 must be zero.

   imm8 is the original immediate from the instruction.  isSTRM
   indicates whether this is a xSTRM or xSTRI variant, which controls
   how much of *res is written.

   If the given imm8 case can be handled, the return value is True.
   If not, False is returned, and neither *res not *resOSZACP are
   altered.
*/

Bool pcmpXstrX_WRK ( /*OUT*/V128* resV,
                     /*OUT*/UInt* resOSZACP,
                     V128* argLV,  V128* argRV,
                     UInt zmaskL, UInt zmaskR,
                     UInt imm8,   Bool isSTRM )
{
   assert(imm8 < 0x80);
   assert((zmaskL >> 16) == 0);
   assert((zmaskR >> 16) == 0);

   /* Explicitly reject any imm8 values that haven't been validated,
      even if they would probably work.  Life is too short to have
      unvalidated cases in the code base. */
   switch (imm8) {
      case 0x00:
      case 0x02: case 0x08: case 0x0C: case 0x12: case 0x1A:
      case 0x38: case 0x3A: case 0x44: case 0x4A:
         break;
      default:
         return False;
   }

   UInt fmt = (imm8 >> 0) & 3; // imm8[1:0]  data format
   UInt agg = (imm8 >> 2) & 3; // imm8[3:2]  aggregation fn
   UInt pol = (imm8 >> 4) & 3; // imm8[5:4]  polarity
   UInt idx = (imm8 >> 6) & 1; // imm8[6]    1==msb/bytemask

   /*----------------------------------------*/
   /*-- strcmp on byte data                --*/
   /*----------------------------------------*/

   if (agg == 2/*equal each, aka strcmp*/
       && (fmt == 0/*ub*/ || fmt == 2/*sb*/)
       && !isSTRM) {
      Int    i;
      UChar* argL = (UChar*)argLV;
      UChar* argR = (UChar*)argRV;
      UInt boolResII = 0;
      for (i = 15; i >= 0; i--) {
         UChar cL  = argL[i];
         UChar cR  = argR[i];
         boolResII = (boolResII << 1) | (cL == cR ? 1 : 0);
      }
      UInt validL = ~(zmaskL | -zmaskL);  // not(left(zmaskL))
      UInt validR = ~(zmaskR | -zmaskR);  // not(left(zmaskR))

      // do invalidation, common to all equal-each cases
      UInt intRes1
         = (boolResII & validL & validR)  // if both valid, use cmpres
           | (~ (validL | validR));       // if both invalid, force 1
                                          // else force 0
      intRes1 &= 0xFFFF;

      // generate I-format output
      pcmpXstrX_WRK_gen_output_fmt_I(
         resV, resOSZACP,
         intRes1, zmaskL, zmaskR, validL, pol, idx
      );

      return True;
   }

   /*----------------------------------------*/
   /*-- set membership on byte data        --*/
   /*----------------------------------------*/

   if (agg == 0/*equal any, aka find chars in a set*/
       && (fmt == 0/*ub*/ || fmt == 2/*sb*/)
       && !isSTRM) {
      /* argL: the string,  argR: charset */
      UInt   si, ci;
      UChar* argL    = (UChar*)argLV;
      UChar* argR    = (UChar*)argRV;
      UInt   boolRes = 0;
      UInt   validL  = ~(zmaskL | -zmaskL);  // not(left(zmaskL))
      UInt   validR  = ~(zmaskR | -zmaskR);  // not(left(zmaskR))

      for (si = 0; si < 16; si++) {
         if ((validL & (1 << si)) == 0)
            // run off the end of the string.
            break;
         UInt m = 0;
         for (ci = 0; ci < 16; ci++) {
            if ((validR & (1 << ci)) == 0) break;
            if (argR[ci] == argL[si]) { m = 1; break; }
         }
         boolRes |= (m << si);
      }

      // boolRes is "pre-invalidated"
      UInt intRes1 = boolRes & 0xFFFF;
   
      // generate I-format output
      pcmpXstrX_WRK_gen_output_fmt_I(
         resV, resOSZACP,
         intRes1, zmaskL, zmaskR, validL, pol, idx
      );

      return True;
   }

   /*----------------------------------------*/
   /*-- substring search on byte data      --*/
   /*----------------------------------------*/

   if (agg == 3/*equal ordered, aka substring search*/
       && (fmt == 0/*ub*/ || fmt == 2/*sb*/)
       && !isSTRM) {

      /* argL: haystack,  argR: needle */
      UInt   ni, hi;
      UChar* argL    = (UChar*)argLV;
      UChar* argR    = (UChar*)argRV;
      UInt   boolRes = 0;
      UInt   validL  = ~(zmaskL | -zmaskL);  // not(left(zmaskL))
      UInt   validR  = ~(zmaskR | -zmaskR);  // not(left(zmaskR))
      for (hi = 0; hi < 16; hi++) {
         UInt m = 1;
         for (ni = 0; ni < 16; ni++) {
            if ((validR & (1 << ni)) == 0) break;
            UInt i = ni + hi;
            if (i >= 16) break;
            if (argL[i] != argR[ni]) { m = 0; break; }
         }
         boolRes |= (m << hi);
         if ((validL & (1 << hi)) == 0)
            // run off the end of the haystack
            break;
      }

      // boolRes is "pre-invalidated"
      UInt intRes1 = boolRes & 0xFFFF;

      // generate I-format output
      pcmpXstrX_WRK_gen_output_fmt_I(
         resV, resOSZACP,
         intRes1, zmaskL, zmaskR, validL, pol, idx
      );

      return True;
   }

   /*----------------------------------------*/
   /*-- ranges, unsigned byte data         --*/
   /*----------------------------------------*/

   if (agg == 1/*ranges*/
       && fmt == 0/*ub*/
       && !isSTRM) {

      /* argL: string,  argR: range-pairs */
      UInt   ri, si;
      UChar* argL    = (UChar*)argLV;
      UChar* argR    = (UChar*)argRV;
      UInt   boolRes = 0;
      UInt   validL  = ~(zmaskL | -zmaskL);  // not(left(zmaskL))
      UInt   validR  = ~(zmaskR | -zmaskR);  // not(left(zmaskR))
      for (si = 0; si < 16; si++) {
         if ((validL & (1 << si)) == 0)
            // run off the end of the string
            break;
         UInt m = 0;
         for (ri = 0; ri < 16; ri += 2) {
            if ((validR & (3 << ri)) != (3 << ri)) break;
            if (argR[ri] <= argL[si] && argL[si] <= argR[ri+1]) { 
               m = 1; break;
            }
         }
         boolRes |= (m << si);
      }

      // boolRes is "pre-invalidated"
      UInt intRes1 = boolRes & 0xFFFF;

      // generate I-format output
      pcmpXstrX_WRK_gen_output_fmt_I(
         resV, resOSZACP,
         intRes1, zmaskL, zmaskR, validL, pol, idx
      );

      return True;
   }

   return False;
}


//////////////////////////////////////////////////////////
//                                                      //
//                       ISTRI_4A                       //
//                                                      //
//////////////////////////////////////////////////////////

UInt h_pcmpistri_4A ( V128* argL, V128* argR )
{
   V128 block[2];
   memcpy(&block[0], argL, sizeof(V128));
   memcpy(&block[1], argR, sizeof(V128));
   ULong res, flags;
   __asm__ __volatile__(
      "subq      $1024,  %%rsp"             "\n\t"
      "movdqu    0(%2),  %%xmm2"            "\n\t"
      "movdqu    16(%2), %%xmm11"           "\n\t"
      "pcmpistri $0x4A,  %%xmm2, %%xmm11"   "\n\t"
      "pushfq"                              "\n\t"
      "popq      %%rdx"                     "\n\t"
      "movq      %%rcx,  %0"                "\n\t"
      "movq      %%rdx,  %1"                "\n\t"
      "addq      $1024,  %%rsp"             "\n\t"
      : /*out*/ "=r"(res), "=r"(flags) : "r"/*in*/(&block[0])
      : "rcx","rdx","xmm0","xmm2","xmm11","cc","memory"
   );
   return ((flags & 0x8D5) << 16) | (res & 0xFFFF);
}

UInt s_pcmpistri_4A ( V128* argLU, V128* argRU )
{
   V128 resV;
   UInt resOSZACP, resECX;
   Bool ok
      = pcmpXstrX_WRK( &resV, &resOSZACP, argLU, argRU,
                       zmask_from_V128(argLU),
                       zmask_from_V128(argRU),
                       0x4A, False/*!isSTRM*/
        );
   assert(ok);
   resECX = resV.uInt[0];
   return (resOSZACP << 16) | resECX;
}

void istri_4A ( void )
{
   char* wot = "4A";
   UInt(*h)(V128*,V128*) = h_pcmpistri_4A;
   UInt(*s)(V128*,V128*) = s_pcmpistri_4A;

   try_istri(wot,h,s, "0000000000000000", "0000000000000000"); 

   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaaaaaaaaaaaaaa"); 
   try_istri(wot,h,s, "aaaa2aaaaaaaaaaa", "aaaaaaaaaaaaaaaa"); 
   try_istri(wot,h,s, "aaaaaaaaa2aaaaaa", "aaaaaaaaaaaaaaaa"); 
   try_istri(wot,h,s, "aaaaaaaaaaaaa2aa", "aaaaaaaaaaaaaaaa"); 

   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaa2aaaaaaaaaaa"); 
   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaaaaaaa2aaaaaa"); 
   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaaaaaaaaaaaa2a"); 

   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaaaaaaaaaaaaaa"); 
   try_istri(wot,h,s, "baaaaaaaaaaaaaaa", "aaaaaaaaaaaaaaaa"); 
   try_istri(wot,h,s, "b9aaaaaaaaaaaaaa", "aaaaaaaaaaaaaaaa"); 
   try_istri(wot,h,s, "b9baaaaaaaaaaaaa", "aaaaaaaaaaaaaaaa"); 

   try_istri(wot,h,s, "b9baaaaaaaaaaaaa", "aaaaaaaaaaaaaaaa"); 
   try_istri(wot,h,s, "b9baaaaaaaaaaaaa", "aaaaaaaaaaaa7aaa"); 
   try_istri(wot,h,s, "b9baaaaaaaaaaaaa", "aaaaaaaa2aaa4aaa"); 

   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaaaaaaaaaaaaaa"); 

   try_istri(wot,h,s, "aaaaaaaaaaaa0aaa", "aaaaaaaaaaaaaaaa"); 
   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaaaaaaaaaa0aaa"); 
   try_istri(wot,h,s, "aaaaaaaaaaaa0aaa", "aaaaaaaaaaaa0aaa"); 

   try_istri(wot,h,s, "aaaaaaaa0aaaaaaa", "aaaaaaaaaaaaaaaa"); 
   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaaaaaaaaaa0aaa"); 
   try_istri(wot,h,s, "aaaaaaaa0aaaaaaa", "aaaaaaaaaaaa0aaa"); 

   try_istri(wot,h,s, "aaaaaaaaaaaa0aaa", "aaaaaaaaaaaaaaaa"); 
   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaaaaaa0aaaaaaa"); 
   try_istri(wot,h,s, "aaaaaaaaaaaa0aaa", "aaaaaaaa0aaaaaaa"); 

   try_istri(wot,h,s, "0000000000000000", "aaaaaaaa0aaaaaaa"); 
   try_istri(wot,h,s, "8000000000000000", "aaaaaaaa0aaaaaaa"); 
   try_istri(wot,h,s, "0000000000000001", "aaaaaaaa0aaaaaaa"); 

   try_istri(wot,h,s, "0000000000000000", "aaaaaaaaaaaaaaaa"); 
   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "0000000000000000"); 
}

//////////////////////////////////////////////////////////
//                                                      //
//                       ISTRI_3A                       //
//                                                      //
//////////////////////////////////////////////////////////

UInt h_pcmpistri_3A ( V128* argL, V128* argR )
{
   V128 block[2];
   memcpy(&block[0], argL, sizeof(V128));
   memcpy(&block[1], argR, sizeof(V128));
   ULong res, flags;
   __asm__ __volatile__(
      "subq      $1024,  %%rsp"             "\n\t"
      "movdqu    0(%2),  %%xmm2"            "\n\t"
      "movdqu    16(%2), %%xmm11"           "\n\t"
      "pcmpistri $0x3A,  %%xmm2, %%xmm11"   "\n\t"
      "pushfq"                              "\n\t"
      "popq      %%rdx"                     "\n\t"
      "movq      %%rcx,  %0"                "\n\t"
      "movq      %%rdx,  %1"                "\n\t"
      "addq      $1024,  %%rsp"             "\n\t"
      : /*out*/ "=r"(res), "=r"(flags) : "r"/*in*/(&block[0])
      : "rcx","rdx","xmm0","xmm2","xmm11","cc","memory"
   );
   return ((flags & 0x8D5) << 16) | (res & 0xFFFF);
}

UInt s_pcmpistri_3A ( V128* argLU, V128* argRU )
{
   V128 resV;
   UInt resOSZACP, resECX;
   Bool ok
      = pcmpXstrX_WRK( &resV, &resOSZACP, argLU, argRU,
                       zmask_from_V128(argLU),
                       zmask_from_V128(argRU),
                       0x3A, False/*!isSTRM*/
        );
   assert(ok);
   resECX = resV.uInt[0];
   return (resOSZACP << 16) | resECX;
}

void istri_3A ( void )
{
   char* wot = "3A";
   UInt(*h)(V128*,V128*) = h_pcmpistri_3A;
   UInt(*s)(V128*,V128*) = s_pcmpistri_3A;

   try_istri(wot,h,s, "0000000000000000", "0000000000000000"); 

   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaaaaaaaaaaaaaa"); 
   try_istri(wot,h,s, "aaaa2aaaaaaaaaaa", "aaaaaaaaaaaaaaaa"); 
   try_istri(wot,h,s, "aaaaaaaaa2aaaaaa", "aaaaaaaaaaaaaaaa"); 
   try_istri(wot,h,s, "aaaaaaaaaaaaa2aa", "aaaaaaaaaaaaaaaa"); 

   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaa2aaaaaaaaaaa"); 
   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaaaaaaa2aaaaaa"); 
   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaaaaaaaaaaaa2a"); 

   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaaaaaaaaaaaaaa"); 
   try_istri(wot,h,s, "baaaaaaaaaaaaaaa", "aaaaaaaaaaaaaaaa"); 
   try_istri(wot,h,s, "b9aaaaaaaaaaaaaa", "aaaaaaaaaaaaaaaa"); 
   try_istri(wot,h,s, "b9baaaaaaaaaaaaa", "aaaaaaaaaaaaaaaa"); 

   try_istri(wot,h,s, "b9baaaaaaaaaaaaa", "aaaaaaaaaaaaaaaa"); 
   try_istri(wot,h,s, "b9baaaaaaaaaaaaa", "aaaaaaaaaaaa7aaa"); 
   try_istri(wot,h,s, "b9baaaaaaaaaaaaa", "aaaaaaaa2aaa4aaa"); 

   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaaaaaaaaaaaaaa"); 

   try_istri(wot,h,s, "aaaaaaaaaaaa0aaa", "aaaaaaaaaaaaaaaa"); 
   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaaaaaaaaaa0aaa"); 
   try_istri(wot,h,s, "aaaaaaaaaaaa0aaa", "aaaaaaaaaaaa0aaa"); 

   try_istri(wot,h,s, "aaaaaaaa0aaaaaaa", "aaaaaaaaaaaaaaaa"); 
   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaaaaaaaaaa0aaa"); 
   try_istri(wot,h,s, "aaaaaaaa0aaaaaaa", "aaaaaaaaaaaa0aaa"); 

   try_istri(wot,h,s, "aaaaaaaaaaaa0aaa", "aaaaaaaaaaaaaaaa"); 
   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaaaaaa0aaaaaaa"); 
   try_istri(wot,h,s, "aaaaaaaaaaaa0aaa", "aaaaaaaa0aaaaaaa"); 

   try_istri(wot,h,s, "0000000000000000", "aaaaaaaa0aaaaaaa"); 
   try_istri(wot,h,s, "8000000000000000", "aaaaaaaa0aaaaaaa"); 
   try_istri(wot,h,s, "0000000000000001", "aaaaaaaa0aaaaaaa"); 

   try_istri(wot,h,s, "0000000000000000", "aaaaaaaaaaaaaaaa"); 
   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "0000000000000000"); 
}



//////////////////////////////////////////////////////////
//                                                      //
//                       ISTRI_0C                       //
//                                                      //
//////////////////////////////////////////////////////////

__attribute__((noinline))
UInt h_pcmpistri_0C ( V128* argL, V128* argR )
{
   V128 block[2];
   memcpy(&block[0], argL, sizeof(V128));
   memcpy(&block[1], argR, sizeof(V128));
   ULong res = 0, flags = 0;
   __asm__ __volatile__(
      "movdqu    0(%2),  %%xmm2"            "\n\t"
      "movdqu    16(%2), %%xmm11"           "\n\t"
      "pcmpistri $0x0C,  %%xmm2, %%xmm11"   "\n\t"
      //"pcmpistrm $0x0C,  %%xmm2, %%xmm11"   "\n\t"
      //"movd %%xmm0, %%ecx" "\n\t"
      "pushfq"                              "\n\t"
      "popq      %%rdx"                     "\n\t"
      "movq      %%rcx,  %0"                "\n\t"
      "movq      %%rdx,  %1"                "\n\t"
      : /*out*/ "=r"(res), "=r"(flags) : "r"/*in*/(&block[0])
      : "rcx","rdx","xmm0","xmm2","xmm11","cc","memory"
   );
   return ((flags & 0x8D5) << 16) | (res & 0xFFFF);
}

UInt s_pcmpistri_0C ( V128* argLU, V128* argRU )
{
   V128 resV;
   UInt resOSZACP, resECX;
   Bool ok
      = pcmpXstrX_WRK( &resV, &resOSZACP, argLU, argRU,
                       zmask_from_V128(argLU),
                       zmask_from_V128(argRU),
                       0x0C, False/*!isSTRM*/
        );
   assert(ok);
   resECX = resV.uInt[0];
   return (resOSZACP << 16) | resECX;
}

void istri_0C ( void )
{
   char* wot = "0C";
   UInt(*h)(V128*,V128*) = h_pcmpistri_0C;
   UInt(*s)(V128*,V128*) = s_pcmpistri_0C;
   
   try_istri(wot,h,s, "111111111abcde11", "00000000000abcde"); 

   try_istri(wot,h,s, "111111111abcde11", "0000abcde00abcde"); 

   try_istri(wot,h,s, "1111111111abcde1", "00000000000abcde"); 
   try_istri(wot,h,s, "11111111111abcde", "00000000000abcde"); 
   try_istri(wot,h,s, "111111111111abcd", "00000000000abcde"); 

   try_istri(wot,h,s, "111abcde1abcde11", "00000000000abcde"); 

   try_istri(wot,h,s, "11abcde11abcde11", "00000000000abcde"); 
   try_istri(wot,h,s, "1abcde111abcde11", "00000000000abcde"); 
   try_istri(wot,h,s, "abcde1111abcde11", "00000000000abcde"); 
   try_istri(wot,h,s, "bcde11111abcde11", "00000000000abcde"); 
   try_istri(wot,h,s, "cde111111abcde11", "00000000000abcde"); 

   try_istri(wot,h,s, "01abcde11abcde11", "00000000000abcde"); 
   try_istri(wot,h,s, "00abcde11abcde11", "00000000000abcde"); 
   try_istri(wot,h,s, "000bcde11abcde11", "00000000000abcde"); 

   try_istri(wot,h,s, "00abcde10abcde11", "00000000000abcde"); 
   try_istri(wot,h,s, "00abcde100bcde11", "00000000000abcde"); 

   try_istri(wot,h,s, "1111111111111234", "0000000000000000"); 
   try_istri(wot,h,s, "1111111111111234", "0000000000000001"); 
   try_istri(wot,h,s, "1111111111111234", "0000000000000011"); 

   try_istri(wot,h,s, "1111111111111234", "1111111111111234"); 
   try_istri(wot,h,s, "a111111111111111", "000000000000000a"); 
   try_istri(wot,h,s, "b111111111111111", "000000000000000a"); 

   try_istri(wot,h,s, "b111111111111111", "0000000000000000");
   try_istri(wot,h,s, "0000000000000000", "0000000000000000");
   try_istri(wot,h,s, "123456789abcdef1", "0000000000000000");
   try_istri(wot,h,s, "0000000000000000", "123456789abcdef1");
}


//////////////////////////////////////////////////////////
//                                                      //
//                       ISTRI_08                       //
//                                                      //
//////////////////////////////////////////////////////////

UInt h_pcmpistri_08 ( V128* argL, V128* argR )
{
   V128 block[2];
   memcpy(&block[0], argL, sizeof(V128));
   memcpy(&block[1], argR, sizeof(V128));
   ULong res, flags;
   __asm__ __volatile__(
      "subq      $1024,  %%rsp"             "\n\t"
      "movdqu    0(%2),  %%xmm2"            "\n\t"
      "movdqu    16(%2), %%xmm11"           "\n\t"
      "pcmpistri $0x08,  %%xmm2, %%xmm11"   "\n\t"
      "pushfq"                              "\n\t"
      "popq      %%rdx"                     "\n\t"
      "movq      %%rcx,  %0"                "\n\t"
      "movq      %%rdx,  %1"                "\n\t"
      "addq      $1024,  %%rsp"             "\n\t"
      : /*out*/ "=r"(res), "=r"(flags) : "r"/*in*/(&block[0])
      : "rcx","rdx","xmm0","xmm2","xmm11","cc","memory"
   );
   return ((flags & 0x8D5) << 16) | (res & 0xFFFF);
}

UInt s_pcmpistri_08 ( V128* argLU, V128* argRU )
{
   V128 resV;
   UInt resOSZACP, resECX;
   Bool ok
      = pcmpXstrX_WRK( &resV, &resOSZACP, argLU, argRU,
                       zmask_from_V128(argLU),
                       zmask_from_V128(argRU),
                       0x08, False/*!isSTRM*/
        );
   assert(ok);
   resECX = resV.uInt[0];
   return (resOSZACP << 16) | resECX;
}

void istri_08 ( void )
{
   char* wot = "08";
   UInt(*h)(V128*,V128*) = h_pcmpistri_08;
   UInt(*s)(V128*,V128*) = s_pcmpistri_08;

   try_istri(wot,h,s, "0000000000000000", "0000000000000000"); 

   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaaaaaaaaaaaaaa"); 
   try_istri(wot,h,s, "aaaa2aaaaaaaaaaa", "aaaaaaaaaaaaaaaa"); 
   try_istri(wot,h,s, "aaaaaaaaa2aaaaaa", "aaaaaaaaaaaaaaaa"); 
   try_istri(wot,h,s, "aaaaaaaaaaaaa2aa", "aaaaaaaaaaaaaaaa"); 

   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaa2aaaaaaaaaaa"); 
   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaaaaaaa2aaaaaa"); 
   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaaaaaaaaaaaa2a"); 

   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaaaaaaaaaaaaaa"); 
   try_istri(wot,h,s, "baaaaaaaaaaaaaaa", "aaaaaaaaaaaaaaaa"); 
   try_istri(wot,h,s, "b9aaaaaaaaaaaaaa", "aaaaaaaaaaaaaaaa"); 
   try_istri(wot,h,s, "b9baaaaaaaaaaaaa", "aaaaaaaaaaaaaaaa"); 

   try_istri(wot,h,s, "b9baaaaaaaaaaaaa", "aaaaaaaaaaaaaaaa"); 
   try_istri(wot,h,s, "b9baaaaaaaaaaaaa", "aaaaaaaaaaaa7aaa"); 
   try_istri(wot,h,s, "b9baaaaaaaaaaaaa", "aaaaaaaa2aaa4aaa"); 

   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaaaaaaaaaaaaaa"); 

   try_istri(wot,h,s, "aaaaaaaaaaaa0aaa", "aaaaaaaaaaaaaaaa"); 
   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaaaaaaaaaa0aaa"); 
   try_istri(wot,h,s, "aaaaaaaaaaaa0aaa", "aaaaaaaaaaaa0aaa"); 

   try_istri(wot,h,s, "aaaaaaaa0aaaaaaa", "aaaaaaaaaaaaaaaa"); 
   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaaaaaaaaaa0aaa"); 
   try_istri(wot,h,s, "aaaaaaaa0aaaaaaa", "aaaaaaaaaaaa0aaa"); 

   try_istri(wot,h,s, "aaaaaaaaaaaa0aaa", "aaaaaaaaaaaaaaaa"); 
   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaaaaaa0aaaaaaa"); 
   try_istri(wot,h,s, "aaaaaaaaaaaa0aaa", "aaaaaaaa0aaaaaaa"); 

   try_istri(wot,h,s, "0000000000000000", "aaaaaaaa0aaaaaaa"); 
   try_istri(wot,h,s, "8000000000000000", "aaaaaaaa0aaaaaaa"); 
   try_istri(wot,h,s, "0000000000000001", "aaaaaaaa0aaaaaaa"); 

   try_istri(wot,h,s, "0000000000000000", "aaaaaaaaaaaaaaaa"); 
   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "0000000000000000"); 
}



//////////////////////////////////////////////////////////
//                                                      //
//                       ISTRI_1A                       //
//                                                      //
//////////////////////////////////////////////////////////

UInt h_pcmpistri_1A ( V128* argL, V128* argR )
{
   V128 block[2];
   memcpy(&block[0], argL, sizeof(V128));
   memcpy(&block[1], argR, sizeof(V128));
   ULong res, flags;
   __asm__ __volatile__(
      "subq      $1024,  %%rsp"             "\n\t"
      "movdqu    0(%2),  %%xmm2"            "\n\t"
      "movdqu    16(%2), %%xmm11"           "\n\t"
      "pcmpistri $0x1A,  %%xmm2, %%xmm11"   "\n\t"
      "pushfq"                              "\n\t"
      "popq      %%rdx"                     "\n\t"
      "movq      %%rcx,  %0"                "\n\t"
      "movq      %%rdx,  %1"                "\n\t"
      "addq      $1024,  %%rsp"             "\n\t"
      : /*out*/ "=r"(res), "=r"(flags) : "r"/*in*/(&block[0])
      : "rcx","rdx","xmm0","xmm2","xmm11","cc","memory"
   );
   return ((flags & 0x8D5) << 16) | (res & 0xFFFF);
}

UInt s_pcmpistri_1A ( V128* argLU, V128* argRU )
{
   V128 resV;
   UInt resOSZACP, resECX;
   Bool ok
      = pcmpXstrX_WRK( &resV, &resOSZACP, argLU, argRU,
                       zmask_from_V128(argLU),
                       zmask_from_V128(argRU),
                       0x1A, False/*!isSTRM*/
        );
   assert(ok);
   resECX = resV.uInt[0];
   return (resOSZACP << 16) | resECX;
}

void istri_1A ( void )
{
   char* wot = "1A";
   UInt(*h)(V128*,V128*) = h_pcmpistri_1A;
   UInt(*s)(V128*,V128*) = s_pcmpistri_1A;

   try_istri(wot,h,s, "0000000000000000", "0000000000000000"); 

   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaaaaaaaaaaaaaa"); 
   try_istri(wot,h,s, "aaaa2aaaaaaaaaaa", "aaaaaaaaaaaaaaaa"); 
   try_istri(wot,h,s, "aaaaaaaaa2aaaaaa", "aaaaaaaaaaaaaaaa"); 
   try_istri(wot,h,s, "aaaaaaaaaaaaa2aa", "aaaaaaaaaaaaaaaa"); 

   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaa2aaaaaaaaaaa"); 
   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaaaaaaa2aaaaaa"); 
   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaaaaaaaaaaaa2a"); 

   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaaaaaaaaaaaaaa"); 
   try_istri(wot,h,s, "baaaaaaaaaaaaaaa", "aaaaaaaaaaaaaaaa"); 
   try_istri(wot,h,s, "b9aaaaaaaaaaaaaa", "aaaaaaaaaaaaaaaa"); 
   try_istri(wot,h,s, "b9baaaaaaaaaaaaa", "aaaaaaaaaaaaaaaa"); 

   try_istri(wot,h,s, "b9baaaaaaaaaaaaa", "aaaaaaaaaaaaaaaa"); 
   try_istri(wot,h,s, "b9baaaaaaaaaaaaa", "aaaaaaaaaaaa7aaa"); 
   try_istri(wot,h,s, "b9baaaaaaaaaaaaa", "aaaaaaaa2aaa4aaa"); 

   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaaaaaaaaaaaaaa"); 

   try_istri(wot,h,s, "aaaaaaaaaaaa0aaa", "aaaaaaaaaaaaaaaa"); 
   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaaaaaaaaaa0aaa"); 
   try_istri(wot,h,s, "aaaaaaaaaaaa0aaa", "aaaaaaaaaaaa0aaa"); 

   try_istri(wot,h,s, "aaaaaaaa0aaaaaaa", "aaaaaaaaaaaaaaaa"); 
   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaaaaaaaaaa0aaa"); 
   try_istri(wot,h,s, "aaaaaaaa0aaaaaaa", "aaaaaaaaaaaa0aaa"); 

   try_istri(wot,h,s, "aaaaaaaaaaaa0aaa", "aaaaaaaaaaaaaaaa"); 
   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaaaaaa0aaaaaaa"); 
   try_istri(wot,h,s, "aaaaaaaaaaaa0aaa", "aaaaaaaa0aaaaaaa"); 

   try_istri(wot,h,s, "0000000000000000", "aaaaaaaa0aaaaaaa"); 
   try_istri(wot,h,s, "8000000000000000", "aaaaaaaa0aaaaaaa"); 
   try_istri(wot,h,s, "0000000000000001", "aaaaaaaa0aaaaaaa"); 

   try_istri(wot,h,s, "0000000000000000", "aaaaaaaaaaaaaaaa"); 
   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "0000000000000000"); 
}



//////////////////////////////////////////////////////////
//                                                      //
//                       ISTRI_02                       //
//                                                      //
//////////////////////////////////////////////////////////

UInt h_pcmpistri_02 ( V128* argL, V128* argR )
{
   V128 block[2];
   memcpy(&block[0], argL, sizeof(V128));
   memcpy(&block[1], argR, sizeof(V128));
   ULong res, flags;
   __asm__ __volatile__(
      "subq      $1024,  %%rsp"             "\n\t"
      "movdqu    0(%2),  %%xmm2"            "\n\t"
      "movdqu    16(%2), %%xmm11"           "\n\t"
      "pcmpistri $0x02,  %%xmm2, %%xmm11"   "\n\t"
//"pcmpistrm $0x02, %%xmm2, %%xmm11"   "\n\t"
//"movd %%xmm0, %%ecx" "\n\t"
      "pushfq"                              "\n\t"
      "popq      %%rdx"                     "\n\t"
      "movq      %%rcx,  %0"                "\n\t"
      "movq      %%rdx,  %1"                "\n\t"
      "addq      $1024,  %%rsp"             "\n\t"
      : /*out*/ "=r"(res), "=r"(flags) : "r"/*in*/(&block[0])
      : "rcx","rdx","xmm0","xmm2","xmm11","cc","memory"
   );
   return ((flags & 0x8D5) << 16) | (res & 0xFFFF);
}

UInt s_pcmpistri_02 ( V128* argLU, V128* argRU )
{
   V128 resV;
   UInt resOSZACP, resECX;
   Bool ok
      = pcmpXstrX_WRK( &resV, &resOSZACP, argLU, argRU,
                       zmask_from_V128(argLU),
                       zmask_from_V128(argRU),
                       0x02, False/*!isSTRM*/
        );
   assert(ok);
   resECX = resV.uInt[0];
   return (resOSZACP << 16) | resECX;
}

void istri_02 ( void )
{
   char* wot = "02";
   UInt(*h)(V128*,V128*) = h_pcmpistri_02;
   UInt(*s)(V128*,V128*) = s_pcmpistri_02;

   try_istri(wot,h,s, "abcdacbdabcdabcd", "000000000000000a"); 
   try_istri(wot,h,s, "abcdabcdabcdabcd", "000000000000000b"); 
   try_istri(wot,h,s, "abcdabcdabcdabcd", "00000000000000ab"); 
   try_istri(wot,h,s, "abcdabc0abcdabcd", "000000000000abcd"); 

   try_istri(wot,h,s, "abcdabcdabcdabcd", "000000000000abcd"); 
   try_istri(wot,h,s, "0bcdabcdabcdabcd", "000000000000abcd"); 
   try_istri(wot,h,s, "abcdabcdabcda0cd", "000000000000abcd"); 
   try_istri(wot,h,s, "abcdabcdabcdab0d", "000000000000abcd"); 
   try_istri(wot,h,s, "abcdabcdabcdabc0", "000000000000abcd"); 

   try_istri(wot,h,s, "abcdabcdabcdabcd", "000000000000abcd"); 
   try_istri(wot,h,s, "abcdabcdabcdabcd", "000000000000a0cd"); 
   try_istri(wot,h,s, "abcdabcdabcdabcd", "000000000000ab0d"); 
   try_istri(wot,h,s, "abcdabcdabcdabcd", "000000000000abc0"); 

   try_istri(wot,h,s, "0000000000000000", "0000000000000000"); 
   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaaaaaaaaaaaaaa"); 

   try_istri(wot,h,s, "0000abcdabcdabcd", "000000000000abcd"); 
   try_istri(wot,h,s, "0000abcdabcdabcd", "000000000000dcba"); 
   try_istri(wot,h,s, "0000abcdabcdabcd", "000000000000bbbb"); 
   try_istri(wot,h,s, "0000abcdabcdabcd", "000000000000baba"); 

   try_istri(wot,h,s, "0000abcdabcdabcd", "00000000000baba0"); 

   try_istri(wot,h,s, "0ddc0ffeebadf00d", "00000000cafebabe"); 
   try_istri(wot,h,s, "0ddc0ffeebadfeed", "00000000cafebabe"); 
}


//////////////////////////////////////////////////////////
//                                                      //
//                       ISTRI_12                       //
//                                                      //
//////////////////////////////////////////////////////////

UInt h_pcmpistri_12 ( V128* argL, V128* argR )
{
   V128 block[2];
   memcpy(&block[0], argL, sizeof(V128));
   memcpy(&block[1], argR, sizeof(V128));
   ULong res, flags;
   __asm__ __volatile__(
      "subq      $1024,  %%rsp"             "\n\t"
      "movdqu    0(%2),  %%xmm2"            "\n\t"
      "movdqu    16(%2), %%xmm11"           "\n\t"
      "pcmpistri $0x12,  %%xmm2, %%xmm11"   "\n\t"
//"pcmpistrm $0x12, %%xmm2, %%xmm11"   "\n\t"
//"movd %%xmm0, %%ecx" "\n\t"
      "pushfq"                              "\n\t"
      "popq      %%rdx"                     "\n\t"
      "movq      %%rcx,  %0"                "\n\t"
      "movq      %%rdx,  %1"                "\n\t"
      "addq      $1024,  %%rsp"             "\n\t"
      : /*out*/ "=r"(res), "=r"(flags) : "r"/*in*/(&block[0])
      : "rcx","rdx","xmm0","xmm2","xmm11","cc","memory"
   );
   return ((flags & 0x8D5) << 16) | (res & 0xFFFF);
}

UInt s_pcmpistri_12 ( V128* argLU, V128* argRU )
{
   V128 resV;
   UInt resOSZACP, resECX;
   Bool ok
      = pcmpXstrX_WRK( &resV, &resOSZACP, argLU, argRU,
                       zmask_from_V128(argLU),
                       zmask_from_V128(argRU),
                       0x12, False/*!isSTRM*/
        );
   assert(ok);
   resECX = resV.uInt[0];
   return (resOSZACP << 16) | resECX;
}

void istri_12 ( void )
{
   char* wot = "12";
   UInt(*h)(V128*,V128*) = h_pcmpistri_12;
   UInt(*s)(V128*,V128*) = s_pcmpistri_12;

   try_istri(wot,h,s, "abcdacbdabcdabcd", "000000000000000a"); 
   try_istri(wot,h,s, "abcdabcdabcdabcd", "000000000000000b"); 
   try_istri(wot,h,s, "abcdabcdabcdabcd", "00000000000000ab"); 
   try_istri(wot,h,s, "abcdabc0abcdabcd", "000000000000abcd"); 

   try_istri(wot,h,s, "abcdabcdabcdabcd", "000000000000abcd"); 
   try_istri(wot,h,s, "0bcdabcdabcdabcd", "000000000000abcd"); 
   try_istri(wot,h,s, "abcdabcdabcda0cd", "000000000000abcd"); 
   try_istri(wot,h,s, "abcdabcdabcdab0d", "000000000000abcd"); 
   try_istri(wot,h,s, "abcdabcdabcdabc0", "000000000000abcd"); 

   try_istri(wot,h,s, "abcdabcdabcdabcd", "000000000000abcd"); 
   try_istri(wot,h,s, "abcdabcdabcdabcd", "000000000000a0cd"); 
   try_istri(wot,h,s, "abcdabcdabcdabcd", "000000000000ab0d"); 
   try_istri(wot,h,s, "abcdabcdabcdabcd", "000000000000abc0"); 

   try_istri(wot,h,s, "0000000000000000", "0000000000000000"); 
   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaaaaaaaaaaaaaa"); 

   try_istri(wot,h,s, "0000abcdabcdabcd", "000000000000abcd"); 
   try_istri(wot,h,s, "0000abcdabcdabcd", "000000000000dcba"); 
   try_istri(wot,h,s, "0000abcdabcdabcd", "000000000000bbbb"); 
   try_istri(wot,h,s, "0000abcdabcdabcd", "000000000000baba"); 

   try_istri(wot,h,s, "0000abcdabcdabcd", "00000000000baba0"); 

   try_istri(wot,h,s, "0ddc0ffeebadf00d", "00000000cafebabe"); 
   try_istri(wot,h,s, "0ddc0ffeebadfeed", "00000000cafebabe"); 
}



//////////////////////////////////////////////////////////
//                                                      //
//                       ISTRI_44                       //
//                                                      //
//////////////////////////////////////////////////////////

UInt h_pcmpistri_44 ( V128* argL, V128* argR )
{
   V128 block[2];
   memcpy(&block[0], argL, sizeof(V128));
   memcpy(&block[1], argR, sizeof(V128));
   ULong res, flags;
   __asm__ __volatile__(
      "subq      $1024,  %%rsp"             "\n\t"
      "movdqu    0(%2),  %%xmm2"            "\n\t"
      "movdqu    16(%2), %%xmm11"           "\n\t"
      "pcmpistri $0x44,  %%xmm2, %%xmm11"   "\n\t"
//"pcmpistrm $0x04, %%xmm2, %%xmm11"   "\n\t"
//"movd %%xmm0, %%ecx" "\n\t"
      "pushfq"                              "\n\t"
      "popq      %%rdx"                     "\n\t"
      "movq      %%rcx,  %0"                "\n\t"
      "movq      %%rdx,  %1"                "\n\t"
      "addq      $1024,  %%rsp"             "\n\t"
      : /*out*/ "=r"(res), "=r"(flags) : "r"/*in*/(&block[0])
      : "rcx","rdx","xmm0","xmm2","xmm11","cc","memory"
   );
   return ((flags & 0x8D5) << 16) | (res & 0xFFFF);
}

UInt s_pcmpistri_44 ( V128* argLU, V128* argRU )
{
   V128 resV;
   UInt resOSZACP, resECX;
   Bool ok
      = pcmpXstrX_WRK( &resV, &resOSZACP, argLU, argRU,
                       zmask_from_V128(argLU),
                       zmask_from_V128(argRU),
                       0x44, False/*!isSTRM*/
        );
   assert(ok);
   resECX = resV.uInt[0];
   return (resOSZACP << 16) | resECX;
}

void istri_44 ( void )
{
   char* wot = "44";
   UInt(*h)(V128*,V128*) = h_pcmpistri_44;
   UInt(*s)(V128*,V128*) = s_pcmpistri_44;

   try_istri(wot,h,s, "aaaabbbbccccdddd", "00000000000000bc"); 
   try_istri(wot,h,s, "aaaabbbbccccdddd", "00000000000000cb"); 
   try_istri(wot,h,s, "baaabbbbccccdddd", "00000000000000cb"); 
   try_istri(wot,h,s, "baaabbbbccccdddc", "00000000000000cb"); 

   try_istri(wot,h,s, "bbbbbbbbbbbbbbbb", "00000000000000cb"); 
   try_istri(wot,h,s, "bbbbbbbb0bbbbbbb", "00000000000000cb"); 
   try_istri(wot,h,s, "bbbbbbbbbbbbbb0b", "00000000000000cb"); 
   try_istri(wot,h,s, "bbbbbbbbbbbbbbb0", "00000000000000cb"); 
   try_istri(wot,h,s, "0000000000000000", "00000000000000cb"); 

   try_istri(wot,h,s, "0000000000000000", "0000000000000000"); 

   try_istri(wot,h,s, "bbbbbbbbbbbbbbbb", "00000000000000cb"); 
   try_istri(wot,h,s, "bbbbbbbbbbbbbbbb", "000000000000000b"); 
   try_istri(wot,h,s, "b4b4b4b4b4b4b4b4", "00000000000062cb"); 

   try_istri(wot,h,s, "b4b4b4b4b4b4b4b4", "00000000000002cb"); 
   try_istri(wot,h,s, "b4b4b4b4b4b4b4b4", "00000000000000cb"); 
   try_istri(wot,h,s, "b4b4b4b4b4b4b4b4", "000000000000000b");

   try_istri(wot,h,s, "0123456789abcdef", "000000fecb975421");
   try_istri(wot,h,s, "123456789abcdef1", "000000fecb975421");

   try_istri(wot,h,s, "0123456789abcdef", "00000000dca86532");
   try_istri(wot,h,s, "123456789abcdef1", "00000000dca86532");
}


//////////////////////////////////////////////////////////
//                                                      //
//                       ISTRI_00                       //
//                                                      //
//////////////////////////////////////////////////////////

UInt h_pcmpistri_00 ( V128* argL, V128* argR )
{
   V128 block[2];
   memcpy(&block[0], argL, sizeof(V128));
   memcpy(&block[1], argR, sizeof(V128));
   ULong res, flags;
   __asm__ __volatile__(
      "subq      $1024,  %%rsp"             "\n\t"
      "movdqu    0(%2),  %%xmm2"            "\n\t"
      "movdqu    16(%2), %%xmm11"           "\n\t"
      "pcmpistri $0x00,  %%xmm2, %%xmm11"   "\n\t"
//"pcmpistrm $0x00, %%xmm2, %%xmm11"   "\n\t"
//"movd %%xmm0, %%ecx" "\n\t"
      "pushfq"                              "\n\t"
      "popq      %%rdx"                     "\n\t"
      "movq      %%rcx,  %0"                "\n\t"
      "movq      %%rdx,  %1"                "\n\t"
      "addq      $1024,  %%rsp"             "\n\t"
      : /*out*/ "=r"(res), "=r"(flags) : "r"/*in*/(&block[0])
      : "rcx","rdx","xmm0","xmm2","xmm11","cc","memory"
   );
   return ((flags & 0x8D5) << 16) | (res & 0xFFFF);
}

UInt s_pcmpistri_00 ( V128* argLU, V128* argRU )
{
   V128 resV;
   UInt resOSZACP, resECX;
   Bool ok
      = pcmpXstrX_WRK( &resV, &resOSZACP, argLU, argRU,
                       zmask_from_V128(argLU),
                       zmask_from_V128(argRU),
                       0x00, False/*!isSTRM*/
        );
   assert(ok);
   resECX = resV.uInt[0];
   return (resOSZACP << 16) | resECX;
}

void istri_00 ( void )
{
   char* wot = "00";
   UInt(*h)(V128*,V128*) = h_pcmpistri_00;
   UInt(*s)(V128*,V128*) = s_pcmpistri_00;

   try_istri(wot,h,s, "abcdacbdabcdabcd", "000000000000000a"); 
   try_istri(wot,h,s, "abcdabcdabcdabcd", "000000000000000b"); 
   try_istri(wot,h,s, "abcdabcdabcdabcd", "00000000000000ab"); 
   try_istri(wot,h,s, "abcdabc0abcdabcd", "000000000000abcd"); 

   try_istri(wot,h,s, "abcdabcdabcdabcd", "000000000000abcd"); 
   try_istri(wot,h,s, "0bcdabcdabcdabcd", "000000000000abcd"); 
   try_istri(wot,h,s, "abcdabcdabcda0cd", "000000000000abcd"); 
   try_istri(wot,h,s, "abcdabcdabcdab0d", "000000000000abcd"); 
   try_istri(wot,h,s, "abcdabcdabcdabc0", "000000000000abcd"); 

   try_istri(wot,h,s, "abcdabcdabcdabcd", "000000000000abcd"); 
   try_istri(wot,h,s, "abcdabcdabcdabcd", "000000000000a0cd"); 
   try_istri(wot,h,s, "abcdabcdabcdabcd", "000000000000ab0d"); 
   try_istri(wot,h,s, "abcdabcdabcdabcd", "000000000000abc0"); 

   try_istri(wot,h,s, "0000000000000000", "0000000000000000"); 
   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaaaaaaaaaaaaaa"); 

   try_istri(wot,h,s, "0000abcdabcdabcd", "000000000000abcd"); 
   try_istri(wot,h,s, "0000abcdabcdabcd", "000000000000dcba"); 
   try_istri(wot,h,s, "0000abcdabcdabcd", "000000000000bbbb"); 
   try_istri(wot,h,s, "0000abcdabcdabcd", "000000000000baba"); 

   try_istri(wot,h,s, "0000abcdabcdabcd", "00000000000baba0"); 

   try_istri(wot,h,s, "0ddc0ffeebadf00d", "00000000cafebabe"); 
   try_istri(wot,h,s, "0ddc0ffeebadfeed", "00000000cafebabe"); 
}


//////////////////////////////////////////////////////////
//                                                      //
//                       ISTRI_38                       //
//                                                      //
//////////////////////////////////////////////////////////

UInt h_pcmpistri_38 ( V128* argL, V128* argR )
{
   V128 block[2];
   memcpy(&block[0], argL, sizeof(V128));
   memcpy(&block[1], argR, sizeof(V128));
   ULong res, flags;
   __asm__ __volatile__(
      "subq      $1024,  %%rsp"             "\n\t"
      "movdqu    0(%2),  %%xmm2"            "\n\t"
      "movdqu    16(%2), %%xmm11"           "\n\t"
      "pcmpistri $0x38,  %%xmm2, %%xmm11"   "\n\t"
      "pushfq"                              "\n\t"
      "popq      %%rdx"                     "\n\t"
      "movq      %%rcx,  %0"                "\n\t"
      "movq      %%rdx,  %1"                "\n\t"
      "addq      $1024,  %%rsp"             "\n\t"
      : /*out*/ "=r"(res), "=r"(flags) : "r"/*in*/(&block[0])
      : "rcx","rdx","xmm0","xmm2","xmm11","cc","memory"
   );
   return ((flags & 0x8D5) << 16) | (res & 0xFFFF);
}

UInt s_pcmpistri_38 ( V128* argLU, V128* argRU )
{
   V128 resV;
   UInt resOSZACP, resECX;
   Bool ok
      = pcmpXstrX_WRK( &resV, &resOSZACP, argLU, argRU,
                       zmask_from_V128(argLU),
                       zmask_from_V128(argRU),
                       0x38, False/*!isSTRM*/
        );
   assert(ok);
   resECX = resV.uInt[0];
   return (resOSZACP << 16) | resECX;
}

void istri_38 ( void )
{
   char* wot = "38";
   UInt(*h)(V128*,V128*) = h_pcmpistri_38;
   UInt(*s)(V128*,V128*) = s_pcmpistri_38;

   try_istri(wot,h,s, "0000000000000000", "0000000000000000"); 

   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaaaaaaaaaaaaaa"); 
   try_istri(wot,h,s, "aaaa2aaaaaaaaaaa", "aaaaaaaaaaaaaaaa"); 
   try_istri(wot,h,s, "aaaaaaaaa2aaaaaa", "aaaaaaaaaaaaaaaa"); 
   try_istri(wot,h,s, "aaaaaaaaaaaaa2aa", "aaaaaaaaaaaaaaaa"); 

   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaa2aaaaaaaaaaa"); 
   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaaaaaaa2aaaaaa"); 
   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaaaaaaaaaaaa2a"); 

   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaaaaaaaaaaaaaa"); 
   try_istri(wot,h,s, "baaaaaaaaaaaaaaa", "aaaaaaaaaaaaaaaa"); 
   try_istri(wot,h,s, "b9aaaaaaaaaaaaaa", "aaaaaaaaaaaaaaaa"); 
   try_istri(wot,h,s, "b9baaaaaaaaaaaaa", "aaaaaaaaaaaaaaaa"); 

   try_istri(wot,h,s, "b9baaaaaaaaaaaaa", "aaaaaaaaaaaaaaaa"); 
   try_istri(wot,h,s, "b9baaaaaaaaaaaaa", "aaaaaaaaaaaa7aaa"); 
   try_istri(wot,h,s, "b9baaaaaaaaaaaaa", "aaaaaaaa2aaa4aaa"); 

   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaaaaaaaaaaaaaa"); 

   try_istri(wot,h,s, "aaaaaaaaaaaa0aaa", "aaaaaaaaaaaaaaaa"); 
   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaaaaaaaaaa0aaa"); 
   try_istri(wot,h,s, "aaaaaaaaaaaa0aaa", "aaaaaaaaaaaa0aaa"); 

   try_istri(wot,h,s, "aaaaaaaa0aaaaaaa", "aaaaaaaaaaaaaaaa"); 
   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaaaaaaaaaa0aaa"); 
   try_istri(wot,h,s, "aaaaaaaa0aaaaaaa", "aaaaaaaaaaaa0aaa"); 

   try_istri(wot,h,s, "aaaaaaaaaaaa0aaa", "aaaaaaaaaaaaaaaa"); 
   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaaaaaa0aaaaaaa"); 
   try_istri(wot,h,s, "aaaaaaaaaaaa0aaa", "aaaaaaaa0aaaaaaa"); 

   try_istri(wot,h,s, "0000000000000000", "aaaaaaaa0aaaaaaa"); 
   try_istri(wot,h,s, "8000000000000000", "aaaaaaaa0aaaaaaa"); 
   try_istri(wot,h,s, "0000000000000001", "aaaaaaaa0aaaaaaa"); 

   try_istri(wot,h,s, "0000000000000000", "aaaaaaaaaaaaaaaa"); 
   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "0000000000000000"); 
}



//////////////////////////////////////////////////////////
//                                                      //
//                         main                         //
//                                                      //
//////////////////////////////////////////////////////////

int main ( void )
{
   istri_4A();
   istri_3A();
   istri_08();
   istri_1A();
   istri_02();
   istri_0C();
   istri_12();
   istri_44();
   istri_00();
   istri_38();
   return 0;
}
