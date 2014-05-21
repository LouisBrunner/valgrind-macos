
/* Tests in detail the core arithmetic for pcmp{e,i}str{i,m} using
   pcmpistri to drive it.  Does not check the e-vs-i or i-vs-m
   aspect. */

#include <string.h>
#include <stdio.h>
#include <assert.h>

typedef  unsigned int   UInt;
typedef  signed int     Int;
typedef  unsigned char  UChar;
typedef  unsigned short UShort;
typedef  unsigned long long int ULong;
typedef  UChar          Bool;
#define False ((Bool)0)
#define True  ((Bool)1)

//typedef  unsigned char  V128[16];
typedef
   union {
      UChar  uChar[16];
      UShort uShort[8];
      UInt   uInt[4];
      UInt   w32[4];
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
   for (i = 0; i < 8; i++) {
      res |=  ((arg->uShort[i] == 0) ? 1 : 0) << i;
   }
   return res;
}

//////////////////////////////////////////////////////////
//                                                      //
//                       GENERAL                        //
//                                                      //
//////////////////////////////////////////////////////////


/* Given partial results from a 16-bit pcmpXstrX operation (intRes1,
   basically), generate an I- or M-format output value, also the new
   OSZACP flags.  */
static
void PCMPxSTRx_WRK_gen_output_fmt_I_wide ( /*OUT*/V128* resV,
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
   intRes2 &= 0xFF;

   // generate I-format output (an index in ECX)
   // generate ecx value
   UInt newECX = 0;
   if (idx) {
     // index of ms-1-bit
     newECX = intRes2 == 0 ? 8 : (31 - clz32(intRes2));
   } else {
     // index of ls-1-bit
     newECX = intRes2 == 0 ? 8 : ctz32(intRes2);
   }

   resV->w32[0] = newECX;
   resV->w32[1] = 0;
   resV->w32[2] = 0;
   resV->w32[3] = 0;

   // generate new flags, common to all ISTRI and ISTRM cases
   *resOSZACP    // A, P are zero
     = ((intRes2 == 0) ? 0 : MASK_C) // C == 0 iff intRes2 == 0
     | ((zmaskL == 0)  ? 0 : MASK_Z) // Z == 1 iff any in argL is 0
     | ((zmaskR == 0)  ? 0 : MASK_S) // S == 1 iff any in argR is 0
     | ((intRes2 & 1) << SHIFT_O);   // O == IntRes2[0]
}

/* Compute result and new OSZACP flags for all PCMP{E,I}STR{I,M}
   variants on 16-bit characters.

   For xSTRI variants, the new ECX value is placed in the 32 bits
   pointed to by *resV, and the top 96 bits are zeroed.  For xSTRM
   variants, the result is a 128 bit value and is placed at *resV in
   the obvious way.

   For all variants, the new OSZACP value is placed at *resOSZACP.

   argLV and argRV are the vector args.  The caller must prepare a
   8-bit mask for each, zmaskL and zmaskR.  For ISTRx variants this
   must be 1 for each zero byte of of the respective arg.  For ESTRx
   variants this is derived from the explicit length indication, and
   must be 0 in all places except at the bit index corresponding to
   the valid length (0 .. 8).  If the valid length is 8 then the
   mask must be all zeroes.  In all cases, bits 31:8 must be zero.

   imm8 is the original immediate from the instruction.  isSTRM
   indicates whether this is a xSTRM or xSTRI variant, which controls
   how much of *res is written.

   If the given imm8 case can be handled, the return value is True.
   If not, False is returned, and neither *res not *resOSZACP are
   altered.
*/

Bool pcmpXstrX_WRK_wide ( /*OUT*/V128* resV,
			  /*OUT*/UInt* resOSZACP,
			  V128* argLV,  V128* argRV,
			  UInt zmaskL, UInt zmaskR,
			  UInt imm8,   Bool isxSTRM )
{
   assert(imm8 < 0x80);
   assert((zmaskL >> 8) == 0);
   assert((zmaskR >> 8) == 0);

   /* Explicitly reject any imm8 values that haven't been validated,
      even if they would probably work.  Life is too short to have
      unvalidated cases in the code base. */
   switch (imm8) {
      case 0x01: case 0x03: case 0x09: case 0x0B: case 0x0D:
      case 0x13:            case 0x1B:
                            case 0x39: case 0x3B:
                 case 0x45:            case 0x4B:
         break;
      default:
         return False;
   }

   UInt fmt = (imm8 >> 0) & 3; // imm8[1:0]  data format
   UInt agg = (imm8 >> 2) & 3; // imm8[3:2]  aggregation fn
   UInt pol = (imm8 >> 4) & 3; // imm8[5:4]  polarity
   UInt idx = (imm8 >> 6) & 1; // imm8[6]    1==msb/bytemask

   /*----------------------------------------*/
   /*-- strcmp on wide data                --*/
   /*----------------------------------------*/

   if (agg == 2/*equal each, aka strcmp*/
       && (fmt == 1/*uw*/ || fmt == 3/*sw*/)) {
      Int    i;
      UShort* argL = (UShort*)argLV;
      UShort* argR = (UShort*)argRV;
      UInt boolResII = 0;
      for (i = 7; i >= 0; i--) {
         UShort cL  = argL[i];
         UShort cR  = argR[i];
         boolResII = (boolResII << 1) | (cL == cR ? 1 : 0);
      }
      UInt validL = ~(zmaskL | -zmaskL);  // not(left(zmaskL))
      UInt validR = ~(zmaskR | -zmaskR);  // not(left(zmaskR))

      // do invalidation, common to all equal-each cases
      UInt intRes1
         = (boolResII & validL & validR)  // if both valid, use cmpres
           | (~ (validL | validR));       // if both invalid, force 1
                                          // else force 0
      intRes1 &= 0xFF;

      // generate I-format output
      PCMPxSTRx_WRK_gen_output_fmt_I_wide(
         resV, resOSZACP,
         intRes1, zmaskL, zmaskR, validL, pol, idx
      );

      return True;
   }

   /*----------------------------------------*/
   /*-- set membership on wide data        --*/
   /*----------------------------------------*/

   if (agg == 0/*equal any, aka find chars in a set*/
       && (fmt == 1/*uw*/ || fmt == 3/*sw*/)) {
      /* argL: the string,  argR: charset */
      UInt   si, ci;
      UShort* argL    = (UShort*)argLV;
      UShort* argR    = (UShort*)argRV;
      UInt   boolRes = 0;
      UInt   validL  = ~(zmaskL | -zmaskL);  // not(left(zmaskL))
      UInt   validR  = ~(zmaskR | -zmaskR);  // not(left(zmaskR))

      for (si = 0; si < 8; si++) {
         if ((validL & (1 << si)) == 0)
            // run off the end of the string.
            break;
         UInt m = 0;
         for (ci = 0; ci < 8; ci++) {
            if ((validR & (1 << ci)) == 0) break;
            if (argR[ci] == argL[si]) { m = 1; break; }
         }
         boolRes |= (m << si);
      }

      // boolRes is "pre-invalidated"
      UInt intRes1 = boolRes & 0xFF;

      // generate I-format output
      PCMPxSTRx_WRK_gen_output_fmt_I_wide(
         resV, resOSZACP,
         intRes1, zmaskL, zmaskR, validL, pol, idx
      );

      return True;
   }

   /*----------------------------------------*/
   /*-- substring search on wide data      --*/
   /*----------------------------------------*/

   if (agg == 3/*equal ordered, aka substring search*/
       && (fmt == 1/*uw*/ || fmt == 3/*sw*/)) {

      /* argL: haystack,  argR: needle */
      UInt   ni, hi;
      UShort* argL    = (UShort*)argLV;
      UShort* argR    = (UShort*)argRV;
      UInt   boolRes = 0;
      UInt   validL  = ~(zmaskL | -zmaskL);  // not(left(zmaskL))
      UInt   validR  = ~(zmaskR | -zmaskR);  // not(left(zmaskR))
      for (hi = 0; hi < 8; hi++) {
         UInt m = 1;
         for (ni = 0; ni < 8; ni++) {
            if ((validR & (1 << ni)) == 0) break;
            UInt i = ni + hi;
            if (i >= 8) break;
            if (argL[i] != argR[ni]) { m = 0; break; }
         }
         boolRes |= (m << hi);
         if ((validL & (1 << hi)) == 0)
            // run off the end of the haystack
            break;
      }

      // boolRes is "pre-invalidated"
      UInt intRes1 = boolRes & 0xFF;

      // generate I-format output
      PCMPxSTRx_WRK_gen_output_fmt_I_wide(
         resV, resOSZACP,
         intRes1, zmaskL, zmaskR, validL, pol, idx
      );

      return True;
   }

   /*----------------------------------------*/
   /*-- ranges, unsigned wide data         --*/
   /*----------------------------------------*/

   if (agg == 1/*ranges*/
       && fmt == 1/*uw*/) {

      /* argL: string,  argR: range-pairs */
      UInt   ri, si;
      UShort* argL    = (UShort*)argLV;
      UShort* argR    = (UShort*)argRV;
      UInt   boolRes = 0;
      UInt   validL  = ~(zmaskL | -zmaskL);  // not(left(zmaskL))
      UInt   validR  = ~(zmaskR | -zmaskR);  // not(left(zmaskR))
      for (si = 0; si < 8; si++) {
         if ((validL & (1 << si)) == 0)
            // run off the end of the string
            break;
         UInt m = 0;
         for (ri = 0; ri < 8; ri += 2) {
            if ((validR & (3 << ri)) != (3 << ri)) break;
            if (argR[ri] <= argL[si] && argL[si] <= argR[ri+1]) {
               m = 1; break;
            }
         }
         boolRes |= (m << si);
      }

      // boolRes is "pre-invalidated"
      UInt intRes1 = boolRes & 0xFF;

      // generate I-format output
      PCMPxSTRx_WRK_gen_output_fmt_I_wide(
         resV, resOSZACP,
         intRes1, zmaskL, zmaskR, validL, pol, idx
      );

      return True;
   }

   return False;
}

//////////////////////////////////////////////////////////
//                                                      //
//                       ISTRI_4B                       //
//                                                      //
//////////////////////////////////////////////////////////

UInt h_pcmpistri_4B ( V128* argL, V128* argR )
{
   V128 block[2];
   memcpy(&block[0], argL, sizeof(V128));
   memcpy(&block[1], argR, sizeof(V128));
   ULong res, flags;
   __asm__ __volatile__(
      "subq      $1024,  %%rsp"             "\n\t"
      "movdqu    0(%2),  %%xmm2"            "\n\t"
      "movdqu    16(%2), %%xmm11"           "\n\t"
      "pcmpistri $0x4B,  %%xmm2, %%xmm11"   "\n\t"
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

UInt s_pcmpistri_4B ( V128* argLU, V128* argRU )
{
   V128 resV;
   UInt resOSZACP, resECX;
   Bool ok
      = pcmpXstrX_WRK_wide( &resV, &resOSZACP, argLU, argRU,
			    zmask_from_V128(argLU),
			    zmask_from_V128(argRU),
			    0x4B, False/*!isSTRM*/
        );
   assert(ok);
   resECX = resV.uInt[0];
   return (resOSZACP << 16) | resECX;
}

void istri_4B ( void )
{
   char* wot = "4B";
   UInt(*h)(V128*,V128*) = h_pcmpistri_4B;
   UInt(*s)(V128*,V128*) = s_pcmpistri_4B;

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

   try_istri(wot,h,s, "aaaaaaaaaaaa00aa", "aaaaaaaaaaaaaaaa");
   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaaaaaaaaaa00aa");
   try_istri(wot,h,s, "aaaaaaaaaaaa00aa", "aaaaaaaaaaaa00aa");

   try_istri(wot,h,s, "aaaaaaaa00aaaaaa", "aaaaaaaaaaaaaaaa");
   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaaaaaaaaaa00aa");
   try_istri(wot,h,s, "aaaaaaaa00aaaaaa", "aaaaaaaaaaaa00aa");

   try_istri(wot,h,s, "aaaaaaaaaaaa00aa", "aaaaaaaaaaaaaaaa");
   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaaaaaa00aaaaaa");
   try_istri(wot,h,s, "aaaaaaaaaaaa00aa", "aaaaaaaa00aaaaaa");

   try_istri(wot,h,s, "0000000000000000", "aaaaaaaa00aaaaaa");
   try_istri(wot,h,s, "8000000000000000", "aaaaaaaa00aaaaaa");
   try_istri(wot,h,s, "0000000000000001", "aaaaaaaa00aaaaaa");

   try_istri(wot,h,s, "0000000000000000", "aaaaaaaaaaaaaaaa");
   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "0000000000000000");
}

//////////////////////////////////////////////////////////
//                                                      //
//                       ISTRI_3B                       //
//                                                      //
//////////////////////////////////////////////////////////

UInt h_pcmpistri_3B ( V128* argL, V128* argR )
{
   V128 block[2];
   memcpy(&block[0], argL, sizeof(V128));
   memcpy(&block[1], argR, sizeof(V128));
   ULong res, flags;
   __asm__ __volatile__(
      "subq      $1024,  %%rsp"             "\n\t"
      "movdqu    0(%2),  %%xmm2"            "\n\t"
      "movdqu    16(%2), %%xmm11"           "\n\t"
      "pcmpistri $0x3B,  %%xmm2, %%xmm11"   "\n\t"
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

UInt s_pcmpistri_3B ( V128* argLU, V128* argRU )
{
   V128 resV;
   UInt resOSZACP, resECX;
   Bool ok
      = pcmpXstrX_WRK_wide( &resV, &resOSZACP, argLU, argRU,
			    zmask_from_V128(argLU),
			    zmask_from_V128(argRU),
			    0x3B, False/*!isSTRM*/
        );
   assert(ok);
   resECX = resV.uInt[0];
   return (resOSZACP << 16) | resECX;
}

void istri_3B ( void )
{
   char* wot = "3B";
   UInt(*h)(V128*,V128*) = h_pcmpistri_3B;
   UInt(*s)(V128*,V128*) = s_pcmpistri_3B;

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

   try_istri(wot,h,s, "aaaaaaaaaaaa00aa", "aaaaaaaaaaaaaaaa");
   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaaaaaaaaaa00aa");
   try_istri(wot,h,s, "aaaaaaaaaaaa00aa", "aaaaaaaaaaaa00aa");

   try_istri(wot,h,s, "aaaaaaaa00aaaaaa", "aaaaaaaaaaaaaaaa");
   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaaaaaaaaaa00aa");
   try_istri(wot,h,s, "aaaaaaaa00aaaaaa", "aaaaaaaaaaaa00aa");

   try_istri(wot,h,s, "aaaaaaaaaaaa00aa", "aaaaaaaaaaaaaaaa");
   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaaaaaa00aaaaaa");
   try_istri(wot,h,s, "aaaaaaaaaaaa00aa", "aaaaaaaa00aaaaaa");

   try_istri(wot,h,s, "0000000000000000", "aaaaaaaa00aaaaaa");
   try_istri(wot,h,s, "8000000000000000", "aaaaaaaa00aaaaaa");
   try_istri(wot,h,s, "0000000000000001", "aaaaaaaa00aaaaaa");

   try_istri(wot,h,s, "0000000000000000", "aaaaaaaaaaaaaaaa");
   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "0000000000000000");
}



//////////////////////////////////////////////////////////
//                                                      //
//                       ISTRI_0D                       //
//                                                      //
//////////////////////////////////////////////////////////

__attribute__((noinline))
UInt h_pcmpistri_0D ( V128* argL, V128* argR )
{
   V128 block[2];
   memcpy(&block[0], argL, sizeof(V128));
   memcpy(&block[1], argR, sizeof(V128));
   ULong res = 0, flags = 0;
   __asm__ __volatile__(
      "movdqu    0(%2),  %%xmm2"            "\n\t"
      "movdqu    16(%2), %%xmm11"           "\n\t"
      "pcmpistri $0x0D,  %%xmm2, %%xmm11"   "\n\t"
      //"pcmpistrm $0x0D,  %%xmm2, %%xmm11"   "\n\t"
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

UInt s_pcmpistri_0D ( V128* argLU, V128* argRU )
{
   V128 resV;
   UInt resOSZACP, resECX;
   Bool ok
      = pcmpXstrX_WRK_wide( &resV, &resOSZACP, argLU, argRU,
			    zmask_from_V128(argLU),
			    zmask_from_V128(argRU),
			    0x0D, False/*!isSTRM*/
        );
   assert(ok);
   resECX = resV.uInt[0];
   return (resOSZACP << 16) | resECX;
}

void istri_0D ( void )
{
   char* wot = "0D";
   UInt(*h)(V128*,V128*) = h_pcmpistri_0D;
   UInt(*s)(V128*,V128*) = s_pcmpistri_0D;

   try_istri(wot,h,s, "11111111abcdef11", "0000000000abcdef");

   try_istri(wot,h,s, "11111111abcdef11", "00abcdef00abcdef");

   try_istri(wot,h,s, "11111111abcdef11", "0000000000abcdef");
   try_istri(wot,h,s, "1111111111abcdef", "0000000000abcdef");
   try_istri(wot,h,s, "111111111111abcd", "0000000000abcdef");

   try_istri(wot,h,s, "1111abcd11abcd11", "000000000000abcd");

   try_istri(wot,h,s, "11abcd1111abcd11", "000000000000abcd");
   try_istri(wot,h,s, "abcd111111abcd11", "000000000000abcd");
   try_istri(wot,h,s, "cd11111111abcd11", "000000000000abcd");

   try_istri(wot,h,s, "01abcd11abcd1111", "000000000000abcd");
   try_istri(wot,h,s, "00abcd11abcd1111", "000000000000abcd");
   try_istri(wot,h,s, "0000cd11abcd1111", "000000000000abcd");

   try_istri(wot,h,s, "00abcd1100abcd11", "000000000000abcd");
   try_istri(wot,h,s, "00abcd110000cd11", "000000000000abcd");

   try_istri(wot,h,s, "1111111111111234", "0000000000000000");
   try_istri(wot,h,s, "1111111111111234", "0000000000000011");
   try_istri(wot,h,s, "1111111111111234", "0000000000001111");

   try_istri(wot,h,s, "1111111111111234", "1111111111111234");
   try_istri(wot,h,s, "0a11111111111111", "000000000000000a");
   try_istri(wot,h,s, "0b11111111111111", "000000000000000a");

   try_istri(wot,h,s, "b111111111111111", "0000000000000000");
   try_istri(wot,h,s, "0000000000000000", "0000000000000000");
   try_istri(wot,h,s, "123456789abcdef1", "0000000000000000");
   try_istri(wot,h,s, "0000000000000000", "123456789abcdef1");
}


//////////////////////////////////////////////////////////
//                                                      //
//                       ISTRI_09                       //
//                                                      //
//////////////////////////////////////////////////////////

UInt h_pcmpistri_09 ( V128* argL, V128* argR )
{
   V128 block[2];
   memcpy(&block[0], argL, sizeof(V128));
   memcpy(&block[1], argR, sizeof(V128));
   ULong res, flags;
   __asm__ __volatile__(
      "subq      $1024,  %%rsp"             "\n\t"
      "movdqu    0(%2),  %%xmm2"            "\n\t"
      "movdqu    16(%2), %%xmm11"           "\n\t"
      "pcmpistri $0x09,  %%xmm2, %%xmm11"   "\n\t"
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

UInt s_pcmpistri_09 ( V128* argLU, V128* argRU )
{
   V128 resV;
   UInt resOSZACP, resECX;
   Bool ok
      = pcmpXstrX_WRK_wide( &resV, &resOSZACP, argLU, argRU,
			    zmask_from_V128(argLU),
			    zmask_from_V128(argRU),
			    0x09, False/*!isSTRM*/
        );
   assert(ok);
   resECX = resV.uInt[0];
   return (resOSZACP << 16) | resECX;
}

void istri_09 ( void )
{
   char* wot = "09";
   UInt(*h)(V128*,V128*) = h_pcmpistri_09;
   UInt(*s)(V128*,V128*) = s_pcmpistri_09;

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

   try_istri(wot,h,s, "aaaaaaaaaaaa00aa", "aaaaaaaaaaaaaaaa");
   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaaaaaaaaaa00aa");
   try_istri(wot,h,s, "aaaaaaaaaaaa00aa", "aaaaaaaaaaaa00aa");

   try_istri(wot,h,s, "aaaaaaaa00aaaaaa", "aaaaaaaaaaaaaaaa");
   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaaaaaaaaaa00aa");
   try_istri(wot,h,s, "aaaaaaaa00aaaaaa", "aaaaaaaaaaaa00aa");

   try_istri(wot,h,s, "aaaaaaaaaaaa00aa", "aaaaaaaaaaaaaaaa");
   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaaaaaa00aaaaaa");
   try_istri(wot,h,s, "aaaaaaaaaaaa00aa", "aaaaaaaa00aaaaaa");

   try_istri(wot,h,s, "0000000000000000", "aaaaaaaa00aaaaaa");
   try_istri(wot,h,s, "8000000000000000", "aaaaaaaa00aaaaaa");
   try_istri(wot,h,s, "0000000000000001", "aaaaaaaa00aaaaaa");

   try_istri(wot,h,s, "0000000000000000", "aaaaaaaaaaaaaaaa");
   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "0000000000000000");
}



//////////////////////////////////////////////////////////
//                                                      //
//                       ISTRI_1B                       //
//                                                      //
//////////////////////////////////////////////////////////

UInt h_pcmpistri_1B ( V128* argL, V128* argR )
{
   V128 block[2];
   memcpy(&block[0], argL, sizeof(V128));
   memcpy(&block[1], argR, sizeof(V128));
   ULong res, flags;
   __asm__ __volatile__(
      "subq      $1024,  %%rsp"             "\n\t"
      "movdqu    0(%2),  %%xmm2"            "\n\t"
      "movdqu    16(%2), %%xmm11"           "\n\t"
      "pcmpistri $0x1B,  %%xmm2, %%xmm11"   "\n\t"
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

UInt s_pcmpistri_1B ( V128* argLU, V128* argRU )
{
   V128 resV;
   UInt resOSZACP, resECX;
   Bool ok
      = pcmpXstrX_WRK_wide( &resV, &resOSZACP, argLU, argRU,
			    zmask_from_V128(argLU),
			    zmask_from_V128(argRU),
			    0x1B, False/*!isSTRM*/
        );
   assert(ok);
   resECX = resV.uInt[0];
   return (resOSZACP << 16) | resECX;
}

void istri_1B ( void )
{
   char* wot = "1B";
   UInt(*h)(V128*,V128*) = h_pcmpistri_1B;
   UInt(*s)(V128*,V128*) = s_pcmpistri_1B;

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

   try_istri(wot,h,s, "aaaaaaaaaaaa00aa", "aaaaaaaaaaaaaaaa");
   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaaaaaaaaaa00aa");
   try_istri(wot,h,s, "aaaaaaaaaaaa00aa", "aaaaaaaaaaaa00aa");

   try_istri(wot,h,s, "aaaaaaaa00aaaaaa", "aaaaaaaaaaaaaaaa");
   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaaaaaaaaaa00aa");
   try_istri(wot,h,s, "aaaaaaaa00aaaaaa", "aaaaaaaaaaaa00aa");

   try_istri(wot,h,s, "aaaaaaaaaaaa00aa", "aaaaaaaaaaaaaaaa");
   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaaaaaa00aaaaaa");
   try_istri(wot,h,s, "aaaaaaaaaaaa00aa", "aaaaaaaa00aaaaaa");

   try_istri(wot,h,s, "0000000000000000", "aaaaaaaa00aaaaaa");
   try_istri(wot,h,s, "8000000000000000", "aaaaaaaa00aaaaaa");
   try_istri(wot,h,s, "0000000000000001", "aaaaaaaa00aaaaaa");

   try_istri(wot,h,s, "0000000000000000", "aaaaaaaaaaaaaaaa");
   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "0000000000000000");
}



//////////////////////////////////////////////////////////
//                                                      //
//                       ISTRI_03                       //
//                                                      //
//////////////////////////////////////////////////////////

UInt h_pcmpistri_03 ( V128* argL, V128* argR )
{
   V128 block[2];
   memcpy(&block[0], argL, sizeof(V128));
   memcpy(&block[1], argR, sizeof(V128));
   ULong res, flags;
   __asm__ __volatile__(
      "subq      $1024,  %%rsp"             "\n\t"
      "movdqu    0(%2),  %%xmm2"            "\n\t"
      "movdqu    16(%2), %%xmm11"           "\n\t"
      "pcmpistri $0x03,  %%xmm2, %%xmm11"   "\n\t"
//"pcmpistrm $0x03, %%xmm2, %%xmm11"   "\n\t"
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

UInt s_pcmpistri_03 ( V128* argLU, V128* argRU )
{
   V128 resV;
   UInt resOSZACP, resECX;
   Bool ok
      = pcmpXstrX_WRK_wide( &resV, &resOSZACP, argLU, argRU,
			    zmask_from_V128(argLU),
			    zmask_from_V128(argRU),
			    0x03, False/*!isSTRM*/
        );
   assert(ok);
   resECX = resV.uInt[0];
   return (resOSZACP << 16) | resECX;
}

void istri_03 ( void )
{
   char* wot = "03";
   UInt(*h)(V128*,V128*) = h_pcmpistri_03;
   UInt(*s)(V128*,V128*) = s_pcmpistri_03;

   try_istri(wot,h,s, "aacdacbdaacdaacd", "00000000000000aa");
   try_istri(wot,h,s, "aabbaabbaabbaabb", "00000000000000bb");
   try_istri(wot,h,s, "aabbccddaabbccdd", "000000000000aabb");
   try_istri(wot,h,s, "abcdabc0abcdabcd", "000000000000abcd");

   try_istri(wot,h,s, "aabbccddaabbccdd", "00000000aabbccdd");
   try_istri(wot,h,s, "00bbccddaabbccdd", "00000000aabbccdd");
   try_istri(wot,h,s, "aabbccddaa00ccdd", "00000000aabbccdd");
   try_istri(wot,h,s, "aabbccddaabb00dd", "00000000aabbccdd");
   try_istri(wot,h,s, "aabbccddaabbcc00", "00000000aabbccdd");

   try_istri(wot,h,s, "aabbccddaabbccdd", "00000000aabbccdd");
   try_istri(wot,h,s, "aabbccddaabbccdd", "00000000aa00ccdd");
   try_istri(wot,h,s, "aabbccddaabbccdd", "00000000aabb00dd");
   try_istri(wot,h,s, "aabbccddaabbccdd", "00000000aabbcc00");

   try_istri(wot,h,s, "0000000000000000", "0000000000000000");
   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaaaaaaaaaaaaaa");

   try_istri(wot,h,s, "0000abcdabcdabcd", "000000000000abcd");
   try_istri(wot,h,s, "0000abcdabcdabcd", "000000000000dcba");
   try_istri(wot,h,s, "0000aabbaabbaabb", "000000000000bbbb");
   try_istri(wot,h,s, "0000ccddaabbccdd", "00000000bbaabbaa");

   try_istri(wot,h,s, "0000ccddaabbccdd", "000000bbaabbaa00");

   try_istri(wot,h,s, "0ddc0ffeebadf00d", "00000000cafebabe");
   try_istri(wot,h,s, "0ddc0ffeebadfeed", "00000000cafebabe");
}


//////////////////////////////////////////////////////////
//                                                      //
//                       ISTRI_13                       //
//                                                      //
//////////////////////////////////////////////////////////

UInt h_pcmpistri_13 ( V128* argL, V128* argR )
{
   V128 block[2];
   memcpy(&block[0], argL, sizeof(V128));
   memcpy(&block[1], argR, sizeof(V128));
   ULong res, flags;
   __asm__ __volatile__(
      "subq      $1024,  %%rsp"             "\n\t"
      "movdqu    0(%2),  %%xmm2"            "\n\t"
      "movdqu    16(%2), %%xmm11"           "\n\t"
      "pcmpistri $0x13,  %%xmm2, %%xmm11"   "\n\t"
//"pcmpistrm $0x13, %%xmm2, %%xmm11"   "\n\t"
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

UInt s_pcmpistri_13 ( V128* argLU, V128* argRU )
{
   V128 resV;
   UInt resOSZACP, resECX;
   Bool ok
      = pcmpXstrX_WRK_wide( &resV, &resOSZACP, argLU, argRU,
			    zmask_from_V128(argLU),
			    zmask_from_V128(argRU),
			    0x13, False/*!isSTRM*/
        );
   assert(ok);
   resECX = resV.uInt[0];
   return (resOSZACP << 16) | resECX;
}

void istri_13 ( void )
{
   char* wot = "13";
   UInt(*h)(V128*,V128*) = h_pcmpistri_13;
   UInt(*s)(V128*,V128*) = s_pcmpistri_13;

   try_istri(wot,h,s, "aacdacbdaacdaacd", "00000000000000aa");
   try_istri(wot,h,s, "aabbaabbaabbaabb", "00000000000000bb");
   try_istri(wot,h,s, "aabbccddaabbccdd", "000000000000aabb");
   try_istri(wot,h,s, "abcdabc0abcdabcd", "000000000000abcd");

   try_istri(wot,h,s, "aabbccddaabbccdd", "00000000aabbccdd");
   try_istri(wot,h,s, "00bbccddaabbccdd", "00000000aabbccdd");
   try_istri(wot,h,s, "aabbccddaa00ccdd", "00000000aabbccdd");
   try_istri(wot,h,s, "aabbccddaabb00dd", "00000000aabbccdd");
   try_istri(wot,h,s, "aabbccddaabbcc00", "00000000aabbccdd");

   try_istri(wot,h,s, "aabbccddaabbccdd", "00000000aabbccdd");
   try_istri(wot,h,s, "aabbccddaabbccdd", "00000000aa00ccdd");
   try_istri(wot,h,s, "aabbccddaabbccdd", "00000000aabb00dd");
   try_istri(wot,h,s, "aabbccddaabbccdd", "00000000aabbcc00");

   try_istri(wot,h,s, "0000000000000000", "0000000000000000");
   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaaaaaaaaaaaaaa");

   try_istri(wot,h,s, "0000abcdabcdabcd", "000000000000abcd");
   try_istri(wot,h,s, "0000abcdabcdabcd", "000000000000dcba");
   try_istri(wot,h,s, "0000aabbaabbaabb", "000000000000bbbb");
   try_istri(wot,h,s, "0000ccddaabbccdd", "00000000bbaabbaa");

   try_istri(wot,h,s, "0000ccddaabbccdd", "000000bbaabbaa00");

   try_istri(wot,h,s, "0ddc0ffeebadf00d", "00000000cafebabe");
   try_istri(wot,h,s, "0ddc0ffeebadfeed", "00000000cafebabe");
}



//////////////////////////////////////////////////////////
//                                                      //
//                       ISTRI_45                       //
//                                                      //
//////////////////////////////////////////////////////////

UInt h_pcmpistri_45 ( V128* argL, V128* argR )
{
   V128 block[2];
   memcpy(&block[0], argL, sizeof(V128));
   memcpy(&block[1], argR, sizeof(V128));
   ULong res, flags;
   __asm__ __volatile__(
      "subq      $1024,  %%rsp"             "\n\t"
      "movdqu    0(%2),  %%xmm2"            "\n\t"
      "movdqu    16(%2), %%xmm11"           "\n\t"
      "pcmpistri $0x45,  %%xmm2, %%xmm11"   "\n\t"
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

UInt s_pcmpistri_45 ( V128* argLU, V128* argRU )
{
   V128 resV;
   UInt resOSZACP, resECX;
   Bool ok
      = pcmpXstrX_WRK_wide( &resV, &resOSZACP, argLU, argRU,
			    zmask_from_V128(argLU),
			    zmask_from_V128(argRU),
			    0x45, False/*!isSTRM*/
        );
   assert(ok);
   resECX = resV.uInt[0];
   return (resOSZACP << 16) | resECX;
}

void istri_45 ( void )
{
   char* wot = "45";
   UInt(*h)(V128*,V128*) = h_pcmpistri_45;
   UInt(*s)(V128*,V128*) = s_pcmpistri_45;

   try_istri(wot,h,s, "aaaabbbbccccdddd", "000000000000bbcc");
   try_istri(wot,h,s, "aaaabbbbccccdddd", "000000000000ccbb");
   try_istri(wot,h,s, "baaabbbbccccdddd", "000000000000ccbb");
   try_istri(wot,h,s, "baaabbbbccccdddc", "000000000000ccbb");

   try_istri(wot,h,s, "bbbbbbbbbbbbbbbb", "000000000000ccbb");
   try_istri(wot,h,s, "bbbbbbbb00bbbbbb", "000000000000ccbb");
   try_istri(wot,h,s, "bbbbbbbbbbbb00bb", "000000000000ccbb");
   try_istri(wot,h,s, "bbbbbbbbbbbbbb00", "000000000000ccbb");
   try_istri(wot,h,s, "0000000000000000", "000000000000ccbb");

   try_istri(wot,h,s, "0000000000000000", "0000000000000000");

   try_istri(wot,h,s, "bbbbbbbbbbbbbbbb", "000000000000ccbb");
   try_istri(wot,h,s, "bbbbbbbbbbbbbbbb", "00000000000000bb");
   try_istri(wot,h,s, "bb44bb44bb44bb44", "000000006622ccbb");

   try_istri(wot,h,s, "bb44bb44bb44bb44", "000000000022ccbb");
   try_istri(wot,h,s, "bb44bb44bb44bb44", "000000000000ccbb");
   try_istri(wot,h,s, "bb44bb44bb44bb44", "00000000000000bb");

   try_istri(wot,h,s, "0011223344556677", "0000997755442211");
   try_istri(wot,h,s, "1122334455667711", "0000997755442211");

   try_istri(wot,h,s, "0011223344556677", "0000aa8866553322");
   try_istri(wot,h,s, "1122334455667711", "0000aa8866553322");
}


//////////////////////////////////////////////////////////
//                                                      //
//                       ISTRI_01                       //
//                                                      //
//////////////////////////////////////////////////////////

UInt h_pcmpistri_01 ( V128* argL, V128* argR )
{
   V128 block[2];
   memcpy(&block[0], argL, sizeof(V128));
   memcpy(&block[1], argR, sizeof(V128));
   ULong res, flags;
   __asm__ __volatile__(
      "subq      $1024,  %%rsp"             "\n\t"
      "movdqu    0(%2),  %%xmm2"            "\n\t"
      "movdqu    16(%2), %%xmm11"           "\n\t"
      "pcmpistri $0x01,  %%xmm2, %%xmm11"   "\n\t"
//"pcmpistrm $0x01, %%xmm2, %%xmm11"   "\n\t"
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

UInt s_pcmpistri_01 ( V128* argLU, V128* argRU )
{
   V128 resV;
   UInt resOSZACP, resECX;
   Bool ok
      = pcmpXstrX_WRK_wide( &resV, &resOSZACP, argLU, argRU,
			    zmask_from_V128(argLU),
			    zmask_from_V128(argRU),
			    0x01, False/*!isSTRM*/
        );
   assert(ok);
   resECX = resV.uInt[0];
   return (resOSZACP << 16) | resECX;
}

void istri_01 ( void )
{
   char* wot = "01";
   UInt(*h)(V128*,V128*) = h_pcmpistri_01;
   UInt(*s)(V128*,V128*) = s_pcmpistri_01;

   try_istri(wot,h,s, "aacdacbdaacdaacd", "00000000000000aa");
   try_istri(wot,h,s, "aabbaabbaabbaabb", "00000000000000bb");
   try_istri(wot,h,s, "aabbccddaabbccdd", "000000000000aabb");
   try_istri(wot,h,s, "abcdabc0abcdabcd", "000000000000abcd");

   try_istri(wot,h,s, "aabbccddaabbccdd", "00000000aabbccdd");
   try_istri(wot,h,s, "00bbccddaabbccdd", "00000000aabbccdd");
   try_istri(wot,h,s, "aabbccddaa00ccdd", "00000000aabbccdd");
   try_istri(wot,h,s, "aabbccddaabb00dd", "00000000aabbccdd");
   try_istri(wot,h,s, "aabbccddaabbcc00", "00000000aabbccdd");

   try_istri(wot,h,s, "aabbccddaabbccdd", "00000000aabbccdd");
   try_istri(wot,h,s, "aabbccddaabbccdd", "00000000aa00ccdd");
   try_istri(wot,h,s, "aabbccddaabbccdd", "00000000aabb00dd");
   try_istri(wot,h,s, "aabbccddaabbccdd", "00000000aabbcc00");

   try_istri(wot,h,s, "0000000000000000", "0000000000000000");
   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaaaaaaaaaaaaaa");

   try_istri(wot,h,s, "0000abcdabcdabcd", "000000000000abcd");
   try_istri(wot,h,s, "0000abcdabcdabcd", "000000000000dcba");
   try_istri(wot,h,s, "0000aabbaabbaabb", "000000000000bbbb");
   try_istri(wot,h,s, "0000ccddaabbccdd", "00000000bbaabbaa");

   try_istri(wot,h,s, "0000ccddaabbccdd", "000000bbaabbaa00");

   try_istri(wot,h,s, "0ddc0ffeebadf00d", "00000000cafebabe");
   try_istri(wot,h,s, "0ddc0ffeebadfeed", "00000000cafebabe");
}


//////////////////////////////////////////////////////////
//                                                      //
//                       ISTRI_39                       //
//                                                      //
//////////////////////////////////////////////////////////

UInt h_pcmpistri_39 ( V128* argL, V128* argR )
{
   V128 block[2];
   memcpy(&block[0], argL, sizeof(V128));
   memcpy(&block[1], argR, sizeof(V128));
   ULong res, flags;
   __asm__ __volatile__(
      "subq      $1024,  %%rsp"             "\n\t"
      "movdqu    0(%2),  %%xmm2"            "\n\t"
      "movdqu    16(%2), %%xmm11"           "\n\t"
      "pcmpistri $0x39,  %%xmm2, %%xmm11"   "\n\t"
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

UInt s_pcmpistri_39 ( V128* argLU, V128* argRU )
{
   V128 resV;
   UInt resOSZACP, resECX;
   Bool ok
      = pcmpXstrX_WRK_wide( &resV, &resOSZACP, argLU, argRU,
			    zmask_from_V128(argLU),
			    zmask_from_V128(argRU),
			    0x39, False/*!isSTRM*/
        );
   assert(ok);
   resECX = resV.uInt[0];
   return (resOSZACP << 16) | resECX;
}

void istri_39 ( void )
{
   char* wot = "39";
   UInt(*h)(V128*,V128*) = h_pcmpistri_39;
   UInt(*s)(V128*,V128*) = s_pcmpistri_39;

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

   try_istri(wot,h,s, "aaaaaaaaaaaa00aa", "aaaaaaaaaaaaaaaa");
   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaaaaaaaaaa00aa");
   try_istri(wot,h,s, "aaaaaaaaaaaa00aa", "aaaaaaaaaaaa00aa");

   try_istri(wot,h,s, "aaaaaaaa00aaaaaa", "aaaaaaaaaaaaaaaa");
   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaaaaaaaaaa00aa");
   try_istri(wot,h,s, "aaaaaaaa00aaaaaa", "aaaaaaaaaaaa00aa");

   try_istri(wot,h,s, "aaaaaaaaaaaa00aa", "aaaaaaaaaaaaaaaa");
   try_istri(wot,h,s, "aaaaaaaaaaaaaaaa", "aaaaaaaa00aaaaaa");
   try_istri(wot,h,s, "aaaaaaaaaaaa00aa", "aaaaaaaa00aaaaaa");

   try_istri(wot,h,s, "0000000000000000", "aaaaaaaa00aaaaaa");
   try_istri(wot,h,s, "8000000000000000", "aaaaaaaa00aaaaaa");
   try_istri(wot,h,s, "0000000000000001", "aaaaaaaa00aaaaaa");

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
   istri_4B();
   istri_3B();
   istri_09();
   istri_1B();
   istri_03();
   istri_0D();
   istri_13();
   istri_45();
   istri_01();
   istri_39();
   return 0;
}
