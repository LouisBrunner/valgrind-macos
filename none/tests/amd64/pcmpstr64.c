
#include <string.h>
#include <stdio.h>
#include <assert.h>

typedef  unsigned char  V128[16];
typedef  unsigned int   UInt;
typedef  signed int     Int;
typedef  unsigned char  UChar;
typedef  unsigned long long int ULong;

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
      (*dst)[i] = xx;
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
   /* signed bytes  (also works for unsigned)
      equal each    (straightforward parallel compare)
      polarity +    (IntRes2 = IntRes1)
      index 1       (want index of ms 1 bit)
   */
   Int    i;
   UChar* argL = (UChar*)argLU;
   UChar* argR = (UChar*)argRU;
   UInt boolResII = 0, zmaskL = 0, zmaskR = 0;
   for (i = 15; i >= 0; i--) {
      UChar cL  = argL[i];
      UChar cR  = argR[i];
      zmaskL    = (zmaskL << 1)    | (cL == 0  ? 1 : 0);
      zmaskR    = (zmaskR << 1)    | (cR == 0  ? 1 : 0);
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

   // polarity: +
   UInt intRes2 = intRes1;

   // generate ecx value, common to all index-of-ms-1-bit cases
   UInt newECX = intRes2 == 0 ? 16 : (31 - clz32(intRes2));

   // generate new flags, common to all ISTRI and ISTRM cases
   UInt newFlags    // A, P are zero
      = ((intRes2 == 0) ? 0 : MASK_C) // C == 0 iff intRes2 == 0
      | ((zmaskL == 0)  ? 0 : MASK_Z) // Z == 1 iff any in argL is 0
      | ((zmaskR == 0)  ? 0 : MASK_S) // S == 1 iff any in argR is 0
      | ((intRes2 & 1) << SHIFT_O);   // O == IntRes2[0]

   return (newFlags << 16) | newECX;
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
   /* signed bytes      (also works for unsigned)
      equal each        (straightforward parallel compare)
      polarity Masked-  (IntRes2 = IntRes1 ^ validL)
      index 0           (want index of ls 1 bit)
   */
   Int    i;
   UChar* argL = (UChar*)argLU;
   UChar* argR = (UChar*)argRU;
   UInt boolResII = 0, zmaskL = 0, zmaskR = 0;
   for (i = 15; i >= 0; i--) {
      UChar cL  = argL[i];
      UChar cR  = argR[i];
      zmaskL    = (zmaskL << 1)    | (cL == 0  ? 1 : 0);
      zmaskR    = (zmaskR << 1)    | (cR == 0  ? 1 : 0);
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

   // polarity: Masked-
   UInt intRes2 = (intRes1 ^ validL) & 0xFFFF;

   // generate ecx value, common to all index-of-ls-1-bit cases
   UInt newECX = intRes2 == 0 ? 16 : ctz32(intRes2);

   // generate new flags, common to all ISTRI and ISTRM cases
   UInt newFlags    // A, P are zero
      = ((intRes2 == 0) ? 0 : MASK_C) // C == 0 iff intRes2 == 0
      | ((zmaskL == 0)  ? 0 : MASK_Z) // Z == 1 iff any in argL is 0
      | ((zmaskR == 0)  ? 0 : MASK_S) // S == 1 iff any in argR is 0
      | ((intRes2 & 1) << SHIFT_O);   // O == IntRes2[0]

   return (newFlags << 16) | newECX;
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
      "movdqa    0(%2),  %%xmm2"            "\n\t"
      "movdqa    16(%2), %%xmm11"           "\n\t"
      "pcmpistri $0x0C,  %%xmm2, %%xmm11"   "\n\t"
      //"pcmpistrm $0x0C,  %%xmm2, %%xmm11"   "\n\t"
      //"movd %%xmm0, %%ecx" "\n\t"
      "pushfq"                              "\n\t"
      "popq      %%rdx"                     "\n\t"
      "movq      %%rcx,  %0"                "\n\t"
      "movq      %%rdx,  %1"                "\n\t"
      : /*out*/ "=r"(res), "=r"(flags) : "r"/*in*/(&block[0][0])
      : "rcx","rdx","xmm0","xmm2","xmm11","cc","memory"
   );
   return ((flags & 0x8D5) << 16) | (res & 0xFFFF);
}

UInt s_pcmpistri_0C ( V128* argLU, V128* argRU )
{
   /* unsigned bytes
      equal ordered     (substring search)
      polarity +        (IntRes2 = IntRes1)
      index 0           (want index of ls 1 bit)

      argL: haystack,  argR: needle
   */
   UInt i, hi, ni;
   UChar* argL = (UChar*)argLU;
   UChar* argR = (UChar*)argRU;
   UInt boolRes = 0, zmaskL = 0, zmaskR = 0;
   UInt keepSearching = 1;
   for (i = 0; i < 16; i++) {
      UChar cL  = argL[i];
      UChar cR  = argR[i];
      zmaskL    = (zmaskL >> 1) | (cL == 0 ? (1 << 15) : 0);
      zmaskR    = (zmaskR >> 1) | (cR == 0 ? (1 << 15) : 0);

      if (argL[i] == 0) {
        // run off the end of the haystack.
        keepSearching = 0;
      } 

      UInt m = 1;
      if (keepSearching) {
         for (ni = 0; ni < 16; ni++) {
            if (argR[ni] == 0) break;
            hi = ni + i;
            if (hi >= 16) break;
            if (argL[hi] != argR[ni]) { m = 0; break; }
         }
      } else {
         m = 0;
      }
      boolRes = (boolRes >> 1) | (m << 15);

   }

   // boolRes is "pre-invalidated"
   UInt intRes1 = boolRes & 0xFFFF;

   // polarity: +
   UInt intRes2 = intRes1;

   // generate ecx value, common to all index-of-ls-1-bit cases
   UInt newECX = intRes2 == 0 ? 16 : ctz32(intRes2);

   // generate new flags, common to all ISTRI and ISTRM cases
   UInt newFlags    // A, P are zero
      = ((intRes2 == 0) ? 0 : MASK_C) // C == 0 iff intRes2 == 0
      | ((zmaskL == 0)  ? 0 : MASK_Z) // Z == 1 iff any in argL is 0
      | ((zmaskR == 0)  ? 0 : MASK_S) // S == 1 iff any in argR is 0
      | ((intRes2 & 1) << SHIFT_O);   // O == IntRes2[0]

   return (newFlags << 16) | newECX;
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
   /* unsigned bytes    (also works for unsigned)
      equal each        (straightforward parallel compare)
      polarity +        (IntRes2 = IntRes1)
      index 0           (want index of ls 1 bit)
   */
   Int    i;
   UChar* argL = (UChar*)argLU;
   UChar* argR = (UChar*)argRU;
   UInt boolResII = 0, zmaskL = 0, zmaskR = 0;
   for (i = 15; i >= 0; i--) {
      UChar cL  = argL[i];
      UChar cR  = argR[i];
      zmaskL    = (zmaskL << 1)    | (cL == 0  ? 1 : 0);
      zmaskR    = (zmaskR << 1)    | (cR == 0  ? 1 : 0);
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

   // polarity: +
   UInt intRes2 = intRes1;

   // generate ecx value, common to all index-of-ls-1-bit cases
   UInt newECX = intRes2 == 0 ? 16 : ctz32(intRes2);

   // generate new flags, common to all ISTRI and ISTRM cases
   UInt newFlags    // A, P are zero
      = ((intRes2 == 0) ? 0 : MASK_C) // C == 0 iff intRes2 == 0
      | ((zmaskL == 0)  ? 0 : MASK_Z) // Z == 1 iff any in argL is 0
      | ((zmaskR == 0)  ? 0 : MASK_S) // S == 1 iff any in argR is 0
      | ((intRes2 & 1) << SHIFT_O);   // O == IntRes2[0]

   return (newFlags << 16) | newECX;
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
//                         main                         //
//                                                      //
//////////////////////////////////////////////////////////

int main ( void )
{
   istri_4A();
   istri_3A();
   istri_08();
   istri_0C();
   return 0;
}
