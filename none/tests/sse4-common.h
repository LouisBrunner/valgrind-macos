/* Common infrastructure for SSE4 tests (both x86 and amd64) */

#ifndef __SSE4_COMMON_H
#define __SSE4_COMMON_H

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "tests/malloc.h"
#include <string.h>

typedef  unsigned char           V128[16];
typedef  unsigned int            UInt;
typedef  signed int              Int;
typedef  unsigned char           UChar;
typedef  unsigned long long int  ULong;

typedef  unsigned char           Bool;
#define False ((Bool)0)
#define True  ((Bool)1)


typedef
   struct {
      V128 arg1;
      V128 arg2;
      V128 res;
   }
   RRArgs;

typedef
   struct {
      V128 arg1;
      V128 res;
   }
   RMArgs;

static UChar randUChar ( void )
{
   static UInt seed = 80021;
   seed = 1103515245 * seed + 12345;
   return (seed >> 17) & 0xFF;
}


static ULong randULong ( void )
{
   Int i;
   ULong r = 0;
   for (i = 0; i < 8; i++) {
      r = (r << 8) | (ULong)(0xFF & randUChar());
   }
   return r;
}

static void randV128 ( V128* v )
{
   Int i;
   for (i = 0; i < 16; i++)
      (*v)[i] = randUChar();
}

static void showV128 ( V128* v )
{
   Int i;
   for (i = 15; i >= 0; i--)
      printf("%02x", (Int)(*v)[i]);
}

static void showMaskedV128 ( V128* v, V128* mask )
{
   Int i;
   for (i = 15; i >= 0; i--)
      printf("%02x", (Int)( ((*v)[i]) & ((*mask)[i]) ));
}

static void do64HLtoV128 ( /*OUT*/V128* res, ULong wHi, ULong wLo )
{
   // try to sidestep strict-aliasing snafus by memcpying explicitly
   UChar* p = (UChar*)res;
   memcpy(&p[8], (UChar*)&wHi, 8);
   memcpy(&p[0], (UChar*)&wLo, 8);
}

static void showIGVV( char* rOrM, char* op, Int imm,
                      ULong src64, V128* dst, V128* res )
{
   printf("%s %10s $%d ", rOrM, op, imm);
   printf("%016llx", src64);
   printf(" ");
   showV128(dst);
   printf(" ");
   showV128(res);
   printf("\n");
}

static void showIAG ( char* rOrM, char* op, Int imm,
                      V128* argL, ULong argR, ULong res )
{
   printf("%s %10s $%d ", rOrM, op, imm);
   showV128(argL);
   printf(" ");
   printf("%016llx", argR);
   printf(" ");
   printf("%016llx", res);
   printf("\n");
}

static void showIAA ( char* rOrM, char* op, Int imm, RRArgs* rra, V128* rmask )
{
   printf("%s %10s $%d ", rOrM, op, imm);
   showV128(&rra->arg1);
   printf(" ");
   showV128(&rra->arg2);
   printf(" ");
   showMaskedV128(&rra->res, rmask);
   printf("\n");
}

static void showAA ( char* rOrM, char* op, RRArgs* rra, V128* rmask )
{
   printf("%s %10s ", rOrM, op);
   showV128(&rra->arg1);
   printf(" ");
   showV128(&rra->arg2);
   printf(" ");
   showMaskedV128(&rra->res, rmask);
   printf("\n");
}

/* Note: these are little endian.  Hence first byte is the least
   significant byte of lane zero. */

/* Mask for insns where all result bits are non-approximated. */
static V128 AllMask  = { 0xFF,0xFF,0xFF,0xFF, 0xFF,0xFF,0xFF,0xFF,
                         0xFF,0xFF,0xFF,0xFF, 0xFF,0xFF,0xFF,0xFF };

/* Mark for insns which produce approximated vector short results. */
__attribute__((unused))
static V128 ApproxPS = { 0x00,0x00,0x80,0xFF, 0x00,0x00,0x80,0xFF,
                         0x00,0x00,0x80,0xFF, 0x00,0x00,0x80,0xFF };

/* Mark for insns which produce approximated scalar short results. */
__attribute__((unused))
static V128 ApproxSS = { 0x00,0x00,0x80,0xFF, 0xFF,0xFF,0xFF,0xFF,
                         0xFF,0xFF,0xFF,0xFF, 0xFF,0xFF,0xFF,0xFF };

static V128 fives    = { 0x55,0x55,0x55,0x55, 0x55,0x55,0x55,0x55,
                         0x55,0x55,0x55,0x55, 0x55,0x55,0x55,0x55 };

static V128 zeroes   = { 0x00,0x00,0x00,0x00, 0x00,0x00,0x00,0x00,
                         0x00,0x00,0x00,0x00, 0x00,0x00,0x00,0x00 };

/* Helper functions for creating special float values */
static inline double mkPosInf ( void ) { return 1.0 / 0.0; }
static inline double mkNegInf ( void ) { return -mkPosInf(); }
static inline double mkPosNan ( void ) { return 0.0 / 0.0; }
static inline double mkNegNan ( void ) { return -mkPosNan(); }

/* Macros for testing XMM register to register and memory to register operations */

#define DO_imm_r_r(_opname, _imm, _src, _dst)  \
   {  \
      V128 _tmp;  \
      __asm__ __volatile__(  \
         "movupd (%0), %%xmm2"    "\n\t"  \
         "movupd (%1), %%xmm11"   "\n\t"  \
         _opname " $" #_imm ", %%xmm2, %%xmm11"  "\n\t"  \
         "movupd %%xmm11, (%2)" "\n"  \
         : /*out*/ : /*in*/ "r"(&(_src)), "r"(&(_dst)), "r"(&(_tmp))  \
         : "cc", "memory", "xmm2", "xmm11"                            \
      );  \
      RRArgs rra;  \
      memcpy(&rra.arg1, &(_src), sizeof(V128));  \
      memcpy(&rra.arg2, &(_dst), sizeof(V128));  \
      memcpy(&rra.res,  &(_tmp), sizeof(V128));  \
      showIAA("r", (_opname), (_imm), &rra, &AllMask);  \
   }

#define DO_imm_m_r(_opname, _imm, _src, _dst)  \
   {  \
      V128 _tmp;  \
      V128* _srcM = memalign16(sizeof(V128));  \
      memcpy(_srcM, &(_src), sizeof(V128));  \
      __asm__ __volatile__(  \
         "movupd (%1), %%xmm11"   "\n\t"  \
         _opname " $" #_imm ", (%0), %%xmm11"  "\n\t"  \
         "movupd %%xmm11, (%2)" "\n"  \
         : /*out*/ : /*in*/ "r"(_srcM), "r"(&(_dst)), "r"(&(_tmp))  \
         : "cc", "memory", "xmm11"  \
      );  \
      RRArgs rra;  \
      memcpy(&rra.arg1, &(_src), sizeof(V128));  \
      memcpy(&rra.arg2, &(_dst), sizeof(V128));  \
      memcpy(&rra.res,  &(_tmp), sizeof(V128));  \
      showIAA("m", (_opname), (_imm), &rra, &AllMask);  \
      free(_srcM);  \
   }

#define DO_imm_mandr_r(_opname, _imm, _src, _dst)  \
      DO_imm_r_r( _opname, _imm, _src, _dst ) \
      DO_imm_m_r( _opname, _imm, _src, _dst )

#define DO_r_r(_opname, _src, _dst)  \
   {  \
      V128 _tmp;  \
      __asm__ __volatile__(  \
         "movupd (%0), %%xmm2"    "\n\t"  \
         "movupd (%1), %%xmm11"   "\n\t"  \
         _opname " %%xmm2, %%xmm11"  "\n\t"  \
         "movupd %%xmm11, (%2)" "\n"  \
         : /*out*/ : /*in*/ "r"(&(_src)), "r"(&(_dst)), "r"(&(_tmp))  \
         : "cc", "memory", "xmm2", "xmm11"  \
      );  \
      RRArgs rra;  \
      memcpy(&rra.arg1, &(_src), sizeof(V128));  \
      memcpy(&rra.arg2, &(_dst), sizeof(V128));  \
      memcpy(&rra.res,  &(_tmp), sizeof(V128));  \
      showAA("r", (_opname), &rra, &AllMask);  \
   }

#define DO_m_r(_opname, _src, _dst)  \
   {  \
      V128 _tmp;  \
      V128* _srcM = memalign16(sizeof(V128));  \
      memcpy(_srcM, &(_src), sizeof(V128));  \
      __asm__ __volatile__(  \
         "movupd (%1), %%xmm11"   "\n\t"  \
         _opname " (%0), %%xmm11"  "\n\t"  \
         "movupd %%xmm11, (%2)" "\n"  \
         : /*out*/ : /*in*/ "r"(_srcM), "r"(&(_dst)), "r"(&(_tmp))  \
         : "cc", "memory", "xmm11"  \
      );  \
      RRArgs rra;  \
      memcpy(&rra.arg1, &(_src), sizeof(V128));  \
      memcpy(&rra.arg2, &(_dst), sizeof(V128));  \
      memcpy(&rra.res,  &(_tmp), sizeof(V128));  \
      showAA("m", (_opname), &rra, &AllMask);  \
      free(_srcM);  \
   }

#define DO_mandr_r(_opname, _src, _dst)  \
      DO_r_r(_opname, _src, _dst) \
      DO_m_r(_opname, _src, _dst)

#endif /* __SSE4_COMMON_H */
