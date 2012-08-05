
/*--------------------------------------------------------------------*/
/*--- Entirely standalone libc stuff.                 m_libcbase.c ---*/
/*--------------------------------------------------------------------*/
 
/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2012 Julian Seward 
      jseward@acm.org

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307, USA.

   The GNU General Public License is contained in the file COPYING.
*/

#include "pub_core_basics.h"
#include "pub_core_libcbase.h"

/* ---------------------------------------------------------------------
   Char functions.
   ------------------------------------------------------------------ */

Bool VG_(isspace) ( Char c )
{
   return (c == ' '  || c == '\n' || c == '\t' || 
           c == '\f' || c == '\v' || c == '\r');
}

Bool VG_(isdigit) ( Char c )
{
   return (c >= '0' && c <= '9');
}

/* ---------------------------------------------------------------------
   Converting strings to numbers
   ------------------------------------------------------------------ */

static Bool is_dec_digit(Char c, Long* digit)
{
   if (c >= '0' && c <= '9') { *digit = (Long)(c - '0'); return True; }
   return False;
}

static Bool is_hex_digit(Char c, Long* digit)
{
   if (c >= '0' && c <= '9') { *digit = (Long)(c - '0');        return True; }
   if (c >= 'A' && c <= 'F') { *digit = (Long)((c - 'A') + 10); return True; }
   if (c >= 'a' && c <= 'f') { *digit = (Long)((c - 'a') + 10); return True; }
   return False;
}

Long VG_(strtoll10) ( Char* str, Char** endptr )
{
   Bool neg = False, converted = False;
   Long n = 0, digit = 0;
   Char* str0 = str;

   // Skip leading whitespace.
   while (VG_(isspace)(*str)) str++;

   // Allow a leading '-' or '+'.
   if (*str == '-') { str++; neg = True; }
   else if (*str == '+') { str++; }

   while (is_dec_digit(*str, &digit)) {
      converted = True;          // Ok, we've actually converted a digit.
      n = 10*n + digit;
      str++;
   }

   if (!converted) str = str0;   // If nothing converted, endptr points to
   if (neg) n = -n;              //   the start of the string.
   if (endptr) *endptr = str;    // Record first failing character.
   return n;
}

ULong VG_(strtoull10) ( Char* str, Char** endptr )
{
   Bool converted = False;
   ULong n = 0;
   Long digit = 0;
   Char* str0 = str;

   // Skip leading whitespace.
   while (VG_(isspace)(*str)) str++;

   // Allow a leading '+'.
   if (*str == '+') { str++; }

   while (is_dec_digit(*str, &digit)) {
      converted = True;          // Ok, we've actually converted a digit.
      n = 10*n + digit;
      str++;
   }

   if (!converted) str = str0;   // If nothing converted, endptr points to
   //   the start of the string.
   if (endptr) *endptr = str;    // Record first failing character.
   return n;
}

Long VG_(strtoll16) ( Char* str, Char** endptr )
{
   Bool neg = False, converted = False;
   Long n = 0, digit = 0;
   Char* str0 = str;

   // Skip leading whitespace.
   while (VG_(isspace)(*str)) str++;

   // Allow a leading '-' or '+'.
   if (*str == '-') { str++; neg = True; }
   else if (*str == '+') { str++; }

   // Allow leading "0x", but only if there's a hex digit
   // following it.
   if (*str == '0'
    && (*(str+1) == 'x' || *(str+1) == 'X')
    && is_hex_digit( *(str+2), &digit )) {
      str += 2;
   }

   while (is_hex_digit(*str, &digit)) {
      converted = True;          // Ok, we've actually converted a digit.
      n = 16*n + digit;
      str++;
   }

   if (!converted) str = str0;   // If nothing converted, endptr points to
   if (neg) n = -n;              //   the start of the string.
   if (endptr) *endptr = str;    // Record first failing character.
   return n;
}

ULong VG_(strtoull16) ( Char* str, Char** endptr )
{
   Bool converted = False;
   ULong n = 0;
   Long digit = 0;
   Char* str0 = str;

   // Skip leading whitespace.
   while (VG_(isspace)(*str)) str++;

   // Allow a leading '+'.
   if (*str == '+') { str++; }

   // Allow leading "0x", but only if there's a hex digit
   // following it.
   if (*str == '0'
    && (*(str+1) == 'x' || *(str+1) == 'X')
    && is_hex_digit( *(str+2), &digit )) {
      str += 2;
   }

   while (is_hex_digit(*str, &digit)) {
      converted = True;          // Ok, we've actually converted a digit.
      n = 16*n + digit;
      str++;
   }

   if (!converted) str = str0;   // If nothing converted, endptr points to
   //   the start of the string.
   if (endptr) *endptr = str;    // Record first failing character.
   return n;
}

double VG_(strtod) ( Char* str, Char** endptr )
{
   Bool neg = False;
   Long digit;
   double n = 0, frac = 0, x = 0.1;

   // Skip leading whitespace.
   while (VG_(isspace)(*str)) str++;

   // Allow a leading '-' or '+'.
   if (*str == '-') { str++; neg = True; }
   else if (*str == '+') { str++; }

   while (is_dec_digit(*str, &digit)) {
      n = 10*n + digit;
      str++;
   }

   if (*str == '.') {
      str++;
      while (is_dec_digit(*str, &digit)) {
         frac += x*digit;
         x /= 10;
         str++;
      }
   }

   n += frac;
   if (neg) n = -n;
   if (endptr) *endptr = str;    // Record first failing character.
   return n;
}

Char VG_(tolower) ( Char c )
{
   if ( c >= 'A'  &&  c <= 'Z' ) {
      return c - 'A' + 'a';
   } else {
      return c;
   }
}

/* ---------------------------------------------------------------------
   String functions
   ------------------------------------------------------------------ */

SizeT VG_(strlen) ( const Char* str )
{
   SizeT i = 0;
   while (str[i] != 0) i++;
   return i;
}

Char* VG_(strcat) ( Char* dest, const Char* src )
{
   Char* dest_orig = dest;
   while (*dest) dest++;
   while (*src) *dest++ = *src++;
   *dest = 0;
   return dest_orig;
}

Char* VG_(strncat) ( Char* dest, const Char* src, SizeT n )
{
   Char* dest_orig = dest;
   while (*dest) dest++;
   while (*src && n > 0) { *dest++ = *src++; n--; }
   *dest = 0;
   return dest_orig;
}

Char* VG_(strpbrk) ( const Char* s, const Char* accpt )
{
   const Char* a;
   while (*s) {
      a = accpt;
      while (*a)
         if (*a++ == *s)
            return (Char *) s;
      s++;
   }
   return NULL;
}

Char* VG_(strcpy) ( Char* dest, const Char* src )
{
   Char* dest_orig = dest;
   while (*src) *dest++ = *src++;
   *dest = 0;
   return dest_orig;
}

/* Copy bytes, not overrunning the end of dest and always ensuring
   zero termination. */
void VG_(strncpy_safely) ( Char* dest, const Char* src, SizeT ndest )
{
   SizeT i = 0;
   while (True) {
      dest[i] = 0;
      if (src[i] == 0) return;
      if (i >= ndest-1) return;
      dest[i] = src[i];
      i++;
   }
}

Char* VG_(strncpy) ( Char* dest, const Char* src, SizeT ndest )
{
   SizeT i = 0;
   while (True) {
      if (i >= ndest) return dest;     /* reached limit */
      dest[i] = src[i];
      if (src[i++] == 0) {
         /* reached NUL;  pad rest with zeroes as required */
         while (i < ndest) dest[i++] = 0;
         return dest;
      }
   }
}

Int VG_(strcmp) ( const Char* s1, const Char* s2 )
{
   while (True) {
      if (*(UChar*)s1 < *(UChar*)s2) return -1;
      if (*(UChar*)s1 > *(UChar*)s2) return 1;

      /* *s1 == *s2 */
      if (*s1 == 0) return 0;

      s1++; s2++;
   }
}

Int VG_(strcasecmp) ( const Char* s1, const Char* s2 )
{
   while (True) {
      UChar c1 = (UChar)VG_(tolower)(*s1);
      UChar c2 = (UChar)VG_(tolower)(*s2);
      if (c1 < c2) return -1;
      if (c1 > c2) return 1;
      
      /* c1 == c2 */
      if (c1 == 0) return 0;

      s1++; s2++;
   }
}

Int VG_(strncmp) ( const Char* s1, const Char* s2, SizeT nmax )
{
   SizeT n = 0;
   while (True) {
      if (n >= nmax) return 0;
      if (*(UChar*)s1 < *(UChar*)s2) return -1;
      if (*(UChar*)s1 > *(UChar*)s2) return 1;
      
      /* *s1 == *s2 */
      if (*s1 == 0) return 0;

      s1++; s2++; n++;
   }
}

Int VG_(strncasecmp) ( const Char* s1, const Char* s2, SizeT nmax )
{
   Int n = 0;
   while (True) {
      UChar c1;
      UChar c2;
      if (n >= nmax) return 0;
      c1 = (UChar)VG_(tolower)(*s1);
      c2 = (UChar)VG_(tolower)(*s2);
      if (c1 < c2) return -1;
      if (c1 > c2) return 1;

      /* c1 == c2 */
      if (c1 == 0) return 0;

      s1++; s2++; n++;
   }
}

Char* VG_(strstr) ( const Char* haystack, Char* needle )
{
   SizeT n; 
   if (haystack == NULL)
      return NULL;
   n = VG_(strlen)(needle);
   while (True) {
      if (haystack[0] == 0) 
         return NULL;
      if (VG_(strncmp)(haystack, needle, n) == 0) 
         return (Char*)haystack;
      haystack++;
   }
}

Char* VG_(strcasestr) ( const Char* haystack, Char* needle )
{
   Int n; 
   if (haystack == NULL)
      return NULL;
   n = VG_(strlen)(needle);
   while (True) {
      if (haystack[0] == 0) 
         return NULL;
      if (VG_(strncasecmp)(haystack, needle, n) == 0) 
         return (Char*)haystack;
      haystack++;
   }
}

Char* VG_(strchr) ( const Char* s, Char c )
{
   while (True) {
      if (*s == c) return (Char*)s;
      if (*s == 0) return NULL;
      s++;
   }
}

Char* VG_(strrchr) ( const Char* s, Char c )
{
   Int n = VG_(strlen)(s);
   while (--n > 0) {
      if (s[n] == c) return (Char*)s + n;
   }
   return NULL;
}

/* (code copied from glib then updated to valgrind types) */
static Char *olds;
Char *
VG_(strtok) (Char *s, const Char *delim)
{
   return VG_(strtok_r) (s, delim, &olds);
}

Char *
VG_(strtok_r) (Char* s, const Char* delim, Char** saveptr)
{
   Char *token;

   if (s == NULL)
      s = *saveptr;

   /* Scan leading delimiters.  */
   s += VG_(strspn (s, delim));
   if (*s == '\0')
      {
         *saveptr = s;
         return NULL;
      }

   /* Find the end of the token.  */
   token = s;
   s = VG_(strpbrk (token, delim));
   if (s == NULL)
      /* This token finishes the string.  */
      *saveptr = token + VG_(strlen) (token);
   else
      {
         /* Terminate the token and make OLDS point past it.  */
         *s = '\0';
         *saveptr = s + 1;
      }
   return token;
}

static Bool isHex ( UChar c )
{
  return ((c >= '0' && c <= '9') ||
	  (c >= 'a' && c <= 'f') ||
	  (c >= 'A' && c <= 'F'));
}

static UInt fromHex ( UChar c )
{
   if (c >= '0' && c <= '9')
      return (UInt)c - (UInt)'0';
   if (c >= 'a' && c <= 'f')
      return 10 +  (UInt)c - (UInt)'a';
   if (c >= 'A' && c <= 'F')
      return 10 +  (UInt)c - (UInt)'A';
   /*NOTREACHED*/
   // ??? need to vg_assert(0);
   return 0;
}

Bool VG_(parse_Addr) ( UChar** ppc, Addr* result )
{
   Int used, limit = 2 * sizeof(Addr);
   if (**ppc != '0')
      return False;
   (*ppc)++;
   if (**ppc != 'x')
      return False;
   (*ppc)++;
   *result = 0;
   used = 0;
   while (isHex(**ppc)) {
      // ??? need to vg_assert(d < fromHex(**ppc));
      *result = ((*result) << 4) | fromHex(**ppc);
      (*ppc)++;
      used++;
      if (used > limit) return False;
   }
   if (used == 0)
      return False;
   return True;
}

SizeT VG_(strspn) ( const Char* s, const Char* accpt )
{
   const Char *p, *a;
   SizeT count = 0;
   for (p = s; *p != '\0'; ++p) {
      for (a = accpt; *a != '\0'; ++a)
         if (*p == *a)
            break;
      if (*a == '\0')
         return count;
      else
         ++count;
   }
   return count;
}

SizeT VG_(strcspn) ( const Char* s, const char* reject )
{
   SizeT count = 0;
   while (*s != '\0') {
      if (VG_(strchr) (reject, *s++) == NULL)
         ++count;
      else
         return count;
   }
   return count;
}


/* ---------------------------------------------------------------------
   mem* functions
   ------------------------------------------------------------------ */

void* VG_(memcpy) ( void *dest, const void *src, SizeT sz )
{
   const UChar* s  = (const UChar*)src;
         UChar* d  =       (UChar*)dest;
   const UInt*  sI = (const UInt*)src;
         UInt*  dI =       (UInt*)dest;

   if (VG_IS_4_ALIGNED(dI) && VG_IS_4_ALIGNED(sI)) {
      while (sz >= 16) {
         dI[0] = sI[0];
         dI[1] = sI[1];
         dI[2] = sI[2];
         dI[3] = sI[3];
         sz -= 16;
         dI += 4;
         sI += 4;
      }
      if (sz == 0) 
         return dest;
      while (sz >= 4) {
         dI[0] = sI[0];
         sz -= 4;
         dI += 1;
         sI += 1;
      }
      if (sz == 0) 
         return dest;
      s = (const UChar*)sI;
      d = (UChar*)dI;
   }

   while (sz--)
      *d++ = *s++;

   return dest;
}

void* VG_(memmove)(void *dest, const void *src, SizeT sz)
{
   SizeT i;
   if (sz == 0)
      return dest;
   if (dest < src) {
      for (i = 0; i < sz; i++) {
         ((UChar*)dest)[i] = ((UChar*)src)[i];
      }
   }
   else if (dest > src) {
      for (i = 0; i < sz; i++) {
         ((UChar*)dest)[sz-i-1] = ((UChar*)src)[sz-i-1];
      }
   }
   return dest;
}

void* VG_(memset) ( void *destV, Int c, SizeT sz )
{
   Int   c4;
   Char* d = (Char*)destV;
   while ((!VG_IS_4_ALIGNED(d)) && sz >= 1) {
      d[0] = c;
      d++;
      sz--;
   }
   if (sz == 0)
      return destV;
   c4 = c & 0xFF;
   c4 |= (c4 << 8);
   c4 |= (c4 << 16);
   while (sz >= 16) {
      ((Int*)d)[0] = c4;
      ((Int*)d)[1] = c4;
      ((Int*)d)[2] = c4;
      ((Int*)d)[3] = c4;
      d += 16;
      sz -= 16;
   }
   while (sz >= 4) {
      ((Int*)d)[0] = c4;
      d += 4;
      sz -= 4;
   }
   while (sz >= 1) {
      d[0] = c;
      d++;
      sz--;
   }
   return destV;
}

Int VG_(memcmp) ( const void* s1, const void* s2, SizeT n )
{
   Int res;
   const UChar *p1 = s1;
   const UChar *p2 = s2;
   UChar a0;
   UChar b0;

   while (n != 0) {
      a0 = p1[0];
      b0 = p2[0];
      p1 += 1;
      p2 += 1;
      res = a0 - b0;
      if (res != 0)
         return res;
      n -= 1;
   }
   return 0;
}

/* ---------------------------------------------------------------------
   Misc useful functions
   ------------------------------------------------------------------ */

/////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////
/// begin Bentley-McIlroy style quicksort
/// See "Engineering a Sort Function".  Jon L Bentley, M. Douglas
/// McIlroy.  Software Practice and Experience Vol 23(11), Nov 1993.

#define BM_MIN(a, b)                                     \
   (a) < (b) ? a : b

#define BM_SWAPINIT(a, es)                               \
   swaptype =   ((a-(Char*)0) | es) % sizeof(Word)  ? 2  \
              : es > (SizeT)sizeof(Word) ? 1             \
              : 0

#define BM_EXCH(a, b, t)                                 \
   (t = a, a = b, b = t)

#define BM_SWAP(a, b)                                    \
   swaptype != 0                                         \
      ? bm_swapfunc(a, b, es, swaptype)                  \
      : (void)BM_EXCH(*(Word*)(a), *(Word*)(b), t)

#define BM_VECSWAP(a, b, n)                              \
   if (n > 0) bm_swapfunc(a, b, n, swaptype)

#define BM_PVINIT(pv, pm)                                \
   if (swaptype != 0)                                    \
      pv = a, BM_SWAP(pv, pm);                           \
   else                                                  \
      pv = (Char*)&v, v = *(Word*)pm

static Char* bm_med3 ( Char* a, Char* b, Char* c, 
                       Int (*cmp)(void*,void*) ) {
   return cmp(a, b) < 0
          ? (cmp(b, c) < 0 ? b : cmp(a, c) < 0 ? c : a)
          : (cmp(b, c) > 0 ? b : cmp(a, c) > 0 ? c : a);
}

static void bm_swapfunc ( Char* a, Char* b, SizeT n, Int swaptype )
{
   if (swaptype <= 1) {
      Word t;
      for ( ; n > 0; a += sizeof(Word), b += sizeof(Word),
                                        n -= sizeof(Word))
         BM_EXCH(*(Word*)a, *(Word*)b, t);
   } else {
      Char t;
      for ( ; n > 0; a += 1, b += 1, n -= 1)
         BM_EXCH(*a, *b, t);
   }
}

static void bm_qsort ( Char* a, SizeT n, SizeT es,
                       Int (*cmp)(void*,void*) )
{
   Char  *pa, *pb, *pc, *pd, *pl, *pm, *pn, *pv;
   Int   r, swaptype;
   Word  t, v;
   SizeT s, s1, s2;
  tailcall:
   BM_SWAPINIT(a, es);
   if (n < 7) {
      for (pm = a + es; pm < a + n*es; pm += es)
         for (pl = pm; pl > a && cmp(pl-es, pl) > 0; pl -= es)
            BM_SWAP(pl, pl-es);
      return;
   }
   pm = a + (n/2)*es;
   if (n > 7) {
      pl = a;
      pn = a + (n-1)*es;
      if (n > 40) {
         s = (n/8)*es;
         pl = bm_med3(pl, pl+s, pl+2*s, cmp);
         pm = bm_med3(pm-s, pm, pm+s, cmp);
         pn = bm_med3(pn-2*s, pn-s, pn, cmp);
      }
      pm = bm_med3(pl, pm, pn, cmp);
   }
   BM_PVINIT(pv, pm);
   pa = pb = a;
   pc = pd = a + (n-1)*es;
   for (;;) {
      while (pb <= pc && (r = cmp(pb, pv)) <= 0) {
         if (r == 0) { BM_SWAP(pa, pb); pa += es; }
         pb += es;
      }
      while (pc >= pb && (r = cmp(pc, pv)) >= 0) {
         if (r == 0) { BM_SWAP(pc, pd); pd -= es; }
         pc -= es;
      }
      if (pb > pc) break;
      BM_SWAP(pb, pc);
      pb += es;
      pc -= es;
   }
   pn = a + n*es;
   s = BM_MIN(pa-a,  pb-pa   ); BM_VECSWAP(a,  pb-s, s);
   s = BM_MIN(pd-pc, pn-pd-es); BM_VECSWAP(pb, pn-s, s);
   /* Now recurse.  Do the smaller partition first with an explicit
      recursion, then do the larger partition using a tail call.
      Except we can't rely on gcc to implement a tail call in any sane
      way, so simply jump back to the start.  This guarantees stack
      growth can never exceed O(log N) even in the worst case. */
   s1 = pb-pa;
   s2 = pd-pc;
   if (s1 < s2) {
      if (s1 > es) {
         bm_qsort(a, s1/es, es, cmp);
      }
      if (s2 > es) {
         /* bm_qsort(pn-s2, s2/es, es, cmp); */
         a = pn-s2; n = s2/es; es = es; cmp = cmp;
         goto tailcall;
      }
   } else {
      if (s2 > es) {
         bm_qsort(pn-s2, s2/es, es, cmp);
      }
      if (s1 > es) {
         /* bm_qsort(a, s1/es, es, cmp); */
         a = a; n = s1/es; es = es; cmp = cmp;
         goto tailcall;
      } 
   }
}

#undef BM_MIN
#undef BM_SWAPINIT
#undef BM_EXCH
#undef BM_SWAP
#undef BM_VECSWAP
#undef BM_PVINIT

/// end Bentley-McIlroy style quicksort
/////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////

/* Returns the base-2 logarithm of x.  Returns -1 if x is not a power
   of two. */
Int VG_(log2) ( UInt x ) 
{
   Int i;
   /* Any more than 32 and we overflow anyway... */
   for (i = 0; i < 32; i++) {
      if ((1U << i) == x) return i;
   }
   return -1;
}

/* Ditto for 64 bit numbers. */
Int VG_(log2_64) ( ULong x ) 
{
   Int i;
   for (i = 0; i < 64; i++) {
      if ((1ULL << i) == x) return i;
   }
   return -1;
}

// Generic quick sort.
void VG_(ssort)( void* base, SizeT nmemb, SizeT size,
                 Int (*compar)(void*, void*) )
{
   bm_qsort(base,nmemb,size,compar);
}


// This random number generator is based on the one suggested in Kernighan
// and Ritchie's "The C Programming Language".

// A pseudo-random number generator returning a random UInt.  If pSeed
// is NULL, it uses its own seed, which starts at zero.  If pSeed is
// non-NULL, it uses and updates whatever pSeed points at.

static UInt seed = 0;

UInt VG_(random)( /*MOD*/UInt* pSeed )
{
   if (pSeed == NULL) 
      pSeed = &seed;

   *pSeed = (1103515245 * *pSeed + 12345);
   return *pSeed;
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/

