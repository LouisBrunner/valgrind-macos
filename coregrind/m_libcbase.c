
/*--------------------------------------------------------------------*/
/*--- Entirely standalone libc stuff.                 m_libcbase.c ---*/
/*--------------------------------------------------------------------*/
 
/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2013 Julian Seward 
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
   HChar functions.
   ------------------------------------------------------------------ */

Bool VG_(isspace) ( HChar c )
{
   return (c == ' '  || c == '\n' || c == '\t' || 
           c == '\f' || c == '\v' || c == '\r');
}

Bool VG_(isdigit) ( HChar c )
{
   return (c >= '0' && c <= '9');
}

/* ---------------------------------------------------------------------
   Converting strings to numbers
   ------------------------------------------------------------------ */

static Bool is_dec_digit(HChar c, Long* digit)
{
   if (c >= '0' && c <= '9') { *digit = (Long)(c - '0'); return True; }
   return False;
}

static Bool is_hex_digit(HChar c, Long* digit)
{
   if (c >= '0' && c <= '9') { *digit = (Long)(c - '0');        return True; }
   if (c >= 'A' && c <= 'F') { *digit = (Long)((c - 'A') + 10); return True; }
   if (c >= 'a' && c <= 'f') { *digit = (Long)((c - 'a') + 10); return True; }
   return False;
}

Long VG_(strtoll10) ( const HChar* str, HChar** endptr )
{
   Bool neg = False, converted = False;
   Long n = 0, digit = 0;
   const HChar* str0 = str;

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
   if (endptr) *endptr = (HChar *)str;    // Record first failing character.
   return n;
}

ULong VG_(strtoull10) ( const HChar* str, HChar** endptr )
{
   Bool converted = False;
   ULong n = 0;
   Long digit = 0;
   const HChar* str0 = str;

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
   if (endptr) *endptr = (HChar *)str;    // Record first failing character.
   return n;
}

Long VG_(strtoll16) ( const HChar* str, HChar** endptr )
{
   Bool neg = False, converted = False;
   Long n = 0, digit = 0;
   const HChar* str0 = str;

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
   if (endptr) *endptr = (HChar *)str;    // Record first failing character.
   return n;
}

ULong VG_(strtoull16) ( const HChar* str, HChar** endptr )
{
   Bool converted = False;
   ULong n = 0;
   Long digit = 0;
   const HChar* str0 = str;

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
   if (endptr) *endptr = (HChar *)str;    // Record first failing character.
   return n;
}

double VG_(strtod) ( const HChar* str, HChar** endptr )
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
   if (endptr) *endptr = (HChar *)str;    // Record first failing character.
   return n;
}

HChar VG_(tolower) ( HChar c )
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

SizeT VG_(strlen) ( const HChar* str )
{
   SizeT i = 0;
   while (str[i] != 0) i++;
   return i;
}

HChar* VG_(strcat) ( HChar* dest, const HChar* src )
{
   HChar* dest_orig = dest;
   while (*dest) dest++;
   while (*src) *dest++ = *src++;
   *dest = 0;
   return dest_orig;
}

HChar* VG_(strncat) ( HChar* dest, const HChar* src, SizeT n )
{
   HChar* dest_orig = dest;
   while (*dest) dest++;
   while (*src && n > 0) { *dest++ = *src++; n--; }
   *dest = 0;
   return dest_orig;
}

HChar* VG_(strpbrk) ( const HChar* s, const HChar* accpt )
{
   const HChar* a;
   while (*s) {
      a = accpt;
      while (*a)
         if (*a++ == *s)
           return (HChar *)s;
      s++;
   }
   return NULL;
}

HChar* VG_(strcpy) ( HChar* dest, const HChar* src )
{
   HChar* dest_orig = dest;
   while (*src) *dest++ = *src++;
   *dest = 0;
   return dest_orig;
}

/* Copy bytes, not overrunning the end of dest and always ensuring
   zero termination. */
void VG_(strncpy_safely) ( HChar* dest, const HChar* src, SizeT ndest )
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

HChar* VG_(strncpy) ( HChar* dest, const HChar* src, SizeT ndest )
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

Int VG_(strcmp) ( const HChar* s1, const HChar* s2 )
{
   while (True) {
      if (*(const UChar*)s1 < *(const UChar*)s2) return -1;
      if (*(const UChar*)s1 > *(const UChar*)s2) return 1;

      /* *s1 == *s2 */
      if (*s1 == 0) return 0;

      s1++; s2++;
   }
}

Int VG_(strcasecmp) ( const HChar* s1, const HChar* s2 )
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

Int VG_(strncmp) ( const HChar* s1, const HChar* s2, SizeT nmax )
{
   SizeT n = 0;
   while (True) {
      if (n >= nmax) return 0;
      if (*(const UChar*)s1 < *(const UChar*)s2) return -1;
      if (*(const UChar*)s1 > *(const UChar*)s2) return 1;
      
      /* *s1 == *s2 */
      if (*s1 == 0) return 0;

      s1++; s2++; n++;
   }
}

Int VG_(strncasecmp) ( const HChar* s1, const HChar* s2, SizeT nmax )
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

HChar* VG_(strstr) ( const HChar* haystack, const HChar* needle )
{
   SizeT n; 
   if (haystack == NULL)
      return NULL;
   n = VG_(strlen)(needle);
   while (True) {
      if (haystack[0] == 0) 
         return NULL;
      if (VG_(strncmp)(haystack, needle, n) == 0) 
         return (HChar*)haystack;
      haystack++;
   }
}

HChar* VG_(strcasestr) ( const HChar* haystack, const HChar* needle )
{
   Int n; 
   if (haystack == NULL)
      return NULL;
   n = VG_(strlen)(needle);
   while (True) {
      if (haystack[0] == 0) 
         return NULL;
      if (VG_(strncasecmp)(haystack, needle, n) == 0) 
         return (HChar*)haystack;
      haystack++;
   }
}

HChar* VG_(strchr) ( const HChar* s, HChar c )
{
   while (True) {
     if (*s == c) return (HChar *)s;
      if (*s == 0) return NULL;
      s++;
   }
}

HChar* VG_(strrchr) ( const HChar* s, HChar c )
{
   Int n = VG_(strlen)(s);
   while (--n > 0) {
     if (s[n] == c) return (HChar *)s + n;
   }
   return NULL;
}

/* (code copied from glib then updated to valgrind types) */
static HChar *olds;
HChar *
VG_(strtok) (HChar *s, const HChar *delim)
{
   return VG_(strtok_r) (s, delim, &olds);
}

HChar *
VG_(strtok_r) (HChar* s, const HChar* delim, HChar** saveptr)
{
   HChar *token;

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

static Bool isHex ( HChar c )
{
  return ((c >= '0' && c <= '9') ||
	  (c >= 'a' && c <= 'f') ||
	  (c >= 'A' && c <= 'F'));
}

static UInt fromHex ( HChar c )
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

Bool VG_(parse_Addr) ( const HChar** ppc, Addr* result )
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

Bool VG_(parse_enum_set) ( const HChar *tokens,
                           const HChar *input,
                           UInt *enum_set)
{
   const SizeT tokens_len = VG_(strlen)(tokens);
   if (tokens_len > 1000) return False; /* "obviously invalid" */
   HChar  tok_tokens[tokens_len+1];
   HChar *tokens_saveptr;
   HChar *token;
   UInt token_nr = 0;
   UInt all_set = 0;

   const SizeT input_len = VG_(strlen)(input);
   if (input_len > 1000) return False; /* "obviously invalid" */
   HChar  tok_input[input_len+1];
   HChar *input_saveptr;
   HChar *input_word;
   UInt word_nr = 0;
   UInt known_words = 0;
   Bool seen_all_kw = False;
   Bool seen_none_kw = False;

   *enum_set = 0;

   VG_(strcpy) (tok_input, input);
   for (input_word = VG_(strtok_r)(tok_input, ",", &input_saveptr);
        input_word;
        input_word = VG_(strtok_r)(NULL, ",", &input_saveptr)) {
      word_nr++;
      if (0 == VG_(strcmp)(input_word, "all")) {
         seen_all_kw = True;
         known_words++;
      } else if (0 == VG_(strcmp)(input_word, "none")) {
         seen_none_kw = True;
         known_words++;
      }

      // Scan tokens + compute all_set. Do that even if all or none was
      // recognised to have a correct value for all_set when exiting
      // of the 'input' loop.
      all_set = 0;
      token_nr = 0;
      VG_(strcpy) (tok_tokens, tokens);
      for (token = VG_(strtok_r)(tok_tokens, ",", &tokens_saveptr);
           token;
           token = VG_(strtok_r)(NULL, ",", &tokens_saveptr)) {
         if (0 != VG_(strcmp)(token, "-")) {
            if (0 == VG_(strcmp)(input_word, token)) {
               *enum_set |= 1 << token_nr;
               known_words++;
            }
            all_set |= 1 << token_nr;
         }
         token_nr++;
      }
   }

   if (known_words != word_nr)
      return False; // One or more input_words not recognised.
   if (seen_all_kw) {
      if (seen_none_kw || *enum_set)
         return False; // mixing all with either none or a specific value.
      *enum_set = all_set;
   } else if (seen_none_kw) {
      if (seen_all_kw || *enum_set)
         return False; // mixing none with either all or a specific value.
      *enum_set = 0;
   } else {
      // seen neither all or none, we must see at least one value
      if (*enum_set == 0)
         return False;
   }

   return True;
}

SizeT VG_(strspn) ( const HChar* s, const HChar* accpt )
{
   const HChar *p, *a;
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

SizeT VG_(strcspn) ( const HChar* s, const HChar* reject )
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

   /* If we're unlucky, the alignment constraints for the fast case
      above won't apply, and we'll have to to it all here.  Hence the
      unrolling. */
   while (sz >= 4) {
      d[0] = s[0];
      d[1] = s[1];
      d[2] = s[2];
      d[3] = s[3];
      d += 4;
      s += 4;
      sz -= 4;
   }
   while (sz >= 1) {
      d[0] = s[0];
      d += 1;
      s += 1;
      sz -= 1;
   }

   return dest;
}

void* VG_(memmove)(void *dest, const void *src, SizeT sz)
{
   SizeT i;
   if (sz == 0)
      return dest;
   if (dest < src) {
      for (i = 0; i < sz; i++) {
         ((UChar*)dest)[i] = ((const UChar*)src)[i];
      }
   }
   else if (dest > src) {
      for (i = 0; i < sz; i++) {
         ((UChar*)dest)[sz-i-1] = ((const UChar*)src)[sz-i-1];
      }
   }
   return dest;
}

void* VG_(memset) ( void *destV, Int c, SizeT sz )
{
   Int   c4;
   HChar* d = (HChar*)destV;
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
                       Int (*cmp)(const void*, const void*) ) {
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
                       Int (*cmp)(const void*, const void*) )
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
                 Int (*compar)(const void*, const void*) )
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


/* The following Adler-32 checksum code is taken from zlib-1.2.3, which
   has the following copyright notice. */
/*
Copyright notice:

 (C) 1995-2004 Jean-loup Gailly and Mark Adler

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.

  Jean-loup Gailly        Mark Adler
  jloup@gzip.org          madler@alumni.caltech.edu

If you use the zlib library in a product, we would appreciate *not*
receiving lengthy legal documents to sign. The sources are provided
for free but without warranty of any kind.  The library has been
entirely written by Jean-loup Gailly and Mark Adler; it does not
include third-party code.

If you redistribute modified sources, we would appreciate that you include
in the file ChangeLog history information documenting your changes. Please
read the FAQ for more information on the distribution of modified source
versions.
*/

/* Update a running Adler-32 checksum with the bytes buf[0..len-1] and
   return the updated checksum. If buf is NULL, this function returns
   the required initial value for the checksum. An Adler-32 checksum is
   almost as reliable as a CRC32 but can be computed much faster. */
UInt VG_(adler32)( UInt adler, const UChar* buf, UInt len )
{
#  define BASE 65521UL    /* largest prime smaller than 65536 */
#  define NMAX 5552
   /* NMAX is the largest n such that 
      255n(n+1)/2 + (n+1)(BASE-1) <= 2^32-1 */

#  define DO1(buf,i)  {adler += (buf)[i]; sum2 += adler;}
#  define DO2(buf,i)  DO1(buf,i); DO1(buf,i+1);
#  define DO4(buf,i)  DO2(buf,i); DO2(buf,i+2);
#  define DO8(buf,i)  DO4(buf,i); DO4(buf,i+4);
#  define DO16(buf)   DO8(buf,0); DO8(buf,8);

   /* The zlib sources recommend this definition of MOD if the
      processor cannot do integer division in hardware. */
#  define MOD(a) \
      do { \
          if (a >= (BASE << 16)) a -= (BASE << 16); \
          if (a >= (BASE << 15)) a -= (BASE << 15); \
          if (a >= (BASE << 14)) a -= (BASE << 14); \
          if (a >= (BASE << 13)) a -= (BASE << 13); \
          if (a >= (BASE << 12)) a -= (BASE << 12); \
          if (a >= (BASE << 11)) a -= (BASE << 11); \
          if (a >= (BASE << 10)) a -= (BASE << 10); \
          if (a >= (BASE << 9)) a -= (BASE << 9); \
          if (a >= (BASE << 8)) a -= (BASE << 8); \
          if (a >= (BASE << 7)) a -= (BASE << 7); \
          if (a >= (BASE << 6)) a -= (BASE << 6); \
          if (a >= (BASE << 5)) a -= (BASE << 5); \
          if (a >= (BASE << 4)) a -= (BASE << 4); \
          if (a >= (BASE << 3)) a -= (BASE << 3); \
          if (a >= (BASE << 2)) a -= (BASE << 2); \
          if (a >= (BASE << 1)) a -= (BASE << 1); \
          if (a >= BASE) a -= BASE; \
      } while (0)
#  define MOD4(a) \
      do { \
          if (a >= (BASE << 4)) a -= (BASE << 4); \
          if (a >= (BASE << 3)) a -= (BASE << 3); \
          if (a >= (BASE << 2)) a -= (BASE << 2); \
          if (a >= (BASE << 1)) a -= (BASE << 1); \
          if (a >= BASE) a -= BASE; \
      } while (0)

    UInt sum2;
    UInt n;

    /* split Adler-32 into component sums */
    sum2 = (adler >> 16) & 0xffff;
    adler &= 0xffff;

    /* in case user likes doing a byte at a time, keep it fast */
    if (len == 1) {
        adler += buf[0];
        if (adler >= BASE)
            adler -= BASE;
        sum2 += adler;
        if (sum2 >= BASE)
            sum2 -= BASE;
        return adler | (sum2 << 16);
    }

    /* initial Adler-32 value (deferred check for len == 1 speed) */
    if (buf == NULL)
        return 1L;

    /* in case short lengths are provided, keep it somewhat fast */
    if (len < 16) {
        while (len--) {
            adler += *buf++;
            sum2 += adler;
        }
        if (adler >= BASE)
            adler -= BASE;
        MOD4(sum2);             /* only added so many BASE's */
        return adler | (sum2 << 16);
    }

    /* do length NMAX blocks -- requires just one modulo operation */
    while (len >= NMAX) {
        len -= NMAX;
        n = NMAX / 16;          /* NMAX is divisible by 16 */
        do {
            DO16(buf);          /* 16 sums unrolled */
            buf += 16;
        } while (--n);
        MOD(adler);
        MOD(sum2);
    }

    /* do remaining bytes (less than NMAX, still just one modulo) */
    if (len) {                  /* avoid modulos if none remaining */
        while (len >= 16) {
            len -= 16;
            DO16(buf);
            buf += 16;
        }
        while (len--) {
            adler += *buf++;
            sum2 += adler;
        }
        MOD(adler);
        MOD(sum2);
    }

    /* return recombined sums */
    return adler | (sum2 << 16);

#  undef MOD4
#  undef MOD
#  undef DO16
#  undef DO8
#  undef DO4
#  undef DO2
#  undef DO1
#  undef NMAX
#  undef BASE
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/

