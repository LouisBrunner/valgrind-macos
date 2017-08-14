
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (vex_util.c) is                               ---*/
/*--- Copyright (C) OpenWorks LLP.  All rights reserved.      ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of LibVEX, a library for dynamic binary
   instrumentation and translation.

   Copyright (C) 2004-2008 OpenWorks LLP.  All rights reserved.

   This library is made available under a dual licensing scheme.

   If you link LibVEX against other code all of which is itself
   licensed under the GNU General Public License, version 2 dated June
   1991 ("GPL v2"), then you may use LibVEX under the terms of the GPL
   v2, as appearing in the file LICENSE.GPL.  If the file LICENSE.GPL
   is missing, you can obtain a copy of the GPL v2 from the Free
   Software Foundation Inc., 51 Franklin St, Fifth Floor, Boston, MA
   02110-1301, USA.

   For any other uses of LibVEX, you must first obtain a commercial
   license from OpenWorks LLP.  Please contact info@open-works.co.uk
   for information about commercial licensing.

   This software is provided by OpenWorks LLP "as is" and any express
   or implied warranties, including, but not limited to, the implied
   warranties of merchantability and fitness for a particular purpose
   are disclaimed.  In no event shall OpenWorks LLP be liable for any
   direct, indirect, incidental, special, exemplary, or consequential
   damages (including, but not limited to, procurement of substitute
   goods or services; loss of use, data, or profits; or business
   interruption) however caused and on any theory of liability,
   whether in contract, strict liability, or tort (including
   negligence or otherwise) arising in any way out of the use of this
   software, even if advised of the possibility of such damage.

   Neither the names of the U.S. Department of Energy nor the
   University of California nor the names of its contributors may be
   used to endorse or promote products derived from this software
   without prior written permission.
*/

#include "libvex_basictypes.h"
#include "libvex.h"

#include "vex_globals.h"
#include "vex_util.h"


/*---------------------------------------------------------*/
/*--- Storage                                           ---*/
/*---------------------------------------------------------*/

/* Try to keep this as low as possible -- in particular, less than the
   size of the smallest L2 cache we might encounter.  At 50000, my VIA
   Nehemiah 1 GHz (a weedy machine) can satisfy 27 million calls/
   second to LibVEX_Alloc(16) -- that is, allocate memory at over 400
   MByte/sec.  Once the size increases enough to fall out of the cache
   into memory, the rate falls by about a factor of 3. 
*/
#define N_TEMPORARY_BYTES 4000000

static HChar  temporary[N_TEMPORARY_BYTES] __attribute__((aligned(8)));
static HChar* temporary_first = &temporary[0];
static HChar* temporary_curr  = &temporary[0];
static HChar* temporary_last  = &temporary[N_TEMPORARY_BYTES-1];

static ULong  temporary_bytes_allocd_TOT = 0;

#define N_PERMANENT_BYTES 10000

static HChar  permanent[N_PERMANENT_BYTES] __attribute__((aligned(8)));
static HChar* permanent_first = &permanent[0];
static HChar* permanent_curr  = &permanent[0];
static HChar* permanent_last  = &permanent[N_PERMANENT_BYTES-1];

static VexAllocMode mode = VexAllocModeTEMP;

void vexAllocSanityCheck ( void )
{
   vassert(temporary_first == &temporary[0]);
   vassert(temporary_last  == &temporary[N_TEMPORARY_BYTES-1]);
   vassert(permanent_first == &permanent[0]);
   vassert(permanent_last  == &permanent[N_PERMANENT_BYTES-1]);
   vassert(temporary_first <= temporary_curr);
   vassert(temporary_curr  <= temporary_last);
   vassert(permanent_first <= permanent_curr);
   vassert(permanent_curr  <= permanent_last);
   vassert(private_LibVEX_alloc_first <= private_LibVEX_alloc_curr);
   vassert(private_LibVEX_alloc_curr  <= private_LibVEX_alloc_last);
   if (mode == VexAllocModeTEMP){
      vassert(private_LibVEX_alloc_first == temporary_first);
      vassert(private_LibVEX_alloc_last  == temporary_last);
   } 
   else
   if (mode == VexAllocModePERM) {
      vassert(private_LibVEX_alloc_first == permanent_first);
      vassert(private_LibVEX_alloc_last  == permanent_last);
   }
   else 
      vassert(0);

#  define IS_WORD_ALIGNED(p)   (0 == (((HWord)p) & (sizeof(HWord)-1)))
   vassert(sizeof(HWord) == 4 || sizeof(HWord) == 8);
   vassert(IS_WORD_ALIGNED(temporary_first));
   vassert(IS_WORD_ALIGNED(temporary_curr));
   vassert(IS_WORD_ALIGNED(temporary_last+1));
   vassert(IS_WORD_ALIGNED(permanent_first));
   vassert(IS_WORD_ALIGNED(permanent_curr));
   vassert(IS_WORD_ALIGNED(permanent_last+1));
   vassert(IS_WORD_ALIGNED(private_LibVEX_alloc_first));
   vassert(IS_WORD_ALIGNED(private_LibVEX_alloc_curr));
   vassert(IS_WORD_ALIGNED(private_LibVEX_alloc_last+1));
#  undef IS_WORD_ALIGNED
}

/* The current allocation mode. */

void vexSetAllocMode ( VexAllocMode m )
{
   vexAllocSanityCheck();

   /* Save away the current allocation point .. */
   if (mode == VexAllocModeTEMP){
      temporary_curr = private_LibVEX_alloc_curr;
   } 
   else
   if (mode == VexAllocModePERM) {
      permanent_curr = private_LibVEX_alloc_curr;
   }
   else 
      vassert(0);

   /* Did that screw anything up? */
   vexAllocSanityCheck();

   if (m == VexAllocModeTEMP){
      private_LibVEX_alloc_first = temporary_first;
      private_LibVEX_alloc_curr  = temporary_curr;
      private_LibVEX_alloc_last  = temporary_last;
   } 
   else
   if (m == VexAllocModePERM) {
      private_LibVEX_alloc_first = permanent_first;
      private_LibVEX_alloc_curr  = permanent_curr;
      private_LibVEX_alloc_last  = permanent_last;
   }
   else 
      vassert(0);

   mode = m;
}

VexAllocMode vexGetAllocMode ( void )
{
   return mode;
}

/* Visible to library client, unfortunately. */

HChar* private_LibVEX_alloc_first = &temporary[0];
HChar* private_LibVEX_alloc_curr  = &temporary[0];
HChar* private_LibVEX_alloc_last  = &temporary[N_TEMPORARY_BYTES-1];

__attribute__((noreturn))
void private_LibVEX_alloc_OOM(void)
{
   HChar* pool = "???";
   if (private_LibVEX_alloc_first == &temporary[0]) pool = "TEMP";
   if (private_LibVEX_alloc_first == &permanent[0]) pool = "PERM";
   vex_printf("VEX temporary storage exhausted.\n");
   vex_printf("Pool = %s,  start %p curr %p end %p (size %lld)\n",
              pool, 
              private_LibVEX_alloc_first,
              private_LibVEX_alloc_curr,
              private_LibVEX_alloc_last,
              (Long)(private_LibVEX_alloc_last + 1 - private_LibVEX_alloc_first));
   vpanic("VEX temporary storage exhausted.\n"
          "Increase N_{TEMPORARY,PERMANENT}_BYTES and recompile.");
}

void vexSetAllocModeTEMP_and_clear ( void )
{
   /* vassert(vex_initdone); */ /* causes infinite assert loops */
   temporary_bytes_allocd_TOT 
      += (ULong)(private_LibVEX_alloc_curr - private_LibVEX_alloc_first);

   mode = VexAllocModeTEMP;
   temporary_curr            = &temporary[0];
   private_LibVEX_alloc_curr = &temporary[0];
   vexAllocSanityCheck();
}


/* Exported to library client. */

void LibVEX_ShowAllocStats ( void )
{
   vex_printf("vex storage: T total %lld bytes allocated\n",
              (Long)temporary_bytes_allocd_TOT );
   vex_printf("vex storage: P total %lld bytes allocated\n",
              (Long)(permanent_curr - permanent_first) );
}


/*---------------------------------------------------------*/
/*--- Bombing out                                       ---*/
/*---------------------------------------------------------*/

__attribute__ ((noreturn))
void vex_assert_fail ( const HChar* expr,
                       const HChar* file, Int line, const HChar* fn )
{
   vex_printf( "\nvex: %s:%d (%s): Assertion `%s' failed.\n",
               file, line, fn, expr );
   (*vex_failure_exit)();
}

__attribute__ ((noreturn))
void vpanic ( HChar* str )
{
   vex_printf("\nvex: the `impossible' happened:\n   %s\n", str);
   (*vex_failure_exit)();
}


/*---------------------------------------------------------*/
/*--- vex_printf                                        ---*/
/*---------------------------------------------------------*/

/* This should be the only <...> include in the entire VEX library.
   New code for vex_util.c should go above this point. */
#include <stdarg.h>

static Int vex_strlen ( const HChar* str )
{
   Int i = 0;
   while (str[i] != 0) i++;
   return i;
}

Bool vex_streq ( const HChar* s1, const HChar* s2 )
{
   while (True) {
      if (*s1 == 0 && *s2 == 0)
         return True;
      if (*s1 != *s2)
         return False;
      s1++;
      s2++;
   }
}


/* Convert N0 into ascii in BUF, which is assumed to be big enough (at
   least 67 bytes long).  Observe BASE, SYNED and HEXCAPS. */
static
void convert_int ( /*OUT*/HChar* buf, Long n0, 
                   Int base, Bool syned, Bool hexcaps )
{
   ULong u0;
   HChar c;
   Bool minus = False;
   Int i, j, bufi = 0;
   buf[bufi] = 0;

   if (syned) {
      if (n0 < 0) {
         minus = True;
         u0 = (ULong)(-n0);
      } else {
         u0 = (ULong)(n0);
      }
   } else {
      u0 = (ULong)n0;
   }

   while (1) {
     buf[bufi++] = toHChar('0' + toUInt(u0 % base));
     u0 /= base;
     if (u0 == 0) break;
   }
   if (minus)
      buf[bufi++] = '-';

   buf[bufi] = 0;
   for (i = 0; i < bufi; i++)
      if (buf[i] > '9') 
         buf[i] = toHChar(buf[i] + (hexcaps ? 'A' : 'a') - '9' - 1);

   i = 0;
   j = bufi-1;
   while (i <= j) {
      c = buf[i];
      buf[i] = buf[j];
      buf[j] = c;
      i++;
      j--;
   }
}


/* A half-arsed and buggy, but good-enough, implementation of
   printf. */
static
UInt vprintf_wrk ( void(*sink)(HChar),
                   HChar* format,
                   va_list ap )
{
#  define PUT(_ch)  \
      do { sink(_ch); nout++; } \
      while (0)

#  define PAD(_n) \
      do { Int _qq = (_n); for (; _qq > 0; _qq--) PUT(padchar); } \
      while (0)

#  define PUTSTR(_str) \
      do { HChar* _qq = _str; for (; *_qq; _qq++) PUT(*_qq); } \
      while (0)

   HChar* saved_format;
   Bool   longlong, ljustify;
   HChar  padchar;
   Int    fwidth, nout, len1, len2, len3;
   HChar  intbuf[100];  /* big enough for a 64-bit # in base 2 */

   nout = 0;
   while (1) {

      if (!format)
         break;
      if (*format == 0) 
         break;

      if (*format != '%') {
         PUT(*format); 
         format++;
         continue;
      }

      saved_format = format;
      longlong = False;
      ljustify = False;
      padchar = ' ';
      fwidth = 0;
      format++;

      if (*format == '-') {
         format++;
         ljustify = True;
      }
      if (*format == '0') {
         format++;
         padchar = '0';
      }
      while (*format >= '0' && *format <= '9') {
         fwidth = fwidth * 10 + (*format - '0');
         format++;
      }
      if (*format == 'l') {
         format++;
         if (*format == 'l') {
            format++;
           longlong = True;
         }
      }

      switch (*format) {
         case 's': {
            HChar* str = va_arg(ap, HChar*);
            if (str == NULL)
               str = "(null)";
            len1 = len3 = 0;
            len2 = vex_strlen(str);
            if (fwidth > len2) { len1 = ljustify ? 0 : fwidth-len2;
                                 len3 = ljustify ? fwidth-len2 : 0; }
            PAD(len1); PUTSTR(str); PAD(len3);
            break;
         }
         case 'c': {
            HChar c = (HChar)va_arg(ap, int);
            HChar str[2];
            str[0] = c;
            str[1] = 0;
            len1 = len3 = 0;
            len2 = vex_strlen(str);
            if (fwidth > len2) { len1 = ljustify ? 0 : fwidth-len2;
                                 len3 = ljustify ? fwidth-len2 : 0; }
            PAD(len1); PUTSTR(str); PAD(len3);
            break;
         }
         case 'd': {
            Long l;
            if (longlong) {
               l = va_arg(ap, Long);
            } else {
               l = (Long)va_arg(ap, Int);
            }
            convert_int(intbuf, l, 10/*base*/, True/*signed*/,
                                False/*irrelevant*/);
            len1 = len3 = 0;
            len2 = vex_strlen(intbuf);
            if (fwidth > len2) { len1 = ljustify ? 0 : fwidth-len2;
                                 len3 = ljustify ? fwidth-len2 : 0; }
            PAD(len1); PUTSTR(intbuf); PAD(len3);
            break;
         }
         case 'u': 
         case 'x': 
         case 'X': {
            Int   base = *format == 'u' ? 10 : 16;
            Bool  hexcaps = True; /* *format == 'X'; */
            ULong l;
            if (longlong) {
               l = va_arg(ap, ULong);
            } else {
               l = (ULong)va_arg(ap, UInt);
            }
            convert_int(intbuf, l, base, False/*unsigned*/, hexcaps);
            len1 = len3 = 0;
            len2 = vex_strlen(intbuf);
            if (fwidth > len2) { len1 = ljustify ? 0 : fwidth-len2;
                                 len3 = ljustify ? fwidth-len2 : 0; }
            PAD(len1); PUTSTR(intbuf); PAD(len3);
            break;
         }
         case 'p': 
         case 'P': {
            Bool hexcaps = toBool(*format == 'P');
            ULong l = Ptr_to_ULong( va_arg(ap, void*) );
            convert_int(intbuf, l, 16/*base*/, False/*unsigned*/, hexcaps);
            len1 = len3 = 0;
            len2 = vex_strlen(intbuf)+2;
            if (fwidth > len2) { len1 = ljustify ? 0 : fwidth-len2;
                                 len3 = ljustify ? fwidth-len2 : 0; }
            PAD(len1); PUT('0'); PUT('x'); PUTSTR(intbuf); PAD(len3);
            break;
         }
         case '%': {
            PUT('%');
            break;
         }
         default:
            /* no idea what it is.  Print the format literally and
               move on. */
            while (saved_format <= format) {
               PUT(*saved_format);
               saved_format++;
            }
            break;
      }

      format++;

   }

   return nout;

#  undef PUT
#  undef PAD
#  undef PUTSTR
}


/* A general replacement for printf().  Note that only low-level 
   debugging info should be sent via here.  The official route is to
   to use vg_message().  This interface is deprecated.
*/
static HChar myprintf_buf[1000];
static Int   n_myprintf_buf;

static void add_to_myprintf_buf ( HChar c )
{
   Bool emit = toBool(c == '\n' || n_myprintf_buf >= 1000-10 /*paranoia*/);
   myprintf_buf[n_myprintf_buf++] = c;
   myprintf_buf[n_myprintf_buf] = 0;
   if (emit) {
      (*vex_log_bytes)( myprintf_buf, vex_strlen(myprintf_buf) );
      n_myprintf_buf = 0;
      myprintf_buf[n_myprintf_buf] = 0;
   }
}

UInt vex_printf ( HChar* format, ... )
{
   UInt ret;
   va_list vargs;
   va_start(vargs,format);
   
   n_myprintf_buf = 0;
   myprintf_buf[n_myprintf_buf] = 0;      
   ret = vprintf_wrk ( add_to_myprintf_buf, format, vargs );

   if (n_myprintf_buf > 0) {
      (*vex_log_bytes)( myprintf_buf, n_myprintf_buf );
   }

   va_end(vargs);

   return ret;
}


/* A general replacement for sprintf(). */

static HChar *vg_sprintf_ptr;

static void add_to_vg_sprintf_buf ( HChar c )
{
   *vg_sprintf_ptr++ = c;
}

UInt vex_sprintf ( HChar* buf, HChar *format, ... )
{
   Int ret;
   va_list vargs;

   vg_sprintf_ptr = buf;

   va_start(vargs,format);

   ret = vprintf_wrk ( add_to_vg_sprintf_buf, format, vargs );
   add_to_vg_sprintf_buf(0);

   va_end(vargs);

   vassert(vex_strlen(buf) == ret);
   return ret;
}


/*---------------------------------------------------------------*/
/*--- end                                          vex_util.c ---*/
/*---------------------------------------------------------------*/
