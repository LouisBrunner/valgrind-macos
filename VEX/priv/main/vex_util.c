
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (vex_util.c) is                               ---*/
/*--- Copyright (c) 2004 OpenWorks LLP.  All rights reserved. ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of LibVEX, a library for dynamic binary
   instrumentation and translation.

   Copyright (C) 2004 OpenWorks, LLP.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; Version 2 dated June 1991 of the
   license.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE, or liability
   for damages.  See the GNU General Public License for more details.

   Neither the names of the U.S. Department of Energy nor the
   University of California nor the names of its contributors may be
   used to endorse or promote products derived from this software
   without prior written permission.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
   USA.
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
#define N_TEMPORARY_BYTES 1200000

static Char temporary[N_TEMPORARY_BYTES];
static Int  temporary_used = 0;

#define N_PERMANENT_BYTES 1000

static Char permanent[N_TEMPORARY_BYTES];
static Int  permanent_used = 0;


/* Gather statistics. */
static Int temporary_bytes_allocd = 0;
static Int temporary_count_allocs = 0;

static ULong temporary_bytes_allocd_TOT = 0;
static ULong temporary_count_allocs_TOT = 0;

/* The current allocation mode. */
static VexAllocMode mode = VexAllocModeTEMP;


void vexSetAllocMode ( VexAllocMode m )
{
   mode = m;
}

VexAllocMode vexGetAllocMode ( void )
{
   return mode;
}

/* Exported to library client. */

void* LibVEX_Alloc ( Int nbytes ) 
{
   /* 3 or 7 depending on host word size. */
#  define ALIGN (sizeof(void*)-1)
   vassert(vex_initdone);
   vassert(nbytes >= 0);
   if (vex_valgrind_support) {
      /* ugly hack -- do not remove */
      extern void* malloc ( int );
      return malloc(nbytes);
   } else {
      nbytes = (nbytes + ALIGN) & ~ALIGN;
      if (mode == VexAllocModeTEMP) {
         if (temporary_used + nbytes > N_TEMPORARY_BYTES)
            vpanic("VEX temporary storage exhausted.\n"
                   "Increase N_TEMPORARY_BYTES and recompile.");
         temporary_count_allocs++;
         temporary_bytes_allocd += nbytes;
         temporary_used += nbytes;
         return (void*)(&temporary[temporary_used - nbytes]);
      } else {
         if (permanent_used + nbytes > N_PERMANENT_BYTES)
            vpanic("VEX permanent storage exhausted.\n"
                   "Increase N_PERMANENT_BYTES and recompile.");
         permanent_used += nbytes;
         return (void*)(&permanent[permanent_used - nbytes]);
      }
   }
#  undef ALIGN
}

void vexClearTEMP ( void )
{
   /* vassert(vex_initdone); */ /* causes infinite assert loops */
   temporary_bytes_allocd_TOT += (ULong)temporary_bytes_allocd;
   temporary_count_allocs_TOT += (ULong)temporary_count_allocs;
   temporary_used = 0;
   temporary_bytes_allocd = 0;
   temporary_count_allocs = 0;
}


/* Exported to library client. */

void LibVEX_ShowAllocStats ( void )
{
   vex_printf("vex storage:  P %d,  T total %lld (%lld),  T curr %d (%d)\n",
              permanent_used,
              (Long)temporary_bytes_allocd_TOT, 
              (Long)temporary_count_allocs_TOT,
              temporary_bytes_allocd, temporary_count_allocs );
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

/* ---------------------------------------------------------------------
   printf implementation.  The key function, vg_vprintf(), emits chars 
   into a caller-supplied function.  Distantly derived from:

      vprintf replacement for Checker.
      Copyright 1993, 1994, 1995 Tristan Gingold
      Written September 1993 Tristan Gingold
      Tristan Gingold, 8 rue Parmentier, F-91120 PALAISEAU, FRANCE

   (Checker itself was GPL'd.)
   ------------------------------------------------------------------ */

static HChar vex_toupper ( HChar c )
{
   if (c >= 'a' && c <= 'z')
      return toHChar(c + ('A' - 'a'));
   else
      return c;
}

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

/* Some flags.  */
#define VG_MSG_SIGNED    1 /* The value is signed. */
#define VG_MSG_ZJUSTIFY  2 /* Must justify with '0'. */
#define VG_MSG_LJUSTIFY  4 /* Must justify on the left. */
#define VG_MSG_PAREN     8 /* Parenthesize if present (for %y) */
#define VG_MSG_COMMA    16 /* Add commas to numbers (for %d, %u) */

/* Copy a string into the buffer. */
static UInt
myvprintf_str ( void(*send)(HChar), Int flags, Int width, HChar* str, 
                Bool capitalise )
{
#  define MAYBE_TOUPPER(ch) toHChar(capitalise ? vex_toupper(ch) : (ch))
   UInt ret = 0;
   Int i, extra;
   Int len = vex_strlen(str);

   if (width == 0) {
      ret += len;
      for (i = 0; i < len; i++)
         send(MAYBE_TOUPPER(str[i]));
      return ret;
   }

   if (len > width) {
      ret += width;
      for (i = 0; i < width; i++)
         send(MAYBE_TOUPPER(str[i]));
      return ret;
   }

   extra = width - len;
   if (flags & VG_MSG_LJUSTIFY) {
      ret += extra;
      for (i = 0; i < extra; i++)
         send(' ');
   }
   ret += len;
   for (i = 0; i < len; i++)
      send(MAYBE_TOUPPER(str[i]));
   if (!(flags & VG_MSG_LJUSTIFY)) {
      ret += extra;
      for (i = 0; i < extra; i++)
         send(' ');
   }

#  undef MAYBE_TOUPPER

   return ret;
}

/* Write P into the buffer according to these args:
 *  If SIGN is true, p is a signed.
 *  BASE is the base.
 *  If WITH_ZERO is true, '0' must be added.
 *  WIDTH is the width of the field.
 */
static UInt
myvprintf_int64 ( void(*send)(HChar), Int flags, Int base, Int width, ULong p)
{
   HChar buf[40];
   Int   ind = 0;
   Int   i, nc = 0;
   Bool  neg = False;
   HChar *digits = "0123456789ABCDEF";
   UInt  ret = 0;

   if (base < 2 || base > 16)
      return ret;
 
   if ((flags & VG_MSG_SIGNED) && (Long)p < 0) {
      p   = - (Long)p;
      neg = True;
   }

   if (p == 0)
      buf[ind++] = '0';
   else {
      while (p > 0) {
         if ((flags & VG_MSG_COMMA) && 10 == base &&
             0 == (ind-nc) % 3 && 0 != ind) 
         {
            buf[ind++] = ',';
            nc++;
         }
         buf[ind++] = digits[p % base];
         p /= base;
      }
   }

   if (neg)
      buf[ind++] = '-';

   if (width > 0 && !(flags & VG_MSG_LJUSTIFY)) {
      for(; ind < width; ind++) {
         vassert(ind < 39);
         buf[ind] = toHChar((flags & VG_MSG_ZJUSTIFY) ? '0': ' ');
      }
   }

   /* Reverse copy to buffer.  */
   ret += ind;
   for (i = ind -1; i >= 0; i--) {
      send(buf[i]);
   }
   if (width > 0 && (flags & VG_MSG_LJUSTIFY)) {
      for(; ind < width; ind++) {
         ret++;
         send(' ');  // Never pad with zeroes on RHS -- changes the value!
      }
   }
   return ret;
}


/* A simple vprintf().  */
static 
UInt vprintf_wrk ( void(*send)(HChar), const HChar *format, va_list vargs )
{
   UInt ret = 0;
   int i;
   int flags;
   int width;
   Bool is_long;

   /* We assume that vargs has already been initialised by the 
      caller, using va_start, and that the caller will similarly
      clean up with va_end.
   */

   for (i = 0; format[i] != 0; i++) {
      if (format[i] != '%') {
         send(format[i]);
         ret++;
         continue;
      }
      i++;
      /* A '%' has been found.  Ignore a trailing %. */
      if (format[i] == 0)
         break;
      if (format[i] == '%') {
         /* `%%' is replaced by `%'. */
         send('%');
         ret++;
         continue;
      }
      flags = 0;
      is_long = False;
      width = 0; /* length of the field. */
      if (format[i] == '(') {
         flags |= VG_MSG_PAREN;
         i++;
      }
      /* If ',' follows '%', commas will be inserted. */
      if (format[i] == ',') {
         flags |= VG_MSG_COMMA;
         i++;
      }
      /* If '-' follows '%', justify on the left. */
      if (format[i] == '-') {
         flags |= VG_MSG_LJUSTIFY;
         i++;
      }
      /* If '0' follows '%', pads will be inserted. */
      if (format[i] == '0') {
         flags |= VG_MSG_ZJUSTIFY;
         i++;
      }
      /* Compute the field length. */
      while (format[i] >= '0' && format[i] <= '9') {
         width *= 10;
         width += format[i++] - '0';
      }
      while (format[i] == 'l') {
         i++;
         is_long = True;
      }

      switch (format[i]) {
         case 'd': /* %d */
            flags |= VG_MSG_SIGNED;
            if (is_long)
               ret += myvprintf_int64(send, flags, 10, width, 
                                      (ULong)(va_arg (vargs, Long)));
            else
               ret += myvprintf_int64(send, flags, 10, width, 
                                      (ULong)(va_arg (vargs, Int)));
            break;
         case 'u': /* %u */
            if (is_long)
               ret += myvprintf_int64(send, flags, 10, width, 
                                      (ULong)(va_arg (vargs, ULong)));
            else
               ret += myvprintf_int64(send, flags, 10, width, 
                                      (ULong)(va_arg (vargs, UInt)));
            break;
         case 'p': /* %p */
            ret += 2;
            send('0');
            send('x');
            ret += myvprintf_int64(send, flags, 16, width, 
                                   (ULong)((HWord)va_arg (vargs, void *)));
            break;
         case 'x': /* %x */
            if (is_long)
               ret += myvprintf_int64(send, flags, 16, width, 
                                      (ULong)(va_arg (vargs, ULong)));
            else
               ret += myvprintf_int64(send, flags, 16, width, 
                                      (ULong)(va_arg (vargs, UInt)));
            break;
         case 'c': /* %c */
            ret++;
            send(toHChar(va_arg (vargs, int)));
            break;
         case 's': case 'S': { /* %s */
            char *str = va_arg (vargs, char *);
            if (str == (char*) 0) str = "(null)";
            ret += myvprintf_str(send, flags, width, str, 
                                 toBool(format[i]=='S'));
            break;
         }
#        if 0
         case 'y': { /* %y - print symbol */
            Char buf[100];
            Char *cp = buf;
            Addr a = va_arg(vargs, Addr);

            if (flags & VG_MSG_PAREN)
               *cp++ = '(';
            if (VG_(get_fnname_w_offset)(a, cp, sizeof(buf)-4)) {
               if (flags & VG_MSG_PAREN) {
                  cp += VG_(strlen)(cp);
                  *cp++ = ')';
                  *cp = '\0';
               }
               ret += myvprintf_str(send, flags, width, buf, 0);
            }
            break;
         }
#        endif
         default:
            break;
      }
   }
   return ret;
}


/* A general replacement for printf().  Note that only low-level 
   debugging info should be sent via here.  The official route is to
   to use vg_message().  This interface is deprecated.
*/
static HChar myprintf_buf[1000];
static Int   n_myprintf_buf;

static void add_to_myprintf_buf ( HChar c )
{
   if (c == '\n' || n_myprintf_buf >= 1000-10 /*paranoia*/ ) {
      (*vex_log_bytes)( myprintf_buf, vex_strlen(myprintf_buf) );
      n_myprintf_buf = 0;
      myprintf_buf[n_myprintf_buf] = 0;      
   }
   myprintf_buf[n_myprintf_buf++] = c;
   myprintf_buf[n_myprintf_buf] = 0;
}

UInt vex_printf ( const char *format, ... )
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

UInt vex_sprintf ( HChar* buf, const HChar *format, ... )
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
