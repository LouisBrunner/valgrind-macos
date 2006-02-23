
/*--------------------------------------------------------------------*/
/*--- Client-space code for the core.               vg_preloaded.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2005 Julian Seward 
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


/* ---------------------------------------------------------------------
   ALL THE CODE IN THIS FILE RUNS ON THE SIMULATED CPU. 

   These functions are not called directly - they're the targets of code
   redirection or load notifications (see pub_core_redir.h for info).
   They're named weirdly so that the intercept code can find them when the
   shared object is initially loaded.

   Note that this filename has the "vg_" prefix because it can appear
   in stack traces, and the "vg_" makes it a little clearer that it
   originates from Valgrind.
   ------------------------------------------------------------------ */

#include "valgrind.h"
#include "coregrind.h"

#include "pub_core_basics.h"
#include "pub_core_debuginfo.h"  // Needed for pub_core_redir.h
#include "pub_core_redir.h"      // For VG_NOTIFY_ON_LOAD

/* ---------------------------------------------------------------------
   Hook for running __libc_freeres once the program exits.
   ------------------------------------------------------------------ */

void VG_NOTIFY_ON_LOAD(freeres)( void );
void VG_NOTIFY_ON_LOAD(freeres)( void )
{
   int res;
#ifndef __UCLIBC__
   extern void __libc_freeres(void);
   __libc_freeres();
#endif
   VALGRIND_MAGIC_SEQUENCE(res, 0 /* default */,
                           VG_USERREQ__LIBC_FREERES_DONE, 0, 0, 0, 0);
   /*NOTREACHED*/
   *(int *)0 = 'x';
}


/* ---------------------------------------------------------------------
   Avoid glibc's floor/ceil functions on ppc32/64.  In recent glibcs
   (about 2.3.4 and after) these rely on doing fadd/fsub with with
   round to +inf/-inf set, which vex does not currently handle
   correctly.  This just reroutes to the glibc default implementations.
   This is a really ugly hack.
   ------------------------------------------------------------------ */

#if defined(VGP_ppc32_linux)
/*
 * ====================================================
 * Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
 *
 * Developed at SunPro, a Sun Microsystems, Inc. business.
 * Permission to use, copy, modify, and distribute this
 * software is freely granted, provided that this notice
 * is preserved.
 * ====================================================
 */
/*
 * floor(x)
 * Return x rounded toward -inf to integral value
 * Method:
 *      Bit twiddling.
 * Exception:
 *      Inexact flag raised if x not equal to floor(x).
 */

typedef union
{
  double value;
  struct
  {
    /*u_int32_t*/ UInt msw;
    /*u_int32_t*/ UInt lsw;
  } parts;
} ieee_double_shape_type;

/* Get two 32 bit ints from a double.  */
#define EXTRACT_WORDS(ix0,ix1,d)                                \
do {                                                            \
  ieee_double_shape_type ew_u;                                  \
  ew_u.value = (d);                                             \
  (ix0) = ew_u.parts.msw;                                       \
  (ix1) = ew_u.parts.lsw;                                       \
} while (0)


/* Set a double from two 32 bit ints.  */
#define INSERT_WORDS(d,ix0,ix1)                                 \
do {                                                            \
  ieee_double_shape_type iw_u;                                  \
  iw_u.parts.msw = (ix0);                                       \
  iw_u.parts.lsw = (ix1);                                       \
  (d) = iw_u.value;                                             \
} while (0)

static double bit_twiddling_floor ( double x )
{
   static const double huge = 1.0e300;
        /*int32_t*/   Int i0,i1,j0;
        /*u_int32_t*/ UInt i,j;
        EXTRACT_WORDS(i0,i1,x);
        j0 = ((i0>>20)&0x7ff)-0x3ff;
        if(j0<20) {
            if(j0<0) {  /* raise inexact if x != 0 */
                if(huge+x>0.0) {/* return 0*sign(x) if |x|<1 */
                    if(i0>=0) {i0=i1=0;}
                    else if(((i0&0x7fffffff)|i1)!=0)
                        { i0=0xbff00000;i1=0;}
                }
            } else {
                i = (0x000fffff)>>j0;
                if(((i0&i)|i1)==0) return x; /* x is integral */
                if(huge+x>0.0) {        /* raise inexact flag */
                    if(i0<0) i0 += (0x00100000)>>j0;
                    i0 &= (~i); i1=0;
                }
            }
        } else if (j0>51) {
            if(j0==0x400) return x+x;   /* inf or NaN */
            else return x;              /* x is integral */
        } else {
            i = ((/*u_int32_t*/UInt)(0xffffffff))>>(j0-20);
            if((i1&i)==0) return x;     /* x is integral */
            if(huge+x>0.0) {            /* raise inexact flag */
                if(i0<0) {
                    if(j0==20) i0+=1;
                    else {
                        j = i1+(1<<(52-j0));
                        if(j<i1) i0 +=1 ;       /* got a carry */
                        i1=j;
                    }
                }
                i1 &= (~i);
            }
        }
        INSERT_WORDS(x,i0,i1);
        return x;
}

/* Catch libm.so.6:__floor */
double VG_REPLACE_FUNCTION(libmZdsoZd6,__floor)(double);
double VG_REPLACE_FUNCTION(libmZdsoZd6,__floor)(double x) {
   return bit_twiddling_floor(x);
}

/* Catch libm.so.6:floor */
double VG_REPLACE_FUNCTION(libmZdsoZd6,floor)(double);
double VG_REPLACE_FUNCTION(libmZdsoZd6,floor)(double x) {
   return bit_twiddling_floor(x);
}


/*
 * ceil(x)
 * Return x rounded toward -inf to integral value
 * Method:
 *      Bit twiddling.
 * Exception:
 *      Inexact flag raised if x not equal to ceil(x).
 */
static double bit_twiddling_ceil ( double x )
{
   static const double huge = 1.0e300;
        /*int32_t*/ Int i0,i1,j0;
        /*u_int32_t*/ UInt i,j;
        EXTRACT_WORDS(i0,i1,x);
        j0 = ((i0>>20)&0x7ff)-0x3ff;
        if(j0<20) {
            if(j0<0) {  /* raise inexact if x != 0 */
                if(huge+x>0.0) {/* return 0*sign(x) if |x|<1 */
                    if(i0<0) {i0=0x80000000;i1=0;}
                    else if((i0|i1)!=0) { i0=0x3ff00000;i1=0;}
                }
            } else {
                i = (0x000fffff)>>j0;
                if(((i0&i)|i1)==0) return x; /* x is integral */
                if(huge+x>0.0) {        /* raise inexact flag */
                    if(i0>0) i0 += (0x00100000)>>j0;
                    i0 &= (~i); i1=0;
                }
            }
        } else if (j0>51) {
            if(j0==0x400) return x+x;   /* inf or NaN */
            else return x;              /* x is integral */
        } else {
            i = ((/*u_int32_t*/UInt)(0xffffffff))>>(j0-20);
            if((i1&i)==0) return x;     /* x is integral */
            if(huge+x>0.0) {            /* raise inexact flag */
                if(i0>0) {
                    if(j0==20) i0+=1;
                    else {
                        j = i1 + (1<<(52-j0));
                        if(j<i1) i0+=1; /* got a carry */
                        i1 = j;
                    }
                }
                i1 &= (~i);
            }
        }
        INSERT_WORDS(x,i0,i1);
        return x;
}

/* Catch libm.so.6:__ceil */
double VG_REPLACE_FUNCTION(libmZdsoZd6,__ceil)(double);
double VG_REPLACE_FUNCTION(libmZdsoZd6,__ceil)(double x) {
   return bit_twiddling_ceil(x);
}

/* Catch libm.so.6:ceil */
double VG_REPLACE_FUNCTION(libmZdsoZd6,ceil)(double);
double VG_REPLACE_FUNCTION(libmZdsoZd6,ceil)(double x) {
   return bit_twiddling_ceil(x);
}
#endif

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
