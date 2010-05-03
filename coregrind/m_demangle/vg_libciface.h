
/*--------------------------------------------------------------------*/
/*--- Demangling of C++ mangled names.              vg_libciface.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2010 Julian Seward 
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

/* This file contains a bunch of macro definitions etc which are
   really "impedance matchers".  They make it possible for
   cp-demangle.c, cplus-dem.c, dyn-string.c and safe-ctype.c (taken
   from GNU libiberty) to be compiled in the Valgrind framework with a
   minimum of changes.

   This file should only be included in files which contain libiberty
   code. */

#include "pub_core_basics.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcprint.h"
#include "pub_core_mallocfree.h"


#define abort()              vg_assert(0)
#define atoi(_str)           VG_(strtoll10)((_str), NULL)
#define free(_pt)            VG_(arena_free)   (VG_AR_DEMANGLE,(_pt))
#define memcmp(_s1,_s2,_sz)  VG_(memcmp)((_s1),(_s2),(_sz))
#define memcpy(_dd,_ss,_sz)  VG_(memcpy)((_dd),(_ss),(_sz))
#define memset(_ss,_cc,_sz)  VG_(memset)((_ss),(_cc),(_sz))
#define realloc(_cc,_pt,_sz) VG_(arena_realloc)(VG_AR_DEMANGLE,(_cc),(_pt),(_sz))
#define sprintf(_buf,_fmt,_args...)  VG_(sprintf)((_buf),(_fmt),(_args))
#define strcat(_dd,_ss)      VG_(strcat)((_dd),(_ss))
#define strchr(_ss,_cc)      VG_(strchr)((_ss),(_cc))
#define strcmp(_s1,_s2)      VG_(strcmp)((_s1),(_s2))
#define strcpy(_dd,_ss)      VG_(strcpy)((_dd),(_ss))
#define strcspn(_ss,_rr)     VG_(strcspn)((_ss),(_rr))
#define strlen(_str)         VG_(strlen)(_str)
#define strncat(_dd,_ss,_nn) VG_(strncat)((_dd),(_ss),(_nn))
#define strncmp(_s1,_s2,_sz) VG_(strncmp)((_s1),(_s2),(_sz))
#define strncpy(_dd,_ss,_sz) VG_(strncpy)((_dd),(_ss),(_sz))
#define strpbrk(_ss,_aa)     VG_(strpbrk)((_ss),(_aa))
#define strspn(_ss,_aa)      VG_(strspn)((_ss),(_aa))
#define strstr(_hh,_nn)      VG_(strstr)((_hh),(_nn))

#define size_t SizeT

#define xmalloc(_nn)      \
   VG_(arena_malloc)(VG_AR_DEMANGLE, "m_demangle.xmalloc", (_nn))
#define xrealloc(_pt,_sz) \
   VG_(arena_realloc)(VG_AR_DEMANGLE,"m_demangle.xrealloc",(_pt),(_sz))
#define xstrdup(_str) \
   VG_(arena_strdup)(VG_AR_DEMANGLE,"m_demangle.xstrdup",(_str))

/* Required by safe-ctype.h */

#undef EOF
#define EOF -1

/* Taken from libiberty.h: */

#define ARRAY_SIZE(_arr) \
        (sizeof (_arr) / sizeof ((_arr)[0]))

#define XNEWVEC(_Ty, _Nn)    \
        ((_Ty *) xmalloc(sizeof (_Ty) * (_Nn)))

#define XRESIZEVEC(_Ty, _Pt, _Nn) \
        ((_Ty *) xrealloc((void *) (_Pt), sizeof (_Ty) * (_Nn)))

#define XRESIZEVAR(_Ty, _Pt, _Sz) \
        ((_Ty *) xrealloc((_Pt), (_Sz)))

#define XNEW(_Ty) \
        ((_Ty *) xmalloc(sizeof (_Ty)))


/*--------------------------------------------------------------------*/
/*--- end                                           vg_libciface.h ---*/
/*--------------------------------------------------------------------*/
