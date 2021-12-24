/*--------------------------------------------------------------------*/
/*--- User-mode execve().                               priv_ume.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2017 Julian Seward 
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
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

#if defined(VGO_linux) || defined(VGO_darwin) || defined(VGO_solaris) || defined(VGO_freebsd)

#ifndef __PRIV_UME_H
#define __PRIV_UME_H

#include "pub_core_ume.h"   // ExeInfo

extern Int VG_(do_exec_inner)(const HChar *exe, ExeInfo *info);

#if defined(VGO_linux) || defined(VGO_solaris) || defined(VGO_freebsd)
extern Bool VG_(match_ELF) ( const void *hdr, SizeT len );
extern Int  VG_(load_ELF)  ( Int fd, const HChar *name, ExeInfo *info );
#elif defined(VGO_darwin)
extern Bool VG_(match_macho) ( const void *hdr, SizeT len );
extern Int  VG_(load_macho)  ( Int fd, const HChar *name, ExeInfo *info );
#else
#  error Unknown OS
#endif

extern Bool VG_(match_script) ( const void *hdr, SizeT len );
extern Int  VG_(load_script)  ( Int fd, const HChar *name, ExeInfo *info );


#endif // __PRIV_UME_H

#endif // defined(VGO_linux) || defined(VGO_darwin) || defined(VGO_solaris)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/

