
/*--------------------------------------------------------------------*/
/*--- Linux-variant specific syscalls stuff.                       ---*/
/*---                                         priv_syswrap-linux.h ---*/
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

#ifndef __PRIV_SYSWRAP_LINUX_VARIANTS_H
#define __PRIV_SYSWRAP_LINUX_VARIANTS_H

#include "pub_core_basics.h"   // ThreadId

/* ---------------------------------------------------------------
   BProc wrappers
   ------------------------------------------------------------ */

#define TId ThreadId
#define UW  UWord
#define SR  SysRes

/* Return 0 means hand to kernel, non-0 means fail w/ that value. */
extern Int  ML_(linux_variant_PRE_sys_bproc)( UW, UW, UW, UW, UW, UW );

extern void ML_(linux_variant_POST_sys_bproc)( UW, UW, UW, UW, UW, UW );

#undef TId
#undef UW
#undef SR

#endif   // __PRIV_SYSWRAP_LINUX_VARIANTS_H

/*--------------------------------------------------------------------*/
/*--- end                            priv_syswrap-linux-variants.h ---*/
/*--------------------------------------------------------------------*/
