
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (ir/irmatch.h) is                             ---*/
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

/* Provides a facility for doing IR tree matching. */

#ifndef __LIBVEX_IRMATCH_H
#define __LIBVEX_IRMATCH_H

#include "libvex_basictypes.h"
#include "libvex_ir.h"


/* Patterns are simply IRExpr* trees, with IRExpr_Binder nodes at the
   leaves, indicating binding points.  Use these magic macros to
   declare and define patterns. */

#define DECLARE_PATTERN(_patt) \
   static IRExpr* _patt = NULL

#define DEFINE_PATTERN(_patt,_expr)                            \
   do {                                                        \
      if (!(_patt)) {                                          \
         vassert(vexGetAllocMode() == VexAllocModeTEMP);       \
         vexSetAllocMode(VexAllocModePERM);                    \
         _patt = (_expr);                                      \
         vexSetAllocMode(VexAllocModeTEMP);                    \
         vassert(vexGetAllocMode() == VexAllocModeTEMP);       \
      }                                                        \
   } while (0)


/* This type returns the result of a match -- it records what
   the binders got instantiated to. */

#define N_IRMATCH_BINDERS 4

typedef
   struct {
      IRExpr* bindee[N_IRMATCH_BINDERS];
   }
   MatchInfo;


/* The matching function.  p is expected to have zero or more
   IRExpr_Binds in it, numbered 0, 1, 2 ... Returns True if a match
   succeeded. */

extern
Bool matchIRExpr ( MatchInfo* mi, IRExpr* p/*attern*/, IRExpr* e/*xpr*/ );


#endif /* ndef __LIBVEX_IRMATCH_H */



/*---------------------------------------------------------------*/
/*--- end                                        ir/irmatch.h ---*/
/*---------------------------------------------------------------*/
