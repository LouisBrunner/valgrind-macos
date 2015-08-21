
/*---------------------------------------------------------------*/
/*--- begin                                        ir_match.h ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2004-2015 OpenWorks LLP
      info@open-works.net

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
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
   02110-1301, USA.

   The GNU General Public License is contained in the file COPYING.

   Neither the names of the U.S. Department of Energy nor the
   University of California nor the names of its contributors may be
   used to endorse or promote products derived from this software
   without prior written permission.
*/

/* Provides a facility for doing IR tree matching. */

#ifndef __VEX_IR_MATCH_H
#define __VEX_IR_MATCH_H

#include "libvex_basictypes.h"
#include "libvex_ir.h"
#include "main_util.h"          // NULL

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


#endif /* ndef __VEX_IR_MATCH_H */

/*---------------------------------------------------------------*/
/*--- end                                          ir_match.h ---*/
/*---------------------------------------------------------------*/
