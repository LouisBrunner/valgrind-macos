
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (ir/iropt.h) is                               ---*/
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

#ifndef __LIBVEX_IROPT_H
#define __LIBVEX_IROPT_H

#include "libvex_basictypes.h"
#include "libvex_ir.h"
#include "libvex.h"

/* Top level optimiser entry point.  Returns a new BB.  Operates
   under the control of the global "vex_control" struct. */
extern 
IRBB* do_iropt_BB ( IRBB* bb,
                    IRExpr* (*specHelper) (HChar*, IRExpr**),
                    Bool (*preciseMemExnsFn)(Int,Int),
                    Addr64 guest_addr );

/* Do a constant folding/propagation pass. */
extern
IRBB* cprop_BB ( IRBB* );

/* Do a dead-code removal pass, which is generally needed to avoid
   crashing the tree-builder. bb is destructively modified. */
extern
void do_deadcode_BB ( IRBB* bb );

/* Do a CSE pass.  bb is destructively modified. */
extern
void do_cse_BB ( IRBB* bb );

/* The tree-builder.  Make maximal safe trees.  bb is destructively
   modified. */
extern
void do_treebuild_BB ( IRBB* bb );

#endif /* ndef __LIBVEX_IROPT_H */

/*---------------------------------------------------------------*/
/*--- end                                          ir/iropt.h ---*/
/*---------------------------------------------------------------*/
