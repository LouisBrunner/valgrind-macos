
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (ir/iropt.h) is                               ---*/
/*--- Copyright (c) 2004 OpenWorks LLP.  All rights reserved. ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

#include "libvex_basictypes.h"
#include "libvex_ir.h"
#include "libvex.h"

/* Top level optimiser entry point.  Returns a new BB.  Operates
   under the control of the global "vex_control" struct. */
extern 
IRBB* do_iropt_BB ( IRBB* bb,
                    IRExpr* (*specHelper) ( Char*, IRExpr**),
                    Bool (*preciseMemExnsFn)(Int,Int),
                    Addr64 guest_addr );

/* Do a dead-code removal pass, which is generally needed to avoid
   crashing the tree-builder. bb is destructively modified. */
extern
void do_deadcode_BB ( IRBB* bb );

/* The tree-builder.  Make maximal safe trees.  bb is destructively
   modified. */
extern
void do_treebuild_BB ( IRBB* bb );

/*---------------------------------------------------------------*/
/*--- end                                          ir/iropt.h ---*/
/*---------------------------------------------------------------*/
