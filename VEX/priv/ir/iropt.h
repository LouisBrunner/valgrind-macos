
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (ir/iropt.h) is                               ---*/
/*--- Copyright (c) 2004 OpenWorks LLP.  All rights reserved. ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

#include "libvex_basictypes.h"
#include "libvex_ir.h"
#include "libvex.h"


extern IRBB* do_iropt_BB ( IRBB* bb,
                           IRExpr* (*specHelper) ( Char*, IRExpr**),
                           Bool (*preciseMemExnsFn)(Int,Int),
                           Addr64 guest_addr );

/*---------------------------------------------------------------*/
/*--- end                                          ir/iropt.h ---*/
/*---------------------------------------------------------------*/
