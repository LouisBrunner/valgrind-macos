
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (ir/irmatch.c) is                             ---*/
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

#include "main/vex_util.h"
#include "ir/irmatch.h"


/* Assign a value to a binder.  Checks for obvious stupidities. */

static 
void setBindee ( MatchInfo* mi, Int n, IRExpr* bindee )
{
   if (n < 0 || n >= N_IRMATCH_BINDERS)
      vpanic("setBindee: out of range index");
   if (mi->bindee[n] != NULL)
      vpanic("setBindee: bindee already set");
   mi->bindee[n] = bindee;
}


/* This is the actual matching function, recursing over the pattern
   and expression trees in the obvious way, and dumping any matches
   found into 'mi'. */

static 
Bool matchWrk ( MatchInfo* mi, IRExpr* p/*attern*/, IRExpr* e/*xpr*/ )
{
   switch (p->tag) {
      case Iex_Binder: /* aha, what we were looking for. */
         setBindee(mi, p->Iex.Binder.binder, e);
         return True;
#if 0
      case Iex_GetI:
         if (e->tag != Iex_GetI) return False;
         if (p->Iex.GetI.ty != e->Iex.GetI.ty) return False;
         /* we ignore the offset limit hints .. */
         if (!matchWrk(mi, p->Iex.GetI.offset, e->Iex.GetI.offset))
            return False;
         return True;
#endif
      case Iex_Unop:
         if (e->tag != Iex_Unop) return False;
         if (p->Iex.Unop.op != e->Iex.Unop.op) return False;
         if (!matchWrk(mi, p->Iex.Unop.arg, e->Iex.Unop.arg))
            return False;
         return True;
      case Iex_Binop:
         if (e->tag != Iex_Binop) return False;
         if (p->Iex.Binop.op != e->Iex.Binop.op) return False;
         if (!matchWrk(mi, p->Iex.Binop.arg1, e->Iex.Binop.arg1))
            return False;
         if (!matchWrk(mi, p->Iex.Binop.arg2, e->Iex.Binop.arg2))
            return False;
         return True;
      case Iex_LDle:
         if (e->tag != Iex_LDle) return False;
         if (p->Iex.LDle.ty != e->Iex.LDle.ty) return False;
         if (!matchWrk(mi, p->Iex.LDle.addr, e->Iex.LDle.addr))
            return False;
         return True;
      case Iex_Const:
         if (e->tag != Iex_Const) return False;
         return eqIRConst(p->Iex.Const.con, e->Iex.Const.con);
      default: 
         ppIRExpr(p);
         vpanic("match");
   }
}


/* Top level entry point to the matcher. */

Bool matchIRExpr ( MatchInfo* mi, IRExpr* p/*attern*/, IRExpr* e/*xpr*/ )
{
   Int i;
   for (i = 0; i < N_IRMATCH_BINDERS; i++)
      mi->bindee[i] = NULL;
   return matchWrk(mi, p, e);
}



/*---------------------------------------------------------------*/
/*--- end                                        ir/irmatch.c ---*/
/*---------------------------------------------------------------*/
