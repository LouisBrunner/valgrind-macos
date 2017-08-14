
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (ir/irmatch.c) is                             ---*/
/*--- Copyright (C) OpenWorks LLP.  All rights reserved.      ---*/
/*---                                                         ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of LibVEX, a library for dynamic binary
   instrumentation and translation.

   Copyright (C) 2004-2008 OpenWorks LLP.  All rights reserved.

   This library is made available under a dual licensing scheme.

   If you link LibVEX against other code all of which is itself
   licensed under the GNU General Public License, version 2 dated June
   1991 ("GPL v2"), then you may use LibVEX under the terms of the GPL
   v2, as appearing in the file LICENSE.GPL.  If the file LICENSE.GPL
   is missing, you can obtain a copy of the GPL v2 from the Free
   Software Foundation Inc., 51 Franklin St, Fifth Floor, Boston, MA
   02110-1301, USA.

   For any other uses of LibVEX, you must first obtain a commercial
   license from OpenWorks LLP.  Please contact info@open-works.co.uk
   for information about commercial licensing.

   This software is provided by OpenWorks LLP "as is" and any express
   or implied warranties, including, but not limited to, the implied
   warranties of merchantability and fitness for a particular purpose
   are disclaimed.  In no event shall OpenWorks LLP be liable for any
   direct, indirect, incidental, special, exemplary, or consequential
   damages (including, but not limited to, procurement of substitute
   goods or services; loss of use, data, or profits; or business
   interruption) however caused and on any theory of liability,
   whether in contract, strict liability, or tort (including
   negligence or otherwise) arising in any way out of the use of this
   software, even if advised of the possibility of such damage.

   Neither the names of the U.S. Department of Energy nor the
   University of California nor the names of its contributors may be
   used to endorse or promote products derived from this software
   without prior written permission.
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
      case Iex_Load:
         if (e->tag != Iex_Load) return False;
         if (p->Iex.Load.end != e->Iex.Load.end) return False;
         if (p->Iex.Load.ty != e->Iex.Load.ty) return False;
         if (!matchWrk(mi, p->Iex.Load.addr, e->Iex.Load.addr))
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
