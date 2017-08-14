
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (ir/iropt.h) is                               ---*/
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

#ifndef __LIBVEX_IROPT_H
#define __LIBVEX_IROPT_H

#include "libvex_basictypes.h"
#include "libvex_ir.h"
#include "libvex.h"

/* Top level optimiser entry point.  Returns a new BB.  Operates
   under the control of the global "vex_control" struct. */
extern 
IRSB* do_iropt_BB ( IRSB* bb,
                    IRExpr* (*specHelper) (HChar*, IRExpr**),
                    Bool (*preciseMemExnsFn)(Int,Int),
                    Addr64 guest_addr );

/* Do a constant folding/propagation pass. */
extern
IRSB* cprop_BB ( IRSB* );

/* Do a dead-code removal pass.  bb is destructively modified. */
extern
void do_deadcode_BB ( IRSB* bb );

/* The tree-builder.  Make (approximately) maximal safe trees.  bb is
   destructively modified. */
extern
void ado_treebuild_BB ( IRSB* bb );

#endif /* ndef __LIBVEX_IROPT_H */

/*---------------------------------------------------------------*/
/*--- end                                          ir/iropt.h ---*/
/*---------------------------------------------------------------*/
