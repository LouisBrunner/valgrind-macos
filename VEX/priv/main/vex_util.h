
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (vex_util.h) is                               ---*/
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

#ifndef __VEX_UTIL_H
#define __VEX_UTIL_H

#include "libvex_basictypes.h"


/* Misc. */

#define NULL ((void*)0)


/* Stuff for panicking and assertion. */

#define VG__STRING(__str)  #__str

#define vassert(expr)                                           \
  ((void) ((expr) ? 0 :                                         \
           (vex_assert_fail (VG__STRING(expr),                  \
                             __FILE__, __LINE__,                \
                             __PRETTY_FUNCTION__), 0)))

__attribute__ ((__noreturn__))
extern void vex_assert_fail ( const HChar* expr, const HChar* file,
                              Int line, const HChar* fn );
__attribute__ ((__noreturn__))
extern void vpanic ( HChar* str );


/* Printing */

__attribute__ ((format (printf, 1, 2)))
extern UInt vex_printf ( HChar *format, ... );

__attribute__ ((format (printf, 2, 3)))
extern UInt vex_sprintf ( HChar* buf, HChar *format, ... );


/* String ops */

extern Bool vex_streq ( const HChar* s1, const HChar* s2 );


/* Storage management: clear the area, and allocate from it. */

/* By default allocation occurs in the temporary area.  However, it is
   possible to switch to permanent area allocation if that's what you
   want.  Permanent area allocation is very limited, tho. */

typedef
   enum {
      VexAllocModeTEMP, 
      VexAllocModePERM 
   }
   VexAllocMode;

extern void         vexSetAllocMode ( VexAllocMode );
extern VexAllocMode vexGetAllocMode ( void );
extern void         vexAllocSanityCheck ( void );

extern void vexSetAllocModeTEMP_and_clear ( void );

#endif /* ndef __VEX_UTIL_H */

/*---------------------------------------------------------------*/
/*---                                              vex_util.h ---*/
/*---------------------------------------------------------------*/
