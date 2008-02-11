
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (libvex_trc_values.h) is                      ---*/
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

#ifndef __LIBVEX_TRC_VALUES_H
#define __LIBVEX_TRC_VALUES_H


/* Magic values that the guest state pointer might be set to when
   returning to the dispatcher.  The only other legitimate value is to
   point to the start of the thread's VEX guest state.

   This file may get included in assembly code, so do not put
   C-specific constructs in it.

   These values should be 61 or above so as not to conflict
   with Valgrind's VG_TRC_ values, which are 60 or below.
*/

#define VEX_TRC_JMP_TINVAL     61  /* invalidate translations before
                                      continuing */
#define VEX_TRC_JMP_NOREDIR    81  /* jump to undirected guest addr */
#define VEX_TRC_JMP_SIGTRAP    85  /* deliver trap (SIGTRAP) before
                                      continuing */
#define VEX_TRC_JMP_SIGSEGV    87  /* deliver segv (SIGSEGV) before
                                      continuing */
#define VEX_TRC_JMP_EMWARN     63  /* deliver emulation warning before
                                      continuing */
#define VEX_TRC_JMP_EMFAIL     83  /* emulation fatal error; abort system */

#define VEX_TRC_JMP_CLIENTREQ  65  /* do a client req before continuing */
#define VEX_TRC_JMP_YIELD      67  /* yield to thread sched 
                                      before continuing */
#define VEX_TRC_JMP_NODECODE   69  /* next instruction is not decodable */
#define VEX_TRC_JMP_MAPFAIL    71  /* address translation failed */

#define VEX_TRC_JMP_SYS_SYSCALL  73 /* do syscall before continuing */
#define VEX_TRC_JMP_SYS_INT32    75 /* do syscall before continuing */
#define VEX_TRC_JMP_SYS_INT128   77 /* do syscall before continuing */
#define VEX_TRC_JMP_SYS_SYSENTER 79 /* do syscall before continuing */

#endif /* ndef __LIBVEX_TRC_VALUES_H */

/*---------------------------------------------------------------*/
/*---                                     libvex_trc_values.h ---*/
/*---------------------------------------------------------------*/
