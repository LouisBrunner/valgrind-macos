
/*--------------------------------------------------------------------*/
/*--- Stack management.                                 m_stacks.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2017 Julian Seward 
      jseward@acm.org

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

#ifndef __PUB_CORE_STACKS_H
#define __PUB_CORE_STACKS_H

#include "pub_core_basics.h"    // VG_ macro

//--------------------------------------------------------------------
// PURPOSE: This module deals with the registration of stacks for the
// purposes of detecting stack switches.
//--------------------------------------------------------------------

/* Convention for start and end:
   'start' is the lowest address, 'end' is the highest address.
   'start' and 'end' bytes are included in the stack.
   In other words, the stack is the byte interval ['start', 'end']
   (both bounds are included).

   Note: for compatibility reasons, VG_(register_stack) accepts
   'start' bigger than 'end' and will (transparently) swap 'start/end'
   to register the stack. */
extern UWord VG_(register_stack)   ( Addr start, Addr end );
extern void  VG_(deregister_stack) ( UWord id );
extern void  VG_(change_stack)     ( UWord id, Addr start, Addr end );
extern void  VG_(stack_limits)     ( Addr SP, Addr *start, Addr *end );

extern VG_REGPARM(3)
       void VG_(unknown_SP_update_w_ECU)
                                   ( Addr old_SP, Addr new_SP, UInt ecu );
extern VG_REGPARM(2)
       void VG_(unknown_SP_update) ( Addr old_SP, Addr new_SP );

#endif   // __PUB_CORE_STACKS_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/

