
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (libvex_trc_values.h) is                      ---*/
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

#ifndef __LIBVEX_TRC_VALUES_H
#define __LIBVEX_TRC_VALUES_H


/* Magic values that the guest state pointer might be set to when
   returning to the dispatcher.  The only other legitimate value is to
   point to the start of the thread's VEX guest state.

   This file may get included in assembly code, so do not put
   C-specific constructs in it.
*/

#define VEX_TRC_JMP_TINVAL     13  /* invalidate translations before
                                      continuing */
#define VEX_TRC_JMP_EMWARN     17  /* deliver emulation warning before
                                      continuing */
#define VEX_TRC_JMP_SYSCALL    19  /* do a system call before continuing */
#define VEX_TRC_JMP_CLIENTREQ  23  /* do a client req before continuing */
#define VEX_TRC_JMP_YIELD      27  /* yield to thread sched 
                                      before continuing */
#define VEX_TRC_JMP_NODECODE   29  /* next instruction in not decodable */
#define VEX_TRC_JMP_MAPFAIL    31  /* address translation failed */


#endif /* ndef __LIBVEX_TRC_VALUES_H */

/*---------------------------------------------------------------*/
/*---                                     libvex_trc_values.h ---*/
/*---------------------------------------------------------------*/
