
/*---------------------------------------------------------------*/
/*---                                                         ---*/
/*--- This file (guest-amd64/ghelpers.c) is                   ---*/
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

#include "libvex_basictypes.h"
#include "libvex_emwarn.h"
#include "libvex_guest_amd64.h"
#include "libvex_ir.h"
#include "libvex.h"

#include "main/vex_util.h"
//#include "guest-amd64/gdefs.h"    // put back in when that file is added


/* VISIBLE TO LIBVEX CLIENT */
void LibVEX_GuestAMD64_initialise ( /*OUT*/VexGuestAMD64State* vex_state )
{
   //Int i;

   vex_state->guest_RAX = 0;
   vex_state->guest_RCX = 0;
   vex_state->guest_RDX = 0;
   vex_state->guest_RBX = 0;
   vex_state->guest_RSP = 0;
   vex_state->guest_RBP = 0;
   vex_state->guest_RSI = 0;
   vex_state->guest_RDI = 0;
   vex_state->guest_R8  = 0;
   vex_state->guest_R9  = 0;
   vex_state->guest_R10 = 0;
   vex_state->guest_R11 = 0;
   vex_state->guest_R12 = 0;
   vex_state->guest_R13 = 0;
   vex_state->guest_R14 = 0;
   vex_state->guest_R15 = 0;

   vex_state->guest_CC_OP   = 999;//X86G_CC_OP_COPY;  // XXX ???
   vex_state->guest_CC_DEP1 = 0;
   vex_state->guest_CC_DEP2 = 0;
   vex_state->guest_CC_NDEP = 0;

   // XXX: add more here later, for D/ID flags

   vex_state->guest_RIP = 0;

   // XXX: add more here later, for segment registers, FPU, etc.

   vex_state->guest_EMWARN = EmWarn_NONE;
}

/*---------------------------------------------------------------*/
/*--- end                              guest-amd64/ghelpers.c ---*/
/*---------------------------------------------------------------*/
