
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
#include "guest-amd64/gdefs.h"


ULong amd64g_calculate_rflags_c ( 
                ULong cc_op, 
                ULong cc_dep1, ULong cc_dep2, ULong cc_ndep 
             )
{
  vassert(0);
  return 0;
}

ULong amd64g_calculate_condition ( 
                ULong/*AMD64Condcode*/ cond, 
                ULong cc_op, 
                ULong cc_dep1, ULong cc_dep2, ULong cc_ndep 
             )
{
  vassert(0);
  return 0;
}


IRExpr* guest_amd64_spechelper ( Char* function_name,
                                 IRExpr** args )
{
  return NULL;
}

/* Figure out if any part of the guest state contained in minoff
   .. maxoff requires precise memory exceptions.  If in doubt return
   True (but this is generates significantly slower code).  

   We enforce precise exns for guest %RSP and %RIP only.
*/
Bool guest_amd64_state_requires_precise_mem_exns ( Int minoff,
                                                   Int maxoff)
{
   Int rsp_min = offsetof(VexGuestAMD64State, guest_RSP);
   Int rsp_max = rsp_min + 8 - 1;
   Int rip_min = offsetof(VexGuestAMD64State, guest_RIP);
   Int rip_max = rip_min + 8 - 1;

   if (maxoff < rsp_min || minoff > rsp_max) {
      /* no overlap with rsp */
   } else {
      return True;
   }

   if (maxoff < rip_min || minoff > rip_max) {
      /* no overlap with eip */
   } else {
      return True;
   }

   return False;
}



#define ALWAYSDEFD(field)                           \
    { offsetof(VexGuestX86State, field),            \
      (sizeof ((VexGuestX86State*)0)->field) }

VexGuestLayout
amd64guest_layout;
#if 0
      = {
          /* Total size of the guest state, in bytes. */
          .total_sizeB = sizeof(VexGuestX86State),

          /* Describe the stack pointer. */
          .offset_SP = offsetof(VexGuestX86State,guest_ESP),
          .sizeof_SP = 4,

          /* Describe the instruction pointer. */
          .offset_IP = offsetof(VexGuestX86State,guest_EIP),
          .sizeof_IP = 4,

          /* Describe any sections to be regarded by Memcheck as
             'always-defined'. */
          .n_alwaysDefd = 18,

          /* flags thunk: OP and NDEP are always defd, whereas DEP1
             and DEP2 have to be tracked.  See detailed comment in
             gdefs.h on meaning of thunk fields. */
          .alwaysDefd
             = { /*  0 */ ALWAYSDEFD(guest_CC_OP),
                 /*  1 */ ALWAYSDEFD(guest_CC_NDEP),
                 /*  2 */ ALWAYSDEFD(guest_DFLAG),
                 /*  3 */ ALWAYSDEFD(guest_IDFLAG),
                 /*  4 */ ALWAYSDEFD(guest_EIP),
                 /*  5 */ ALWAYSDEFD(guest_FTOP),
                 /*  6 */ ALWAYSDEFD(guest_FPTAG),
                 /*  7 */ ALWAYSDEFD(guest_FPROUND),
                 /*  8 */ ALWAYSDEFD(guest_FC3210),
                 /*  9 */ ALWAYSDEFD(guest_CS),
                 /* 10 */ ALWAYSDEFD(guest_DS),
                 /* 11 */ ALWAYSDEFD(guest_ES),
                 /* 12 */ ALWAYSDEFD(guest_FS),
                 /* 13 */ ALWAYSDEFD(guest_GS),
                 /* 14 */ ALWAYSDEFD(guest_SS),
                 /* 15 */ ALWAYSDEFD(guest_LDT),
                 /* 16 */ ALWAYSDEFD(guest_GDT),
                 /* 17 */ ALWAYSDEFD(guest_EMWARN)
               }
        };
#endif


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
