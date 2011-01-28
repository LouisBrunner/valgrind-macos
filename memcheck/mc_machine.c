
/*--------------------------------------------------------------------*/
/*--- Contains machine-specific (guest-state-layout-specific)      ---*/
/*--- support for origin tracking.                                 ---*/
/*---                                                 mc_machine.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of MemCheck, a heavyweight Valgrind tool for
   detecting memory errors.

   Copyright (C) 2008-2010 OpenWorks Ltd
      info@open-works.co.uk

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307, USA.

   The GNU General Public License is contained in the file COPYING.

   Neither the names of the U.S. Department of Energy nor the
   University of California nor the names of its contributors may be
   used to endorse or promote products derived from this software
   without prior written permission.
*/

#include "pub_tool_basics.h"
#include "pub_tool_hashtable.h"     // For mc_include.h
#include "pub_tool_libcassert.h"
#include "pub_tool_libcprint.h"
#include "pub_tool_tooliface.h"

#include "mc_include.h"

#undef MC_SIZEOF_GUEST_STATE

#if defined(VGA_x86)
# include "libvex_guest_x86.h"
# define MC_SIZEOF_GUEST_STATE sizeof(VexGuestX86State)
#endif

#if defined(VGA_amd64)
# include "libvex_guest_amd64.h"
# define MC_SIZEOF_GUEST_STATE sizeof(VexGuestAMD64State)
#endif

#if defined(VGA_ppc32)
# include "libvex_guest_ppc32.h"
# define MC_SIZEOF_GUEST_STATE sizeof(VexGuestPPC32State)
#endif

#if defined(VGA_ppc64)
# include "libvex_guest_ppc64.h"
# define MC_SIZEOF_GUEST_STATE sizeof(VexGuestPPC64State)
#endif

#if defined(VGA_arm)
# include "libvex_guest_arm.h"
# define MC_SIZEOF_GUEST_STATE sizeof(VexGuestARMState)
#endif

static inline Bool host_is_big_endian ( void ) {
   UInt x = 0x11223344;
   return 0x1122 == *(UShort*)(&x);
}
static inline Bool host_is_little_endian ( void ) {
   UInt x = 0x11223344;
   return 0x3344 == *(UShort*)(&x);
}


/* Let (offset,szB) describe a reference to the guest state section
   [offset, offset+szB).

   This function returns the corresponding guest state reference to be
   used for the origin tag (which of course will be in the second
   shadow area), or -1 if this piece of guest state is not to be
   tracked.

   Since origin tags are 32-bits long, we expect any returned value
   (except -1) to be a multiple of 4, between 0 and
   sizeof(guest-state)-4 inclusive.

   This is inherently (guest-)architecture specific.  For x86 and
   amd64 we do some somewhat tricky things to give %AH .. %DH their
   own tags.  On ppc32/64 we do some marginally tricky things to give
   all 16 %CR components their own tags.

   This function only deals with references to the guest state whose
   offsets are known at translation time (that is, references arising
   from Put and Get).  References whose offset is not known until run
   time (that is, arise from PutI and GetI) are handled by
   MC_(get_otrack_reg_array_equiv_int_type) below.

   Note that since some guest state arrays (eg, the x86 FP reg stack)
   are accessed both as arrays (eg, x87 insns) and directly (eg, MMX
   insns), the two functions must be consistent for those sections of
   guest state -- that is, they must both say the area is shadowed, or
   both say it is not.

   This function is dependent on the host's endianness, hence we
   assert that the use case is supported.
*/
static Int get_otrack_shadow_offset_wrk ( Int offset, Int szB ); /*fwds*/

Int MC_(get_otrack_shadow_offset) ( Int offset, Int szB )
{
   Int cand = get_otrack_shadow_offset_wrk( offset, szB );
   if (cand == -1) 
      return cand;
   tl_assert(0 == (cand & 3));
   tl_assert(cand <= MC_SIZEOF_GUEST_STATE-4);
   return cand;
}


static Int get_otrack_shadow_offset_wrk ( Int offset, Int szB )
{
   /* -------------------- ppc64 -------------------- */

#  if defined(VGA_ppc64)

#  define GOF(_fieldname) \
      (offsetof(VexGuestPPC64State,guest_##_fieldname))
#  define SZB(_fieldname) \
      (sizeof(((VexGuestPPC64State*)0)->guest_##_fieldname))

   Int  sz   = szB;
   Int  o    = offset;
   tl_assert(sz > 0);
   tl_assert(host_is_big_endian());

   if (sz == 8 || sz == 4) {
      /* The point of this is to achieve
         if ((o == GOF(GPRn) && sz == 8) || (o == 4+GOF(GPRn) && sz == 4))
            return GOF(GPRn);
         by testing ox instead of o, and setting ox back 4 bytes when sz == 4.
      */
      Int ox = sz == 8 ? o : (o - 4);
      if (ox == GOF(GPR0)) return ox;
      if (ox == GOF(GPR1)) return ox;
      if (ox == GOF(GPR2)) return ox;
      if (ox == GOF(GPR3)) return ox;
      if (ox == GOF(GPR4)) return ox;
      if (ox == GOF(GPR5)) return ox;
      if (ox == GOF(GPR6)) return ox;
      if (ox == GOF(GPR7)) return ox;
      if (ox == GOF(GPR8)) return ox;
      if (ox == GOF(GPR9)) return ox;
      if (ox == GOF(GPR10)) return ox;
      if (ox == GOF(GPR11)) return ox;
      if (ox == GOF(GPR12)) return ox;
      if (ox == GOF(GPR13)) return ox;
      if (ox == GOF(GPR14)) return ox;
      if (ox == GOF(GPR15)) return ox;
      if (ox == GOF(GPR16)) return ox;
      if (ox == GOF(GPR17)) return ox;
      if (ox == GOF(GPR18)) return ox;
      if (ox == GOF(GPR19)) return ox;
      if (ox == GOF(GPR20)) return ox;
      if (ox == GOF(GPR21)) return ox;
      if (ox == GOF(GPR22)) return ox;
      if (ox == GOF(GPR23)) return ox;
      if (ox == GOF(GPR24)) return ox;
      if (ox == GOF(GPR25)) return ox;
      if (ox == GOF(GPR26)) return ox;
      if (ox == GOF(GPR27)) return ox;
      if (ox == GOF(GPR28)) return ox;
      if (ox == GOF(GPR29)) return ox;
      if (ox == GOF(GPR30)) return ox;
      if (ox == GOF(GPR31)) return ox;
   }

   if (o == GOF(LR)  && sz == 8) return o;
   if (o == GOF(CTR) && sz == 8) return o;

   if (o == GOF(CIA)       && sz == 8) return -1;
   if (o == GOF(IP_AT_SYSCALL) && sz == 8) return -1; /* slot unused */
   if (o == GOF(FPROUND)   && sz == 4) return -1;
   if (o == GOF(EMWARN)    && sz == 4) return -1;
   if (o == GOF(TISTART)   && sz == 8) return -1;
   if (o == GOF(TILEN)     && sz == 8) return -1;
   if (o == GOF(VSCR)      && sz == 4) return -1;
   if (o == GOF(VRSAVE)    && sz == 4) return -1;
   if (o == GOF(REDIR_SP)  && sz == 8) return -1;

   tl_assert(SZB(FPR0) == 8);
   if (o == GOF(FPR0) && sz == 8) return o;
   if (o == GOF(FPR1) && sz == 8) return o;
   if (o == GOF(FPR2) && sz == 8) return o;
   if (o == GOF(FPR3) && sz == 8) return o;
   if (o == GOF(FPR4) && sz == 8) return o;
   if (o == GOF(FPR5) && sz == 8) return o;
   if (o == GOF(FPR6) && sz == 8) return o;
   if (o == GOF(FPR7) && sz == 8) return o;
   if (o == GOF(FPR8) && sz == 8) return o;
   if (o == GOF(FPR9) && sz == 8) return o;
   if (o == GOF(FPR10) && sz == 8) return o;
   if (o == GOF(FPR11) && sz == 8) return o;
   if (o == GOF(FPR12) && sz == 8) return o;
   if (o == GOF(FPR13) && sz == 8) return o;
   if (o == GOF(FPR14) && sz == 8) return o;
   if (o == GOF(FPR15) && sz == 8) return o;
   if (o == GOF(FPR16) && sz == 8) return o;
   if (o == GOF(FPR17) && sz == 8) return o;
   if (o == GOF(FPR18) && sz == 8) return o;
   if (o == GOF(FPR19) && sz == 8) return o;
   if (o == GOF(FPR20) && sz == 8) return o;
   if (o == GOF(FPR21) && sz == 8) return o;
   if (o == GOF(FPR22) && sz == 8) return o;
   if (o == GOF(FPR23) && sz == 8) return o;
   if (o == GOF(FPR24) && sz == 8) return o;
   if (o == GOF(FPR25) && sz == 8) return o;
   if (o == GOF(FPR26) && sz == 8) return o;
   if (o == GOF(FPR27) && sz == 8) return o;
   if (o == GOF(FPR28) && sz == 8) return o;
   if (o == GOF(FPR29) && sz == 8) return o;
   if (o == GOF(FPR30) && sz == 8) return o;
   if (o == GOF(FPR31) && sz == 8) return o;

   /* For the various byte sized XER/CR pieces, use offset 8
      in VR0 .. VR31. */
   tl_assert(SZB(VR0) == 16);
   if (o == GOF(XER_SO) && sz == 1) return 8 +GOF(VR0);
   if (o == GOF(XER_OV) && sz == 1) return 8 +GOF(VR1);
   if (o == GOF(XER_CA) && sz == 1) return 8 +GOF(VR2);
   if (o == GOF(XER_BC) && sz == 1) return 8 +GOF(VR3);

   if (o == GOF(CR0_321) && sz == 1) return 8 +GOF(VR4);
   if (o == GOF(CR0_0)   && sz == 1) return 8 +GOF(VR5);
   if (o == GOF(CR1_321) && sz == 1) return 8 +GOF(VR6);
   if (o == GOF(CR1_0)   && sz == 1) return 8 +GOF(VR7);
   if (o == GOF(CR2_321) && sz == 1) return 8 +GOF(VR8);
   if (o == GOF(CR2_0)   && sz == 1) return 8 +GOF(VR9);
   if (o == GOF(CR3_321) && sz == 1) return 8 +GOF(VR10);
   if (o == GOF(CR3_0)   && sz == 1) return 8 +GOF(VR11);
   if (o == GOF(CR4_321) && sz == 1) return 8 +GOF(VR12);
   if (o == GOF(CR4_0)   && sz == 1) return 8 +GOF(VR13);
   if (o == GOF(CR5_321) && sz == 1) return 8 +GOF(VR14);
   if (o == GOF(CR5_0)   && sz == 1) return 8 +GOF(VR15);
   if (o == GOF(CR6_321) && sz == 1) return 8 +GOF(VR16);
   if (o == GOF(CR6_0)   && sz == 1) return 8 +GOF(VR17);
   if (o == GOF(CR7_321) && sz == 1) return 8 +GOF(VR18);
   if (o == GOF(CR7_0)   && sz == 1) return 8 +GOF(VR19);

   /* Vector registers .. use offset 0 in VR0 .. VR31. */
   if (o >= GOF(VR0)  && o+sz <= GOF(VR0) +SZB(VR0))  return 0+ GOF(VR0);
   if (o >= GOF(VR1)  && o+sz <= GOF(VR1) +SZB(VR1))  return 0+ GOF(VR1);
   if (o >= GOF(VR2)  && o+sz <= GOF(VR2) +SZB(VR2))  return 0+ GOF(VR2);
   if (o >= GOF(VR3)  && o+sz <= GOF(VR3) +SZB(VR3))  return 0+ GOF(VR3);
   if (o >= GOF(VR4)  && o+sz <= GOF(VR4) +SZB(VR4))  return 0+ GOF(VR4);
   if (o >= GOF(VR5)  && o+sz <= GOF(VR5) +SZB(VR5))  return 0+ GOF(VR5);
   if (o >= GOF(VR6)  && o+sz <= GOF(VR6) +SZB(VR6))  return 0+ GOF(VR6);
   if (o >= GOF(VR7)  && o+sz <= GOF(VR7) +SZB(VR7))  return 0+ GOF(VR7);
   if (o >= GOF(VR8)  && o+sz <= GOF(VR8) +SZB(VR8))  return 0+ GOF(VR8);
   if (o >= GOF(VR9)  && o+sz <= GOF(VR9) +SZB(VR9))  return 0+ GOF(VR9);
   if (o >= GOF(VR10) && o+sz <= GOF(VR10)+SZB(VR10)) return 0+ GOF(VR10);
   if (o >= GOF(VR11) && o+sz <= GOF(VR11)+SZB(VR11)) return 0+ GOF(VR11);
   if (o >= GOF(VR12) && o+sz <= GOF(VR12)+SZB(VR12)) return 0+ GOF(VR12);
   if (o >= GOF(VR13) && o+sz <= GOF(VR13)+SZB(VR13)) return 0+ GOF(VR13);
   if (o >= GOF(VR14) && o+sz <= GOF(VR14)+SZB(VR14)) return 0+ GOF(VR14);
   if (o >= GOF(VR15) && o+sz <= GOF(VR15)+SZB(VR15)) return 0+ GOF(VR15);
   if (o >= GOF(VR16) && o+sz <= GOF(VR16)+SZB(VR16)) return 0+ GOF(VR16);
   if (o >= GOF(VR17) && o+sz <= GOF(VR17)+SZB(VR17)) return 0+ GOF(VR17);
   if (o >= GOF(VR18) && o+sz <= GOF(VR18)+SZB(VR18)) return 0+ GOF(VR18);
   if (o >= GOF(VR19) && o+sz <= GOF(VR19)+SZB(VR19)) return 0+ GOF(VR19);
   if (o >= GOF(VR20) && o+sz <= GOF(VR20)+SZB(VR20)) return 0+ GOF(VR20);
   if (o >= GOF(VR21) && o+sz <= GOF(VR21)+SZB(VR21)) return 0+ GOF(VR21);
   if (o >= GOF(VR22) && o+sz <= GOF(VR22)+SZB(VR22)) return 0+ GOF(VR22);
   if (o >= GOF(VR23) && o+sz <= GOF(VR23)+SZB(VR23)) return 0+ GOF(VR23);
   if (o >= GOF(VR24) && o+sz <= GOF(VR24)+SZB(VR24)) return 0+ GOF(VR24);
   if (o >= GOF(VR25) && o+sz <= GOF(VR25)+SZB(VR25)) return 0+ GOF(VR25);
   if (o >= GOF(VR26) && o+sz <= GOF(VR26)+SZB(VR26)) return 0+ GOF(VR26);
   if (o >= GOF(VR27) && o+sz <= GOF(VR27)+SZB(VR27)) return 0+ GOF(VR27);
   if (o >= GOF(VR28) && o+sz <= GOF(VR28)+SZB(VR28)) return 0+ GOF(VR28);
   if (o >= GOF(VR29) && o+sz <= GOF(VR29)+SZB(VR29)) return 0+ GOF(VR29);
   if (o >= GOF(VR30) && o+sz <= GOF(VR30)+SZB(VR30)) return 0+ GOF(VR30);
   if (o >= GOF(VR31) && o+sz <= GOF(VR31)+SZB(VR31)) return 0+ GOF(VR31);

   VG_(printf)("MC_(get_otrack_shadow_offset)(ppc64)(off=%d,sz=%d)\n",
               offset,szB);
   tl_assert(0);
#  undef GOF
#  undef SZB

   /* -------------------- ppc32 -------------------- */

#  elif defined(VGA_ppc32)

#  define GOF(_fieldname) \
      (offsetof(VexGuestPPC32State,guest_##_fieldname))
#  define SZB(_fieldname) \
      (sizeof(((VexGuestPPC32State*)0)->guest_##_fieldname))
   Int  o  = offset;
   Int  sz = szB;
   tl_assert(sz > 0);
   tl_assert(host_is_big_endian());

   if (o == GOF(GPR0) && sz == 4) return o;
   if (o == GOF(GPR1) && sz == 4) return o;
   if (o == GOF(GPR2) && sz == 4) return o;
   if (o == GOF(GPR3) && sz == 4) return o;
   if (o == GOF(GPR4) && sz == 4) return o;
   if (o == GOF(GPR5) && sz == 4) return o;
   if (o == GOF(GPR6) && sz == 4) return o;
   if (o == GOF(GPR7) && sz == 4) return o;
   if (o == GOF(GPR8) && sz == 4) return o;
   if (o == GOF(GPR9) && sz == 4) return o;
   if (o == GOF(GPR10) && sz == 4) return o;
   if (o == GOF(GPR11) && sz == 4) return o;
   if (o == GOF(GPR12) && sz == 4) return o;
   if (o == GOF(GPR13) && sz == 4) return o;
   if (o == GOF(GPR14) && sz == 4) return o;
   if (o == GOF(GPR15) && sz == 4) return o;
   if (o == GOF(GPR16) && sz == 4) return o;
   if (o == GOF(GPR17) && sz == 4) return o;
   if (o == GOF(GPR18) && sz == 4) return o;
   if (o == GOF(GPR19) && sz == 4) return o;
   if (o == GOF(GPR20) && sz == 4) return o;
   if (o == GOF(GPR21) && sz == 4) return o;
   if (o == GOF(GPR22) && sz == 4) return o;
   if (o == GOF(GPR23) && sz == 4) return o;
   if (o == GOF(GPR24) && sz == 4) return o;
   if (o == GOF(GPR25) && sz == 4) return o;
   if (o == GOF(GPR26) && sz == 4) return o;
   if (o == GOF(GPR27) && sz == 4) return o;
   if (o == GOF(GPR28) && sz == 4) return o;
   if (o == GOF(GPR29) && sz == 4) return o;
   if (o == GOF(GPR30) && sz == 4) return o;
   if (o == GOF(GPR31) && sz == 4) return o;

   if (o == GOF(LR)  && sz == 4) return o;
   if (o == GOF(CTR) && sz == 4) return o;

   if (o == GOF(CIA)       && sz == 4) return -1;
   if (o == GOF(IP_AT_SYSCALL) && sz == 4) return -1; /* slot unused */
   if (o == GOF(FPROUND)   && sz == 4) return -1;
   if (o == GOF(VRSAVE)    && sz == 4) return -1;
   if (o == GOF(EMWARN)    && sz == 4) return -1;
   if (o == GOF(TISTART)   && sz == 4) return -1;
   if (o == GOF(TILEN)     && sz == 4) return -1;
   if (o == GOF(VSCR)      && sz == 4) return -1;
   if (o == GOF(REDIR_SP)  && sz == 4) return -1;
   if (o == GOF(SPRG3_RO)  && sz == 4) return -1;

   tl_assert(SZB(FPR0) == 8);
   if (o == GOF(FPR0) && sz == 8) return o;
   if (o == GOF(FPR1) && sz == 8) return o;
   if (o == GOF(FPR2) && sz == 8) return o;
   if (o == GOF(FPR3) && sz == 8) return o;
   if (o == GOF(FPR4) && sz == 8) return o;
   if (o == GOF(FPR5) && sz == 8) return o;
   if (o == GOF(FPR6) && sz == 8) return o;
   if (o == GOF(FPR7) && sz == 8) return o;
   if (o == GOF(FPR8) && sz == 8) return o;
   if (o == GOF(FPR9) && sz == 8) return o;
   if (o == GOF(FPR10) && sz == 8) return o;
   if (o == GOF(FPR11) && sz == 8) return o;
   if (o == GOF(FPR12) && sz == 8) return o;
   if (o == GOF(FPR13) && sz == 8) return o;
   if (o == GOF(FPR14) && sz == 8) return o;
   if (o == GOF(FPR15) && sz == 8) return o;
   if (o == GOF(FPR16) && sz == 8) return o;
   if (o == GOF(FPR17) && sz == 8) return o;
   if (o == GOF(FPR18) && sz == 8) return o;
   if (o == GOF(FPR19) && sz == 8) return o;
   if (o == GOF(FPR20) && sz == 8) return o;
   if (o == GOF(FPR21) && sz == 8) return o;
   if (o == GOF(FPR22) && sz == 8) return o;
   if (o == GOF(FPR23) && sz == 8) return o;
   if (o == GOF(FPR24) && sz == 8) return o;
   if (o == GOF(FPR25) && sz == 8) return o;
   if (o == GOF(FPR26) && sz == 8) return o;
   if (o == GOF(FPR27) && sz == 8) return o;
   if (o == GOF(FPR28) && sz == 8) return o;
   if (o == GOF(FPR29) && sz == 8) return o;
   if (o == GOF(FPR30) && sz == 8) return o;
   if (o == GOF(FPR31) && sz == 8) return o;

   /* For the various byte sized XER/CR pieces, use offset 8
      in VR0 .. VR31. */
   tl_assert(SZB(VR0) == 16);
   if (o == GOF(XER_SO) && sz == 1) return 8 +GOF(VR0);
   if (o == GOF(XER_OV) && sz == 1) return 8 +GOF(VR1);
   if (o == GOF(XER_CA) && sz == 1) return 8 +GOF(VR2);
   if (o == GOF(XER_BC) && sz == 1) return 8 +GOF(VR3);

   if (o == GOF(CR0_321) && sz == 1) return 8 +GOF(VR4);
   if (o == GOF(CR0_0)   && sz == 1) return 8 +GOF(VR5);
   if (o == GOF(CR1_321) && sz == 1) return 8 +GOF(VR6);
   if (o == GOF(CR1_0)   && sz == 1) return 8 +GOF(VR7);
   if (o == GOF(CR2_321) && sz == 1) return 8 +GOF(VR8);
   if (o == GOF(CR2_0)   && sz == 1) return 8 +GOF(VR9);
   if (o == GOF(CR3_321) && sz == 1) return 8 +GOF(VR10);
   if (o == GOF(CR3_0)   && sz == 1) return 8 +GOF(VR11);
   if (o == GOF(CR4_321) && sz == 1) return 8 +GOF(VR12);
   if (o == GOF(CR4_0)   && sz == 1) return 8 +GOF(VR13);
   if (o == GOF(CR5_321) && sz == 1) return 8 +GOF(VR14);
   if (o == GOF(CR5_0)   && sz == 1) return 8 +GOF(VR15);
   if (o == GOF(CR6_321) && sz == 1) return 8 +GOF(VR16);
   if (o == GOF(CR6_0)   && sz == 1) return 8 +GOF(VR17);
   if (o == GOF(CR7_321) && sz == 1) return 8 +GOF(VR18);
   if (o == GOF(CR7_0)   && sz == 1) return 8 +GOF(VR19);

   /* Vector registers .. use offset 0 in VR0 .. VR31. */
   if (o >= GOF(VR0)  && o+sz <= GOF(VR0) +SZB(VR0))  return 0+ GOF(VR0);
   if (o >= GOF(VR1)  && o+sz <= GOF(VR1) +SZB(VR1))  return 0+ GOF(VR1);
   if (o >= GOF(VR2)  && o+sz <= GOF(VR2) +SZB(VR2))  return 0+ GOF(VR2);
   if (o >= GOF(VR3)  && o+sz <= GOF(VR3) +SZB(VR3))  return 0+ GOF(VR3);
   if (o >= GOF(VR4)  && o+sz <= GOF(VR4) +SZB(VR4))  return 0+ GOF(VR4);
   if (o >= GOF(VR5)  && o+sz <= GOF(VR5) +SZB(VR5))  return 0+ GOF(VR5);
   if (o >= GOF(VR6)  && o+sz <= GOF(VR6) +SZB(VR6))  return 0+ GOF(VR6);
   if (o >= GOF(VR7)  && o+sz <= GOF(VR7) +SZB(VR7))  return 0+ GOF(VR7);
   if (o >= GOF(VR8)  && o+sz <= GOF(VR8) +SZB(VR8))  return 0+ GOF(VR8);
   if (o >= GOF(VR9)  && o+sz <= GOF(VR9) +SZB(VR9))  return 0+ GOF(VR9);
   if (o >= GOF(VR10) && o+sz <= GOF(VR10)+SZB(VR10)) return 0+ GOF(VR10);
   if (o >= GOF(VR11) && o+sz <= GOF(VR11)+SZB(VR11)) return 0+ GOF(VR11);
   if (o >= GOF(VR12) && o+sz <= GOF(VR12)+SZB(VR12)) return 0+ GOF(VR12);
   if (o >= GOF(VR13) && o+sz <= GOF(VR13)+SZB(VR13)) return 0+ GOF(VR13);
   if (o >= GOF(VR14) && o+sz <= GOF(VR14)+SZB(VR14)) return 0+ GOF(VR14);
   if (o >= GOF(VR15) && o+sz <= GOF(VR15)+SZB(VR15)) return 0+ GOF(VR15);
   if (o >= GOF(VR16) && o+sz <= GOF(VR16)+SZB(VR16)) return 0+ GOF(VR16);
   if (o >= GOF(VR17) && o+sz <= GOF(VR17)+SZB(VR17)) return 0+ GOF(VR17);
   if (o >= GOF(VR18) && o+sz <= GOF(VR18)+SZB(VR18)) return 0+ GOF(VR18);
   if (o >= GOF(VR19) && o+sz <= GOF(VR19)+SZB(VR19)) return 0+ GOF(VR19);
   if (o >= GOF(VR20) && o+sz <= GOF(VR20)+SZB(VR20)) return 0+ GOF(VR20);
   if (o >= GOF(VR21) && o+sz <= GOF(VR21)+SZB(VR21)) return 0+ GOF(VR21);
   if (o >= GOF(VR22) && o+sz <= GOF(VR22)+SZB(VR22)) return 0+ GOF(VR22);
   if (o >= GOF(VR23) && o+sz <= GOF(VR23)+SZB(VR23)) return 0+ GOF(VR23);
   if (o >= GOF(VR24) && o+sz <= GOF(VR24)+SZB(VR24)) return 0+ GOF(VR24);
   if (o >= GOF(VR25) && o+sz <= GOF(VR25)+SZB(VR25)) return 0+ GOF(VR25);
   if (o >= GOF(VR26) && o+sz <= GOF(VR26)+SZB(VR26)) return 0+ GOF(VR26);
   if (o >= GOF(VR27) && o+sz <= GOF(VR27)+SZB(VR27)) return 0+ GOF(VR27);
   if (o >= GOF(VR28) && o+sz <= GOF(VR28)+SZB(VR28)) return 0+ GOF(VR28);
   if (o >= GOF(VR29) && o+sz <= GOF(VR29)+SZB(VR29)) return 0+ GOF(VR29);
   if (o >= GOF(VR30) && o+sz <= GOF(VR30)+SZB(VR30)) return 0+ GOF(VR30);
   if (o >= GOF(VR31) && o+sz <= GOF(VR31)+SZB(VR31)) return 0+ GOF(VR31);

   VG_(printf)("MC_(get_otrack_shadow_offset)(ppc32)(off=%d,sz=%d)\n",
               offset,szB);
   tl_assert(0);
#  undef GOF
#  undef SZB

   /* -------------------- amd64 -------------------- */

#  elif defined(VGA_amd64)

#  define GOF(_fieldname) \
      (offsetof(VexGuestAMD64State,guest_##_fieldname))
#  define SZB(_fieldname) \
      (sizeof(((VexGuestAMD64State*)0)->guest_##_fieldname))
   Int  o      = offset;
   Int  sz     = szB;
   Bool is1248 = sz == 8 || sz == 4 || sz == 2 || sz == 1;
   tl_assert(sz > 0);
   tl_assert(host_is_little_endian());

   if (o == GOF(RAX) && is1248) return o;
   if (o == GOF(RCX) && is1248) return o;
   if (o == GOF(RDX) && is1248) return o;
   if (o == GOF(RBX) && is1248) return o;
   if (o == GOF(RSP) && is1248) return o;
   if (o == GOF(RBP) && is1248) return o;
   if (o == GOF(RSI) && is1248) return o;
   if (o == GOF(RDI) && is1248) return o;
   if (o == GOF(R8)  && is1248) return o;
   if (o == GOF(R9)  && is1248) return o;
   if (o == GOF(R10) && is1248) return o;
   if (o == GOF(R11) && is1248) return o;
   if (o == GOF(R12) && is1248) return o;
   if (o == GOF(R13) && is1248) return o;
   if (o == GOF(R14) && is1248) return o;
   if (o == GOF(R15) && is1248) return o;

   if (o == GOF(CC_DEP1) && sz == 8) return o;
   if (o == GOF(CC_DEP2) && sz == 8) return o;

   if (o == GOF(CC_OP)   && sz == 8) return -1; /* slot used for %AH */
   if (o == GOF(CC_NDEP) && sz == 8) return -1; /* slot used for %BH */
   if (o == GOF(DFLAG)   && sz == 8) return -1; /* slot used for %CH */
   if (o == GOF(RIP)     && sz == 8) return -1; /* slot unused */
   if (o == GOF(IP_AT_SYSCALL) && sz == 8) return -1; /* slot unused */
   if (o == GOF(IDFLAG)  && sz == 8) return -1; /* slot used for %DH */
   if (o == GOF(ACFLAG)  && sz == 8) return -1; /* slot unused */
   if (o == GOF(FS_ZERO) && sz == 8) return -1; /* slot unused */
   if (o == GOF(GS_0x60) && sz == 8) return -1; /* slot unused */
   if (o == GOF(TISTART) && sz == 8) return -1; /* slot unused */
   if (o == GOF(TILEN)   && sz == 8) return -1; /* slot unused */

   /* Treat %AH, %BH, %CH, %DH as independent registers.  To do this
      requires finding 4 unused 32-bit slots in the second-shadow
      guest state, respectively: CC_OP CC_NDEP DFLAG IDFLAG, since
      none of those are tracked. */
   tl_assert(SZB(CC_OP)   == 8);
   tl_assert(SZB(CC_NDEP) == 8);
   tl_assert(SZB(IDFLAG)  == 8);
   tl_assert(SZB(DFLAG)   == 8);

   if (o == 1+ GOF(RAX) && szB == 1) return GOF(CC_OP);
   if (o == 1+ GOF(RBX) && szB == 1) return GOF(CC_NDEP);
   if (o == 1+ GOF(RCX) && szB == 1) return GOF(DFLAG);
   if (o == 1+ GOF(RDX) && szB == 1) return GOF(IDFLAG);

   /* skip XMM and FP admin stuff */
   if (o == GOF(SSEROUND) && szB == 8) return -1;
   if (o == GOF(FTOP)     && szB == 4) return -1;
   if (o == GOF(FPROUND)  && szB == 8) return -1;
   if (o == GOF(EMWARN)   && szB == 4) return -1;
   if (o == GOF(FC3210)   && szB == 8) return -1;

   /* XMM registers */
   if (o >= GOF(XMM0)  && o+sz <= GOF(XMM0) +SZB(XMM0))  return GOF(XMM0);
   if (o >= GOF(XMM1)  && o+sz <= GOF(XMM1) +SZB(XMM1))  return GOF(XMM1);
   if (o >= GOF(XMM2)  && o+sz <= GOF(XMM2) +SZB(XMM2))  return GOF(XMM2);
   if (o >= GOF(XMM3)  && o+sz <= GOF(XMM3) +SZB(XMM3))  return GOF(XMM3);
   if (o >= GOF(XMM4)  && o+sz <= GOF(XMM4) +SZB(XMM4))  return GOF(XMM4);
   if (o >= GOF(XMM5)  && o+sz <= GOF(XMM5) +SZB(XMM5))  return GOF(XMM5);
   if (o >= GOF(XMM6)  && o+sz <= GOF(XMM6) +SZB(XMM6))  return GOF(XMM6);
   if (o >= GOF(XMM7)  && o+sz <= GOF(XMM7) +SZB(XMM7))  return GOF(XMM7);
   if (o >= GOF(XMM8)  && o+sz <= GOF(XMM8) +SZB(XMM8))  return GOF(XMM8);
   if (o >= GOF(XMM9)  && o+sz <= GOF(XMM9) +SZB(XMM9))  return GOF(XMM9);
   if (o >= GOF(XMM10) && o+sz <= GOF(XMM10)+SZB(XMM10)) return GOF(XMM10);
   if (o >= GOF(XMM11) && o+sz <= GOF(XMM11)+SZB(XMM11)) return GOF(XMM11);
   if (o >= GOF(XMM12) && o+sz <= GOF(XMM12)+SZB(XMM12)) return GOF(XMM12);
   if (o >= GOF(XMM13) && o+sz <= GOF(XMM13)+SZB(XMM13)) return GOF(XMM13);
   if (o >= GOF(XMM14) && o+sz <= GOF(XMM14)+SZB(XMM14)) return GOF(XMM14);
   if (o >= GOF(XMM15) && o+sz <= GOF(XMM15)+SZB(XMM15)) return GOF(XMM15);
   if (o >= GOF(XMM16) && o+sz <= GOF(XMM16)+SZB(XMM16)) return GOF(XMM16);

   /* MMX accesses to FP regs.  Need to allow for 32-bit references
      due to dirty helpers for frstor etc, which reference the entire
      64-byte block in one go. */
   if (o >= GOF(FPREG[0])
       && o+sz <= GOF(FPREG[0])+SZB(FPREG[0])) return GOF(FPREG[0]);
   if (o >= GOF(FPREG[1])
       && o+sz <= GOF(FPREG[1])+SZB(FPREG[1])) return GOF(FPREG[1]);
   if (o >= GOF(FPREG[2])
       && o+sz <= GOF(FPREG[2])+SZB(FPREG[2])) return GOF(FPREG[2]);
   if (o >= GOF(FPREG[3])
       && o+sz <= GOF(FPREG[3])+SZB(FPREG[3])) return GOF(FPREG[3]);
   if (o >= GOF(FPREG[4])
       && o+sz <= GOF(FPREG[4])+SZB(FPREG[4])) return GOF(FPREG[4]);
   if (o >= GOF(FPREG[5])
       && o+sz <= GOF(FPREG[5])+SZB(FPREG[5])) return GOF(FPREG[5]);
   if (o >= GOF(FPREG[6])
       && o+sz <= GOF(FPREG[6])+SZB(FPREG[6])) return GOF(FPREG[6]);
   if (o >= GOF(FPREG[7])
       && o+sz <= GOF(FPREG[7])+SZB(FPREG[7])) return GOF(FPREG[7]);

   /* Map high halves of %RAX,%RCX,%RDX,%RBX to the whole register.
      This is needed because the general handling of dirty helper
      calls is done in 4 byte chunks.  Hence we will see these.
      Currently we only expect to see artefacts from CPUID. */
   if (o == 4+ GOF(RAX) && sz == 4) return GOF(RAX);
   if (o == 4+ GOF(RCX) && sz == 4) return GOF(RCX);
   if (o == 4+ GOF(RDX) && sz == 4) return GOF(RDX);
   if (o == 4+ GOF(RBX) && sz == 4) return GOF(RBX);

   VG_(printf)("MC_(get_otrack_shadow_offset)(amd64)(off=%d,sz=%d)\n",
               offset,szB);
   tl_assert(0);
#  undef GOF
#  undef SZB

   /* --------------------- x86 --------------------- */

#  elif defined(VGA_x86)

#  define GOF(_fieldname) \
      (offsetof(VexGuestX86State,guest_##_fieldname))
#  define SZB(_fieldname) \
      (sizeof(((VexGuestX86State*)0)->guest_##_fieldname))

   Int  o     = offset;
   Int  sz    = szB;
   Bool is124 = sz == 4 || sz == 2 || sz == 1;
   tl_assert(sz > 0);
   tl_assert(host_is_little_endian());

   if (o == GOF(EAX) && is124) return o;
   if (o == GOF(ECX) && is124) return o;
   if (o == GOF(EDX) && is124) return o;
   if (o == GOF(EBX) && is124) return o;
   if (o == GOF(ESP) && is124) return o;
   if (o == GOF(EBP) && is124) return o;
   if (o == GOF(ESI) && is124) return o;
   if (o == GOF(EDI) && is124) return o;

   if (o == GOF(CC_DEP1) && sz == 4) return o;
   if (o == GOF(CC_DEP2) && sz == 4) return o;

   if (o == GOF(CC_OP)   && sz == 4) return -1; /* slot used for %AH */
   if (o == GOF(CC_NDEP) && sz == 4) return -1; /* slot used for %BH */
   if (o == GOF(DFLAG)   && sz == 4) return -1; /* slot used for %CH */
   if (o == GOF(EIP)     && sz == 4) return -1; /* slot unused */
   if (o == GOF(IP_AT_SYSCALL) && sz == 4) return -1; /* slot unused */
   if (o == GOF(IDFLAG)  && sz == 4) return -1; /* slot used for %DH */
   if (o == GOF(ACFLAG)  && sz == 4) return -1; /* slot unused */
   if (o == GOF(TISTART) && sz == 4) return -1; /* slot unused */
   if (o == GOF(TILEN)   && sz == 4) return -1; /* slot unused */
   if (o == GOF(NRADDR)  && sz == 4) return -1; /* slot unused */

   /* Treat %AH, %BH, %CH, %DH as independent registers.  To do this
      requires finding 4 unused 32-bit slots in the second-shadow
      guest state, respectively: CC_OP CC_NDEP DFLAG IDFLAG since none
      of those are tracked. */
   tl_assert(SZB(CC_OP)   == 4);
   tl_assert(SZB(CC_NDEP) == 4);
   tl_assert(SZB(DFLAG)   == 4);
   tl_assert(SZB(IDFLAG)  == 4);
   if (o == 1+ GOF(EAX) && szB == 1) return GOF(CC_OP);
   if (o == 1+ GOF(EBX) && szB == 1) return GOF(CC_NDEP);
   if (o == 1+ GOF(ECX) && szB == 1) return GOF(DFLAG);
   if (o == 1+ GOF(EDX) && szB == 1) return GOF(IDFLAG);

   /* skip XMM and FP admin stuff */
   if (o == GOF(SSEROUND) && szB == 4) return -1;
   if (o == GOF(FTOP)     && szB == 4) return -1;
   if (o == GOF(FPROUND)  && szB == 4) return -1;
   if (o == GOF(EMWARN)   && szB == 4) return -1;
   if (o == GOF(FC3210)   && szB == 4) return -1;

   /* XMM registers */
   if (o >= GOF(XMM0)  && o+sz <= GOF(XMM0)+SZB(XMM0)) return GOF(XMM0);
   if (o >= GOF(XMM1)  && o+sz <= GOF(XMM1)+SZB(XMM1)) return GOF(XMM1);
   if (o >= GOF(XMM2)  && o+sz <= GOF(XMM2)+SZB(XMM2)) return GOF(XMM2);
   if (o >= GOF(XMM3)  && o+sz <= GOF(XMM3)+SZB(XMM3)) return GOF(XMM3);
   if (o >= GOF(XMM4)  && o+sz <= GOF(XMM4)+SZB(XMM4)) return GOF(XMM4);
   if (o >= GOF(XMM5)  && o+sz <= GOF(XMM5)+SZB(XMM5)) return GOF(XMM5);
   if (o >= GOF(XMM6)  && o+sz <= GOF(XMM6)+SZB(XMM6)) return GOF(XMM6);
   if (o >= GOF(XMM7)  && o+sz <= GOF(XMM7)+SZB(XMM7)) return GOF(XMM7);

   /* MMX accesses to FP regs.  Need to allow for 32-bit references
      due to dirty helpers for frstor etc, which reference the entire
      64-byte block in one go. */
   if (o >= GOF(FPREG[0])
       && o+sz <= GOF(FPREG[0])+SZB(FPREG[0])) return GOF(FPREG[0]);
   if (o >= GOF(FPREG[1])
       && o+sz <= GOF(FPREG[1])+SZB(FPREG[1])) return GOF(FPREG[1]);
   if (o >= GOF(FPREG[2])
       && o+sz <= GOF(FPREG[2])+SZB(FPREG[2])) return GOF(FPREG[2]);
   if (o >= GOF(FPREG[3])
       && o+sz <= GOF(FPREG[3])+SZB(FPREG[3])) return GOF(FPREG[3]);
   if (o >= GOF(FPREG[4])
       && o+sz <= GOF(FPREG[4])+SZB(FPREG[4])) return GOF(FPREG[4]);
   if (o >= GOF(FPREG[5])
       && o+sz <= GOF(FPREG[5])+SZB(FPREG[5])) return GOF(FPREG[5]);
   if (o >= GOF(FPREG[6])
       && o+sz <= GOF(FPREG[6])+SZB(FPREG[6])) return GOF(FPREG[6]);
   if (o >= GOF(FPREG[7])
       && o+sz <= GOF(FPREG[7])+SZB(FPREG[7])) return GOF(FPREG[7]);

   /* skip %GS and other segment related stuff.  We could shadow
      guest_LDT and guest_GDT, although it seems pointless.
      guest_CS .. guest_SS are too small to shadow directly and it
      also seems pointless to shadow them indirectly (that is, in 
      the style of %AH .. %DH). */
   if (o == GOF(CS) && sz == 2) return -1;
   if (o == GOF(DS) && sz == 2) return -1;
   if (o == GOF(ES) && sz == 2) return -1;
   if (o == GOF(FS) && sz == 2) return -1;
   if (o == GOF(GS) && sz == 2) return -1;
   if (o == GOF(SS) && sz == 2) return -1;
   if (o == GOF(LDT) && sz == 4) return -1;
   if (o == GOF(GDT) && sz == 4) return -1;

   VG_(printf)("MC_(get_otrack_shadow_offset)(x86)(off=%d,sz=%d)\n",
               offset,szB);
   tl_assert(0);
#  undef GOF
#  undef SZB

   /* --------------------- arm --------------------- */

#  elif defined(VGA_arm)

#  define GOF(_fieldname) \
      (offsetof(VexGuestARMState,guest_##_fieldname))
#  define SZB(_fieldname) \
      (sizeof(((VexGuestARMState*)0)->guest_##_fieldname))

   Int  o     = offset;
   Int  sz    = szB;
   tl_assert(sz > 0);
   tl_assert(host_is_little_endian());

   if (o == GOF(R0)  && sz == 4) return o;
   if (o == GOF(R1)  && sz == 4) return o;
   if (o == GOF(R2)  && sz == 4) return o;
   if (o == GOF(R3)  && sz == 4) return o;
   if (o == GOF(R4)  && sz == 4) return o;
   if (o == GOF(R5)  && sz == 4) return o;
   if (o == GOF(R6)  && sz == 4) return o;
   if (o == GOF(R7)  && sz == 4) return o;
   if (o == GOF(R8)  && sz == 4) return o;
   if (o == GOF(R9)  && sz == 4) return o;
   if (o == GOF(R10) && sz == 4) return o;
   if (o == GOF(R11) && sz == 4) return o;
   if (o == GOF(R12) && sz == 4) return o;
   if (o == GOF(R13) && sz == 4) return o;
   if (o == GOF(R14) && sz == 4) return o;

   /* EAZG: These may be completely wrong. */
   if (o == GOF(R15T)  && sz == 4) return -1; /* slot unused */
   if (o == GOF(CC_OP) && sz == 4) return -1; /* slot unused */

   if (o == GOF(CC_DEP1) && sz == 4) return o;
   if (o == GOF(CC_DEP2) && sz == 4) return o;

   if (o == GOF(CC_NDEP) && sz == 4) return -1; /* slot unused */

   if (o == GOF(QFLAG32) && sz == 4) return o;

   if (o == GOF(GEFLAG0) && sz == 4) return o;
   if (o == GOF(GEFLAG1) && sz == 4) return o;
   if (o == GOF(GEFLAG2) && sz == 4) return o;
   if (o == GOF(GEFLAG3) && sz == 4) return o;

   //if (o == GOF(SYSCALLNO)     && sz == 4) return -1; /* slot unused */
   //if (o == GOF(CC)     && sz == 4) return -1; /* slot unused */
   //if (o == GOF(EMWARN)     && sz == 4) return -1; /* slot unused */
   //if (o == GOF(TISTART)     && sz == 4) return -1; /* slot unused */
   //if (o == GOF(NRADDR)     && sz == 4) return -1; /* slot unused */

   if (o == GOF(FPSCR)    && sz == 4) return -1;
   if (o == GOF(TPIDRURO) && sz == 4) return -1;
   if (o == GOF(ITSTATE)  && sz == 4) return -1;

   /* Accesses to F or D registers */
   if (sz == 4 || sz == 8) {
      if (o >= GOF(D0)  && o+sz <= GOF(D0) +SZB(D0))  return GOF(D0);
      if (o >= GOF(D1)  && o+sz <= GOF(D1) +SZB(D1))  return GOF(D1);
      if (o >= GOF(D2)  && o+sz <= GOF(D2) +SZB(D2))  return GOF(D2);
      if (o >= GOF(D3)  && o+sz <= GOF(D3) +SZB(D3))  return GOF(D3);
      if (o >= GOF(D4)  && o+sz <= GOF(D4) +SZB(D4))  return GOF(D4);
      if (o >= GOF(D5)  && o+sz <= GOF(D5) +SZB(D5))  return GOF(D5);
      if (o >= GOF(D6)  && o+sz <= GOF(D6) +SZB(D6))  return GOF(D6);
      if (o >= GOF(D7)  && o+sz <= GOF(D7) +SZB(D7))  return GOF(D7);
      if (o >= GOF(D8)  && o+sz <= GOF(D8) +SZB(D8))  return GOF(D8);
      if (o >= GOF(D9)  && o+sz <= GOF(D9) +SZB(D9))  return GOF(D9);
      if (o >= GOF(D10) && o+sz <= GOF(D10)+SZB(D10)) return GOF(D10);
      if (o >= GOF(D11) && o+sz <= GOF(D11)+SZB(D11)) return GOF(D11);
      if (o >= GOF(D12) && o+sz <= GOF(D12)+SZB(D12)) return GOF(D12);
      if (o >= GOF(D13) && o+sz <= GOF(D13)+SZB(D13)) return GOF(D13);
      if (o >= GOF(D14) && o+sz <= GOF(D14)+SZB(D14)) return GOF(D14);
      if (o >= GOF(D15) && o+sz <= GOF(D15)+SZB(D15)) return GOF(D15);
      if (o >= GOF(D16) && o+sz <= GOF(D16)+SZB(D16)) return GOF(D16);
      if (o >= GOF(D17) && o+sz <= GOF(D17)+SZB(D17)) return GOF(D17);
      if (o >= GOF(D18) && o+sz <= GOF(D18)+SZB(D18)) return GOF(D18);
      if (o >= GOF(D19) && o+sz <= GOF(D19)+SZB(D19)) return GOF(D19);
      if (o >= GOF(D20) && o+sz <= GOF(D20)+SZB(D20)) return GOF(D20);
      if (o >= GOF(D21) && o+sz <= GOF(D21)+SZB(D21)) return GOF(D21);
      if (o >= GOF(D22) && o+sz <= GOF(D22)+SZB(D22)) return GOF(D22);
      if (o >= GOF(D23) && o+sz <= GOF(D23)+SZB(D23)) return GOF(D23);
      if (o >= GOF(D24) && o+sz <= GOF(D24)+SZB(D24)) return GOF(D24);
      if (o >= GOF(D25) && o+sz <= GOF(D25)+SZB(D25)) return GOF(D25);
      if (o >= GOF(D26) && o+sz <= GOF(D26)+SZB(D26)) return GOF(D26);
      if (o >= GOF(D27) && o+sz <= GOF(D27)+SZB(D27)) return GOF(D27);
      if (o >= GOF(D28) && o+sz <= GOF(D28)+SZB(D28)) return GOF(D28);
      if (o >= GOF(D29) && o+sz <= GOF(D29)+SZB(D29)) return GOF(D29);
      if (o >= GOF(D30) && o+sz <= GOF(D30)+SZB(D30)) return GOF(D30);
      if (o >= GOF(D31) && o+sz <= GOF(D31)+SZB(D31)) return GOF(D31);
   }

   /* Accesses to Q registers */
   if (sz == 16) {
      if (o >= GOF(D0)  && o+sz <= GOF(D0) +2*SZB(D0))  return GOF(D0);  // Q0
      if (o >= GOF(D2)  && o+sz <= GOF(D2) +2*SZB(D2))  return GOF(D2);  // Q1
      if (o >= GOF(D4)  && o+sz <= GOF(D4) +2*SZB(D4))  return GOF(D4);  // Q2
      if (o >= GOF(D6)  && o+sz <= GOF(D6) +2*SZB(D6))  return GOF(D6);  // Q3
      if (o >= GOF(D8)  && o+sz <= GOF(D8) +2*SZB(D8))  return GOF(D8);  // Q4
      if (o >= GOF(D10) && o+sz <= GOF(D10)+2*SZB(D10)) return GOF(D10); // Q5
      if (o >= GOF(D12) && o+sz <= GOF(D12)+2*SZB(D12)) return GOF(D12); // Q6
      if (o >= GOF(D14) && o+sz <= GOF(D14)+2*SZB(D14)) return GOF(D14); // Q7
      if (o >= GOF(D16) && o+sz <= GOF(D16)+2*SZB(D16)) return GOF(D16); // Q8
      if (o >= GOF(D18) && o+sz <= GOF(D18)+2*SZB(D18)) return GOF(D18); // Q9
      if (o >= GOF(D20) && o+sz <= GOF(D20)+2*SZB(D20)) return GOF(D20); // Q10
      if (o >= GOF(D22) && o+sz <= GOF(D22)+2*SZB(D22)) return GOF(D22); // Q11
      if (o >= GOF(D24) && o+sz <= GOF(D24)+2*SZB(D24)) return GOF(D24); // Q12
      if (o >= GOF(D26) && o+sz <= GOF(D26)+2*SZB(D26)) return GOF(D26); // Q13
      if (o >= GOF(D28) && o+sz <= GOF(D28)+2*SZB(D28)) return GOF(D28); // Q14
      if (o >= GOF(D30) && o+sz <= GOF(D30)+2*SZB(D30)) return GOF(D30); // Q15
   }

   VG_(printf)("MC_(get_otrack_shadow_offset)(arm)(off=%d,sz=%d)\n",
               offset,szB);
   tl_assert(0);
#  undef GOF
#  undef SZB

#  else
#    error "FIXME: not implemented for this architecture"
#  endif
}


/* Let 'arr' describe an indexed reference to a guest state section
   (guest state array).

   This function returns the corresponding guest state type to be used
   when indexing the corresponding array in the second shadow (origin
   tracking) area.  If the array is not to be origin-tracked, return
   Ity_INVALID.

   This function must agree with MC_(get_otrack_shadow_offset) above.
   See comments at the start of MC_(get_otrack_shadow_offset).
*/
IRType MC_(get_otrack_reg_array_equiv_int_type) ( IRRegArray* arr )
{
   /* -------------------- ppc64 -------------------- */
#  if defined(VGA_ppc64)
   /* The redir stack. */
   if (arr->base == offsetof(VexGuestPPC64State,guest_REDIR_STACK[0])
       && arr->elemTy == Ity_I64
       && arr->nElems == VEX_GUEST_PPC64_REDIR_STACK_SIZE)
      return Ity_I64;

   VG_(printf)("get_reg_array_equiv_int_type(ppc64): unhandled: ");
   ppIRRegArray(arr);
   VG_(printf)("\n");
   tl_assert(0);

   /* -------------------- ppc32 -------------------- */
#  elif defined(VGA_ppc32)
   /* The redir stack. */
   if (arr->base == offsetof(VexGuestPPC32State,guest_REDIR_STACK[0])
       && arr->elemTy == Ity_I32
       && arr->nElems == VEX_GUEST_PPC32_REDIR_STACK_SIZE)
      return Ity_I32;

   VG_(printf)("get_reg_array_equiv_int_type(ppc32): unhandled: ");
   ppIRRegArray(arr);
   VG_(printf)("\n");
   tl_assert(0);

   /* -------------------- amd64 -------------------- */
#  elif defined(VGA_amd64)
   /* Ignore the FP tag array - pointless to shadow, and in any case
      the elements are too small */
   if (arr->base == offsetof(VexGuestAMD64State,guest_FPTAG)
       && arr->elemTy == Ity_I8 && arr->nElems == 8)
      return Ity_INVALID;

   /* The FP register array */
   if (arr->base == offsetof(VexGuestAMD64State,guest_FPREG[0])
       && arr->elemTy == Ity_F64 && arr->nElems == 8)
      return Ity_I64;

   VG_(printf)("get_reg_array_equiv_int_type(amd64): unhandled: ");
   ppIRRegArray(arr);
   VG_(printf)("\n");
   tl_assert(0);

   /* --------------------- x86 --------------------- */
#  elif defined(VGA_x86)
   /* Ignore the FP tag array - pointless to shadow, and in any case
      the elements are too small */
   if (arr->base == offsetof(VexGuestX86State,guest_FPTAG)
       && arr->elemTy == Ity_I8 && arr->nElems == 8)
      return Ity_INVALID;

   /* The FP register array */
   if (arr->base == offsetof(VexGuestX86State,guest_FPREG[0])
       && arr->elemTy == Ity_F64 && arr->nElems == 8)
      return Ity_I64;

   VG_(printf)("get_reg_array_equiv_int_type(x86): unhandled: ");
   ppIRRegArray(arr);
   VG_(printf)("\n");
   tl_assert(0);

   /* --------------------- arm --------------------- */
#  elif defined(VGA_arm)

   VG_(printf)("get_reg_array_equiv_int_type(arm): unhandled: ");
   ppIRRegArray(arr);
   VG_(printf)("\n");
   tl_assert(0);

#  else
#    error "FIXME: not implemented for this architecture"
#  endif
}


/*--------------------------------------------------------------------*/
/*--- end                                             mc_machine.c ---*/
/*--------------------------------------------------------------------*/
