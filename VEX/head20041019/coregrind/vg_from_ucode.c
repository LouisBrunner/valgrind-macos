
/*--------------------------------------------------------------------*/
/*--- The JITter: translate ucode back to x86 code.                ---*/
/*---                                              vg_from_ucode.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an extensible x86 protected-mode
   emulator for monitoring program execution on x86-Unixes.

   Copyright (C) 2000-2004 Julian Seward
      jseward@acm.org

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
*/

#include "core.h"

void VG_(set_thread_shadow_archreg) ( ThreadId tid, UInt archreg, UInt val )
{
   ThreadState* tst;

   vg_assert(VG_(is_valid_tid)(tid));
   tst = & VG_(threads)[tid];
   if (0)
   VG_(printf)("set_thread_shadow_archreg(%d, %d, 0x%x)\n",
	       tid, archreg, val);
   switch (archreg) {
      case R_EAX: tst->arch.vex_shadow.guest_EAX = val; break;
      case R_ECX: tst->arch.vex_shadow.guest_ECX = val; break;
      case R_EDX: tst->arch.vex_shadow.guest_EDX = val; break;
      case R_EBX: tst->arch.vex_shadow.guest_EBX = val; break;
      case R_ESP: tst->arch.vex_shadow.guest_ESP = val; break;
      case R_EBP: tst->arch.vex_shadow.guest_EBP = val; break;
      case R_ESI: tst->arch.vex_shadow.guest_ESI = val; break;
      case R_EDI: tst->arch.vex_shadow.guest_EDI = val; break;
      default:    VG_(core_panic)( "set_thread_shadow_archreg");
   }
}

UInt VG_(get_thread_shadow_archreg) ( ThreadId tid, UInt archreg )
{
   ThreadState* tst;

   vg_assert(VG_(is_valid_tid)(tid));
   tst = & VG_(threads)[tid];

   VG_(printf)("get_thread_shadow_archreg(%d, %d)\n",
	       tid, archreg);

   switch (archreg) {
      case R_EAX: return tst->arch.vex_shadow.guest_EAX;
      case R_ECX: return tst->arch.vex_shadow.guest_ECX;
      case R_EDX: return tst->arch.vex_shadow.guest_EDX;
      case R_EBX: return tst->arch.vex_shadow.guest_EBX;
      case R_ESP: return tst->arch.vex_shadow.guest_ESP;
      case R_EBP: return tst->arch.vex_shadow.guest_EBP;
      case R_ESI: return tst->arch.vex_shadow.guest_ESI;
      case R_EDI: return tst->arch.vex_shadow.guest_EDI;
      default:    VG_(core_panic)( "get_thread_shadow_archreg");
   }
}

/* Return the baseBlock index for the specified shadow register */
static Int shadow_reg_index ( Int arch )
{
   VG_(printf)("shadow_reg_index(%d)\n",
	       arch);
   switch (arch) {
      case R_EAX: return VGOFF_(m_vex_shadow) + offsetof(VexGuestX86State,guest_EAX)/4;
      case R_ECX: return VGOFF_(m_vex_shadow) + offsetof(VexGuestX86State,guest_ECX)/4;
      case R_EDX: return VGOFF_(m_vex_shadow) + offsetof(VexGuestX86State,guest_EDX)/4;
      case R_EBX: return VGOFF_(m_vex_shadow) + offsetof(VexGuestX86State,guest_EBX)/4;
      case R_ESP: return VGOFF_(m_vex_shadow) + offsetof(VexGuestX86State,guest_ESP)/4;
      case R_EBP: return VGOFF_(m_vex_shadow) + offsetof(VexGuestX86State,guest_EBP)/4;
      case R_ESI: return VGOFF_(m_vex_shadow) + offsetof(VexGuestX86State,guest_ESI)/4;
      case R_EDI: return VGOFF_(m_vex_shadow) + offsetof(VexGuestX86State,guest_EDI)/4;
      default:    VG_(core_panic)( "shadow_reg_index");
   }
}

/* Accessing shadow arch. registers */
UInt VG_(get_shadow_archreg) ( UInt archreg )
{
   return VG_(baseBlock)[ shadow_reg_index(archreg) ];
}


/*--------------------------------------------------------------------*/
/*--- end                                          vg_from_ucode.c ---*/
/*--------------------------------------------------------------------*/
