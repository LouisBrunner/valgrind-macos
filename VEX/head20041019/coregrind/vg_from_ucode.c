
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

   switch (archreg) {
      case R_EAX: tst->arch.sh_eax = val; break;
      case R_ECX: tst->arch.sh_ecx = val; break;
      case R_EDX: tst->arch.sh_edx = val; break;
      case R_EBX: tst->arch.sh_ebx = val; break;
      case R_ESP: tst->arch.sh_esp = val; break;
      case R_EBP: tst->arch.sh_ebp = val; break;
      case R_ESI: tst->arch.sh_esi = val; break;
      case R_EDI: tst->arch.sh_edi = val; break;
      default:    VG_(core_panic)( "set_thread_shadow_archreg");
   }
}

UInt VG_(get_thread_shadow_archreg) ( ThreadId tid, UInt archreg )
{
   ThreadState* tst;

   vg_assert(VG_(is_valid_tid)(tid));
   tst = & VG_(threads)[tid];

   switch (archreg) {
      case R_EAX: return tst->arch.sh_eax;
      case R_ECX: return tst->arch.sh_ecx;
      case R_EDX: return tst->arch.sh_edx;
      case R_EBX: return tst->arch.sh_ebx;
      case R_ESP: return tst->arch.sh_esp;
      case R_EBP: return tst->arch.sh_ebp;
      case R_ESI: return tst->arch.sh_esi;
      case R_EDI: return tst->arch.sh_edi;
      default:    VG_(core_panic)( "get_thread_shadow_archreg");
   }
}

/* Return the baseBlock index for the specified shadow register */
static Int shadow_reg_index ( Int arch )
{
   switch (arch) {
      case R_EAX: return VGOFF_(sh_eax);
      case R_ECX: return VGOFF_(sh_ecx);
      case R_EDX: return VGOFF_(sh_edx);
      case R_EBX: return VGOFF_(sh_ebx);
      case R_ESP: return VGOFF_(sh_esp);
      case R_EBP: return VGOFF_(sh_ebp);
      case R_ESI: return VGOFF_(sh_esi);
      case R_EDI: return VGOFF_(sh_edi);
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
