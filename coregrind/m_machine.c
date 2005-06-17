
/*--------------------------------------------------------------------*/
/*--- Machine-related stuff.                           m_machine.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2005 Julian Seward 
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
#include "pub_core_libcassert.h"
#include "pub_core_libcbase.h"
#include "pub_core_machine.h"
#include "pub_core_scheduler.h"

#define INSTR_PTR(regs)    ((regs).vex.VGA_INSTR_PTR)
#define STACK_PTR(regs)    ((regs).vex.VGA_STACK_PTR)
#define FRAME_PTR(regs)    ((regs).vex.VGA_FRAME_PTR)

Addr VG_(get_SP) ( ThreadId tid )
{
   return STACK_PTR( VG_(threads)[tid].arch );
}

Addr VG_(get_IP) ( ThreadId tid )
{
   return INSTR_PTR( VG_(threads)[tid].arch );
}

Addr VG_(get_FP) ( ThreadId tid )
{
   return FRAME_PTR( VG_(threads)[tid].arch );
}

void VG_(set_SP) ( ThreadId tid, Addr sp )
{
   STACK_PTR( VG_(threads)[tid].arch ) = sp;
}

void VG_(set_IP) ( ThreadId tid, Addr ip )
{
   INSTR_PTR( VG_(threads)[tid].arch ) = ip;
}


void VG_(get_shadow_regs_area) ( ThreadId tid, OffT offset, SizeT size,
                                 UChar* area )
{
   ThreadState* tst;

   vg_assert(VG_(is_valid_tid)(tid));
   tst = & VG_(threads)[tid];

   // Bounds check
   vg_assert(0 <= offset && offset < sizeof(VexGuestArchState));
   vg_assert(offset + size <= sizeof(VexGuestArchState));

   VG_(memcpy)( area, (void*)(((Addr)&(tst->arch.vex_shadow)) + offset), size);
}

void VG_(set_shadow_regs_area) ( ThreadId tid, OffT offset, SizeT size,
                                 const UChar* area )
{
   ThreadState* tst;

   vg_assert(VG_(is_valid_tid)(tid));
   tst = & VG_(threads)[tid];

   // Bounds check
   vg_assert(0 <= offset && offset < sizeof(VexGuestArchState));
   vg_assert(offset + size <= sizeof(VexGuestArchState));

   VG_(memcpy)( (void*)(((Addr)(&tst->arch.vex_shadow)) + offset), area, size);
}


static void apply_to_GPs_of_tid(VexGuestArchState* vex, void (*f)(Addr))
{
#if defined(VGA_x86)
   (*f)(vex->guest_EAX);
   (*f)(vex->guest_ECX);
   (*f)(vex->guest_EDX);
   (*f)(vex->guest_EBX);
   (*f)(vex->guest_ESI);
   (*f)(vex->guest_EDI);
   (*f)(vex->guest_ESP);
   (*f)(vex->guest_EBP);
#elif defined(VGA_amd64)
   (*f)(vex->guest_RAX);
   (*f)(vex->guest_RCX);
   (*f)(vex->guest_RDX);
   (*f)(vex->guest_RBX);
   (*f)(vex->guest_RSI);
   (*f)(vex->guest_RDI);
   (*f)(vex->guest_RSP);
   (*f)(vex->guest_RBP);
   (*f)(vex->guest_R8);
   (*f)(vex->guest_R9);
   (*f)(vex->guest_R10);
   (*f)(vex->guest_R11);
   (*f)(vex->guest_R12);
   (*f)(vex->guest_R13);
   (*f)(vex->guest_R14);
   (*f)(vex->guest_R15);
#else
#  error Unknown arch
#endif
}


void VG_(apply_to_GP_regs)(void (*f)(UWord))
{
   ThreadId tid;

   for (tid = 1; tid < VG_N_THREADS; tid++) {
      if (VG_(is_valid_tid)(tid)) {
         ThreadState* tst = VG_(get_ThreadState)(tid);
         apply_to_GPs_of_tid(&(tst->arch.vex), f);
      }
   }
}



/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
