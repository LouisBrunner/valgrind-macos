
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

#include "pub_core_basics.h"
#include "pub_core_threadstate.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcbase.h"
#include "pub_core_machine.h"

#define INSTR_PTR(regs)    ((regs).vex.VG_INSTR_PTR)
#define STACK_PTR(regs)    ((regs).vex.VG_STACK_PTR)
#define FRAME_PTR(regs)    ((regs).vex.VG_FRAME_PTR)

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
#elif defined(VGA_ppc32)
   /* XXX ask tool about validity? */
   (*f)(vex->guest_GPR0);
   (*f)(vex->guest_GPR1);
   (*f)(vex->guest_GPR2);
   (*f)(vex->guest_GPR3);
   (*f)(vex->guest_GPR4);
   (*f)(vex->guest_GPR5);
   (*f)(vex->guest_GPR6);
   (*f)(vex->guest_GPR7);
   (*f)(vex->guest_GPR8);
   (*f)(vex->guest_GPR9);
   (*f)(vex->guest_GPR10);
   (*f)(vex->guest_GPR11);
   (*f)(vex->guest_GPR12);
   (*f)(vex->guest_GPR13);
   (*f)(vex->guest_GPR14);
   (*f)(vex->guest_GPR15);
   (*f)(vex->guest_GPR16);
   (*f)(vex->guest_GPR17);
   (*f)(vex->guest_GPR18);
   (*f)(vex->guest_GPR19);
   (*f)(vex->guest_GPR20);
   (*f)(vex->guest_GPR21);
   (*f)(vex->guest_GPR22);
   (*f)(vex->guest_GPR23);
   (*f)(vex->guest_GPR24);
   (*f)(vex->guest_GPR25);
   (*f)(vex->guest_GPR26);
   (*f)(vex->guest_GPR27);
   (*f)(vex->guest_GPR28);
   (*f)(vex->guest_GPR29);
   (*f)(vex->guest_GPR30);
   (*f)(vex->guest_GPR31);
   (*f)(vex->guest_CTR);
   (*f)(vex->guest_LR);

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

static ThreadId thread_stack_iter = VG_INVALID_THREADID;

void VG_(thread_stack_reset_iter)(void)
{
   thread_stack_iter = 1;
}

Bool VG_(thread_stack_next)(ThreadId* tid, Addr* stack_min, Addr* stack_max)
{
   ThreadId i;
   for (i = thread_stack_iter; i < VG_N_THREADS; i++) {
      if (VG_(threads)[i].status != VgTs_Empty) {
         *tid       = i;
         *stack_min = VG_(get_SP)(i);
         *stack_max = VG_(threads)[i].client_stack_highest_word;
         thread_stack_iter = i + 1;
         return True;
      }
   }
   return False;
}

//////////////////////////////////////////////////////////////////
// Architecture specifics

// PPC: what is the cache line size (for dcbz etc) ?  This info is
// harvested on Linux at startup from the AT_SYSINFO entries.  0 means
// not-yet-set.
#if defined(VGA_ppc32)
Int VG_(cache_line_size_ppc32) = 0;
#endif

// X86: set to 1 if the host is able to do {ld,st}mxcsr (load/store
// the SSE control/status register.  For most modern CPUs this will be
// 1.  It is set to 1, if possible, by m_translate.getArchAndArchInfo.
// The value is read by m_dispatch.dispatch-x86.S, which is why it
// is an Int rather than a Bool.
//
// Ugly hack: this has to start as 0 and be set to 1 in the normal
// case, rather than the other way round, because the dispatch
// loop needs it, and it runs before the first translation is 
// made.  Yet it is the act of making that first translation which
// causes getArchAndArchInfo to set this value to its final value.
// So it is necessary to start this value off at 0 as only that
// guarantees that the dispatch loop will not SIGILL on its first
// attempt.
#if defined(VGA_x86)
Int VG_(have_mxcsr_x86) = 0;
#endif


/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
