
/*--------------------------------------------------------------------*/
/*--- Dumping core.                         coredump-ppc32-linux.c ---*/
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
#include "pub_core_coredump.h"
#include "pub_core_threadstate.h"

#include "priv_elf.h"

void ML_(fill_elfregs_from_tst)(struct vki_user_regs_struct* regs, 
                                const ThreadArchState* arch)
{
#  define DO(n)  regs->gpr[n] = arch->vex.guest_GPR##n
   DO(0);  DO(1);  DO(2);  DO(3);  DO(4);  DO(5);  DO(6);  DO(7);
   DO(8);  DO(9);  DO(10); DO(11); DO(12); DO(13); DO(14); DO(15);
   DO(16); DO(17); DO(18); DO(19); DO(20); DO(21); DO(22); DO(23);
   DO(24); DO(25); DO(26); DO(27); DO(28); DO(29); DO(30); DO(31);
#  undef DO

   regs->nip = arch->vex.guest_CIA;
   regs->msr = 0xf032;   /* pretty arbitrary */
   regs->orig_gpr3 = arch->vex.guest_GPR3;
   regs->ctr = arch->vex.guest_CTR;
   regs->link = arch->vex.guest_LR;
   regs->xer = LibVEX_GuestPPC32_get_XER( &((ThreadArchState*)arch)->vex );
   regs->ccr = LibVEX_GuestPPC32_get_CR( &((ThreadArchState*)arch)->vex );
   regs->mq = 0;
   regs->trap = 0;
   regs->dar = 0; /* should be fault address? */
   regs->dsisr = 0;
   regs->result = 0;
}

void ML_(fill_elffpregs_from_tst)(vki_elf_fpregset_t* fpu,
                                  const ThreadArchState* arch)
{
   /* The guest state has the FPR fields declared as ULongs, so need
      to fish out the values without converting them. */
#  define DO(n)  (*fpu)[n] = *(double*)(&arch->vex.guest_FPR##n)
   DO(0);  DO(1);  DO(2);  DO(3);  DO(4);  DO(5);  DO(6);  DO(7);
   DO(8);  DO(9);  DO(10); DO(11); DO(12); DO(13); DO(14); DO(15);
   DO(16); DO(17); DO(18); DO(19); DO(20); DO(21); DO(22); DO(23);
   DO(24); DO(25); DO(26); DO(27); DO(28); DO(29); DO(30); DO(31);
#  undef DO
}

void VG_(make_coredump)(ThreadId tid, const vki_siginfo_t *si, UInt max_size)
{
   ML_(make_elf_coredump)(tid, si, max_size);
}
