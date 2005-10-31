
/*--------------------------------------------------------------------*/
/*--- Dumping core.                         coredump-amd64-linux.c ---*/
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
#include "pub_core_libcbase.h"
#include "pub_core_coredump.h"
#include "pub_core_threadstate.h"

#include "priv_elf.h"

void ML_(fill_elfregs_from_tst)(struct vki_user_regs_struct* regs, 
                                const ThreadArchState* arch)
{
   regs->eflags = LibVEX_GuestAMD64_get_rflags(&arch->vex);
   regs->rsp    = arch->vex.guest_RSP;
   regs->rip    = arch->vex.guest_RIP;

   regs->rbx    = arch->vex.guest_RBX;
   regs->rcx    = arch->vex.guest_RCX;
   regs->rdx    = arch->vex.guest_RDX;
   regs->rsi    = arch->vex.guest_RSI;
   regs->rdi    = arch->vex.guest_RDI;
   regs->rbp    = arch->vex.guest_RBP;
   regs->rax    = arch->vex.guest_RAX;
   regs->r8     = arch->vex.guest_R8;
   regs->r9     = arch->vex.guest_R9;
   regs->r10    = arch->vex.guest_R10;
   regs->r11    = arch->vex.guest_R11;
   regs->r12    = arch->vex.guest_R12;
   regs->r13    = arch->vex.guest_R13;
   regs->r14    = arch->vex.guest_R14;
   regs->r15    = arch->vex.guest_R15;

//::    regs->cs     = arch->vex.guest_cs;
//::    regs->fs     = arch->vex.guest_fs;
//::    regs->gs     = arch->vex.guest_gs;
}

void ML_(fill_elffpregs_from_tst)(vki_elf_fpregset_t* fpu,
                                  const ThreadArchState* arch)
{
//::    fpu->cwd = ?;
//::    fpu->swd = ?;
//::    fpu->twd = ?;
//::    fpu->fop = ?;
//::    fpu->rip = ?;
//::    fpu->rdp = ?;
//::    fpu->mxcsr = ?;
//::    fpu->mxcsr_mask = ?;
//::    fpu->st_space = ?;

#  define DO(n)  VG_(memcpy)(fpu->xmm_space + n * 4, &arch->vex.guest_XMM##n, sizeof(arch->vex.guest_XMM##n))
   DO(0);  DO(1);  DO(2);  DO(3);  DO(4);  DO(5);  DO(6);  DO(7);
   DO(8);  DO(9);  DO(10); DO(11); DO(12); DO(13); DO(14); DO(15);
#  undef DO

   VG_(memset)(fpu->padding, 0, sizeof(fpu->padding));
}

void VG_(make_coredump)(ThreadId tid, const vki_siginfo_t *si, UInt max_size)
{
   ML_(make_elf_coredump)(tid, si, max_size);
}
