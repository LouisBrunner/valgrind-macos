
/*--------------------------------------------------------------------*/
/*--- x86 registers, etc.                                  state.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an extensible x86 protected-mode
   emulator for monitoring program execution on x86-Unixes.

   Copyright (C) 2000-2004 Nicholas Nethercote
      njn25@cam.ac.uk

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

static Int extractDflag(UInt eflags)
{
   return ( eflags & EFlagD ? -1 : 1 );
}

static UInt insertDflag(UInt eflags, Int d)
{
   vg_assert(d == 1 || d == -1);
   eflags &= ~EFlagD;
   if (d < 0) eflags |= EFlagD;
   return eflags;
}


/* Junk to fill up a thread's shadow regs with when shadow regs aren't
   being used. */
#define VG_UNUSED_SHADOW_REG_VALUE  0x27182818

void VGA_(load_state) ( arch_thread_t* arch, ThreadId tid )
{
   Int i;

   VG_(baseBlock)[VGOFF_(ldt)]  = (UInt)arch->ldt;
   VG_(baseBlock)[VGOFF_(tls_ptr)]  = (UInt)arch->tls;
   VG_(baseBlock)[VGOFF_(m_cs)] = arch->m_cs;
   VG_(baseBlock)[VGOFF_(m_ss)] = arch->m_ss;
   VG_(baseBlock)[VGOFF_(m_ds)] = arch->m_ds;
   VG_(baseBlock)[VGOFF_(m_es)] = arch->m_es;
   VG_(baseBlock)[VGOFF_(m_fs)] = arch->m_fs;
   VG_(baseBlock)[VGOFF_(m_gs)] = arch->m_gs;

   VG_(baseBlock)[VGOFF_(m_eax)] = arch->m_eax;
   VG_(baseBlock)[VGOFF_(m_ebx)] = arch->m_ebx;
   VG_(baseBlock)[VGOFF_(m_ecx)] = arch->m_ecx;
   VG_(baseBlock)[VGOFF_(m_edx)] = arch->m_edx;
   VG_(baseBlock)[VGOFF_(m_esi)] = arch->m_esi;
   VG_(baseBlock)[VGOFF_(m_edi)] = arch->m_edi;
   VG_(baseBlock)[VGOFF_(m_ebp)] = arch->m_ebp;
   VG_(baseBlock)[VGOFF_(m_esp)] = arch->m_esp;
   VG_(baseBlock)[VGOFF_(m_eflags)] = arch->m_eflags & ~EFlagD;
   VG_(baseBlock)[VGOFF_(m_dflag)] = extractDflag(arch->m_eflags);
   VG_(baseBlock)[VGOFF_(m_eip)] = arch->m_eip;

   for (i = 0; i < VG_SIZE_OF_SSESTATE_W; i++)
      VG_(baseBlock)[VGOFF_(m_ssestate) + i] = arch->m_sse[i];

   if (VG_(needs).shadow_regs) {
      VG_(baseBlock)[VGOFF_(sh_eax)] = arch->sh_eax;
      VG_(baseBlock)[VGOFF_(sh_ebx)] = arch->sh_ebx;
      VG_(baseBlock)[VGOFF_(sh_ecx)] = arch->sh_ecx;
      VG_(baseBlock)[VGOFF_(sh_edx)] = arch->sh_edx;
      VG_(baseBlock)[VGOFF_(sh_esi)] = arch->sh_esi;
      VG_(baseBlock)[VGOFF_(sh_edi)] = arch->sh_edi;
      VG_(baseBlock)[VGOFF_(sh_ebp)] = arch->sh_ebp;
      VG_(baseBlock)[VGOFF_(sh_esp)] = arch->sh_esp;
      VG_(baseBlock)[VGOFF_(sh_eflags)] = arch->sh_eflags;
   } else {
      /* Fields shouldn't be used -- check their values haven't changed. */
      vg_assert(
         VG_UNUSED_SHADOW_REG_VALUE == arch->sh_eax &&
         VG_UNUSED_SHADOW_REG_VALUE == arch->sh_ebx &&
         VG_UNUSED_SHADOW_REG_VALUE == arch->sh_ecx &&
         VG_UNUSED_SHADOW_REG_VALUE == arch->sh_edx &&
         VG_UNUSED_SHADOW_REG_VALUE == arch->sh_esi &&
         VG_UNUSED_SHADOW_REG_VALUE == arch->sh_edi &&
         VG_UNUSED_SHADOW_REG_VALUE == arch->sh_ebp &&
         VG_UNUSED_SHADOW_REG_VALUE == arch->sh_esp &&
         VG_UNUSED_SHADOW_REG_VALUE == arch->sh_eflags);
   }

}

void VGA_(save_state)( arch_thread_t *arch, ThreadId tid )
{
   Int i;
   const UInt junk = 0xDEADBEEF;

   /* We don't copy out the LDT entry, because it can never be changed
      by the normal actions of the thread, only by the modify_ldt
      syscall, in which case we will correctly be updating
      VG_(threads)[tid].ldt.  This printf happens iff the following
      assertion fails. */
   if ((void*)arch->ldt != (void*)VG_(baseBlock)[VGOFF_(ldt)])
      VG_(printf)("VG_(threads)[%d].ldt=%p  VG_(baseBlock)[VGOFF_(ldt)]=%p\n",
                 tid, (void*)arch->ldt, 
                       (void*)VG_(baseBlock)[VGOFF_(ldt)]);

   vg_assert((void*)arch->ldt == (void*)VG_(baseBlock)[VGOFF_(ldt)]);

   /* We don't copy out the TLS entry, because it can never be changed
      by the normal actions of the thread, only by the set_thread_area
      syscall, in which case we will correctly be updating
      arch->tls.  This printf happens iff the following
      assertion fails. */
   if ((void*)arch->tls != (void*)VG_(baseBlock)[VGOFF_(tls_ptr)])
      VG_(printf)("VG_(threads)[%d].tls=%p  VG_(baseBlock)[VGOFF_(tls_ptr)]=%p\
n",
                 tid, (void*)arch->tls, 
                       (void*)VG_(baseBlock)[VGOFF_(tls_ptr)]);

   vg_assert((void*)arch->tls 
             == (void*)VG_(baseBlock)[VGOFF_(tls_ptr)]);

   arch->m_cs = VG_(baseBlock)[VGOFF_(m_cs)];
   arch->m_ss = VG_(baseBlock)[VGOFF_(m_ss)];
   arch->m_ds = VG_(baseBlock)[VGOFF_(m_ds)];
   arch->m_es = VG_(baseBlock)[VGOFF_(m_es)];
   arch->m_fs = VG_(baseBlock)[VGOFF_(m_fs)];
   arch->m_gs = VG_(baseBlock)[VGOFF_(m_gs)];

   arch->m_eax = VG_(baseBlock)[VGOFF_(m_eax)];
   arch->m_ebx = VG_(baseBlock)[VGOFF_(m_ebx)];
   arch->m_ecx = VG_(baseBlock)[VGOFF_(m_ecx)];
   arch->m_edx = VG_(baseBlock)[VGOFF_(m_edx)];
   arch->m_esi = VG_(baseBlock)[VGOFF_(m_esi)];
   arch->m_edi = VG_(baseBlock)[VGOFF_(m_edi)];
   arch->m_ebp = VG_(baseBlock)[VGOFF_(m_ebp)];
   arch->m_esp = VG_(baseBlock)[VGOFF_(m_esp)];
   arch->m_eflags 
      = insertDflag(VG_(baseBlock)[VGOFF_(m_eflags)],
                    VG_(baseBlock)[VGOFF_(m_dflag)]);
   arch->m_eip = VG_(baseBlock)[VGOFF_(m_eip)];

   for (i = 0; i < VG_SIZE_OF_SSESTATE_W; i++)
      arch->m_sse[i] 
         = VG_(baseBlock)[VGOFF_(m_ssestate) + i];

   if (VG_(needs).shadow_regs) {
      arch->sh_eax = VG_(baseBlock)[VGOFF_(sh_eax)];
      arch->sh_ebx = VG_(baseBlock)[VGOFF_(sh_ebx)];
      arch->sh_ecx = VG_(baseBlock)[VGOFF_(sh_ecx)];
      arch->sh_edx = VG_(baseBlock)[VGOFF_(sh_edx)];
      arch->sh_esi = VG_(baseBlock)[VGOFF_(sh_esi)];
      arch->sh_edi = VG_(baseBlock)[VGOFF_(sh_edi)];
      arch->sh_ebp = VG_(baseBlock)[VGOFF_(sh_ebp)];
      arch->sh_esp = VG_(baseBlock)[VGOFF_(sh_esp)];
      arch->sh_eflags = VG_(baseBlock)[VGOFF_(sh_eflags)];
   } else {
      /* Fill with recognisable junk */
      arch->sh_eax =
      arch->sh_ebx =
      arch->sh_ecx =
      arch->sh_edx =
      arch->sh_esi =
      arch->sh_edi =
      arch->sh_ebp =
      arch->sh_esp = 
      arch->sh_eflags = VG_UNUSED_SHADOW_REG_VALUE;
   }
   /* Fill it up with junk. */
   VG_(baseBlock)[VGOFF_(ldt)] = junk;
   VG_(baseBlock)[VGOFF_(tls_ptr)] = junk;
   VG_(baseBlock)[VGOFF_(m_cs)] = junk;
   VG_(baseBlock)[VGOFF_(m_ss)] = junk;
   VG_(baseBlock)[VGOFF_(m_ds)] = junk;
   VG_(baseBlock)[VGOFF_(m_es)] = junk;
   VG_(baseBlock)[VGOFF_(m_fs)] = junk;
   VG_(baseBlock)[VGOFF_(m_gs)] = junk;

   VG_(baseBlock)[VGOFF_(m_eax)] = junk;
   VG_(baseBlock)[VGOFF_(m_ebx)] = junk;
   VG_(baseBlock)[VGOFF_(m_ecx)] = junk;
   VG_(baseBlock)[VGOFF_(m_edx)] = junk;
   VG_(baseBlock)[VGOFF_(m_esi)] = junk;
   VG_(baseBlock)[VGOFF_(m_edi)] = junk;
   VG_(baseBlock)[VGOFF_(m_ebp)] = junk;
   VG_(baseBlock)[VGOFF_(m_esp)] = junk;
   VG_(baseBlock)[VGOFF_(m_eflags)] = junk;
   VG_(baseBlock)[VGOFF_(m_eip)] = junk;

   for (i = 0; i < VG_SIZE_OF_SSESTATE_W; i++)
      VG_(baseBlock)[VGOFF_(m_ssestate) + i] = junk;
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
