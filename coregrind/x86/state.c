
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

/*------------------------------------------------------------*/
/*--- baseBlock setup and operations                       ---*/
/*------------------------------------------------------------*/

/* The variables storing offsets. */

Int VGOFF_(m_eax) = INVALID_OFFSET;
Int VGOFF_(m_ecx) = INVALID_OFFSET;
Int VGOFF_(m_edx) = INVALID_OFFSET;
Int VGOFF_(m_ebx) = INVALID_OFFSET;
Int VGOFF_(m_esp) = INVALID_OFFSET;
Int VGOFF_(m_ebp) = INVALID_OFFSET;
Int VGOFF_(m_esi) = INVALID_OFFSET;
Int VGOFF_(m_edi) = INVALID_OFFSET;
Int VGOFF_(m_eflags) = INVALID_OFFSET;
Int VGOFF_(m_dflag)  = INVALID_OFFSET;
Int VGOFF_(m_ssestate) = INVALID_OFFSET;
Int VGOFF_(ldt)   = INVALID_OFFSET;
Int VGOFF_(tls_ptr) = INVALID_OFFSET;
Int VGOFF_(m_cs)  = INVALID_OFFSET;
Int VGOFF_(m_ss)  = INVALID_OFFSET;
Int VGOFF_(m_ds)  = INVALID_OFFSET;
Int VGOFF_(m_es)  = INVALID_OFFSET;
Int VGOFF_(m_fs)  = INVALID_OFFSET;
Int VGOFF_(m_gs)  = INVALID_OFFSET;
Int VGOFF_(m_eip) = INVALID_OFFSET;
Int VGOFF_(spillslots) = INVALID_OFFSET;
Int VGOFF_(sh_eax) = INVALID_OFFSET;
Int VGOFF_(sh_ecx) = INVALID_OFFSET;
Int VGOFF_(sh_edx) = INVALID_OFFSET;
Int VGOFF_(sh_ebx) = INVALID_OFFSET;
Int VGOFF_(sh_esp) = INVALID_OFFSET;
Int VGOFF_(sh_ebp) = INVALID_OFFSET;
Int VGOFF_(sh_esi) = INVALID_OFFSET;
Int VGOFF_(sh_edi) = INVALID_OFFSET;
Int VGOFF_(sh_eflags) = INVALID_OFFSET;

Int VGOFF_(helper_idiv_64_32) = INVALID_OFFSET;
Int VGOFF_(helper_div_64_32) = INVALID_OFFSET;
Int VGOFF_(helper_idiv_32_16) = INVALID_OFFSET;
Int VGOFF_(helper_div_32_16) = INVALID_OFFSET;
Int VGOFF_(helper_idiv_16_8) = INVALID_OFFSET;
Int VGOFF_(helper_div_16_8) = INVALID_OFFSET;
Int VGOFF_(helper_imul_32_64) = INVALID_OFFSET;
Int VGOFF_(helper_mul_32_64) = INVALID_OFFSET;
Int VGOFF_(helper_imul_16_32) = INVALID_OFFSET;
Int VGOFF_(helper_mul_16_32) = INVALID_OFFSET;
Int VGOFF_(helper_imul_8_16) = INVALID_OFFSET;
Int VGOFF_(helper_mul_8_16) = INVALID_OFFSET;
Int VGOFF_(helper_CLD) = INVALID_OFFSET;
Int VGOFF_(helper_STD) = INVALID_OFFSET;
Int VGOFF_(helper_get_dirflag) = INVALID_OFFSET;
Int VGOFF_(helper_CLC) = INVALID_OFFSET;
Int VGOFF_(helper_STC) = INVALID_OFFSET;
Int VGOFF_(helper_CMC) = INVALID_OFFSET;
Int VGOFF_(helper_shldl) = INVALID_OFFSET;
Int VGOFF_(helper_shldw) = INVALID_OFFSET;
Int VGOFF_(helper_shrdl) = INVALID_OFFSET;
Int VGOFF_(helper_shrdw) = INVALID_OFFSET;
Int VGOFF_(helper_IN) = INVALID_OFFSET;
Int VGOFF_(helper_OUT) = INVALID_OFFSET;
Int VGOFF_(helper_RDTSC) = INVALID_OFFSET;
Int VGOFF_(helper_CPUID) = INVALID_OFFSET;
Int VGOFF_(helper_BSWAP) = INVALID_OFFSET;
Int VGOFF_(helper_bsfw) = INVALID_OFFSET;
Int VGOFF_(helper_bsfl) = INVALID_OFFSET;
Int VGOFF_(helper_bsrw) = INVALID_OFFSET;
Int VGOFF_(helper_bsrl) = INVALID_OFFSET;
Int VGOFF_(helper_fstsw_AX) = INVALID_OFFSET;
Int VGOFF_(helper_SAHF) = INVALID_OFFSET;
Int VGOFF_(helper_LAHF) = INVALID_OFFSET;
Int VGOFF_(helper_DAS) = INVALID_OFFSET;
Int VGOFF_(helper_DAA) = INVALID_OFFSET;
Int VGOFF_(helper_AAS) = INVALID_OFFSET;
Int VGOFF_(helper_AAA) = INVALID_OFFSET;
Int VGOFF_(helper_AAD) = INVALID_OFFSET;
Int VGOFF_(helper_AAM) = INVALID_OFFSET;
Int VGOFF_(helper_cmpxchg8b) = INVALID_OFFSET;

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

/* Here we assign actual offsets.  It's important on x86 to get the most
   popular referents within 128 bytes of the start, so we can take
   advantage of short addressing modes relative to %ebp.  Popularity
   of offsets was measured on 22 Feb 02 running a KDE application, and
   the slots rearranged accordingly, with a 1.5% reduction in total
   size of translations. */
void VGA_(init_low_baseBlock) ( Addr client_eip, Addr esp_at_startup )
{
   /* Those with offsets under 128 are carefully chosen. */

   /* WORD offsets in this column */
   /* 0   */ VGOFF_(m_eax)     = VG_(alloc_BaB_1_set)(0);
   /* 1   */ VGOFF_(m_ecx)     = VG_(alloc_BaB_1_set)(0);
   /* 2   */ VGOFF_(m_edx)     = VG_(alloc_BaB_1_set)(0);
   /* 3   */ VGOFF_(m_ebx)     = VG_(alloc_BaB_1_set)(0);
   /* 4   */ VGOFF_(m_esp)     = VG_(alloc_BaB_1_set)(esp_at_startup);
   /* 5   */ VGOFF_(m_ebp)     = VG_(alloc_BaB_1_set)(0);
   /* 6   */ VGOFF_(m_esi)     = VG_(alloc_BaB_1_set)(0);
   /* 7   */ VGOFF_(m_edi)     = VG_(alloc_BaB_1_set)(0);
   /* 8   */ VGOFF_(m_eflags)  = VG_(alloc_BaB_1_set)(0);

   if (VG_(needs).shadow_regs) {
      /* 9   */ VGOFF_(sh_eax)    = VG_(alloc_BaB_1_set)(0);
      /* 10  */ VGOFF_(sh_ecx)    = VG_(alloc_BaB_1_set)(0);
      /* 11  */ VGOFF_(sh_edx)    = VG_(alloc_BaB_1_set)(0);
      /* 12  */ VGOFF_(sh_ebx)    = VG_(alloc_BaB_1_set)(0);
      /* 13  */ VGOFF_(sh_esp)    = VG_(alloc_BaB_1_set)(0);
      /* 14  */ VGOFF_(sh_ebp)    = VG_(alloc_BaB_1_set)(0);
      /* 15  */ VGOFF_(sh_esi)    = VG_(alloc_BaB_1_set)(0);
      /* 16  */ VGOFF_(sh_edi)    = VG_(alloc_BaB_1_set)(0);
      /* 17  */ VGOFF_(sh_eflags) = VG_(alloc_BaB_1_set)(0);
      VG_TRACK( post_regs_write_init );
   }

   /* 9,10,11 or 18,19,20... depends on number whether shadow regs are used
    * and on compact helpers registered */ 

   /* Make these most-frequently-called specialised ones compact, if they
      are used. */
   if (VG_(defined_new_mem_stack_4)())
      VG_(register_compact_helper)( (Addr) VG_(tool_interface).track_new_mem_stack_4);

   if (VG_(defined_die_mem_stack_4)())
      VG_(register_compact_helper)( (Addr) VG_(tool_interface).track_die_mem_stack_4);

}

void VGA_(init_high_baseBlock)( Addr client_eip, Addr esp_at_startup )
{
   /* (9/10 or 18/19) + n_compact_helpers */
   VGOFF_(m_eip) = VG_(alloc_BaB_1_set)(client_eip);

   /* There are currently 24 spill slots */
   /* (11+/20+ .. 32+/43+) + n_compact_helpers.  This can overlap the magic
    * boundary at >= 32 words, but most spills are to low numbered spill
    * slots, so the ones above the boundary don't see much action. */
   VGOFF_(spillslots) = VG_(alloc_BaB)(VG_MAX_SPILLSLOTS);

   /* I gave up counting at this point.  Since they're above the
      short-amode-boundary, there's no point. */

   VGOFF_(m_dflag) = VG_(alloc_BaB_1_set)(1);  // 1 == forward D-flag

   /* The FPU/SSE state.  This _must_ be 16-byte aligned.  Initial
      state doesn't matter much, as long as it's not totally borked. */
   VG_(align_BaB)(16);
   VGOFF_(m_ssestate) = VG_(alloc_BaB)(VG_SIZE_OF_SSESTATE_W);
   vg_assert( 
      0 == ( ((UInt)(& VG_(baseBlock)[VGOFF_(m_ssestate)])) % 16 )
   );

   /* I assume that if we have SSE2 we also have SSE */
   VG_(have_ssestate) = 
	   VG_(cpu_has_feature)(VG_X86_FEAT_FXSR) &&
	   VG_(cpu_has_feature)(VG_X86_FEAT_SSE);

   /* set up an initial FPU state (doesn't really matter what it is,
      so long as it's somewhat valid) */
   if (!VG_(have_ssestate))
      asm volatile("fwait; fnsave %0; fwait; frstor %0; fwait" 
                   : 
                   : "m" (VG_(baseBlock)[VGOFF_(m_ssestate)]) 
                   : "cc", "memory");
   else
      asm volatile("fwait; fxsave %0; fwait; andl $0xffbf, %1;"
                   "fxrstor %0; fwait"
                   : 
                   : "m" (VG_(baseBlock)[VGOFF_(m_ssestate)]), 
                     "m" (VG_(baseBlock)[VGOFF_(m_ssestate)+(24/4)]) 
                   : "cc", "memory");

   if (0) {
      if (VG_(have_ssestate))
         VG_(printf)("Looks like a SSE-capable CPU\n");
      else
         VG_(printf)("Looks like a MMX-only CPU\n");
   }

   /* LDT pointer: pretend the root thread has an empty LDT to start with. */
   VGOFF_(ldt)   = VG_(alloc_BaB_1_set)((UInt)NULL);

   /* TLS pointer: pretend the root thread has no TLS array for now. */
   VGOFF_(tls_ptr) = VG_(alloc_BaB_1_set)((UInt)NULL);

   /* segment registers */
   VGOFF_(m_cs)  = VG_(alloc_BaB_1_set)(0);
   VGOFF_(m_ss)  = VG_(alloc_BaB_1_set)(0);
   VGOFF_(m_ds)  = VG_(alloc_BaB_1_set)(0);
   VGOFF_(m_es)  = VG_(alloc_BaB_1_set)(0);
   VGOFF_(m_fs)  = VG_(alloc_BaB_1_set)(0);
   VGOFF_(m_gs)  = VG_(alloc_BaB_1_set)(0);

   /* initialise %cs, %ds and %ss to point at the operating systems
      default code, data and stack segments */
   asm volatile("movw %%cs, %0"
                :
                : "m" (VG_(baseBlock)[VGOFF_(m_cs)]));
   asm volatile("movw %%ds, %0"
                :
                : "m" (VG_(baseBlock)[VGOFF_(m_ds)]));
   asm volatile("movw %%ss, %0"
                :
                : "m" (VG_(baseBlock)[VGOFF_(m_ss)]));

   VG_(register_noncompact_helper)( (Addr) & VG_(do_useseg) );

#  define HELPER(name) \
   VGOFF_(helper_##name) = VG_(alloc_BaB_1_set)( (Addr) & VG_(helper_##name))

   /* Helper functions. */
   HELPER(idiv_64_32);     HELPER(div_64_32);
   HELPER(idiv_32_16);     HELPER(div_32_16);
   HELPER(idiv_16_8);      HELPER(div_16_8);

   HELPER(imul_32_64);     HELPER(mul_32_64);
   HELPER(imul_16_32);     HELPER(mul_16_32);
   HELPER(imul_8_16);      HELPER(mul_8_16);

   HELPER(CLD);            HELPER(STD);
   HELPER(get_dirflag);

   HELPER(CLC);            HELPER(STC);
   HELPER(CMC);

   HELPER(shldl);          HELPER(shldw);
   HELPER(shrdl);          HELPER(shrdw);

   HELPER(RDTSC);          HELPER(CPUID);

   HELPER(bsfw);           HELPER(bsfl);
   HELPER(bsrw);           HELPER(bsrl);

   HELPER(fstsw_AX);
   HELPER(SAHF);           HELPER(LAHF);
   HELPER(DAS);            HELPER(DAA);
   HELPER(AAS);            HELPER(AAA);
   HELPER(AAD);            HELPER(AAM);
   HELPER(IN);             HELPER(OUT);
   HELPER(cmpxchg8b);

   HELPER(undefined_instruction);

#  undef HELPER
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

/*------------------------------------------------------------*/
/*--- pointercheck                                         ---*/
/*------------------------------------------------------------*/

Bool VGA_(setup_pointercheck)(void)
{
   vki_modify_ldt_t ldt = { 
      VG_POINTERCHECK_SEGIDX,    // entry_number
      VG_(client_base),          // base_addr
      (VG_(client_end)-VG_(client_base)) / VKI_BYTES_PER_PAGE, // limit
      1,                         // seg_32bit
      0,                         // contents: data, RW, non-expanding
      0,                         // ! read_exec_only
      1,                         // limit_in_pages
      0,                         // ! seg not present
      1,                         // useable
   };
   int ret = VG_(do_syscall)(__NR_modify_ldt, 1, &ldt, sizeof(ldt));
   if (ret < 0) {
      VG_(message)(Vg_UserMsg,
                   "Warning: ignoring --pointercheck=yes, "
                   "because modify_ldt failed (errno=%d)", -ret);
      return False;
   } else {
      return True;
   }
}

/*------------------------------------------------------------*/
/*--- Debugger-related operations                          ---*/
/*------------------------------------------------------------*/

void VGA_(regs_for_ptrace_from_BB)(struct user_regs_struct* regs)
{
   regs->cs     = VG_(baseBlock)[VGOFF_(m_cs)];
   regs->ss     = VG_(baseBlock)[VGOFF_(m_ss)];
   regs->ds     = VG_(baseBlock)[VGOFF_(m_ds)];
   regs->es     = VG_(baseBlock)[VGOFF_(m_es)];
   regs->fs     = VG_(baseBlock)[VGOFF_(m_fs)];
   regs->gs     = VG_(baseBlock)[VGOFF_(m_gs)];
   regs->eax    = VG_(baseBlock)[VGOFF_(m_eax)];
   regs->ebx    = VG_(baseBlock)[VGOFF_(m_ebx)];
   regs->ecx    = VG_(baseBlock)[VGOFF_(m_ecx)];
   regs->edx    = VG_(baseBlock)[VGOFF_(m_edx)];
   regs->esi    = VG_(baseBlock)[VGOFF_(m_esi)];
   regs->edi    = VG_(baseBlock)[VGOFF_(m_edi)];
   regs->ebp    = VG_(baseBlock)[VGOFF_(m_ebp)];
   regs->esp    = VG_(baseBlock)[VGOFF_(m_esp)];
   regs->eflags = VG_(baseBlock)[VGOFF_(m_eflags)];
   regs->eip    = VG_(baseBlock)[VGOFF_(m_eip)];
}

void VGA_(regs_for_ptrace_from_tst)(arch_thread_t *tst,
                                    struct user_regs_struct* regs)
{
   regs->cs     = tst->m_cs;
   regs->ss     = tst->m_ss;
   regs->ds     = tst->m_ds;
   regs->es     = tst->m_es;
   regs->fs     = tst->m_fs;
   regs->gs     = tst->m_gs;
   regs->eax    = tst->m_eax;
   regs->ebx    = tst->m_ebx;
   regs->ecx    = tst->m_ecx;
   regs->edx    = tst->m_edx;
   regs->esi    = tst->m_esi;
   regs->edi    = tst->m_edi;
   regs->ebp    = tst->m_ebp;
   regs->esp    = tst->m_esp;
   regs->eflags = tst->m_eflags;
   regs->eip    = tst->m_eip;
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
