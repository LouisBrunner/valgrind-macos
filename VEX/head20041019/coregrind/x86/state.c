
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
#include "x86_private.h"
#include <sys/ptrace.h>

#include "../../../pub/libvex_guest_x86.h"

/*------------------------------------------------------------*/
/*--- baseBlock setup and operations                       ---*/
/*------------------------------------------------------------*/

/* The variables storing offsets. */
Int VGOFF_(m_vex) = INVALID_OFFSET;

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
   VGOFF_(m_vex) = VG_(alloc_BaB)( (3 + sizeof(VexGuestX86State)) / 4 );

   /* Set up the new vex integer state in a marginally safe way. */
   BASEBLOCK_VEX->guest_ESP = esp_at_startup;
   BASEBLOCK_VEX->guest_EIP = client_eip;
   BASEBLOCK_VEX->guest_CC_OP = 0; /* CC_OP_COPY */
   BASEBLOCK_VEX->guest_CC_SRC = 0;
   BASEBLOCK_VEX->guest_CC_DST = 0;

   BASEBLOCK_VEX->guest_EAX = BASEBLOCK_VEX->guest_EBX
                            = BASEBLOCK_VEX->guest_ECX
                            = BASEBLOCK_VEX->guest_EDX
                            = BASEBLOCK_VEX->guest_ESI
                            = BASEBLOCK_VEX->guest_EDI
                            = BASEBLOCK_VEX->guest_EBP
                            = 0;

   BASEBLOCK_VEX->guest_DFLAG = 1; /* forwards */

   /* set up an initial FPU state (doesn't really matter what it is,
      so long as it's somewhat valid) */
   vex_initialise_x87(BASEBLOCK_VEX);

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
   VGOFF_(m_eip) 
      = VGOFF_(m_vex) + offsetof(VexGuestX86State,guest_EIP)/4;
   VG_(baseBlock)[VGOFF_(m_eip)] = client_eip;
   VG_(printf)("o_vex %d, o_eip %d\n", VGOFF_(m_vex), VGOFF_(m_eip));
   /* There are currently 24 spill slots */
   /* (11+/20+ .. 32+/43+) + n_compact_helpers.  This can overlap the magic
    * boundary at >= 32 words, but most spills are to low numbered spill
    * slots, so the ones above the boundary don't see much action. */
   VGOFF_(spillslots) = VG_(alloc_BaB)(VG_MAX_SPILLSLOTS);
  VG_(printf)("SPILL SLOTS start at %d\n", VGOFF_(spillslots));
   /* I gave up counting at this point.  Since they're above the
      short-amode-boundary, there's no point. */

   /* I assume that if we have SSE2 we also have SSE */
   VG_(have_ssestate) = False;
   //	   VG_(cpu_has_feature)(VG_X86_FEAT_FXSR) &&
   //   VG_(cpu_has_feature)(VG_X86_FEAT_SSE);

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

   HELPER(RDTSC);          HELPER(CPUID);

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
   VG_(baseBlock)[VGOFF_(ldt)]  = (UInt)arch->ldt;
   VG_(baseBlock)[VGOFF_(tls_ptr)]  = (UInt)arch->tls;
   VG_(baseBlock)[VGOFF_(m_cs)] = arch->m_cs;
   VG_(baseBlock)[VGOFF_(m_ss)] = arch->m_ss;
   VG_(baseBlock)[VGOFF_(m_ds)] = arch->m_ds;
   VG_(baseBlock)[VGOFF_(m_es)] = arch->m_es;
   VG_(baseBlock)[VGOFF_(m_fs)] = arch->m_fs;
   VG_(baseBlock)[VGOFF_(m_gs)] = arch->m_gs;

   *BASEBLOCK_VEX = arch->vex;

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

   arch->vex = *BASEBLOCK_VEX;

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

   for (i = 0; i < (3 + sizeof(VexGuestX86State)) / 4; i++)
      VG_(baseBlock)[VGOFF_(m_vex) + i] = junk;
}

/*------------------------------------------------------------*/
/*--- Thread stuff                                         ---*/
/*------------------------------------------------------------*/

void VGA_(clear_thread)( arch_thread_t *arch )
{
   arch->ldt = NULL;
   VG_(clear_TLS_for_thread)(arch->tls);
}  

void VGA_(init_thread)( arch_thread_t *arch )
{
   VG_(baseBlock)[VGOFF_(tls_ptr)] = (UInt)arch->tls;
}  

void VGA_(cleanup_thread) ( arch_thread_t *arch )
{  
   /* Deallocate its LDT, if it ever had one. */
   VG_(deallocate_LDT_for_thread)( arch->ldt ); 
   arch->ldt = NULL;
   
   /* Clear its TLS array. */
   VG_(clear_TLS_for_thread)( arch->tls );
}  

void VGA_(setup_child) ( arch_thread_t *regs, arch_thread_t *parent_regs )
{  
   /* We inherit our parent's LDT. */
   if (parent_regs->ldt == NULL) {
      /* We hope this is the common case. */
      VG_(baseBlock)[VGOFF_(ldt)] = 0;
   } else {
      /* No luck .. we have to take a copy of the parent's. */
      regs->ldt = VG_(allocate_LDT_for_thread)( parent_regs->ldt );
      VG_(baseBlock)[VGOFF_(ldt)] = (UInt) regs->ldt;
   }

   /* Initialise the thread's TLS array */
   VG_(clear_TLS_for_thread)( regs->tls );
   VG_(baseBlock)[VGOFF_(tls_ptr)] = (UInt) regs->tls;
}  

void VGA_(set_arg_and_bogus_ret)( ThreadId tid, UWord arg, Addr ret )
{
   /* Push the arg, and mark it as readable. */
   SET_PTHREQ_ESP(tid, VG_(threads)[tid].arch.vex.guest_ESP - sizeof(UWord));
   * (UInt*)(VG_(threads)[tid].arch.vex.guest_ESP) = arg;
   VG_TRACK( post_mem_write, VG_(threads)[tid].arch.vex.guest_ESP, sizeof(void*) );

   /* Don't mark the pushed return address as readable; any attempt to read
      this is an internal valgrind bug since thread_exit_wrapper() should not
      return. */
   SET_PTHREQ_ESP(tid, VG_(threads)[tid].arch.vex.guest_ESP - sizeof(UWord));
   * (UInt*)(VG_(threads)[tid].arch.vex.guest_ESP) = ret;
}

void VGA_(thread_initial_stack)(ThreadId tid, UWord arg, Addr ret)
{
   Addr esp = (Addr)ARCH_STACK_PTR(VG_(threads)[tid].arch);

   /* push two args */
   esp -= 8;
   SET_PTHREQ_ESP(tid, esp);
   
   VG_TRACK ( new_mem_stack, esp, 2 * 4 );
   VG_TRACK ( pre_mem_write, Vg_CorePThread, tid, "new thread: stack",
                             esp, 2 * 4 );

   /* push arg and (bogus) return address */
   *(UWord*)(esp+4) = arg;
   *(UWord*)(esp)   = ret;

   VG_TRACK ( post_mem_write, esp, 2 * 4 );
}


/*------------------------------------------------------------*/
/*--- Symtab stuff                                         ---*/
/*------------------------------------------------------------*/

UInt *VGA_(reg_addr_from_BB)(Int regno)
{
   switch (regno) {
   case R_EAX: return &(BASEBLOCK_VEX->guest_EAX);
   case R_ECX: return &(BASEBLOCK_VEX->guest_ECX);
   case R_EDX: return &(BASEBLOCK_VEX->guest_EDX);
   case R_EBX: return &(BASEBLOCK_VEX->guest_EBX);
   case R_ESP: return &(BASEBLOCK_VEX->guest_ESP);
   case R_EBP: return &(BASEBLOCK_VEX->guest_EBP);
   case R_ESI: return &(BASEBLOCK_VEX->guest_ESI);
   case R_EDI: return &(BASEBLOCK_VEX->guest_EDI);
   default:    return NULL;
   }
}

UInt *VGA_(reg_addr_from_tst)(Int regno, arch_thread_t *arch)
{
   switch (regno) {
   case R_EAX: return &arch->vex.guest_EAX;
   case R_ECX: return &arch->vex.guest_ECX;
   case R_EDX: return &arch->vex.guest_EDX;
   case R_EBX: return &arch->vex.guest_EBX;
   case R_ESP: return &arch->vex.guest_ESP;
   case R_EBP: return &arch->vex.guest_EBP;
   case R_ESI: return &arch->vex.guest_ESI;
   case R_EDI: return &arch->vex.guest_EDI;
   default:    return NULL;
   }
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

Int VGA_(ptrace_setregs_from_BB)(Int pid)
{
   struct user_regs_struct regs;

   regs.cs     = VG_(baseBlock)[VGOFF_(m_cs)];
   regs.ss     = VG_(baseBlock)[VGOFF_(m_ss)];
   regs.ds     = VG_(baseBlock)[VGOFF_(m_ds)];
   regs.es     = VG_(baseBlock)[VGOFF_(m_es)];
   regs.fs     = VG_(baseBlock)[VGOFF_(m_fs)];
   regs.gs     = VG_(baseBlock)[VGOFF_(m_gs)];
   regs.eax    = BASEBLOCK_VEX->guest_EAX;
   regs.ebx    = BASEBLOCK_VEX->guest_EBX;
   regs.ecx    = BASEBLOCK_VEX->guest_ECX;
   regs.edx    = BASEBLOCK_VEX->guest_EDX;
   regs.esi    = BASEBLOCK_VEX->guest_ESI;
   regs.edi    = BASEBLOCK_VEX->guest_EDI;
   regs.ebp    = BASEBLOCK_VEX->guest_EBP;
   regs.esp    = BASEBLOCK_VEX->guest_ESP;
   regs.eflags = vex_to_eflags(BASEBLOCK_VEX);
   regs.eip    = BASEBLOCK_VEX->guest_EIP;

   return ptrace(PTRACE_SETREGS, pid, NULL, &regs);
}

Int VGA_(ptrace_setregs_from_tst)(Int pid, arch_thread_t* arch)
{
   struct user_regs_struct regs;

   regs.cs     = arch->m_cs;
   regs.ss     = arch->m_ss;
   regs.ds     = arch->m_ds;
   regs.es     = arch->m_es;
   regs.fs     = arch->m_fs;
   regs.gs     = arch->m_gs;
   regs.eax    = arch->vex.guest_EAX;
   regs.ebx    = arch->vex.guest_EBX;
   regs.ecx    = arch->vex.guest_ECX;
   regs.edx    = arch->vex.guest_EDX;
   regs.esi    = arch->vex.guest_ESI;
   regs.edi    = arch->vex.guest_EDI;
   regs.ebp    = arch->vex.guest_EBP;
   regs.esp    = arch->vex.guest_ESP;
   regs.eflags = vex_to_eflags(&arch->vex);
   regs.eip    = arch->vex.guest_EIP;

   return ptrace(PTRACE_SETREGS, pid, NULL, &regs);
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
