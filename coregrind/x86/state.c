
/*--------------------------------------------------------------------*/
/*--- x86 registers, etc.                              x86/state.c ---*/
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

#include "libvex_guest_x86.h"

/*------------------------------------------------------------*/
/*--- baseBlock setup and operations                       ---*/
/*------------------------------------------------------------*/

/* The variables storing offsets. */
Int VGOFF_(m_vex) = INVALID_OFFSET;
Int VGOFF_(m_vex_shadow) = INVALID_OFFSET;

Int VGOFF_(ldt)   = INVALID_OFFSET;
Int VGOFF_(tls_ptr) = INVALID_OFFSET;
Int VGOFF_(m_eip) = INVALID_OFFSET;

Int VGOFF_(spillslots) = INVALID_OFFSET;



/* Here we assign actual offsets.  VEX dictates the layout (see
   comment at the end of libvex.h).  
*/
void VGA_(init_baseBlock) ( Addr client_eip, Addr esp_at_startup )
{
   vg_assert(0 == sizeof(VexGuestX86State) % 8);

   /* First the guest state. */
   VGOFF_(m_vex) = VG_(alloc_BaB)( sizeof(VexGuestX86State) / 4 );

   /* Then equal sized shadow state. */
   VGOFF_(m_vex_shadow) = VG_(alloc_BaB)( sizeof(VexGuestX86State) / 4 );

   /* Finally the spill area. */
   VGOFF_(spillslots) = VG_(alloc_BaB)( LibVEX_N_SPILL_BYTES/4 );
   if (0) VG_(printf)("SPILL SLOTS start at %d\n", VGOFF_(spillslots));

   /* Zero out the initial state, and set up the simulated FPU in a
      sane way. */
   LibVEX_GuestX86_initialise(BASEBLOCK_VEX);

   /* Zero out the shadow area. */
   VG_(memset)(BASEBLOCK_VEX_SHADOW, 0, sizeof(VexGuestX86State));

   /* Put essential stuff into the new state. */
   BASEBLOCK_VEX->guest_ESP = esp_at_startup;
   BASEBLOCK_VEX->guest_EIP = client_eip;

   /* The dispatch loop needs to be able to find %EIP. */
   VGOFF_(m_eip)
      = VGOFF_(m_vex) + offsetof(VexGuestX86State,guest_EIP)/4;

   if (VG_(needs).shadow_regs) {
      VG_TRACK( post_regs_write_init );
   }

   /* I assume that if we have SSE2 we also have SSE */
   VG_(have_ssestate) = False;
   //      VG_(cpu_has_feature)(VG_X86_FEAT_FXSR) &&
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

   /* initialise %cs, %ds and %ss to point at the operating systems
      default code, data and stack segments */
   asm volatile("movw %%cs, %0"
                :
                : "m" (BASEBLOCK_VEX->guest_CS));
   asm volatile("movw %%ds, %0"
                :
                : "m" (BASEBLOCK_VEX->guest_DS));
   asm volatile("movw %%ss, %0"
                :
                : "m" (BASEBLOCK_VEX->guest_SS));
}

/* Junk to fill up a thread's shadow regs with when shadow regs aren't
   being used. */
#define VG_UNUSED_SHADOW_REG_VALUE  0x27182818

void VGA_(load_state) ( arch_thread_t* arch, ThreadId tid )
{
   VG_(baseBlock)[VGOFF_(ldt)]  = (UInt)arch->ldt;
   VG_(baseBlock)[VGOFF_(tls_ptr)]  = (UInt)arch->tls;

   *BASEBLOCK_VEX = arch->vex;

   if (VG_(needs).shadow_regs) {
      *BASEBLOCK_VEX_SHADOW = arch->vex_shadow;
   } else {
      /* Fields shouldn't be used -- check their values haven't changed. */
     /* ummm ...
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
     */
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

   arch->vex = *BASEBLOCK_VEX;

   if (VG_(needs).shadow_regs) {
      arch->vex_shadow = *BASEBLOCK_VEX_SHADOW;
   } else {
      /* Fill with recognisable junk */
      /* can't easily do this ...
      arch->sh_eax =
      arch->sh_ebx =
      arch->sh_ecx =
      arch->sh_edx =
      arch->sh_esi =
      arch->sh_edi =
      arch->sh_ebp =
      arch->sh_esp =
      arch->sh_eflags = VG_UNUSED_SHADOW_REG_VALUE;
      */
   }
   /* Fill it up with junk. */
   VG_(baseBlock)[VGOFF_(ldt)] = junk;
   VG_(baseBlock)[VGOFF_(tls_ptr)] = junk;

   for (i = 0; i < (3 + sizeof(VexGuestX86State)) / 4; i++)
      VG_(baseBlock)[VGOFF_(m_vex) + i] = junk;
}

/*------------------------------------------------------------*/
/*--- Register access stuff                                ---*/
/*------------------------------------------------------------*/

void VGA_(set_thread_shadow_archreg) ( ThreadId tid, UInt archreg, UInt val )
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

UInt VGA_(get_thread_shadow_archreg) ( ThreadId tid, UInt archreg )
{
   ThreadState* tst;

   vg_assert(VG_(is_valid_tid)(tid));
   tst = & VG_(threads)[tid];

   if (0)
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
   if (0)
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
UInt VGA_(get_shadow_archreg) ( UInt archreg )
{
   return VG_(baseBlock)[ shadow_reg_index(archreg) ];
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
   esp -= 2 * sizeof(UWord);
   SET_PTHREQ_ESP(tid, esp);
   
   VG_TRACK ( new_mem_stack, esp, 2 * sizeof(UWord) );
   VG_TRACK ( pre_mem_write, Vg_CorePThread, tid, "new thread: stack",
                             esp, 2 * sizeof(UWord) );

   /* push arg and (bogus) return address */
   *(UWord*)(esp+sizeof(UWord)) = arg;
   *(UWord*)(esp)               = ret;

   VG_TRACK ( post_mem_write, esp, 2 * sizeof(UWord) );
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
      (VG_(client_end)-VG_(client_base)) / VKI_PAGE_SIZE, // limit
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
   struct vki_user_regs_struct regs;

   regs.cs     = BASEBLOCK_VEX->guest_CS;
   regs.ss     = BASEBLOCK_VEX->guest_SS;
   regs.ds     = BASEBLOCK_VEX->guest_DS;
   regs.es     = BASEBLOCK_VEX->guest_ES;
   regs.fs     = BASEBLOCK_VEX->guest_FS;
   regs.gs     = BASEBLOCK_VEX->guest_GS;
   regs.eax    = BASEBLOCK_VEX->guest_EAX;
   regs.ebx    = BASEBLOCK_VEX->guest_EBX;
   regs.ecx    = BASEBLOCK_VEX->guest_ECX;
   regs.edx    = BASEBLOCK_VEX->guest_EDX;
   regs.esi    = BASEBLOCK_VEX->guest_ESI;
   regs.edi    = BASEBLOCK_VEX->guest_EDI;
   regs.ebp    = BASEBLOCK_VEX->guest_EBP;
   regs.esp    = BASEBLOCK_VEX->guest_ESP;
   regs.eflags = LibVEX_GuestX86_get_eflags(BASEBLOCK_VEX);
   regs.eip    = BASEBLOCK_VEX->guest_EIP;

   return ptrace(PTRACE_SETREGS, pid, NULL, &regs);
}

Int VGA_(ptrace_setregs_from_tst)(Int pid, arch_thread_t* arch)
{
   struct vki_user_regs_struct regs;

   regs.cs     = arch->vex.guest_CS;
   regs.ss     = arch->vex.guest_SS;
   regs.ds     = arch->vex.guest_DS;
   regs.es     = arch->vex.guest_ES;
   regs.fs     = arch->vex.guest_FS;
   regs.gs     = arch->vex.guest_GS;
   regs.eax    = arch->vex.guest_EAX;
   regs.ebx    = arch->vex.guest_EBX;
   regs.ecx    = arch->vex.guest_ECX;
   regs.edx    = arch->vex.guest_EDX;
   regs.esi    = arch->vex.guest_ESI;
   regs.edi    = arch->vex.guest_EDI;
   regs.ebp    = arch->vex.guest_EBP;
   regs.esp    = arch->vex.guest_ESP;
   regs.eflags = LibVEX_GuestX86_get_eflags(&arch->vex);
   regs.eip    = arch->vex.guest_EIP;

   return ptrace(PTRACE_SETREGS, pid, NULL, &regs);
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
