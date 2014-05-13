/*--------------------------------------------------------------------*/
/*--- Machine-related stuff.                           m_machine.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2013 Julian Seward 
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
#include "pub_core_vki.h"
#include "pub_core_libcsetjmp.h"   // setjmp facilities
#include "pub_core_threadstate.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcfile.h"
#include "pub_core_libcprint.h"
#include "pub_core_mallocfree.h"
#include "pub_core_machine.h"
#include "pub_core_cpuid.h"
#include "pub_core_libcsignal.h"   // for ppc32 messing with SIGILL and SIGFPE
#include "pub_core_debuglog.h"


#define INSTR_PTR(regs)    ((regs).vex.VG_INSTR_PTR)
#define STACK_PTR(regs)    ((regs).vex.VG_STACK_PTR)
#define FRAME_PTR(regs)    ((regs).vex.VG_FRAME_PTR)

Addr VG_(get_IP) ( ThreadId tid ) {
   return INSTR_PTR( VG_(threads)[tid].arch );
}
Addr VG_(get_SP) ( ThreadId tid ) {
   return STACK_PTR( VG_(threads)[tid].arch );
}
Addr VG_(get_FP) ( ThreadId tid ) {
   return FRAME_PTR( VG_(threads)[tid].arch );
}

void VG_(set_IP) ( ThreadId tid, Addr ip ) {
   INSTR_PTR( VG_(threads)[tid].arch ) = ip;
}
void VG_(set_SP) ( ThreadId tid, Addr sp ) {
   STACK_PTR( VG_(threads)[tid].arch ) = sp;
}

void VG_(get_UnwindStartRegs) ( /*OUT*/UnwindStartRegs* regs,
                                ThreadId tid )
{
#  if defined(VGA_x86)
   regs->r_pc = (ULong)VG_(threads)[tid].arch.vex.guest_EIP;
   regs->r_sp = (ULong)VG_(threads)[tid].arch.vex.guest_ESP;
   regs->misc.X86.r_ebp
      = VG_(threads)[tid].arch.vex.guest_EBP;
#  elif defined(VGA_amd64)
   regs->r_pc = VG_(threads)[tid].arch.vex.guest_RIP;
   regs->r_sp = VG_(threads)[tid].arch.vex.guest_RSP;
   regs->misc.AMD64.r_rbp
      = VG_(threads)[tid].arch.vex.guest_RBP;
#  elif defined(VGA_ppc32)
   regs->r_pc = (ULong)VG_(threads)[tid].arch.vex.guest_CIA;
   regs->r_sp = (ULong)VG_(threads)[tid].arch.vex.guest_GPR1;
   regs->misc.PPC32.r_lr
      = VG_(threads)[tid].arch.vex.guest_LR;
#  elif defined(VGA_ppc64)
   regs->r_pc = VG_(threads)[tid].arch.vex.guest_CIA;
   regs->r_sp = VG_(threads)[tid].arch.vex.guest_GPR1;
   regs->misc.PPC64.r_lr
      = VG_(threads)[tid].arch.vex.guest_LR;
#  elif defined(VGA_arm)
   regs->r_pc = (ULong)VG_(threads)[tid].arch.vex.guest_R15T;
   regs->r_sp = (ULong)VG_(threads)[tid].arch.vex.guest_R13;
   regs->misc.ARM.r14
      = VG_(threads)[tid].arch.vex.guest_R14;
   regs->misc.ARM.r12
      = VG_(threads)[tid].arch.vex.guest_R12;
   regs->misc.ARM.r11
      = VG_(threads)[tid].arch.vex.guest_R11;
   regs->misc.ARM.r7
      = VG_(threads)[tid].arch.vex.guest_R7;
#  elif defined(VGA_arm64)
   regs->r_pc = VG_(threads)[tid].arch.vex.guest_PC;
   regs->r_sp = VG_(threads)[tid].arch.vex.guest_XSP;
   regs->misc.ARM64.x29 = VG_(threads)[tid].arch.vex.guest_X29;
   regs->misc.ARM64.x30 = VG_(threads)[tid].arch.vex.guest_X30;
#  elif defined(VGA_s390x)
   regs->r_pc = (ULong)VG_(threads)[tid].arch.vex.guest_IA;
   regs->r_sp = (ULong)VG_(threads)[tid].arch.vex.guest_SP;
   regs->misc.S390X.r_fp
      = VG_(threads)[tid].arch.vex.guest_r11;
   regs->misc.S390X.r_lr
      = VG_(threads)[tid].arch.vex.guest_r14;
#  elif defined(VGA_mips32)
   regs->r_pc = VG_(threads)[tid].arch.vex.guest_PC;
   regs->r_sp = VG_(threads)[tid].arch.vex.guest_r29;
   regs->misc.MIPS32.r30
      = VG_(threads)[tid].arch.vex.guest_r30;
   regs->misc.MIPS32.r31
      = VG_(threads)[tid].arch.vex.guest_r31;
   regs->misc.MIPS32.r28
      = VG_(threads)[tid].arch.vex.guest_r28;
#  elif defined(VGA_mips64)
   regs->r_pc = VG_(threads)[tid].arch.vex.guest_PC;
   regs->r_sp = VG_(threads)[tid].arch.vex.guest_r29;
   regs->misc.MIPS64.r30
      = VG_(threads)[tid].arch.vex.guest_r30;
   regs->misc.MIPS64.r31
      = VG_(threads)[tid].arch.vex.guest_r31;
   regs->misc.MIPS64.r28
      = VG_(threads)[tid].arch.vex.guest_r28;
#  else
#    error "Unknown arch"
#  endif
}

void
VG_(get_shadow_regs_area) ( ThreadId tid, 
                            /*DST*/UChar* dst,
                            /*SRC*/Int shadowNo, PtrdiffT offset, SizeT size )
{
   void*        src;
   ThreadState* tst;
   vg_assert(shadowNo == 0 || shadowNo == 1 || shadowNo == 2);
   vg_assert(VG_(is_valid_tid)(tid));
   // Bounds check
   vg_assert(0 <= offset && offset < sizeof(VexGuestArchState));
   vg_assert(offset + size <= sizeof(VexGuestArchState));
   // Copy
   tst = & VG_(threads)[tid];
   src = NULL;
   switch (shadowNo) {
      case 0: src = (void*)(((Addr)&(tst->arch.vex)) + offset); break;
      case 1: src = (void*)(((Addr)&(tst->arch.vex_shadow1)) + offset); break;
      case 2: src = (void*)(((Addr)&(tst->arch.vex_shadow2)) + offset); break;
   }
   tl_assert(src != NULL);
   VG_(memcpy)( dst, src, size);
}

void
VG_(set_shadow_regs_area) ( ThreadId tid, 
                            /*DST*/Int shadowNo, PtrdiffT offset, SizeT size,
                            /*SRC*/const UChar* src )
{
   void*        dst;
   ThreadState* tst;
   vg_assert(shadowNo == 0 || shadowNo == 1 || shadowNo == 2);
   vg_assert(VG_(is_valid_tid)(tid));
   // Bounds check
   vg_assert(0 <= offset && offset < sizeof(VexGuestArchState));
   vg_assert(offset + size <= sizeof(VexGuestArchState));
   // Copy
   tst = & VG_(threads)[tid];
   dst = NULL;
   switch (shadowNo) {
      case 0: dst = (void*)(((Addr)&(tst->arch.vex)) + offset); break;
      case 1: dst = (void*)(((Addr)&(tst->arch.vex_shadow1)) + offset); break;
      case 2: dst = (void*)(((Addr)&(tst->arch.vex_shadow2)) + offset); break;
   }
   tl_assert(dst != NULL);
   VG_(memcpy)( dst, src, size);
}


static void apply_to_GPs_of_tid(ThreadId tid, void (*f)(ThreadId,
                                                        const HChar*, Addr))
{
   VexGuestArchState* vex = &(VG_(get_ThreadState)(tid)->arch.vex);
   VG_(debugLog)(2, "machine", "apply_to_GPs_of_tid %d\n", tid);
#if defined(VGA_x86)
   (*f)(tid, "EAX", vex->guest_EAX);
   (*f)(tid, "ECX", vex->guest_ECX);
   (*f)(tid, "EDX", vex->guest_EDX);
   (*f)(tid, "EBX", vex->guest_EBX);
   (*f)(tid, "ESI", vex->guest_ESI);
   (*f)(tid, "EDI", vex->guest_EDI);
   (*f)(tid, "ESP", vex->guest_ESP);
   (*f)(tid, "EBP", vex->guest_EBP);
#elif defined(VGA_amd64)
   (*f)(tid, "RAX", vex->guest_RAX);
   (*f)(tid, "RCX", vex->guest_RCX);
   (*f)(tid, "RDX", vex->guest_RDX);
   (*f)(tid, "RBX", vex->guest_RBX);
   (*f)(tid, "RSI", vex->guest_RSI);
   (*f)(tid, "RDI", vex->guest_RDI);
   (*f)(tid, "RSP", vex->guest_RSP);
   (*f)(tid, "RBP", vex->guest_RBP);
   (*f)(tid, "R8" , vex->guest_R8 );
   (*f)(tid, "R9" , vex->guest_R9 );
   (*f)(tid, "R10", vex->guest_R10);
   (*f)(tid, "R11", vex->guest_R11);
   (*f)(tid, "R12", vex->guest_R12);
   (*f)(tid, "R13", vex->guest_R13);
   (*f)(tid, "R14", vex->guest_R14);
   (*f)(tid, "R15", vex->guest_R15);
#elif defined(VGA_ppc32) || defined(VGA_ppc64)
   (*f)(tid, "GPR0" , vex->guest_GPR0 );
   (*f)(tid, "GPR1" , vex->guest_GPR1 );
   (*f)(tid, "GPR2" , vex->guest_GPR2 );
   (*f)(tid, "GPR3" , vex->guest_GPR3 );
   (*f)(tid, "GPR4" , vex->guest_GPR4 );
   (*f)(tid, "GPR5" , vex->guest_GPR5 );
   (*f)(tid, "GPR6" , vex->guest_GPR6 );
   (*f)(tid, "GPR7" , vex->guest_GPR7 );
   (*f)(tid, "GPR8" , vex->guest_GPR8 );
   (*f)(tid, "GPR9" , vex->guest_GPR9 );
   (*f)(tid, "GPR10", vex->guest_GPR10);
   (*f)(tid, "GPR11", vex->guest_GPR11);
   (*f)(tid, "GPR12", vex->guest_GPR12);
   (*f)(tid, "GPR13", vex->guest_GPR13);
   (*f)(tid, "GPR14", vex->guest_GPR14);
   (*f)(tid, "GPR15", vex->guest_GPR15);
   (*f)(tid, "GPR16", vex->guest_GPR16);
   (*f)(tid, "GPR17", vex->guest_GPR17);
   (*f)(tid, "GPR18", vex->guest_GPR18);
   (*f)(tid, "GPR19", vex->guest_GPR19);
   (*f)(tid, "GPR20", vex->guest_GPR20);
   (*f)(tid, "GPR21", vex->guest_GPR21);
   (*f)(tid, "GPR22", vex->guest_GPR22);
   (*f)(tid, "GPR23", vex->guest_GPR23);
   (*f)(tid, "GPR24", vex->guest_GPR24);
   (*f)(tid, "GPR25", vex->guest_GPR25);
   (*f)(tid, "GPR26", vex->guest_GPR26);
   (*f)(tid, "GPR27", vex->guest_GPR27);
   (*f)(tid, "GPR28", vex->guest_GPR28);
   (*f)(tid, "GPR29", vex->guest_GPR29);
   (*f)(tid, "GPR30", vex->guest_GPR30);
   (*f)(tid, "GPR31", vex->guest_GPR31);
   (*f)(tid, "CTR"  , vex->guest_CTR  );
   (*f)(tid, "LR"   , vex->guest_LR   );
#elif defined(VGA_arm)
   (*f)(tid, "R0" , vex->guest_R0 );
   (*f)(tid, "R1" , vex->guest_R1 );
   (*f)(tid, "R2" , vex->guest_R2 );
   (*f)(tid, "R3" , vex->guest_R3 );
   (*f)(tid, "R4" , vex->guest_R4 );
   (*f)(tid, "R5" , vex->guest_R5 );
   (*f)(tid, "R6" , vex->guest_R6 );
   (*f)(tid, "R8" , vex->guest_R8 );
   (*f)(tid, "R9" , vex->guest_R9 );
   (*f)(tid, "R10", vex->guest_R10);
   (*f)(tid, "R11", vex->guest_R11);
   (*f)(tid, "R12", vex->guest_R12);
   (*f)(tid, "R13", vex->guest_R13);
   (*f)(tid, "R14", vex->guest_R14);
#elif defined(VGA_s390x)
   (*f)(tid, "r0" , vex->guest_r0 );
   (*f)(tid, "r1" , vex->guest_r1 );
   (*f)(tid, "r2" , vex->guest_r2 );
   (*f)(tid, "r3" , vex->guest_r3 );
   (*f)(tid, "r4" , vex->guest_r4 );
   (*f)(tid, "r5" , vex->guest_r5 );
   (*f)(tid, "r6" , vex->guest_r6 );
   (*f)(tid, "r7" , vex->guest_r7 );
   (*f)(tid, "r8" , vex->guest_r8 );
   (*f)(tid, "r9" , vex->guest_r9 );
   (*f)(tid, "r10", vex->guest_r10);
   (*f)(tid, "r11", vex->guest_r11);
   (*f)(tid, "r12", vex->guest_r12);
   (*f)(tid, "r13", vex->guest_r13);
   (*f)(tid, "r14", vex->guest_r14);
   (*f)(tid, "r15", vex->guest_r15);
#elif defined(VGA_mips32) || defined(VGA_mips64)
   (*f)(tid, "r0" , vex->guest_r0 );
   (*f)(tid, "r1" , vex->guest_r1 );
   (*f)(tid, "r2" , vex->guest_r2 );
   (*f)(tid, "r3" , vex->guest_r3 );
   (*f)(tid, "r4" , vex->guest_r4 );
   (*f)(tid, "r5" , vex->guest_r5 );
   (*f)(tid, "r6" , vex->guest_r6 );
   (*f)(tid, "r7" , vex->guest_r7 );
   (*f)(tid, "r8" , vex->guest_r8 );
   (*f)(tid, "r9" , vex->guest_r9 );
   (*f)(tid, "r10", vex->guest_r10);
   (*f)(tid, "r11", vex->guest_r11);
   (*f)(tid, "r12", vex->guest_r12);
   (*f)(tid, "r13", vex->guest_r13);
   (*f)(tid, "r14", vex->guest_r14);
   (*f)(tid, "r15", vex->guest_r15);
   (*f)(tid, "r16", vex->guest_r16);
   (*f)(tid, "r17", vex->guest_r17);
   (*f)(tid, "r18", vex->guest_r18);
   (*f)(tid, "r19", vex->guest_r19);
   (*f)(tid, "r20", vex->guest_r20);
   (*f)(tid, "r21", vex->guest_r21);
   (*f)(tid, "r22", vex->guest_r22);
   (*f)(tid, "r23", vex->guest_r23);
   (*f)(tid, "r24", vex->guest_r24);
   (*f)(tid, "r25", vex->guest_r25);
   (*f)(tid, "r26", vex->guest_r26);
   (*f)(tid, "r27", vex->guest_r27);
   (*f)(tid, "r28", vex->guest_r28);
   (*f)(tid, "r29", vex->guest_r29);
   (*f)(tid, "r30", vex->guest_r30);
   (*f)(tid, "r31", vex->guest_r31);
#elif defined(VGA_arm64)
   (*f)(tid, "x0" , vex->guest_X0 );
   (*f)(tid, "x1" , vex->guest_X1 );
   (*f)(tid, "x2" , vex->guest_X2 );
   (*f)(tid, "x3" , vex->guest_X3 );
   (*f)(tid, "x4" , vex->guest_X4 );
   (*f)(tid, "x5" , vex->guest_X5 );
   (*f)(tid, "x6" , vex->guest_X6 );
   (*f)(tid, "x7" , vex->guest_X7 );
   (*f)(tid, "x8" , vex->guest_X8 );
   (*f)(tid, "x9" , vex->guest_X9 );
   (*f)(tid, "x10", vex->guest_X10);
   (*f)(tid, "x11", vex->guest_X11);
   (*f)(tid, "x12", vex->guest_X12);
   (*f)(tid, "x13", vex->guest_X13);
   (*f)(tid, "x14", vex->guest_X14);
   (*f)(tid, "x15", vex->guest_X15);
   (*f)(tid, "x16", vex->guest_X16);
   (*f)(tid, "x17", vex->guest_X17);
   (*f)(tid, "x18", vex->guest_X18);
   (*f)(tid, "x19", vex->guest_X19);
   (*f)(tid, "x20", vex->guest_X20);
   (*f)(tid, "x21", vex->guest_X21);
   (*f)(tid, "x22", vex->guest_X22);
   (*f)(tid, "x23", vex->guest_X23);
   (*f)(tid, "x24", vex->guest_X24);
   (*f)(tid, "x25", vex->guest_X25);
   (*f)(tid, "x26", vex->guest_X26);
   (*f)(tid, "x27", vex->guest_X27);
   (*f)(tid, "x28", vex->guest_X28);
   (*f)(tid, "x29", vex->guest_X29);
   (*f)(tid, "x30", vex->guest_X30);
#else
#  error Unknown arch
#endif
}


void VG_(apply_to_GP_regs)(void (*f)(ThreadId, const HChar*, UWord))
{
   ThreadId tid;

   for (tid = 1; tid < VG_N_THREADS; tid++) {
      if (VG_(is_valid_tid)(tid)
          || VG_(threads)[tid].exitreason == VgSrc_ExitProcess) {
         // live thread or thread instructed to die by another thread that
         // called exit.
         apply_to_GPs_of_tid(tid, f);
      }
   }
}

void VG_(thread_stack_reset_iter)(/*OUT*/ThreadId* tid)
{
   *tid = (ThreadId)(-1);
}

Bool VG_(thread_stack_next)(/*MOD*/ThreadId* tid,
                            /*OUT*/Addr* stack_min, 
                            /*OUT*/Addr* stack_max)
{
   ThreadId i;
   for (i = (*tid)+1; i < VG_N_THREADS; i++) {
      if (i == VG_INVALID_THREADID)
         continue;
      if (VG_(threads)[i].status != VgTs_Empty) {
         *tid       = i;
         *stack_min = VG_(get_SP)(i);
         *stack_max = VG_(threads)[i].client_stack_highest_word;
         return True;
      }
   }
   return False;
}

Addr VG_(thread_get_stack_max)(ThreadId tid)
{
   vg_assert(0 <= tid && tid < VG_N_THREADS && tid != VG_INVALID_THREADID);
   vg_assert(VG_(threads)[tid].status != VgTs_Empty);
   return VG_(threads)[tid].client_stack_highest_word;
}

SizeT VG_(thread_get_stack_size)(ThreadId tid)
{
   vg_assert(0 <= tid && tid < VG_N_THREADS && tid != VG_INVALID_THREADID);
   vg_assert(VG_(threads)[tid].status != VgTs_Empty);
   return VG_(threads)[tid].client_stack_szB;
}

Addr VG_(thread_get_altstack_min)(ThreadId tid)
{
   vg_assert(0 <= tid && tid < VG_N_THREADS && tid != VG_INVALID_THREADID);
   vg_assert(VG_(threads)[tid].status != VgTs_Empty);
   return (Addr)VG_(threads)[tid].altstack.ss_sp;
}

SizeT VG_(thread_get_altstack_size)(ThreadId tid)
{
   vg_assert(0 <= tid && tid < VG_N_THREADS && tid != VG_INVALID_THREADID);
   vg_assert(VG_(threads)[tid].status != VgTs_Empty);
   return VG_(threads)[tid].altstack.ss_size;
}

//-------------------------------------------------------------
/* Details about the capabilities of the underlying (host) CPU.  These
   details are acquired by (1) enquiring with the CPU at startup, or
   (2) from the AT_SYSINFO entries the kernel gave us (ppc32 cache
   line size).  It's a bit nasty in the sense that there's no obvious
   way to stop uses of some of this info before it's ready to go.
   See pub_core_machine.h for more information about that.

   VG_(machine_get_hwcaps) may use signals (although it attempts to
   leave signal state unchanged) and therefore should only be
   called before m_main sets up the client's signal state.
*/

/* --------- State --------- */
static Bool hwcaps_done = False;

/* --- all archs --- */
static VexArch     va = VexArch_INVALID;
static VexArchInfo vai;

#if defined(VGA_x86)
UInt VG_(machine_x86_have_mxcsr) = 0;
#endif
#if defined(VGA_ppc32)
UInt VG_(machine_ppc32_has_FP)  = 0;
UInt VG_(machine_ppc32_has_VMX) = 0;
#endif
#if defined(VGA_ppc64)
ULong VG_(machine_ppc64_has_VMX) = 0;
#endif
#if defined(VGA_arm)
Int VG_(machine_arm_archlevel) = 4;
#endif


/* For hwcaps detection on ppc32/64, s390x, and arm we'll need to do SIGILL
   testing, so we need a VG_MINIMAL_JMP_BUF. */
#if defined(VGA_ppc32) || defined(VGA_ppc64) \
    || defined(VGA_arm) || defined(VGA_s390x) || defined(VGA_mips32)
#include "pub_core_libcsetjmp.h"
static VG_MINIMAL_JMP_BUF(env_unsup_insn);
static void handler_unsup_insn ( Int x ) {
   VG_MINIMAL_LONGJMP(env_unsup_insn);
}
#endif


/* Helper function for VG_(machine_get_hwcaps), assumes the SIGILL/etc
 * handlers are installed.  Determines the the sizes affected by dcbz
 * and dcbzl instructions and updates the given VexArchInfo structure
 * accordingly.
 *
 * Not very defensive: assumes that as long as the dcbz/dcbzl
 * instructions don't raise a SIGILL, that they will zero an aligned,
 * contiguous block of memory of a sensible size. */
#if defined(VGA_ppc32) || defined(VGA_ppc64)
static void find_ppc_dcbz_sz(VexArchInfo *arch_info)
{
   Int dcbz_szB = 0;
   Int dcbzl_szB;
#  define MAX_DCBZL_SZB (128) /* largest known effect of dcbzl */
   char test_block[4*MAX_DCBZL_SZB];
   char *aligned = test_block;
   Int i;

   /* round up to next max block size, assumes MAX_DCBZL_SZB is pof2 */
   aligned = (char *)(((HWord)aligned + MAX_DCBZL_SZB) & ~(MAX_DCBZL_SZB - 1));
   vg_assert((aligned + MAX_DCBZL_SZB) <= &test_block[sizeof(test_block)]);

   /* dcbz often clears 32B, although sometimes whatever the native cache
    * block size is */
   VG_(memset)(test_block, 0xff, sizeof(test_block));
   __asm__ __volatile__("dcbz 0,%0"
                        : /*out*/
                        : "r" (aligned) /*in*/
                        : "memory" /*clobber*/);
   for (dcbz_szB = 0, i = 0; i < sizeof(test_block); ++i) {
      if (!test_block[i])
         ++dcbz_szB;
   }
   vg_assert(dcbz_szB == 16 || dcbz_szB == 32 || dcbz_szB == 64 || dcbz_szB == 128);

   /* dcbzl clears 128B on G5/PPC970, and usually 32B on other platforms */
   if (VG_MINIMAL_SETJMP(env_unsup_insn)) {
      dcbzl_szB = 0; /* indicates unsupported */
   }
   else {
      VG_(memset)(test_block, 0xff, sizeof(test_block));
      /* some older assemblers won't understand the dcbzl instruction
       * variant, so we directly emit the instruction ourselves */
      __asm__ __volatile__("mr 9, %0 ; .long 0x7C204FEC" /*dcbzl 0,9*/
                           : /*out*/
                           : "r" (aligned) /*in*/
                           : "memory", "r9" /*clobber*/);
      for (dcbzl_szB = 0, i = 0; i < sizeof(test_block); ++i) {
         if (!test_block[i])
            ++dcbzl_szB;
      }
      vg_assert(dcbzl_szB == 16 || dcbzl_szB == 32 || dcbzl_szB == 64 || dcbzl_szB == 128);
   }

   arch_info->ppc_dcbz_szB  = dcbz_szB;
   arch_info->ppc_dcbzl_szB = dcbzl_szB;

   VG_(debugLog)(1, "machine", "dcbz_szB=%d dcbzl_szB=%d\n",
                 dcbz_szB, dcbzl_szB);
#  undef MAX_DCBZL_SZB
}
#endif /* defined(VGA_ppc32) || defined(VGA_ppc64) */

#ifdef VGA_s390x

/* Read /proc/cpuinfo. Look for lines like these

   processor 0: version = FF,  identification = 0117C9,  machine = 2064

   and return the machine model. If the machine model could not be determined
   or it is an unknown model, return VEX_S390X_MODEL_UNKNOWN. */

static UInt VG_(get_machine_model)(void)
{
   static struct model_map {
      HChar name[5];
      UInt  id;
   } model_map[] = {
      { "2064", VEX_S390X_MODEL_Z900 },
      { "2066", VEX_S390X_MODEL_Z800 },
      { "2084", VEX_S390X_MODEL_Z990 },
      { "2086", VEX_S390X_MODEL_Z890 },
      { "2094", VEX_S390X_MODEL_Z9_EC },
      { "2096", VEX_S390X_MODEL_Z9_BC },
      { "2097", VEX_S390X_MODEL_Z10_EC },
      { "2098", VEX_S390X_MODEL_Z10_BC },
      { "2817", VEX_S390X_MODEL_Z196 },
      { "2818", VEX_S390X_MODEL_Z114 },
      { "2827", VEX_S390X_MODEL_ZEC12 },
      { "2828", VEX_S390X_MODEL_ZBC12 },
   };

   Int    model, n, fh;
   SysRes fd;
   SizeT  num_bytes, file_buf_size;
   HChar *p, *m, *model_name, *file_buf;

   /* Slurp contents of /proc/cpuinfo into FILE_BUF */
   fd = VG_(open)( "/proc/cpuinfo", 0, VKI_S_IRUSR );
   if ( sr_isError(fd) ) return VEX_S390X_MODEL_UNKNOWN;

   fh  = sr_Res(fd);

   /* Determine the size of /proc/cpuinfo.
      Work around broken-ness in /proc file system implementation.
      fstat returns a zero size for /proc/cpuinfo although it is
      claimed to be a regular file. */
   num_bytes = 0;
   file_buf_size = 1000;
   file_buf = VG_(malloc)("cpuinfo", file_buf_size + 1);
   while (42) {
      n = VG_(read)(fh, file_buf, file_buf_size);
      if (n < 0) break;

      num_bytes += n;
      if (n < file_buf_size) break;  /* reached EOF */
   }

   if (n < 0) num_bytes = 0;   /* read error; ignore contents */

   if (num_bytes > file_buf_size) {
      VG_(free)( file_buf );
      VG_(lseek)( fh, 0, VKI_SEEK_SET );
      file_buf = VG_(malloc)( "cpuinfo", num_bytes + 1 );
      n = VG_(read)( fh, file_buf, num_bytes );
      if (n < 0) num_bytes = 0;
   }

   file_buf[num_bytes] = '\0';
   VG_(close)(fh);

   /* Parse file */
   model = VEX_S390X_MODEL_UNKNOWN;
   for (p = file_buf; *p; ++p) {
      /* Beginning of line */
     if (VG_(strncmp)( p, "processor", sizeof "processor" - 1 ) != 0) continue;

     m = VG_(strstr)( p, "machine" );
     if (m == NULL) continue;

     p = m + sizeof "machine" - 1;
     while ( VG_(isspace)( *p ) || *p == '=') {
       if (*p == '\n') goto next_line;
       ++p;
     }

     model_name = p;
     for (n = 0; n < sizeof model_map / sizeof model_map[0]; ++n) {
       struct model_map *mm = model_map + n;
       SizeT len = VG_(strlen)( mm->name );
       if ( VG_(strncmp)( mm->name, model_name, len ) == 0 &&
            VG_(isspace)( model_name[len] )) {
         if (mm->id < model) model = mm->id;
         p = model_name + len;
         break;
       }
     }
     /* Skip until end-of-line */
     while (*p != '\n')
       ++p;
   next_line: ;
   }

   VG_(free)( file_buf );
   VG_(debugLog)(1, "machine", "model = %s\n",
                 model == VEX_S390X_MODEL_UNKNOWN ? "UNKNOWN"
                                                  : model_map[model].name);
   return model;
}

#endif /* VGA_s390x */

#if defined(VGA_mips32) || defined(VGA_mips64)

/* Read /proc/cpuinfo and return the machine model. */
static UInt VG_(get_machine_model)(void)
{
   const char *search_MIPS_str = "MIPS";
   const char *search_Broadcom_str = "Broadcom";
   const char *search_Netlogic_str = "Netlogic";
   const char *search_Cavium_str= "Cavium";
   Int    n, fh;
   SysRes fd;
   SizeT  num_bytes, file_buf_size;
   HChar  *file_buf;

   /* Slurp contents of /proc/cpuinfo into FILE_BUF */
   fd = VG_(open)( "/proc/cpuinfo", 0, VKI_S_IRUSR );
   if ( sr_isError(fd) ) return -1;

   fh  = sr_Res(fd);

   /* Determine the size of /proc/cpuinfo.
      Work around broken-ness in /proc file system implementation.
      fstat returns a zero size for /proc/cpuinfo although it is
      claimed to be a regular file. */
   num_bytes = 0;
   file_buf_size = 1000;
   file_buf = VG_(malloc)("cpuinfo", file_buf_size + 1);
   while (42) {
      n = VG_(read)(fh, file_buf, file_buf_size);
      if (n < 0) break;

      num_bytes += n;
      if (n < file_buf_size) break;  /* reached EOF */
   }

   if (n < 0) num_bytes = 0;   /* read error; ignore contents */

   if (num_bytes > file_buf_size) {
      VG_(free)( file_buf );
      VG_(lseek)( fh, 0, VKI_SEEK_SET );
      file_buf = VG_(malloc)( "cpuinfo", num_bytes + 1 );
      n = VG_(read)( fh, file_buf, num_bytes );
      if (n < 0) num_bytes = 0;
   }

   file_buf[num_bytes] = '\0';
   VG_(close)(fh);

   /* Parse file */
   if (VG_(strstr) (file_buf, search_Broadcom_str) != NULL)
       return VEX_PRID_COMP_BROADCOM;
   if (VG_(strstr) (file_buf, search_Netlogic_str) != NULL)
       return VEX_PRID_COMP_NETLOGIC;
   if (VG_(strstr)(file_buf, search_Cavium_str) != NULL)
       return VEX_PRID_COMP_CAVIUM;
   if (VG_(strstr) (file_buf, search_MIPS_str) != NULL)
       return VEX_PRID_COMP_MIPS;

   /* Did not find string in the proc file. */
   return -1;
}

#endif

/* Determine what insn set and insn set variant the host has, and
   record it.  To be called once at system startup.  Returns False if
   this a CPU incapable of running Valgrind.
   Also determine information about the caches on this host. */

Bool VG_(machine_get_hwcaps)( void )
{
   vg_assert(hwcaps_done == False);
   hwcaps_done = True;

   // Whack default settings into vai, so that we only need to fill in
   // any interesting bits.
   LibVEX_default_VexArchInfo(&vai);

#if defined(VGA_x86)
   { Bool have_sse1, have_sse2, have_cx8, have_lzcnt, have_mmxext;
     UInt eax, ebx, ecx, edx, max_extended;
     HChar vstr[13];
     vstr[0] = 0;

     if (!VG_(has_cpuid)())
        /* we can't do cpuid at all.  Give up. */
        return False;

     VG_(cpuid)(0, 0, &eax, &ebx, &ecx, &edx);
     if (eax < 1)
        /* we can't ask for cpuid(x) for x > 0.  Give up. */
        return False;

     /* Get processor ID string, and max basic/extended index
        values. */
     VG_(memcpy)(&vstr[0], &ebx, 4);
     VG_(memcpy)(&vstr[4], &edx, 4);
     VG_(memcpy)(&vstr[8], &ecx, 4);
     vstr[12] = 0;

     VG_(cpuid)(0x80000000, 0, &eax, &ebx, &ecx, &edx);
     max_extended = eax;

     /* get capabilities bits into edx */
     VG_(cpuid)(1, 0, &eax, &ebx, &ecx, &edx);

     have_sse1 = (edx & (1<<25)) != 0; /* True => have sse insns */
     have_sse2 = (edx & (1<<26)) != 0; /* True => have sse2 insns */

     /* cmpxchg8b is a minimum requirement now; if we don't have it we
        must simply give up.  But all CPUs since Pentium-I have it, so
        that doesn't seem like much of a restriction. */
     have_cx8 = (edx & (1<<8)) != 0; /* True => have cmpxchg8b */
     if (!have_cx8)
        return False;

     /* Figure out if this is an AMD that can do MMXEXT. */
     have_mmxext = False;
     if (0 == VG_(strcmp)(vstr, "AuthenticAMD")
         && max_extended >= 0x80000001) {
        VG_(cpuid)(0x80000001, 0, &eax, &ebx, &ecx, &edx);
        /* Some older AMD processors support a sse1 subset (Integer SSE). */
        have_mmxext = !have_sse1 && ((edx & (1<<22)) != 0);
     }

     /* Figure out if this is an AMD or Intel that can do LZCNT. */
     have_lzcnt = False;
     if ((0 == VG_(strcmp)(vstr, "AuthenticAMD")
          || 0 == VG_(strcmp)(vstr, "GenuineIntel"))
         && max_extended >= 0x80000001) {
        VG_(cpuid)(0x80000001, 0, &eax, &ebx, &ecx, &edx);
        have_lzcnt = (ecx & (1<<5)) != 0; /* True => have LZCNT */
     }

     /* Intel processors don't define the mmxext extension, but since it
        is just a sse1 subset always define it when we have sse1. */
     if (have_sse1)
        have_mmxext = True;

     va = VexArchX86;
     if (have_sse2 && have_sse1 && have_mmxext) {
        vai.hwcaps  = VEX_HWCAPS_X86_MMXEXT;
        vai.hwcaps |= VEX_HWCAPS_X86_SSE1;
        vai.hwcaps |= VEX_HWCAPS_X86_SSE2;
        if (have_lzcnt)
           vai.hwcaps |= VEX_HWCAPS_X86_LZCNT;
        VG_(machine_x86_have_mxcsr) = 1;
     } else if (have_sse1 && have_mmxext) {
        vai.hwcaps  = VEX_HWCAPS_X86_MMXEXT;
        vai.hwcaps |= VEX_HWCAPS_X86_SSE1;
        VG_(machine_x86_have_mxcsr) = 1;
     } else if (have_mmxext) {
        vai.hwcaps  = VEX_HWCAPS_X86_MMXEXT; /*integer only sse1 subset*/
        VG_(machine_x86_have_mxcsr) = 0;
     } else {
       vai.hwcaps = 0; /*baseline - no sse at all*/
       VG_(machine_x86_have_mxcsr) = 0;
     }

     VG_(machine_get_cache_info)(&vai);

     return True;
   }

#elif defined(VGA_amd64)
   { Bool have_sse3, have_cx8, have_cx16;
     Bool have_lzcnt, have_avx, have_bmi, have_avx2;
     Bool have_rdtscp;
     UInt eax, ebx, ecx, edx, max_basic, max_extended;
     HChar vstr[13];
     vstr[0] = 0;

     if (!VG_(has_cpuid)())
        /* we can't do cpuid at all.  Give up. */
        return False;

     VG_(cpuid)(0, 0, &eax, &ebx, &ecx, &edx);
     max_basic = eax;
     if (max_basic < 1)
        /* we can't ask for cpuid(x) for x > 0.  Give up. */
        return False;

     /* Get processor ID string, and max basic/extended index
        values. */
     VG_(memcpy)(&vstr[0], &ebx, 4);
     VG_(memcpy)(&vstr[4], &edx, 4);
     VG_(memcpy)(&vstr[8], &ecx, 4);
     vstr[12] = 0;

     VG_(cpuid)(0x80000000, 0, &eax, &ebx, &ecx, &edx);
     max_extended = eax;

     /* get capabilities bits into edx */
     VG_(cpuid)(1, 0, &eax, &ebx, &ecx, &edx);

     // we assume that SSE1 and SSE2 are available by default
     have_sse3 = (ecx & (1<<0)) != 0;  /* True => have sse3 insns */
     // ssse3   is ecx:9
     // sse41   is ecx:19
     // sse42   is ecx:20

     // osxsave is ecx:27
     // avx     is ecx:28
     // fma     is ecx:12
     have_avx = False;
     /* have_fma = False; */
     if ( (ecx & ((1<<27)|(1<<28))) == ((1<<27)|(1<<28)) ) {
        /* processor supports AVX instructions and XGETBV is enabled
           by OS */
        ULong w;
        __asm__ __volatile__("movq $0,%%rcx ; "
                             ".byte 0x0F,0x01,0xD0 ; " /* xgetbv */
                             "movq %%rax,%0"
                             :/*OUT*/"=r"(w) :/*IN*/
                             :/*TRASH*/"rdx","rcx");
        if ((w & 6) == 6) {
           /* OS has enabled both XMM and YMM state support */
           have_avx = True;
           /* have_fma = (ecx & (1<<12)) != 0; */
           /* have_fma: Probably correct, but gcc complains due to
              unusedness. &*/
        }
     }

     /* cmpxchg8b is a minimum requirement now; if we don't have it we
        must simply give up.  But all CPUs since Pentium-I have it, so
        that doesn't seem like much of a restriction. */
     have_cx8 = (edx & (1<<8)) != 0; /* True => have cmpxchg8b */
     if (!have_cx8)
        return False;

     /* on amd64 we tolerate older cpus, which don't have cmpxchg16b */
     have_cx16 = (ecx & (1<<13)) != 0; /* True => have cmpxchg16b */

     /* Figure out if this CPU can do LZCNT. */
     have_lzcnt = False;
     if (max_extended >= 0x80000001) {
        VG_(cpuid)(0x80000001, 0, &eax, &ebx, &ecx, &edx);
        have_lzcnt = (ecx & (1<<5)) != 0; /* True => have LZCNT */
     }

     /* Can we do RDTSCP? */
     have_rdtscp = False;
     if (max_extended >= 0x80000001) {
        VG_(cpuid)(0x80000001, 0, &eax, &ebx, &ecx, &edx);
        have_rdtscp = (edx & (1<<27)) != 0; /* True => have RDTSVCP */
     }

     /* Check for BMI1 and AVX2. If we have AVX1 (plus OS support). */
     have_bmi = False;
     have_avx2 = False;
     if (have_avx && max_basic >= 7) {
        VG_(cpuid)(7, 0, &eax, &ebx, &ecx, &edx);
        have_bmi = (ebx & (1<<3)) != 0; /* True => have BMI1 */
        have_avx2 = (ebx & (1<<5)) != 0; /* True => have AVX2 */
     }

     va         = VexArchAMD64;
     vai.hwcaps = (have_sse3   ? VEX_HWCAPS_AMD64_SSE3   : 0)
                | (have_cx16   ? VEX_HWCAPS_AMD64_CX16   : 0)
                | (have_lzcnt  ? VEX_HWCAPS_AMD64_LZCNT  : 0)
                | (have_avx    ? VEX_HWCAPS_AMD64_AVX    : 0)
                | (have_bmi    ? VEX_HWCAPS_AMD64_BMI    : 0)
                | (have_avx2   ? VEX_HWCAPS_AMD64_AVX2   : 0)
                | (have_rdtscp ? VEX_HWCAPS_AMD64_RDTSCP : 0);

     VG_(machine_get_cache_info)(&vai);

     return True;
   }

#elif defined(VGA_ppc32)
   {
     /* Find out which subset of the ppc32 instruction set is supported by
        verifying whether various ppc32 instructions generate a SIGILL
        or a SIGFPE. An alternative approach is to check the AT_HWCAP and
        AT_PLATFORM entries in the ELF auxiliary table -- see also
        the_iifii.client_auxv in m_main.c.
      */
     vki_sigset_t          saved_set, tmp_set;
     vki_sigaction_fromK_t saved_sigill_act, saved_sigfpe_act;
     vki_sigaction_toK_t     tmp_sigill_act,   tmp_sigfpe_act;

     volatile Bool have_F, have_V, have_FX, have_GX, have_VX, have_DFP;
     volatile Bool have_isa_2_07;
     Int r;

     /* This is a kludge.  Really we ought to back-convert saved_act
        into a toK_t using VG_(convert_sigaction_fromK_to_toK), but
        since that's a no-op on all ppc32 platforms so far supported,
        it's not worth the typing effort.  At least include most basic
        sanity check: */
     vg_assert(sizeof(vki_sigaction_fromK_t) == sizeof(vki_sigaction_toK_t));

     VG_(sigemptyset)(&tmp_set);
     VG_(sigaddset)(&tmp_set, VKI_SIGILL);
     VG_(sigaddset)(&tmp_set, VKI_SIGFPE);

     r = VG_(sigprocmask)(VKI_SIG_UNBLOCK, &tmp_set, &saved_set);
     vg_assert(r == 0);

     r = VG_(sigaction)(VKI_SIGILL, NULL, &saved_sigill_act);
     vg_assert(r == 0);
     tmp_sigill_act = saved_sigill_act;

     r = VG_(sigaction)(VKI_SIGFPE, NULL, &saved_sigfpe_act);
     vg_assert(r == 0);
     tmp_sigfpe_act = saved_sigfpe_act;

     /* NODEFER: signal handler does not return (from the kernel's point of
        view), hence if it is to successfully catch a signal more than once,
        we need the NODEFER flag. */
     tmp_sigill_act.sa_flags &= ~VKI_SA_RESETHAND;
     tmp_sigill_act.sa_flags &= ~VKI_SA_SIGINFO;
     tmp_sigill_act.sa_flags |=  VKI_SA_NODEFER;
     tmp_sigill_act.ksa_handler = handler_unsup_insn;
     r = VG_(sigaction)(VKI_SIGILL, &tmp_sigill_act, NULL);
     vg_assert(r == 0);

     tmp_sigfpe_act.sa_flags &= ~VKI_SA_RESETHAND;
     tmp_sigfpe_act.sa_flags &= ~VKI_SA_SIGINFO;
     tmp_sigfpe_act.sa_flags |=  VKI_SA_NODEFER;
     tmp_sigfpe_act.ksa_handler = handler_unsup_insn;
     r = VG_(sigaction)(VKI_SIGFPE, &tmp_sigfpe_act, NULL);
     vg_assert(r == 0);

     /* standard FP insns */
     have_F = True;
     if (VG_MINIMAL_SETJMP(env_unsup_insn)) {
        have_F = False;
     } else {
        __asm__ __volatile__(".long 0xFC000090"); /*fmr 0,0 */
     }

     /* Altivec insns */
     have_V = True;
     if (VG_MINIMAL_SETJMP(env_unsup_insn)) {
        have_V = False;
     } else {
        /* Unfortunately some older assemblers don't speak Altivec (or
           choose not to), so to be safe we directly emit the 32-bit
           word corresponding to "vor 0,0,0".  This fixes a build
           problem that happens on Debian 3.1 (ppc32), and probably
           various other places. */
        __asm__ __volatile__(".long 0x10000484"); /*vor 0,0,0*/
     }

     /* General-Purpose optional (fsqrt, fsqrts) */
     have_FX = True;
     if (VG_MINIMAL_SETJMP(env_unsup_insn)) {
        have_FX = False;
     } else {
        __asm__ __volatile__(".long 0xFC00002C"); /*fsqrt 0,0 */
     }

     /* Graphics optional (stfiwx, fres, frsqrte, fsel) */
     have_GX = True;
     if (VG_MINIMAL_SETJMP(env_unsup_insn)) {
        have_GX = False;
     } else {
        __asm__ __volatile__(".long 0xFC000034"); /* frsqrte 0,0 */
     }

     /* VSX support implies Power ISA 2.06 */
     have_VX = True;
     if (VG_MINIMAL_SETJMP(env_unsup_insn)) {
        have_VX = False;
     } else {
        __asm__ __volatile__(".long 0xf0000564"); /* xsabsdp XT,XB */
     }

     /* Check for Decimal Floating Point (DFP) support. */
     have_DFP = True;
     if (VG_MINIMAL_SETJMP(env_unsup_insn)) {
        have_DFP = False;
     } else {
        __asm__ __volatile__(".long 0xee4e8005"); /* dadd  FRT,FRA, FRB */
     }

     /* Check for ISA 2.07 support. */
     have_isa_2_07 = True;
     if (VG_MINIMAL_SETJMP(env_unsup_insn)) {
        have_isa_2_07 = False;
     } else {
        __asm__ __volatile__(".long 0x7c000166"); /* mtvsrd XT,RA */
     }

     /* determine dcbz/dcbzl sizes while we still have the signal
      * handlers registered */
     find_ppc_dcbz_sz(&vai);

     r = VG_(sigaction)(VKI_SIGILL, &saved_sigill_act, NULL);
     vg_assert(r == 0);
     r = VG_(sigaction)(VKI_SIGFPE, &saved_sigfpe_act, NULL);
     vg_assert(r == 0);
     r = VG_(sigprocmask)(VKI_SIG_SETMASK, &saved_set, NULL);
     vg_assert(r == 0);
     VG_(debugLog)(1, "machine", "F %d V %d FX %d GX %d VX %d DFP %d ISA2.07 %d\n",
                    (Int)have_F, (Int)have_V, (Int)have_FX,
                    (Int)have_GX, (Int)have_VX, (Int)have_DFP,
                    (Int)have_isa_2_07);
     /* Make FP a prerequisite for VMX (bogusly so), and for FX and GX. */
     if (have_V && !have_F)
        have_V = False;
     if (have_FX && !have_F)
        have_FX = False;
     if (have_GX && !have_F)
        have_GX = False;

     VG_(machine_ppc32_has_FP)  = have_F ? 1 : 0;
     VG_(machine_ppc32_has_VMX) = have_V ? 1 : 0;

     va = VexArchPPC32;

     vai.hwcaps = 0;
     if (have_F)  vai.hwcaps |= VEX_HWCAPS_PPC32_F;
     if (have_V)  vai.hwcaps |= VEX_HWCAPS_PPC32_V;
     if (have_FX) vai.hwcaps |= VEX_HWCAPS_PPC32_FX;
     if (have_GX) vai.hwcaps |= VEX_HWCAPS_PPC32_GX;
     if (have_VX) vai.hwcaps |= VEX_HWCAPS_PPC32_VX;
     if (have_DFP) vai.hwcaps |= VEX_HWCAPS_PPC32_DFP;
     if (have_isa_2_07) vai.hwcaps |= VEX_HWCAPS_PPC32_ISA2_07;

     VG_(machine_get_cache_info)(&vai);

     /* But we're not done yet: VG_(machine_ppc32_set_clszB) must be
        called before we're ready to go. */
     return True;
   }

#elif defined(VGA_ppc64)
   {
     /* Same instruction set detection algorithm as for ppc32. */
     vki_sigset_t          saved_set, tmp_set;
     vki_sigaction_fromK_t saved_sigill_act, saved_sigfpe_act;
     vki_sigaction_toK_t     tmp_sigill_act,   tmp_sigfpe_act;

     volatile Bool have_F, have_V, have_FX, have_GX, have_VX, have_DFP;
     volatile Bool have_isa_2_07;
     Int r;

     /* This is a kludge.  Really we ought to back-convert saved_act
        into a toK_t using VG_(convert_sigaction_fromK_to_toK), but
        since that's a no-op on all ppc64 platforms so far supported,
        it's not worth the typing effort.  At least include most basic
        sanity check: */
     vg_assert(sizeof(vki_sigaction_fromK_t) == sizeof(vki_sigaction_toK_t));

     VG_(sigemptyset)(&tmp_set);
     VG_(sigaddset)(&tmp_set, VKI_SIGILL);
     VG_(sigaddset)(&tmp_set, VKI_SIGFPE);

     r = VG_(sigprocmask)(VKI_SIG_UNBLOCK, &tmp_set, &saved_set);
     vg_assert(r == 0);

     r = VG_(sigaction)(VKI_SIGILL, NULL, &saved_sigill_act);
     vg_assert(r == 0);
     tmp_sigill_act = saved_sigill_act;

     VG_(sigaction)(VKI_SIGFPE, NULL, &saved_sigfpe_act);
     tmp_sigfpe_act = saved_sigfpe_act;

     /* NODEFER: signal handler does not return (from the kernel's point of
        view), hence if it is to successfully catch a signal more than once,
        we need the NODEFER flag. */
     tmp_sigill_act.sa_flags &= ~VKI_SA_RESETHAND;
     tmp_sigill_act.sa_flags &= ~VKI_SA_SIGINFO;
     tmp_sigill_act.sa_flags |=  VKI_SA_NODEFER;
     tmp_sigill_act.ksa_handler = handler_unsup_insn;
     VG_(sigaction)(VKI_SIGILL, &tmp_sigill_act, NULL);

     tmp_sigfpe_act.sa_flags &= ~VKI_SA_RESETHAND;
     tmp_sigfpe_act.sa_flags &= ~VKI_SA_SIGINFO;
     tmp_sigfpe_act.sa_flags |=  VKI_SA_NODEFER;
     tmp_sigfpe_act.ksa_handler = handler_unsup_insn;
     VG_(sigaction)(VKI_SIGFPE, &tmp_sigfpe_act, NULL);

     /* standard FP insns */
     have_F = True;
     if (VG_MINIMAL_SETJMP(env_unsup_insn)) {
        have_F = False;
     } else {
        __asm__ __volatile__("fmr 0,0");
     }

     /* Altivec insns */
     have_V = True;
     if (VG_MINIMAL_SETJMP(env_unsup_insn)) {
        have_V = False;
     } else {
        __asm__ __volatile__(".long 0x10000484"); /*vor 0,0,0*/
     }

     /* General-Purpose optional (fsqrt, fsqrts) */
     have_FX = True;
     if (VG_MINIMAL_SETJMP(env_unsup_insn)) {
        have_FX = False;
     } else {
        __asm__ __volatile__(".long 0xFC00002C"); /*fsqrt 0,0*/
     }

     /* Graphics optional (stfiwx, fres, frsqrte, fsel) */
     have_GX = True;
     if (VG_MINIMAL_SETJMP(env_unsup_insn)) {
        have_GX = False;
     } else {
        __asm__ __volatile__(".long 0xFC000034"); /*frsqrte 0,0*/
     }

     /* VSX support implies Power ISA 2.06 */
     have_VX = True;
     if (VG_MINIMAL_SETJMP(env_unsup_insn)) {
        have_VX = False;
     } else {
        __asm__ __volatile__(".long 0xf0000564"); /* xsabsdp XT,XB */
     }

     /* Check for Decimal Floating Point (DFP) support. */
     have_DFP = True;
     if (VG_MINIMAL_SETJMP(env_unsup_insn)) {
        have_DFP = False;
     } else {
        __asm__ __volatile__(".long 0xee4e8005"); /* dadd  FRT,FRA, FRB */
     }

     /* Check for ISA 2.07 support. */
     have_isa_2_07 = True;
     if (VG_MINIMAL_SETJMP(env_unsup_insn)) {
        have_isa_2_07 = False;
     } else {
        __asm__ __volatile__(".long 0x7c000166"); /* mtvsrd XT,RA */
     }

     /* determine dcbz/dcbzl sizes while we still have the signal
      * handlers registered */
     find_ppc_dcbz_sz(&vai);

     VG_(sigaction)(VKI_SIGILL, &saved_sigill_act, NULL);
     VG_(sigaction)(VKI_SIGFPE, &saved_sigfpe_act, NULL);
     VG_(sigprocmask)(VKI_SIG_SETMASK, &saved_set, NULL);
     VG_(debugLog)(1, "machine", "F %d V %d FX %d GX %d VX %d DFP %d ISA2.07 %d\n",
                    (Int)have_F, (Int)have_V, (Int)have_FX,
                    (Int)have_GX, (Int)have_VX, (Int)have_DFP,
                    (Int)have_isa_2_07);
     /* on ppc64, if we don't even have FP, just give up. */
     if (!have_F)
        return False;

     VG_(machine_ppc64_has_VMX) = have_V ? 1 : 0;

     va = VexArchPPC64;

     vai.hwcaps = 0;
     if (have_V)  vai.hwcaps |= VEX_HWCAPS_PPC64_V;
     if (have_FX) vai.hwcaps |= VEX_HWCAPS_PPC64_FX;
     if (have_GX) vai.hwcaps |= VEX_HWCAPS_PPC64_GX;
     if (have_VX) vai.hwcaps |= VEX_HWCAPS_PPC64_VX;
     if (have_DFP) vai.hwcaps |= VEX_HWCAPS_PPC64_DFP;
     if (have_isa_2_07) vai.hwcaps |= VEX_HWCAPS_PPC64_ISA2_07;

     VG_(machine_get_cache_info)(&vai);

     /* But we're not done yet: VG_(machine_ppc64_set_clszB) must be
        called before we're ready to go. */
     return True;
   }

#elif defined(VGA_s390x)

#  include "libvex_s390x_common.h"

   {
     /* Instruction set detection code borrowed from ppc above. */
     vki_sigset_t          saved_set, tmp_set;
     vki_sigaction_fromK_t saved_sigill_act;
     vki_sigaction_toK_t     tmp_sigill_act;

     volatile Bool have_LDISP, have_STFLE;
     Int i, r, model;

     /* If the model is "unknown" don't treat this as an error. Assume
        this is a brand-new machine model for which we don't have the 
        identification yet. Keeping fingers crossed. */
     model = VG_(get_machine_model)();

     /* Unblock SIGILL and stash away the old action for that signal */
     VG_(sigemptyset)(&tmp_set);
     VG_(sigaddset)(&tmp_set, VKI_SIGILL);

     r = VG_(sigprocmask)(VKI_SIG_UNBLOCK, &tmp_set, &saved_set);
     vg_assert(r == 0);

     r = VG_(sigaction)(VKI_SIGILL, NULL, &saved_sigill_act);
     vg_assert(r == 0);
     tmp_sigill_act = saved_sigill_act;

     /* NODEFER: signal handler does not return (from the kernel's point of
        view), hence if it is to successfully catch a signal more than once,
        we need the NODEFER flag. */
     tmp_sigill_act.sa_flags &= ~VKI_SA_RESETHAND;
     tmp_sigill_act.sa_flags &= ~VKI_SA_SIGINFO;
     tmp_sigill_act.sa_flags |=  VKI_SA_NODEFER;
     tmp_sigill_act.ksa_handler = handler_unsup_insn;
     VG_(sigaction)(VKI_SIGILL, &tmp_sigill_act, NULL);

     /* Determine hwcaps. Note, we cannot use the stfle insn because it
        is not supported on z900. */

     have_LDISP = True;
     if (VG_MINIMAL_SETJMP(env_unsup_insn)) {
        have_LDISP = False;
     } else {
       /* BASR loads the address of the next insn into r1. Needed to avoid
          a segfault in XY. */
        __asm__ __volatile__("basr %%r1,%%r0\n\t"
                             ".long  0xe3001000\n\t"  /* XY  0,0(%r1) */
                             ".short 0x0057" : : : "r0", "r1", "cc", "memory");
     }

     /* Check availability og STFLE. If available store facility bits
        in hoststfle. */
     ULong hoststfle[S390_NUM_FACILITY_DW];

     for (i = 0; i < S390_NUM_FACILITY_DW; ++i)
        hoststfle[i] = 0;

     have_STFLE = True;
     if (VG_MINIMAL_SETJMP(env_unsup_insn)) {
        have_STFLE = False;
     } else {
         register ULong reg0 asm("0") = S390_NUM_FACILITY_DW - 1;

         __asm__ __volatile__(" .insn s,0xb2b00000,%0\n"   /* stfle */
                              : "=m" (hoststfle), "+d"(reg0)
                              : : "cc", "memory");
     }

     /* Restore signals */
     r = VG_(sigaction)(VKI_SIGILL, &saved_sigill_act, NULL);
     vg_assert(r == 0);
     r = VG_(sigprocmask)(VKI_SIG_SETMASK, &saved_set, NULL);
     vg_assert(r == 0);
     va = VexArchS390X;

     vai.hwcaps = model;
     if (have_STFLE) vai.hwcaps |= VEX_HWCAPS_S390X_STFLE;
     if (have_LDISP) {
        /* Use long displacement only on machines >= z990. For all other
           machines it is millicoded and therefore slow. */
        if (model >= VEX_S390X_MODEL_Z990)
           vai.hwcaps |= VEX_HWCAPS_S390X_LDISP;
     }

     /* Detect presence of certain facilities using the STFLE insn.
        Note, that these facilities were introduced at the same time or later
        as STFLE, so the absence of STLFE implies the absence of the facility
        we're trying to detect. */
     struct fac_hwcaps_map {
        UInt installed;
        UInt facility_bit;
        UInt hwcaps_bit;
        const HChar name[6];   // may need adjustment for new facility names
     } fac_hwcaps[] = {
        { False, S390_FAC_EIMM,  VEX_HWCAPS_S390X_EIMM,  "EIMM"  },
        { False, S390_FAC_GIE,   VEX_HWCAPS_S390X_GIE,   "GIE"   },
        { False, S390_FAC_DFP,   VEX_HWCAPS_S390X_DFP,   "DFP"   },
        { False, S390_FAC_FPSE,  VEX_HWCAPS_S390X_FGX,   "FGX"   },
        { False, S390_FAC_ETF2,  VEX_HWCAPS_S390X_ETF2,  "ETF2"  },
        { False, S390_FAC_ETF3,  VEX_HWCAPS_S390X_ETF3,  "ETF3"  },
        { False, S390_FAC_STCKF, VEX_HWCAPS_S390X_STCKF, "STCKF" },
        { False, S390_FAC_FPEXT, VEX_HWCAPS_S390X_FPEXT, "FPEXT" },
        { False, S390_FAC_LSC,   VEX_HWCAPS_S390X_LSC,   "LSC"   },
        { False, S390_FAC_PFPO,  VEX_HWCAPS_S390X_PFPO,  "PFPO"  },
     };

     /* Set hwcaps according to the detected facilities */
     for (i=0; i < sizeof fac_hwcaps / sizeof fac_hwcaps[0]; ++i) {
        vg_assert(fac_hwcaps[i].facility_bit <= 63);  // for now
        if (hoststfle[0] & (1ULL << (63 - fac_hwcaps[i].facility_bit))) {
           fac_hwcaps[i].installed = True;
           vai.hwcaps |= fac_hwcaps[i].hwcaps_bit;
        }
     }

     /* Build up a string showing the probed-for facilities */
     HChar fac_str[(sizeof fac_hwcaps / sizeof fac_hwcaps[0]) *
                   (sizeof fac_hwcaps[0].name + 3) + //  %s %d
                   7 + 1 + 4 + 2  // machine %4d
                   + 1];  // \0
     HChar *p = fac_str;
     p += VG_(sprintf)(p, "machine %4d  ", model);
     for (i=0; i < sizeof fac_hwcaps / sizeof fac_hwcaps[0]; ++i) {
        p += VG_(sprintf)(p, " %s %1d", fac_hwcaps[i].name,
                          fac_hwcaps[i].installed);
     }
     *p++ = '\0';

     VG_(debugLog)(1, "machine", "%s\n", fac_str);
     VG_(debugLog)(1, "machine", "hwcaps = 0x%x\n", vai.hwcaps);

     VG_(machine_get_cache_info)(&vai);

     return True;
   }

#elif defined(VGA_arm)
   {
     /* Same instruction set detection algorithm as for ppc32. */
     vki_sigset_t          saved_set, tmp_set;
     vki_sigaction_fromK_t saved_sigill_act, saved_sigfpe_act;
     vki_sigaction_toK_t     tmp_sigill_act,   tmp_sigfpe_act;

     volatile Bool have_VFP, have_VFP2, have_VFP3, have_NEON;
     volatile Int archlevel;
     Int r;

     /* This is a kludge.  Really we ought to back-convert saved_act
        into a toK_t using VG_(convert_sigaction_fromK_to_toK), but
        since that's a no-op on all ppc64 platforms so far supported,
        it's not worth the typing effort.  At least include most basic
        sanity check: */
     vg_assert(sizeof(vki_sigaction_fromK_t) == sizeof(vki_sigaction_toK_t));

     VG_(sigemptyset)(&tmp_set);
     VG_(sigaddset)(&tmp_set, VKI_SIGILL);
     VG_(sigaddset)(&tmp_set, VKI_SIGFPE);

     r = VG_(sigprocmask)(VKI_SIG_UNBLOCK, &tmp_set, &saved_set);
     vg_assert(r == 0);

     r = VG_(sigaction)(VKI_SIGILL, NULL, &saved_sigill_act);
     vg_assert(r == 0);
     tmp_sigill_act = saved_sigill_act;

     VG_(sigaction)(VKI_SIGFPE, NULL, &saved_sigfpe_act);
     tmp_sigfpe_act = saved_sigfpe_act;

     /* NODEFER: signal handler does not return (from the kernel's point of
        view), hence if it is to successfully catch a signal more than once,
        we need the NODEFER flag. */
     tmp_sigill_act.sa_flags &= ~VKI_SA_RESETHAND;
     tmp_sigill_act.sa_flags &= ~VKI_SA_SIGINFO;
     tmp_sigill_act.sa_flags |=  VKI_SA_NODEFER;
     tmp_sigill_act.ksa_handler = handler_unsup_insn;
     VG_(sigaction)(VKI_SIGILL, &tmp_sigill_act, NULL);

     tmp_sigfpe_act.sa_flags &= ~VKI_SA_RESETHAND;
     tmp_sigfpe_act.sa_flags &= ~VKI_SA_SIGINFO;
     tmp_sigfpe_act.sa_flags |=  VKI_SA_NODEFER;
     tmp_sigfpe_act.ksa_handler = handler_unsup_insn;
     VG_(sigaction)(VKI_SIGFPE, &tmp_sigfpe_act, NULL);

     /* VFP insns */
     have_VFP = True;
     if (VG_MINIMAL_SETJMP(env_unsup_insn)) {
        have_VFP = False;
     } else {
        __asm__ __volatile__(".word 0xEEB02B42"); /* VMOV.F64 d2, d2 */
     }
     /* There are several generation of VFP extension but they differs very
        little so for now we will not distinguish them. */
     have_VFP2 = have_VFP;
     have_VFP3 = have_VFP;

     /* NEON insns */
     have_NEON = True;
     if (VG_MINIMAL_SETJMP(env_unsup_insn)) {
        have_NEON = False;
     } else {
        __asm__ __volatile__(".word 0xF2244154"); /* VMOV q2, q2 */
     }

     /* ARM architecture level */
     archlevel = 5; /* v5 will be base level */
     if (archlevel < 7) {
        archlevel = 7;
        if (VG_MINIMAL_SETJMP(env_unsup_insn)) {
           archlevel = 5;
        } else {
           __asm__ __volatile__(".word 0xF45FF000"); /* PLI [PC,#-0] */
        }
     }
     if (archlevel < 6) {
        archlevel = 6;
        if (VG_MINIMAL_SETJMP(env_unsup_insn)) {
           archlevel = 5;
        } else {
           __asm__ __volatile__(".word 0xE6822012"); /* PKHBT r2, r2, r2 */
        }
     }

     VG_(convert_sigaction_fromK_to_toK)(&saved_sigill_act, &tmp_sigill_act);
     VG_(convert_sigaction_fromK_to_toK)(&saved_sigfpe_act, &tmp_sigfpe_act);
     VG_(sigaction)(VKI_SIGILL, &tmp_sigill_act, NULL);
     VG_(sigaction)(VKI_SIGFPE, &tmp_sigfpe_act, NULL);
     VG_(sigprocmask)(VKI_SIG_SETMASK, &saved_set, NULL);

     VG_(debugLog)(1, "machine", "ARMv%d VFP %d VFP2 %d VFP3 %d NEON %d\n",
           archlevel, (Int)have_VFP, (Int)have_VFP2, (Int)have_VFP3,
           (Int)have_NEON);

     VG_(machine_arm_archlevel) = archlevel;

     va = VexArchARM;

     vai.hwcaps = VEX_ARM_ARCHLEVEL(archlevel);
     if (have_VFP3) vai.hwcaps |= VEX_HWCAPS_ARM_VFP3;
     if (have_VFP2) vai.hwcaps |= VEX_HWCAPS_ARM_VFP2;
     if (have_VFP)  vai.hwcaps |= VEX_HWCAPS_ARM_VFP;
     if (have_NEON) vai.hwcaps |= VEX_HWCAPS_ARM_NEON;

     VG_(machine_get_cache_info)(&vai);

     return True;
   }

#elif defined(VGA_arm64)
   {
     va = VexArchARM64;

     /* So far there are no variants. */
     vai.hwcaps = 0;

     VG_(machine_get_cache_info)(&vai);

     /* 0 denotes 'not set'.  The range of legitimate values here,
        after being set that is, is 2 though 17 inclusive. */
     vg_assert(vai.arm64_dMinLine_lg2_szB == 0);
     vg_assert(vai.arm64_iMinLine_lg2_szB == 0);
     ULong ctr_el0;
     __asm__ __volatile__("mrs %0, ctr_el0" : "=r"(ctr_el0));
     vai.arm64_dMinLine_lg2_szB = ((ctr_el0 >> 16) & 0xF) + 2;
     vai.arm64_iMinLine_lg2_szB = ((ctr_el0 >>  0) & 0xF) + 2;
     VG_(debugLog)(1, "machine", "ARM64: ctr_el0.dMinLine_szB = %d, "
                      "ctr_el0.iMinLine_szB = %d\n",
                   1 << vai.arm64_dMinLine_lg2_szB,
                   1 << vai.arm64_iMinLine_lg2_szB);

     return True;
   }

#elif defined(VGA_mips32)
   {
     /* Define the position of F64 bit in FIR register. */
#    define FP64 22
     va = VexArchMIPS32;
     UInt model = VG_(get_machine_model)();
     if (model == -1)
         return False;

     vai.hwcaps = model;

     /* Same instruction set detection algorithm as for ppc32/arm... */
     vki_sigset_t          saved_set, tmp_set;
     vki_sigaction_fromK_t saved_sigill_act;
     vki_sigaction_toK_t   tmp_sigill_act;

     volatile Bool have_DSP, have_DSPr2;
     Int r;

     vg_assert(sizeof(vki_sigaction_fromK_t) == sizeof(vki_sigaction_toK_t));

     VG_(sigemptyset)(&tmp_set);
     VG_(sigaddset)(&tmp_set, VKI_SIGILL);

     r = VG_(sigprocmask)(VKI_SIG_UNBLOCK, &tmp_set, &saved_set);
     vg_assert(r == 0);

     r = VG_(sigaction)(VKI_SIGILL, NULL, &saved_sigill_act);
     vg_assert(r == 0);
     tmp_sigill_act = saved_sigill_act;

     /* NODEFER: signal handler does not return (from the kernel's point of
        view), hence if it is to successfully catch a signal more than once,
        we need the NODEFER flag. */
     tmp_sigill_act.sa_flags &= ~VKI_SA_RESETHAND;
     tmp_sigill_act.sa_flags &= ~VKI_SA_SIGINFO;
     tmp_sigill_act.sa_flags |=  VKI_SA_NODEFER;
     tmp_sigill_act.ksa_handler = handler_unsup_insn;
     VG_(sigaction)(VKI_SIGILL, &tmp_sigill_act, NULL);

     if (model == VEX_PRID_COMP_MIPS) {
        /* DSPr2 instructions. */
        have_DSPr2 = True;
        if (VG_MINIMAL_SETJMP(env_unsup_insn)) {
           have_DSPr2 = False;
        } else {
           __asm__ __volatile__(".word 0x7d095351"); /* precr.qb.ph t2, t0, t1 */
        }
        if (have_DSPr2) {
           /* We assume it's 74K, since it can run DSPr2. */
           vai.hwcaps |= VEX_PRID_IMP_74K;
        } else {
           /* DSP instructions. */
           have_DSP = True;
           if (VG_MINIMAL_SETJMP(env_unsup_insn)) {
              have_DSP = False;
           } else {
              __asm__ __volatile__(".word 0x7c3f44b8"); /* rddsp t0, 0x3f */
           }
           if (have_DSP) {
              /* We assume it's 34K, since it has support for DSP. */
              vai.hwcaps |= VEX_PRID_IMP_34K;
           }
        }
     }

     /* Check if CPU has FPU and 32 dbl. prec. FP registers */
     int FIR = 0;
     __asm__ __volatile__(
        "cfc1 %0, $0"  "\n\t"
        : "=r" (FIR)
     );
     if (FIR & (1 << FP64)) {
        vai.hwcaps |= VEX_PRID_CPU_32FPR;
     }

     VG_(convert_sigaction_fromK_to_toK)(&saved_sigill_act, &tmp_sigill_act);
     VG_(sigaction)(VKI_SIGILL, &tmp_sigill_act, NULL);
     VG_(sigprocmask)(VKI_SIG_SETMASK, &saved_set, NULL);

     VG_(debugLog)(1, "machine", "hwcaps = 0x%x\n", vai.hwcaps);
     VG_(machine_get_cache_info)(&vai);

     return True;
   }

#elif defined(VGA_mips64)
   {
     va = VexArchMIPS64;
     UInt model = VG_(get_machine_model)();
     if (model== -1)
         return False;

     vai.hwcaps = model;

     VG_(machine_get_cache_info)(&vai);

     return True;
   }

#else
#  error "Unknown arch"
#endif
}

/* Notify host cpu instruction cache line size. */
#if defined(VGA_ppc32)
void VG_(machine_ppc32_set_clszB)( Int szB )
{
   vg_assert(hwcaps_done);

   /* Either the value must not have been set yet (zero) or we can
      tolerate it being set to the same value multiple times, as the
      stack scanning logic in m_main is a bit stupid. */
   vg_assert(vai.ppc_icache_line_szB == 0
             || vai.ppc_icache_line_szB == szB);

   vg_assert(szB == 16 || szB == 32 || szB == 64 || szB == 128);
   vai.ppc_icache_line_szB = szB;
}
#endif


/* Notify host cpu instruction cache line size. */
#if defined(VGA_ppc64)
void VG_(machine_ppc64_set_clszB)( Int szB )
{
   vg_assert(hwcaps_done);

   /* Either the value must not have been set yet (zero) or we can
      tolerate it being set to the same value multiple times, as the
      stack scanning logic in m_main is a bit stupid. */
   vg_assert(vai.ppc_icache_line_szB == 0
             || vai.ppc_icache_line_szB == szB);

   vg_assert(szB == 16 || szB == 32 || szB == 64 || szB == 128);
   vai.ppc_icache_line_szB = szB;
}
#endif


/* Notify host's ability to handle NEON instructions. */
#if defined(VGA_arm)
void VG_(machine_arm_set_has_NEON)( Bool has_neon )
{
   vg_assert(hwcaps_done);
   /* There's nothing else we can sanity check. */

   if (has_neon) {
      vai.hwcaps |= VEX_HWCAPS_ARM_NEON;
   } else {
      vai.hwcaps &= ~VEX_HWCAPS_ARM_NEON;
   }
}
#endif


/* Fetch host cpu info, once established. */
void VG_(machine_get_VexArchInfo)( /*OUT*/VexArch* pVa,
                                   /*OUT*/VexArchInfo* pVai )
{
   vg_assert(hwcaps_done);
   if (pVa)  *pVa  = va;
   if (pVai) *pVai = vai;
}


/* Returns the size of the largest guest register that we will
   simulate in this run.  This depends on both the guest architecture
   and on the specific capabilities we are simulating for that guest
   (eg, AVX or non-AVX ?, for amd64).  Should return either 4, 8, 16
   or 32.  General rule: if in doubt, return a value larger than
   reality.

   This information is needed by Cachegrind and Callgrind to decide
   what the minimum cache line size they are prepared to simulate is.
   Basically require that the minimum cache line size is at least as
   large as the largest register that might get transferred to/from
   memory, so as to guarantee that any such transaction can straddle
   at most 2 cache lines.
*/
Int VG_(machine_get_size_of_largest_guest_register) ( void )
{
   vg_assert(hwcaps_done);
   /* Once hwcaps_done is True, we can fish around inside va/vai to
      find the information we need. */

#  if defined(VGA_x86)
   vg_assert(va == VexArchX86);
   /* We don't support AVX, so 32 is out.  At the other end, even if
      we don't support any SSE, the X87 can generate 10 byte
      transfers, so let's say 16 to be on the safe side.  Hence the
      answer is always 16. */
   return 16;

#  elif defined(VGA_amd64)
   /* if AVX then 32 else 16 */
   return (vai.hwcaps & VEX_HWCAPS_AMD64_AVX) ? 32 : 16;

#  elif defined(VGA_ppc32)
   /* 8 if boring; 16 if signs of Altivec or other exotic stuff */
   if (vai.hwcaps & VEX_HWCAPS_PPC32_V) return 16;
   if (vai.hwcaps & VEX_HWCAPS_PPC32_VX) return 16;
   if (vai.hwcaps & VEX_HWCAPS_PPC32_DFP) return 16;
   return 8;

#  elif defined(VGA_ppc64)
   /* 8 if boring; 16 if signs of Altivec or other exotic stuff */
   if (vai.hwcaps & VEX_HWCAPS_PPC64_V) return 16;
   if (vai.hwcaps & VEX_HWCAPS_PPC64_VX) return 16;
   if (vai.hwcaps & VEX_HWCAPS_PPC64_DFP) return 16;
   return 8;

#  elif defined(VGA_s390x)
   return 8;

#  elif defined(VGA_arm)
   /* Really it depends whether or not we have NEON, but let's just
      assume we always do. */
   return 16;

#  elif defined(VGA_arm64)
   /* ARM64 always has Neon, AFAICS. */
   return 16;

#  elif defined(VGA_mips32)
   /* The guest state implies 4, but that can't really be true, can
      it? */
   return 8;

#  elif defined(VGA_mips64)
   return 8;

#  else
#    error "Unknown arch"
#  endif
}


// Given a pointer to a function as obtained by "& functionname" in C,
// produce a pointer to the actual entry point for the function.
void* VG_(fnptr_to_fnentry)( void* f )
{
#  if defined(VGP_x86_linux) || defined(VGP_amd64_linux)  \
      || defined(VGP_arm_linux)                           \
      || defined(VGP_ppc32_linux) || defined(VGO_darwin)  \
      || defined(VGP_s390x_linux) || defined(VGP_mips32_linux) \
      || defined(VGP_mips64_linux) || defined(VGP_arm64_linux)
   return f;
#  elif defined(VGP_ppc64_linux)
   /* ppc64-linux uses the AIX scheme, in which f is a pointer to a
      3-word function descriptor, of which the first word is the entry
      address. */
   UWord* descr = (UWord*)f;
   return (void*)(descr[0]);
#  else
#    error "Unknown platform"
#  endif
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
