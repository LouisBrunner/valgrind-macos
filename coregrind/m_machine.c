
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
#include "pub_core_cpuid.h"
#include "pub_core_libcsignal.h"   // for ppc32 messing with SIGILL


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

Addr VG_(get_LR) ( ThreadId tid )
{
#  if defined(VGA_ppc32) || defined(VGA_ppc64)
   return VG_(threads)[tid].arch.vex.guest_LR;
#  elif defined(VGA_x86) || defined(VGA_amd64)
   return 0;
#  else
#    error "Unknown arch"
#  endif
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
#elif defined(VGA_ppc32) || defined(VGA_ppc64)
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

//-------------------------------------------------------------
/* Details about the capabilities of the underlying (host) CPU.  These
   details are acquired by (1) enquiring with the CPU at startup, or
   (2) from the AT_SYSINFO entries the kernel gave us (ppc32 cache
   line size).  It's a bit nasty in the sense that there's no obvious
   way to stop uses of some of this info before it's ready to go.

   Current dependencies are:

   x86:   initially:  call VG_(machine_get_hwcaps)

          then safe to use VG_(machine_get_VexArchInfo) 
                       and VG_(machine_x86_have_mxcsr)
   -------------
   amd64: initially:  call VG_(machine_get_hwcaps)

          then safe to use VG_(machine_get_VexArchInfo) 
   -------------
   ppc32: initially:  call VG_(machine_get_hwcaps)
                      call VG_(machine_ppc32_set_clszB)

          then safe to use VG_(machine_get_VexArchInfo) 
                       and VG_(machine_ppc32_has_FP)
                       and VG_(machine_ppc32_has_VMX)

   VG_(machine_get_hwcaps) may use signals (although it attempts to
   leave signal state unchanged) and therefore should only be
   called before m_main sets up the client's signal state.
*/

/* --------- State --------- */
static Bool        hwcaps_done = False;

/* --- all archs --- */
static VexArch     va;
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


/* Determine what insn set and insn set variant the host has, and
   record it.  To be called once at system startup.  Returns False if
   this a CPU incapable of running Valgrind. */

#if defined(VGA_ppc32) || defined(VGA_ppc64)
#include <setjmp.h> // For jmp_buf
static jmp_buf env_sigill;
static void handler_sigill ( Int x ) { __builtin_longjmp(env_sigill,1); }
#endif

Bool VG_(machine_get_hwcaps)( void )
{
   vg_assert(hwcaps_done == False);
   hwcaps_done = True;

   // Whack default settings into vai, so that we only need to fill in
   // any interesting bits.
   LibVEX_default_VexArchInfo(&vai);

#if defined(VGA_x86)
   { Bool have_sse1, have_sse2;
     UInt eax, ebx, ecx, edx;

     if (!VG_(has_cpuid)())
        /* we can't do cpuid at all.  Give up. */
        return False;

     VG_(cpuid)(0, &eax, &ebx, &ecx, &edx);
     if (eax < 1)
        /* we can't ask for cpuid(x) for x > 0.  Give up. */
        return False;

     /* get capabilities bits into edx */
     VG_(cpuid)(1, &eax, &ebx, &ecx, &edx);

     have_sse1 = (edx & (1<<25)) != 0; /* True => have sse insns */
     have_sse2 = (edx & (1<<26)) != 0; /* True => have sse2 insns */

     if (have_sse2 && have_sse1) {
        va          = VexArchX86;
        vai.subarch = VexSubArchX86_sse2;
        VG_(machine_x86_have_mxcsr) = 1;
        return True;
     }

     if (have_sse1) {
        va          = VexArchX86;
        vai.subarch = VexSubArchX86_sse1;
        VG_(machine_x86_have_mxcsr) = 1;
        return True;
     }

     va          = VexArchX86;
     vai.subarch = VexSubArchX86_sse0;
     VG_(machine_x86_have_mxcsr) = 0;
     return True;
   }

#elif defined(VGA_amd64)
   vg_assert(VG_(has_cpuid)());
   va          = VexArchAMD64;
   vai.subarch = VexSubArch_NONE;
   return True;

#elif defined(VGA_ppc32)
   { /* ppc32 doesn't seem to have a sane way to find out what insn
        sets the CPU supports.  So we have to arse around with
        SIGILLs.  Yuck. */
     vki_sigset_t         saved_set, tmp_set;
     struct vki_sigaction saved_act, tmp_act;

     volatile Bool have_fp, have_vmx;

     VG_(sigemptyset)(&tmp_set);
     VG_(sigaddset)(&tmp_set, VKI_SIGILL);

     VG_(sigprocmask)(VKI_SIG_UNBLOCK, &tmp_set, &saved_set);

     VG_(sigaction)(VKI_SIGILL, NULL, &saved_act);
     tmp_act = saved_act;

     tmp_act.sa_flags &= ~VKI_SA_RESETHAND;
     tmp_act.sa_flags &= ~VKI_SA_SIGINFO;

     tmp_act.ksa_handler = handler_sigill;
     VG_(sigaction)(VKI_SIGILL, &tmp_act, NULL);

     have_fp = True;
     if (__builtin_setjmp(env_sigill)) {
        have_fp = False;
     } else {
        __asm__ __volatile__("fmr 0,0");
     }

     tmp_act.ksa_handler = handler_sigill;
     VG_(sigaction)(VKI_SIGILL, &tmp_act, NULL);

     have_vmx = True;
     if (__builtin_setjmp(env_sigill)) {
        have_vmx = False;
     } else {
        __asm__ __volatile__("vor 0,0,0");
     }

     VG_(sigaction)(VKI_SIGILL, &saved_act, NULL);
     VG_(sigprocmask)(VKI_SIG_SETMASK, &saved_set, NULL);

     /* VG_(printf)("FP %d VMX %d\n", (Int)have_fp, (Int)have_vmx); */

     /* We can only support 3 cases, not 4 (vmx but no fp).  So make
	fp a prerequisite for vmx. */
     if (have_vmx && !have_fp)
        have_vmx = False;

     VG_(machine_ppc32_has_FP)  = have_fp  ? 1 : 0;
     VG_(machine_ppc32_has_VMX) = have_vmx ? 1 : 0;

     va = VexArchPPC32;

     if (have_fp == False && have_vmx == False) {
        vai.subarch = VexSubArchPPC32_I;
     }
     else if (have_fp == True && have_vmx == False) {
        vai.subarch = VexSubArchPPC32_FI;
     }
     else if (have_fp == True && have_vmx == True) {
        vai.subarch = VexSubArchPPC32_VFI;
     } else {
        /* this can't happen. */
        vg_assert2(0, "VG_(machine_get_hwcaps)(ppc32)");
     }

     /* But we're not done yet: VG_(machine_ppc32_set_clszB) must be
        called before we're ready to go. */
     return True;
   }

#elif defined(VGA_ppc64)
   { vki_sigset_t         saved_set, tmp_set;
     struct vki_sigaction saved_act, tmp_act;

     volatile Bool have_vmx;

     VG_(sigemptyset)(&tmp_set);
     VG_(sigaddset)(&tmp_set, VKI_SIGILL);

     VG_(sigprocmask)(VKI_SIG_UNBLOCK, &tmp_set, &saved_set);

     VG_(sigaction)(VKI_SIGILL, NULL, &saved_act);
     tmp_act = saved_act;

     tmp_act.sa_flags &= ~VKI_SA_RESETHAND;
     tmp_act.sa_flags &= ~VKI_SA_SIGINFO;

     tmp_act.ksa_handler = handler_sigill;
     VG_(sigaction)(VKI_SIGILL, &tmp_act, NULL);

     have_vmx = True;
     if (__builtin_setjmp(env_sigill)) {
        have_vmx = False;
     } else {
        __asm__ __volatile__("vor 0,0,0");
     }

     VG_(sigaction)(VKI_SIGILL, &saved_act, NULL);
     VG_(sigprocmask)(VKI_SIG_SETMASK, &saved_set, NULL);

     /* VG_(printf)("VMX %d\n", (Int)have_vmx); */

     VG_(machine_ppc64_has_VMX) = have_vmx ? 1 : 0;

     va = VexArchPPC64;
     vai.subarch = have_vmx ? VexSubArchPPC64_VFI : VexSubArchPPC64_FI;

     /* But we're not done yet: VG_(machine_ppc64_set_clszB) must be
        called before we're ready to go. */
     return True;
   }

#else
#  error "Unknown arch"
#endif
}

/* Notify host cpu cache line size. */
#if defined(VGA_ppc32)
void VG_(machine_ppc32_set_clszB)( Int szB )
{
   vg_assert(hwcaps_done);

   /* Either the value must not have been set yet (zero) or we can
      tolerate it being set to the same value multiple times, as the
      stack scanning logic in m_main is a bit stupid. */
   vg_assert(vai.ppc32_cache_line_szB == 0
             || vai.ppc32_cache_line_szB == szB);

   vg_assert(szB == 32 || szB == 128);
   vai.ppc32_cache_line_szB = szB;
}
#endif


/* Notify host cpu cache line size. */
#if defined(VGA_ppc64)
void VG_(machine_ppc64_set_clszB)( Int szB )
{
   vg_assert(hwcaps_done);

   /* Either the value must not have been set yet (zero) or we can
      tolerate it being set to the same value multiple times, as the
      stack scanning logic in m_main is a bit stupid. */
   vg_assert(vai.ppc32_cache_line_szB == 0
             || vai.ppc32_cache_line_szB == szB);

   vg_assert(szB == 32 || szB == 128);
   vai.ppc32_cache_line_szB = szB;
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


/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
