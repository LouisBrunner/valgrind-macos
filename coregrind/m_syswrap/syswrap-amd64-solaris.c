
/*--------------------------------------------------------------------*/
/*--- Platform-specific syscalls stuff.    syswrap-amd64-solaris.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2014-2017 Petr Pavlu
      setup@dagobah.cz

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

#if defined(VGP_amd64_solaris)

#include "libvex_guest_offsets.h"
#include "pub_core_basics.h"
#include "pub_core_debuglog.h"
#include "pub_core_vki.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcprint.h"
#include "pub_core_libcsignal.h"
#include "pub_core_tooliface.h"
#include "pub_core_syswrap.h"

#include "priv_types_n_macros.h"
#include "priv_syswrap-generic.h"
#include "priv_syswrap-solaris.h"


/* Call f(arg1), but first switch stacks, using 'stack' as the new stack, and
   use 'retaddr' as f's return-to address.  Also, clear all the integer
   registers before entering f. */
__attribute__((noreturn))
void ML_(call_on_new_stack_0_1)(Addr stack,             /* %rdi */
                                Addr retaddr,           /* %rsi */
                                void (*f)(Word),        /* %rdx */
                                Word arg1);             /* %rcx */
__asm__ (
".text\n"
".globl vgModuleLocal_call_on_new_stack_0_1\n"
"vgModuleLocal_call_on_new_stack_0_1:\n"
"   movq  %rdi, %rsp\n"         /* set stack */
"   movq  %rcx, %rdi\n"         /* set arg1 */
"   pushq %rsi\n"               /* retaddr to stack */
"   pushq %rdx\n"               /* f to stack */
"   movq  $0, %rax\n"           /* zero all GP regs (except %rdi) */
"   movq  $0, %rbx\n"
"   movq  $0, %rcx\n"
"   movq  $0, %rdx\n"
"   movq  $0, %rsi\n"
"   movq  $0, %rbp\n"
"   movq  $0, %r8\n"
"   movq  $0, %r9\n"
"   movq  $0, %r10\n"
"   movq  $0, %r11\n"
"   movq  $0, %r12\n"
"   movq  $0, %r13\n"
"   movq  $0, %r14\n"
"   movq  $0, %r15\n"
"   ret\n"                      /* jump to f */
"   ud2\n"                      /* should never get here */
".previous\n"
);

/* This function is called to setup a context of a new Valgrind thread (which
   will run the client code). */
void ML_(setup_start_thread_context)(ThreadId tid, vki_ucontext_t *uc)
{
   ThreadState *tst = VG_(get_ThreadState)(tid);
   UWord *stack = (UWord*)tst->os_state.valgrind_stack_init_SP;

   VG_(memset)(uc, 0, sizeof(*uc));
   uc->uc_flags = VKI_UC_CPU | VKI_UC_SIGMASK;

   /* Start the thread with everything blocked. */
   VG_(sigfillset)(&uc->uc_sigmask);

   /* Set up the stack, it should be always 16-byte aligned before doing
      a function call, i.e. the first parameter is also 16-byte aligned. */
   vg_assert(VG_IS_16_ALIGNED(stack));
   stack -= 1;
   stack[0] = 0; /* bogus return value */

   /* Set up the registers. */
   uc->uc_mcontext.gregs[VKI_REG_RDI] = (UWord)tst; /* the parameter */
   uc->uc_mcontext.gregs[VKI_REG_RIP] = (UWord)ML_(start_thread_NORETURN);
   uc->uc_mcontext.gregs[VKI_REG_RSP] = (UWord)stack;
}

/* Architecture-specific part of VG_(save_context). */
void ML_(save_machine_context)(ThreadId tid, vki_ucontext_t *uc,
                               CorePart part)
{
   ThreadState *tst = VG_(get_ThreadState)(tid);
   struct vki_fpchip_state *fs
      = &uc->uc_mcontext.fpregs.fp_reg_set.fpchip_state;
   SizeT i;

   /* CPU */
   /* Common registers */
   uc->uc_mcontext.gregs[VKI_REG_RIP] = tst->arch.vex.guest_RIP;
   VG_TRACK(copy_reg_to_mem, part, tid, OFFSET_amd64_RIP,
            (Addr)&uc->uc_mcontext.gregs[VKI_REG_RIP], sizeof(UWord));
   uc->uc_mcontext.gregs[VKI_REG_RAX] = tst->arch.vex.guest_RAX;
   VG_TRACK(copy_reg_to_mem, part, tid, OFFSET_amd64_RAX,
            (Addr)&uc->uc_mcontext.gregs[VKI_REG_RAX], sizeof(UWord));
   uc->uc_mcontext.gregs[VKI_REG_RBX] = tst->arch.vex.guest_RBX;
   VG_TRACK(copy_reg_to_mem, part, tid, OFFSET_amd64_RBX,
            (Addr)&uc->uc_mcontext.gregs[VKI_REG_RBX], sizeof(UWord));
   uc->uc_mcontext.gregs[VKI_REG_RCX] = tst->arch.vex.guest_RCX;
   VG_TRACK(copy_reg_to_mem, part, tid, OFFSET_amd64_RCX,
            (Addr)&uc->uc_mcontext.gregs[VKI_REG_RCX], sizeof(UWord));
   uc->uc_mcontext.gregs[VKI_REG_RDX] = tst->arch.vex.guest_RDX;
   VG_TRACK(copy_reg_to_mem, part, tid, OFFSET_amd64_RDX,
            (Addr)&uc->uc_mcontext.gregs[VKI_REG_RDX], sizeof(UWord));
   uc->uc_mcontext.gregs[VKI_REG_RBP] = tst->arch.vex.guest_RBP;
   VG_TRACK(copy_reg_to_mem, part, tid, OFFSET_amd64_RBP,
            (Addr)&uc->uc_mcontext.gregs[VKI_REG_RBP], sizeof(UWord));
   uc->uc_mcontext.gregs[VKI_REG_RSI] = tst->arch.vex.guest_RSI;
   VG_TRACK(copy_reg_to_mem, part, tid, OFFSET_amd64_RSI,
            (Addr)&uc->uc_mcontext.gregs[VKI_REG_RSI], sizeof(UWord));
   uc->uc_mcontext.gregs[VKI_REG_RDI] = tst->arch.vex.guest_RDI;
   VG_TRACK(copy_reg_to_mem, part, tid, OFFSET_amd64_RDI,
            (Addr)&uc->uc_mcontext.gregs[VKI_REG_RDI], sizeof(UWord));
   uc->uc_mcontext.gregs[VKI_REG_R8] = tst->arch.vex.guest_R8;
   VG_TRACK(copy_reg_to_mem, part, tid, OFFSET_amd64_R8,
            (Addr)&uc->uc_mcontext.gregs[VKI_REG_R8], sizeof(UWord));
   uc->uc_mcontext.gregs[VKI_REG_R9] = tst->arch.vex.guest_R9;
   VG_TRACK(copy_reg_to_mem, part, tid, OFFSET_amd64_R9,
            (Addr)&uc->uc_mcontext.gregs[VKI_REG_R9], sizeof(UWord));
   uc->uc_mcontext.gregs[VKI_REG_R10] = tst->arch.vex.guest_R10;
   VG_TRACK(copy_reg_to_mem, part, tid, OFFSET_amd64_R10,
            (Addr)&uc->uc_mcontext.gregs[VKI_REG_R10], sizeof(UWord));
   uc->uc_mcontext.gregs[VKI_REG_R11] = tst->arch.vex.guest_R11;
   VG_TRACK(copy_reg_to_mem, part, tid, OFFSET_amd64_R11,
            (Addr)&uc->uc_mcontext.gregs[VKI_REG_R11], sizeof(UWord));
   uc->uc_mcontext.gregs[VKI_REG_R12] = tst->arch.vex.guest_R12;
   VG_TRACK(copy_reg_to_mem, part, tid, OFFSET_amd64_R12,
            (Addr)&uc->uc_mcontext.gregs[VKI_REG_R12], sizeof(UWord));
   uc->uc_mcontext.gregs[VKI_REG_R13] = tst->arch.vex.guest_R13;
   VG_TRACK(copy_reg_to_mem, part, tid, OFFSET_amd64_R13,
            (Addr)&uc->uc_mcontext.gregs[VKI_REG_R13], sizeof(UWord));
   uc->uc_mcontext.gregs[VKI_REG_R14] = tst->arch.vex.guest_R14;
   VG_TRACK(copy_reg_to_mem, part, tid, OFFSET_amd64_R14,
            (Addr)&uc->uc_mcontext.gregs[VKI_REG_R14], sizeof(UWord));
   uc->uc_mcontext.gregs[VKI_REG_R15] = tst->arch.vex.guest_R15;
   VG_TRACK(copy_reg_to_mem, part, tid, OFFSET_amd64_R15,
            (Addr)&uc->uc_mcontext.gregs[VKI_REG_R15], sizeof(UWord));
   uc->uc_mcontext.gregs[VKI_REG_RSP] = tst->arch.vex.guest_RSP;
   VG_TRACK(copy_reg_to_mem, part, tid, OFFSET_amd64_RSP,
            (Addr)&uc->uc_mcontext.gregs[VKI_REG_RSP], sizeof(UWord));

   /* ERR and TRAPNO */
   uc->uc_mcontext.gregs[VKI_REG_ERR] = 0;
   VG_TRACK(post_mem_write, part, tid,
            (Addr)&uc->uc_mcontext.gregs[VKI_REG_ERR], sizeof(UWord));
   uc->uc_mcontext.gregs[VKI_REG_TRAPNO] = 0;
   VG_TRACK(post_mem_write, part, tid,
            (Addr)&uc->uc_mcontext.gregs[VKI_REG_TRAPNO], sizeof(UWord));

   /* Segment registers */
   /* Valgrind does not support moves from/to segment registers on AMD64.  The
      values returned below are the ones that are set by the kernel when
      a program is started. */
   uc->uc_mcontext.gregs[VKI_REG_CS] = VKI_UCS_SEL;
   VG_TRACK(post_mem_write, part, tid,
            (Addr)&uc->uc_mcontext.gregs[VKI_REG_CS], sizeof(UWord));
   uc->uc_mcontext.gregs[VKI_REG_DS] = 0;
   VG_TRACK(post_mem_write, part, tid,
            (Addr)&uc->uc_mcontext.gregs[VKI_REG_DS], sizeof(UWord));
   uc->uc_mcontext.gregs[VKI_REG_SS] = VKI_UDS_SEL;
   VG_TRACK(post_mem_write, part, tid,
            (Addr)&uc->uc_mcontext.gregs[VKI_REG_SS], sizeof(UWord));
   uc->uc_mcontext.gregs[VKI_REG_ES] = 0;
   VG_TRACK(post_mem_write, part, tid,
            (Addr)&uc->uc_mcontext.gregs[VKI_REG_ES], sizeof(UWord));
   uc->uc_mcontext.gregs[VKI_REG_FS] = 0;
   VG_TRACK(post_mem_write, part, tid,
            (Addr)&uc->uc_mcontext.gregs[VKI_REG_FS], sizeof(UWord));
   uc->uc_mcontext.gregs[VKI_REG_GS] = 0;
   VG_TRACK(post_mem_write, part, tid,
            (Addr)&uc->uc_mcontext.gregs[VKI_REG_GS], sizeof(UWord));

   /* Segment bases */
   uc->uc_mcontext.gregs[VKI_REG_FSBASE] = tst->arch.vex.guest_FS_CONST;
   VG_TRACK(post_mem_write, part, tid,
            (Addr)&uc->uc_mcontext.gregs[VKI_REG_FSBASE], sizeof(UWord));
   uc->uc_mcontext.gregs[VKI_REG_GSBASE] = 0;
   VG_TRACK(post_mem_write, part, tid,
            (Addr)&uc->uc_mcontext.gregs[VKI_REG_GSBASE], sizeof(UWord));

   /* Handle rflags.  Refer to the x86-solaris variant of this code for
      a detailed description. */
   uc->uc_mcontext.gregs[VKI_REG_RFL] =
      LibVEX_GuestAMD64_get_rflags(&tst->arch.vex);
   VG_TRACK(post_mem_write, part, tid,
         (Addr)&uc->uc_mcontext.gregs[VKI_REG_RFL], sizeof(UWord));
   VKI_UC_GUEST_CC_OP(uc) = tst->arch.vex.guest_CC_OP;
   VKI_UC_GUEST_CC_NDEP(uc) = tst->arch.vex.guest_CC_NDEP;
   VKI_UC_GUEST_CC_DEP1(uc) = tst->arch.vex.guest_CC_DEP1;
   VG_TRACK(copy_reg_to_mem, part, tid,
            offsetof(VexGuestAMD64State, guest_CC_DEP1),
            (Addr)&VKI_UC_GUEST_CC_DEP1(uc), sizeof(UWord));
   VKI_UC_GUEST_CC_DEP2(uc) = tst->arch.vex.guest_CC_DEP2;
   VG_TRACK(copy_reg_to_mem, part, tid,
            offsetof(VexGuestAMD64State, guest_CC_DEP2),
            (Addr)&VKI_UC_GUEST_CC_DEP2(uc), sizeof(UWord));
   VKI_UC_GUEST_RFLAGS_NEG(uc) = ~uc->uc_mcontext.gregs[VKI_REG_RFL];
   /* Calculate a checksum. */
   {
      ULong buf[5];
      ULong checksum;

      buf[0] = VKI_UC_GUEST_CC_OP(uc);
      buf[1] = VKI_UC_GUEST_CC_NDEP(uc);
      buf[2] = VKI_UC_GUEST_CC_DEP1(uc);
      buf[3] = VKI_UC_GUEST_CC_DEP2(uc);
      buf[4] = uc->uc_mcontext.gregs[VKI_REG_RFL];
      checksum = ML_(fletcher64)((UInt*)&buf, sizeof(buf) / sizeof(UInt));
      VKI_UC_GUEST_RFLAGS_CHECKSUM(uc) = checksum;
   }

   /* FPU */
   /* The fpregset_t structure on amd64 follows the layout that is used by the
      FXSAVE instruction, therefore it is only necessary to call a VEX
      function that simulates this instruction. */
   LibVEX_GuestAMD64_fxsave(&tst->arch.vex, (HWord)fs);

   /* Control word */
   VG_TRACK(post_mem_write, part, tid, (Addr)&fs->cw, sizeof(fs->cw));
   /* Status word */
   VG_TRACK(post_mem_write, part, tid, (Addr)&fs->sw, sizeof(fs->sw));
   /* Compressed tag word */
   VG_TRACK(post_mem_write, part, tid, (Addr)&fs->fctw, sizeof(fs->fctw));
   /* Unused */
   VG_TRACK(post_mem_write, part, tid, (Addr)&fs->__fx_rsvd,
            sizeof(fs->__fx_rsvd));
   vg_assert(fs->__fx_rsvd == 0);
   /* Last x87 opcode */
   VG_TRACK(post_mem_write, part, tid, (Addr)&fs->fop, sizeof(fs->fop));
   vg_assert(fs->fop == 0);
   /* Last x87 instruction pointer */
   VG_TRACK(post_mem_write, part, tid, (Addr)&fs->rip, sizeof(fs->rip));
   vg_assert(fs->rip == 0);
   /* Last x87 data pointer */
   VG_TRACK(post_mem_write, part, tid, (Addr)&fs->rdp, sizeof(fs->rdp));
   vg_assert(fs->rdp == 0);
   /* Media-instruction control and status register */
   VG_TRACK(post_mem_write, part, tid, (Addr)&fs->mxcsr, sizeof(fs->mxcsr));
   /* Supported features in MXCSR */
   VG_TRACK(post_mem_write, part, tid, (Addr)&fs->mxcsr_mask,
            sizeof(fs->mxcsr_mask));

   /* ST registers */
   for (i = 0; i < 8; i++) {
      Addr addr = (Addr)&fs->st[i];
      /* x87 uses 80b FP registers but VEX uses only 64b registers, thus we
         have to lie here. :< */
      VG_TRACK(copy_reg_to_mem, part, tid, offsetof(VexGuestAMD64State,
               guest_FPREG[i]), addr, sizeof(ULong));
      VG_TRACK(copy_reg_to_mem, part, tid, offsetof(VexGuestAMD64State,
               guest_FPREG[i]), addr + 8, sizeof(UShort));
   }

   /* XMM registers */
   VG_TRACK(copy_reg_to_mem, part, tid, offsetof(VexGuestAMD64State,
            guest_YMM0), (Addr)&fs->xmm[0], sizeof(U128));
   VG_TRACK(copy_reg_to_mem, part, tid, offsetof(VexGuestAMD64State,
            guest_YMM1), (Addr)&fs->xmm[1], sizeof(U128));
   VG_TRACK(copy_reg_to_mem, part, tid, offsetof(VexGuestAMD64State,
            guest_YMM2), (Addr)&fs->xmm[2], sizeof(U128));
   VG_TRACK(copy_reg_to_mem, part, tid, offsetof(VexGuestAMD64State,
            guest_YMM3), (Addr)&fs->xmm[3], sizeof(U128));
   VG_TRACK(copy_reg_to_mem, part, tid, offsetof(VexGuestAMD64State,
            guest_YMM4), (Addr)&fs->xmm[4], sizeof(U128));
   VG_TRACK(copy_reg_to_mem, part, tid, offsetof(VexGuestAMD64State,
            guest_YMM5), (Addr)&fs->xmm[5], sizeof(U128));
   VG_TRACK(copy_reg_to_mem, part, tid, offsetof(VexGuestAMD64State,
            guest_YMM6), (Addr)&fs->xmm[6], sizeof(U128));
   VG_TRACK(copy_reg_to_mem, part, tid, offsetof(VexGuestAMD64State,
            guest_YMM7), (Addr)&fs->xmm[7], sizeof(U128));

   /* Status word (sw) at exception */
   fs->status = 0;
   VG_TRACK(post_mem_write, part, tid, (Addr)&fs->status, sizeof(fs->status));

   /* MXCSR at exception */
   fs->xstatus = 0;
   VG_TRACK(post_mem_write, part, tid, (Addr)&fs->xstatus,
            sizeof(fs->xstatus));
}

/* Architecture-specific part of VG_(restore_context). */
void ML_(restore_machine_context)(ThreadId tid, vki_ucontext_t *uc,
                                  CorePart part, Bool esp_is_thrptr)
{
   ThreadState *tst = VG_(get_ThreadState)(tid);
   struct vki_fpchip_state *fs
      = &uc->uc_mcontext.fpregs.fp_reg_set.fpchip_state;

   /* CPU */
   if (uc->uc_flags & VKI_UC_CPU) {
      /* Common registers */
      tst->arch.vex.guest_RIP = uc->uc_mcontext.gregs[VKI_REG_RIP];
      VG_TRACK(copy_mem_to_reg, part, tid,
               (Addr)&uc->uc_mcontext.gregs[VKI_REG_RIP], OFFSET_amd64_RIP,
               sizeof(UWord));
      tst->arch.vex.guest_RAX = uc->uc_mcontext.gregs[VKI_REG_RAX];
      VG_TRACK(copy_mem_to_reg, part, tid,
               (Addr)&uc->uc_mcontext.gregs[VKI_REG_RAX], OFFSET_amd64_RAX,
               sizeof(UWord));
      tst->arch.vex.guest_RBX = uc->uc_mcontext.gregs[VKI_REG_RBX];
      VG_TRACK(copy_mem_to_reg, part, tid,
               (Addr)&uc->uc_mcontext.gregs[VKI_REG_RBX], OFFSET_amd64_RBX,
               sizeof(UWord));
      tst->arch.vex.guest_RCX = uc->uc_mcontext.gregs[VKI_REG_RCX];
      VG_TRACK(copy_mem_to_reg, part, tid,
               (Addr)&uc->uc_mcontext.gregs[VKI_REG_RCX], OFFSET_amd64_RCX,
               sizeof(UWord));
      tst->arch.vex.guest_RDX = uc->uc_mcontext.gregs[VKI_REG_RDX];
      VG_TRACK(copy_mem_to_reg, part, tid,
               (Addr)&uc->uc_mcontext.gregs[VKI_REG_RDX], OFFSET_amd64_RDX,
               sizeof(UWord));
      tst->arch.vex.guest_RBP = uc->uc_mcontext.gregs[VKI_REG_RBP];
      VG_TRACK(copy_mem_to_reg, part, tid,
               (Addr)&uc->uc_mcontext.gregs[VKI_REG_RBP], OFFSET_amd64_RBP,
               sizeof(UWord));
      tst->arch.vex.guest_RSI = uc->uc_mcontext.gregs[VKI_REG_RSI];
      VG_TRACK(copy_mem_to_reg, part, tid,
               (Addr)&uc->uc_mcontext.gregs[VKI_REG_RSI], OFFSET_amd64_RSI,
               sizeof(UWord));
      tst->arch.vex.guest_RDI = uc->uc_mcontext.gregs[VKI_REG_RDI];
      VG_TRACK(copy_mem_to_reg, part, tid,
               (Addr)&uc->uc_mcontext.gregs[VKI_REG_RDI], OFFSET_amd64_RDI,
               sizeof(UWord));
      tst->arch.vex.guest_R8 = uc->uc_mcontext.gregs[VKI_REG_R8];
      VG_TRACK(copy_mem_to_reg, part, tid,
               (Addr)&uc->uc_mcontext.gregs[VKI_REG_R8], OFFSET_amd64_R8,
               sizeof(UWord));
      tst->arch.vex.guest_R9 = uc->uc_mcontext.gregs[VKI_REG_R9];
      VG_TRACK(copy_mem_to_reg, part, tid,
               (Addr)&uc->uc_mcontext.gregs[VKI_REG_R9], OFFSET_amd64_R9,
               sizeof(UWord));
      tst->arch.vex.guest_R10 = uc->uc_mcontext.gregs[VKI_REG_R10];
      VG_TRACK(copy_mem_to_reg, part, tid,
               (Addr)&uc->uc_mcontext.gregs[VKI_REG_R10], OFFSET_amd64_R10,
               sizeof(UWord));
      tst->arch.vex.guest_R11 = uc->uc_mcontext.gregs[VKI_REG_R11];
      VG_TRACK(copy_mem_to_reg, part, tid,
               (Addr)&uc->uc_mcontext.gregs[VKI_REG_R11], OFFSET_amd64_R11,
               sizeof(UWord));
      tst->arch.vex.guest_R12 = uc->uc_mcontext.gregs[VKI_REG_R12];
      VG_TRACK(copy_mem_to_reg, part, tid,
               (Addr)&uc->uc_mcontext.gregs[VKI_REG_R12], OFFSET_amd64_R12,
               sizeof(UWord));
      tst->arch.vex.guest_R13 = uc->uc_mcontext.gregs[VKI_REG_R13];
      VG_TRACK(copy_mem_to_reg, part, tid,
               (Addr)&uc->uc_mcontext.gregs[VKI_REG_R13], OFFSET_amd64_R13,
               sizeof(UWord));
      tst->arch.vex.guest_R14 = uc->uc_mcontext.gregs[VKI_REG_R14];
      VG_TRACK(copy_mem_to_reg, part, tid,
               (Addr)&uc->uc_mcontext.gregs[VKI_REG_R14], OFFSET_amd64_R14,
               sizeof(UWord));
      tst->arch.vex.guest_R15 = uc->uc_mcontext.gregs[VKI_REG_R15];
      VG_TRACK(copy_mem_to_reg, part, tid,
               (Addr)&uc->uc_mcontext.gregs[VKI_REG_R15], OFFSET_amd64_R15,
               sizeof(UWord));
      tst->arch.vex.guest_RSP = uc->uc_mcontext.gregs[VKI_REG_RSP];
      VG_TRACK(copy_mem_to_reg, part, tid,
               (Addr)&uc->uc_mcontext.gregs[VKI_REG_RSP], OFFSET_amd64_RSP,
               sizeof(UWord));

      /* Ignore ERR and TRAPNO. */

      /* Ignore segment registers. */

      /* Segment bases */
      tst->arch.vex.guest_FS_CONST = uc->uc_mcontext.gregs[VKI_REG_FSBASE];
      VG_TRACK(copy_mem_to_reg, part, tid,
               (Addr)&uc->uc_mcontext.gregs[VKI_REG_FSBASE],
               offsetof(VexGuestAMD64State, guest_FS_CONST), sizeof(UWord));

      /* Rflags.  Refer to the x86-solaris variant of this code for a detailed
         description. */
      {
         ULong rflags;
         ULong orig_rflags;
         ULong new_rflags;
         Bool ok_restore = False;

         VG_TRACK(pre_mem_read, part, tid,
                  "restore_machine_context(uc->uc_mcontext.gregs[VKI_REG_RFL])",
                  (Addr)&uc->uc_mcontext.gregs[VKI_REG_RFL], sizeof(UWord));
         rflags = uc->uc_mcontext.gregs[VKI_REG_RFL];
         orig_rflags = LibVEX_GuestAMD64_get_rflags(&tst->arch.vex);
         new_rflags = rflags;
         /* The kernel disallows the ID flag to be changed via the setcontext
            call, thus do the same. */
         if (orig_rflags & VKI_RFLAGS_ID_BIT)
            new_rflags |= VKI_RFLAGS_ID_BIT;
         else
            new_rflags &= ~VKI_RFLAGS_ID_BIT;
         LibVEX_GuestAMD64_put_rflags(new_rflags, &tst->arch.vex);
         VG_TRACK(post_reg_write, part, tid,
                  offsetof(VexGuestAMD64State, guest_CC_DEP1), sizeof(UWord));
         VG_TRACK(post_reg_write, part, tid,
                  offsetof(VexGuestAMD64State, guest_CC_DEP2), sizeof(UWord));

         if (rflags != ~VKI_UC_GUEST_RFLAGS_NEG(uc)) {
            VG_(debugLog)(1, "syswrap-solaris",
                             "The rflags value was restored from an "
                             "explicitly set value in thread %u.\n", tid);
            ok_restore = True;
         }
         else {
            ULong buf[5];
            ULong checksum;

            buf[0] = VKI_UC_GUEST_CC_OP(uc);
            buf[1] = VKI_UC_GUEST_CC_NDEP(uc);
            buf[2] = VKI_UC_GUEST_CC_DEP1(uc);
            buf[3] = VKI_UC_GUEST_CC_DEP2(uc);
            buf[4] = rflags;
            checksum = ML_(fletcher64)((UInt*)&buf,
                                       sizeof(buf) / sizeof(UInt));
            if (checksum == VKI_UC_GUEST_RFLAGS_CHECKSUM(uc)) {
               /* Check ok, the full restoration is possible. */
               VG_(debugLog)(1, "syswrap-solaris",
                                "The CC_* guest state values were fully "
                                "restored in thread %u.\n", tid);
               ok_restore = True;

               tst->arch.vex.guest_CC_OP = VKI_UC_GUEST_CC_OP(uc);
               tst->arch.vex.guest_CC_NDEP = VKI_UC_GUEST_CC_NDEP(uc);
               tst->arch.vex.guest_CC_DEP1 = VKI_UC_GUEST_CC_DEP1(uc);
               VG_TRACK(copy_mem_to_reg, part, tid,
                        (Addr)&VKI_UC_GUEST_CC_DEP1(uc),
                        offsetof(VexGuestAMD64State, guest_CC_DEP1),
                        sizeof(UWord));
               tst->arch.vex.guest_CC_DEP2 = VKI_UC_GUEST_CC_DEP2(uc);
               VG_TRACK(copy_mem_to_reg, part, tid,
                        (Addr)&VKI_UC_GUEST_CC_DEP2(uc),
                        offsetof(VexGuestAMD64State, guest_CC_DEP2),
                        sizeof(UWord));
            }
         }

         if (!ok_restore)
            VG_(debugLog)(1, "syswrap-solaris",
                             "Cannot fully restore the CC_* guest state "
                             "values, using approximate rflags in thread "
                             "%u.\n", tid);
      }
   }

   if (uc->uc_flags & VKI_UC_FPU) {
      /* FPU */
      VexEmNote note;
      SizeT i;

      /* x87 */
      /* Control word */
      VG_TRACK(pre_mem_read, part, tid,
               "restore_machine_context(uc->uc_mcontext.fpregs..cw)",
               (Addr)&fs->cw, sizeof(fs->cw));
      /* Status word */
      VG_TRACK(pre_mem_read, part, tid,
               "restore_machine_context(uc->uc_mcontext.fpregs..sw)",
               (Addr)&fs->sw, sizeof(fs->sw));
      /* Compressed tag word */
      VG_TRACK(pre_mem_read, part, tid,
               "restore_machine_context(uc->uc_mcontext.fpregs..fctw)",
               (Addr)&fs->fctw, sizeof(fs->fctw));
      /* Last x87 opcode */
      VG_TRACK(pre_mem_read, part, tid,
               "restore_machine_context(uc->uc_mcontext.fpregs..fop)",
               (Addr)&fs->fop, sizeof(fs->fop));
      /* Last x87 instruction pointer */
      VG_TRACK(pre_mem_read, part, tid,
               "restore_machine_context(uc->uc_mcontext.fpregs..rip)",
               (Addr)&fs->rip, sizeof(fs->rip));
      /* Last x87 data pointer */
      VG_TRACK(pre_mem_read, part, tid,
               "restore_machine_context(uc->uc_mcontext.fpregs..rdp)",
               (Addr)&fs->rdp, sizeof(fs->rdp));
      /* Media-instruction control and status register */
      VG_TRACK(pre_mem_read, part, tid,
               "restore_machine_context(uc->uc_mcontext.fpregs..mxcsr)",
               (Addr)&fs->mxcsr, sizeof(fs->mxcsr));
      /* Supported features in MXCSR */
      VG_TRACK(pre_mem_read, part, tid,
               "restore_machine_context(uc->uc_mcontext.fpregs..mxcsr_mask)",
               (Addr)&fs->mxcsr_mask, sizeof(fs->mxcsr_mask));

      /* ST registers */
      for (i = 0; i < 8; i++) {
         Addr addr = (Addr)&fs->st[i];
         VG_TRACK(copy_mem_to_reg, part, tid, addr,
                  offsetof(VexGuestAMD64State, guest_FPREG[i]), sizeof(ULong));
      }

      /* XMM registers */
      VG_TRACK(copy_mem_to_reg, part, tid, (Addr)&fs->xmm[0],
               offsetof(VexGuestAMD64State, guest_YMM0), sizeof(U128));
      VG_TRACK(copy_mem_to_reg, part, tid, (Addr)&fs->xmm[1],
               offsetof(VexGuestAMD64State, guest_YMM1), sizeof(U128));
      VG_TRACK(copy_mem_to_reg, part, tid, (Addr)&fs->xmm[2],
               offsetof(VexGuestAMD64State, guest_YMM2), sizeof(U128));
      VG_TRACK(copy_mem_to_reg, part, tid, (Addr)&fs->xmm[3],
               offsetof(VexGuestAMD64State, guest_YMM3), sizeof(U128));
      VG_TRACK(copy_mem_to_reg, part, tid, (Addr)&fs->xmm[4],
               offsetof(VexGuestAMD64State, guest_YMM4), sizeof(U128));
      VG_TRACK(copy_mem_to_reg, part, tid, (Addr)&fs->xmm[5],
               offsetof(VexGuestAMD64State, guest_YMM5), sizeof(U128));
      VG_TRACK(copy_mem_to_reg, part, tid, (Addr)&fs->xmm[6],
               offsetof(VexGuestAMD64State, guest_YMM6), sizeof(U128));
      VG_TRACK(copy_mem_to_reg, part, tid, (Addr)&fs->xmm[7],
               offsetof(VexGuestAMD64State, guest_YMM7), sizeof(U128));

      note = LibVEX_GuestAMD64_fxrstor((HWord)fs, &tst->arch.vex);
      if (note != EmNote_NONE)
         VG_(message)(Vg_UserMsg,
                      "Error restoring FP state in thread %u: %s.\n",
                      tid, LibVEX_EmNote_string(note));
   }
}


/* ---------------------------------------------------------------------
   PRE/POST wrappers for AMD64/Solaris-specific syscalls
   ------------------------------------------------------------------ */

#define PRE(name)       DEFN_PRE_TEMPLATE(amd64_solaris, name)
#define POST(name)      DEFN_POST_TEMPLATE(amd64_solaris, name)

/* implementation */

#undef PRE
#undef POST

#endif // defined(VGP_amd64_solaris)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
