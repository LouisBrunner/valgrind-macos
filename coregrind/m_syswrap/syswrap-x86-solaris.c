
/*--------------------------------------------------------------------*/
/*--- Platform-specific syscalls stuff.      syswrap-x86-solaris.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2011-2017 Petr Pavlu
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

#if defined(VGP_x86_solaris)

#include "libvex_guest_offsets.h"
#include "pub_core_basics.h"
#include "pub_core_vki.h"
#include "pub_core_threadstate.h"
#include "pub_core_aspacemgr.h"
#include "pub_core_xarray.h"
#include "pub_core_clientstate.h"
#include "pub_core_debuglog.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcfile.h"
#include "pub_core_libcprint.h"
#include "pub_core_libcsignal.h"
#include "pub_core_machine.h"           // VG_(get_SP)
#include "pub_core_mallocfree.h"
#include "pub_core_options.h"
#include "pub_core_tooliface.h"
#include "pub_core_signals.h"
#include "pub_core_syscall.h"
#include "pub_core_syswrap.h"

#include "priv_types_n_macros.h"
#include "priv_syswrap-generic.h"
#include "priv_syswrap-solaris.h"

/* Call f(arg1), but first switch stacks, using 'stack' as the new stack, and
   use 'retaddr' as f's return-to address.  Also, clear all the integer
   registers before entering f. */
__attribute__((noreturn))
void ML_(call_on_new_stack_0_1)(Addr stack,             /* 4(%esp) */
                                Addr retaddr,           /* 8(%esp) */
                                void (*f)(Word),        /* 12(%esp) */
                                Word arg1);             /* 16(%esp) */
__asm__ (
".text\n"
".globl vgModuleLocal_call_on_new_stack_0_1\n"
"vgModuleLocal_call_on_new_stack_0_1:\n"
"   movl  %esp, %esi\n"         /* remember old stack pointer */
"   movl  4(%esi), %esp\n"      /* set stack */
"   pushl $0\n"                 /* align stack */
"   pushl $0\n"                 /* align stack */
"   pushl $0\n"                 /* align stack */
"   pushl 16(%esi)\n"           /* arg1 to stack */
"   pushl 8(%esi)\n"            /* retaddr to stack */
"   pushl 12(%esi)\n"           /* f to stack */
"   movl  $0, %eax\n"           /* zero all GP regs */
"   movl  $0, %ebx\n"
"   movl  $0, %ecx\n"
"   movl  $0, %edx\n"
"   movl  $0, %esi\n"
"   movl  $0, %edi\n"
"   movl  $0, %ebp\n"
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
   UShort cs, ds, ss, es, fs, gs;

   VG_(memset)(uc, 0, sizeof(*uc));
   uc->uc_flags = VKI_UC_CPU | VKI_UC_SIGMASK;

   /* Start the thread with everything blocked. */
   VG_(sigfillset)(&uc->uc_sigmask);

   /* Set up the stack, it should be always 16-byte aligned before doing
      a function call, i.e. the first parameter is also 16-byte aligned. */
   vg_assert(VG_IS_16_ALIGNED(stack));
   stack -= 1;
   stack[0] = 0; /* bogus return value */
   stack[1] = (UWord)tst; /* the parameter */

   /* Set up the registers. */
   uc->uc_mcontext.gregs[VKI_EIP] = (UWord)ML_(start_thread_NORETURN);
   uc->uc_mcontext.gregs[VKI_UESP] = (UWord)stack;

   /* Copy segment registers. */
   __asm__ __volatile__(
      "movw %%cs, %[cs]\n"
      "movw %%ds, %[ds]\n"
      "movw %%ss, %[ss]\n"
      "movw %%es, %[es]\n"
      "movw %%fs, %[fs]\n"
      "movw %%gs, %[gs]\n"
      : [cs] "=m" (cs), [ds] "=m" (ds), [ss] "=m" (ss), [es] "=m" (es),
        [fs] "=m" (fs), [gs] "=m" (gs));
   uc->uc_mcontext.gregs[VKI_CS] = cs;
   uc->uc_mcontext.gregs[VKI_DS] = ds;
   uc->uc_mcontext.gregs[VKI_SS] = ss;
   uc->uc_mcontext.gregs[VKI_ES] = es;
   uc->uc_mcontext.gregs[VKI_FS] = fs;
   uc->uc_mcontext.gregs[VKI_GS] = gs;
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
   uc->uc_mcontext.gregs[VKI_EIP] = tst->arch.vex.guest_EIP;
   VG_TRACK(copy_reg_to_mem, part, tid, OFFSET_x86_EIP,
            (Addr)&uc->uc_mcontext.gregs[VKI_EIP], sizeof(UWord));
   uc->uc_mcontext.gregs[VKI_EAX] = tst->arch.vex.guest_EAX;
   VG_TRACK(copy_reg_to_mem, part, tid, OFFSET_x86_EAX,
            (Addr)&uc->uc_mcontext.gregs[VKI_EAX], sizeof(UWord));
   uc->uc_mcontext.gregs[VKI_EBX] = tst->arch.vex.guest_EBX;
   VG_TRACK(copy_reg_to_mem, part, tid, OFFSET_x86_EBX,
            (Addr)&uc->uc_mcontext.gregs[VKI_EBX], sizeof(UWord));
   uc->uc_mcontext.gregs[VKI_ECX] = tst->arch.vex.guest_ECX;
   VG_TRACK(copy_reg_to_mem, part, tid, OFFSET_x86_ECX,
            (Addr)&uc->uc_mcontext.gregs[VKI_ECX], sizeof(UWord));
   uc->uc_mcontext.gregs[VKI_EDX] = tst->arch.vex.guest_EDX;
   VG_TRACK(copy_reg_to_mem, part, tid, OFFSET_x86_EDX,
            (Addr)&uc->uc_mcontext.gregs[VKI_EDX], sizeof(UWord));
   uc->uc_mcontext.gregs[VKI_EBP] = tst->arch.vex.guest_EBP;
   VG_TRACK(copy_reg_to_mem, part, tid, OFFSET_x86_EBP,
            (Addr)&uc->uc_mcontext.gregs[VKI_EBP], sizeof(UWord));
   uc->uc_mcontext.gregs[VKI_ESI] = tst->arch.vex.guest_ESI;
   VG_TRACK(copy_reg_to_mem, part, tid, OFFSET_x86_ESI,
            (Addr)&uc->uc_mcontext.gregs[VKI_ESI], sizeof(UWord));
   uc->uc_mcontext.gregs[VKI_EDI] = tst->arch.vex.guest_EDI;
   VG_TRACK(copy_reg_to_mem, part, tid, OFFSET_x86_EDI,
            (Addr)&uc->uc_mcontext.gregs[VKI_EDI], sizeof(UWord));
   uc->uc_mcontext.gregs[VKI_UESP] = tst->arch.vex.guest_ESP;
   VG_TRACK(copy_reg_to_mem, part, tid, OFFSET_x86_ESP,
            (Addr)&uc->uc_mcontext.gregs[VKI_UESP], sizeof(UWord));
   uc->uc_mcontext.gregs[VKI_ESP] = 0;
   VG_TRACK(post_mem_write, part, tid, (Addr)&uc->uc_mcontext.gregs[VKI_ESP],
            sizeof(UWord));

   /* ERR and TRAPNO */
   uc->uc_mcontext.gregs[VKI_ERR] = 0;
   VG_TRACK(post_mem_write, part, tid, (Addr)&uc->uc_mcontext.gregs[VKI_ERR],
            sizeof(UWord));
   uc->uc_mcontext.gregs[VKI_TRAPNO] = 0;
   VG_TRACK(post_mem_write, part, tid, (Addr)&uc->uc_mcontext.gregs[VKI_TRAPNO],
            sizeof(UWord));

   /* Segment registers */
   /* Note that segment registers are 16b in VEX, but 32b in mcontext.  Thus
      we tell a tool that the lower 16 bits were copied and that the higher 16
      bits were set (to zero).  (This assumes a little-endian
      architecture.) */
   uc->uc_mcontext.gregs[VKI_CS] = tst->arch.vex.guest_CS;
   VG_TRACK(copy_reg_to_mem, part, tid, OFFSET_x86_CS,
            (Addr)&uc->uc_mcontext.gregs[VKI_CS], sizeof(UShort));
   VG_TRACK(post_mem_write, part, tid,
            (Addr)(&uc->uc_mcontext.gregs[VKI_CS]) + 2, sizeof(UShort));
   uc->uc_mcontext.gregs[VKI_DS] = tst->arch.vex.guest_DS;
   VG_TRACK(copy_reg_to_mem, part, tid, OFFSET_x86_DS,
            (Addr)&uc->uc_mcontext.gregs[VKI_DS], sizeof(UShort));
   VG_TRACK(post_mem_write, part, tid,
            (Addr)(&uc->uc_mcontext.gregs[VKI_DS]) + 2, sizeof(UShort));
   uc->uc_mcontext.gregs[VKI_SS] = tst->arch.vex.guest_SS;
   VG_TRACK(copy_reg_to_mem, part, tid, OFFSET_x86_SS,
            (Addr)&uc->uc_mcontext.gregs[VKI_SS], sizeof(UShort));
   VG_TRACK(post_mem_write, part, tid,
            (Addr)(&uc->uc_mcontext.gregs[VKI_SS]) + 2, sizeof(UShort));
   uc->uc_mcontext.gregs[VKI_ES] = tst->arch.vex.guest_ES;
   VG_TRACK(copy_reg_to_mem, part, tid, OFFSET_x86_ES,
            (Addr)&uc->uc_mcontext.gregs[VKI_ES], sizeof(UShort));
   VG_TRACK(post_mem_write, part, tid,
            (Addr)(&uc->uc_mcontext.gregs[VKI_ES]) + 2, sizeof(UShort));
   uc->uc_mcontext.gregs[VKI_FS] = tst->arch.vex.guest_FS;
   VG_TRACK(copy_reg_to_mem, part, tid, OFFSET_x86_FS,
            (Addr)&uc->uc_mcontext.gregs[VKI_FS], sizeof(UShort));
   VG_TRACK(post_mem_write, part, tid,
            (Addr)(&uc->uc_mcontext.gregs[VKI_FS]) + 2, sizeof(UShort));
   uc->uc_mcontext.gregs[VKI_GS] = tst->arch.vex.guest_GS;
   VG_TRACK(copy_reg_to_mem, part, tid, OFFSET_x86_GS,
            (Addr)&uc->uc_mcontext.gregs[VKI_GS], sizeof(UShort));
   VG_TRACK(post_mem_write, part, tid,
            (Addr)(&uc->uc_mcontext.gregs[VKI_GS]) + 2, sizeof(UShort));

   /* Handle eflags (optimistically make all flags defined). */
   uc->uc_mcontext.gregs[VKI_EFL] =
      LibVEX_GuestX86_get_eflags(&tst->arch.vex);
   VG_TRACK(post_mem_write, part, tid, (Addr)&uc->uc_mcontext.gregs[VKI_EFL],
         sizeof(UWord));
   /* The LibVEX_GuestX86_get_eflags() call calculates eflags value from the
      CC_OP, CC_DEP1, CC_DEP2, CC_NDEP, DFLAG, IDFLAG and ACFLAG guest state
      values.  The *FLAG values represent one-bit information and are saved
      without loss of precision into eflags.  However when CC_* values are
      converted into eflags then precision is lost.  What we do here is to
      save unmodified CC_* values into unused ucontext members (the 'long
      uc_filler[5] and 'int fs->__pad[2]' arrays) so we can then restore the
      context in ML_(restore_machine_context)() without the loss of precision.
      This imposes a requirement on client programs to not use these two
      members. Luckily this is never a case in Solaris-gate programs and
      libraries. */
   /* CC_OP and CC_NDEP are always defined, but we don't want to tell a tool
      that we just defined uc_filler[0,1].  This helps if someone uses an
      uninitialized ucontext and tries to read (use) uc_filler[0,1].  Memcheck
      in such a case should detect this error. */
   VKI_UC_GUEST_CC_OP(uc) = tst->arch.vex.guest_CC_OP;
   VKI_UC_GUEST_CC_NDEP(uc) = tst->arch.vex.guest_CC_NDEP;
   /* We want to copy shadow values of CC_DEP1 and CC_DEP2 so we have to tell
      a tool about this copy. */
   VKI_UC_GUEST_CC_DEP1(uc) = tst->arch.vex.guest_CC_DEP1;
   VG_TRACK(copy_reg_to_mem, part, tid,
            offsetof(VexGuestX86State, guest_CC_DEP1),
            (Addr)&VKI_UC_GUEST_CC_DEP1(uc), sizeof(UWord));
   VKI_UC_GUEST_CC_DEP2(uc) = tst->arch.vex.guest_CC_DEP2;
   VG_TRACK(copy_reg_to_mem, part, tid,
            offsetof(VexGuestX86State, guest_CC_DEP2),
            (Addr)&VKI_UC_GUEST_CC_DEP2(uc), sizeof(UWord));
   /* Make another copy of eflags. */
   VKI_UC_GUEST_EFLAGS_NEG(uc) = ~uc->uc_mcontext.gregs[VKI_EFL];
   /* Calculate a checksum. */
   {
      UInt buf[5];
      UInt checksum;

      buf[0] = VKI_UC_GUEST_CC_OP(uc);
      buf[1] = VKI_UC_GUEST_CC_NDEP(uc);
      buf[2] = VKI_UC_GUEST_CC_DEP1(uc);
      buf[3] = VKI_UC_GUEST_CC_DEP2(uc);
      buf[4] = uc->uc_mcontext.gregs[VKI_EFL];
      checksum = ML_(fletcher32)((UShort*)&buf, sizeof(buf) / sizeof(UShort));
      /* Store the checksum. */
      VKI_UC_GUEST_EFLAGS_CHECKSUM(uc) = checksum;
   }

   /* FPU */
   /* x87 */
   vg_assert(sizeof(fs->state) == 108);
   LibVEX_GuestX86_get_x87(&tst->arch.vex, (UChar*)&fs->state);

   /* Flags and control words */
   VG_TRACK(post_mem_write, part, tid, (Addr)&fs->state, 28);
   /* ST registers */
   for (i = 0; i < 8; i++) {
      Addr addr = (Addr)&fs->state + 28 + i * 10;
      /* x87 uses 80b FP registers but VEX uses only 64b registers, thus we
         have to lie here. :< */
      VG_TRACK(copy_reg_to_mem, part, tid, offsetof(VexGuestX86State,
               guest_FPREG[i]), addr, sizeof(ULong));
      VG_TRACK(copy_reg_to_mem, part, tid, offsetof(VexGuestX86State,
               guest_FPREG[i]), addr + 8, sizeof(UShort));
      }

   /* Status word (sw) at exception */
   fs->status = 0;
   VG_TRACK(post_mem_write, part, tid, (Addr)&fs->status, sizeof(fs->status));

   /* SSE */
   fs->mxcsr = LibVEX_GuestX86_get_mxcsr(&tst->arch.vex);
   VG_TRACK(post_mem_write, part, tid, (Addr)&fs->mxcsr, sizeof(fs->mxcsr));

   /* MXCSR at exception */
   fs->xstatus = 0;
   VG_TRACK(post_mem_write, part, tid, (Addr)&fs->xstatus,
            sizeof(fs->xstatus));

   /* XMM registers */
#define COPY_OUT_XMM(dest, src) \
   do {                         \
      dest._l[0] = src[0];      \
      dest._l[1] = src[1];      \
      dest._l[2] = src[2];      \
      dest._l[3] = src[3];      \
   } while (0)
   COPY_OUT_XMM(fs->xmm[0], tst->arch.vex.guest_XMM0);
   VG_TRACK(copy_reg_to_mem, part, tid, offsetof(VexGuestX86State,
            guest_XMM0), (Addr)&fs->xmm[0], sizeof(U128));
   COPY_OUT_XMM(fs->xmm[1], tst->arch.vex.guest_XMM1);
   VG_TRACK(copy_reg_to_mem, part, tid, offsetof(VexGuestX86State,
            guest_XMM1), (Addr)&fs->xmm[1], sizeof(U128));
   COPY_OUT_XMM(fs->xmm[2], tst->arch.vex.guest_XMM2);
   VG_TRACK(copy_reg_to_mem, part, tid, offsetof(VexGuestX86State,
            guest_XMM2), (Addr)&fs->xmm[2], sizeof(U128));
   COPY_OUT_XMM(fs->xmm[3], tst->arch.vex.guest_XMM3);
   VG_TRACK(copy_reg_to_mem, part, tid, offsetof(VexGuestX86State,
            guest_XMM3), (Addr)&fs->xmm[3], sizeof(U128));
   COPY_OUT_XMM(fs->xmm[4], tst->arch.vex.guest_XMM4);
   VG_TRACK(copy_reg_to_mem, part, tid, offsetof(VexGuestX86State,
            guest_XMM4), (Addr)&fs->xmm[4], sizeof(U128));
   COPY_OUT_XMM(fs->xmm[5], tst->arch.vex.guest_XMM5);
   VG_TRACK(copy_reg_to_mem, part, tid, offsetof(VexGuestX86State,
            guest_XMM5), (Addr)&fs->xmm[5], sizeof(U128));
   COPY_OUT_XMM(fs->xmm[6], tst->arch.vex.guest_XMM6);
   VG_TRACK(copy_reg_to_mem, part, tid, offsetof(VexGuestX86State,
            guest_XMM6), (Addr)&fs->xmm[6], sizeof(U128));
   COPY_OUT_XMM(fs->xmm[7], tst->arch.vex.guest_XMM7);
   VG_TRACK(copy_reg_to_mem, part, tid, offsetof(VexGuestX86State,
            guest_XMM7), (Addr)&fs->xmm[7], sizeof(U128));
#undef COPY_OUT_XMM
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
      tst->arch.vex.guest_EIP = uc->uc_mcontext.gregs[VKI_EIP];
      VG_TRACK(copy_mem_to_reg, part, tid,
               (Addr)&uc->uc_mcontext.gregs[VKI_EIP], OFFSET_x86_EIP,
               sizeof(UWord));
      tst->arch.vex.guest_EAX = uc->uc_mcontext.gregs[VKI_EAX];
      VG_TRACK(copy_mem_to_reg, part, tid,
               (Addr)&uc->uc_mcontext.gregs[VKI_EAX], OFFSET_x86_EAX,
               sizeof(UWord));
      tst->arch.vex.guest_EBX = uc->uc_mcontext.gregs[VKI_EBX];
      VG_TRACK(copy_mem_to_reg, part, tid,
               (Addr)&uc->uc_mcontext.gregs[VKI_EBX], OFFSET_x86_EBX,
               sizeof(UWord));
      tst->arch.vex.guest_ECX = uc->uc_mcontext.gregs[VKI_ECX];
      VG_TRACK(copy_mem_to_reg, part, tid,
               (Addr)&uc->uc_mcontext.gregs[VKI_ECX], OFFSET_x86_ECX,
               sizeof(UWord));
      tst->arch.vex.guest_EDX = uc->uc_mcontext.gregs[VKI_EDX];
      VG_TRACK(copy_mem_to_reg, part, tid,
               (Addr)&uc->uc_mcontext.gregs[VKI_EDX], OFFSET_x86_EDX,
               sizeof(UWord));
      tst->arch.vex.guest_EBP = uc->uc_mcontext.gregs[VKI_EBP];
      VG_TRACK(copy_mem_to_reg, part, tid,
               (Addr)&uc->uc_mcontext.gregs[VKI_EBP], OFFSET_x86_EBP,
               sizeof(UWord));
      tst->arch.vex.guest_ESI = uc->uc_mcontext.gregs[VKI_ESI];
      VG_TRACK(copy_mem_to_reg, part, tid,
               (Addr)&uc->uc_mcontext.gregs[VKI_ESI], OFFSET_x86_ESI,
               sizeof(UWord));
      tst->arch.vex.guest_EDI = uc->uc_mcontext.gregs[VKI_EDI];
      VG_TRACK(copy_mem_to_reg, part, tid,
               (Addr)&uc->uc_mcontext.gregs[VKI_EDI], OFFSET_x86_EDI,
               sizeof(UWord));
      tst->arch.vex.guest_ESP = uc->uc_mcontext.gregs[VKI_UESP];
      VG_TRACK(copy_mem_to_reg, part, tid,
               (Addr)&uc->uc_mcontext.gregs[VKI_UESP], OFFSET_x86_ESP,
               sizeof(UWord));

      if (esp_is_thrptr) {
         /* The thrptr value is passed by libc to the kernel in the otherwise
            unused ESP field.  This is used when a new thread is created. */
         VG_TRACK(pre_mem_read, part, tid,
                  "restore_machine_context(uc->uc_mcontext.gregs[VKI_ESP])",
                  (Addr)&uc->uc_mcontext.gregs[VKI_ESP], sizeof(UWord));
         if (uc->uc_mcontext.gregs[VKI_ESP]) {
            tst->os_state.thrptr = uc->uc_mcontext.gregs[VKI_ESP];
            ML_(update_gdt_lwpgs)(tid);
         }
      }

      /* Ignore ERR and TRAPNO. */

      /* Segment registers */
      tst->arch.vex.guest_CS = uc->uc_mcontext.gregs[VKI_CS];
      VG_TRACK(copy_mem_to_reg, part, tid,
               (Addr)&uc->uc_mcontext.gregs[VKI_CS], OFFSET_x86_CS,
               sizeof(UShort));
      tst->arch.vex.guest_DS = uc->uc_mcontext.gregs[VKI_DS];
      VG_TRACK(copy_mem_to_reg, part, tid,
               (Addr)&uc->uc_mcontext.gregs[VKI_DS], OFFSET_x86_DS,
               sizeof(UShort));
      tst->arch.vex.guest_SS = uc->uc_mcontext.gregs[VKI_SS];
      VG_TRACK(copy_mem_to_reg, part, tid,
               (Addr)&uc->uc_mcontext.gregs[VKI_SS], OFFSET_x86_SS,
               sizeof(UShort));
      tst->arch.vex.guest_ES = uc->uc_mcontext.gregs[VKI_ES];
      VG_TRACK(copy_mem_to_reg, part, tid,
               (Addr)&uc->uc_mcontext.gregs[VKI_ES], OFFSET_x86_ES,
               sizeof(UShort));
      tst->arch.vex.guest_FS = uc->uc_mcontext.gregs[VKI_FS];
      VG_TRACK(copy_mem_to_reg, part, tid,
               (Addr)&uc->uc_mcontext.gregs[VKI_FS], OFFSET_x86_FS,
               sizeof(UShort));
      tst->arch.vex.guest_GS = uc->uc_mcontext.gregs[VKI_GS];
      VG_TRACK(copy_mem_to_reg, part, tid,
               (Addr)&uc->uc_mcontext.gregs[VKI_GS], OFFSET_x86_GS,
               sizeof(UShort));

      /* Eflags */
      {
         UInt eflags;
         UInt orig_eflags;
         UInt new_eflags;
         Bool ok_restore = False;

         VG_TRACK(pre_mem_read, part, tid,
                  "restore_machine_context(uc->uc_mcontext.gregs[VKI_EFL])",
                  (Addr)&uc->uc_mcontext.gregs[VKI_EFL], sizeof(UWord));
         eflags = uc->uc_mcontext.gregs[VKI_EFL];
         orig_eflags = LibVEX_GuestX86_get_eflags(&tst->arch.vex);
         new_eflags = eflags;
         /* The kernel disallows the ID flag to be changed via the setcontext
            call, thus do the same. */
         if (orig_eflags & VKI_EFLAGS_ID_BIT)
            new_eflags |= VKI_EFLAGS_ID_BIT;
         else
            new_eflags &= ~VKI_EFLAGS_ID_BIT;
         LibVEX_GuestX86_put_eflags(new_eflags, &tst->arch.vex);
         VG_TRACK(post_reg_write, part, tid,
                  offsetof(VexGuestX86State, guest_CC_DEP1), sizeof(UWord));
         VG_TRACK(post_reg_write, part, tid,
                  offsetof(VexGuestX86State, guest_CC_DEP2), sizeof(UWord));

         /* Check if this context was created by us in VG_(save_context). In
            that case, try to restore the CC_OP, CC_DEP1, CC_DEP2 and CC_NDEP
            values which we previously stashed into unused members of the
            context. */
         if (eflags != ~VKI_UC_GUEST_EFLAGS_NEG(uc)) {
            VG_(debugLog)(1, "syswrap-solaris",
                             "The eflags value was restored from an "
                             "explicitly set value in thread %u.\n", tid);
            ok_restore = True;
         }
         else {
            UInt buf[5];
            UInt checksum;

            buf[0] = VKI_UC_GUEST_CC_OP(uc);
            buf[1] = VKI_UC_GUEST_CC_NDEP(uc);
            buf[2] = VKI_UC_GUEST_CC_DEP1(uc);
            buf[3] = VKI_UC_GUEST_CC_DEP2(uc);
            buf[4] = eflags;
            checksum = ML_(fletcher32)((UShort*)&buf,
                                       sizeof(buf) / sizeof(UShort));
            if (checksum == VKI_UC_GUEST_EFLAGS_CHECKSUM(uc)) {
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
                        offsetof(VexGuestX86State, guest_CC_DEP1),
                        sizeof(UWord));
               tst->arch.vex.guest_CC_DEP2 = VKI_UC_GUEST_CC_DEP2(uc);
               VG_TRACK(copy_mem_to_reg, part, tid,
                        (Addr)&VKI_UC_GUEST_CC_DEP2(uc),
                        offsetof(VexGuestX86State, guest_CC_DEP2),
                        sizeof(UWord));
            }
         }

         if (!ok_restore)
            VG_(debugLog)(1, "syswrap-solaris",
                             "Cannot fully restore the CC_* guest state "
                             "values, using approximate eflags in thread "
                             "%u.\n", tid);
      }
   }

   if (uc->uc_flags & VKI_UC_FPU) {
      /* FPU */
      VexEmNote note;
      SizeT i;

      /* x87 */
      /* Flags and control words */
      VG_TRACK(pre_mem_read, part, tid,
               "restore_machine_context(uc->uc_mcontext.fpregs..x87_state)",
               (Addr)&fs->state, 28);
      /* ST registers */
      for (i = 0; i < 8; i++) {
         Addr addr = (Addr)&fs->state + 28 + i * 10;
         VG_TRACK(copy_mem_to_reg, part, tid, addr,
                  offsetof(VexGuestX86State, guest_FPREG[i]), sizeof(ULong));
      }
      note = LibVEX_GuestX86_put_x87((UChar*)&fs->state, &tst->arch.vex);
      if (note != EmNote_NONE)
         VG_(message)(Vg_UserMsg,
                      "Error restoring x87 state in thread %u: %s.\n",
                      tid, LibVEX_EmNote_string(note));

      /* SSE */
      VG_TRACK(pre_mem_read, part, tid,
               "restore_machine_context(uc->uc_mcontext.fpregs..mxcsr)",
               (Addr)&fs->mxcsr, sizeof(fs->mxcsr));
      note = LibVEX_GuestX86_put_mxcsr(fs->mxcsr, &tst->arch.vex);
      if (note != EmNote_NONE)
         VG_(message)(Vg_UserMsg,
                      "Error restoring mxcsr state in thread %u: %s.\n",
                      tid, LibVEX_EmNote_string(note));
      /* XMM registers */
#define COPY_IN_XMM(src, dest) \
      do {                     \
         dest[0] = src._l[0];  \
         dest[1] = src._l[1];  \
         dest[2] = src._l[2];  \
         dest[3] = src._l[3];  \
      } while (0)
      COPY_IN_XMM(fs->xmm[0], tst->arch.vex.guest_XMM0);
      VG_TRACK(copy_mem_to_reg, part, tid, (Addr)&fs->xmm[0],
               offsetof(VexGuestX86State, guest_XMM0), sizeof(U128));
      COPY_IN_XMM(fs->xmm[1], tst->arch.vex.guest_XMM1);
      VG_TRACK(copy_mem_to_reg, part, tid, (Addr)&fs->xmm[1],
               offsetof(VexGuestX86State, guest_XMM1), sizeof(U128));
      COPY_IN_XMM(fs->xmm[2], tst->arch.vex.guest_XMM2);
      VG_TRACK(copy_mem_to_reg, part, tid, (Addr)&fs->xmm[2],
               offsetof(VexGuestX86State, guest_XMM2), sizeof(U128));
      COPY_IN_XMM(fs->xmm[3], tst->arch.vex.guest_XMM3);
      VG_TRACK(copy_mem_to_reg, part, tid, (Addr)&fs->xmm[3],
               offsetof(VexGuestX86State, guest_XMM3), sizeof(U128));
      COPY_IN_XMM(fs->xmm[4], tst->arch.vex.guest_XMM4);
      VG_TRACK(copy_mem_to_reg, part, tid, (Addr)&fs->xmm[4],
               offsetof(VexGuestX86State, guest_XMM4), sizeof(U128));
      COPY_IN_XMM(fs->xmm[5], tst->arch.vex.guest_XMM5);
      VG_TRACK(copy_mem_to_reg, part, tid, (Addr)&fs->xmm[5],
               offsetof(VexGuestX86State, guest_XMM5), sizeof(U128));
      COPY_IN_XMM(fs->xmm[6], tst->arch.vex.guest_XMM6);
      VG_TRACK(copy_mem_to_reg, part, tid, (Addr)&fs->xmm[6],
               offsetof(VexGuestX86State, guest_XMM6), sizeof(U128));
      COPY_IN_XMM(fs->xmm[7], tst->arch.vex.guest_XMM7);
      VG_TRACK(copy_mem_to_reg, part, tid, (Addr)&fs->xmm[7],
               offsetof(VexGuestX86State, guest_XMM7), sizeof(U128));
#undef COPY_IN_XMM
   }
}

/* Allocate GDT for a given thread. */
void ML_(setup_gdt)(VexGuestX86State *vex)
{
   Addr gdt = (Addr)VG_(calloc)("syswrap-solaris-x86.gdt",
                                VEX_GUEST_X86_GDT_NENT,
                                sizeof(VexGuestX86SegDescr));
   vex->guest_GDT = gdt;
}

/* Deallocate GDT for a given thread. */
void ML_(cleanup_gdt)(VexGuestX86State *vex)
{
   if (!vex->guest_GDT)
      return;
   VG_(free)((void *) (HWord) vex->guest_GDT);
   vex->guest_GDT = 0;
}

/* For a given thread, update the LWPGS descriptor in the thread's GDT
   according to the thread pointer. */
void ML_(update_gdt_lwpgs)(ThreadId tid)
{
   ThreadState *tst = VG_(get_ThreadState)(tid);
   Addr base = tst->os_state.thrptr;
   VexGuestX86SegDescr *gdt
      = (VexGuestX86SegDescr *) (HWord) tst->arch.vex.guest_GDT;
   VexGuestX86SegDescr desc;

   vg_assert(gdt);

   VG_(memset)(&desc, 0, sizeof(desc));
   if (base) {
      desc.LdtEnt.Bits.LimitLow = -1;
      desc.LdtEnt.Bits.LimitHi = -1;
      desc.LdtEnt.Bits.BaseLow = base & 0xffff;
      desc.LdtEnt.Bits.BaseMid = (base >> 16) & 0xff;
      desc.LdtEnt.Bits.BaseHi = (base >> 24) & 0xff;
      desc.LdtEnt.Bits.Pres = 1;
      desc.LdtEnt.Bits.Dpl = 3; /* SEL_UPL */
      desc.LdtEnt.Bits.Type = 19; /* SDT_MEMRWA */
      desc.LdtEnt.Bits.Granularity = 1; /* SDP_PAGES */
      desc.LdtEnt.Bits.Default_Big = 1; /* SDP_OP32 */
   }

   gdt[VKI_GDT_LWPGS] = desc;

   /* Write %gs. */
   tst->arch.vex.guest_GS = VKI_LWPGS_SEL;
   VG_TRACK(post_reg_write, Vg_CoreSysCall, tid, OFFSET_x86_GS,
            sizeof(UShort));
}


/* ---------------------------------------------------------------------
   PRE/POST wrappers for x86/Solaris-specific syscalls
   ------------------------------------------------------------------ */

#define PRE(name)       DEFN_PRE_TEMPLATE(x86_solaris, name)
#define POST(name)      DEFN_POST_TEMPLATE(x86_solaris, name)

/* implementation */

PRE(sys_fstatat64)
{
   /* int fstatat64(int fildes, const char *path, struct stat64 *buf,
                    int flag); */
   PRINT("sys_fstatat64 ( %ld, %#lx(%s), %#lx, %ld )", SARG1, ARG2,
         (HChar*)ARG2, ARG3, SARG4);
   PRE_REG_READ4(long, "fstatat64", int, fildes, const char *, path,
                 struct stat64 *, buf, int, flag);
   if (ARG2)
      PRE_MEM_RASCIIZ("fstatat64(path)", ARG2);
   PRE_MEM_WRITE("fstatat64(buf)", ARG3, sizeof(struct vki_stat64));

   /* Be strict. */
   if (ARG1 != VKI_AT_FDCWD &&
       !ML_(fd_allowed)(ARG1, "fstatat64", tid, False))
      SET_STATUS_Failure(VKI_EBADF);
}

POST(sys_fstatat64)
{
   POST_MEM_WRITE(ARG3, sizeof(struct vki_stat64));
}

PRE(sys_openat64)
{
   /* int openat64(int fildes, const char *filename, int flags);
      int openat64(int fildes, const char *filename, int flags, mode_t mode);
    */
   *flags |= SfMayBlock;

   if (ARG3 & VKI_O_CREAT) {
      /* 4-arg version */
      PRINT("sys_openat64 ( %ld, %#lx(%s), %ld, %ld )", SARG1, ARG2,
            (HChar*)ARG2, SARG3, SARG4);
      PRE_REG_READ4(long, "openat64", int, fildes, const char *, filename,
                    int, flags, vki_mode_t, mode);
   }
   else {
      /* 3-arg version */
      PRINT("sys_openat64 ( %ld, %#lx(%s), %ld )", SARG1, ARG2, (HChar*)ARG2,
            SARG3);
      PRE_REG_READ3(long, "openat64", int, fildes, const char *, filename,
                    int, flags);
   }

   PRE_MEM_RASCIIZ("openat64(filename)", ARG2);

   /* Be strict. */
   if (ARG1 != VKI_AT_FDCWD && !ML_(fd_allowed)(ARG1, "openat64", tid, False))
      SET_STATUS_Failure(VKI_EBADF);
}

POST(sys_openat64)
{
   if (!ML_(fd_allowed)(RES, "openat64", tid, True)) {
      VG_(close)(RES);
      SET_STATUS_Failure(VKI_EMFILE);
   }
   else if (VG_(clo_track_fds))
      ML_(record_fd_open_with_given_name)(tid, RES, (HChar*)ARG2);
}

PRE(sys_llseek32)
{
   /* offset_t llseek(int fildes, offset_t offset, int whence); */
   PRINT("sys_llseek32 ( %ld, %#lx, %#lx, %ld )", SARG1, ARG2, ARG3, SARG4);
   PRE_REG_READ4(long, "llseek", int, fildes, vki_u32, offset_low,
                 vki_u32, offset_high, int, whence);

   /* Stay sane. */
   if (!ML_(fd_allowed)(ARG1, "llseek", tid, False))
      SET_STATUS_Failure(VKI_EBADF);
}

PRE(sys_mmap64)
{
   /* void *mmap64(void *addr, size_t len, int prot, int flags,
                   int fildes, uint32_t offlo, uint32_t offhi); */
   /* Note this wrapper assumes a little-endian architecture, offlo and offhi
      have to be swapped if a big-endian architecture is present. */
#if !defined(VG_LITTLEENDIAN)
#error "Unexpected endianness."
#endif /* !VG_LITTLEENDIAN */

   SysRes r;
   ULong u;
   Off64T offset;

   /* Stay sane. */
   vg_assert(VKI_PAGE_SIZE == 4096);
   vg_assert(sizeof(u) == sizeof(offset));

   PRINT("sys_mmap ( %#lx, %#lx, %#lx, %#lx, %ld, %#lx, %#lx )",
         ARG1, ARG2, ARG3, ARG4, SARG5, ARG6, ARG7);
   PRE_REG_READ7(long, "mmap", void *, start, vki_size_t, length,
                 int, prot, int, flags, int, fd, uint32_t, offlo,
                 uint32_t, offhi);

   /* The offlo and offhi values can actually represent a negative value.
      Make sure it's passed correctly to the generic mmap wrapper. */
   u = ((ULong)ARG7 << 32) + ARG6;
   offset = *(Off64T*)&u;

   r = ML_(generic_PRE_sys_mmap)(tid, ARG1, ARG2, ARG3, ARG4, ARG5, offset);
   SET_STATUS_from_SysRes(r);
}

PRE(sys_stat64)
{
   /* int stat64(const char *path, struct stat64 *buf); */
   PRINT("sys_stat64 ( %#lx(%s), %#lx )", ARG1, (HChar*)ARG1, ARG2);
   PRE_REG_READ2(long, "stat64", const char *, path, struct stat64 *, buf);

   PRE_MEM_RASCIIZ("stat64(path)", ARG1);
   PRE_MEM_WRITE("stat64(buf)", ARG2, sizeof(struct vki_stat64));
}

POST(sys_stat64)
{
   POST_MEM_WRITE(ARG2, sizeof(struct vki_stat64));
}

PRE(sys_lstat64)
{
   /* int lstat64(const char *path, struct stat64 *buf); */
   PRINT("sys_lstat64 ( %#lx(%s), %#lx )", ARG1, (HChar*)ARG1, ARG2);
   PRE_REG_READ2(long, "lstat64", const char *, path, struct stat64 *, buf);

   PRE_MEM_RASCIIZ("lstat64(path)", ARG1);
   PRE_MEM_WRITE("lstat64(buf)", ARG2, sizeof(struct vki_stat64));
}

POST(sys_lstat64)
{
   POST_MEM_WRITE(ARG2, sizeof(struct vki_stat64));
}

PRE(sys_fstat64)
{
   /* int fstat64(int fildes, struct stat64 *buf); */
   PRINT("sys_fstat64 ( %ld, %#lx )", SARG1, ARG2);
   PRE_REG_READ2(long, "fstat64", int, fildes, struct stat64 *, buf);
   PRE_MEM_WRITE("fstat64(buf)", ARG2, sizeof(struct vki_stat64));

   /* Be strict. */
   if (!ML_(fd_allowed)(ARG1, "fstat64", tid, False))
      SET_STATUS_Failure(VKI_EBADF);
}

POST(sys_fstat64)
{
   POST_MEM_WRITE(ARG2, sizeof(struct vki_stat64));
}

static void do_statvfs64_post(struct vki_statvfs64 *stats, ThreadId tid)
{
   POST_FIELD_WRITE(stats->f_bsize);
   POST_FIELD_WRITE(stats->f_frsize);
   POST_FIELD_WRITE(stats->f_blocks);
   POST_FIELD_WRITE(stats->f_bfree);
   POST_FIELD_WRITE(stats->f_bavail);
   POST_FIELD_WRITE(stats->f_files);
   POST_FIELD_WRITE(stats->f_ffree);
   POST_FIELD_WRITE(stats->f_favail);
   POST_FIELD_WRITE(stats->f_fsid);
   POST_MEM_WRITE((Addr) stats->f_basetype, VG_(strlen)(stats->f_basetype) + 1);
   POST_FIELD_WRITE(stats->f_flag);
   POST_FIELD_WRITE(stats->f_namemax);
   POST_MEM_WRITE((Addr) stats->f_fstr, VG_(strlen)(stats->f_fstr) + 1);
}

PRE(sys_statvfs64)
{
   /* int statvfs64(const char *path, struct statvfs64 *buf); */
   *flags |= SfMayBlock;
   PRINT("sys_statvfs64 ( %#lx(%s), %#lx )", ARG1, (HChar *) ARG1, ARG2);
   PRE_REG_READ2(long, "statvfs64", const char *, path,
                 struct vki_statvfs64 *, buf);
   PRE_MEM_RASCIIZ("statvfs64(path)", ARG1);
   PRE_MEM_WRITE("statvfs64(buf)", ARG2, sizeof(struct vki_statvfs64));
}

POST(sys_statvfs64)
{
   do_statvfs64_post((struct vki_statvfs64 *) ARG2, tid);
}

PRE(sys_fstatvfs64)
{
   /* int fstatvfs64(int fd, struct statvfs64 *buf); */
   *flags |= SfMayBlock;
   PRINT("sys_fstatvfs64 ( %ld, %#lx )", SARG1, ARG2);
   PRE_REG_READ2(long, "fstatvfs64", int, fd, struct vki_statvfs64 *, buf);
   PRE_MEM_WRITE("fstatvfs64(buf)", ARG2, sizeof(struct vki_statvfs64));

   /* Be strict. */
   if (!ML_(fd_allowed)(ARG1, "fstatvfs64", tid, False))
      SET_STATUS_Failure(VKI_EBADF);
}

POST(sys_fstatvfs64)
{
   do_statvfs64_post((struct vki_statvfs64 *) ARG2, tid);
}

PRE(sys_setrlimit64)
{
   /* int setrlimit64(int resource, struct rlimit64 *rlim); */
   struct vki_rlimit64 *limit = (struct vki_rlimit64 *)ARG2;
   PRINT("sys_setrlimit64 ( %ld, %#lx )", SARG1, ARG2);
   PRE_REG_READ2(long, "setrlimit64", int, resource, struct rlimit64 *, rlim);
   PRE_MEM_READ("setrlimit64(rlim)", ARG2, sizeof(struct vki_rlimit64));

   if (limit && limit->rlim_cur > limit->rlim_max)
      SET_STATUS_Failure(VKI_EINVAL);
   else if (ARG1 == VKI_RLIMIT_NOFILE) {
      if (limit->rlim_cur > VG_(fd_hard_limit) ||
          limit->rlim_max != VG_(fd_hard_limit)) {
         SET_STATUS_Failure(VKI_EPERM);
      }
      else {
         VG_(fd_soft_limit) = limit->rlim_cur;
         SET_STATUS_Success(0);
      }
   }
   else if (ARG1 == VKI_RLIMIT_DATA) {
      if (limit->rlim_cur > VG_(client_rlimit_data).rlim_max ||
          limit->rlim_max > VG_(client_rlimit_data).rlim_max) {
         SET_STATUS_Failure(VKI_EPERM);
      }
      else {
         VG_(client_rlimit_data).rlim_max = limit->rlim_max;
         VG_(client_rlimit_data).rlim_cur = limit->rlim_cur;
         SET_STATUS_Success(0);
      }
   }
   else if (ARG1 == VKI_RLIMIT_STACK && tid == 1) {
      if (limit->rlim_cur > VG_(client_rlimit_stack).rlim_max ||
          limit->rlim_max > VG_(client_rlimit_stack).rlim_max) {
         SET_STATUS_Failure(VKI_EPERM);
      }
      else {
         /* Change the value of client_stack_szB to the rlim_cur value but
            only if it is smaller than the size of the allocated stack for the
            client. */
         if (limit->rlim_cur <= VG_(clstk_max_size))
            VG_(threads)[tid].client_stack_szB = limit->rlim_cur;

         VG_(client_rlimit_stack).rlim_max = limit->rlim_max;
         VG_(client_rlimit_stack).rlim_cur = limit->rlim_cur;
         SET_STATUS_Success(0);
      }
   }
}

PRE(sys_getrlimit64)
{
   /* int getrlimit64(int resource, struct rlimit64 *rlim); */
   PRINT("sys_getrlimit64 ( %ld, %#lx )", SARG1, ARG2);
   PRE_REG_READ2(long, "getrlimit64",
                 int, resource, struct rlimit64 *, rlim);
   PRE_MEM_WRITE("getrlimit64(rlim)", ARG2, sizeof(struct vki_rlimit64));
}

POST(sys_getrlimit64)
{
   /* Based on common_post_getrlimit() from syswrap-generic.c. */
   struct vki_rlimit64 *rlim = (struct vki_rlimit64*)ARG2;

   POST_MEM_WRITE(ARG2, sizeof(struct vki_rlimit64));

   switch (ARG1 /*resource*/) {
   case VKI_RLIMIT_NOFILE:
      rlim->rlim_cur = VG_(fd_soft_limit);
      rlim->rlim_max = VG_(fd_hard_limit);
      break;
   case VKI_RLIMIT_DATA:
      rlim->rlim_cur = VG_(client_rlimit_data).rlim_cur;
      rlim->rlim_max = VG_(client_rlimit_data).rlim_max;
      break;
   case VKI_RLIMIT_STACK:
      rlim->rlim_cur = VG_(client_rlimit_stack).rlim_cur;
      rlim->rlim_max = VG_(client_rlimit_stack).rlim_max;
      break;
   }
}

PRE(sys_pread64)
{
   /* ssize32_t pread64(int fd, void *buf, size32_t count,
                        uint32_t offset_1, uint32_t offset_2);
    */
   *flags |= SfMayBlock;
   PRINT("sys_pread64 ( %ld, %#lx, %lu, %#lx, %#lx )",
         SARG1, ARG2, ARG3, ARG4, ARG5);
   PRE_REG_READ5(long, "pread64", int, fd, void *, buf, vki_size32_t, count,
                 vki_uint32_t, offset_1, vki_uint32_t, offset_2);
   PRE_MEM_WRITE("pread64(buf)", ARG2, ARG3);

   /* Be strict. */
   if (!ML_(fd_allowed)(ARG1, "pread64", tid, False))
      SET_STATUS_Failure(VKI_EBADF);
}

POST(sys_pread64)
{
   POST_MEM_WRITE(ARG2, RES);
}

PRE(sys_pwrite64)
{
   /* ssize32_t pwrite64(int fd, void *buf, size32_t count,
                         uint32_t offset_1, uint32_t offset_2);
    */
   *flags |= SfMayBlock;
   PRINT("sys_pwrite64 ( %ld, %#lx, %lu, %#lx, %#lx )",
         SARG1, ARG2, ARG3, ARG4, ARG5);
   PRE_REG_READ5(long, "pwrite64", int, fd, void *, buf, vki_size32_t, count,
                 vki_uint32_t, offset_1, vki_uint32_t, offset_2);
   PRE_MEM_READ("pwrite64(buf)", ARG2, ARG3);

   /* Be strict. */
   if (!ML_(fd_allowed)(ARG1, "pwrite64", tid, False))
      SET_STATUS_Failure(VKI_EBADF);
}

PRE(sys_open64)
{
   /* int open64(const char *filename, int flags);
      int open64(const char *filename, int flags, mode_t mode); */
   *flags |= SfMayBlock;

   if (ARG2 & VKI_O_CREAT) {
      /* 3-arg version */
      PRINT("sys_open64 ( %#lx(%s), %#lx, %ld )", ARG1, (HChar*)ARG1, ARG2,
            SARG3);
      PRE_REG_READ3(long, "open64", const char *, filename, int, flags,
                    vki_mode_t, mode);
   }
   else {
      /* 2-arg version */
      PRINT("sys_open64 ( %#lx(%s), %#lx )", ARG1, (HChar*)ARG1, ARG2);
      PRE_REG_READ2(long, "open64", const char *, filename, int, flags);
   }
   PRE_MEM_RASCIIZ("open(filename)", ARG1);
}

POST(sys_open64)
{
   if (!ML_(fd_allowed)(RES, "open64", tid, True)) {
      VG_(close)(RES);
      SET_STATUS_Failure(VKI_EMFILE);
   }
   else if (VG_(clo_track_fds))
      ML_(record_fd_open_with_given_name)(tid, RES, (HChar*)ARG1);
}

#undef PRE
#undef POST

#endif // defined(VGP_x86_solaris)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
