
/*--------------------------------------------------------------------*/
/*--- Dumping core.                                 coredump-elf.c ---*/
/*--------------------------------------------------------------------*/
 
/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2017 Julian Seward 
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
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

#if defined(VGO_linux) || defined(VGO_freebsd)

#include "pub_core_basics.h"
#include "pub_core_vki.h"
#include "pub_core_aspacehl.h"
#include "pub_core_aspacemgr.h"
#include "pub_core_libcbase.h"
#include "pub_core_machine.h"
#include "pub_core_coredump.h"
#include "pub_core_libcprint.h"
#include "pub_core_libcfile.h"    // VG_(close) et al
#include "pub_core_libcproc.h"    // VG_(geteuid), VG_(getegid)
#include "pub_core_libcassert.h"  // VG_(exit), vg_assert
#include "pub_core_mallocfree.h"  // VG_(malloc), VG_(free)
#include "pub_core_threadstate.h"
#include "pub_core_xarray.h"
#include "pub_core_clientstate.h"
#include "pub_core_options.h"

/*
  Dump core
   
  Generate a standard ELF core file corresponding to the client state
  at the time of a crash.
 */
#include <elf.h>
#ifndef NT_PRXFPREG
#define NT_PRXFPREG    0x46e62b7f /* copied from gdb5.1/include/elf/common.h */
#endif /* NT_PRXFPREG */

#if	VG_WORDSIZE == 8
#define ESZ(x)	Elf64_##x
#elif	VG_WORDSIZE == 4
#define ESZ(x)	Elf32_##x
#else
#error VG_WORDSIZE needs to ==4 or ==8
#endif

/* If true, then this Segment may be mentioned in the core */
static Bool may_dump(const NSegment *seg)
{
   if (seg->kind == SkAnonC ||
       seg->kind == SkShmC ||
       (seg->kind == SkFileC &&
        !VKI_S_ISCHR(seg->mode) && !VKI_S_ISBLK(seg->mode)))
      return True;

   return False;
}

/* If true, then this Segment's contents will be in the core */
static Bool should_dump(const NSegment *seg)
{
   return may_dump(seg); // && seg->hasW;
}

static void fill_ehdr(ESZ(Ehdr) *ehdr, Int num_phdrs)
{
   VG_(memset)(ehdr, 0, sizeof(*ehdr));

   VG_(memcpy)(ehdr->e_ident, ELFMAG, SELFMAG);
   ehdr->e_ident[EI_CLASS]   = VG_ELF_CLASS;
   ehdr->e_ident[EI_DATA]    = VG_ELF_DATA2XXX;
   ehdr->e_ident[EI_VERSION] = EV_CURRENT;
#if defined(VGO_freebsd)
   ehdr->e_ident[EI_OSABI]   = ELFOSABI_FREEBSD;
#endif

   ehdr->e_type = ET_CORE;
   ehdr->e_machine = VG_ELF_MACHINE;
   ehdr->e_version = EV_CURRENT;
   ehdr->e_entry = 0;
   ehdr->e_phoff = sizeof(ESZ(Ehdr));
   ehdr->e_shoff = 0;
#if defined(VGP_nanomips_linux)
   ehdr->e_flags = VKI_EF_NANOMIPS_ABI_P32;
#else
   ehdr->e_flags = 0;
#endif
   ehdr->e_ehsize = sizeof(ESZ(Ehdr));
   ehdr->e_phentsize = sizeof(ESZ(Phdr));
   ehdr->e_phnum = num_phdrs;
   ehdr->e_shentsize = 0;
   ehdr->e_shnum = 0;
   ehdr->e_shstrndx = 0;

}

static void fill_phdr(ESZ(Phdr) *phdr, const NSegment *seg, UInt off, Bool write)
{
   SizeT len = seg->end - seg->start + 1;

   write = write && should_dump(seg);

   VG_(memset)(phdr, 0, sizeof(*phdr));

   phdr->p_type = PT_LOAD;
   phdr->p_offset = off;
   phdr->p_vaddr = seg->start;
   phdr->p_paddr = 0;
   phdr->p_filesz = write ? len : 0;
   phdr->p_memsz = len;
   phdr->p_flags = 0;

   if (seg->hasR)
      phdr->p_flags |= PF_R;
   if (seg->hasW)
      phdr->p_flags |= PF_W;
   if (seg->hasX)
      phdr->p_flags |= PF_X;

   phdr->p_align = VKI_PAGE_SIZE;
}

#if defined(VGPV_arm_linux_android) || defined(VGPV_x86_linux_android) \
    || defined(VGPV_mips32_linux_android)
/* Android's libc doesn't provide a definition for this.  Hence: */
typedef
   struct {
      Elf32_Word n_namesz;
      Elf32_Word n_descsz;
      Elf32_Word n_type;
   }
   Elf32_Nhdr;
#endif

struct note {
   struct note *next;
   ESZ(Nhdr) note;
   HChar name[0];
};

static UInt note_size(const struct note *n)
{
   return sizeof(ESZ(Nhdr)) + VG_ROUNDUP(VG_(strlen)(n->name)+1, 4)
                            + VG_ROUNDUP(n->note.n_descsz, 4);
}

#if !defined(VGPV_arm_linux_android) \
    && !defined(VGPV_x86_linux_android) \
    && !defined(VGPV_mips32_linux_android) \
    && !defined(VGPV_arm64_linux_android)
static void add_note(struct note **list, const HChar *name, UInt type,
                     const void *data, UInt datasz)
{
   Int namelen = VG_(strlen)(name)+1;
   Int notelen = sizeof(struct note) + 
      VG_ROUNDUP(namelen, 4) + 
      VG_ROUNDUP(datasz, 4);
   struct note *n = VG_(malloc)("coredump-elf.an.1", notelen);

   VG_(memset)(n, 0, notelen);

   n->next = *list;
   *list = n;

   n->note.n_type = type;
   n->note.n_namesz = namelen;
   n->note.n_descsz = datasz;

   VG_(memcpy)(n->name, name, namelen);
   VG_(memcpy)(n->name+VG_ROUNDUP(namelen,4), data, datasz);
}
#endif /* !defined(VGPV_*_linux_android) */

static void write_note(Int fd, const struct note *n)
{
   VG_(write)(fd, &n->note, note_size(n));
}

#if defined(VGO_freebsd)
static void fill_prpsinfo(const ThreadState *tst,
                          struct vki_elf_prpsinfo *prpsinfo)
{
   VG_(memset)(prpsinfo, 0, sizeof(*prpsinfo));

   prpsinfo->pr_version = VKI_PRPSINFO_VERSION;
   prpsinfo->pr_psinfosz = sizeof(struct vki_elf_prpsinfo);
   VG_(client_fname)(prpsinfo->pr_fname, sizeof(prpsinfo->pr_fname), False);
   // why?
   VG_(strncpy)(prpsinfo->pr_psargs, prpsinfo->pr_fname, sizeof(prpsinfo->pr_psargs) - 1);
}
#else
static void fill_prpsinfo(const ThreadState *tst,
                          struct vki_elf_prpsinfo *prpsinfo)
{
   VG_(memset)(prpsinfo, 0, sizeof(*prpsinfo));

   switch(tst->status) {
   case VgTs_Runnable:
   case VgTs_Yielding:
      prpsinfo->pr_sname = 'R';
      break;

   case VgTs_WaitSys:
      prpsinfo->pr_sname = 'S';
      break;

   case VgTs_Zombie:
      prpsinfo->pr_sname = 'Z';
      break;

   case VgTs_Empty:
   case VgTs_Init:
      prpsinfo->pr_sname = '?';
      break;
   }

   prpsinfo->pr_uid = 0;
   prpsinfo->pr_gid = 0;
   
   VG_(client_fname)(prpsinfo->pr_fname, sizeof(prpsinfo->pr_fname), False);
}
#endif

static void fill_prstatus(const ThreadState *tst, 
			  /*OUT*/struct vki_elf_prstatus *prs, 
			  const vki_siginfo_t *si)
{
#if defined(VGP_mips32_linux) || defined(VGP_mips64_linux) \
    || defined(VGP_nanomips_linux)
   vki_elf_greg_t *regs;
#else
   struct vki_user_regs_struct *regs;
#endif
   const ThreadArchState* arch = &tst->arch;

   VG_(memset)(prs, 0, sizeof(*prs));

#if defined(VGO_freebsd)
   prs->pr_version = VKI_PRSTATUS_VERSION;
   prs->pr_statussz = sizeof(struct vki_elf_prstatus);
   prs->pr_gregsetsz = sizeof(vki_elf_gregset_t);
   prs->pr_fpregsetsz = sizeof(vki_elf_fpregset_t);
   prs->pr_osreldate = VG_(getosreldate)();

   prs->pr_cursig = si->si_signo;
   prs->pr_pid = tst->os_state.lwpid;
#else
   prs->pr_info.si_signo = si->si_signo;
   prs->pr_info.si_code = si->si_code;
   prs->pr_info.si_errno = 0;

   prs->pr_cursig = si->si_signo;

   prs->pr_pid = tst->os_state.lwpid;
   prs->pr_ppid = 0;
   prs->pr_pgrp = VG_(getpgrp)();
   prs->pr_sid = VG_(getpgrp)();
#endif
   
#if defined(VGP_s390x_linux)
   /* prs->pr_reg has struct type. Need to take address. */
   regs = (struct vki_user_regs_struct *)&(prs->pr_reg);
#elif defined(VGP_mips32_linux) || defined(VGP_mips64_linux) \
   || defined(VGP_nanomips_linux)
   regs = (vki_elf_greg_t *)prs->pr_reg;
#else
   regs = (struct vki_user_regs_struct *)prs->pr_reg;
   vg_assert(sizeof(*regs) == sizeof(prs->pr_reg));
#endif

#if defined(VGP_x86_linux)
   regs->eflags = LibVEX_GuestX86_get_eflags( &arch->vex );
   regs->esp    = arch->vex.guest_ESP;
   regs->eip    = arch->vex.guest_EIP;

   regs->ebx    = arch->vex.guest_EBX;
   regs->ecx    = arch->vex.guest_ECX;
   regs->edx    = arch->vex.guest_EDX;
   regs->esi    = arch->vex.guest_ESI;
   regs->edi    = arch->vex.guest_EDI;
   regs->ebp    = arch->vex.guest_EBP;
   regs->eax    = arch->vex.guest_EAX;

   regs->cs     = arch->vex.guest_CS;
   regs->ds     = arch->vex.guest_DS;
   regs->ss     = arch->vex.guest_SS;
   regs->es     = arch->vex.guest_ES;
   regs->fs     = arch->vex.guest_FS;
   regs->gs     = arch->vex.guest_GS;

#elif defined(VGP_amd64_linux)
   regs->eflags = LibVEX_GuestAMD64_get_rflags( &arch->vex );
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

#elif defined(VGP_ppc32_linux)
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
   regs->xer = LibVEX_GuestPPC32_get_XER( &arch->vex );
   regs->ccr = LibVEX_GuestPPC32_get_CR( &arch->vex );
   regs->mq = 0;
   regs->trap = 0;
   regs->dar = 0; /* should be fault address? */
   regs->dsisr = 0;
   regs->result = 0;

#elif defined(VGP_ppc64be_linux)
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
   regs->xer = LibVEX_GuestPPC64_get_XER( &arch->vex );
   regs->ccr = LibVEX_GuestPPC64_get_CR( &arch->vex );
   /* regs->mq = 0; */
   regs->trap = 0;
   regs->dar = 0; /* should be fault address? */
   regs->dsisr = 0;
   regs->result = 0;

#elif defined(VGP_ppc64le_linux)
#  define DO(n)  regs->gpr[n] = arch->vex.guest_GPR##n
   DO(0);  DO(1);  DO(2);  DO(3);  DO(4);  DO(5);  DO(6);  DO(7);
   DO(8);  DO(9);  DO(10); DO(11); DO(12); DO(13); DO(14); DO(15);
   DO(16); DO(17); DO(18); DO(19); DO(20); DO(21); DO(22); DO(23);
   DO(24); DO(25); DO(26); DO(27); DO(28); DO(29); DO(30); DO(31);
#  undef DO

   regs->nip = arch->vex.guest_CIA;
   regs->msr = 0xf033;   /* pretty arbitrary */
   regs->orig_gpr3 = arch->vex.guest_GPR3;
   regs->ctr = arch->vex.guest_CTR;
   regs->link = arch->vex.guest_LR;
   regs->xer = LibVEX_GuestPPC64_get_XER( &(arch->vex) );
   regs->ccr = LibVEX_GuestPPC64_get_CR( &(arch->vex) );
   /* regs->mq = 0; */
   regs->trap = 0;
   regs->dar = 0; /* should be fault address? */
   regs->dsisr = 0;
   regs->result = 0;

#elif defined(VGP_arm_linux)
   regs->ARM_r0   = arch->vex.guest_R0;
   regs->ARM_r1   = arch->vex.guest_R1;
   regs->ARM_r2   = arch->vex.guest_R2;
   regs->ARM_r3   = arch->vex.guest_R3;
   regs->ARM_r4   = arch->vex.guest_R4;
   regs->ARM_r5   = arch->vex.guest_R5;
   regs->ARM_r6   = arch->vex.guest_R6;
   regs->ARM_r7   = arch->vex.guest_R7;
   regs->ARM_r8   = arch->vex.guest_R8;
   regs->ARM_r9   = arch->vex.guest_R9;
   regs->ARM_r10  = arch->vex.guest_R10;
   regs->ARM_fp   = arch->vex.guest_R11;
   regs->ARM_ip   = arch->vex.guest_R12;
   regs->ARM_sp   = arch->vex.guest_R13;
   regs->ARM_lr   = arch->vex.guest_R14;
   regs->ARM_pc   = arch->vex.guest_R15T;
   regs->ARM_cpsr = LibVEX_GuestARM_get_cpsr( &arch->vex );

#elif defined(VGP_arm64_linux)
   regs->regs[0]  = arch->vex.guest_X0;
   regs->regs[1]  = arch->vex.guest_X1;
   regs->regs[2]  = arch->vex.guest_X2;
   regs->regs[3]  = arch->vex.guest_X3;
   regs->regs[4]  = arch->vex.guest_X4;
   regs->regs[5]  = arch->vex.guest_X5;
   regs->regs[6]  = arch->vex.guest_X6;
   regs->regs[7]  = arch->vex.guest_X7;
   regs->regs[8]  = arch->vex.guest_X8;
   regs->regs[9]  = arch->vex.guest_X9;
   regs->regs[10] = arch->vex.guest_X10;
   regs->regs[11] = arch->vex.guest_X11;
   regs->regs[12] = arch->vex.guest_X12;
   regs->regs[13] = arch->vex.guest_X13;
   regs->regs[14] = arch->vex.guest_X14;
   regs->regs[15] = arch->vex.guest_X15;
   regs->regs[16] = arch->vex.guest_X16;
   regs->regs[17] = arch->vex.guest_X17;
   regs->regs[18] = arch->vex.guest_X18;
   regs->regs[19] = arch->vex.guest_X19;
   regs->regs[20] = arch->vex.guest_X20;
   regs->regs[21] = arch->vex.guest_X21;
   regs->regs[22] = arch->vex.guest_X22;
   regs->regs[23] = arch->vex.guest_X23;
   regs->regs[24] = arch->vex.guest_X24;
   regs->regs[25] = arch->vex.guest_X25;
   regs->regs[26] = arch->vex.guest_X26;
   regs->regs[27] = arch->vex.guest_X27;
   regs->regs[28] = arch->vex.guest_X28;
   regs->regs[29] = arch->vex.guest_X29;
   regs->regs[30] = arch->vex.guest_X30;
   regs->sp       = arch->vex.guest_XSP;
   regs->pc       = arch->vex.guest_PC;
   regs->pstate  = LibVEX_GuestARM64_get_nzcv( &arch->vex ); /* is this correct? */

#elif defined(VGP_s390x_linux)
#  define DO(n)  regs->gprs[n] = arch->vex.guest_r##n
   DO(0);  DO(1);  DO(2);  DO(3);  DO(4);  DO(5);  DO(6);  DO(7);
   DO(8);  DO(9);  DO(10); DO(11); DO(12); DO(13); DO(14); DO(15);
#  undef DO
#  define DO(n)  regs->acrs[n] = arch->vex.guest_a##n
   DO(0);  DO(1);  DO(2);  DO(3);  DO(4);  DO(5);  DO(6);  DO(7);
   DO(8);  DO(9);  DO(10); DO(11); DO(12); DO(13); DO(14); DO(15);
#  undef DO
   regs->orig_gpr2 = arch->vex.guest_r2;

#elif defined(VGP_mips32_linux)
#  define DO(n)  regs[VKI_MIPS32_EF_R##n] = arch->vex.guest_r##n
   DO(1);  DO(2);  DO(3);  DO(4);  DO(5);  DO(6);  DO(7);  DO(8);
   DO(9);  DO(10); DO(11); DO(12); DO(13); DO(14); DO(15); DO(16);
   DO(17); DO(18); DO(19); DO(20); DO(21); DO(22); DO(23); DO(24);
   DO(25); DO(28); DO(29); DO(30); DO(31);
#  undef DO
   regs[VKI_MIPS32_EF_LO]         = arch->vex.guest_LO;
   regs[VKI_MIPS32_EF_HI]         = arch->vex.guest_HI;
   regs[VKI_MIPS32_EF_CP0_STATUS] = arch->vex.guest_CP0_status;
   regs[VKI_MIPS32_EF_CP0_EPC]    = arch->vex.guest_PC;
#elif defined(VGP_mips64_linux)
#  define DO(n)  regs[VKI_MIPS64_EF_R##n] = arch->vex.guest_r##n
   DO(1);  DO(2);  DO(3);  DO(4);  DO(5);  DO(6);  DO(7);  DO(8);
   DO(9);  DO(10); DO(11); DO(12); DO(13); DO(14); DO(15); DO(16);
   DO(17); DO(18); DO(19); DO(20); DO(21); DO(22); DO(23); DO(24);
   DO(25); DO(28); DO(29); DO(30); DO(31);
#  undef DO
   regs[VKI_MIPS64_EF_LO]         = arch->vex.guest_LO;
   regs[VKI_MIPS64_EF_HI]         = arch->vex.guest_HI;
   regs[VKI_MIPS64_EF_CP0_STATUS] = arch->vex.guest_CP0_status;
   regs[VKI_MIPS64_EF_CP0_EPC]    = arch->vex.guest_PC;
#elif defined(VGP_nanomips_linux)
#  define DO(n)  regs[VKI_MIPS32_EF_R##n] = arch->vex.guest_r##n
   DO(1);  DO(2);  DO(3);  DO(4);  DO(5);  DO(6);  DO(7);  DO(8);
   DO(9);  DO(10); DO(11); DO(12); DO(13); DO(14); DO(15); DO(16);
   DO(17); DO(18); DO(19); DO(20); DO(21); DO(22); DO(23); DO(24);
   DO(25); DO(28); DO(29); DO(30); DO(31);
   regs[VKI_MIPS32_EF_CP0_STATUS] = arch->vex.guest_CP0_status;
   regs[VKI_MIPS32_EF_CP0_EPC]    = arch->vex.guest_PC;
#  undef DO
#elif defined(VGP_amd64_freebsd)
   regs->rflags = LibVEX_GuestAMD64_get_rflags( &arch->vex );
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
#elif defined(VGP_x86_freebsd)
   regs->eflags = LibVEX_GuestX86_get_eflags( &arch->vex );
   regs->esp    = arch->vex.guest_ESP;
   regs->eip    = arch->vex.guest_EIP;

   regs->ebx    = arch->vex.guest_EBX;
   regs->ecx    = arch->vex.guest_ECX;
   regs->edx    = arch->vex.guest_EDX;
   regs->esi    = arch->vex.guest_ESI;
   regs->edi    = arch->vex.guest_EDI;
   regs->ebp    = arch->vex.guest_EBP;
   regs->eax    = arch->vex.guest_EAX;

   regs->cs     = arch->vex.guest_CS;
   regs->ds     = arch->vex.guest_DS;
   regs->ss     = arch->vex.guest_SS;
   regs->es     = arch->vex.guest_ES;
   regs->fs     = arch->vex.guest_FS;
   regs->gs     = arch->vex.guest_GS;

#else
#  error Unknown ELF platform
#endif
}

static void fill_fpu(const ThreadState *tst, vki_elf_fpregset_t *fpu)
{
   __attribute__((unused))
   const ThreadArchState* arch = &tst->arch;

#if defined(VGP_x86_linux)
//:: static void fill_fpu(vki_elf_fpregset_t *fpu, const HChar *from)
//:: {
//::    if (VG_(have_ssestate)) {
//::       UShort *to;
//::       Int i;
//:: 
//::       /* This is what the kernel does */
//::       VG_(memcpy)(fpu, from, 7*sizeof(long));
//::    
//::       to = (UShort *)&fpu->st_space[0];
//::       from += 18 * sizeof(UShort);
//:: 
//::       for (i = 0; i < 8; i++, to += 5, from += 8) 
//:: 	 VG_(memcpy)(to, from, 5*sizeof(UShort));
//::    } else
//::       VG_(memcpy)(fpu, from, sizeof(*fpu));
//:: }

//::    fill_fpu(fpu, (const HChar *)&arch->m_sse);

#elif defined(VGP_amd64_linux)
//::    fpu->cwd = ?;
//::    fpu->swd = ?;
//::    fpu->twd = ?;
//::    fpu->fop = ?;
//::    fpu->rip = ?;
//::    fpu->rdp = ?;
//::    fpu->mxcsr = ?;
//::    fpu->mxcsr_mask = ?;
//::    fpu->st_space = ?;

#  define DO(n)  VG_(memcpy)(fpu->xmm_space + n * 4, \
                             &arch->vex.guest_YMM##n[0], 16)
   DO(0);  DO(1);  DO(2);  DO(3);  DO(4);  DO(5);  DO(6);  DO(7);
   DO(8);  DO(9);  DO(10); DO(11); DO(12); DO(13); DO(14); DO(15);
#  undef DO

   VG_(memset)(fpu->padding, 0, sizeof(fpu->padding));

#elif defined(VGP_ppc32_linux)
   /* The guest state has the FPR fields declared as ULongs, so need
      to fish out the values without converting them.
      NOTE: The 32 FP registers map to the first 32 VSX registers.*/
#  define DO(n)  (*fpu)[n] = *(double*)(&arch->vex.guest_VSR##n)
   DO(0);  DO(1);  DO(2);  DO(3);  DO(4);  DO(5);  DO(6);  DO(7);
   DO(8);  DO(9);  DO(10); DO(11); DO(12); DO(13); DO(14); DO(15);
   DO(16); DO(17); DO(18); DO(19); DO(20); DO(21); DO(22); DO(23);
   DO(24); DO(25); DO(26); DO(27); DO(28); DO(29); DO(30); DO(31);
#  undef DO

#elif defined(VGP_ppc64be_linux) || defined(VGP_ppc64le_linux)
   /* The guest state has the FPR fields declared as ULongs, so need
      to fish out the values without converting them.
      NOTE: The 32 FP registers map to the first 32 VSX registers.*/
#  define DO(n)  (*fpu)[n] = *(double*)(&arch->vex.guest_VSR##n)
   DO(0);  DO(1);  DO(2);  DO(3);  DO(4);  DO(5);  DO(6);  DO(7);
   DO(8);  DO(9);  DO(10); DO(11); DO(12); DO(13); DO(14); DO(15);
   DO(16); DO(17); DO(18); DO(19); DO(20); DO(21); DO(22); DO(23);
   DO(24); DO(25); DO(26); DO(27); DO(28); DO(29); DO(30); DO(31);
#  undef DO

#elif defined(VGP_arm_linux)
   // umm ...

#elif defined(VGP_arm64_linux)
    fpu->vregs[0]  = *(const __uint128_t*)arch->vex.guest_Q0;
    fpu->vregs[1]  = *(const __uint128_t*)arch->vex.guest_Q1;
    fpu->vregs[2]  = *(const __uint128_t*)arch->vex.guest_Q2;
    fpu->vregs[3]  = *(const __uint128_t*)arch->vex.guest_Q3;
    fpu->vregs[4]  = *(const __uint128_t*)arch->vex.guest_Q4;
    fpu->vregs[5]  = *(const __uint128_t*)arch->vex.guest_Q5;
    fpu->vregs[6]  = *(const __uint128_t*)arch->vex.guest_Q6;
    fpu->vregs[7]  = *(const __uint128_t*)arch->vex.guest_Q7;
    fpu->vregs[8]  = *(const __uint128_t*)arch->vex.guest_Q8;
    fpu->vregs[9]  = *(const __uint128_t*)arch->vex.guest_Q9;
    fpu->vregs[10] = *(const __uint128_t*)arch->vex.guest_Q10;
    fpu->vregs[11] = *(const __uint128_t*)arch->vex.guest_Q11;
    fpu->vregs[12] = *(const __uint128_t*)arch->vex.guest_Q12;
    fpu->vregs[13] = *(const __uint128_t*)arch->vex.guest_Q13;
    fpu->vregs[14] = *(const __uint128_t*)arch->vex.guest_Q14;
    fpu->vregs[15] = *(const __uint128_t*)arch->vex.guest_Q15;
    fpu->vregs[16] = *(const __uint128_t*)arch->vex.guest_Q16;
    fpu->vregs[17] = *(const __uint128_t*)arch->vex.guest_Q17;
    fpu->vregs[18] = *(const __uint128_t*)arch->vex.guest_Q18;
    fpu->vregs[19] = *(const __uint128_t*)arch->vex.guest_Q19;
    fpu->vregs[20] = *(const __uint128_t*)arch->vex.guest_Q20;
    fpu->vregs[21] = *(const __uint128_t*)arch->vex.guest_Q21;
    fpu->vregs[22] = *(const __uint128_t*)arch->vex.guest_Q22;
    fpu->vregs[23] = *(const __uint128_t*)arch->vex.guest_Q23;
    fpu->vregs[24] = *(const __uint128_t*)arch->vex.guest_Q24;
    fpu->vregs[25] = *(const __uint128_t*)arch->vex.guest_Q25;
    fpu->vregs[26] = *(const __uint128_t*)arch->vex.guest_Q26;
    fpu->vregs[27] = *(const __uint128_t*)arch->vex.guest_Q27;
    fpu->vregs[28] = *(const __uint128_t*)arch->vex.guest_Q28;
    fpu->vregs[29] = *(const __uint128_t*)arch->vex.guest_Q29;
    fpu->vregs[30] = *(const __uint128_t*)arch->vex.guest_Q30;
    fpu->vregs[31] = *(const __uint128_t*)arch->vex.guest_Q31;
    fpu->fpsr      = *(const __vki_u32*)arch->vex.guest_QCFLAG;
    fpu->fpcr      = arch->vex.guest_FPCR;

#elif defined(VGP_s390x_linux)
   /* NOTE: The 16 FP registers map to the first 16 VSX registers. */
#  define DO(n)  fpu->fprs[n].ui = *(const Double*)(&arch->vex.guest_v##n.w64[0])
   DO(0);  DO(1);  DO(2);  DO(3);  DO(4);  DO(5);  DO(6);  DO(7);
   DO(8);  DO(9);  DO(10); DO(11); DO(12); DO(13); DO(14); DO(15);
# undef DO
#elif defined(VGP_mips32_linux) || defined(VGP_mips64_linux)
#  define DO(n)  (*fpu)[n] = *(const double*)(&arch->vex.guest_f##n)
   DO(0);  DO(1);  DO(2);  DO(3);  DO(4);  DO(5);  DO(6);  DO(7);
   DO(8);  DO(9);  DO(10); DO(11); DO(12); DO(13); DO(14); DO(15);
   DO(16); DO(17); DO(18); DO(19); DO(20); DO(21); DO(22); DO(23);
   DO(24); DO(25); DO(26); DO(27); DO(28); DO(29); DO(30); DO(31);
#  undef DO
#elif defined(VGP_nanomips_linux)

#elif defined(VGP_x86_freebsd)

#elif defined(VGP_amd64_freebsd)

#  define DO(n)  VG_(memcpy)(fpu->xmm_space + n * 4, \
                             &arch->vex.guest_YMM##n[0], 16)
   DO(0);  DO(1);  DO(2);  DO(3);  DO(4);  DO(5);  DO(6);  DO(7);
   DO(8);  DO(9);  DO(10); DO(11); DO(12); DO(13); DO(14); DO(15);
#  undef DO

#else
#  error Unknown ELF platform
#endif
}

#if defined(VGP_x86_linux) && !defined(VGPV_x86_linux_android)
static void fill_xfpu(const ThreadState *tst, vki_elf_fpxregset_t *xfpu)
{
   const ThreadArchState* arch = &tst->arch;

//::    xfpu->cwd = ?;
//::    xfpu->swd = ?;
//::    xfpu->twd = ?;
//::    xfpu->fop = ?;
//::    xfpu->fip = ?;
//::    xfpu->fcs = ?;
//::    xfpu->foo = ?;
//::    xfpu->fos = ?;
//::    xfpu->mxcsr = ?;
   xfpu->reserved = 0;
//::    xfpu->st_space = ?;

#  define DO(n)  VG_(memcpy)(xfpu->xmm_space + n * 4, &arch->vex.guest_XMM##n, sizeof(arch->vex.guest_XMM##n))
   DO(0);  DO(1);  DO(2);  DO(3);  DO(4);  DO(5);  DO(6);  DO(7);
#  undef DO

   VG_(memset)(xfpu->padding, 0, sizeof(xfpu->padding));
}
#endif

static
void dump_one_thread(struct note **notelist, const vki_siginfo_t *si, ThreadId tid)
{
   vki_elf_fpregset_t  fpu;
   struct vki_elf_prstatus prstatus;
   const HChar* name;
#  if   !defined(VGO_freebsd)
   name = "CORE";
#  else
   /* lldb on FreeBSD expects a prstatus with name "FreeBSD"
    * see llvm::Error ProcessElfCore::parseFreeBSDNotes(llvm::ArrayRef<CoreNote> notes) in ProcessElfCore.cpp
    * Otherwise it exits with "Could not find NT_PRSTATUS note in core file." */
   name = "FreeBSD";
#  endif
   VG_(memset)(&fpu, 0, sizeof(fpu));
   VG_(memset)(&prstatus, 0, sizeof(prstatus));
#     if defined(VGP_x86_linux) && !defined(VGPV_x86_linux_android)
      {
         vki_elf_fpxregset_t xfpu;
         VG_(memset)(&xfpu, 0, sizeof(xfpu));
         fill_xfpu(&VG_(threads)[tid], &xfpu);
         add_note(notelist, "LINUX", NT_PRXFPREG, &xfpu, sizeof(xfpu));
      }
#     endif

      fill_fpu(&VG_(threads)[tid], &fpu);
#     if !defined(VGPV_arm_linux_android) \
         && !defined(VGPV_x86_linux_android) \
         && !defined(VGPV_mips32_linux_android) \
         && !defined(VGPV_arm64_linux_android) \
         && !defined(VGP_nanomips_linux)
      add_note(notelist, name, NT_FPREGSET, &fpu, sizeof(fpu));
#     endif

      fill_prstatus(&VG_(threads)[tid], &prstatus, si);
#     if !defined(VGPV_arm_linux_android) \
         && !defined(VGPV_x86_linux_android) \
         && !defined(VGPV_mips32_linux_android) \
         && !defined(VGPV_arm64_linux_android)
      add_note(notelist, name, NT_PRSTATUS, &prstatus, sizeof(prstatus));
#     endif
}

static
void make_elf_coredump(ThreadId tid, const vki_siginfo_t *si, ULong max_size)
{
   HChar* buf = NULL;
   HChar *basename;
   const HChar *coreext = "";
   Int seq = 0;
   Int core_fd;
   NSegment const * seg;
   ESZ(Ehdr) ehdr;
   ESZ(Phdr) *phdrs = NULL;
   Int num_phdrs;
   Int i, idx;
   UInt off;
   struct note *notelist, *note;
   UInt notesz;
   struct vki_elf_prpsinfo prpsinfo;
   Addr *seg_starts = NULL;
   Int n_seg_starts;

   if (VG_(clo_log_fname_unexpanded) != NULL) {
      coreext = ".core";
      basename = VG_(expand_file_name)("--log-file",
                                       VG_(clo_log_fname_unexpanded));
   } else
      basename = VG_(strdup)("coredump-elf.mec.1", "vgcore");

   vg_assert(coreext);
   vg_assert(basename);
   buf = VG_(malloc)( "coredump-elf.mec.1",
                      VG_(strlen)(coreext) + VG_(strlen)(basename)
                         + 100/*for the two %ds. */ );

   for(;;) {
      Int oflags = VKI_O_CREAT|VKI_O_WRONLY|VKI_O_EXCL|VKI_O_TRUNC;
      SysRes sres;

      if (seq == 0)
	 VG_(sprintf)(buf, "%s%s.%d",
		      basename, coreext, VG_(getpid)());
      else
	 VG_(sprintf)(buf, "%s%s.%d.%d",
		      basename, coreext, VG_(getpid)(), seq);
      seq++;

#     if defined(VKI_O_LARGEFILE)
      oflags |= VKI_O_LARGEFILE;
#     endif

      sres = VG_(open)(buf, oflags, VKI_S_IRUSR|VKI_S_IWUSR);
      if (!sr_isError(sres)) {
         core_fd = sr_Res(sres);
	 break;
      }

      if (sr_isError(sres) && sr_Err(sres) != VKI_EEXIST)
	 goto cleanup; /* can't create file */
   }

   /* Get the client segments */
   seg_starts = VG_(get_segment_starts)(SkFileC | SkAnonC | SkShmC,
                                        &n_seg_starts);

   /* First, count how many memory segments to dump */
   num_phdrs = 1;		/* start with notes */
   for(i = 0; i < n_seg_starts; i++) {
      if (!may_dump(VG_(am_find_nsegment)(seg_starts[i])))
	 continue;

      num_phdrs++;
   }

   fill_ehdr(&ehdr, num_phdrs);

   notelist = NULL;

   /* Second, work out their layout */
   phdrs = VG_(malloc)("coredump-elf.mec.1", sizeof(*phdrs) * num_phdrs);

   /* Add details for all threads except the one that faulted */
   for(i = 1; i < VG_N_THREADS; i++) {

      if (VG_(threads)[i].status == VgTs_Empty)
	 continue;

      if (i == tid)
	 continue;

      dump_one_thread(&notelist, si, i);
   }

   /* Add details for the faulting thread. Note that because we are
      adding to the head of a linked list this thread will actually
      come out first in the core file, which seems to be how
      debuggers determine that it is the faulting thread. */
   dump_one_thread(&notelist, si, tid);

   fill_prpsinfo(&VG_(threads)[tid], &prpsinfo);
#  if !defined(VGPV_arm_linux_android) \
      && !defined(VGPV_x86_linux_android) \
      && !defined(VGPV_mips32_linux_android) \
      && !defined(VGPV_arm64_linux_android)
   add_note(&notelist, "CORE", NT_PRPSINFO, &prpsinfo, sizeof(prpsinfo));
#  endif

   for (note = notelist, notesz = 0; note != NULL; note = note->next)
      notesz += note_size(note);

   off = sizeof(ehdr) + sizeof(*phdrs) * num_phdrs;

   phdrs[0].p_type = PT_NOTE;
   phdrs[0].p_offset = off;
   phdrs[0].p_vaddr = 0;
   phdrs[0].p_paddr = 0;
   phdrs[0].p_filesz = notesz;
   phdrs[0].p_memsz = 0;
   phdrs[0].p_flags = 0;
   phdrs[0].p_align = 0;

   off += notesz;

   off = VG_PGROUNDUP(off);

   for(i = 0, idx = 1; i < n_seg_starts; i++) {
      seg = VG_(am_find_nsegment)(seg_starts[i]);

      if (!may_dump(seg))
	 continue;

      fill_phdr(&phdrs[idx], seg, off,
                (seg->end - seg->start + 1 + off) < max_size);

      off += phdrs[idx].p_filesz;

      idx++;
   }

   /* write everything out */
   VG_(write)(core_fd, &ehdr, sizeof(ehdr));
   VG_(write)(core_fd, phdrs, sizeof(*phdrs) * num_phdrs);

   for(note = notelist; note != NULL; note = note->next)
      write_note(core_fd, note);

   VG_(lseek)(core_fd, phdrs[1].p_offset, VKI_SEEK_SET);

   for(i = 0, idx = 1; i < n_seg_starts; i++) {
      seg = VG_(am_find_nsegment)(seg_starts[i]);

      if (!should_dump(seg))
	 continue;

      if (phdrs[idx].p_filesz > 0) {
	 vg_assert(VG_(lseek)(core_fd, phdrs[idx].p_offset, VKI_SEEK_SET)
                   == phdrs[idx].p_offset);
	 vg_assert(seg->end - seg->start + 1 >= phdrs[idx].p_filesz);

	 (void)VG_(write)(core_fd, (void *)seg->start, phdrs[idx].p_filesz);
      }
      idx++;
   }

   VG_(close)(core_fd);

 cleanup:
   VG_(free)(basename);
   VG_(free)(buf);
   VG_(free)(seg_starts);
   VG_(free)(phdrs);
}

void VG_(make_coredump)(ThreadId tid, const vki_siginfo_t *si, ULong max_size)
{
   make_elf_coredump(tid, si, max_size);
}

#endif // defined(VGO_linux)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
