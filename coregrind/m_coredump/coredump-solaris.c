
/*--------------------------------------------------------------------*/
/*--- Dumping core on Solaris.                  coredump-solaris.c ---*/
/*--------------------------------------------------------------------*/
 
/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2013-2017 Ivo Raisr
      ivosh@ivosh.net

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

#if defined(VGO_solaris)

#include "pub_core_basics.h"
#include "pub_core_vki.h"
#include "pub_core_aspacehl.h"
#include "pub_core_aspacemgr.h"
#include "pub_core_coredump.h"
#include "pub_core_debuglog.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcfile.h"
#include "pub_core_libcprint.h"
#include "pub_core_libcproc.h"
#include "pub_core_machine.h"
#include "pub_core_mallocfree.h"
#include "pub_core_options.h"
#include "pub_core_syscall.h"
#include "pub_core_threadstate.h"
#include "pub_core_xarray.h"
#include "pub_core_clientstate.h"

typedef struct __attribute__ ((__packed__)) note {
   struct note *next;
   VKI_ESZ(Nhdr) nhdr;
   HChar name[8];
   HChar data[0];
} note_t;

static void add_note(note_t **list, UInt type, const void *data,
                     UInt datasz);

/* If true, then this Segment may be mentioned in the core */
static Bool may_dump(const NSegment *seg)
{
   if ((seg->kind == SkAnonC) ||
       (seg->kind == SkShmC) ||
       ((seg->kind == SkFileC) &&
        !VKI_S_ISCHR(seg->mode) && !VKI_S_ISBLK(seg->mode)))
      return True;

   return False;
}

/* If true, then this Segment's contents will be in the core */
static Bool should_dump(const NSegment *seg)
{
   return may_dump(seg);
}

#if defined(SOLARIS_PRXREGSET_T)
static Bool should_dump_xregs(const ThreadState *tst)
{
#if defined(VGP_x86_solaris)
   return False;
#elif defined(VGP_amd64_solaris)
   const ThreadArchState *arch = (const ThreadArchState *) &tst->arch;

   /* Dump 256-bit wide %ymm only when their upper half is non-zero. */
   #define YMM_NON_ZERO(reg) \
      ((reg[4] != 0) || (reg[5] != 0) || (reg[6] != 0) || (reg[7] != 0))
   if (YMM_NON_ZERO(arch->vex.guest_YMM0) ||
       YMM_NON_ZERO(arch->vex.guest_YMM1) ||
       YMM_NON_ZERO(arch->vex.guest_YMM2) ||
       YMM_NON_ZERO(arch->vex.guest_YMM3) ||
       YMM_NON_ZERO(arch->vex.guest_YMM4) ||
       YMM_NON_ZERO(arch->vex.guest_YMM5) ||
       YMM_NON_ZERO(arch->vex.guest_YMM6) ||
       YMM_NON_ZERO(arch->vex.guest_YMM7) ||
       YMM_NON_ZERO(arch->vex.guest_YMM9) ||
       YMM_NON_ZERO(arch->vex.guest_YMM0) ||
       YMM_NON_ZERO(arch->vex.guest_YMM10) ||
       YMM_NON_ZERO(arch->vex.guest_YMM11) ||
       YMM_NON_ZERO(arch->vex.guest_YMM12) ||
       YMM_NON_ZERO(arch->vex.guest_YMM13) ||
       YMM_NON_ZERO(arch->vex.guest_YMM14) ||
       YMM_NON_ZERO(arch->vex.guest_YMM15))
      return True;

   return False;

   #undef YMM_NON_ZERO
#else
#  error Unknown ELF platform
#endif
}
#endif /* SOLARIS_PRXREGSET_T */

static void write_part(Int fd, const HChar *filename,
                       void *buf, SizeT buf_size, const HChar *part)
{
   Int ret = VG_(write)(fd, buf, buf_size);
   if (ret < 0) {
      VG_(umsg)("Failed to write %s to coredump file %s, it may be "
                "incomplete.\n", part, filename);
      VG_(debugLog)(1, "coredump-solaris", "write_part: failed to write "
                    "%s to file %s. Buffer address=%p, length=%lu. "
                    "Error=%d.\n", part, filename, buf, buf_size, -ret);
   }
}

/*====================================================================*/
/*=== Miscellaneous getters                                        ===*/
/*====================================================================*/

static Int get_uid(void)
{
   return sr_Res(VG_(do_syscall0)(SYS_getuid));
}

static Int get_gid(void)
{
   return sr_Res(VG_(do_syscall0)(SYS_getgid));
}

static Int get_dmodel(void)
{
#if defined(VGP_x86_solaris)
   return PR_MODEL_ILP32;
#elif defined(VGP_amd64_solaris)
   return PR_MODEL_LP64;
#else
#  error "Unknown platform"
#endif
}

static vki_zoneid_t get_zoneid(void)
{
   SysRes sres = VG_(do_syscall2)(SYS_zone, VKI_ZONE_LOOKUP,
                                  (UWord) NULL);
   if (sr_isError(sres))
       return 0;

   return sr_Res(sres);
}

static UInt count_auxv(void)
{
   UInt count = 1;

   vki_auxv_t *auxv = (vki_auxv_t *) VG_(client_auxv);
   while (auxv->a_type != VKI_AT_NULL) {
      count += 1;
      auxv++;
   }

   return count;
}

static Addr compute_stkbase(const ThreadState *tst)
{
   return tst->client_stack_highest_byte + 1
          - tst->client_stack_szB;
}

static Int get_wstat(const vki_siginfo_t *si)
{
   return (si->si_signo & 0xff) | WCOREFLG;
}

/*====================================================================*/
/*=== Utility fillers                                              ===*/
/*====================================================================*/

static void fill_platform(HChar *buf, UInt buf_size)
{
   vg_assert(buf != NULL);
   vg_assert(buf_size >= 1);

   buf[0] = '\0';

   VG_(do_syscall3)(SYS_systeminfo, VKI_SI_PLATFORM,
                    (UWord) buf, buf_size);
}

static void fill_zonename(HChar *buf, UInt buf_size)
{
   vg_assert(buf != NULL);
   vg_assert(buf_size >= 1);

   buf[0] = '\0';

   VG_(do_syscall5)(SYS_zone, VKI_ZONE_GETATTR, get_zoneid(),
                    VKI_ZONE_ATTR_NAME, (UWord) buf, buf_size);
}

static void fill_thread_state(const ThreadState *tst,
                              HChar *state, HChar *sname)
{
   switch (tst->status) {
   case VgTs_Runnable:
   case VgTs_Yielding:
      *state = VKI_SRUN;
      *sname = 'R';
      break;

   case VgTs_WaitSys:
      *state = VKI_SSLEEP;
      *sname = 'S';
      break;

   case VgTs_Zombie:
      *state = VKI_SZOMB;
      *sname = 'Z';
      break;

   case VgTs_Empty:
   case VgTs_Init:
      *state = 0;
      *sname = '?';
      break;
   }
}

static void fill_siginfo(const vki_siginfo_t *si, vki_siginfo_t *di,
                         Short *signo)
{
   di->si_signo = si->si_signo;
   di->si_code  = si->si_code;
   di->si_errno = 0;
   di->si_addr = si->si_addr;
   *signo = si->si_signo;
}

static void fill_argv(Int *argc, Addr *argv)
{
   Addr *ptr = (Addr *) VG_(get_initial_client_SP)();
   *argc = *ptr++;
   *argv = (Addr) ptr;
}

static void fill_scheduling_class(HChar *buf, SizeT buf_size)
{
   vg_assert(buf != NULL);
   vg_assert(buf_size >= 1);

   /* Valgrind currently schedules one thread at time which
      resembles the default timeshare class. */
   VG_(strncpy)(buf, "TS", buf_size);
}

static void fill_regset(vki_prgregset_t *regs, const ThreadState *tst)
{
   const ThreadArchState *arch = (const ThreadArchState *) &tst->arch;

#if defined(VGP_x86_solaris)
   (*regs)[VKI_EIP]        = arch->vex.guest_EIP;
   (*regs)[VKI_EAX]        = arch->vex.guest_EAX;
   (*regs)[VKI_EBX]        = arch->vex.guest_EBX;
   (*regs)[VKI_ECX]        = arch->vex.guest_ECX;
   (*regs)[VKI_EDX]        = arch->vex.guest_EDX;
   (*regs)[VKI_ESI]        = arch->vex.guest_ESI;
   (*regs)[VKI_EDI]        = arch->vex.guest_EDI;
   (*regs)[VKI_EBP]        = arch->vex.guest_EBP;
   (*regs)[VKI_UESP]       = arch->vex.guest_ESP;
   (*regs)[VKI_SS]         = arch->vex.guest_SS;
   (*regs)[VKI_CS]         = arch->vex.guest_CS;
   (*regs)[VKI_DS]         = arch->vex.guest_DS;
   (*regs)[VKI_ES]         = arch->vex.guest_ES;
   (*regs)[VKI_FS]         = arch->vex.guest_FS;
   (*regs)[VKI_GS]         = arch->vex.guest_GS;
   (*regs)[VKI_EFL]        = LibVEX_GuestX86_get_eflags(&arch->vex);
#elif defined(VGP_amd64_solaris)
   (*regs)[VKI_REG_RIP]    = arch->vex.guest_RIP;
   (*regs)[VKI_REG_RAX]    = arch->vex.guest_RAX;
   (*regs)[VKI_REG_RBX]    = arch->vex.guest_RBX;
   (*regs)[VKI_REG_RCX]    = arch->vex.guest_RCX;
   (*regs)[VKI_REG_RDX]    = arch->vex.guest_RDX;
   (*regs)[VKI_REG_RBP]    = arch->vex.guest_RBP;
   (*regs)[VKI_REG_RSI]    = arch->vex.guest_RSI;
   (*regs)[VKI_REG_RDI]    = arch->vex.guest_RDI;
   (*regs)[VKI_REG_R8]     = arch->vex.guest_R8;
   (*regs)[VKI_REG_R9]     = arch->vex.guest_R9;
   (*regs)[VKI_REG_R10]    = arch->vex.guest_R10;
   (*regs)[VKI_REG_R11]    = arch->vex.guest_R11;
   (*regs)[VKI_REG_R12]    = arch->vex.guest_R12;
   (*regs)[VKI_REG_R13]    = arch->vex.guest_R13;
   (*regs)[VKI_REG_R14]    = arch->vex.guest_R14;
   (*regs)[VKI_REG_R15]    = arch->vex.guest_R15;
   (*regs)[VKI_REG_RSP]    = arch->vex.guest_RSP;
   (*regs)[VKI_REG_CS]     = VKI_UCS_SEL;
   (*regs)[VKI_REG_DS]     = 0;
   (*regs)[VKI_REG_ES]     = 0;
   (*regs)[VKI_REG_FS]     = 0;
   (*regs)[VKI_REG_GS]     = 0;
   (*regs)[VKI_REG_SS]     = VKI_UDS_SEL;
   (*regs)[VKI_REG_FSBASE] = arch->vex.guest_FS_CONST;
   (*regs)[VKI_REG_GSBASE] = 0;
   (*regs)[VKI_REG_RFL]    = LibVEX_GuestAMD64_get_rflags(&arch->vex);
#else
#  error "Unknown platform"
#endif
}

static void fill_fpregset(vki_fpregset_t *fpu, const ThreadState *tst)
{
   const ThreadArchState *arch = (const ThreadArchState *) &tst->arch;

#if defined(VGP_x86_solaris)
   VG_(memset)(fpu, 0, sizeof(*fpu));

   struct vki_fpchip_state *fs = &fpu->fp_reg_set.fpchip_state;
   vg_assert(sizeof(fs->state) == 108);

   LibVEX_GuestX86_get_x87(CONST_CAST(VexGuestX86State *, &arch->vex),
                           (UChar *) &fs->state);

   /* SSE */
   UInt mxcsr = LibVEX_GuestX86_get_mxcsr(CONST_CAST(VexGuestX86State *,
                                                     &arch->vex));
   fs->mxcsr = mxcsr;

   /* XMM registers */
   #define COPY_OUT_XMM(dest, src) \
      do {                      \
         dest._l[0] = src[0];   \
         dest._l[1] = src[1];   \
         dest._l[2] = src[2];   \
         dest._l[3] = src[3];   \
      } while (0);
   COPY_OUT_XMM(fs->xmm[0], arch->vex.guest_XMM0);
   COPY_OUT_XMM(fs->xmm[1], arch->vex.guest_XMM1);
   COPY_OUT_XMM(fs->xmm[2], arch->vex.guest_XMM2);
   COPY_OUT_XMM(fs->xmm[3], arch->vex.guest_XMM3);
   COPY_OUT_XMM(fs->xmm[4], arch->vex.guest_XMM4);
   COPY_OUT_XMM(fs->xmm[5], arch->vex.guest_XMM5);
   COPY_OUT_XMM(fs->xmm[6], arch->vex.guest_XMM6);
   COPY_OUT_XMM(fs->xmm[7], arch->vex.guest_XMM7);
   #undef COPY_OUT_XMM
#elif defined(VGP_amd64_solaris)
   VG_(memset)(fpu, 0, sizeof(*fpu));
   struct vki_fpchip_state *fs = &fpu->fp_reg_set.fpchip_state;

   /* LibVEX_GuestAMD64_fxsave() requires at least 416 bytes. */
   vg_assert(sizeof(*fs) >= 416);
   LibVEX_GuestAMD64_fxsave(CONST_CAST(VexGuestAMD64State *, &arch->vex),
                            (Addr) fs);
#else
#  error Unknown platform
#endif
}

/*====================================================================*/
/*=== Header fillers                                               ===*/
/*====================================================================*/

static void fill_ehdr(VKI_ESZ(Ehdr) *ehdr, Int num_phdrs)
{
   VG_(memset)(ehdr, 0, sizeof(*ehdr));

   VG_(memcpy)(ehdr->e_ident, VKI_ELFMAG, VKI_SELFMAG);
   ehdr->e_ident[VKI_EI_CLASS]   = VG_ELF_CLASS;
   ehdr->e_ident[VKI_EI_DATA]    = VG_ELF_DATA2XXX;
   ehdr->e_ident[VKI_EI_VERSION] = VKI_EV_CURRENT;

   ehdr->e_type = VKI_ET_CORE;
   ehdr->e_machine = VG_ELF_MACHINE;
   ehdr->e_version = VKI_EV_CURRENT;
   ehdr->e_entry = 0;
   ehdr->e_flags = 0;
   ehdr->e_ehsize = sizeof(VKI_ESZ(Ehdr));

   ehdr->e_phoff = sizeof(VKI_ESZ(Ehdr));
   ehdr->e_phentsize = sizeof(VKI_ESZ(Phdr));

   /* If the count of program headers can't fit in the mere 16 bits
    * shortsightedly allotted to them in the ELF header, we use the
    * extended formats and put the real values in the section header
    * at index 0.
    */
   if (num_phdrs >= VKI_PN_XNUM) {
      ehdr->e_phnum = VKI_PN_XNUM;
      ehdr->e_shnum = 1;
      ehdr->e_shoff = ehdr->e_phoff + ehdr->e_phentsize * num_phdrs;
      ehdr->e_shentsize = sizeof(VKI_ESZ(Shdr));
   } else {
      ehdr->e_phnum = num_phdrs;
      ehdr->e_shnum = 0;
      ehdr->e_shoff = 0;
      ehdr->e_shentsize = 0;
   }

   ehdr->e_shstrndx = 0;
}

static void fill_phdr(VKI_ESZ(Phdr) *phdr, const NSegment *seg, UInt off,
                      Bool really_write)
{
   SizeT len = seg->end - seg->start + 1;

   really_write = really_write && should_dump(seg);

   VG_(memset)(phdr, 0, sizeof(*phdr));

   phdr->p_type = PT_LOAD;
   phdr->p_offset = off;
   phdr->p_vaddr = seg->start;
   phdr->p_paddr = 0;
   phdr->p_filesz = really_write ? len : 0;
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

/* Fills the section header at index zero when num_phdrs >= PN_XNUM. */
static void fill_zero_shdr(VKI_ESZ(Shdr) *shdr, UInt num_phdrs)
{
   vg_assert(num_phdrs >= VKI_PN_XNUM);

   VG_(memset)(shdr, 0, sizeof(*shdr));

   shdr->sh_name = 0; // STR_NONE
   shdr->sh_info = num_phdrs;
}

static void fill_prpsinfo(vki_elf_prpsinfo_t *prpsinfo,
                          const ThreadState *tst,
                          const vki_siginfo_t *si)
{
   VG_(memset)(prpsinfo, 0, sizeof(*prpsinfo));

   fill_thread_state(tst, &prpsinfo->pr_state, &prpsinfo->pr_sname);
   prpsinfo->pr_uid = get_uid();
   prpsinfo->pr_gid = get_gid();
   prpsinfo->pr_pid = VG_(getpid)();
   prpsinfo->pr_ppid = VG_(getppid)();
   prpsinfo->pr_pgrp = VG_(getpgrp)();
   prpsinfo->pr_sid = VG_(getpgrp)();
   fill_scheduling_class(prpsinfo->pr_clname, sizeof(prpsinfo->pr_clname));
   VG_(client_fname)(prpsinfo->pr_fname, sizeof(prpsinfo->pr_fname), True);
   VG_(client_cmd_and_args)(prpsinfo->pr_psargs,
                            sizeof(prpsinfo->pr_psargs));
   fill_argv(&prpsinfo->pr_argc, (Addr *) &prpsinfo->pr_argv);
   prpsinfo->pr_envp = (char **) VG_(client_envp);
   prpsinfo->pr_wstat = get_wstat(si);
   prpsinfo->pr_euid = VG_(geteuid)();
   prpsinfo->pr_egid = VG_(getegid)();
   prpsinfo->pr_dmodel = get_dmodel();
}

static void fill_prstatus(vki_elf_prstatus_t *prs,
                          const ThreadState *tst,
			  const vki_siginfo_t *si)
{
   VG_(memset)(prs, 0, sizeof(*prs));

   prs->pr_flags = VKI_ELF_OLD_PR_PCINVAL;
   fill_siginfo(si, &prs->pr_info, &prs->pr_cursig);
   prs->pr_nlwp = VG_(count_living_threads)();
   prs->pr_sighold = tst->sig_mask;
   prs->pr_pid = VG_(getpid)();
   prs->pr_ppid = VG_(getppid)();
   prs->pr_pgrp = VG_(getpgrp)();
   prs->pr_sid = VG_(getpgrp)();
   fill_scheduling_class(prs->pr_clname, sizeof(prs->pr_clname));
   prs->pr_who = tst->os_state.lwpid; 
   prs->pr_brkbase = (vki_caddr_t) VG_(brk_base);
   prs->pr_brksize = VG_(brk_limit) - VG_(brk_base);
   prs->pr_stkbase = (vki_caddr_t) compute_stkbase(tst);
   prs->pr_stksize = tst->client_stack_szB;
   fill_regset(&prs->pr_reg, tst);
}

static void fill_psinfo(vki_psinfo_t *psinfo, const ThreadState *tst,
                        const vki_siginfo_t *si)
{
   VG_(memset)(psinfo, 0, sizeof(*psinfo));

   psinfo->pr_nlwp = VG_(count_living_threads)();
   psinfo->pr_uid = get_uid();
   psinfo->pr_gid = get_gid();
   psinfo->pr_pid = VG_(getpid)();
   psinfo->pr_ppid = VG_(getppid)();
   psinfo->pr_pgid = VG_(getpgrp)();
   psinfo->pr_sid = VG_(getpgrp)();
   psinfo->pr_euid = VG_(geteuid)();
   psinfo->pr_egid = VG_(getegid)();
   VG_(client_fname)(psinfo->pr_fname, sizeof(psinfo->pr_fname), True);
   psinfo->pr_wstat = get_wstat(si);
   VG_(client_cmd_and_args)(psinfo->pr_psargs,
                            sizeof(psinfo->pr_psargs));
   fill_argv(&psinfo->pr_argc, (Addr *) &psinfo->pr_argv);
   psinfo->pr_envp = (uintptr_t) VG_(client_envp);
   psinfo->pr_dmodel = get_dmodel();
   psinfo->pr_zoneid = get_zoneid();

   psinfo->pr_lwp.pr_lwpid = tst->os_state.lwpid;
   fill_thread_state(tst, &psinfo->pr_lwp.pr_state,
                     &psinfo->pr_lwp.pr_sname);
   fill_scheduling_class(psinfo->pr_lwp.pr_clname,
                         sizeof(psinfo->pr_lwp.pr_clname));
}

static void fill_pstatus(vki_pstatus_t *pstatus,
                         const ThreadState *tst,
                         const vki_siginfo_t *si)
{
   VG_(memset)(pstatus, 0, sizeof(*pstatus));

   pstatus->pr_flags = VKI_PR_PCINVAL;
   pstatus->pr_nlwp = VG_(count_living_threads)();
   pstatus->pr_pid = VG_(getpid)();
   pstatus->pr_ppid = VG_(getppid)();
   pstatus->pr_pgid = VG_(getpgrp)();
   pstatus->pr_sid = VG_(getpgrp)();
   pstatus->pr_brkbase = (uintptr_t) VG_(brk_base);
   pstatus->pr_brksize = VG_(brk_limit) - VG_(brk_base);
   pstatus->pr_stkbase = (uintptr_t) compute_stkbase(tst);
   pstatus->pr_stksize = tst->client_stack_szB;
   pstatus->pr_dmodel = get_dmodel();
   pstatus->pr_zoneid = get_zoneid();

   pstatus->pr_lwp.pr_flags = VKI_PR_PCINVAL;
   pstatus->pr_lwp.pr_lwpid = tst->os_state.lwpid;
   fill_siginfo(si, &pstatus->pr_lwp.pr_info,
                &pstatus->pr_lwp.pr_cursig);
   pstatus->pr_lwp.pr_lwphold = tst->sig_mask;
   fill_scheduling_class(pstatus->pr_lwp.pr_clname,
                         sizeof(pstatus->pr_lwp.pr_clname));
   fill_regset(&pstatus->pr_lwp.pr_reg, tst);
   fill_fpregset(&pstatus->pr_lwp.pr_fpreg, tst);
}

#if defined(SOLARIS_PRXREGSET_T)
static void fill_xregs(vki_prxregset_t *xregs, const ThreadState *tst)
{
   const ThreadArchState *arch = (const ThreadArchState *) &tst->arch;

#if defined(VGP_x86_solaris)
   VG_(memset)(xregs, 0, sizeof(*xregs));
   xregs->pr_xsize = sizeof(xregs->pr_un.pr_xsave);

   /* SSE */
   UInt mxcsr = LibVEX_GuestX86_get_mxcsr(CONST_CAST(VexGuestX86State *,
                                                     &arch->vex));
   xregs->pr_un.pr_xsave.pr_mxcsr = mxcsr;

   /* XMM registers */
   #define COPY_OUT_XMM(dest, src) \
      do {                      \
         dest._l[0] = src[0];   \
         dest._l[1] = src[1];   \
         dest._l[2] = src[2];   \
         dest._l[3] = src[3];   \
      } while (0);
   COPY_OUT_XMM(xregs->pr_un.pr_xsave.pr_xmm[0], arch->vex.guest_XMM0);
   COPY_OUT_XMM(xregs->pr_un.pr_xsave.pr_xmm[1], arch->vex.guest_XMM1);
   COPY_OUT_XMM(xregs->pr_un.pr_xsave.pr_xmm[2], arch->vex.guest_XMM2);
   COPY_OUT_XMM(xregs->pr_un.pr_xsave.pr_xmm[3], arch->vex.guest_XMM3);
   COPY_OUT_XMM(xregs->pr_un.pr_xsave.pr_xmm[4], arch->vex.guest_XMM4);
   COPY_OUT_XMM(xregs->pr_un.pr_xsave.pr_xmm[5], arch->vex.guest_XMM5);
   COPY_OUT_XMM(xregs->pr_un.pr_xsave.pr_xmm[6], arch->vex.guest_XMM6);
   COPY_OUT_XMM(xregs->pr_un.pr_xsave.pr_xmm[7], arch->vex.guest_XMM7);
   #undef COPY_OUT_XMM

#elif defined(VGP_amd64_solaris)
   VG_(memset)(xregs, 0, sizeof(*xregs));
   xregs->pr_xsize = sizeof(xregs->pr_un.pr_xsave);

   /* LibVEX_GuestAMD64_fxsave() requires at least 416 bytes. */
   vg_assert(sizeof(xregs->pr_un.pr_xsave) >= 416);
   LibVEX_GuestAMD64_fxsave(CONST_CAST(VexGuestAMD64State *, &arch->vex),
                            (Addr) &xregs->pr_un.pr_xsave);
#else
#  error "Unknown platform"
#endif
}
#endif /* SOLARIS_PRXREGSET_T */

static void fill_utsname(struct vki_utsname *uts)
{
   VG_(memset)(uts, 0, sizeof(*uts));

   VG_(do_syscall3)(SYS_systeminfo, VKI_SI_SYSNAME,
                    (UWord) &uts->sysname, sizeof(uts->sysname));
   VG_(do_syscall3)(SYS_systeminfo, VKI_SI_HOSTNAME,
                    (UWord) &uts->nodename, sizeof(uts->nodename));
   VG_(do_syscall3)(SYS_systeminfo, VKI_SI_RELEASE,
                    (UWord) &uts->release, sizeof(uts->release));
   VG_(do_syscall3)(SYS_systeminfo, VKI_SI_VERSION,
                    (UWord) &uts->version, sizeof(uts->version));
   VG_(do_syscall3)(SYS_systeminfo, VKI_SI_MACHINE,
                    (UWord) &uts->machine, sizeof(uts->machine));
}

static vki_prcred_t *create_prcred(SizeT *size)
{
   UInt group_list[VKI_NGROUPS_MAX];
   Int ngroups = VG_(getgroups)(VKI_NGROUPS_MAX, group_list);
   if (ngroups == -1)
      ngroups = 0;

   *size = sizeof(vki_prcred_t) + (ngroups - 1) * sizeof(gid_t);
   vki_prcred_t *prcred = VG_(malloc)("coredump-elf.cp.1", *size);
   VG_(memset)(prcred, 0, *size);

   prcred->pr_euid = VG_(geteuid)();
   prcred->pr_ruid = get_uid();
   prcred->pr_suid = prcred->pr_euid;
   prcred->pr_egid = VG_(getegid)();
   prcred->pr_rgid = get_gid();
   prcred->pr_sgid = prcred->pr_egid;
   prcred->pr_ngroups = ngroups;

   UInt i;
   for (i = 0; i < ngroups; i++)
      prcred->pr_groups[i] = group_list[i];

   return prcred;
}

static void fill_core_content(vki_core_content_t *content)
{
   *content = VKI_CC_CONTENT_STACK | VKI_CC_CONTENT_HEAP
              | VKI_CC_CONTENT_SHANON | VKI_CC_CONTENT_TEXT
              | VKI_CC_CONTENT_DATA | VKI_CC_CONTENT_RODATA
              | VKI_CC_CONTENT_ANON | VKI_CC_CONTENT_SHM
              | VKI_CC_CONTENT_ISM | VKI_CC_CONTENT_DISM;
}

static vki_prpriv_t *create_prpriv(SizeT *size)
{
   Int fd = VG_(fd_open)("/proc/self/priv", O_RDONLY, 0);
   if (fd < 0)
      return NULL;

   struct vg_stat stats;
   if (VG_(fstat)(fd, &stats) != 0) {
      VG_(close)(fd);
      return NULL;
   }

   vki_prpriv_t *prpriv = VG_(malloc)("coredump-elf.cp.1", stats.size);

   if (VG_(read)(fd, prpriv, stats.size) != stats.size) {
      VG_(free)(prpriv);
      VG_(close)(fd);
      return NULL;
   }

   VG_(close)(fd);
   *size = stats.size;
   return prpriv;
}

static vki_priv_impl_info_t *create_priv_info(SizeT *size)
{
   /* Size of the returned priv_impl_info_t is apriori unknown. */
   vki_priv_impl_info_t first_cut[100];
   SysRes sres = VG_(do_syscall5)(SYS_privsys, VKI_PRIVSYS_GETIMPLINFO,
                                  0, 0, (UWord) first_cut,
                                  sizeof(first_cut));
   if (sr_isError(sres))
       return NULL;

   SizeT real_size = first_cut[0].priv_headersize
                     + first_cut[0].priv_globalinfosize;
   vki_priv_impl_info_t *priv_info = VG_(malloc)("coredump-elf.cpi.1",
                                                 real_size);

   if (real_size <= sizeof(first_cut)) {
      /* if the first_cut was large enough */
      VG_(memcpy)(priv_info, first_cut, real_size);
   } else {
      /* otherwise repeat the syscall with buffer large enough */
      sres = VG_(do_syscall5)(SYS_privsys, VKI_PRIVSYS_GETIMPLINFO,
                              0, 0, (UWord) priv_info, real_size);
      if (sr_isError(sres)) {
          VG_(free)(priv_info);
          return NULL;
      }
   }

   *size = real_size;
   return priv_info;
}

static void fill_lwpsinfo(vki_lwpsinfo_t *lwp,
                          const ThreadState *tst)
{
   VG_(memset)(lwp, 0, sizeof(*lwp));

   lwp->pr_lwpid = tst->os_state.lwpid;
   fill_thread_state(tst, &lwp->pr_state, &lwp->pr_sname);
   fill_scheduling_class(lwp->pr_clname, sizeof(lwp->pr_clname));
}

static void fill_lwpstatus(vki_lwpstatus_t *lwp,
                           const ThreadState *tst,
			   const vki_siginfo_t *si)
{
   VG_(memset)(lwp, 0, sizeof(*lwp));

   lwp->pr_flags = VKI_PR_PCINVAL;
   lwp->pr_lwpid = tst->os_state.lwpid;
   fill_siginfo(si, &lwp->pr_info, &lwp->pr_cursig);
   fill_scheduling_class(lwp->pr_clname, sizeof(lwp->pr_clname));
   fill_regset(&lwp->pr_reg, tst);
   fill_fpregset(&lwp->pr_fpreg, tst);
}

static void fill_old_note_for_thread(note_t **notes,
                                     const ThreadState *tst,
                                     const vki_siginfo_t *si)
{
   vki_elf_prstatus_t prstatus;
   fill_prstatus(&prstatus, tst, si);
   add_note(notes, VKI_NT_PRSTATUS, &prstatus, sizeof(vki_elf_prstatus_t));

   vki_fpregset_t fpu;
   fill_fpregset(&fpu, tst);
   add_note(notes, VKI_NT_PRFPREG, &fpu, sizeof(vki_fpregset_t));

#if defined(SOLARIS_PRXREGSET_T)
   if (should_dump_xregs(tst)) {
      vki_prxregset_t xregs;
      fill_xregs(&xregs, tst);
      add_note(notes, VKI_NT_PRXREG, &xregs, sizeof(vki_prxregset_t));
   }
#endif /* SOLARIS_PRXREGSET_T */
}

static void fill_new_note_for_thread(note_t **notes,
                                     const ThreadState *tst,
                                     const vki_siginfo_t *si)
{
   vki_lwpsinfo_t lwpsinfo;
   fill_lwpsinfo(&lwpsinfo, tst);
   add_note(notes, VKI_NT_LWPSINFO, &lwpsinfo, sizeof(vki_lwpsinfo_t));

   vki_lwpstatus_t lwpstatus;
   fill_lwpstatus(&lwpstatus, tst, si);
   add_note(notes, VKI_NT_LWPSTATUS, &lwpstatus, sizeof(vki_lwpstatus_t));

#if defined(SOLARIS_PRXREGSET_T)
   if (should_dump_xregs(tst)) {
      vki_prxregset_t xregs;
      fill_xregs(&xregs, tst);
      add_note(notes, VKI_NT_PRXREG, &xregs, sizeof(vki_prxregset_t));
   }
#endif /* SOLARIS_PRXREGSET_T */
}

/*====================================================================*/
/*=== Note utility functions                                       ===*/
/*====================================================================*/

static void add_note(note_t **list, UInt type, const void *data,
                     UInt datasz)
{
   UInt note_size = sizeof(note_t) + VG_ROUNDUP(datasz, 4);

   note_t *n = VG_(malloc)("coredump-elf.an.1", note_size);

   VG_(memset)(n, 0, note_size);
   n->nhdr.n_type = type;
   n->nhdr.n_namesz = 5;
   n->nhdr.n_descsz = VG_ROUNDUP(datasz, 4);
   VG_(memcpy)(n->name, "CORE", 4);
   VG_(memcpy)(n->data, data, datasz);

   if (*list == NULL) {
      *list = n;
      return;
   }

   note_t *tail = *list;
   while (tail->next != NULL)
      tail = tail->next;
   tail->next = n;
}

static UInt note_size(const note_t *note)
{
   return sizeof(note_t) - sizeof(note_t *) + note->nhdr.n_descsz;
}

static UInt notes_size(const note_t *list)
{
   UInt size = 0;
   const note_t *note;

   for (note = list; note != NULL; note = note->next)
      size += note_size(note);

   return size;
}

static void fill_notes_phdr(VKI_ESZ(Phdr) *phdr, UInt offset,
                            UInt size_of_notes)
{
   phdr->p_type = PT_NOTE;
   phdr->p_offset = offset;
   phdr->p_vaddr = 0;
   phdr->p_paddr = 0;
   phdr->p_filesz = size_of_notes;
   phdr->p_memsz = 0;
   phdr->p_flags = PF_R;
   phdr->p_align = 0;
}

static void write_notes(Int fd, const HChar *filename,
                        const note_t *list)
{
   const note_t *note;

   for (note = list; note != NULL; note = note->next)
      write_part(fd, filename, CONST_CAST(void *, &note->nhdr),
                 note_size(note), "notes");
}

static void free_notes(note_t *list)
{
   while (list != NULL) {
      note_t *next = list->next;
      VG_(free)(list);
      list = next;
   }
}

/*====================================================================*/
/*=== Main coredump function                                       ===*/
/*====================================================================*/

void VG_(make_coredump)(ThreadId tid, const vki_siginfo_t *si,
                        ULong max_size)
{
   const HChar *basename = "vgcore";
   const HChar *coreext = "";
   Int core_fd;

   if (VG_(clo_log_fname_unexpanded) != NULL) {
      coreext = ".core";
      basename = VG_(expand_file_name)("--log-file",
                                       VG_(clo_log_fname_unexpanded));
   }

   vg_assert(coreext != NULL);
   vg_assert(basename != NULL);

   UInt filename_size = VG_(strlen)(coreext) + VG_(strlen)(basename)
                        + 100; /* for the two %d's */
   HChar *filename = VG_(malloc)("coredump-elf.mc.1", filename_size);

   /* Try to come with a non-existent coredump filename. */
   UInt seq = 0;
   for (;;) {
      Int oflags = VKI_O_CREAT|VKI_O_WRONLY|VKI_O_EXCL|VKI_O_TRUNC;

      if (seq == 0)
	 VG_(snprintf)(filename, filename_size, "%s%s.%d",
		      basename, coreext, VG_(getpid)());
      else
	 VG_(snprintf)(filename, filename_size, "%s%s.%d.%u",
		      basename, coreext, VG_(getpid)(), seq);
      seq++;

#ifdef VKI_O_LARGEFILE
      oflags |= VKI_O_LARGEFILE;
#endif

      SysRes sres = VG_(open)(filename, oflags,
                              VKI_S_IRUSR|VKI_S_IWUSR);
      if (!sr_isError(sres)) {
         core_fd = sr_Res(sres);
	 break;
      }

      if (sr_isError(sres) && sr_Err(sres) != VKI_EEXIST) {
         VG_(umsg)("Cannot create coredump file %s (%lu)\n",
                   filename, sr_Err(sres));
         VG_(free)(filename);
	 return;
      }
   }

   /* Get the client segments. Free seg_starts after use. */
   Int n_seg_starts;
   Addr *seg_starts = VG_(get_segment_starts)(SkFileC | SkAnonC | SkShmC,
                                              &n_seg_starts);

   /* Count how many memory segments to dump. */
   Int i;
   UInt num_phdrs = 2;		/* two CORE note sections */
   for (i = 0; i < n_seg_starts; i++) {
      if (!may_dump(VG_(am_find_nsegment)(seg_starts[i])))
	 continue;

      num_phdrs++;
   }

   VKI_ESZ(Ehdr) ehdr;
   fill_ehdr(&ehdr, num_phdrs);

   VKI_ESZ(Shdr) shdr;
   if (ehdr.e_shnum > 0)
      fill_zero_shdr(&shdr, num_phdrs);
   UInt phdrs_size = num_phdrs * ehdr.e_phentsize;

   /* Construct the old-style notes. */
   note_t *old_notes = NULL;

   vki_elf_prpsinfo_t prpsinfo;
   fill_prpsinfo(&prpsinfo, &VG_(threads)[tid], si);
   add_note(&old_notes, VKI_NT_PRPSINFO, &prpsinfo,
            sizeof(vki_elf_prpsinfo_t));

   HChar platform[256 + 1];
   fill_platform(platform, sizeof(platform));
   add_note(&old_notes, VKI_NT_PLATFORM, platform,
            VG_(strlen)(platform) + 1);

   add_note(&old_notes, VKI_NT_AUXV, VG_(client_auxv),
            count_auxv() * sizeof(auxv_t));

   /* Add detail about the faulting thread as the first note.
      This is how gdb determines which thread faulted. Note that
      mdb does not need such aid. */
   fill_old_note_for_thread(&old_notes, &VG_(threads)[tid], si);

   /* Now add details for all threads except the one that faulted. */
   ThreadId t_idx;
   for (t_idx = 1; t_idx < VG_N_THREADS; t_idx++)
      if ((VG_(threads)[t_idx].status != VgTs_Empty) &&
            (VG_(threads)[t_idx].status != VgTs_Zombie)) {
         if (t_idx == tid)
            continue;

         fill_old_note_for_thread(&old_notes, &VG_(threads)[t_idx], si);
   }

   /* Construct the new-style notes. */
   note_t *new_notes = NULL;
   vki_psinfo_t psinfo;
   fill_psinfo(&psinfo, &VG_(threads)[tid], si);
   add_note(&new_notes, VKI_NT_PSINFO, &psinfo, sizeof(vki_psinfo_t));

   vki_pstatus_t pstatus;
   fill_pstatus(&pstatus, &VG_(threads)[tid], si);
   add_note(&new_notes, VKI_NT_PSTATUS, &pstatus, sizeof(vki_pstatus_t));

   add_note(&new_notes, VKI_NT_PLATFORM, platform,
            VG_(strlen)(platform) + 1);

   add_note(&new_notes, VKI_NT_AUXV, VG_(client_auxv),
            count_auxv() * sizeof(auxv_t));

   struct vki_utsname uts;
   fill_utsname(&uts);
   add_note(&new_notes, VKI_NT_UTSNAME, &uts,
            sizeof(struct vki_utsname));

   SizeT prcred_size;
   vki_prcred_t *prcred = create_prcred(&prcred_size);
   if (prcred != NULL) {
      add_note(&new_notes, VKI_NT_PRCRED, prcred, prcred_size);
      VG_(free)(prcred);
   }

   vki_core_content_t core_content;
   fill_core_content(&core_content);
   add_note(&new_notes, VKI_NT_CONTENT, &core_content,
            sizeof(vki_core_content_t));

   SizeT priv_size;
   vki_prpriv_t *prpriv = create_prpriv(&priv_size);
   if (prpriv != NULL) {
      add_note(&new_notes, VKI_NT_PRPRIV, prpriv, priv_size);
      VG_(free)(prpriv);
   }

   vki_priv_impl_info_t *priv_info = create_priv_info(&priv_size);
   if (priv_info != NULL) {
      add_note(&new_notes, VKI_NT_PRPRIVINFO, priv_info, priv_size);
      VG_(free)(priv_info);
   }

   HChar zonename[VKI_ZONENAME_MAX + 1];
   fill_zonename(zonename, sizeof(zonename));
   add_note(&new_notes, VKI_NT_ZONENAME, zonename,
            VG_(strlen)(zonename) + 1);

   /* Add detail about the faulting thread as the first note.
      This is how gdb determines which thread faulted. Note that
      mdb does not need such aid. */
   fill_new_note_for_thread(&new_notes, &VG_(threads)[tid], si);

   /* Now add details for all threads except the one that faulted. */
   for (t_idx = 1; t_idx < VG_N_THREADS; t_idx++) {
      if ((VG_(threads)[t_idx].status != VgTs_Empty) &&
            (VG_(threads)[t_idx].status != VgTs_Zombie)) {
         if (t_idx == tid)
            continue;

         fill_new_note_for_thread(&new_notes, &VG_(threads)[t_idx], si);
      }
   }

   VKI_ESZ(Phdr) *phdrs = VG_(malloc)("coredump-elf.mc.2", phdrs_size);

   UInt size_of_notes = notes_size(old_notes);
   UInt offset = ehdr.e_ehsize + phdrs_size +
                 (ehdr.e_shnum * ehdr.e_shentsize);

   /* fill program header for old notes */
   fill_notes_phdr(&phdrs[0], offset, size_of_notes);
   offset += size_of_notes;

   size_of_notes = notes_size(new_notes);
   /* fill program header for new notes */
   fill_notes_phdr(&phdrs[1], offset, size_of_notes);
   offset += size_of_notes;

   /* fill program headers for segments */
   UInt idx;
   for (i = 0, idx = 2; i < n_seg_starts; i++) {
      NSegment const *seg = VG_(am_find_nsegment)(seg_starts[i]);

      if (!may_dump(seg))
	 continue;

      fill_phdr(&phdrs[idx], seg, offset,
                (seg->end - seg->start + 1 + offset) < max_size);
      
      offset += phdrs[idx].p_filesz;

      idx++;
   }

   /* write everything out */
   write_part(core_fd, filename, &ehdr, sizeof(ehdr),
             "elf headers");
   write_part(core_fd, filename, phdrs, phdrs_size,
              "program headers");
   if (ehdr.e_shnum > 0)
      write_part(core_fd, filename, &shdr, sizeof(shdr),
                 "section headers");
   write_notes(core_fd, filename, old_notes);
   write_notes(core_fd, filename, new_notes);

   VG_(lseek)(core_fd, phdrs[2].p_offset, VKI_SEEK_SET);

   for (i = 0, idx = 2; i < n_seg_starts; i++) {
      NSegment const *seg = VG_(am_find_nsegment)(seg_starts[i]);

      if (!should_dump(seg))
	 continue;

      if (phdrs[idx].p_filesz > 0) {
         Off64T off = VG_(lseek)(core_fd, phdrs[idx].p_offset,
                                 VKI_SEEK_SET);
         vg_assert(off == phdrs[idx].p_offset);
         vg_assert(seg->end - seg->start + 1 >= phdrs[idx].p_filesz);

         write_part(core_fd, filename, (void *) seg->start,
                    phdrs[idx].p_filesz, "program segment");
      }
      idx++;
   }

   VG_(close)(core_fd);
   VG_(free)(filename);
   VG_(free)(phdrs);
   free_notes(old_notes);
   free_notes(new_notes);
   VG_(free)(seg_starts);
}

#endif

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
