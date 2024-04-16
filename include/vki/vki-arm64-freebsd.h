/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2024 Paul Floyd
      pjfloyd@wanadoo.fr

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

#ifndef VKI_ARM64_FREEBSD_H
#define VKI_ARM64_FREEBSD_H

//----------------------------------------------------------------------
// arm/param.h
//----------------------------------------------------------------------

/* PAGE_SHIFT determines the page size. */
#define VKI_PAGE_SHIFT     12UL
#define VKI_PAGE_SIZE      (1UL << VKI_PAGE_SHIFT)
#define VKI_MAX_PAGE_SHIFT VKI_PAGE_SHIFT
#define VKI_MAX_PAGE_SIZE  VKI_PAGE_SIZE

//----------------------------------------------------------------------
// machine/_limits.h
//----------------------------------------------------------------------
#define VKI_MINSIGSTKSZ (1024 * 4)

//----------------------------------------------------------------------
// sys/_sigset.h
//----------------------------------------------------------------------
#define _VKI_NSIG_WORDS 4
#define _VKI_NSIG       128
#define _VKI_NSIG_BPW   ((_VKI_NSIG) / (_VKI_NSIG_WORDS))

#include "vki-machine-types-arm64-freebsd.h"

typedef struct {
   vki_uint32_t sig[_VKI_NSIG_WORDS];
} vki_sigset_t;

//----------------------------------------------------------------------
// machine/armreg.h
//----------------------------------------------------------------------

/* mainly for the carry flag, used to signifify syscall success/failure */
#define VKI_PSR_IL    0x00100000UL
#define VKI_PSR_SS    0x00200000UL
#define VKI_PSR_V     0x10000000UL
#define VKI_PSR_C     0x20000000UL
#define VKI_PSR_Z     0x40000000UL
#define VKI_PSR_N     0x80000000UL
#define VKI_PSR_FLAGS 0xf0000000UL

//----------------------------------------------------------------------
// machine/reg.h
// Used by PTRACE and coredump-elf.h */
//----------------------------------------------------------------------

/*
struct vki_reg {
   vki_uint64_t x[30];
   vki_uint64_t lr;
   vki_uint64_t sp;
   vki_uint64_t elr;
   vki_uint64_t spsr;
};
*/

struct vki_dbreg {
   vki_uint8_t db_debug_ver;
   vki_uint8_t db_nbkpts;
   vki_uint8_t db_nwtpts;
   vki_uint8_t db_pad[5];

   struct {
      vki_uint64_t dbr_addr;
      vki_uint32_t dbr_ctrl;
      vki_uint32_t dbr_pad;
   } db_breakregs[16];
   struct {
      vki_uint64_t dbw_addr;
      vki_uint32_t dbw_ctrl;
      vki_uint32_t dbw_pad;
   } db_watchregs[16];
};

//----------------------------------------------------------------------
// machine/ucontext.h
//----------------------------------------------------------------------

/* It's a bit stupid having the struct layout as reg.h struct reg */
struct vki_gpregs {
   __vki_register_t gp_x[30];
   __vki_register_t gp_lr;
   __vki_register_t gp_sp;
   __vki_register_t gp_elr;
   vki_uint64_t     gp_spsr;
};

/* coredump-elf.c compatibility */
#define vki_user_regs_struct vki_gpregs

/* Like Linux */
typedef unsigned long vki_elf_greg_t;
#define VKI_ELF_NGREG (sizeof(struct vki_gpregs) / sizeof(vki_elf_greg_t))
typedef vki_elf_greg_t vki_elf_gregset_t[VKI_ELF_NGREG];

struct vki_fpregs {
   __uint128_t  fp_q[32];
   vki_uint32_t fp_sr;
   vki_uint32_t fp_cr;
   int          fp_flags;
   int          fp_pad;
};

/* amd64 compatibility */
#define vki_fpreg vki_fpregs

/* fpregs in FreeBSD headers fpreg */
// #define vki_fpreg vki_fpregs

// On Linux the equivalent of the above is smaller, without the fp_flags annd
// padding
typedef struct vki_fpregs vki_elf_fpregset_t;

struct vki_mcontext {
   struct vki_gpregs mc_gpregs;
   struct vki_fpregs mc_fpregs;
   int               mc_flags;
#define _MC_FP_VALID 0x1     /* Set when mc_fpregs has valid data */
   int          mc_pad;      /* Padding */
   vki_uint64_t mc_spare[8]; /* Space for expansion, set to zero */
};

struct vki_sigaction_base {
   void (*ksa_handler)(int);
   int          sa_flags;
   vki_sigset_t sa_mask; /* mask last for extensibility */
};
typedef struct vki_sigaction_base vki_sigaction_toK_t;
typedef struct vki_sigaction_base vki_sigaction_fromK_t;

//----------------------------------------------------------------------
// sys/vdso.h and machine/vdso.h
//----------------------------------------------------------------------
#define VKI_VDSO_TIMEHANDS_MD                                                  \
   uint32_t th_physical;                                                       \
   uint32_t th_res[7];

struct vki_bintime {
   vki_time_t   sec;
   vki_uint64_t frac;
};

struct vki_vdso_timehands {
   vki_uint32_t       th_algo;
   vki_uint32_t       th_gen;
   vki_uint64_t       th_scale;
   vki_uint32_t       th_offset_count;
   vki_uint32_t       th_counter_mask;
   struct vki_bintime th_offset;
   struct vki_bintime th_boottime;
   VKI_VDSO_TIMEHANDS_MD
};

#endif /* VKI_ARM64_FREEBSD_H */
