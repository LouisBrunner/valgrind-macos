
/*--------------------------------------------------------------------*/
/*--- Asm-specific core stuff.                          core_asm.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2004 Julian Seward 
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

#ifndef __CORE_ASM_H
#define __CORE_ASM_H

#include "tool_asm.h"            // tool asm stuff
#include "core_arch_asm.h"       // arch-specific asm stuff

/* This file is included in all Valgrind source files, including
   assembly ones. */


/* Magic values that the guest state might be set to when returning to the
   dispatcher.  The only other legitimate value is to point to the
   start of the thread's VEX guest state.  These also are return values from
   VG_(run_innerloop) to the scheduler.
*/
/* Defines values for JMP_EMWARN, JMP_SYSCALL, JMP_CLIENTREQ and
   JMP_YIELD */
#include "libvex_trc_values.h"

/* And some more of our own.  These must not have the same values as
   those from libvex_trc_values.h. */
#define VG_TRC_INNER_FASTMISS     37 /* TRC only; means fast-cache miss. */
#define VG_TRC_INNER_COUNTERZERO  41 /* TRC only; means bb ctr == 0 */
#define VG_TRC_UNRESUMABLE_SIGNAL 43 /* TRC only; got sigsegv/sigbus */
#define VG_TRC_INVARIANT_FAILED   47 /* TRC only; invariant violation */


/* Constants for the fast translation lookup cache. */
#define VG_TT_FAST_BITS 15
#define VG_TT_FAST_SIZE (1 << VG_TT_FAST_BITS)
#define VG_TT_FAST_MASK ((VG_TT_FAST_SIZE) - 1)

/* Assembly code stubs make this request */
#define VG_USERREQ__SIGNAL_RETURNS          0x4001

// XXX: all this will go into x86/ eventually...
/* 
   0 - standard feature flags
   1 - Intel extended flags
   2 - Valgrind internal flags
   3 - AMD-specific flags
 */
#define VG_N_FEATURE_WORDS	4

#define VG_X86_FEAT		0
#define VG_EXT_FEAT		1
#define VG_INT_FEAT		2
#define VG_AMD_FEAT		3

/* CPU features (generic) */
#define VG_X86_FEAT_FPU		(VG_X86_FEAT*32 + 0)
#define VG_X86_FEAT_VME		(VG_X86_FEAT*32 + 1)
#define VG_X86_FEAT_DE		(VG_X86_FEAT*32 + 2)
#define VG_X86_FEAT_PSE		(VG_X86_FEAT*32 + 3)
#define VG_X86_FEAT_TSC		(VG_X86_FEAT*32 + 4)
#define VG_X86_FEAT_MSR		(VG_X86_FEAT*32 + 5)
#define VG_X86_FEAT_PAE		(VG_X86_FEAT*32 + 6)
#define VG_X86_FEAT_MCE		(VG_X86_FEAT*32 + 7)
#define VG_X86_FEAT_CX8		(VG_X86_FEAT*32 + 8)
#define VG_X86_FEAT_APIC	(VG_X86_FEAT*32 + 9)
#define VG_X86_FEAT_SEP		(VG_X86_FEAT*32 + 11)
#define VG_X86_FEAT_MTRR	(VG_X86_FEAT*32 + 12)
#define VG_X86_FEAT_PGE		(VG_X86_FEAT*32 + 13)
#define VG_X86_FEAT_MCA		(VG_X86_FEAT*32 + 14)
#define VG_X86_FEAT_CMOV	(VG_X86_FEAT*32 + 15)
#define VG_X86_FEAT_PAT		(VG_X86_FEAT*32 + 16)
#define VG_X86_FEAT_PSE36	(VG_X86_FEAT*32 + 17)
#define VG_X86_FEAT_CLFSH	(VG_X86_FEAT*32 + 19)
#define VG_X86_FEAT_DS		(VG_X86_FEAT*32 + 21)
#define VG_X86_FEAT_ACPI	(VG_X86_FEAT*32 + 22)
#define VG_X86_FEAT_MMX		(VG_X86_FEAT*32 + 23)
#define VG_X86_FEAT_FXSR	(VG_X86_FEAT*32 + 24)
#define VG_X86_FEAT_SSE		(VG_X86_FEAT*32 + 25)
#define VG_X86_FEAT_SSE2	(VG_X86_FEAT*32 + 26)
#define VG_X86_FEAT_SS		(VG_X86_FEAT*32 + 27)
#define VG_X86_FEAT_HT		(VG_X86_FEAT*32 + 28)
#define VG_X86_FEAT_TM		(VG_X86_FEAT*32 + 29)
#define VG_X86_FEAT_IA64	(VG_X86_FEAT*32 + 30)
#define VG_X86_FEAT_PBE		(VG_X86_FEAT*32 + 31)

/* Intel extended feature word */
#define VG_X86_FEAT_SSE3	(VG_EXT_FEAT*32 + 0)
#define VG_X86_FEAT_MON		(VG_EXT_FEAT*32 + 3)
#define VG_X86_FEAT_DSCPL	(VG_EXT_FEAT*32 + 4)
#define VG_X86_FEAT_EST		(VG_EXT_FEAT*32 + 7)
#define VG_X86_FEAT_TM2		(VG_EXT_FEAT*32 + 8)
#define VG_X86_FEAT_CNXTID	(VG_EXT_FEAT*32 + 10)

/* Used internally to mark whether CPUID is even implemented */
#define VG_X86_FEAT_CPUID	(VG_INT_FEAT*32 + 0)

/* AMD special features */
#define VG_AMD_FEAT_SYSCALL	(VG_AMD_FEAT*32 + 11)
#define VG_AMD_FEAT_NXP		(VG_AMD_FEAT*32 + 20)
#define VG_AMD_FEAT_MMXEXT	(VG_AMD_FEAT*32 + 22)
#define VG_AMD_FEAT_FFXSR	(VG_AMD_FEAT*32 + 25)
#define VG_AMD_FEAT_LONGMODE	(VG_AMD_FEAT*32 + 29)
#define VG_AMD_FEAT_3DNOWEXT	(VG_AMD_FEAT*32 + 30)
#define VG_AMD_FEAT_3DNOW	(VG_AMD_FEAT*32 + 31)

#endif /* __CORE_ASM_H */

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
