/*--------------------------------------------------------------------*/
/*---                                              x86/tool_arch.h ---*/
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

#ifndef __X86_TOOL_ARCH_H
#define __X86_TOOL_ARCH_H

// XXX: eventually a lot of the stuff in this file can be made private to
// the x86/ subdir, and not visible to the core.  But as long as the core
// still refers to them, they'll have to stay in here.

/*====================================================================*/
/*=== Registers, etc                                               ===*/
/*====================================================================*/

#define REGPARM(n)      __attribute__((regparm(n)))

#define FIRST_ARCH_REG  R_EAX
#define LAST_ARCH_REG   R_EDI

#define N_ARCH_REGS        8

#define MIN_INSTR_SIZE     1
#define MAX_INSTR_SIZE    16

/* Total number of integer registers available for allocation -- all of
   them except %esp (points to Valgrind's stack) and %ebp (permanently
   points at the baseBlock).

   If you increase this you'll have to also change at least these:
     - VG_(rank_to_realreg)()
     - VG_(realreg_to_rank)()
     - ppRegsLiveness()
     - the RegsLive type (maybe -- RegsLive type must have more than
                          VG_MAX_REALREGS bits)

   You can decrease it, and performance will drop because more spills will
   occur.  If you decrease it too much, everything will fall over.

   Do not change this unless you really know what you are doing!  */
#define VG_MAX_REALREGS 6


/*====================================================================*/
/*=== Instrumenting UCode                                          ===*/
/*====================================================================*/

/* ------------------------------------------------------------------ */
/* Offsets of addresses of helper functions.  A "helper" function is one
   which is called from generated code via CALLM. */

// XXX: eventually these should be private to the x86 part, not visible to
// tools, and the IR should provide a better way than this to see what the
// original instruction was.

extern Int VGOFF_(helper_idiv_64_32);
extern Int VGOFF_(helper_div_64_32);
extern Int VGOFF_(helper_idiv_32_16);
extern Int VGOFF_(helper_div_32_16);
extern Int VGOFF_(helper_idiv_16_8);
extern Int VGOFF_(helper_div_16_8);

extern Int VGOFF_(helper_imul_32_64);
extern Int VGOFF_(helper_mul_32_64);
extern Int VGOFF_(helper_imul_16_32);
extern Int VGOFF_(helper_mul_16_32);
extern Int VGOFF_(helper_imul_8_16);
extern Int VGOFF_(helper_mul_8_16);

extern Int VGOFF_(helper_CLD);
extern Int VGOFF_(helper_STD);
extern Int VGOFF_(helper_get_dirflag);

extern Int VGOFF_(helper_CLC);
extern Int VGOFF_(helper_STC);
extern Int VGOFF_(helper_CMC);

extern Int VGOFF_(helper_shldl);
extern Int VGOFF_(helper_shldw);
extern Int VGOFF_(helper_shrdl);
extern Int VGOFF_(helper_shrdw);

extern Int VGOFF_(helper_RDTSC);
extern Int VGOFF_(helper_CPUID);

extern Int VGOFF_(helper_IN);
extern Int VGOFF_(helper_OUT);

extern Int VGOFF_(helper_bsfw);
extern Int VGOFF_(helper_bsfl);
extern Int VGOFF_(helper_bsrw);
extern Int VGOFF_(helper_bsrl);

extern Int VGOFF_(helper_fstsw_AX);
extern Int VGOFF_(helper_SAHF);
extern Int VGOFF_(helper_LAHF);
extern Int VGOFF_(helper_DAS);
extern Int VGOFF_(helper_DAA);
extern Int VGOFF_(helper_AAS);
extern Int VGOFF_(helper_AAA);
extern Int VGOFF_(helper_AAD);
extern Int VGOFF_(helper_AAM);

extern Int VGOFF_(helper_cmpxchg8b);


#endif   // __X86_TOOL_ARCH_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
