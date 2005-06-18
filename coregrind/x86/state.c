
/*--------------------------------------------------------------------*/
/*--- Arch-specific registers, etc.                    x86/state.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2005 Nicholas Nethercote
      njn@valgrind.org

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
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_tooliface.h"
#include <sys/ptrace.h>

#include "libvex_guest_x86.h"


/*------------------------------------------------------------*/
/*--- Determining arch/subarch.                            ---*/
/*------------------------------------------------------------*/

// Returns the architecture and subarchitecture, or indicates
// that this subarchitecture is unable to run Valgrind
// Returns False to indicate we cannot proceed further.
Bool VGA_(getArchAndSubArch)( /*OUT*/VexArch*    vex_arch, 
                              /*OUT*/VexSubArch* vex_subarch )
{
   Bool have_sse0, have_sse1, have_sse2;
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

   have_sse0 = (edx & (1<<24)) != 0; /* True => have fxsave/fxrstor */
   have_sse1 = (edx & (1<<25)) != 0; /* True => have sse insns */
   have_sse2 = (edx & (1<<26)) != 0; /* True => have sse2 insns */

   if (have_sse2 && have_sse1 && have_sse0) {
      *vex_arch    = VexArchX86;
      *vex_subarch = VexSubArchX86_sse2;
      return True;
   }

   if (have_sse1 && have_sse0) {
      *vex_arch    = VexArchX86;
      *vex_subarch = VexSubArchX86_sse1;
      return True;
   }

   if (have_sse0) {
      *vex_arch    = VexArchX86;
      *vex_subarch = VexSubArchX86_sse0;
      return True;
   }

   /* we need at least SSE state to operate. */
   return False;
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
