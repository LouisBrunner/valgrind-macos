
/*--------------------------------------------------------------------*/
/*--- Handlers for syscalls on minor variants of Linux kernels.    ---*/
/*---                                     syswrap-linux-variants.c ---*/
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

#if defined(VGO_linux)

/* The files syswrap-generic.c, syswrap-linux.c, syswrap-*-linux.c,
   and associated vki*.h header files, constitute Valgrind's model of how a
   vanilla Linux kernel behaves with respect to syscalls.

   On a few occasions, it is useful to run with a kernel that has some
   (minor) extensions to the vanilla model, either due to running on a
   hacked kernel, or using a vanilla kernel which has incorporated a
   custom kernel module.  Rather than clutter the standard model, all
   such variant handlers are placed in here.

   Unlike the C files for the standard model, this file should also
   contain all constants/types needed for said wrappers.  The vki*.h
   headers should not be polluted with non-vanilla info. */


#include "pub_core_basics.h"
#include "pub_core_vki.h"
#include "pub_core_threadstate.h"
#include "pub_core_aspacemgr.h"
#include "pub_core_debuginfo.h"    // VG_(di_notify_*)
#include "pub_core_transtab.h"     // VG_(discard_translations)
#include "pub_core_debuglog.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcfile.h"
#include "pub_core_libcprint.h"
#include "pub_core_libcproc.h"
#include "pub_core_mallocfree.h"
#include "pub_core_tooliface.h"
#include "pub_core_options.h"
#include "pub_core_scheduler.h"
#include "pub_core_signals.h"
#include "pub_core_syscall.h"

#include "priv_types_n_macros.h"
#include "priv_syswrap-linux-variants.h"


/* ---------------------------------------------------------------
   BProc wrappers
   ------------------------------------------------------------ */

/* Return 0 means hand to kernel, non-0 means fail w/ that value. */
Int ML_(linux_variant_PRE_sys_bproc)( UWord arg1, UWord arg2,
                                      UWord arg3, UWord arg4,
                                      UWord arg5, UWord arg6 )
{
   return 0;
}

void ML_(linux_variant_POST_sys_bproc)( UWord arg1, UWord arg2,
                                        UWord arg3, UWord arg4,
                                        UWord arg5, UWord arg6 )
{
}

#endif // defined(VGO_linux)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
