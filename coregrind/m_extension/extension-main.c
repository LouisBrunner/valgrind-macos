
/*--------------------------------------------------------------------*/
/*--- Handle extensions.                          extension-main.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) IBM Corp. 2024

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 3 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

/* Contributed by Andreas Arnez */

#include "libvex_guest_offsets.h"
#include "pub_core_extension.h"
#include "pub_core_libcassert.h"
#include "pub_core_threadstate.h"
#include "priv_extension.h"

/* This is the top-level of the extension handler module.  Extensions provide a
   means of executing instructions whose register and memory effects are too
   complex to be expressed with dirty helpers.
 */

#if defined(VGP_s390x_linux)

/* --- This is the main function of this file. --- */

enum ExtensionError VG_(client_extension)(ThreadId tid)
{
   ThreadState*             tst;

   vg_assert(VG_(is_valid_tid)(tid));
   vg_assert(tid >= 1 && tid < VG_N_THREADS);
   vg_assert(VG_(is_running_thread)(tid));

   tst = VG_(get_ThreadState)(tid);

   return ML_(do_client_extension)(tst);
}

#else

enum ExtensionError VG_(client_extension)(ThreadId tid)
{
   VG_(core_panic)("Extension handler not implemented for this architecture");
}

#endif
