
/*--------------------------------------------------------------------*/
/*--- Header included by every core C file.      pub_core_basics.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2005 Julian Seward
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

#ifndef __PUB_CORE_BASICS_H
#define __PUB_CORE_BASICS_H

//--------------------------------------------------------------------
// PURPOSE: This header should be imported by every single C file
// in the core.  It contains the basic types and other things needed
// everywhere.
//--------------------------------------------------------------------

#include "pub_tool_basics.h"

/* ---------------------------------------------------------------------
   Other headers to include
   ------------------------------------------------------------------ */

// Might as well have the following two in here, their contents are used so
// broadly (eg. in pub_core_threadstate.h).

#include "libvex.h"

#if defined(VGA_x86)
#  include "libvex_guest_x86.h"
#elif defined(VGA_amd64)
#  include "libvex_guest_amd64.h"
#elif defined(VGA_ppc32)
#  include "libvex_guest_ppc32.h"
#else
#  error Unknown arch
#endif

#include <setjmp.h>

#endif   // __PUB_CORE_BASICS_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
