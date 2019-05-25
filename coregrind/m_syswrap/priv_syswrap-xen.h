/*--------------------------------------------------------------------*/
/*--- Xen Hypercalls                            priv_syswrap-xen.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2012-2017 Citrix Systems
      ian.campbell@citrix.com

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

#ifndef __PRIV_SYSWRAP_XEN_H
#define __PRIV_SYSWRAP_XEN_H

#include "priv_types_n_macros.h"    // DECL_TEMPLATE

DECL_TEMPLATE(xen, hypercall);

#endif   // __PRIV_SYSWRAP_XEN_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
