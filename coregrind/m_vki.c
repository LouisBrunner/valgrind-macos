
/*--------------------------------------------------------------------*/
/*--- Notional "implementation" for m_vki.                         ---*/
/*---                                                      m_vki.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2006-2007 OpenWorks LLP
      info@open-works.co.uk

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

#include "pub_core_basics.h"
#include "pub_core_vki.h"     /* self */

/* We have pub_{core,tool}_vki.h.  This is the matching implementation
   for that interface.  In fact there is no implementation, as the
   sole purpose of the module is to export types and constants
   describing the kernel interface, so this file is nearly empty. */


/* ppc32/64-linux determines page size at startup, hence m_vki is
   the logical place to store that info. */

#if defined(VGP_ppc32_linux) || defined(VGP_ppc64_linux)
unsigned long VKI_PAGE_SHIFT = 12;
unsigned long VKI_PAGE_SIZE  = 1UL << 12;
#endif


/*--------------------------------------------------------------------*/
/*--- end                                                  m_vki.c ---*/
/*--------------------------------------------------------------------*/
