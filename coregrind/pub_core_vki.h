
/*--------------------------------------------------------------------*/
/*--- Top level for kernel interface declarations.                 ---*/
/*---                                               pub_core_vki.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2010 Julian Seward
      jseward@acm.org
   Copyright (C) 2005-2010 Nicholas Nethercote
      njn@valgrind.org
   Copyright (C) 2006-2010 OpenWorks LLP
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

#ifndef __PUB_CORE_VKI_H
#define __PUB_CORE_VKI_H

/* Most unfortunately, all the kernel decls are visible to tools.  Not
   really necessary, but to avoid this would require some tedious
   refactoring of the sources.  Anyway, we live with this kludge, and
   that means the only thing to be done here is ... */

#include "pub_tool_vki.h"

/* Do initial consistency checks on some of the definitions to do with
   signals (vki_sigset_t and vki_sigaction_{toK,fromK}_t).  This stuff
   is fragile enough that it's important to check at startup that
   the world looks like what we expect it to look like. */
void VG_(vki_do_initial_consistency_checks)(void);

#endif // __PUB_CORE_VKI_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
