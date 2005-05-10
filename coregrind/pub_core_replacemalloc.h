
/*--------------------------------------------------------------------*/
/*--- Replacing malloc.                   pub_core_replacemalloc.h ---*/
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

#ifndef __PUB_CORE_REPLACEMALLOC_H
#define __PUB_CORE_REPLACEMALLOC_H

//--------------------------------------------------------------------
// PURPOSE: This module provides a short-cut for tools that need to
// replace malloc/free with their own versions.
//--------------------------------------------------------------------

// Nb: there are no exports in this header;  all exports for this module
// are in include/pub_tool_replacemalloc.h

#include "pub_tool_replacemalloc.h"

#endif   // __PUB_CORE_REPLACEMALLOC_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
