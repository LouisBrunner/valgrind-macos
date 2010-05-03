
/*--------------------------------------------------------------------*/
/*--- A separately-chained hash table.        pub_core_hashtable.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2005-2010 Nicholas Nethercote
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

#ifndef __PUB_CORE_HASHTABLE_H
#define __PUB_CORE_HASHTABLE_H

//--------------------------------------------------------------------
// PURPOSE:  A generic data structure with fairly fast lookup for not too
// many elements, eg. up to a few thousand.
//--------------------------------------------------------------------

#include "pub_tool_hashtable.h"

// No core-only exports;  everything in this module is visible to both
// the core and tools.

#endif   // __PUB_CORE_HASHTABLE_H

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
