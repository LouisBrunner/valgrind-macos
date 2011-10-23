
/*--------------------------------------------------------------------*/
/*--- An sparse array (of words) implementation.                   ---*/
/*---                                          pub_core_sparsewa.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2008-2011 OpenWorks Ltd
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

#ifndef __PUB_CORE_SPARSEWA_H
#define __PUB_CORE_SPARSEWA_H

//--------------------------------------------------------------------
// PURPOSE: Provides an implementation of a sparse array of host words
// (UWord).  The indices are themselves host words.  The implementation
// uses a 256-way radix tree, which is therefore 4 levels deep on a 
// 32-bit platform and 8 levels deep on a 64-bit platform.
//--------------------------------------------------------------------

// No core-only exports; everything in this module is visible to both
// the core and tools.

#include "pub_tool_sparsewa.h"

#endif   // __PUB_CORE_SPARSEWA_H

/*--------------------------------------------------------------------*/
/*--- end                                      pub_core_sparsewa.h ---*/
/*--------------------------------------------------------------------*/
