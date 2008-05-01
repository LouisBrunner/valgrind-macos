/*
  This file is part of drd, a data race detector.

  Copyright (C) 2006-2008 Bart Van Assche
  bart.vanassche@gmail.com

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

#ifndef __MALLOC_WRAPPERS_H
#define __MALLOC_WRAPPERS_H


#include "pub_tool_basics.h"     // Bool
#include "pub_tool_execontext.h" // ExeContext


typedef void (*StartUsingMem)(const Addr a1, const Addr a2, UInt ec_uniq);
typedef void (*StopUsingMem)(const Addr a1, const Addr a2);


void drd_register_malloc_wrappers(const StartUsingMem start_using_mem_callback,
                                  const StopUsingMem stop_using_mem_callback);
Bool drd_heap_addrinfo(Addr const a,
                       Addr* const data,
                       SizeT* const size,
                       ExeContext** const where);
void drd_print_malloc_stats(void);


#endif // __MALLOC_WRAPPERS_H
