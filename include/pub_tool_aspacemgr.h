
/*--------------------------------------------------------------------*/
/*--- Address space manager.                  pub_tool_aspacemgr.h ---*/
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

#ifndef __PUB_TOOL_ASPACEMGR
#define __PUB_TOOL_ASPACEMGR

extern Bool VG_(is_client_addr) (Addr a);

extern Bool VG_(is_shadow_addr) (Addr a);
extern Addr VG_(get_shadow_size)(void);

extern void *VG_(shadow_alloc)(UInt size);

extern Bool VG_(is_addressable)(Addr p, SizeT sz, UInt prot);

/* initialize shadow pages in the range [p, p+sz) This calls
   init_shadow_page for each one.  It should be a lot more efficient
   for bulk-initializing shadow pages than faulting on each one. 
*/
extern void VG_(init_shadow_range)(Addr p, UInt sz, Bool call_init);

/* Calls into the core used by leak-checking */

/* Calls "add_rootrange" with each range of memory which looks like a
   plausible source of root pointers.  This is very Memcheck-specific --
   it's used in leak detection.
*/
extern void VG_(find_root_memory)(void (*add_rootrange)(Addr addr, SizeT sz));

#endif   // __PUB_TOOL_ASPACEMGR

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
