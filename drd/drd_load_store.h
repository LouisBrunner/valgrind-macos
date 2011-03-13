/* -*- mode: C; c-basic-offset: 3; indent-tabs-mode: nil; -*- */
/*
  This file is part of drd, a thread error detector.

  Copyright (C) 2006-2011 Bart Van Assche <bvanassche@acm.org>.

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


/*
 * Functions related to instrumentation of loads and stores.
 */


#ifndef __DRD_LOAD_STORE_H
#define __DRD_LOAD_STORE_H


#include <libvex.h>             /* IRSB */
#include <pub_tool_tooliface.h> /* VgCallbackClosure */


Bool DRD_(get_check_stack_accesses)(void);
void DRD_(set_check_stack_accesses)(const Bool c);
Bool DRD_(get_first_race_only)(void);
void DRD_(set_first_race_only)(const Bool fro);
IRSB* DRD_(instrument)(VgCallbackClosure* const closure,
                       IRSB* const bb_in,
                       VexGuestLayout* const layout,
                       VexGuestExtents* const vge,
                       IRType const gWordTy,
                       IRType const hWordTy);
void DRD_(trace_mem_access)(const Addr addr, const SizeT size,
                            const BmAccessTypeT access_type);
VG_REGPARM(2) void DRD_(trace_load)(Addr addr, SizeT size);
VG_REGPARM(2) void DRD_(trace_store)(Addr addr, SizeT size);
void DRD_(clean_memory)(const Addr a1, const SizeT len);


#endif //  __DRD_LOAD_STORE_H
