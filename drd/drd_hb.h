/* -*- mode: C; c-basic-offset: 3; -*- */
/*
  This file is part of drd, a thread error detector.

  Copyright (C) 2006-2010 Bart Van Assche <bvanassche@acm.org>.

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


#ifndef __DRD_HB_H
#define __DRD_HB_H


#include "drd_thread.h"      /* DrdThreadid */
#include "pub_tool_basics.h" /* Addr        */


/* Forward declarations. */

struct hb_info;


/* Function declarations. */

void DRD_(hb_set_trace)(const Bool trace_hb);
struct hb_info* DRD_(hb_get)(const Addr hb);
struct hb_info* DRD_(hb_get_or_allocate)(const Addr hb);
void DRD_(hb_init)(const Addr hb);
void DRD_(hb_destroy)(const Addr hb);
void DRD_(hb_happens_after)(const DrdThreadId tid, const Addr hb);
void DRD_(hb_happens_before)(const DrdThreadId tid, const Addr hb);
void DRD_(hb_happens_done)(const DrdThreadId tid, const Addr hb);


#endif /* __DRD_HB_H */
