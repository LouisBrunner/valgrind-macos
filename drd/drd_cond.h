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


#ifndef __DRD_COND_H
#define __DRD_COND_H


#include "drd_thread.h"      /* DrdThreadid */
#include "pub_tool_basics.h" /* Addr        */


/* Forward declarations. */

struct cond_info;


/* Variable declarations. */

extern Addr DRD_(pthread_cond_initializer);
extern int DRD_(pthread_cond_initializer_size);


/* Function declarations. */

void DRD_(cond_set_report_signal_unlocked)(const Bool r);
void DRD_(cond_set_trace)(const Bool trace_cond);
struct cond_info* DRD_(cond_get)(const Addr cond);
void DRD_(cond_pre_init)(const Addr cond);
void DRD_(cond_post_destroy)(const Addr cond);
void DRD_(cond_pre_wait)(const Addr cond, const Addr mutex);
void DRD_(cond_post_wait)(const Addr cond);
void DRD_(cond_pre_signal)(const Addr cond);
void DRD_(cond_pre_broadcast)(const Addr cond);


#endif /* __DRD_COND_H */
