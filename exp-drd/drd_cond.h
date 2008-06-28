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


#ifndef __DRD_COND_H
#define __DRD_COND_H


#include "drd_thread.h"      /* DrdThreadid */
#include "pub_tool_basics.h" /* Addr        */


/* Forward declarations. */

struct cond_info;


/* Global variables. */

extern Bool s_drd_report_signal_unlocked;


/* Function declarations. */

void cond_set_trace(const Bool trace_cond);
void cond_pre_init(const Addr cond);
void cond_post_destroy(const Addr cond);
int cond_pre_wait(const Addr cond, const Addr mutex);
int cond_post_wait(const Addr cond);
void cond_pre_signal(const Addr cond);
void cond_pre_broadcast(const Addr cond);
void cond_thread_delete(const DrdThreadId tid);


#endif /* __DRD_COND_H */
