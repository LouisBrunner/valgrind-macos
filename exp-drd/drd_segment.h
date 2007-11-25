/*
  This file is part of drd, a data race detector.

  Copyright (C) 2006-2007 Bart Van Assche
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


#ifndef __SEGMENT_H
#define __SEGMENT_H


// Segments and segment lists. A segment represents information about 
// a contiguous group of statements of a specific thread. There is a vector
// clock associated with each segment.


#include "drd_vc.h"
#include "pub_drd_bitmap.h"
#include "pub_tool_execontext.h" // ExeContext
#include "pub_tool_stacktrace.h" // StackTrace


typedef struct segment
{
  struct segment*    next;
  struct segment*    prev;
  ExeContext*        stacktrace;
  VectorClock        vc;
  struct bitmap*     bm;
} Segment;

void sg_init(Segment* const sg,
             ThreadId const creator,
             ThreadId const created);
void sg_cleanup(Segment* const sg);
Segment* sg_new(ThreadId const creator, ThreadId const created);
void sg_delete(Segment* const sg);
void sg_print(const Segment* const sg);
Bool sg_get_trace(void);
void sg_set_trace(Bool const trace_segment);
ULong sg_get_segments_created_count(void);
ULong sg_get_max_segments_alive_count(void);


#endif // __SEGMENT_H
