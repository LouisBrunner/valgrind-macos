/*--------------------------------------------------------------------*/
/*--- Callgrind                                                    ---*/
/*---                                                     events.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Callgrind, a Valgrind tool for call tracing.

   Copyright (C) 2002-2007, Josef Weidendorfer (Josef.Weidendorfer@gmx.de)

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

#include "global.h"

#define MAX_EVENTTYPE 20

static EventType eventtype[MAX_EVENTTYPE];
static Int eventtype_count = 0;

EventType* CLG_(register_eventtype)(Char* name)
{
  EventType* et;

  if (eventtype_count == MAX_EVENTTYPE) {
    VG_(printf)("\nMore than %d event types used!\n"
		"Increase MAX_EVENTTYPE in ct_events.c and recomile this tool!\n",
		MAX_EVENTTYPE);
    VG_(tool_panic)("Too many event types requested.");
  }

  et = &(eventtype[eventtype_count]);
  et->id = eventtype_count; 
  et->name = (UChar*) VG_(strdup)(name);
  et->description = 0;

  eventtype_count++;

  return et;
}


EventType* CLG_(get_eventtype)(Char* name)
{
  Int i;

  for(i=0;i<eventtype_count;i++)
    if (VG_(strcmp)(eventtype[i].name, name) == 0)
      return eventtype+i;
  return 0;
}

EventType* CLG_(get_eventtype_byindex)(Int id)
{
  if ((id >= 0) && (id < eventtype_count))
    return eventtype+id;
  return 0;
}

/* Allocate space for an event set */
EventSet* CLG_(get_eventset)(Char* n, Int capacity)
{
  EventSet* es;

  es = (EventSet*) CLG_MALLOC(sizeof(EventSet) +
			       capacity * sizeof(EventSetEntry));
  es->capacity = capacity;
  es->size = 0;
  es->name = n;

  return es;
}

/* Incorporate a event type into a set, get start offset */
Int CLG_(add_eventtype)(EventSet* es, EventType* t)
{
  Int offset = es->size;
  if (es->capacity - offset < 1) return -1;

  es->size++;
  es->e[offset].type = t;
  es->e[offset].nextTop = es->size;

  return offset;
}

/* Incorporate one event set into another, get start offset */
Int CLG_(add_eventset)(EventSet* dst, EventSet* src)
{
  Int offset = dst->size, i;
  if (!src || (src->size == 0)) return offset;

  if (dst->capacity - offset < src->size) return -1;
  
  for(i=0;i<src->size;i++) {
    dst->e[offset+i].type = src->e[i].type;
    dst->e[offset+i].nextTop = src->e[i].nextTop + offset;
  }
  dst->size += src->size;

  return offset;
}

/* Incorporate two event types into a set, with second < first */
Int CLG_(add_dep_event2)(EventSet* es, EventType* e1, EventType* e2)
{
  Int offset = es->size;

  if (es->capacity - offset < 2) return -1;

  es->size += 2;
  es->e[offset].type = e1;
  es->e[offset].nextTop = es->size;
  es->e[offset+1].type = e2;
  es->e[offset+1].nextTop = es->size;
  
  return offset;
}

/* Incorporate 3 event types into a set, with third < second < first */
Int CLG_(add_dep_event3)(EventSet* es,
			EventType* e1, EventType* e2, EventType* e3)
{
  Int offset = es->size;

  if (es->capacity - offset < 3) return -1;

  es->size += 3;
  es->e[offset].type = e1;
  es->e[offset].nextTop = es->size;
  es->e[offset+1].type = e2;
  es->e[offset+1].nextTop = es->size;
  es->e[offset+2].type = e3;
  es->e[offset+2].nextTop = es->size;
  
  return offset;
}

Int CLG_(add_dep_event4)(EventSet* es,
			EventType* e1, EventType* e2,
			EventType* e3, EventType* e4)
{
  Int offset = es->size;

  if (es->capacity - offset < 4) return -1;

  es->size += 4;
  es->e[offset].type = e1;
  es->e[offset].nextTop = es->size;
  es->e[offset+1].type = e2;
  es->e[offset+1].nextTop = es->size;
  es->e[offset+2].type = e3;
  es->e[offset+2].nextTop = es->size;
  es->e[offset+3].type = e4;
  es->e[offset+3].nextTop = es->size;
  
  return offset;
}

/* Returns number of characters written */
Int CLG_(sprint_eventset)(Char* buf, EventSet* es)
{
  Int i, pos = 0;

  for(i=0; i< es->size; i++) {
    if (pos>0) buf[pos++] = ' ';
    pos += VG_(sprintf)(buf + pos, es->e[i].type->name);
  }
  buf[pos] = 0;

  return pos;
}

/* Get cost array for an event set */
ULong* CLG_(get_eventset_cost)(EventSet* es)
{
  return CLG_(get_costarray)(es->capacity);
}

/* Set all costs of an event set to zero */
void CLG_(init_cost)(EventSet* es, ULong* cost)
{
  Int i;

  if (!cost) return;

  for(i=0;i<es->capacity;i++)
    cost[i] = 0;
}

/* Set all costs of an event set to zero */
void CLG_(init_cost_lz)(EventSet* es, ULong** cost)
{
  Int i;

  CLG_ASSERT(cost != 0);
  if (!(*cost))
    *cost = CLG_(get_eventset_cost)(es);

  for(i=0;i<es->capacity;i++)
    (*cost)[i] = 0;
}

void CLG_(zero_cost)(EventSet* es, ULong* cost)
{
  Int i;

  if (!cost) return;

  for(i=0;i<es->size;i++)
    cost[i] = 0;
}
  
Bool CLG_(is_zero_cost)(EventSet* es, ULong* cost)
{
  Int i = 0;

  if (!cost) return True;

  while(i<es->size) {
    if (cost[i] != 0) return False;
    i = es->e[i].nextTop;
  }
  return True;
}

Bool CLG_(is_equal_cost)(EventSet* es, ULong* c1, ULong* c2)
{
  Int i = 0;

  if (!c1) return CLG_(is_zero_cost)(es,c2);
  if (!c2) return CLG_(is_zero_cost)(es,c1);

  while(i<es->size) {
    if (c1[i] != c2[i]) return False;
    if (c1[i] == 0)
      i = es->e[i].nextTop;
    else
      i++;
  }
  return True;
}

void CLG_(copy_cost)(EventSet* es, ULong* dst, ULong* src)
{
  Int i;

  if (!src) {
    CLG_(zero_cost)(es, dst);
    return;
  }
  CLG_ASSERT(dst != 0);
  
  for(i=0;i<es->size;i++)
    dst[i] = src[i];
}

void CLG_(copy_cost_lz)(EventSet* es, ULong** pdst, ULong* src)
{
  Int i;
  ULong* dst;

  CLG_ASSERT(pdst != 0);

  if (!src) {
    CLG_(zero_cost)(es, *pdst);
    return;
  }
  dst = *pdst;
  if (!dst)
    dst = *pdst = CLG_(get_eventset_cost)(es);
  
  for(i=0;i<es->size;i++)
    dst[i] = src[i];
}

void CLG_(add_cost)(EventSet* es, ULong* dst, ULong* src)
{
  Int i = 0;

  if (!src) return;
  CLG_ASSERT(dst != 0);

  while(i<es->size) {
    if (src[i] == 0)
      i = es->e[i].nextTop;
    else {
      dst[i] += src[i];
      i++;
    }
  }
}

void CLG_(add_cost_lz)(EventSet* es, ULong** pdst, ULong* src)
{
  Int i;
  ULong* dst;

  if (!src) return;
  CLG_ASSERT(pdst != 0);

  dst = *pdst;
  if (!dst) {
    dst = *pdst = CLG_(get_eventset_cost)(es);
    CLG_(copy_cost)(es,dst,src);
    return;
  }

  i = 0;
  while(i<es->size) {
    if (src[i] == 0)
      i = es->e[i].nextTop;
    else {
      dst[i] += src[i];
      i++;
    }
  }
}

/* Adds src to dst and zeros src. Returns false if nothing changed */
Bool CLG_(add_and_zero_cost)(EventSet* es, ULong* dst, ULong* src)
{
  Int i = 0, j = 0;

  CLG_DEBUGIF(6) {
    CLG_DEBUG(6, "   add_and_zero_cost(%s, dst %p, src %p)\n", es->name, dst, src);
    CLG_(print_cost)(-5, es, src);
  }

  if (!es || !src) return False;

  while(i<es->size) {
    if (src[i] == 0)
      i = es->e[i].nextTop;
    else {
      dst[i] += src[i];
      src[i] = 0;
      i++;
      j++;
    }
  }

  return (j>0);
}

/* Adds src to dst and zeros src. Returns false if nothing changed */
Bool CLG_(add_and_zero_cost_lz)(EventSet* es, ULong** pdst, ULong* src)
{
  Int i;
  ULong* dst;

  if (!src) return False;

  i = 0;
  while(1) {
    if (i >= es->size) return False;
    if (src[i] != 0) break;
    i = es->e[i].nextTop;
  }

  CLG_ASSERT(pdst != 0);
  dst = *pdst;
  if (!dst) {
    dst = *pdst = CLG_(get_eventset_cost)(es);
    CLG_(copy_cost)(es,dst,src);
    CLG_(zero_cost)(es,src);
    return True;
  }

  dst[i] += src[i];
  src[i] = 0;
  i++;

  while(i<es->size) {
    if (src[i] == 0)
      i = es->e[i].nextTop;
    else {
      dst[i] += src[i];
      src[i] = 0;
    }
  }

  return True;
}

/* Adds difference of new and old to dst, and set old to new.
 * Returns false if nothing changed */
Bool CLG_(add_diff_cost)(EventSet* es, ULong* dst, ULong* old, ULong* new)
{
  Int i = 0, j = 0;

  while(i<es->size) {
    if (new[i] == old[i])
      i = es->e[i].nextTop;
    else {
      dst[i] += new[i] - old[i];
      old[i] = new[i];
      i++;
      j++;
    }
  }

  return (j>0);
}

/* Adds difference of new and old to dst, and set old to new.
 * Returns false if nothing changed */
Bool CLG_(add_diff_cost_lz)(EventSet* es, ULong** pdst, 
			    ULong* old, ULong* new)
{
  Int i;
  ULong* dst;

  if (!old && !new) return False;  
  CLG_ASSERT(old && new);

  i = 0;
  while(1) {
    if (i >= es->size) return False;
    if (old[i] != new[i]) break;
    i = es->e[i].nextTop;
  }

  CLG_ASSERT(pdst != 0);
  dst = *pdst;
  if (!dst) {
    dst = *pdst = CLG_(get_eventset_cost)(es);
    CLG_(zero_cost)(es,dst);
  }

  dst[i] += new[i] - old[i];
  old[i] = new[i];
  i++;

  while(i<es->size) {
    if (new[i] == old[i])
      i = es->e[i].nextTop;
    else {
      dst[i] += new[i] - old[i];
      old[i] = new[i];
      i++;
    }
  }

  return True;
}

/* Returns number of characters written */
Int CLG_(sprint_cost)(Char* buf, EventSet* es, ULong* c)
{
  Int i, pos, skipped = 0;

  if (!c || es->size==0) return 0;

  /* At least one entry */
  pos = VG_(sprintf)(buf, "%llu", c[0]);
  i = 1;

  while(i<es->size) {
    if (c[i] == 0) {
      skipped  += es->e[i].nextTop - i;
      i = es->e[i].nextTop;
    }
    else {
      while(skipped>0) {
	buf[pos++] = ' ';
	buf[pos++] = '0';
	skipped--;
      }
      buf[pos++] = ' ';
      pos += VG_(sprintf)(buf+pos, "%llu", c[i]);
      i++;
    }
  }

  return pos;
}


/* Allocate space for an event mapping */
EventMapping* CLG_(get_eventmapping)(EventSet* es)
{
  EventMapping* em;

  CLG_ASSERT(es != 0);

  em = (EventMapping*) CLG_MALLOC(sizeof(EventMapping) +
				   es->capacity * sizeof(Int));
  em->capacity = es->capacity;
  em->size = 0;
  em->set = es;

  return em;
}

void CLG_(append_event)(EventMapping* em, Char* n)
{
  Int i;

  CLG_ASSERT(em != 0);

  for(i=0; i<em->set->size; i++)
    if (VG_(strcmp)(n, em->set->e[i].type->name)==0)
      break;
  
  if (i == em->set->size) return;

  CLG_ASSERT(em->capacity > em->size);

  em->index[em->size] = i;
  em->size++;
}


/* Returns number of characters written */
Int CLG_(sprint_eventmapping)(Char* buf, EventMapping* em)
{
  Int i, pos = 0;

  CLG_ASSERT(em != 0);

  for(i=0; i< em->size; i++) {
    if (pos>0) buf[pos++] = ' ';
    pos += VG_(sprintf)(buf + pos, em->set->e[em->index[i]].type->name);
  }
  buf[pos] = 0;

  return pos;
}

/* Returns number of characters written */
Int CLG_(sprint_mappingcost)(Char* buf, EventMapping* em, ULong* c)
{
  Int i, pos, skipped = 0;

  if (!c || em->size==0) return 0;

    /* At least one entry */
  pos = VG_(sprintf)(buf, "%llu", c[em->index[0]]);
  i = 1;

  while(i<em->size) {
    if (c[em->index[i]] == 0) {
      skipped++;
      i++;
    }
    else {
      while(skipped>0) {
	buf[pos++] = ' ';
	buf[pos++] = '0';
	skipped--;
      }
      buf[pos++] = ' ';
      pos += VG_(sprintf)(buf+pos, "%llu", c[em->index[i]]);
      i++;
    }
  }

  return pos;
}
