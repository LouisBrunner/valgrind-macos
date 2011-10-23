/*--------------------------------------------------------------------*/
/*--- Callgrind                                                    ---*/
/*---                                                     events.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Callgrind, a Valgrind tool for call tracing.

   Copyright (C) 2002-2011, Josef Weidendorfer (Josef.Weidendorfer@gmx.de)

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

/* This should be 2**MAX_EVENTGROUP_COUNT */
#define MAX_EVENTSET_COUNT 1024

static EventGroup* eventGroup[MAX_EVENTGROUP_COUNT];
static EventSet* eventSetTable[MAX_EVENTSET_COUNT];
static Bool eventSets_initialized = 0;

static
void initialize_event_sets(void)
{
    Int i;

    if (eventSets_initialized) return;

    for(i=0; i< MAX_EVENTGROUP_COUNT; i++)
	eventGroup[i] = 0;

    for(i=0; i< MAX_EVENTSET_COUNT; i++)
	eventSetTable[i] = 0;

    eventSets_initialized = 1;
 }

static
EventGroup* new_event_group(int id, int n)
{
    EventGroup* eg;

    initialize_event_sets();

    CLG_ASSERT(id>=0 && id<MAX_EVENTGROUP_COUNT);
    CLG_ASSERT(eventGroup[id]==0);

    eg = (EventGroup*) CLG_MALLOC("cl.events.group.1",
				  sizeof(EventGroup) + n * sizeof(Char*));
    eg->size = n;
    eventGroup[id] = eg;
    return eg;
}

EventGroup* CLG_(register_event_group) (int id, Char* n1)
{
    EventGroup* eg = new_event_group(id, 1);
    eg->name[0] = n1;

    return eg;
}

EventGroup* CLG_(register_event_group2)(int id, Char* n1, Char* n2)
{
    EventGroup* eg = new_event_group(id, 2);
    eg->name[0] = n1;
    eg->name[1] = n2;

    return eg;
}

EventGroup* CLG_(register_event_group3)(int id, Char* n1, Char* n2, Char* n3)
{
    EventGroup* eg = new_event_group(id, 3);
    eg->name[0] = n1;
    eg->name[1] = n2;
    eg->name[2] = n3;

    return eg;
}

EventGroup* CLG_(register_event_group4)(int id,
					Char* n1, Char* n2, Char* n3, Char* n4)
{
    EventGroup* eg = new_event_group(id, 4);
    eg->name[0] = n1;
    eg->name[1] = n2;
    eg->name[2] = n3;
    eg->name[3] = n4;

    return eg;
}

EventGroup* CLG_(get_event_group)(int id)
{
    CLG_ASSERT(id>=0 && id<MAX_EVENTGROUP_COUNT);

    return eventGroup[id];
}


static
EventSet* eventset_from_mask(UInt mask)
{
    EventSet* es;
    Int i, count, offset;

    if (mask >= MAX_EVENTSET_COUNT) return 0;

    initialize_event_sets();
    if (eventSetTable[mask]) return eventSetTable[mask];

    es = (EventSet*) CLG_MALLOC("cl.events.eventset.1", sizeof(EventSet));
    es->mask = mask;

    offset = 0;
    count = 0;
    for(i=0;i<MAX_EVENTGROUP_COUNT;i++) {
	es->offset[i] = offset;
	if ( ((mask & (1u<<i))==0) || (eventGroup[i]==0))
	    continue;

	offset += eventGroup[i]->size;
	count++;
    }
    es->size = offset;
    es->count = count;

    eventSetTable[mask] = es;
    return es;
}

EventSet* CLG_(get_event_set)(Int id)
{
    CLG_ASSERT(id>=0 && id<MAX_EVENTGROUP_COUNT);
    return eventset_from_mask(1u << id);
}

EventSet* CLG_(get_event_set2)(Int id1, Int id2)
{
    CLG_ASSERT(id1>=0 && id1<MAX_EVENTGROUP_COUNT);
    CLG_ASSERT(id2>=0 && id2<MAX_EVENTGROUP_COUNT);
    return eventset_from_mask((1u << id1) | (1u << id2));
}

EventSet* CLG_(get_event_set3)(Int id1, Int id2, Int id3)
{
    CLG_ASSERT(id1>=0 && id1<MAX_EVENTGROUP_COUNT);
    CLG_ASSERT(id2>=0 && id2<MAX_EVENTGROUP_COUNT);
    CLG_ASSERT(id3>=0 && id3<MAX_EVENTGROUP_COUNT);
    return eventset_from_mask((1u << id1) | (1u << id2) | (1u << id3));
}

EventSet* CLG_(add_event_group)(EventSet* es, Int id)
{
    CLG_ASSERT(id>=0 && id<MAX_EVENTGROUP_COUNT);
    if (!es) es = eventset_from_mask(0);
    return eventset_from_mask(es->mask | (1u << id));
}

EventSet* CLG_(add_event_group2)(EventSet* es, Int id1, Int id2)
{
    CLG_ASSERT(id1>=0 && id1<MAX_EVENTGROUP_COUNT);
    CLG_ASSERT(id2>=0 && id2<MAX_EVENTGROUP_COUNT);
    if (!es) es = eventset_from_mask(0);
    return eventset_from_mask(es->mask | (1u << id1) | (1u << id2));
}

EventSet* CLG_(add_event_set)(EventSet* es1, EventSet* es2)
{
    if (!es1) es1 = eventset_from_mask(0);
    if (!es2) es2 = eventset_from_mask(0);
    return eventset_from_mask(es1->mask | es2->mask);
}

Int CLG_(sprint_eventset)(Char* buf, EventSet* es)
{
    Int i, j, pos;
    UInt mask;
    EventGroup* eg;


    CLG_ASSERT(es->size >0);
    pos = 0;
    for(i=0, mask=1; i<MAX_EVENTGROUP_COUNT; i++, mask=mask<<1) {
	if ((es->mask & mask)==0) continue;
	if (eventGroup[i] ==0) continue;

	eg = eventGroup[i];
	for(j=0; j<eg->size; j++) {
	    if (pos>0) buf[pos++] = ' ';
	    pos += VG_(sprintf)(buf + pos, "%s", eg->name[j]);
	}
    }
    buf[pos] = 0;

    return pos;
}


/* Get cost array for an event set */
ULong* CLG_(get_eventset_cost)(EventSet* es)
{
    return CLG_(get_costarray)(es->size);
}

/* Set all costs of an event set to zero */
void CLG_(init_cost)(EventSet* es, ULong* cost)
{
    Int i;

    if (!cost) return;

    for(i=0; i<es->size; i++)
	cost[i] = 0;
}

/* Set all costs of an event set to zero */
void CLG_(init_cost_lz)(EventSet* es, ULong** cost)
{
    Int i;

    CLG_ASSERT(cost != 0);
    if (!(*cost))
	*cost = CLG_(get_eventset_cost)(es);

    for(i=0; i<es->size; i++)
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
    Int i;

    if (!cost) return True;

    for(i=0; i<es->size; i++)
	if (cost[i] != 0) return False;

    return True;
}

Bool CLG_(is_equal_cost)(EventSet* es, ULong* c1, ULong* c2)
{
    Int i;

    if (!c1) return CLG_(is_zero_cost)(es, c2);
    if (!c2) return CLG_(is_zero_cost)(es, c1);

    for(i=0; i<es->size; i++)
	if (c1[i] != c2[i]) return False;

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
    Int i;

    if (!src) return;
    CLG_ASSERT(dst != 0);

    for(i=0; i<es->size; i++)
	dst[i] += src[i];
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
	CLG_(copy_cost)(es, dst, src);
	return;
    }

    for(i=0; i<es->size; i++)
	dst[i] += src[i];
}

/* Adds src to dst and zeros src. Returns false if nothing changed */
Bool CLG_(add_and_zero_cost)(EventSet* es, ULong* dst, ULong* src)
{
    Int i;
    Bool is_nonzero = False;

    CLG_ASSERT((es != 0) && (dst != 0));
    if (!src) return False;

    for(i=0; i<es->size; i++) {
	if (src[i]==0) continue;
	dst[i] += src[i];
	src[i] = 0;
	is_nonzero = True;
    }

    return is_nonzero;
}

/* Adds src to dst and zeros src. Returns false if nothing changed */
Bool CLG_(add_and_zero_cost2)(EventSet* esDst, ULong* dst,
			      EventSet* esSrc, ULong* src)
{
    Int i,j;
    Bool is_nonzero = False;
    UInt mask;
    EventGroup *eg;
    ULong *egDst, *egSrc;

    CLG_ASSERT((esDst != 0) && (dst != 0) && (esSrc != 0));
    if (!src) return False;

    for(i=0, mask=1; i<MAX_EVENTGROUP_COUNT; i++, mask=mask<<1) {
	if ((esSrc->mask & mask)==0) continue;
	if (eventGroup[i] ==0) continue;

	/* if src has a subset, dst must have, too */
	CLG_ASSERT((esDst->mask & mask)>0);
	eg = eventGroup[i];
	egSrc = src + esSrc->offset[i];
	egDst = dst + esDst->offset[i];
	for(j=0; j<eg->size; j++) {
	    if (egSrc[j]==0) continue;
	    egDst[j] += egSrc[j];
	    egSrc[j] = 0;
	    is_nonzero = True;
	}
    }

    return is_nonzero;
}



/* Adds difference of new and old to dst, and set old to new.
 * Returns false if nothing changed */
Bool CLG_(add_diff_cost)(EventSet* es, ULong* dst, ULong* old, ULong* new_cost)
{
    Int i;
    Bool is_nonzero = False;

    CLG_ASSERT((es != 0) && (dst != 0));
    CLG_ASSERT(old && new_cost);

    for(i=0; i<es->size; i++) {
	if (new_cost[i] == old[i]) continue;
	dst[i] += new_cost[i] - old[i];
	old[i] = new_cost[i];
	is_nonzero = True;
    }

    return is_nonzero;
}

Bool CLG_(add_diff_cost_lz)(EventSet* es, ULong** pdst, ULong* old, ULong* new_cost)
{
    Int i;
    ULong* dst;
    Bool is_nonzero = False;

    CLG_ASSERT((es != 0) && (pdst != 0));
    CLG_ASSERT(old && new_cost);

    dst = *pdst;
    if (!dst) {
	dst = *pdst = CLG_(get_eventset_cost)(es);
	CLG_(zero_cost)(es, dst);
    }

    for(i=0; i<es->size; i++) {
	if (new_cost[i] == old[i]) continue;
	dst[i] += new_cost[i] - old[i];
	old[i] = new_cost[i];
	is_nonzero = True;
    }

    return is_nonzero;
}


/* Returns number of characters written */
Int CLG_(sprint_cost)(Char* buf, EventSet* es, ULong* c)
{
    Int i, pos, skipped = 0;

    if (!c || es->size==0) return 0;

    /* At least one entry */
    pos = VG_(sprintf)(buf, "%llu", c[0]);
    for(i=1; i<es->size; i++) {
	if (c[i] == 0) {
	    skipped++;
	    continue;
	}
	while(skipped>0) {
	    buf[pos++] = ' ';
	    buf[pos++] = '0';
	    skipped--;
	}
	buf[pos++] = ' ';
	pos += VG_(sprintf)(buf+pos, "%llu", c[i]);
    }

    return pos;
}


/* Allocate space for an event mapping */
EventMapping* CLG_(get_eventmapping)(EventSet* es)
{
    EventMapping* em;

    CLG_ASSERT(es != 0);

    em = (EventMapping*) CLG_MALLOC("cl.events.geMapping.1",
				    sizeof(EventMapping) +
				    sizeof(struct EventMappingEntry) *
				    es->size);
    em->capacity = es->size;
    em->size = 0;
    em->es = es;

    return em;
}

void CLG_(append_event)(EventMapping* em, Char* n)
{
    Int i, j, offset = 0;
    UInt mask;
    EventGroup* eg;

    CLG_ASSERT(em != 0);
    for(i=0, mask=1; i<MAX_EVENTGROUP_COUNT; i++, mask=mask<<1) {
	if ((em->es->mask & mask)==0) continue;
	if (eventGroup[i] ==0) continue;

	eg = eventGroup[i];
	for(j=0; j<eg->size; j++, offset++) {
	    if (VG_(strcmp)(n, eg->name[j])!=0)
		    continue;

	    CLG_ASSERT(em->capacity > em->size);
	    em->entry[em->size].group = i;
	    em->entry[em->size].index = j;
	    em->entry[em->size].offset = offset;
	    em->size++;
	    return;
	}
    }
}


/* Returns number of characters written */
Int CLG_(sprint_eventmapping)(Char* buf, EventMapping* em)
{
    Int i, pos = 0;
    EventGroup* eg;

    CLG_ASSERT(em != 0);

    for(i=0; i< em->size; i++) {
	if (pos>0) buf[pos++] = ' ';
	eg = eventGroup[em->entry[i].group];
	CLG_ASSERT(eg != 0);
	pos += VG_(sprintf)(buf + pos, "%s", eg->name[em->entry[i].index]);
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
    pos = VG_(sprintf)(buf, "%llu", c[em->entry[0].offset]);

    for(i=1; i<em->size; i++) {
	if (c[em->entry[i].offset] == 0) {
	    skipped++;
	    continue;
	}
	while(skipped>0) {
	    buf[pos++] = ' ';
	    buf[pos++] = '0';
	    skipped--;
	}
	buf[pos++] = ' ';
	pos += VG_(sprintf)(buf+pos, "%llu", c[em->entry[i].offset]);
    }

    return pos;
}
