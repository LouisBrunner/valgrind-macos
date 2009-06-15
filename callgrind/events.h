/*--------------------------------------------------------------------*/
/*--- Callgrind                                                    ---*/
/*---                                                     events.h ---*/
/*--- (C) 2004-2005, Josef Weidendorfer                            ---*/
/*--------------------------------------------------------------------*/


/* Abstractions for 64-bit cost lists (events.h) */

#ifndef CG_EVENTS
#define CG_EVENTS

#include "pub_tool_basics.h"

#define CLG_(str) VGAPPEND(vgCallgrind_,str)

/* An event type */
typedef struct _EventType EventType;
struct _EventType {
  Char* name;
  Char* description;
  Int id;
};

EventType* CLG_(register_eventtype)(Char*);
EventType* CLG_(get_eventtype)(Char*);
EventType* CLG_(get_eventtype_byindex)(Int id);

/* An event set is a ordered list of event types, which comes down
 * to some description for ordered lists of costs.
 * Often, costs of 2 event types are related, e.g. one is always smaller
 * than the other. This is useful to speed up arithmetics on cost lists:
 * Each event type in the set has a <nextTop>. All indexes before are
 * promised to hold smaller values than the current.
 */
typedef struct _EventSetEntry EventSetEntry;
struct _EventSetEntry {
  EventType* type;
  Int nextTop;
};
typedef struct _EventSet EventSet;
struct _EventSet {
  Char* name;
  Int size;
  Int capacity;
  EventSetEntry e[0];
};


/* Some events out of an event set.
 * Used to print out part of an EventSet, or in another order.
 */
typedef struct _EventMapping EventMapping;
struct _EventMapping {
  EventSet* set;
  Int size;
  Int capacity;
  Int index[0];
};

  
/* Allocate space for an event set */
EventSet* CLG_(get_eventset)(Char* n, Int capacity);
/* Incorporate a event type into a set, get start offset */
Int CLG_(add_eventtype)(EventSet* dst, EventType*);
/* Incorporate event types into a set, with ... < second < first */
Int CLG_(add_dep_event2)(EventSet* dst, EventType* e1, EventType* e2);
Int CLG_(add_dep_event3)(EventSet* dst,
			EventType* e1, EventType* e2, EventType* e3);
Int CLG_(add_dep_event4)(EventSet* dst,
			EventType* e1, EventType* e2, EventType* e3,
			EventType* e4);
/* Incorporate one event set into another, get start offset */
Int CLG_(add_eventset)(EventSet* dst, EventSet* src);
/* Returns number of characters written */
Int CLG_(sprint_eventset)(Char* buf, EventSet*);
/* Allocate cost array for an event set */
ULong* CLG_(get_eventset_cost)(EventSet*);

/* Operations on costs. A cost pointer of 0 means zero cost.
 * Functions ending in _lz allocate costs lazy if needed
 */
/* Set costs according full capacity of event set to 0 */
void CLG_(init_cost)(EventSet*,ULong*);
/* This always allocates counter and sets them to 0 */
void CLG_(init_cost_lz)(EventSet*,ULong**);
/* Set costs of an event set to zero */
void CLG_(zero_cost)(EventSet*,ULong*);
Bool CLG_(is_zero_cost)(EventSet*,ULong*);
Bool CLG_(is_equal_cost)(EventSet*,ULong*,ULong*);
void CLG_(copy_cost)(EventSet*,ULong* dst, ULong* src);
void CLG_(copy_cost_lz)(EventSet*,ULong** pdst, ULong* src);
void CLG_(add_cost)(EventSet*,ULong* dst, ULong* src);
void CLG_(add_cost_lz)(EventSet*,ULong** pdst, ULong* src);
/* Adds src to dst and zeros src. Returns false if nothing changed */
Bool CLG_(add_and_zero_cost)(EventSet*,ULong* dst, ULong* src);
Bool CLG_(add_and_zero_cost_lz)(EventSet*,ULong** pdst, ULong* src);
/* Adds difference of new and old to to dst, and set old to new.
 * Returns false if nothing changed */
Bool CLG_(add_diff_cost)(EventSet*,ULong* dst, ULong* old, ULong* new_cost);
Bool CLG_(add_diff_cost_lz)(EventSet*,ULong** pdst, ULong* old, ULong* new_cost);
/* Returns number of characters written */
Int CLG_(sprint_cost)(Char* buf, EventSet*, ULong*);

/* Allocate space for an event mapping */
EventMapping* CLG_(get_eventmapping)(EventSet*);
void CLG_(append_event)(EventMapping*, Char*);
/* Returns number of characters written */
Int CLG_(sprint_eventmapping)(Char* buf, EventMapping*);
/* Returns number of characters written */
Int CLG_(sprint_mappingcost)(Char* buf, EventMapping*, ULong*);

#endif /* CG_EVENTS */
