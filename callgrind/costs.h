/*--------------------------------------------------------------------*/
/*--- Callgrind                                                    ---*/
/*---                                                   ct_costs.h ---*/
/*--- (C) 2004, Josef Weidendorfer                                 ---*/
/*--------------------------------------------------------------------*/

#ifndef CT_COSTS
#define CT_COSTS

#include "pub_tool_basics.h"

#define CLG_(str) VGAPPEND(vgCallgrind_,str)

extern UInt CLG_(costarray_entries);
extern UInt CLG_(costarray_chunks);

/* Array of 64bit costs. This is separated from other structs
 * to support a dynamic number of costs for a cost item.
 * Chunks are allocated on demand, and deallocated at program termination.
 */
typedef struct _CostChunk CostChunk;
struct _CostChunk {
  Int size;
  Int used;
  CostChunk *next, *prev;
  ULong data[0];
};

/* Allocate a number of 64bit cost values.
 * Typically used from ct_events.c */
ULong* CLG_(get_costarray)(Int size);
void CLG_(free_costarrays)(void);


#endif /* CT_COSTS */
