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
 * A bitmap is a data structure that contains information about which
 * addresses have been accessed for reading or writing within a given
 * segment.
 */


#ifndef __PUB_DRD_BITMAP_H
#define __PUB_DRD_BITMAP_H


#include "drd_basics.h"      /* DRD_() */
#include "pub_tool_basics.h" /* Addr, SizeT */
#include "pub_tool_oset.h"   /* struct _OSet */


/* Defines. */

#define LHS_R (1<<0)
#define LHS_W (1<<1)
#define RHS_R (1<<2)
#define RHS_W (1<<3)
#define HAS_RACE(a) ((((a) & RHS_W) && ((a) & (LHS_R | LHS_W)))         \
                     || (((a) & LHS_W) && ((a) & (RHS_R | RHS_W))))


/* Forward declarations. */

struct bitmap;


/* Datatype definitions. */

typedef enum { eLoad, eStore, eStart, eEnd } BmAccessTypeT;

struct bm_cache_elem
{
   Addr            a1;
   struct bitmap2* bm2;
};

#define DRD_BITMAP_N_CACHE_ELEM 4

/* Complete bitmap. */
struct bitmap
{
   struct bm_cache_elem cache[DRD_BITMAP_N_CACHE_ELEM];
   OSet*                oset;
};


/* Function declarations. */

struct bitmap* DRD_(bm_new)(void);
void DRD_(bm_delete)(struct bitmap* const bm);
void DRD_(bm_init)(struct bitmap* const bm);
void DRD_(bm_cleanup)(struct bitmap* const bm);
void DRD_(bm_access_range)(struct bitmap* const bm,
                           const Addr a1, const Addr a2,
                           const BmAccessTypeT access_type);
void DRD_(bm_access_range_load)(struct bitmap* const bm,
                                const Addr a1, const Addr a2);
void DRD_(bm_access_load_1)(struct bitmap* const bm, const Addr a1);
void DRD_(bm_access_load_2)(struct bitmap* const bm, const Addr a1);
void DRD_(bm_access_load_4)(struct bitmap* const bm, const Addr a1);
void DRD_(bm_access_load_8)(struct bitmap* const bm, const Addr a1);
void DRD_(bm_access_range_store)(struct bitmap* const bm,
                                 const Addr a1, const Addr a2);
void DRD_(bm_access_store_1)(struct bitmap* const bm, const Addr a1);
void DRD_(bm_access_store_2)(struct bitmap* const bm, const Addr a1);
void DRD_(bm_access_store_4)(struct bitmap* const bm, const Addr a1);
void DRD_(bm_access_store_8)(struct bitmap* const bm, const Addr a1);
Bool DRD_(bm_has)(struct bitmap* const bm,
                  const Addr a1, const Addr a2,
                  const BmAccessTypeT access_type);
Bool DRD_(bm_has_any_load)(struct bitmap* const bm,
                           const Addr a1, const Addr a2);
Bool DRD_(bm_has_any_store)(struct bitmap* const bm,
                            const Addr a1, const Addr a2);
Bool DRD_(bm_has_any_access)(struct bitmap* const bm,
                             const Addr a1, const Addr a2);
Bool DRD_(bm_has_1)(struct bitmap* const bm,
                    const Addr address, const BmAccessTypeT access_type);
void DRD_(bm_clear)(struct bitmap* const bm,
                    const Addr a1, const Addr a2);
void DRD_(bm_clear_load)(struct bitmap* const bm,
                         const Addr a1, const Addr a2);
void DRD_(bm_clear_store)(struct bitmap* const bm,
                          const Addr a1, const Addr a2);
Bool DRD_(bm_test_and_clear)(struct bitmap* const bm,
                             const Addr a1, const Addr a2);
Bool DRD_(bm_has_conflict_with)(struct bitmap* const bm,
                                const Addr a1, const Addr a2,
                                const BmAccessTypeT access_type);
Bool DRD_(bm_load_1_has_conflict_with)(struct bitmap* const bm, const Addr a1);
Bool DRD_(bm_load_2_has_conflict_with)(struct bitmap* const bm, const Addr a1);
Bool DRD_(bm_load_4_has_conflict_with)(struct bitmap* const bm, const Addr a1);
Bool DRD_(bm_load_8_has_conflict_with)(struct bitmap* const bm, const Addr a1);
Bool DRD_(bm_load_has_conflict_with)(struct bitmap* const bm,
                                     const Addr a1, const Addr a2);
Bool DRD_(bm_store_1_has_conflict_with)(struct bitmap* const bm,const Addr a1);
Bool DRD_(bm_store_2_has_conflict_with)(struct bitmap* const bm,const Addr a1);
Bool DRD_(bm_store_4_has_conflict_with)(struct bitmap* const bm,const Addr a1);
Bool DRD_(bm_store_8_has_conflict_with)(struct bitmap* const bm,const Addr a1);
Bool DRD_(bm_store_has_conflict_with)(struct bitmap* const bm,
                                      const Addr a1, const Addr a2);
Bool DRD_(bm_equal)(struct bitmap* const lhs, struct bitmap* const rhs);
void DRD_(bm_swap)(struct bitmap* const bm1, struct bitmap* const bm2);
void DRD_(bm_merge2)(struct bitmap* const lhs, struct bitmap* const rhs);
void DRD_(bm_unmark)(struct bitmap* bm);
Bool DRD_(bm_is_marked)(struct bitmap* bm, const Addr a);
void DRD_(bm_mark)(struct bitmap* bm1, struct bitmap* bm2);
void DRD_(bm_clear_marked)(struct bitmap* bm);
void DRD_(bm_merge2_marked)(struct bitmap* const lhs, struct bitmap* const rhs);
void DRD_(bm_remove_cleared_marked)(struct bitmap* bm);
int DRD_(bm_has_races)(struct bitmap* const bm1,
                       struct bitmap* const bm2);
void DRD_(bm_report_races)(ThreadId const tid1, ThreadId const tid2,
                           struct bitmap* const bm1,
                           struct bitmap* const bm2);
void DRD_(bm_print)(struct bitmap* bm);
ULong DRD_(bm_get_bitmap_creation_count)(void);
ULong DRD_(bm_get_bitmap2_creation_count)(void);
ULong DRD_(bm_get_bitmap2_merge_count)(void);

void* DRD_(bm2_alloc_node)(HChar* const ec, const SizeT szB);
void  DRD_(bm2_free_node)(void* const bm2);

#endif /* __PUB_DRD_BITMAP_H */
