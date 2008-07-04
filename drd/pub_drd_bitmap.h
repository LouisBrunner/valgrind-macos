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


// A bitmap is a data structure that contains information about which
// addresses have been accessed for reading or writing within a given
// segment.


#ifndef __PUB_DRD_BITMAP_H
#define __PUB_DRD_BITMAP_H


#include "pub_tool_basics.h" /* Addr, SizeT */


// Constant definitions.

#define LHS_R (1<<0)
#define LHS_W (1<<1)
#define RHS_R (1<<2)
#define RHS_W (1<<3)
#define HAS_RACE(a) ((((a) & RHS_W) && ((a) & (LHS_R | LHS_W))) \
                  || (((a) & LHS_W) && ((a) & (RHS_R | RHS_W))))


// Forward declarations.
struct bitmap;


// Datatype definitions.
typedef enum { eLoad, eStore, eStart, eEnd } BmAccessTypeT;


// Function declarations.
struct bitmap* bm_new(void);
void bm_delete(struct bitmap* const bm);
void bm_access_range(struct bitmap* const bm,
                     const Addr a1, const Addr a2,
                     const BmAccessTypeT access_type);
void bm_access_range_load(struct bitmap* const bm,
                          const Addr a1, const Addr a2);
void bm_access_load_1(struct bitmap* const bm, const Addr a1);
void bm_access_load_2(struct bitmap* const bm, const Addr a1);
void bm_access_load_4(struct bitmap* const bm, const Addr a1);
void bm_access_load_8(struct bitmap* const bm, const Addr a1);
void bm_access_range_store(struct bitmap* const bm,
                           const Addr a1, const Addr a2);
void bm_access_store_1(struct bitmap* const bm, const Addr a1);
void bm_access_store_2(struct bitmap* const bm, const Addr a1);
void bm_access_store_4(struct bitmap* const bm, const Addr a1);
void bm_access_store_8(struct bitmap* const bm, const Addr a1);
Bool bm_has(struct bitmap* const bm,
            const Addr a1, const Addr a2,
            const BmAccessTypeT access_type);
Bool bm_has_any_load(struct bitmap* const bm,
                     const Addr a1, const Addr a2);
Bool bm_has_any_store(struct bitmap* const bm,
                      const Addr a1, const Addr a2);
Bool bm_has_any_access(struct bitmap* const bm,
                       const Addr a1, const Addr a2);
Bool bm_has_1(struct bitmap* const bm,
              const Addr address, const BmAccessTypeT access_type);
void bm_clear(struct bitmap* const bm,
              const Addr a1, const Addr a2);
void bm_clear_load(struct bitmap* const bm,
                   const Addr a1, const Addr a2);
void bm_clear_store(struct bitmap* const bm,
                    const Addr a1, const Addr a2);
Bool bm_test_and_clear(struct bitmap* const bm,
                       const Addr a1, const Addr a2);
Bool bm_has_conflict_with(struct bitmap* const bm,
                          const Addr a1, const Addr a2,
                          const BmAccessTypeT access_type);
Bool bm_load_1_has_conflict_with(struct bitmap* const bm, const Addr a1);
Bool bm_load_2_has_conflict_with(struct bitmap* const bm, const Addr a1);
Bool bm_load_4_has_conflict_with(struct bitmap* const bm, const Addr a1);
Bool bm_load_8_has_conflict_with(struct bitmap* const bm, const Addr a1);
Bool bm_load_has_conflict_with(struct bitmap* const bm,
                               const Addr a1, const Addr a2);
Bool bm_store_1_has_conflict_with(struct bitmap* const bm,const Addr a1);
Bool bm_store_2_has_conflict_with(struct bitmap* const bm,const Addr a1);
Bool bm_store_4_has_conflict_with(struct bitmap* const bm,const Addr a1);
Bool bm_store_8_has_conflict_with(struct bitmap* const bm,const Addr a1);
Bool bm_store_has_conflict_with(struct bitmap* const bm,
                                const Addr a1, const Addr a2);
Bool bm_equal(struct bitmap* const lhs, struct bitmap* const rhs);
void bm_swap(struct bitmap* const bm1, struct bitmap* const bm2);
void bm_merge2(struct bitmap* const lhs,
               struct bitmap* const rhs);
int bm_has_races(struct bitmap* const bm1,
                 struct bitmap* const bm2);
void bm_report_races(ThreadId const tid1, ThreadId const tid2,
                     struct bitmap* const bm1,
                     struct bitmap* const bm2);
void bm_print(struct bitmap* bm);
ULong bm_get_bitmap_creation_count(void);
ULong bm_get_bitmap2_node_creation_count(void);
ULong bm_get_bitmap2_creation_count(void);

void bm_test(void);


#endif /* __PUB_DRD_BITMAP_H */
