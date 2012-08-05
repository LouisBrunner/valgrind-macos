/*
  This file is part of drd, a thread error detector.

  Copyright (C) 2006-2012 Bart Van Assche <bvanassche@acm.org>.

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


#ifndef __DRD_THREAD_BITMAP_H
#define __DRD_THREAD_BITMAP_H


#include "drd_bitmap.h"
#include "drd_thread.h" /* running_thread_get_segment() */
#include "pub_drd_bitmap.h"


static __inline__
Bool bm_access_load_1_triggers_conflict(const Addr a1)
{
   DRD_(bm_access_load_1)(DRD_(sg_bm)(DRD_(running_thread_get_segment)()), a1);
   return DRD_(bm_load_1_has_conflict_with)(DRD_(thread_get_conflict_set)(),
                                            a1);
}

static __inline__
Bool bm_access_load_2_triggers_conflict(const Addr a1)
{
   if ((a1 & 1) == 0)
   {
      bm_access_aligned_load(DRD_(sg_bm)(DRD_(running_thread_get_segment)()), a1, 2);
      return bm_aligned_load_has_conflict_with(DRD_(thread_get_conflict_set)(),
                                               a1, 2);
   }
   else
   {
      DRD_(bm_access_range)(DRD_(sg_bm)(DRD_(running_thread_get_segment)()),
                            a1, a1 + 2, eLoad);
      return DRD_(bm_has_conflict_with)(DRD_(thread_get_conflict_set)(),
                                        a1, a1 + 2, eLoad);
   }
}

static __inline__
Bool bm_access_load_4_triggers_conflict(const Addr a1)
{
   if ((a1 & 3) == 0)
   {
      bm_access_aligned_load(DRD_(sg_bm)(DRD_(running_thread_get_segment)()), a1, 4);
      return bm_aligned_load_has_conflict_with(DRD_(thread_get_conflict_set)(),
                                               a1, 4);
   }
   else
   {
      DRD_(bm_access_range)(DRD_(sg_bm)(DRD_(running_thread_get_segment)()),
                            a1, a1 + 4, eLoad);
      return DRD_(bm_has_conflict_with)(DRD_(thread_get_conflict_set)(),
                                        a1, a1 + 4, eLoad);
   }
}

static __inline__
Bool bm_access_load_8_triggers_conflict(const Addr a1)
{
   if ((a1 & 7) == 0)
   {
      bm_access_aligned_load(DRD_(sg_bm)(DRD_(running_thread_get_segment)()), a1, 8);
      return bm_aligned_load_has_conflict_with(DRD_(thread_get_conflict_set)(),
                                               a1, 8);
   }
   else if ((a1 & 3) == 0)
   {
      bm_access_aligned_load(DRD_(sg_bm)(DRD_(running_thread_get_segment)()), a1 + 0, 4);
      bm_access_aligned_load(DRD_(sg_bm)(DRD_(running_thread_get_segment)()), a1 + 4, 4);
      return DRD_(bm_has_conflict_with)(DRD_(thread_get_conflict_set)(),
                                        a1, a1 + 8, eLoad);
   }
   else
   {
      DRD_(bm_access_range)(DRD_(sg_bm)(DRD_(running_thread_get_segment)()),
                            a1, a1 + 8, eLoad);
      return DRD_(bm_has_conflict_with)(DRD_(thread_get_conflict_set)(),
                                        a1, a1 + 8, eLoad);
   }
}

static __inline__
Bool bm_access_load_triggers_conflict(const Addr a1, const Addr a2)
{
   DRD_(bm_access_range_load)(DRD_(sg_bm)(DRD_(running_thread_get_segment)()), a1, a2);
   return DRD_(bm_load_has_conflict_with)(DRD_(thread_get_conflict_set)(),
                                          a1, a2);
}

static __inline__
Bool bm_access_store_1_triggers_conflict(const Addr a1)
{
   DRD_(bm_access_store_1)(DRD_(sg_bm)(DRD_(running_thread_get_segment)()), a1);
   return DRD_(bm_store_1_has_conflict_with)(DRD_(thread_get_conflict_set)(),
                                             a1);
}

static __inline__
Bool bm_access_store_2_triggers_conflict(const Addr a1)
{
   if ((a1 & 1) == 0)
   {
      bm_access_aligned_store(DRD_(sg_bm)(DRD_(running_thread_get_segment)()), a1, 2);
      return bm_aligned_store_has_conflict_with(DRD_(thread_get_conflict_set)(),
                                                a1, 2);
   }
   else
   {
      DRD_(bm_access_range)(DRD_(sg_bm)(DRD_(running_thread_get_segment)()),
                            a1, a1 + 2, eStore);
      return DRD_(bm_has_conflict_with)(DRD_(thread_get_conflict_set)(),
                                        a1, a1 + 2, eStore);
   }
}

static __inline__
Bool bm_access_store_4_triggers_conflict(const Addr a1)
{
   if ((a1 & 3) == 0)
   {
      bm_access_aligned_store(DRD_(sg_bm)(DRD_(running_thread_get_segment)()), a1, 4);
      return bm_aligned_store_has_conflict_with(DRD_(thread_get_conflict_set)(),
                                                a1, 4);
   }
   else
   {
      DRD_(bm_access_range)(DRD_(sg_bm)(DRD_(running_thread_get_segment)()),
                            a1, a1 + 4, eStore);
      return DRD_(bm_has_conflict_with)(DRD_(thread_get_conflict_set)(),
                                        a1, a1 + 4, eStore);
   }
}

static __inline__
Bool bm_access_store_8_triggers_conflict(const Addr a1)
{
   if ((a1 & 7) == 0)
   {
      bm_access_aligned_store(DRD_(sg_bm)(DRD_(running_thread_get_segment)()), a1, 8);
      return bm_aligned_store_has_conflict_with(DRD_(thread_get_conflict_set)(),
                                                a1, 8);
   }
   else if ((a1 & 3) == 0)
   {
      bm_access_aligned_store(DRD_(sg_bm)(DRD_(running_thread_get_segment)()),
                              a1 + 0, 4);
      bm_access_aligned_store(DRD_(sg_bm)(DRD_(running_thread_get_segment)()),
                              a1 + 4, 4);
      return DRD_(bm_has_conflict_with)(DRD_(thread_get_conflict_set)(),
                                        a1, a1 + 8, eStore);
   }
   else
   {
      DRD_(bm_access_range)(DRD_(sg_bm)(DRD_(running_thread_get_segment)()),
                            a1, a1 + 8, eStore);
      return DRD_(bm_has_conflict_with)(DRD_(thread_get_conflict_set)(),
                                        a1, a1 + 8, eStore);
   }
}

static __inline__
Bool bm_access_store_triggers_conflict(const Addr a1, const Addr a2)
{
   DRD_(bm_access_range_store)(DRD_(sg_bm)(DRD_(running_thread_get_segment)()), a1, a2);
   return DRD_(bm_store_has_conflict_with)(DRD_(thread_get_conflict_set)(),
                                           a1, a2);
}

#endif // __DRD_THREAD_BITMAP_H
