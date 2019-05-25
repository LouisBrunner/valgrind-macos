
/*--------------------------------------------------------------------*/
/*--- Support functions for xtree memory reports. m_xtmemory.c     ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2016-2017 Philippe Waroquiers

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

#include "pub_core_libcassert.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcprint.h"
#include "pub_core_libcproc.h"
#include "pub_core_mallocfree.h"
#include "pub_core_options.h"
#include "pub_core_xarray.h"
#include "pub_core_xtree.h"
#include "pub_core_xtmemory.h"    /* self */

static void VG_(XT_Allocs_init)(void* xt_allocs)
{
   VG_(memset) (xt_allocs, 0, sizeof(XT_Allocs));
}
static void VG_(XT_Allocs_add) (void* to, const void* xt_allocs)
{
   XT_Allocs* xto = to;
   const XT_Allocs* xta = xt_allocs;

   xto->nbytes  += xta->nbytes;
   xto->nblocks += xta->nblocks;
}
static void VG_(XT_Allocs_sub) (void* from, const void* xt_allocs)
{
   XT_Allocs* xfrom = from;
   const XT_Allocs* xta = xt_allocs;

   xfrom->nbytes  -= xta->nbytes;
   xfrom->nblocks -= xta->nblocks;
}
static const HChar* VG_(XT_Allocs_img) (const void* xt_allocs)
{
   static HChar buf[100];

   const XT_Allocs* xta = xt_allocs;
   
   if (xta->nbytes > 0 || xta->nblocks > 0) {
      VG_(sprintf) (buf, "%lu %lu",
                    xta->nbytes, xta->nblocks);
      return buf;
   } else {
      return NULL;
   }
}
const HChar* XT_Allocs_events = "curB : currently allocated Bytes"   ","
                                "curBk : currently allocated Blocks";

/* Type and functions for full xtree memory profiling. */
static XTree* full_xt;
typedef
   struct _XT_Full {
      // Current nr of bytes/blocks allocated by this ec
      SizeT cur_alloc_nbytes;
      SizeT cur_alloc_nblocks;

      // Total/cumulative nr of bytes/blocks allocated by this ec
      ULong tot_alloc_nbytes;
      ULong tot_alloc_nblocks;

      // Total/cumulative nr of bytes/blocks freed by this ec
      ULong tot_freed_nbytes;
      ULong tot_freed_nblocks;
   } XT_Full;
/* Note: normally, an ec should never be used as both an alloc_ec and
   a free_ec. This implies that we should never have a XT_Full that has
   at the same time some alloc and some freed components > 0.
   We however still will support this possibility, just in case very
   strange ec are produced and/or given by the tool. */

static void VG_(XT_Full_init)(void* xtfull)
{
   VG_(memset) (xtfull, 0, sizeof(XT_Full));
}
static void VG_(XT_Full_add) (void* to, const void* xtfull)
{
   XT_Full* xto = to;
   const XT_Full* xtf = xtfull;

   xto->cur_alloc_nbytes  += xtf->cur_alloc_nbytes;
   xto->cur_alloc_nblocks += xtf->cur_alloc_nblocks;
   xto->tot_alloc_nbytes  += xtf->tot_alloc_nbytes;
   xto->tot_alloc_nblocks += xtf->tot_alloc_nblocks;
   xto->tot_freed_nbytes  += xtf->tot_freed_nbytes;
   xto->tot_freed_nblocks += xtf->tot_freed_nblocks;
}
static void VG_(XT_Full_sub) (void* from, const void* xtfull)
{
   XT_Full* xfrom = from;
   const XT_Full* xtf = xtfull;

   xfrom->cur_alloc_nbytes  -= xtf->cur_alloc_nbytes;
   xfrom->cur_alloc_nblocks -= xtf->cur_alloc_nblocks;
   xfrom->tot_alloc_nbytes  -= xtf->tot_alloc_nbytes;
   xfrom->tot_alloc_nblocks -= xtf->tot_alloc_nblocks;
   xfrom->tot_freed_nbytes  -= xtf->tot_freed_nbytes;
   xfrom->tot_freed_nblocks -= xtf->tot_freed_nblocks;
}
static const HChar* VG_(XT_Full_img) (const void* xtfull)
{
   static HChar buf[300];

   const XT_Full* xtf = xtfull;
   
   if (   xtf->cur_alloc_nbytes  > 0
       || xtf->cur_alloc_nblocks > 0
       || xtf->tot_alloc_nbytes  > 0
       || xtf->tot_alloc_nblocks > 0
       || xtf->tot_freed_nbytes  > 0
       || xtf->tot_freed_nblocks > 0) {
      VG_(sprintf) (buf, 
                    "%lu %lu "
                    "%llu %llu "
                    "%llu %llu",
                    xtf->cur_alloc_nbytes, xtf->cur_alloc_nblocks,
                    xtf->tot_alloc_nbytes, xtf->tot_alloc_nblocks,
                    xtf->tot_freed_nbytes, xtf->tot_freed_nblocks);
      return buf;
   } else {
      return NULL;
   }
}
static const HChar* XT_Full_events = 
   "curB : currently allocated Bytes"   ","
   "curBk : currently allocated Blocks" ","
   "totB : total allocated Bytes"       ","
   "totBk : total allocated Blocks"     ","
   "totFdB : total Freed Bytes"         ","
   "totFdBk : total Freed Blocks";
void VG_(XTMemory_Full_init)(XT_filter_IPs_t filter_IPs_fn)
{
   full_xt = VG_(XT_create) (VG_(malloc),
                             "m_xtree.full_xt",
                             VG_(free),
                             sizeof(XT_Full),
                             VG_(XT_Full_init),
                             VG_(XT_Full_add),
                             VG_(XT_Full_sub),
                             filter_IPs_fn);
}
void VG_(XTMemory_Full_alloc)(SizeT szB,
                              ExeContext* ec_alloc)
{
   XT_Full xtf = {szB, 1, szB, 1, 0, 0};
   VG_(XT_add_to_ec)(full_xt, ec_alloc, &xtf);
}
void VG_(XTMemory_Full_free)(SizeT szB,
                             ExeContext* ec_alloc,
                             ExeContext* ec_free)
{
   // substract from ec_alloc the freed memory.
   XT_Full xtf_sub = {szB, 1, 0, 0, 0, 0};
   VG_(XT_sub_from_ec)(full_xt, ec_alloc, &xtf_sub);

   // add to ec_free the freed memory
   XT_Full xtf_add = {0, 0, 0, 0, szB, 1};
   VG_(XT_add_to_ec)(full_xt, ec_free, &xtf_add);
}

void VG_(XTMemory_Full_resize_in_place)(SizeT oldSzB, SizeT newSzB,
                                        ExeContext* ec_alloc)
{
   if (oldSzB > newSzB) {
      XT_Full xtf = {oldSzB - newSzB, 0, oldSzB - newSzB, 0, 0, 0};
      VG_(XT_sub_from_ec)(full_xt, ec_alloc, &xtf);
   } else {
      XT_Full xtf = {newSzB - oldSzB, 0, newSzB - oldSzB, 0, 0, 0};
      VG_(XT_add_to_ec)(full_xt, ec_alloc, &xtf);
   }
}

// Indicates which event nr the report_value function must return.
static UInt event_report_value_id;
static ULong XT_Full_report_value(const void* xtfull)
{
   const XT_Full* xtf = xtfull;   
   switch (event_report_value_id) {
      case 0: return (ULong) xtf->cur_alloc_nbytes;
      case 1: return (ULong) xtf->cur_alloc_nblocks;
      case 2: return xtf->tot_alloc_nbytes;
      case 3: return xtf->tot_alloc_nblocks;
      case 4: return xtf->tot_freed_nbytes;
      case 5: return xtf->tot_freed_nblocks;
      default: vg_assert(0);
   }
}
static ULong XT_Allocs_report_value(const void* xt_allocs)
{
   const XT_Allocs* xta = xt_allocs;   
   switch (event_report_value_id) {
      case 0: return (ULong) xta->nbytes;
      case 1: return (ULong) xta->nblocks;
      default: vg_assert(0);
   }
}

static void produce_report(XTree* xt, const HChar* filename,
                           const HChar* events,
                           const HChar* (*img_value) (const void* value),
                           ULong (*report_value)(const void* value))
{
   /* The user can control the kind of report using filename extension. */
   if (VG_(strstr)(filename, ".ms")) {
      /* If needed, some harcoded value below could become parameters. */
      MsFile* fp;
      Massif_Header header = (Massif_Header) {
         .snapshot_n    = 0,
         .time          = VG_(read_millisecond_timer)(),
         .sz_B          = 0ul,
         .extra_B       = 0ul,
         .stacks_B      = 0ul,
         .detailed      = True,
         .peak          = False,
         .top_node_desc = NULL,
         .sig_threshold = 0.00000000000001
         // Currently, we take a very small float value to not output
         // the 0 values, but still output all the rest.
      };

      // Variables to parse events
      HChar strtok_events[VG_(strlen)(events)+1];
      HChar* e;
      HChar* ssaveptr;

      fp = VG_(XT_massif_open)(filename,
                               "xtree.produce_report",
                               NULL,
                               "ms");

      event_report_value_id = 0;
      VG_(strcpy)(strtok_events, events);
      for (e = VG_(strtok_r) (strtok_events, ",", &ssaveptr); 
           e != NULL; 
           e = VG_(strtok_r) (NULL, ",", &ssaveptr)) {
         header.top_node_desc = e;
         VG_(XT_massif_print)(fp, xt, &header, report_value);
         header.snapshot_n++;
         event_report_value_id++;
      }

      VG_(XT_massif_close)(fp);
   } else
      VG_(XT_callgrind_print)(xt,
                             filename,
                             events,
                             img_value);
}

void VG_(XTMemory_report) 
     (const HChar* filename, Bool fini,
      void (*next_block)(XT_Allocs* xta, ExeContext** ec_alloc),
      XT_filter_IPs_t filter_IPs_fn)
{
   HChar* expanded_filename;

   if (fini && VG_(clo_xtree_memory) == Vg_XTMemory_None)
      return;

   expanded_filename 
      = VG_(expand_file_name)("--xtree-memory-file",
                              (filename == NULL) ?
                              (fini ? 
                               VG_(clo_xtree_memory_file)
                               : "xtmemory.kcg.%p.%n")
                              : filename);

   /* fini is False => even if user kept --xtree-memory=none, we
      produce a report when explicitely requested e.g. via a monitor
      command. */
   switch (VG_(clo_xtree_memory)) {
      case Vg_XTMemory_None:
      case Vg_XTMemory_Allocs: {
         XTree* xt;
         XT_Allocs  xta;
         ExeContext* ec_alloc;

         xt = VG_(XT_create) (VG_(malloc),
                              "VG_(XTMemory_report)",
                              VG_(free),
                              sizeof(XT_Allocs),
                              VG_(XT_Allocs_init),
                              VG_(XT_Allocs_add),
                              VG_(XT_Allocs_sub),
                              filter_IPs_fn);
         (*next_block)(&xta, &ec_alloc);
         while ( xta.nblocks > 0 ) {
            VG_(XT_add_to_ec) (xt, ec_alloc, &xta);
            (*next_block)(&xta, &ec_alloc);
         }

         produce_report(xt, expanded_filename,
                        XT_Allocs_events, VG_(XT_Allocs_img),
                        XT_Allocs_report_value);

         VG_(XT_delete)(xt);
         break;
      }
      case Vg_XTMemory_Full:
         produce_report(full_xt, expanded_filename,
                        XT_Full_events, VG_(XT_Full_img),
                        XT_Full_report_value);
         break;
      default: 
         vg_assert(0);
   }
   if (VG_(clo_verbosity) >= 1 || !fini)
      VG_(umsg)("xtree memory report: %s\n", expanded_filename);

   VG_(free)(expanded_filename);
}

/*--------------------------------------------------------------------*/
/*--- end                                                m_xtree.c ---*/
/*--------------------------------------------------------------------*/
