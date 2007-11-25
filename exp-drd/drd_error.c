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


#include "drd_error.h"
#include "drd_malloc_wrappers.h"
#include "drd_mutex.h"            // struct mutex_info
#include "drd_suppression.h"      // drd_start_suppression()
#include "pub_drd_bitmap.h"       // LHS_W, ...
#include "pub_tool_vki.h"
#include "pub_tool_basics.h"
#include "pub_tool_libcassert.h"  // tl_assert()
#include "pub_tool_libcbase.h"    // strlen()
#include "pub_tool_libcfile.h"    // VG_(get_startup_wd)()
#include "pub_tool_libcprint.h"   // VG_(printf)()
#include "pub_tool_machine.h"
#include "pub_tool_threadstate.h" // VG_(get_pthread_id)()
#include "pub_tool_tooliface.h"   // VG_(needs_tool_errors)()


typedef enum {
   ConflictingAccessSupp
} DRD_SuppKind;


static void make_path_relative(Char* const path)
{
   int offset = 0;
   Char cwd[512];

   if (! VG_(get_startup_wd)(cwd, sizeof(cwd)))
      tl_assert(False);
   if (VG_(strncmp)(path + offset, cwd, VG_(strlen)(cwd)) == 0)
   {
      offset += VG_(strlen)(cwd);
      if (path[offset] == '/')
      {
         offset++;
      }
   }
   VG_(memmove)(path, path + offset, VG_(strlen)(path + offset) + 1);
}


/* Describe a data address range [a,a+len[ as good as you can, for error */
/* messages, putting the result in ai. */
void describe_addr(Addr const a, SizeT const len, AddrInfo* const ai)
{
   Addr       stack_min, stack_max;
   SegInfo*   sg;

   /* Perhaps it's on a thread's stack? */
   ai->stack_tid = thread_lookup_stackaddr(a, &stack_min, &stack_max);
   if (ai->stack_tid != DRD_INVALID_THREADID)
   {
      ai->akind     = eStack;
      ai->size      = len;
      ai->rwoffset  = a - stack_max;
      tl_assert(a + ai->size <= stack_max);
      tl_assert(ai->rwoffset < 0);
      return;
   }

   /* Perhaps it's in a mapped segment ? */
   sg = VG_(find_seginfo)(a);
   if (sg)
   {
      int i, n;

      ai->akind   = eSegment;
      ai->seginfo = sg;
      ai->name[0] = 0;
      ai->size = 1;
      ai->rwoffset = 0;

      n = VG_(seginfo_syms_howmany)(sg);
      for (i = 0; i < n; i++)
      {
         Addr addr;
         Addr tocptr;
         UInt size;
         HChar* name;
         Char filename[256];
         Int linenum;

         VG_(seginfo_syms_getidx)(sg, i, &addr, &tocptr, &size, &name);
         if (addr <= a && a < addr + size)
         {
            ai->size     = size;
            ai->rwoffset = a - addr;
            tl_assert(name && name[0]);
            VG_(snprintf)(ai->name, sizeof(ai->name), "%s", name);
            if (VG_(get_filename_linenum)(addr,
                                          filename, sizeof(filename),
                                          0, 0, 0,
                                          &linenum))
            {
               make_path_relative(filename);
               VG_(snprintf)(ai->descr, sizeof(ai->descr),
                             " in %s:%d", filename, linenum);
            }
            else
            {
               i = n;
            }
            break;
         }
      }
      if (i == n)
      {
         Char filename[512];
         Char soname[512];
         Char sect_kind_name[16];

         VG_(seginfo_sect_kind_name)(a, sect_kind_name,
                                     sizeof(sect_kind_name));
         VG_(strncpy)(filename, VG_(seginfo_filename)(sg), sizeof(filename));
         filename[sizeof(filename) - 1] = 0;
         make_path_relative(filename);
         VG_(strncpy)(soname, VG_(seginfo_soname)(sg), sizeof(soname));
         soname[sizeof(soname) - 1] = 0;
         make_path_relative(soname);
         VG_(snprintf)(ai->descr, sizeof(ai->descr),
                       "%s, %s:%s",
                       filename,
                       soname,
                       sect_kind_name);
      }
      return;
   }

   /* Search for a currently malloc'd block which might bracket it. */
   {
      Addr data;
      if (drd_heap_addrinfo(a, &data, &ai->size, &ai->lastchange))
      {
         ai->akind = eMallocd;
         ai->rwoffset = a - data;
         return;
      }
   }

   /* Clueless ... */
   ai->akind = eUnknown;
   return;
}

/**
 * Generate a description string for the data residing at address a.
 */
Char* describe_addr_text(Addr const a, SizeT const len, AddrInfo* const ai,
                         Char* const buf, UInt const n_buf)
{
   tl_assert(a);
   tl_assert(ai);
   tl_assert(buf);

   describe_addr(a, len, ai);

   switch (ai->akind)
   {
   case eStack: {
      VG_(snprintf)(buf, n_buf,
                    "stack of %s, offset %d",
                    thread_get_name(ai->stack_tid), ai->rwoffset);
      break;
   }
   case eSegment: {
      if (ai->name[0])
      {
         VG_(snprintf)(buf, n_buf,
                       "%s (offset %ld, size %ld) in %s",
                       ai->name, ai->rwoffset, ai->size, ai->descr);
      }
      else
      {
         VG_(snprintf)(buf, n_buf,
                       "%s",
                       ai->descr);
      }
      break;
   }
   case eMallocd: {
      VG_(snprintf)(buf, n_buf, "heap");
      VG_(snprintf)(buf + VG_(strlen)(buf), n_buf - VG_(strlen)(buf),
                    ", offset %ld in block at 0x%lx of size %ld",
                    ai->rwoffset, a - ai->rwoffset, ai->size);
      break;
   }
   case eUnknown:
      VG_(snprintf)(buf, n_buf, "unknown");
      break;
   default:
      tl_assert(0);
   }
   return buf;
}

#ifdef OLD_RACE_DETECTION_ALGORITHM
void drd_report_data_race(const DataRaceInfo* const dri)
{
   AddrInfo ai;
   Char descr[256];

   tl_assert(dri);
   tl_assert(dri->range_begin < dri->range_end);
   describe_addr_text(dri->range_begin, dri->range_end - dri->range_begin,
                      &ai, descr, sizeof(descr));
   VG_(message)(Vg_UserMsg,
                "0x%08lx sz %ld %c %c (%s)",
                dri->range_begin,
                dri->range_end - dri->range_begin,
                dri->range_access & LHS_W ? 'W' : 'R',
                dri->range_access & RHS_W ? 'W' : 'R',
                descr);
   if (ai.akind == eMallocd && ai.lastchange)
   {
      VG_(message)(Vg_UserMsg, "Allocation context:");
      VG_(pp_ExeContext)(ai.lastchange);
   }
   // Note: for stack and heap variables suppression should be
   // stopped automatically as soon as the specified memory
   // range has been freed.
   tl_assert(dri->range_begin < dri->range_end);
   drd_start_suppression(dri->range_begin, dri->range_end, "detected race");
}
#endif

static
void drd_report_data_race2(Error* const err, const DataRaceErrInfo* const dri)
{
   AddrInfo ai;
   Char descr[256];

   tl_assert(dri);
   tl_assert(dri->addr);
   tl_assert(dri->size > 0);
   describe_addr_text(dri->addr, dri->size,
                      &ai, descr, sizeof(descr));
   VG_(message)(Vg_UserMsg,
                "Conflicting %s by %s at 0x%08lx size %ld",
                dri->access_type == eStore ? "store" : "load",
                thread_get_name(VgThreadIdToDrdThreadId(dri->tid)),
                dri->addr,
                dri->size);
   VG_(pp_ExeContext)(VG_(get_error_where)(err));
   VG_(message)(Vg_UserMsg, "Allocation context: %s", descr);
   if (ai.akind == eMallocd && ai.lastchange)
   {
      VG_(pp_ExeContext)(ai.lastchange);
   }
   thread_report_conflicting_segments(VgThreadIdToDrdThreadId(dri->tid),
                                      dri->addr, dri->size, dri->access_type);
}

static Bool drd_tool_error_eq(VgRes res, Error* e1, Error* e2)
{
   return False;
}

static void drd_tool_error_pp(Error* const e)
{
   switch (VG_(get_error_kind)(e))
   {
   case DataRaceErr: {
      drd_report_data_race2(e, VG_(get_error_extra)(e));
      break;
   }
   case MutexErr: {
      MutexErrInfo* p = (MutexErrInfo*)(VG_(get_error_extra)(e));
      VG_(message)(Vg_UserMsg,
                   "%s / mutex 0x%lx (recursion count %d, owner %d)",
                   VG_(get_error_string)(e),
                   p->mutex,
                   p->recursion_count,
                   p->owner);
      VG_(pp_ExeContext)(VG_(get_error_where)(e));
      break;
   }
   case CondRaceErr: {
      CondRaceErrInfo* cei = (CondRaceErrInfo*)(VG_(get_error_extra)(e));
      VG_(message)(Vg_UserMsg,
                   "Race condition: condition variable 0x%lx has been signalled"
                   " but the associated mutex 0x%lx is not locked by the"
                   " signalling thread",
                   cei->cond, cei->mutex);
      VG_(pp_ExeContext)(VG_(get_error_where)(e));
      break;
   }
   case CondErr: {
      CondErrInfo* cdei =(CondErrInfo*)(VG_(get_error_extra)(e));
      VG_(message)(Vg_UserMsg,
                   "cond 0x%lx: %s",
                   cdei->cond,
                   VG_(get_error_string)(e));
      VG_(pp_ExeContext)(VG_(get_error_where)(e));
      break;
   }
   default:
      VG_(message)(Vg_UserMsg,
                   "%s",
                   VG_(get_error_string)(e));
      VG_(pp_ExeContext)(VG_(get_error_where)(e));
      break;
   }
}

static UInt drd_tool_error_update_extra(Error* e)
{
   switch (VG_(get_error_kind)(e))
   {
   case DataRaceErr:
      return sizeof(DataRaceErrInfo);
   case MutexErr:
      return sizeof(MutexErrInfo);
   case CondRaceErr:
      return sizeof(CondRaceErrInfo);
   case CondErr:
      return sizeof(CondErrInfo);
   default:
      tl_assert(False);
      break;
   }
}

static Bool drd_tool_error_recog(Char* const name, Supp* const supp)
{
   SuppKind skind;

   if (VG_(strcmp)(name, "ConflictingAccess") == 0)
      skind = ConflictingAccessSupp;
   else
      return False;

   VG_(set_supp_kind)(supp, skind);
   return True;
}

static Bool drd_tool_error_read_extra(Int fd, Char* buf, Int nBuf, Supp* supp)
{
   return True;
}

static Bool drd_tool_error_matches(Error* const e, Supp* const supp)
{
   switch (VG_(get_supp_kind)(supp))
   {
   }
   return True;
}

static Char* drd_tool_error_name(Error* e)
{
   switch (VG_(get_error_kind)(e))
   {
   case DataRaceErr: return "ConflictingAccess";
   case MutexErr:    return "MutexErr";
   case CondRaceErr: return "CondRaceErr";
   default:
      tl_assert(0);
   }
   return 0;
}

static void drd_tool_error_print_extra(Error* e)
{
   switch (VG_(get_error_kind)(e))
   {
      // VG_(printf)("   %s\n", VG_(get_error_string)(err));
   }
}

void drd_register_error_handlers(void)
{
   // Tool error reporting.
   VG_(needs_tool_errors)(drd_tool_error_eq,
                          drd_tool_error_pp,
                          True,
                          drd_tool_error_update_extra,
                          drd_tool_error_recog,
                          drd_tool_error_read_extra,
                          drd_tool_error_matches,
                          drd_tool_error_name,
                          drd_tool_error_print_extra);
}

/*
 * Local variables:
 * c-basic-offset: 3
 * End:
 */
