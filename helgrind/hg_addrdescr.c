
/*--------------------------------------------------------------------*/
/*--- Address Description.                                         ---*/
/*---                                               hg_addrdescr.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Helgrind, a Valgrind tool for detecting errors
   in threaded programs.

   Copyright (C) 2007-2017 OpenWorks Ltd
      info@open-works.co.uk

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
#include "pub_tool_basics.h"
#include "pub_tool_libcbase.h"
#include "pub_tool_libcprint.h"
#include "pub_tool_libcassert.h"
#include "pub_tool_wordfm.h"
#include "pub_tool_xarray.h"
#include "pub_tool_execontext.h"
#include "pub_tool_debuginfo.h"
#include "pub_tool_threadstate.h"
#include "pub_tool_aspacemgr.h"
#include "pub_tool_addrinfo.h"

#include "hg_basics.h"
#include "hg_wordset.h"
#include "hg_lock_n_thread.h"
#include "hg_addrdescr.h"            /* self */

void HG_(describe_addr) ( DiEpoch ep, Addr a, /*OUT*/AddrInfo* ai )
{
   tl_assert(ai->tag == Addr_Undescribed);

   /* hctxt/tnr/haddr/hszB describe the addr if it is a heap block. */
   ExeContext* hctxt;
   UInt        tnr;
   Addr        haddr;
   SizeT       hszB;

   /* First, see if it's in any heap block.  Unfortunately this
      means a linear search through all allocated heap blocks.  The
      assertion says that if it's detected as a heap block, then we
      must have an allocation context for it, since all heap blocks
      should have an allocation context. */
   Bool is_heapblock
      = HG_(mm_find_containing_block)( 
           &hctxt,
           &tnr,
           &haddr,
           &hszB,
           a
        );
   if (is_heapblock) {
      tl_assert(is_heapblock == (hctxt != NULL));
      ai->tag = Addr_Block;
      ai->Addr.Block.block_kind = Block_Mallocd;
      ai->Addr.Block.block_desc = "block";
      ai->Addr.Block.block_szB  = hszB;
      ai->Addr.Block.rwoffset   = (Word)(a) - (Word)(haddr);
      ai->Addr.Block.allocated_at = hctxt;
      VG_(initThreadInfo) (&ai->Addr.Block.alloc_tinfo);
      ai->Addr.Block.alloc_tinfo.tnr = tnr;
      ai->Addr.Block.freed_at = VG_(null_ExeContext)();;
   } else {
      /* No block found. Search a non-heap block description. */
      VG_(describe_addr) (ep, a, ai);

      /* In case ai contains a tid, set tnr to the corresponding helgrind
         thread number. */
      if (ai->tag == Addr_Stack) {
         Thread* thr = get_admin_threads();

         tl_assert(ai->Addr.Stack.tinfo.tid);
         while (thr) {
            if (thr->coretid == ai->Addr.Stack.tinfo.tid) {
               ai->Addr.Stack.tinfo.tnr = thr->errmsg_index;
               break;
            }
            thr = thr->admin;
         }
      }
   }
}

Bool HG_(get_and_pp_addrdescr) (DiEpoch ep, Addr addr)
{

   Bool ret;
   AddrInfo glai;

   glai.tag = Addr_Undescribed;
   HG_(describe_addr) (ep, addr, &glai);
   VG_(pp_addrinfo) (addr, &glai);
   ret = glai.tag != Addr_Unknown;

   VG_(clear_addrinfo) (&glai);

   return ret;
}

/*--------------------------------------------------------------------*/
/*--- end                                           hg_addrdescr.c ---*/
/*--------------------------------------------------------------------*/
