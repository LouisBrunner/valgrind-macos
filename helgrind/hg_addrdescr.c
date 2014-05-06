
/*--------------------------------------------------------------------*/
/*--- Address Description.                                         ---*/
/*---                                               hg_addrdescr.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Helgrind, a Valgrind tool for detecting errors
   in threaded programs.

   Copyright (C) 2007-2012 OpenWorks Ltd
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
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307, USA.

   The GNU General Public License is contained in the file COPYING.
*/
#include "pub_tool_basics.h"
#include "pub_tool_libcbase.h"
#include "pub_tool_libcprint.h"
#include "pub_tool_libcassert.h"
#include "pub_tool_xarray.h"
#include "pub_tool_execontext.h"
#include "pub_tool_debuginfo.h"
#include "pub_tool_threadstate.h"

#include "hg_basics.h"
#include "hg_addrdescr.h"            /* self */

void HG_(init_AddrDescr) ( AddrDescr* ad ) {
   VG_(memset)(ad, 0, sizeof(*ad) );
}

void HG_(describe_addr) ( Addr a, /*OUT*/AddrDescr* ad )
{
   tl_assert(!ad->hctxt);
   tl_assert(!ad->descr1);
   tl_assert(!ad->descr2);

   /* First, see if it's in any heap block.  Unfortunately this
      means a linear search through all allocated heap blocks.  The
      assertion says that if it's detected as a heap block, then we
      must have an allocation context for it, since all heap blocks
      should have an allocation context. */
   Bool is_heapblock
      = HG_(mm_find_containing_block)( 
           &ad->hctxt,
           &ad->haddr,
           &ad->hszB,
           a
        );
   tl_assert(is_heapblock == (ad->hctxt != NULL));

   if (!ad->hctxt) {
      /* It's not in any heap block.  See if we can map it to a
         stack or global symbol. */

      ad->descr1
         = VG_(newXA)( HG_(zalloc), "hg.addrdescr.descr1",
                       HG_(free), sizeof(HChar) );
      ad->descr2
         = VG_(newXA)( HG_(zalloc), "hg.addrdescr.descr2",
                       HG_(free), sizeof(HChar) );

      (void) VG_(get_data_description)( ad->descr1,
                                        ad->descr2,
                                        a );

      /* If there's nothing in descr1/2, free it.  Why is it safe to
         to VG_(indexXA) at zero here?  Because
         VG_(get_data_description) guarantees to zero terminate
         descr1/2 regardless of the outcome of the call.  So there's
         always at least one element in each XA after the call.
      */
      if (0 == VG_(strlen)( VG_(indexXA)(ad->descr1, 0 ))) {
         VG_(deleteXA)( ad->descr1 );
         ad->descr1 = NULL;
      }
      if (0 == VG_(strlen)( VG_(indexXA)( ad->descr2, 0 ))) {
         VG_(deleteXA)( ad->descr2 );
         ad->descr2 = NULL;
      }
   }
}

void HG_(pp_addrdescr) (Bool xml, const HChar* what, Addr addr,
                        AddrDescr* ad,
                        void(*print)(const HChar *format, ...))
{
   /* If we have a description of the address in terms of a heap
      block, show it. */
   if (ad->hctxt) {
      SizeT delta = addr - ad->haddr;
      if (xml) {
         (*print)("  <auxwhat>%s %p is %ld bytes inside a block "
                  "of size %ld alloc'd</auxwhat>\n", what,
                  (void*)addr, delta, 
                  ad->hszB);
         VG_(pp_ExeContext)( ad->hctxt );
      } else {
         (*print)("\n");
         (*print)("%s %p is %ld bytes inside a block "
                  "of size %ld alloc'd\n", what,
                  (void*)addr, delta, 
                  ad->hszB);
         VG_(pp_ExeContext)( ad->hctxt );
      }
   }

   /* If we have a better description of the address, show it.
      Note that in XML mode, it will already by nicely wrapped up
      in tags, either <auxwhat> or <xauxwhat>, so we can just emit
      it verbatim. */
   if (xml) {
      if (ad->descr1)
         (*print)( "  %s\n",
                   (HChar*)VG_(indexXA)( ad->descr1, 0 ) );
      if (ad->descr2)
         (*print)( "  %s\n",
                   (HChar*)VG_(indexXA)( ad->descr2, 0 ) );
   } else {
      if (ad->descr1 || ad->descr2)
         (*print)("\n");
      if (ad->descr1)
         (*print)( "%s\n",
                   (HChar*)VG_(indexXA)( ad->descr1, 0 ) );
      if (ad->descr2)
         (*print)( "%s\n",
                   (HChar*)VG_(indexXA)( ad->descr2, 0 ) );
   }
}

static void void_printf(const HChar *format, ...)
{
   va_list vargs;
   va_start(vargs, format);
   VG_(vprintf)(format, vargs);
   va_end(vargs);
}

Bool HG_(get_and_pp_addrdescr) (const HChar* what, Addr addr)
{

   Bool ret;
   AddrDescr glad;

   HG_(init_AddrDescr) (&glad);

   HG_(describe_addr) (addr, &glad);

   HG_(pp_addrdescr) (False /* xml */, what, addr,
                      &glad,
                      void_printf);
   ret = glad.hctxt || glad.descr1 || glad.descr2;

   HG_(clear_addrdesc) (&glad);

   return ret;
}

void HG_(clear_addrdesc) ( AddrDescr* ad)
{
   ad->hctxt = NULL;
   ad->haddr = 0;
   ad->hszB = 0;
   if (ad->descr1 != NULL) {
      VG_(deleteXA)( ad->descr1 );
      ad->descr1 = NULL;
   }
   if (ad->descr2 != NULL) {
      VG_(deleteXA)( ad->descr2 );
      ad->descr2 = NULL;
   }
}

/*--------------------------------------------------------------------*/
/*--- end                                           hg_addrdescr.c ---*/
/*--------------------------------------------------------------------*/
