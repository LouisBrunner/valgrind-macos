
/*--------------------------------------------------------------------*/
/*--- DMD: A dark matter detector.                      dmd_main.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Nulgrind, the minimal Valgrind tool,
   which does no instrumentation or analysis.

   Copyright (C) 2002-2010 Nicholas Nethercote
      njn@valgrind.org

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
#include "pub_tool_hashtable.h"
#include "pub_tool_libcbase.h"
#include "pub_tool_libcassert.h"
#include "pub_tool_libcprint.h"
#include "pub_tool_mallocfree.h"
#include "pub_tool_replacemalloc.h"
#include "pub_tool_tooliface.h"

//------------------------------------------------------------//
//--- Heap management                                      ---//
//------------------------------------------------------------//

// Nb: first two fields must match core's VgHashNode.
typedef
   struct _HeapBlock {
      struct _HeapBlock* next;
      Addr               data;      // Ptr to actual block
      SizeT              reqSzB;    // Size requested
      SizeT              slopSzB;   // Extra bytes given above those requested
      ExeContext*        where;     // Where allocated; bottom-XPt
   }
   HeapBlock;

static VgHashTable heapBlocks = NULL;   // HeapBlocks

static __inline__
void* allocAndRecordBlock(ThreadId tid, SizeT reqSzB, SizeT reqAlignB,
                          Bool isZeroed)
{
   SizeT actualSzB, slopSzB;
   void* p;

   if ((SSizeT)reqSzB < 0)
      return NULL;

   // Allocate and zero if necessary.
   p = VG_(cli_malloc)(reqAlignB, reqSzB);
   if (!p) {
      return NULL;
   }
   if (isZeroed) {
      VG_(memset)(p, 0, reqSzB);
   }
   actualSzB = VG_(malloc_usable_size)(p);
   tl_assert(actualSzB >= reqSzB);
   slopSzB = actualSzB - reqSzB;

   VG_(printf)("allocAndRecordBlock: %ld + %ld\n", reqSzB, slopSzB);

   // Record block.
   HeapBlock* hb = VG_(malloc)("dmd.main.rb.1", sizeof(HeapBlock));
   hb->reqSzB  = reqSzB;
   hb->slopSzB = slopSzB;
   hb->data    = (Addr)p;
   hb->where   = VG_(record_ExeContext)(tid, 0/*first_ip_delta*/);
   VG_(HT_add_node)(heapBlocks, hb);

   return p;
}

static __inline__
void unrecordAndFreeBlock(void* p)
{
   HeapBlock* hb = VG_(HT_remove)(heapBlocks, (UWord)p);
   if (NULL == hb) {
      return;   // must have been a bogus free()
   }
   VG_(free)(hb);  hb = NULL;
   VG_(cli_free)(p);

   VG_(printf)("unrecordAndFreeBlock\n");
}

static __inline__
void* reallocAndRecordBlock(ThreadId tid, void* pOld, SizeT newReqSzB)
{
   HeapBlock* hb;
   void*     pNew;
   SizeT     oldReqSzB, oldSlopSzB, newSlopSzB, newActualSzB;

   // Remove the old block
   hb = VG_(HT_remove)(heapBlocks, (UWord)pOld);
   if (hb == NULL) {
      return NULL;   // must have been a bogus realloc()
   }

   oldReqSzB  = hb->reqSzB;
   oldSlopSzB = hb->slopSzB;

   VG_(printf)("reallocAndRecordBlock: %ld\n", newReqSzB);

   if (newReqSzB <= oldReqSzB + oldSlopSzB) {
      // New size is smaller or same;  block not moved.
      pNew = pOld;
      newSlopSzB = oldSlopSzB + (oldReqSzB - newReqSzB);

   } else {
      // New size is bigger;  make new block, copy shared contents, free old.
      pNew = VG_(cli_malloc)(VG_(clo_alignment), newReqSzB);
      if (!pNew) {
         // Nb: if realloc fails, NULL is returned but the old block is not
         // touched.  What an awful function.
         return NULL;
      }
      // XXX: need the oldSlopSzB here, so does Massif!  test
      VG_(memcpy)(pNew, pOld, oldReqSzB /*+ oldSlopSzB*/);
      VG_(cli_free)(pOld);
      newActualSzB = VG_(malloc_usable_size)(pNew);
      tl_assert(newActualSzB >= newReqSzB);
      newSlopSzB = newActualSzB - newReqSzB;
   }

   if (pNew) {
      // Update HeapBlock.
      hb->data    = (Addr)pNew;
      hb->reqSzB  = newReqSzB;
      hb->slopSzB = newSlopSzB;
      hb->where   = VG_(record_ExeContext)(tid, 0/*first_ip_delta*/);
   }

   // Now insert the new hb (with a possibly new 'data' field) into
   // heapBlocks.  If this realloc() did not increase the memory size, we
   // will have removed and then re-added hb unnecessarily.  But that's ok
   // because shrinking a block with realloc() is (presumably) much rarer
   // than growing it, and this way simplifies the growing case.
   VG_(HT_add_node)(heapBlocks, hb);

   return pNew;
}


//------------------------------------------------------------//
//--- malloc() et al replacement wrappers                  ---//
//------------------------------------------------------------//

static void* dmd_malloc(ThreadId tid, SizeT szB)
{
   return allocAndRecordBlock(tid, szB, VG_(clo_alignment), /*isZeroed*/False);
}

static void* dmd___builtin_new(ThreadId tid, SizeT szB)
{
   return allocAndRecordBlock(tid, szB, VG_(clo_alignment), /*isZeroed*/False);
}

static void* dmd___builtin_vec_new(ThreadId tid, SizeT szB)
{
   return allocAndRecordBlock(tid, szB, VG_(clo_alignment), /*isZeroed*/False);
}

static void* dmd_calloc(ThreadId tid, SizeT m, SizeT szB)
{
   return allocAndRecordBlock(tid, m*szB, VG_(clo_alignment), /*isZeroed*/True);
}

static void *dmd_memalign(ThreadId tid, SizeT alignB, SizeT szB)
{
   return allocAndRecordBlock(tid, szB, alignB, /*isZeroed*/False);
}

static void dmd_free(ThreadId tid __attribute__((unused)), void* p)
{
   unrecordAndFreeBlock(p);
}

static void dmd___builtin_delete(ThreadId tid, void* p)
{
   unrecordAndFreeBlock(p);
}

static void dmd___builtin_vec_delete(ThreadId tid, void* p)
{
   unrecordAndFreeBlock(p);
}

static void* dmd_realloc(ThreadId tid, void* pOld, SizeT new_szB)
{
   return reallocAndRecordBlock(tid, pOld, new_szB);
}

static SizeT dmd_malloc_usable_size(ThreadId tid, void* p)
{                                                            
   HeapBlock* hb = VG_(HT_lookup)(heapBlocks, (UWord)p);
   return hb ? hb->reqSzB + hb->slopSzB : 0;
}                                                            

//------------------------------------------------------------//
//--- Basic functions                                      ---//
//------------------------------------------------------------//

static void dmd_post_clo_init(void)
{
}

static
IRSB* dmd_instrument(VgCallbackClosure* closure,
                     IRSB* bb,
                     VexGuestLayout* layout, 
                     VexGuestExtents* vge,
                     IRType gWordTy, IRType hWordTy)
{
    return bb;
}

static void dmd_fini(Int exitcode)
{
}

static void dmd_pre_clo_init(void)
{
   VG_(details_name)            ("DMD");
   VG_(details_version)         (NULL);
   VG_(details_description)     ("a dark matter detector");
   VG_(details_copyright_author)(
      "Copyright (C) 2011-2011, and GNU GPL'd, by Nicholas Nethercote.");
   VG_(details_bug_reports_to)  (VG_BUGS_TO);

   VG_(details_avg_translation_sizeB)(275);

   VG_(basic_tool_funcs)        (dmd_post_clo_init,
                                 dmd_instrument,
                                 dmd_fini);

   // Needs.
   VG_(needs_malloc_replacement)(dmd_malloc,
                                 dmd___builtin_new,
                                 dmd___builtin_vec_new,
                                 dmd_memalign,
                                 dmd_calloc,
                                 dmd_free,
                                 dmd___builtin_delete,
                                 dmd___builtin_vec_delete,
                                 dmd_realloc,
                                 dmd_malloc_usable_size,
                                 0);

   heapBlocks = VG_(HT_construct)("DMD's heapBlocks");
}

VG_DETERMINE_INTERFACE_VERSION(dmd_pre_clo_init)

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
