
/*--------------------------------------------------------------------*/
/*--- Ptrcheck: a pointer-use checker.                             ---*/
/*--- This file checks heap accesses.                              ---*/
/*---                                                     h_main.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Ptrcheck, a Valgrind tool for checking pointer
   use in programs.

   Initial version (Annelid):

   Copyright (C) 2003-2009 Nicholas Nethercote
      njn@valgrind.org

   Valgrind-3.X port:

   Copyright (C) 2008-2009 OpenWorks Ltd
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
#include "pub_tool_mallocfree.h"
#include "pub_tool_execontext.h"
#include "pub_tool_hashtable.h"
#include "pub_tool_tooliface.h"
#include "pub_tool_replacemalloc.h"
#include "pub_tool_options.h"
#include "pub_tool_execontext.h"
#include "pub_tool_aspacemgr.h"    // VG_(am_shadow_malloc)
#include "pub_tool_vki.h"          // VKI_MAX_PAGE_SIZE
#include "pub_tool_machine.h"      // VG_({get,set}_shadow_regs_area) et al
#include "pub_tool_debuginfo.h"    // VG_(get_fnname)
#include "pub_tool_threadstate.h"  // VG_(get_running_tid)
#include "pub_tool_oset.h"
#include "pub_tool_vkiscnums.h"
#include "pub_tool_machine.h"
#include "pub_tool_wordfm.h"
#include "pub_tool_xarray.h"

#include "pc_common.h"

//#include "h_list.h"
#include "h_main.h"

#include "sg_main.h"   // sg_instrument_*, and struct _SGEnv



/*------------------------------------------------------------*/
/*--- Debug/trace options                                  ---*/
/*------------------------------------------------------------*/

static ULong stats__client_mallocs = 0;
static ULong stats__client_frees   = 0;
static ULong stats__segs_allocd    = 0;
static ULong stats__segs_recycled  = 0;


//////////////////////////////////////////////////////////////
//                                                          //
// Segments low level storage                               //
//                                                          //
//////////////////////////////////////////////////////////////

// NONPTR, UNKNOWN, BOTTOM defined in h_main.h since 
// pc_common.c needs to see them, for error processing

// we only start recycling segs when this many exist
#define N_FREED_SEGS (1 * 1000 * 1000)

struct _Seg {
   Addr  addr;
   SizeT szB; /* may be zero */
   ExeContext* ec;  /* where malloc'd or freed */
   /* When 1, indicates block is in use.  Otherwise, used to form a
      linked list of freed blocks, running from oldest freed block to
      the most recently freed block. */
   struct _Seg* nextfree;
};

// Determines if 'a' is before, within, or after seg's range.  Sets 'cmp' to
// -1/0/1 accordingly.  Sets 'n' to the number of bytes before/within/after.
void Seg__cmp(Seg* seg, Addr a, Int* cmp, UWord* n)
{
   if (a < seg->addr) {
      *cmp = -1;
      *n   = seg->addr - a;
   } else if (a < seg->addr + seg->szB && seg->szB > 0) {
      *cmp = 0;
      *n = a - seg->addr;
   } else {
      *cmp = 1;
      *n = a - (seg->addr + seg->szB);
   }
}

/*inline*/ Bool Seg__is_freed(Seg* seg)
{
   if (!is_known_segment(seg))
      return False;
   else
      return seg->nextfree != (Seg*)1;
}

ExeContext* Seg__where(Seg* seg)
{
   tl_assert(is_known_segment(seg));
   return seg->ec;
}

SizeT Seg__size(Seg* seg)
{
   tl_assert(is_known_segment(seg));
   return seg->szB;
}

Addr Seg__addr(Seg* seg)
{
   tl_assert(is_known_segment(seg));
   return seg->addr;
}


#define N_SEGS_PER_GROUP 10000

typedef
   struct _SegGroup {
      struct _SegGroup* admin;
      UWord nextfree; /* 0 .. N_SEGS_PER_GROUP */
      Seg segs[N_SEGS_PER_GROUP];
   }
   SegGroup;

static SegGroup* group_list = NULL;
static UWord     nFreeSegs = 0;
static Seg*      freesegs_youngest = NULL;
static Seg*      freesegs_oldest = NULL;


static SegGroup* new_SegGroup ( void ) {
   SegGroup* g = VG_(malloc)("pc.h_main.nTG.1", sizeof(SegGroup));
   VG_(memset)(g, 0, sizeof(*g));
   return g;
}

/* Get a completely new Seg */
static Seg* new_Seg ( void )
{
   Seg*      teg;
   SegGroup* g;
   if (group_list == NULL) {
      g = new_SegGroup();
      g->admin = NULL;
      group_list = g;
   }
   tl_assert(group_list->nextfree <= N_SEGS_PER_GROUP);
   if (group_list->nextfree == N_SEGS_PER_GROUP) {
      g = new_SegGroup();
      g->admin = group_list;
      group_list = g;
   }
   tl_assert(group_list->nextfree < N_SEGS_PER_GROUP);
   teg = &group_list->segs[ group_list->nextfree ];
   group_list->nextfree++;
   stats__segs_allocd++;
   return teg;
}

static Seg* get_Seg_for_malloc ( void )
{
   Seg* seg;
   if (nFreeSegs < N_FREED_SEGS) {
      seg = new_Seg();
      seg->nextfree = (Seg*)1;
      return seg;
   }
   /* else recycle the oldest Seg in the free list */
   tl_assert(freesegs_youngest);
   tl_assert(freesegs_oldest);
   tl_assert(freesegs_youngest != freesegs_oldest);
   seg = freesegs_oldest;
   freesegs_oldest = seg->nextfree;
   nFreeSegs--;
   seg->nextfree = (Seg*)1;
   stats__segs_recycled++;
   return seg;
}

static void set_Seg_freed ( Seg* seg )
{
   tl_assert(seg);
   tl_assert(!Seg__is_freed(seg));
   if (nFreeSegs == 0) {
      tl_assert(freesegs_oldest == NULL);
      tl_assert(freesegs_youngest == NULL);
      seg->nextfree = NULL;
      freesegs_youngest = seg;
      freesegs_oldest = seg;
      nFreeSegs++;
   } else {
      tl_assert(freesegs_youngest);
      tl_assert(freesegs_oldest);
      if (nFreeSegs == 1) {
         tl_assert(freesegs_youngest == freesegs_oldest);
      } else {
         tl_assert(freesegs_youngest != freesegs_oldest);
      }
      tl_assert(freesegs_youngest->nextfree == NULL);
      tl_assert(seg != freesegs_youngest && seg != freesegs_oldest);
      seg->nextfree = NULL;
      freesegs_youngest->nextfree = seg;
      freesegs_youngest = seg;
      nFreeSegs++;
   }
}

static WordFM* addr_to_seg_map = NULL; /* GuestAddr -> Seg* */

static void addr_to_seg_map_ENSURE_INIT ( void )
{
   if (UNLIKELY(addr_to_seg_map == NULL)) {
      addr_to_seg_map = VG_(newFM)( VG_(malloc), "pc.h_main.attmEI.1",
                                    VG_(free), NULL/*unboxedcmp*/ );
   }
}

static Seg* find_Seg_by_addr ( Addr ga )
{
   UWord keyW, valW;
   addr_to_seg_map_ENSURE_INIT();
   if (VG_(lookupFM)( addr_to_seg_map, &keyW, &valW, (UWord)ga )) {
      tl_assert(keyW == ga);
      return (Seg*)valW;
   } else {
      return NULL;
   }
}

static void bind_addr_to_Seg ( Addr ga, Seg* seg )
{
   Bool b;
   addr_to_seg_map_ENSURE_INIT();
   b = VG_(addToFM)( addr_to_seg_map, (UWord)ga, (UWord)seg );
   tl_assert(!b); /* else ga is already bound */
}

static void unbind_addr_from_Seg ( Addr ga )
{
   Bool b;
   UWord keyW, valW;
   addr_to_seg_map_ENSURE_INIT();
   b = VG_(delFromFM)( addr_to_seg_map, &keyW, &valW, (UWord)ga );
   tl_assert(b); /* else ga was not already bound */
   tl_assert(keyW == ga);
   tl_assert(valW != 0);
}


//////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////

// Returns the added heap segment
static Seg* add_new_segment ( ThreadId tid, Addr p, SizeT size )
{
   Seg* seg = get_Seg_for_malloc();
   tl_assert(seg != (Seg*)1); /* since we're using 1 as a special value */
   seg->addr = p;
   seg->szB  = size;
   seg->ec   = VG_(record_ExeContext)( tid, 0/*first_ip_delta*/ );
   tl_assert(!Seg__is_freed(seg));

   bind_addr_to_Seg(p, seg);

   return seg;
}



static
void* alloc_and_new_mem_heap ( ThreadId tid,
                               SizeT size, SizeT alignment, Bool is_zeroed )
{
   Addr p;

   if ( ((SSizeT)size) < 0) return NULL;

   p = (Addr)VG_(cli_malloc)(alignment, size);
   if (is_zeroed) VG_(memset)((void*)p, 0, size);

   add_new_segment( tid, p, size );

   stats__client_mallocs++;
   return (void*)p;
}

static void die_and_free_mem_heap ( ThreadId tid, Seg* seg )
{
   // Empty and free the actual block
   tl_assert(!Seg__is_freed(seg));

   VG_(cli_free)( (void*)seg->addr );

   // Remember where freed
   seg->ec = VG_(record_ExeContext)( tid, 0/*first_ip_delta*/ );

   set_Seg_freed(seg);
   unbind_addr_from_Seg( seg->addr );

   stats__client_frees++;
}

static void handle_free_heap( ThreadId tid, void* p )
{
   Seg* seg = find_Seg_by_addr( (Addr)p );
   if (!seg) {
      /* freeing a block that wasn't malloc'd.  Ignore. */
      return;
   }
   die_and_free_mem_heap( tid, seg );
}


/*------------------------------------------------------------*/
/*--- malloc() et al replacements                          ---*/
/*------------------------------------------------------------*/

void* h_replace_malloc ( ThreadId tid, SizeT n )
{
   return alloc_and_new_mem_heap ( tid, n, VG_(clo_alignment),
                                        /*is_zeroed*/False );
}

void* h_replace___builtin_new ( ThreadId tid, SizeT n )
{
   return alloc_and_new_mem_heap ( tid, n, VG_(clo_alignment),
                                           /*is_zeroed*/False );
}

void* h_replace___builtin_vec_new ( ThreadId tid, SizeT n )
{
   return alloc_and_new_mem_heap ( tid, n, VG_(clo_alignment),
                                           /*is_zeroed*/False );
}

void* h_replace_memalign ( ThreadId tid, SizeT align, SizeT n )
{
   return alloc_and_new_mem_heap ( tid, n, align,
                                        /*is_zeroed*/False );
}

void* h_replace_calloc ( ThreadId tid, SizeT nmemb, SizeT size1 )
{
   return alloc_and_new_mem_heap ( tid, nmemb*size1, VG_(clo_alignment),
                                        /*is_zeroed*/True );
}

void h_replace_free ( ThreadId tid, void* p )
{
   // Should arguably check here if p.vseg matches the segID of the
   // pointed-to block... unfortunately, by this stage, we don't know what
   // p.vseg is, because we don't know the address of p (the p here is a
   // copy, and we've lost the address of its source).  To do so would
   // require passing &p in, which would require rewriting part of
   // vg_replace_malloc.c... argh.
   //
   // However, Memcheck does free checking, and will catch almost all
   // violations this checking would have caught.  (Would only miss if we
   // unluckily passed an unrelated pointer to the very start of a heap
   // block that was unrelated to that block.  This is very unlikely!)    So
   // we haven't lost much.

   handle_free_heap(tid, p);
}

void h_replace___builtin_delete ( ThreadId tid, void* p )
{
   handle_free_heap(tid, p);
}

void h_replace___builtin_vec_delete ( ThreadId tid, void* p )
{
   handle_free_heap(tid, p);
}

void* h_replace_realloc ( ThreadId tid, void* p_old, SizeT new_size )
{
   Seg* seg;

   /* First try and find the block. */
   seg = find_Seg_by_addr( (Addr)p_old );
   if (!seg)
      return NULL;

   tl_assert(seg->addr == (Addr)p_old);

   if (new_size <= seg->szB) {
      /* new size is smaller: allocate, copy from old to new */
      Addr p_new = (Addr)VG_(cli_malloc)(VG_(clo_alignment), new_size);
      VG_(memcpy)((void*)p_new, p_old, new_size);

      /* Free old memory */
      die_and_free_mem_heap( tid, seg );

      /* This has to be after die_and_free_mem_heap, otherwise the
         former succeeds in shorting out the new block, not the
         old, in the case when both are on the same list.  */
      add_new_segment ( tid, p_new, new_size );

      return (void*)p_new;
   } else {
      /* new size is bigger: allocate, copy from old to new */
      Addr p_new = (Addr)VG_(cli_malloc)(VG_(clo_alignment), new_size);
      VG_(memcpy)((void*)p_new, p_old, seg->szB);

      /* Free old memory */
      die_and_free_mem_heap( tid, seg );

      /* This has to be after die_and_free_mem_heap, otherwise the
         former succeeds in shorting out the new block, not the old,
         in the case when both are on the same list.  NB jrs
         2008-Sept-11: not sure if this comment is valid/correct any
         more -- I suspect not. */
      add_new_segment ( tid, p_new, new_size );

      return (void*)p_new;
   }
}

SizeT h_replace_malloc_usable_size ( ThreadId tid, void* p )
{
   Seg* seg = find_Seg_by_addr( (Addr)p );

   // There may be slop, but pretend there isn't because only the asked-for
   // area will have been shadowed properly.
   return ( seg ? seg->szB : 0 );
}


/*--------------------------------------------------------------------*/
/*--- Instrumentation                                              ---*/
/*--------------------------------------------------------------------*/

/* The h_ instrumenter that follows is complex, since it deals with
   shadow value computation.

   It also needs to generate instrumentation for the sg_ side of
   things.  That's relatively straightforward.  However, rather than
   confuse the code herein any further, we simply delegate the problem
   to sg_main.c, by using the four functions
   sg_instrument_{init,fini,IRStmt,final_jump}.  These four completely
   abstractify the sg_ instrumentation.  See comments in sg_main.c's
   instrumentation section for further details. */


/* Carries info about a particular tmp.  The tmp's number is not
   recorded, as this is implied by (equal to) its index in the tmpMap
   in PCEnv.  The tmp's type is also not recorded, as this is present
   in PCEnv.sb->tyenv.

   When .kind is NonShad, .shadow may give the identity of the temp
   currently holding the associated shadow value, or it may be
   IRTemp_INVALID if code to compute the shadow has not yet been
   emitted.

   When .kind is Shad tmp holds a shadow value, and so .shadow must be
   IRTemp_INVALID, since it is illogical for a shadow tmp itself to be
   shadowed.
*/
typedef
   enum { NonShad=1, Shad=2 }
   TempKind;

typedef
   struct {
      TempKind kind;
      IRTemp   shadow;
   }
   TempMapEnt;



/* Carries around state during Ptrcheck instrumentation. */
typedef
   struct {
      /* MODIFIED: the superblock being constructed.  IRStmts are
         added. */
      IRSB* sb;
      Bool  trace;

      /* MODIFIED: a table [0 .. #temps_in_sb-1] which gives the
         current kind and possibly shadow temps for each temp in the
         IRSB being constructed.  Note that it does not contain the
         type of each tmp.  If you want to know the type, look at the
         relevant entry in sb->tyenv.  It follows that at all times
         during the instrumentation process, the valid indices for
         tmpMap and sb->tyenv are identical, being 0 .. N-1 where N is
         total number of NonShad and Shad temps allocated so far.

         The reason for this strange split (types in one place, all
         other info in another) is that we need the types to be
         attached to sb so as to make it possible to do
         "typeOfIRExpr(mce->bb->tyenv, ...)" at various places in the
         instrumentation process.

         Note that only integer temps of the guest word size are
         shadowed, since it is impossible (or meaningless) to hold a
         pointer in any other type of temp. */
      XArray* /* of TempMapEnt */ qmpMap;

      /* READONLY: the host word type.  Needed for constructing
         arguments of type 'HWord' to be passed to helper functions.
         Ity_I32 or Ity_I64 only. */
      IRType hWordTy;

      /* READONLY: the guest word type, Ity_I32 or Ity_I64 only. */
      IRType gWordTy;

      /* READONLY: the guest state size, so we can generate shadow
         offsets correctly. */
      Int guest_state_sizeB;
   }
   PCEnv;

/* SHADOW TMP MANAGEMENT.  Shadow tmps are allocated lazily (on
   demand), as they are encountered.  This is for two reasons.

   (1) (less important reason): Many original tmps are unused due to
   initial IR optimisation, and we do not want to spaces in tables
   tracking them.

   Shadow IRTemps are therefore allocated on demand.  pce.tmpMap is a
   table indexed [0 .. n_types-1], which gives the current shadow for
   each original tmp, or INVALID_IRTEMP if none is so far assigned.
   It is necessary to support making multiple assignments to a shadow
   -- specifically, after testing a shadow for definedness, it needs
   to be made defined.  But IR's SSA property disallows this.

   (2) (more important reason): Therefore, when a shadow needs to get
   a new value, a new temporary is created, the value is assigned to
   that, and the tmpMap is updated to reflect the new binding.

   A corollary is that if the tmpMap maps a given tmp to
   IRTemp_INVALID and we are hoping to read that shadow tmp, it means
   there's a read-before-write error in the original tmps.  The IR
   sanity checker should catch all such anomalies, however.
*/

/* Create a new IRTemp of type 'ty' and kind 'kind', and add it to
   both the table in pce->sb and to our auxiliary mapping.  Note that
   newTemp may cause pce->tmpMap to resize, hence previous results
   from VG_(indexXA)(pce->tmpMap) are invalidated. */
static IRTemp newTemp ( PCEnv* pce, IRType ty, TempKind kind )
{
   Word       newIx;
   TempMapEnt ent;
   IRTemp     tmp = newIRTemp(pce->sb->tyenv, ty);
   ent.kind   = kind;
   ent.shadow = IRTemp_INVALID;
   newIx = VG_(addToXA)( pce->qmpMap, &ent );
   tl_assert(newIx == (Word)tmp);
   return tmp;
}

/*------------------------------------------------------------*/
/*--- Constructing IR fragments                            ---*/
/*------------------------------------------------------------*/

/* add stmt to a bb */
static /*inline*/ void stmt ( HChar cat, PCEnv* pce, IRStmt* st ) {
   if (pce->trace) {
      VG_(printf)("  %c: ", cat);
      ppIRStmt(st);
      VG_(printf)("\n");
   }
   addStmtToIRSB(pce->sb, st);
}

static IRTemp for_sg__newIRTemp_cb ( IRType ty, void* opaque )
{
   PCEnv* pce = (PCEnv*)opaque;
   return newTemp( pce, ty, NonShad );
}


IRSB* h_instrument ( VgCallbackClosure* closure,
                     IRSB* sbIn,
                     VexGuestLayout* layout,
                     VexGuestExtents* vge,
                     IRType gWordTy, IRType hWordTy )
{
   Bool  verboze = 0||False;
   Int   i /*, j*/;
   PCEnv pce;
   struct _SGEnv* sgenv;

   if (gWordTy != hWordTy) {
      /* We don't currently support this case. */
      VG_(tool_panic)("host/guest word size mismatch");
   }

   /* Check we're not completely nuts */
   tl_assert(sizeof(UWord)  == sizeof(void*));
   tl_assert(sizeof(Word)   == sizeof(void*));
   tl_assert(sizeof(Addr)   == sizeof(void*));
   tl_assert(sizeof(ULong)  == 8);
   tl_assert(sizeof(Long)   == 8);
   tl_assert(sizeof(Addr64) == 8);
   tl_assert(sizeof(UInt)   == 4);
   tl_assert(sizeof(Int)    == 4);

   /* Set up the running environment.  Both .sb and .tmpMap are
      modified as we go along.  Note that tmps are added to both
      .sb->tyenv and .tmpMap together, so the valid index-set for
      those two arrays should always be identical. */
   VG_(memset)(&pce, 0, sizeof(pce));
   pce.sb                = deepCopyIRSBExceptStmts(sbIn);
   pce.trace             = verboze;
   pce.hWordTy           = hWordTy;
   pce.gWordTy           = gWordTy;
   pce.guest_state_sizeB = layout->total_sizeB;

   pce.qmpMap = VG_(newXA)( VG_(malloc), "pc.h_instrument.1", VG_(free),
                            sizeof(TempMapEnt));
   for (i = 0; i < sbIn->tyenv->types_used; i++) {
      TempMapEnt ent;
      ent.kind   = NonShad;
      ent.shadow = IRTemp_INVALID;
      VG_(addToXA)( pce.qmpMap, &ent );
   }
   tl_assert( VG_(sizeXA)( pce.qmpMap ) == sbIn->tyenv->types_used );

   /* Also set up for the sg_ instrumenter.  See comments at the top
      of this instrumentation section for details.  The two parameters
      constitute a closure, which sg_ can use to correctly generate
      new IRTemps as needed. */
   sgenv = sg_instrument_init( for_sg__newIRTemp_cb,
                               (void*)&pce );

   /* Copy verbatim any IR preamble preceding the first IMark */

   i = 0;
   while (i < sbIn->stmts_used && sbIn->stmts[i]->tag != Ist_IMark) {
      IRStmt* st = sbIn->stmts[i];
      tl_assert(st);
      tl_assert(isFlatIRStmt(st));
      stmt( 'C', &pce, sbIn->stmts[i] );
      i++;
   }

   /* Iterate over the remaining stmts to generate instrumentation. */

   tl_assert(sbIn->stmts_used > 0);
   tl_assert(i >= 0);
   tl_assert(i < sbIn->stmts_used);
   tl_assert(sbIn->stmts[i]->tag == Ist_IMark);

   for (/*use current i*/; i < sbIn->stmts_used; i++) {
      /* generate sg_ instrumentation for this stmt */
      sg_instrument_IRStmt( sgenv, pce.sb, sbIn->stmts[i],
                            layout, gWordTy, hWordTy );

      stmt( 'C', &pce, sbIn->stmts[i] );
   }

   /* generate sg_ instrumentation for the final jump */
   sg_instrument_final_jump( sgenv, pce.sb, sbIn->next, sbIn->jumpkind,
                             layout, gWordTy, hWordTy );

   /* and finalise .. */
   sg_instrument_fini( sgenv );

   /* If this fails, there's been some serious snafu with tmp management,
      that should be investigated. */
   tl_assert( VG_(sizeXA)( pce.qmpMap ) == pce.sb->tyenv->types_used );
   VG_(deleteXA)( pce.qmpMap );

   return pce.sb;
}


/*--------------------------------------------------------------------*/
/*--- Finalisation                                                 ---*/
/*--------------------------------------------------------------------*/

void h_fini ( Int exitcode )
{
   if (VG_(clo_verbosity) == 1 && !VG_(clo_xml)) {
      VG_(message)(Vg_UserMsg, 
                   "For counts of detected and suppressed errors, "
                   "rerun with: -v\n");
   }

   if (VG_(clo_stats)) {
      VG_(message)(Vg_DebugMsg,
                   "  h_:  %'10llu client allocs, %'10llu client frees\n", 
                   stats__client_mallocs, stats__client_frees);
      VG_(message)(Vg_DebugMsg,
                   "  h_:  %'10llu Segs allocd,   %'10llu Segs recycled\n", 
                   stats__segs_allocd, stats__segs_recycled);
   }
}


/*--------------------------------------------------------------------*/
/*--- end                                                 h_main.c ---*/
/*--------------------------------------------------------------------*/
