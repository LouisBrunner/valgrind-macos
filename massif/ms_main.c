
/*--------------------------------------------------------------------*/
/*--- Massif: a heap profiling tool.                     ms_main.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Massif, a Valgrind tool for profiling memory
   usage of programs.

   Copyright (C) 2003-2004 Nicholas Nethercote
      njn25@cam.ac.uk

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

// Memory profiler.  Produces a graph, gives lots of information about
// allocation contexts, in terms of space.time values (ie. area under the
// graph).  Allocation context information is hierarchical, and can thus
// be inspected step-wise to an appropriate depth.  See comments on data
// structures below for more info on how things work.

#include "tool.h"
//#include "vg_profile.c"

#include "valgrind.h"           // For {MALLOC,FREE}LIKE_BLOCK

/*------------------------------------------------------------*/
/*--- Overview of operation                                ---*/
/*------------------------------------------------------------*/

// Heap blocks are tracked, and the amount of space allocated by various
// contexts (ie. lines of code, more or less) is also tracked.
// Periodically, a census is taken, and the amount of space used, at that
// point, by the most significant (highly allocating) contexts is recorded.
// Census start off frequently, but are scaled back as the program goes on,
// so that there are always a good number of them.  At the end, overall
// spacetimes for different contexts (of differing levels of precision) is
// calculated, the graph is printed, and the text giving spacetimes for the
// increasingly precise contexts is given.
//
// Measures the following:
// - heap blocks
// - heap admin bytes
// - stack(s)
// - code (code segments loaded at startup, and loaded with mmap)
// - data (data segments loaded at startup, and loaded/created with mmap,
//         and brk()d segments)

/*------------------------------------------------------------*/
/*--- Main types                                           ---*/
/*------------------------------------------------------------*/

// An XPt represents an "execution point", ie. a code address.  Each XPt is
// part of a tree of XPts (an "execution tree", or "XTree").  Each
// top-to-bottom path through an XTree gives an execution context ("XCon"),
// and is equivalent to a traditional Valgrind ExeContext.  
//
// The XPt at the top of an XTree (but below "alloc_xpt") is called a
// "top-XPt".  The XPts are the bottom of an XTree (leaf nodes) are
// "bottom-XPTs".  The number of XCons in an XTree is equal to the number of
// bottom-XPTs in that XTree.
//
// All XCons have the same top-XPt, "alloc_xpt", which represents all
// allocation functions like malloc().  It's a bit of a fake XPt, though,
// and is only used because it makes some of the code simpler.
//
// XTrees are bi-directional.
//
//     > parent <       Example: if child1() calls parent() and child2()
//    /    |     \      also calls parent(), and parent() calls malloc(),
//   |    / \     |     the XTree will look like this.
//   |   v   v    |
//  child1   child2

typedef struct _XPt XPt;

struct _XPt {
   Addr  eip;              // code address

   // Bottom-XPts: space for the precise context.
   // Other XPts:  space of all the descendent bottom-XPts.
   // Nb: this value goes up and down as the program executes.
   UInt  curr_space;

   // An approximate space.time calculation used along the way for selecting
   // which contexts to include at each census point.
   // !!! top-XPTs only !!!
   ULong approx_ST;

   // exact_ST_dbld is an exact space.time calculation done at the end, and
   // used in the results.
   // Note that it is *doubled*, to avoid rounding errors.
   // !!! not used for 'alloc_xpt' !!!
   ULong exact_ST_dbld;

   // n_children and max_children are integers;  a very big program might
   // have more than 65536 allocation points (Konqueror startup has 1800).
   XPt*  parent;           // pointer to parent XPt
   UInt  n_children;       // number of children
   UInt  max_children;     // capacity of children array
   XPt** children;         // pointers to children XPts
};

// Each census snapshots the most significant XTrees, each XTree having a
// top-XPt as its root.  The 'curr_space' element for each XPt is recorded 
// in the snapshot.  The snapshot contains all the XTree's XPts, not in a
// tree structure, but flattened into an array.  This flat snapshot is used
// at the end for computing exact_ST_dbld for each XPt.
//
// Graph resolution, x-axis: no point having more than about 200 census
// x-points;  you can't see them on the graph.  Therefore:
//
//   - do a census every 1 ms for first 200 --> 200, all          (200 ms)
//   - halve (drop half of them)            --> 100, every 2nd    (200 ms)
//   - do a census every 2 ms for next 200  --> 200, every 2nd    (400 ms)
//   - halve                                --> 100, every 4th    (400 ms)
//   - do a census every 4 ms for next 400  --> 200, every 4th    (800 ms)
//   - etc.
//
// This isn't exactly right, because we actually drop (N/2)-1 when halving,
// but it shows the basic idea.

#define MAX_N_CENSI           200  // Keep it even, for simplicity

// Graph resolution, y-axis: hp2ps only draws the 19 biggest (in space-time)
// bands, rest get lumped into OTHERS.  I only print the top N
// (cumulative-so-far space-time) at each point.  N should be a bit bigger
// than 19 in case the cumulative space-time doesn't fit with the eventual
// space-time computed by hp2ps (but it should be close if the samples are
// evenly spread, since hp2ps does an approximate per-band space-time
// calculation that just sums the totals;  ie. it assumes all samples are
// the same distance apart).

#define MAX_SNAPSHOTS         32

typedef
   struct {
      XPt* xpt;
      UInt space;
   }
   XPtSnapshot;

// An XTree snapshot is stored as an array of of XPt snapshots.
typedef XPtSnapshot* XTreeSnapshot;

typedef
   struct {
      Int           ms_time;     // Int: must allow -1
      XTreeSnapshot xtree_snapshots[MAX_SNAPSHOTS+1]; // +1 for zero-termination
      UInt          others_space;
      UInt          heap_admin_space;
      UInt          stacks_space;
   } 
   Census;

// Metadata for heap blocks.  Each one contains a pointer to a bottom-XPt,
// which is a foothold into the XCon at which it was allocated.  From
// HP_Chunks, XPt 'space' fields are incremented (at allocation) and
// decremented (at deallocation).
//
// Nb: first two fields must match core's VgHashNode.
typedef
   struct _HP_Chunk {
      struct _HP_Chunk* next;
      Addr              data;    // Ptr to actual block
      SizeT             size;    // Size requested
      XPt*              where;   // Where allocated; bottom-XPt
   }
   HP_Chunk;

/*------------------------------------------------------------*/
/*--- Profiling events                                     ---*/
/*------------------------------------------------------------*/

typedef 
   enum {
      VgpGetXPt = VgpFini+1,
      VgpGetXPtSearch,
      VgpCensus,
      VgpCensusHeap,
      VgpCensusSnapshot,
      VgpCensusTreeSize,
      VgpUpdateXCon,
      VgpCalcSpacetime2,
      VgpPrintHp,
      VgpPrintXPts,
   }
   VgpToolCC;

/*------------------------------------------------------------*/
/*--- Statistics                                           ---*/
/*------------------------------------------------------------*/

// Konqueror startup, to give an idea of the numbers involved with a biggish
// program, with default depth:
//
//  depth=3                   depth=40
//  - 310,000 allocations
//  - 300,000 frees
//  -  15,000 XPts            800,000 XPts
//  -   1,800 top-XPts

static UInt n_xpts               = 0;
static UInt n_bot_xpts           = 0;
static UInt n_allocs             = 0;
static UInt n_zero_allocs        = 0;
static UInt n_frees              = 0;
static UInt n_children_reallocs  = 0;
static UInt n_snapshot_frees     = 0;

static UInt n_halvings           = 0;
static UInt n_real_censi         = 0;
static UInt n_fake_censi         = 0;
static UInt n_attempted_censi    = 0;

/*------------------------------------------------------------*/
/*--- Globals                                              ---*/
/*------------------------------------------------------------*/

#define FILENAME_LEN    256

#define SPRINTF(zz_buf, fmt, args...) \
   do { Int len = VG_(sprintf)(zz_buf, fmt, ## args); \
        VG_(write)(fd, (void*)zz_buf, len); \
   } while (0)

#define BUF_LEN         1024     // general purpose
static Char buf [BUF_LEN];
static Char buf2[BUF_LEN];
static Char buf3[BUF_LEN];

static SizeT sigstacks_space = 0;    // Current signal stacks space sum

static VgHashTable malloc_list  = NULL;   // HP_Chunks

static UInt n_heap_blocks = 0;


#define MAX_ALLOC_FNS      32      // includes the builtin ones

// First few filled in, rest should be zeroed.  Zero-terminated vector.
static UInt  n_alloc_fns = 11;
static Char* alloc_fns[MAX_ALLOC_FNS] = { 
   "malloc",
   "operator new(unsigned)",
   "operator new[](unsigned)",
   "operator new(unsigned, std::nothrow_t const&)",
   "operator new[](unsigned, std::nothrow_t const&)",
   "__builtin_new",
   "__builtin_vec_new",
   "calloc",
   "realloc",
   "my_malloc",   // from vg_libpthread.c
   "memalign",
};


/*------------------------------------------------------------*/
/*--- Command line args                                    ---*/
/*------------------------------------------------------------*/

#define MAX_DEPTH       50

typedef
   enum {
      XText, XHTML,
   }
   XFormat;

static Bool clo_heap        = True;
static UInt clo_heap_admin  = 8;
static Bool clo_stacks      = True;
static Bool clo_depth       = 3;
static XFormat clo_format   = XText;

Bool TL_(process_cmd_line_option)(Char* arg)
{
        VG_BOOL_CLO("--heap",       clo_heap)
   else VG_BOOL_CLO("--stacks",     clo_stacks)

   else VG_NUM_CLO ("--heap-admin",  clo_heap_admin)
   else VG_BNUM_CLO("--depth",       clo_depth, 1, MAX_DEPTH)

   else if (VG_CLO_STREQN(11, arg, "--alloc-fn=")) {
      alloc_fns[n_alloc_fns] = & arg[11];
      n_alloc_fns++;
      if (n_alloc_fns >= MAX_ALLOC_FNS) {
         VG_(printf)("Too many alloc functions specified, sorry");
         VG_(bad_option)(arg);
      }
   }

   else if (VG_CLO_STREQ(arg, "--format=text"))
      clo_format = XText;
   else if (VG_CLO_STREQ(arg, "--format=html"))
      clo_format = XHTML;

   else
      return VG_(replacement_malloc_process_cmd_line_option)(arg);

   return True;
}

void TL_(print_usage)(void)
{
   VG_(printf)( 
"    --heap=no|yes             profile heap blocks [yes]\n"
"    --heap-admin=<number>     average admin bytes per heap block [8]\n"
"    --stacks=no|yes           profile stack(s) [yes]\n"
"    --depth=<number>          depth of contexts [3]\n"
"    --alloc-fn=<name>         specify <fn> as an alloc function [empty]\n"
"    --format=text|html        format of textual output [text]\n"
   );
   VG_(replacement_malloc_print_usage)();
}

void TL_(print_debug_usage)(void)
{
   VG_(replacement_malloc_print_debug_usage)();
}

/*------------------------------------------------------------*/
/*--- Execution contexts                                   ---*/
/*------------------------------------------------------------*/

// Fake XPt representing all allocation functions like malloc().  Acts as
// parent node to all top-XPts.
static XPt* alloc_xpt;

// Cheap allocation for blocks that never need to be freed.  Saves about 10%
// for Konqueror startup with --depth=40.
static void* perm_malloc(SizeT n_bytes)
{
   static Addr hp     = 0;    // current heap pointer
   static Addr hp_lim = 0;    // maximum usable byte in current block

   #define SUPERBLOCK_SIZE  (1 << 20)         // 1 MB

   if (hp + n_bytes > hp_lim) {
      hp     = (Addr)VG_(get_memory_from_mmap)(SUPERBLOCK_SIZE, "perm_malloc");
      hp_lim = hp + SUPERBLOCK_SIZE - 1;
   }

   hp += n_bytes;

   return (void*)(hp - n_bytes);
}



static XPt* new_XPt(Addr eip, XPt* parent, Bool is_bottom)
{
   XPt* xpt          = perm_malloc(sizeof(XPt));
   xpt->eip          = eip;

   xpt->curr_space    = 0;
   xpt->approx_ST     = 0;
   xpt->exact_ST_dbld = 0;

   xpt->parent       = parent;

   // Check parent is not a bottom-XPt
   tl_assert(parent == NULL || 0 != parent->max_children);

   xpt->n_children   = 0;

   // If a bottom-XPt, don't allocate space for children.  This can be 50%
   // or more, although it tends to drop as --depth increases (eg. 10% for
   // konqueror with --depth=20).
   if ( is_bottom ) {
      xpt->max_children = 0;
      xpt->children     = NULL;
      n_bot_xpts++;
   } else {
      xpt->max_children = 4;
      xpt->children     = VG_(malloc)( xpt->max_children * sizeof(XPt*) );
   }

   // Update statistics
   n_xpts++;

   return xpt;
}

static Bool is_alloc_fn(Addr eip)
{
   Int i;

   if ( VG_(get_fnname)(eip, buf, BUF_LEN) ) {
      for (i = 0; i < n_alloc_fns; i++) {
         if (VG_STREQ(buf, alloc_fns[i]))
            return True;
      }
   }
   return False;
}

// Returns an XCon, from the bottom-XPt.  Nb: the XPt returned must be a
// bottom-XPt now and must always remain a bottom-XPt.  We go to some effort
// to ensure this in certain cases.  See comments below.
static XPt* get_XCon( ThreadId tid, Bool custom_malloc )
{
   // Static to minimise stack size.  +1 for added ~0 %eip.
   static Addr eips[MAX_DEPTH + MAX_ALLOC_FNS + 1];

   XPt* xpt = alloc_xpt;
   UInt n_eips, L, A, B, nC;
   UInt overestimate;
   Bool reached_bottom;

   VGP_PUSHCC(VgpGetXPt);

   // Want at least clo_depth non-alloc-fn entries in the snapshot.
   // However, because we have 1 or more (an unknown number, at this point)
   // alloc-fns ignored, we overestimate the size needed for the stack
   // snapshot.  Then, if necessary, we repeatedly increase the size until
   // it is enough.
   overestimate = 2;
   while (True) {
      n_eips = VG_(stack_snapshot)( tid, eips, clo_depth + overestimate );

      // Now we add a dummy "unknown" %eip at the end.  This is only used if we
      // run out of %eips before hitting clo_depth.  It's done to ensure the
      // XPt we return is (now and forever) a bottom-XPt.  If the returned XPt
      // wasn't a bottom-XPt (now or later) it would cause problems later (eg.
      // the parent's approx_ST wouldn't be equal [or almost equal] to the
      // total of the childrens' approx_STs).  
      eips[ n_eips++ ] = ~((Addr)0);

      // Skip over alloc functions in eips[]. 
      for (L = 0; is_alloc_fn(eips[L]) && L < n_eips; L++) { }

      // Must be at least one alloc function, unless client used
      // MALLOCLIKE_BLOCK
      if (!custom_malloc) tl_assert(L > 0);    

      // Should be at least one non-alloc function.  If not, try again.
      if (L == n_eips) {
         overestimate += 2;
         if (overestimate > MAX_ALLOC_FNS)
            VG_(tool_panic)("No stk snapshot big enough to find non-alloc fns");
      } else {
         break;
      }
   }
   A = L;
   B = n_eips - 1;
   reached_bottom = False;

   // By this point, the eips we care about are in eips[A]..eips[B]

   // Now do the search/insertion of the XCon. 'L' is the loop counter,
   // being the index into eips[].
   while (True) {
      // Look for %eip in xpt's children.
      // XXX: linear search, ugh -- about 10% of time for konqueror startup
      // XXX: tried cacheing last result, only hit about 4% for konqueror
      // Nb:  this search hits about 98% of the time for konqueror
      VGP_PUSHCC(VgpGetXPtSearch);

      // If we've searched/added deep enough, or run out of EIPs, this is
      // the bottom XPt.
      if (L - A + 1 == clo_depth || L == B) 
         reached_bottom = True;

      nC = 0;
      while (True) {
         if (nC == xpt->n_children) {
            // not found, insert new XPt
            tl_assert(xpt->max_children != 0);
            tl_assert(xpt->n_children <= xpt->max_children);
            // Expand 'children' if necessary
            if (xpt->n_children == xpt->max_children) {
               xpt->max_children *= 2;
               xpt->children = VG_(realloc)( xpt->children,
                                             xpt->max_children * sizeof(XPt*) );
               n_children_reallocs++;
            }
            // Make new XPt for %eip, insert in list
            xpt->children[ xpt->n_children++ ] = 
               new_XPt(eips[L], xpt, reached_bottom);
            break;
         }
         if (eips[L] == xpt->children[nC]->eip) break;   // found the %eip
         nC++;                                           // keep looking
      }
      VGP_POPCC(VgpGetXPtSearch);

      // Return found/built bottom-XPt.
      if (reached_bottom) {
         tl_assert(0 == xpt->children[nC]->n_children);   // Must be bottom-XPt
         VGP_POPCC(VgpGetXPt);
         return xpt->children[nC];
      }

      // Descend to next level in XTree, the newly found/built non-bottom-XPt
      xpt = xpt->children[nC];
      L++;
   }
}

// Update 'curr_space' of every XPt in the XCon, by percolating upwards.
static void update_XCon(XPt* xpt, Int space_delta)
{
   VGP_PUSHCC(VgpUpdateXCon);

   tl_assert(True == clo_heap);
   tl_assert(0    != space_delta);
   tl_assert(NULL != xpt);
   tl_assert(0    == xpt->n_children);    // must be bottom-XPt

   while (xpt != alloc_xpt) {
      if (space_delta < 0) tl_assert(xpt->curr_space >= -space_delta);
      xpt->curr_space += space_delta;
      xpt = xpt->parent;
   } 
   if (space_delta < 0) tl_assert(alloc_xpt->curr_space >= -space_delta);
   alloc_xpt->curr_space += space_delta;

   VGP_POPCC(VgpUpdateXCon);
}

// Actually want a reverse sort, biggest to smallest
static Int XPt_cmp_approx_ST(void* n1, void* n2)
{
   XPt* xpt1 = *(XPt**)n1;
   XPt* xpt2 = *(XPt**)n2;
   return (xpt1->approx_ST < xpt2->approx_ST ? 1 : -1);
}

static Int XPt_cmp_exact_ST_dbld(void* n1, void* n2)
{
   XPt* xpt1 = *(XPt**)n1;
   XPt* xpt2 = *(XPt**)n2;
   return (xpt1->exact_ST_dbld < xpt2->exact_ST_dbld ? 1 : -1);
}


/*------------------------------------------------------------*/
/*--- A generic Queue                                      ---*/
/*------------------------------------------------------------*/

typedef
   struct {
      UInt   head;         // Index of first entry
      UInt   tail;         // Index of final+1 entry, ie. next free slot
      UInt   max_elems;
      void** elems;
   }
   Queue;

static Queue* construct_queue(UInt size)
{
   UInt i;
   Queue* q     = VG_(malloc)(sizeof(Queue));
   q->head      = 0;
   q->tail      = 0;
   q->max_elems = size;
   q->elems     = VG_(malloc)(size * sizeof(void*));
   for (i = 0; i < size; i++)
      q->elems[i] = NULL;

   return q;
}

static void destruct_queue(Queue* q)
{
   VG_(free)(q->elems);
   VG_(free)(q);
}

static void shuffle(Queue* dest_q, void** old_elems)
{
   UInt i, j;
   for (i = 0, j = dest_q->head;   j < dest_q->tail;   i++, j++)
      dest_q->elems[i] = old_elems[j];
   dest_q->head = 0;
   dest_q->tail = i;
   for (  ; i < dest_q->max_elems; i++)
      dest_q->elems[i] = NULL;      // paranoia
}
      
// Shuffles elements down.  If not enough slots free, increase size. (We
// don't wait until we've completely run out of space, because there could
// be lots of shuffling just before that point which would be slow.)
static void adjust(Queue* q)
{
   void** old_elems;

   tl_assert(q->tail == q->max_elems);
   if (q->head < 10) {
      old_elems     = q->elems;
      q->max_elems *= 2; 
      q->elems      = VG_(malloc)(q->max_elems * sizeof(void*));
      shuffle(q, old_elems);
      VG_(free)(old_elems);
   } else {
      shuffle(q, q->elems);
   }
}

static void enqueue(Queue* q, void* elem)
{
   if (q->tail == q->max_elems)
      adjust(q);
   q->elems[q->tail++] = elem;
}

static Bool is_empty_queue(Queue* q)
{
   return (q->head == q->tail);
}

static void* dequeue(Queue* q)
{
   if (is_empty_queue(q))
      return NULL;         // Queue empty
   else
      return q->elems[q->head++];
}

/*------------------------------------------------------------*/
/*--- malloc() et al replacement wrappers                  ---*/
/*------------------------------------------------------------*/

static __inline__ 
void add_HP_Chunk(HP_Chunk* hc)
{
   n_heap_blocks++;
   VG_(HT_add_node) ( malloc_list, (VgHashNode*)hc );
}

static __inline__ 
HP_Chunk* get_HP_Chunk(void* p, HP_Chunk*** prev_chunks_next_ptr)
{
   return (HP_Chunk*)VG_(HT_get_node) ( malloc_list, (UWord)p,
                                        (VgHashNode***)prev_chunks_next_ptr );
}

static __inline__
void remove_HP_Chunk(HP_Chunk* hc, HP_Chunk** prev_chunks_next_ptr)
{
   tl_assert(n_heap_blocks > 0);
   n_heap_blocks--;
   *prev_chunks_next_ptr = hc->next;
}

// Forward declaration
static void hp_census(void);

static
void* new_block ( void* p, SizeT size, SizeT align, Bool is_zeroed )
{
   HP_Chunk* hc;
   Bool custom_alloc = (NULL == p);
   if (size < 0) return NULL;

   VGP_PUSHCC(VgpCliMalloc);

   // Update statistics
   n_allocs++;
   if (0 == size) n_zero_allocs++;

   // Allocate and zero if necessary
   if (!p) {
      p = VG_(cli_malloc)( align, size );
      if (!p) {
         VGP_POPCC(VgpCliMalloc);
         return NULL;
      }
      if (is_zeroed) VG_(memset)(p, 0, size);
   }

   // Make new HP_Chunk node, add to malloclist
   hc       = VG_(malloc)(sizeof(HP_Chunk));
   hc->size = size;
   hc->data = (Addr)p;
   hc->where = NULL;    // paranoia
   if (clo_heap) {
      hc->where = get_XCon( VG_(get_current_or_recent_tid)(), custom_alloc );
      if (0 != size) 
         update_XCon(hc->where, size);
   }
   add_HP_Chunk( hc );

   // do a census!
   hp_census();      

   VGP_POPCC(VgpCliMalloc);
   return p;
}

static __inline__
void die_block ( void* p, Bool custom_free )
{
   HP_Chunk *hc, **remove_handle;
   
   VGP_PUSHCC(VgpCliMalloc);

   // Update statistics
   n_frees++;

   // Remove HP_Chunk from malloclist
   hc = get_HP_Chunk( p, &remove_handle );
   if (hc == NULL)
      return;   // must have been a bogus free(), or p==NULL
   tl_assert(hc->data == (Addr)p);
   remove_HP_Chunk(hc, remove_handle);

   if (clo_heap && hc->size != 0)
      update_XCon(hc->where, -hc->size);

   VG_(free)( hc );

   // Actually free the heap block, if necessary
   if (!custom_free)
      VG_(cli_free)( p );

   // do a census!
   hp_census();

   VGP_POPCC(VgpCliMalloc);
}
 

void* TL_(malloc) ( SizeT n )
{
   return new_block( NULL, n, VG_(clo_alignment), /*is_zeroed*/False );
}

void* TL_(__builtin_new) ( SizeT n )
{
   return new_block( NULL, n, VG_(clo_alignment), /*is_zeroed*/False );
}

void* TL_(__builtin_vec_new) ( SizeT n )
{
   return new_block( NULL, n, VG_(clo_alignment), /*is_zeroed*/False );
}

void* TL_(calloc) ( SizeT m, SizeT size )
{
   return new_block( NULL, m*size, VG_(clo_alignment), /*is_zeroed*/True );
}

void *TL_(memalign)( SizeT align, SizeT n )
{
   return new_block( NULL, n, align, False );
}

void TL_(free) ( void* p )
{
   die_block( p, /*custom_free*/False );
}

void TL_(__builtin_delete) ( void* p )
{
   die_block( p, /*custom_free*/False);
}

void TL_(__builtin_vec_delete) ( void* p )
{
   die_block( p, /*custom_free*/False );
}

void* TL_(realloc) ( void* p_old, SizeT new_size )
{
   HP_Chunk*    hc;
   HP_Chunk**   remove_handle;
   Int          i;
   void*        p_new;
   SizeT        old_size;
   XPt         *old_where, *new_where;
   
   VGP_PUSHCC(VgpCliMalloc);

   // First try and find the block.
   hc = get_HP_Chunk ( p_old, &remove_handle );
   if (hc == NULL) {
      VGP_POPCC(VgpCliMalloc);
      return NULL;   // must have been a bogus free()
   }

   tl_assert(hc->data == (Addr)p_old);
   old_size = hc->size;
  
   if (new_size <= old_size) {
      // new size is smaller or same;  block not moved
      p_new = p_old;

   } else {
      // new size is bigger;  make new block, copy shared contents, free old
      p_new = VG_(cli_malloc)(VG_(clo_alignment), new_size);

      for (i = 0; i < old_size; i++)
         ((UChar*)p_new)[i] = ((UChar*)p_old)[i];

      VG_(cli_free)(p_old);
   }
   
   old_where = hc->where;
   new_where = get_XCon( VG_(get_current_or_recent_tid)(),
                         /*custom_malloc*/False);

   // Update HP_Chunk
   hc->data  = (Addr)p_new;
   hc->size  = new_size;
   hc->where = new_where;

   // Update XPt curr_space fields
   if (clo_heap) {
      if (0 != old_size) update_XCon(old_where, -old_size);
      if (0 != new_size) update_XCon(new_where,  new_size);
   }

   // If block has moved, have to remove and reinsert in the malloclist
   // (since the updated 'data' field is the hash lookup key).
   if (p_new != p_old) {
      remove_HP_Chunk(hc, remove_handle);
      add_HP_Chunk(hc);
   }

   VGP_POPCC(VgpCliMalloc);
   return p_new;
}


/*------------------------------------------------------------*/
/*--- Taking a census                                      ---*/
/*------------------------------------------------------------*/

static Census censi[MAX_N_CENSI];
static UInt   curr_census = 0;

// Must return False so that all stacks are traversed
static Bool count_stack_size( Addr stack_min, Addr stack_max, void *cp )
{
   *(UInt *)cp  += (stack_max - stack_min);
   return False;
}

static UInt get_xtree_size(XPt* xpt, UInt ix)
{
   UInt i;

   // If no memory allocated at all, nothing interesting to record.
   if (alloc_xpt->curr_space == 0) return 0;
   
   // Ignore sub-XTrees that account for a miniscule fraction of current
   // allocated space.
   if (xpt->curr_space / (double)alloc_xpt->curr_space > 0.002) {
      ix++;

      // Count all (non-zero) descendent XPts
      for (i = 0; i < xpt->n_children; i++)
         ix = get_xtree_size(xpt->children[i], ix);
   }
   return ix;
}

static 
UInt do_space_snapshot(XPt xpt[], XTreeSnapshot xtree_snapshot, UInt ix)
{
   UInt i;

   // Structure of this function mirrors that of get_xtree_size().

   if (alloc_xpt->curr_space == 0) return 0;
   
   if (xpt->curr_space / (double)alloc_xpt->curr_space > 0.002) {
      xtree_snapshot[ix].xpt   = xpt;
      xtree_snapshot[ix].space = xpt->curr_space;
      ix++;

      for (i = 0; i < xpt->n_children; i++)
         ix = do_space_snapshot(xpt->children[i], xtree_snapshot, ix);
   }
   return ix;
}

static UInt ms_interval;
static UInt do_every_nth_census = 30;

// Weed out half the censi;  we choose those that represent the smallest
// time-spans, because that loses the least information.
//
// Algorithm for N censi:  We find the census representing the smallest
// timeframe, and remove it.  We repeat this until (N/2)-1 censi are gone.
// (It's (N/2)-1 because we never remove the first and last censi.)
// We have to do this one census at a time, rather than finding the (N/2)-1
// smallest censi in one hit, because when a census is removed, it's
// neighbours immediately cover greater timespans.  So it's N^2, but N only
// equals 200, and this is only done every 100 censi, which is not too often.
static void halve_censi(void)
{
   Int     i, jp, j, jn, k;
   Census* min_census;

   n_halvings++;
   if (VG_(clo_verbosity) > 1)
      VG_(message)(Vg_UserMsg, "Halving censi...");

   // Sets j to the index of the first not-yet-removed census at or after i
   #define FIND_CENSUS(i, j) \
      for (j = i; -1 == censi[j].ms_time; j++) { }

   for (i = 2; i < MAX_N_CENSI; i += 2) {
      // Find the censi representing the smallest timespan.  The timespan
      // for census n = d(N-1,N)+d(N,N+1), where d(A,B) is the time between
      // censi A and B.  We don't consider the first and last censi for
      // removal.
      Int min_span = 0x7fffffff;
      Int min_j    = 0;

      // Initial triple: (prev, curr, next) == (jp, j, jn)
      jp = 0;
      FIND_CENSUS(1,   j);
      FIND_CENSUS(j+1, jn);
      while (jn < MAX_N_CENSI) {
         Int timespan = censi[jn].ms_time - censi[jp].ms_time;
         tl_assert(timespan >= 0);
         if (timespan < min_span) {
            min_span = timespan;
            min_j    = j;
         }
         // Move on to next triple
         jp = j; 
         j  = jn;
         FIND_CENSUS(jn+1, jn);
      }
      // We've found the least important census, now remove it
      min_census = & censi[ min_j ];
      for (k = 0; NULL != min_census->xtree_snapshots[k]; k++) {
         n_snapshot_frees++;
         VG_(free)(min_census->xtree_snapshots[k]); 
         min_census->xtree_snapshots[k] = NULL;
      }
      min_census->ms_time = -1;
   }

   // Slide down the remaining censi over the removed ones.  The '<=' is
   // because we are removing on (N/2)-1, rather than N/2.
   for (i = 0, j = 0; i <= MAX_N_CENSI / 2; i++, j++) {
      FIND_CENSUS(j, j);
      if (i != j) {
         censi[i] = censi[j];
      }
   }
   curr_census = i;

   // Double intervals
   ms_interval         *= 2;
   do_every_nth_census *= 2;

   if (VG_(clo_verbosity) > 1)
      VG_(message)(Vg_UserMsg, "...done");
}

// Take a census.  Census time seems to be insignificant (usually <= 0 ms,
// almost always <= 1ms) so don't have to worry about subtracting it from
// running time in any way.
//
// XXX: NOT TRUE!  with bigger depths, konqueror censuses can easily take
//      50ms!
static void hp_census(void)
{
   static UInt ms_prev_census = 0;
   static UInt ms_next_census = 0;     // zero allows startup census

   Int     ms_time, ms_time_since_prev;
   Int     i, K;
   Census* census;

   VGP_PUSHCC(VgpCensus);

   // Only do a census if it's time
   ms_time            = VG_(read_millisecond_timer)();
   ms_time_since_prev = ms_time - ms_prev_census;
   if (ms_time < ms_next_census) {
      n_fake_censi++;
      VGP_POPCC(VgpCensus);
      return;
   }
   n_real_censi++;

   census = & censi[curr_census];

   census->ms_time = ms_time;

   // Heap: snapshot the K most significant XTrees -------------------
   if (clo_heap) {
      K = ( alloc_xpt->n_children < MAX_SNAPSHOTS 
          ? alloc_xpt->n_children
          : MAX_SNAPSHOTS);     // max out

      // Update .approx_ST field (approximatively) for all top-XPts.
      // We *do not* do it for any non-top-XPTs.
      for (i = 0; i < alloc_xpt->n_children; i++) {
         XPt* top_XPt = alloc_xpt->children[i];
         top_XPt->approx_ST += top_XPt->curr_space * ms_time_since_prev;
      }
      // Sort top-XPts by approx_ST field.
      VG_(ssort)(alloc_xpt->children, alloc_xpt->n_children, sizeof(XPt*),
                 XPt_cmp_approx_ST);

      VGP_PUSHCC(VgpCensusHeap);

      // For each significant top-level XPt, record space info about its
      // entire XTree, in a single census entry.
      // Nb: the xtree_size count/snapshot buffer allocation, and the actual
      // snapshot, take similar amounts of time (measured with the
      // millisecond counter).
      for (i = 0; i < K; i++) {
         UInt xtree_size, xtree_size2;
//         VG_(printf)("%7u ", alloc_xpt->children[i]->approx_ST);
         // Count how many XPts are in the XTree
         VGP_PUSHCC(VgpCensusTreeSize);
         xtree_size = get_xtree_size( alloc_xpt->children[i], 0 );
         VGP_POPCC(VgpCensusTreeSize);

         // If no XPts counted (ie. alloc_xpt.curr_space==0 or XTree
         // insignificant) then don't take any more snapshots.
         if (0 == xtree_size) break;
         
         // Make array of the appropriate size (+1 for zero termination,
         // which calloc() does for us).
         census->xtree_snapshots[i] =
            VG_(calloc)(xtree_size+1, sizeof(XPtSnapshot));
         if (0 && VG_(clo_verbosity) > 1)
            VG_(printf)("calloc: %d (%d B)\n", xtree_size+1,
                        (xtree_size+1) * sizeof(XPtSnapshot));

         // Take space-snapshot: copy 'curr_space' for every XPt in the
         // XTree into the snapshot array, along with pointers to the XPts.
         // (Except for ones with curr_space==0, which wouldn't contribute
         // to the final exact_ST_dbld calculation anyway;  excluding them
         // saves a lot of memory and up to 40% time with big --depth valus.
         VGP_PUSHCC(VgpCensusSnapshot);
         xtree_size2 = do_space_snapshot(alloc_xpt->children[i],
                                         census->xtree_snapshots[i], 0);
         tl_assert(xtree_size == xtree_size2);
         VGP_POPCC(VgpCensusSnapshot);
      }
//      VG_(printf)("\n\n");
      // Zero-terminate 'xtree_snapshot' array
      census->xtree_snapshots[i] = NULL;

      VGP_POPCC(VgpCensusHeap);

      //VG_(printf)("printed %d censi\n", K);

      // Lump the rest into a single "others" entry.
      census->others_space = 0;
      for (i = K; i < alloc_xpt->n_children; i++) {
         census->others_space += alloc_xpt->children[i]->curr_space;
      }
   }

   // Heap admin -------------------------------------------------------
   if (clo_heap_admin > 0)
      census->heap_admin_space = clo_heap_admin * n_heap_blocks;

   // Stack(s) ---------------------------------------------------------
   if (clo_stacks) {
      census->stacks_space = sigstacks_space;
      // slightly abusing this function
      VG_(first_matching_thread_stack)( count_stack_size, &census->stacks_space );
      i++;
   }

   // Finish, update interval if necessary -----------------------------
   curr_census++;
   census = NULL;    // don't use again now that curr_census changed

   // Halve the entries, if our census table is full
   if (MAX_N_CENSI == curr_census) {
      halve_censi();
   }

   // Take time for next census from now, rather than when this census
   // should have happened.  Because, if there's a big gap due to a kernel
   // operation, there's no point doing catch-up censi every BB for a while
   // -- that would just give N censi at almost the same time.
   if (VG_(clo_verbosity) > 1) {
      VG_(message)(Vg_UserMsg, "census: %d ms (took %d ms)", ms_time, 
                               VG_(read_millisecond_timer)() - ms_time );
   }
   ms_prev_census = ms_time;
   ms_next_census = ms_time + ms_interval;
   //ms_next_census += ms_interval;

   //VG_(printf)("Next: %d ms\n", ms_next_census);

   VGP_POPCC(VgpCensus);
} 

/*------------------------------------------------------------*/
/*--- Tracked events                                       ---*/
/*------------------------------------------------------------*/

static void new_mem_stack_signal(Addr a, SizeT len)
{
   sigstacks_space += len;
}

static void die_mem_stack_signal(Addr a, SizeT len)
{
   tl_assert(sigstacks_space >= len);
   sigstacks_space -= len;
}

/*------------------------------------------------------------*/
/*--- Client Requests                                      ---*/
/*------------------------------------------------------------*/

Bool TL_(handle_client_request) ( ThreadId tid, UWord* argv, UWord* ret )
{
   switch (argv[0]) {
   case VG_USERREQ__MALLOCLIKE_BLOCK: {
      void* res;
      void* p         = (void*)argv[1];
      SizeT sizeB     =        argv[2];
      *ret            = 0;
      res = new_block( p, sizeB, /*align -- ignored*/0, /*is_zeroed*/False );
      tl_assert(res == p);
      return True;
   }
   case VG_USERREQ__FREELIKE_BLOCK: {
      void* p         = (void*)argv[1];
      *ret            = 0;
      die_block( p, /*custom_free*/True );
      return True;
   }
   default:
      *ret = 0;
      return False;
   }
}

/*------------------------------------------------------------*/
/*--- Initialisation                                       ---*/
/*------------------------------------------------------------*/

// Current directory at startup.
static Char* base_dir;

UInt VG_(vg_malloc_redzone_szB) = 0;

void TL_(pre_clo_init)()
{ 
   VG_(details_name)            ("Massif");
   VG_(details_version)         (NULL);
   VG_(details_description)     ("a space profiler");
   VG_(details_copyright_author)("Copyright (C) 2003, Nicholas Nethercote");
   VG_(details_bug_reports_to)  (VG_BUGS_TO);

   // Needs
   VG_(needs_libc_freeres)();
   VG_(needs_command_line_options)();
   VG_(needs_client_requests)     ();

   // Events to track
   VG_(init_new_mem_stack_signal) ( new_mem_stack_signal );
   VG_(init_die_mem_stack_signal) ( die_mem_stack_signal );

   // Profiling events
   VGP_(register_profile_event)(VgpGetXPt,         "get-XPt");
   VGP_(register_profile_event)(VgpGetXPtSearch,   "get-XPt-search");
   VGP_(register_profile_event)(VgpCensus,         "census");
   VGP_(register_profile_event)(VgpCensusHeap,     "census-heap");
   VGP_(register_profile_event)(VgpCensusSnapshot, "census-snapshot");
   VGP_(register_profile_event)(VgpCensusTreeSize, "census-treesize");
   VGP_(register_profile_event)(VgpUpdateXCon,     "update-XCon");
   VGP_(register_profile_event)(VgpCalcSpacetime2, "calc-exact_ST_dbld");
   VGP_(register_profile_event)(VgpPrintHp,        "print-hp");
   VGP_(register_profile_event)(VgpPrintXPts,      "print-XPts");

   // HP_Chunks
   malloc_list  = VG_(HT_construct)();

   // Dummy node at top of the context structure.
   alloc_xpt = new_XPt(0, NULL, /*is_bottom*/False);

   tl_assert( VG_(getcwd_alloc)(&base_dir) );
}

void TL_(post_clo_init)(void)
{
   ms_interval = 1;

   // Do an initial sample for t = 0
   hp_census();
}

/*------------------------------------------------------------*/
/*--- Instrumentation                                      ---*/
/*------------------------------------------------------------*/

IRBB* TL_(instrument) ( IRBB* bb_in, VexGuestLayout* layout, IRType hWordTy )
{
   return bb_in;
}

/*------------------------------------------------------------*/
/*--- Spacetime recomputation                              ---*/
/*------------------------------------------------------------*/

// Although we've been calculating space-time along the way, because the
// earlier calculations were done at a finer timescale, the .approx_ST field
// might not agree with what hp2ps sees, because we've thrown away some of
// the information.  So recompute it at the scale that hp2ps sees, so we can
// confidently determine which contexts hp2ps will choose for displaying as
// distinct bands.  This recomputation only happens to the significant ones
// that get printed in the .hp file, so it's cheap.
//
// The approx_ST calculation: 
//   ( a[0]*d(0,1) + a[1]*(d(0,1) + d(1,2)) + ... + a[N-1]*d(N-2,N-1) ) / 2
//   where
//   a[N] is the space at census N
//   d(A,B) is the time interval between censi A and B
//   and
//   d(A,B) + d(B,C) == d(A,C)
//
// Key point:  we can calculate the area for a census without knowing the
// previous or subsequent censi's space;  because any over/underestimates
// for this census will be reversed in the next, balancing out.  This is
// important, as getting the previous/next census entry for a particular
// AP is a pain with this data structure, but getting the prev/next
// census time is easy.
//
// Each heap calculation gets added to its context's exact_ST_dbld field.
// The ULong* values are all running totals, hence the use of "+=" everywhere.

// This does the calculations for a single census.
static void calc_exact_ST_dbld2(Census* census, UInt d_t1_t2,
                             ULong* twice_heap_ST,
                             ULong* twice_heap_admin_ST,
                             ULong* twice_stack_ST)
{
   UInt i, j;
   XPtSnapshot* xpt_snapshot;

   // Heap --------------------------------------------------------
   if (clo_heap) {
      for (i = 0; NULL != census->xtree_snapshots[i]; i++) {
         // Compute total heap exact_ST_dbld for the entire XTree using only
         // the top-XPt (the first XPt in xtree_snapshot).
         *twice_heap_ST += d_t1_t2 * census->xtree_snapshots[i][0].space;

         // Increment exact_ST_dbld for every XPt in xtree_snapshot (inc.
         // top one)
         for (j = 0; NULL != census->xtree_snapshots[i][j].xpt; j++) {
            xpt_snapshot = & census->xtree_snapshots[i][j];
            xpt_snapshot->xpt->exact_ST_dbld += d_t1_t2 * xpt_snapshot->space;
         }
      }
      *twice_heap_ST += d_t1_t2 * census->others_space;
   }

   // Heap admin --------------------------------------------------
   if (clo_heap_admin > 0)
      *twice_heap_admin_ST += d_t1_t2 * census->heap_admin_space;

   // Stack(s) ----------------------------------------------------
   if (clo_stacks)
      *twice_stack_ST += d_t1_t2 * census->stacks_space;
}

// This does the calculations for all censi.
static void calc_exact_ST_dbld(ULong* heap2, ULong* heap_admin2, ULong* stack2)
{
   UInt i, N = curr_census;

   VGP_PUSHCC(VgpCalcSpacetime2);

   *heap2       = 0;
   *heap_admin2 = 0;
   *stack2      = 0;
   
   if (N <= 1)
      return;

   calc_exact_ST_dbld2( &censi[0], censi[1].ms_time - censi[0].ms_time,
                        heap2, heap_admin2, stack2 );

   for (i = 1; i <= N-2; i++) {
      calc_exact_ST_dbld2( & censi[i], censi[i+1].ms_time - censi[i-1].ms_time,
                           heap2, heap_admin2, stack2 );
   }

   calc_exact_ST_dbld2( & censi[N-1], censi[N-1].ms_time - censi[N-2].ms_time,
                        heap2, heap_admin2, stack2 ); 
   // Now get rid of the halves.  May lose a 0.5 on each, doesn't matter.
   *heap2       /= 2;
   *heap_admin2 /= 2;
   *stack2      /= 2;

   VGP_POPCC(VgpCalcSpacetime2);
}

/*------------------------------------------------------------*/
/*--- Writing the graph file                               ---*/
/*------------------------------------------------------------*/

static Char* make_filename(Char* dir, Char* suffix)
{
   Char* filename;

   /* Block is big enough for dir name + massif.<pid>.<suffix> */
   filename = VG_(malloc)((VG_(strlen)(dir) + 32)*sizeof(Char));
   VG_(sprintf)(filename, "%s/massif.%d%s", dir, VG_(getpid)(), suffix);

   return filename;
}

// Make string acceptable to hp2ps (sigh): remove spaces, escape parentheses.
static Char* clean_fnname(Char *d, Char* s)
{
   Char* dorig = d;
   while (*s) {
      if      (' ' == *s) { *d   = '%';            }
      else if ('(' == *s) { *d++ = '\\'; *d = '('; }
      else if (')' == *s) { *d++ = '\\'; *d = ')'; }
      else                { *d   = *s;             };
      s++;
      d++;
   }
   *d = '\0';
   return dorig;
}

static void file_err ( Char* file )
{
   VG_(message)(Vg_UserMsg, "error: can't open output file `%s'", file );
   VG_(message)(Vg_UserMsg, "       ... so profile results will be missing.");
}

/* Format, by example:

   JOB "a.out -p"
   DATE "Fri Apr 17 11:43:45 1992"
   SAMPLE_UNIT "seconds"
   VALUE_UNIT "bytes"
   BEGIN_SAMPLE 0.00
     SYSTEM 24
   END_SAMPLE 0.00
   BEGIN_SAMPLE 1.00
     elim 180
     insert 24
     intersect 12
     disin 60
     main 12
     reduce 20
     SYSTEM 12
   END_SAMPLE 1.00
   MARK 1.50
   MARK 1.75
   MARK 1.80
   BEGIN_SAMPLE 2.00
     elim 192
     insert 24
     intersect 12
     disin 84
     main 12
     SYSTEM 24
   END_SAMPLE 2.00
   BEGIN_SAMPLE 2.82
   END_SAMPLE 2.82
 */
static void write_hp_file(void)
{
   Int   i, j;
   Int   fd, res;
   Char *hp_file, *ps_file, *aux_file;
   Char* cmdfmt;
   Char* cmdbuf;
   Int   cmdlen;

   VGP_PUSHCC(VgpPrintHp);
   
   // Open file
   hp_file  = make_filename( base_dir, ".hp" );
   ps_file  = make_filename( base_dir, ".ps" );
   aux_file = make_filename( base_dir, ".aux" );
   fd = VG_(open)(hp_file, VKI_O_CREAT|VKI_O_TRUNC|VKI_O_WRONLY,
                           VKI_S_IRUSR|VKI_S_IWUSR);
   if (fd < 0) {
      file_err( hp_file );
      VGP_POPCC(VgpPrintHp);
      return;
   }

   // File header, including command line
   SPRINTF(buf, "JOB         \"");
   for (i = 0; i < VG_(client_argc); i++)
      SPRINTF(buf, "%s ", VG_(client_argv)[i]);
   SPRINTF(buf, /*" (%d ms/sample)\"\n"*/ "\"\n"
                "DATE        \"\"\n"
                "SAMPLE_UNIT \"ms\"\n"
                "VALUE_UNIT  \"bytes\"\n", ms_interval);

   // Censi
   for (i = 0; i < curr_census; i++) {
      Census* census = & censi[i];

      // Census start
      SPRINTF(buf, "MARK %d.0\n"
                   "BEGIN_SAMPLE %d.0\n",
                   census->ms_time, census->ms_time);

      // Heap -----------------------------------------------------------
      if (clo_heap) {
         // Print all the significant XPts from that census
         for (j = 0; NULL != census->xtree_snapshots[j]; j++) {
            // Grab the jth top-XPt
            XTreeSnapshot xtree_snapshot = & census->xtree_snapshots[j][0];
            if ( ! VG_(get_fnname)(xtree_snapshot->xpt->eip, buf2, 16)) {
               VG_(sprintf)(buf2, "???");
            }
            SPRINTF(buf, "x%x:%s %d\n", xtree_snapshot->xpt->eip,
                         clean_fnname(buf3, buf2), xtree_snapshot->space);
         }

         // Remaining heap block alloc points, combined
         if (census->others_space > 0)
            SPRINTF(buf, "other %d\n", census->others_space);
      }

      // Heap admin -----------------------------------------------------
      if (clo_heap_admin > 0 && census->heap_admin_space)
         SPRINTF(buf, "heap-admin %d\n", census->heap_admin_space);
      
      // Stack(s) -------------------------------------------------------
      if (clo_stacks)
         SPRINTF(buf, "stack(s) %d\n", census->stacks_space);

      // Census end
      SPRINTF(buf, "END_SAMPLE %d.0\n", census->ms_time);
   }

   // Close file
   tl_assert(fd >= 0);
   VG_(close)(fd);

   // Attempt to convert file using hp2ps
   cmdfmt = "%s/hp2ps -c -t1 %s";
   cmdlen = VG_(strlen)(VG_(libdir)) + VG_(strlen)(hp_file) 
                                     + VG_(strlen)(cmdfmt);
   cmdbuf = VG_(malloc)( sizeof(Char) * cmdlen );
   VG_(sprintf)(cmdbuf, cmdfmt, VG_(libdir), hp_file);
   res = VG_(system)(cmdbuf);
   VG_(free)(cmdbuf);
   if (res != 0) {      
      VG_(message)(Vg_UserMsg, 
                   "Conversion to PostScript failed.  Try converting manually.");
   } else {
      // remove the .hp and .aux file
      VG_(unlink)(hp_file);
      VG_(unlink)(aux_file);
   }

   VG_(free)(hp_file);
   VG_(free)(ps_file);
   VG_(free)(aux_file);

   VGP_POPCC(VgpPrintHp);
}

/*------------------------------------------------------------*/
/*--- Writing the XPt text/HTML file                       ---*/
/*------------------------------------------------------------*/

static void percentify(Int n, Int pow, Int field_width, char xbuf[])
{
   int i, len, space;

   VG_(sprintf)(xbuf, "%d.%d%%", n / pow, n % pow);
   len = VG_(strlen)(xbuf);
   space = field_width - len;
   if (space < 0) space = 0;     /* Allow for v. small field_width */
   i = len;

   /* Right justify in field */
   for (     ; i >= 0;    i--)  xbuf[i + space] = xbuf[i];
   for (i = 0; i < space; i++)  xbuf[i] = ' ';
}

// Nb: uses a static buffer, each call trashes the last string returned.
static Char* make_perc(ULong spacetime, ULong total_spacetime)
{
   static Char mbuf[32];
   
   UInt  p = 10;
   tl_assert(0 != total_spacetime);
   percentify(spacetime * 100 * p / total_spacetime, p, 5, mbuf); 
   return mbuf;
}

// Nb: passed in XPt is a lower-level XPt;  %eips are grabbed from
// bottom-to-top of XCon, and then printed in the reverse order.
static UInt pp_XCon(Int fd, XPt* xpt)
{
   Addr  rev_eips[clo_depth+1];
   Int   i = 0;
   Int   n = 0;
   Bool  is_HTML      = ( XHTML == clo_format );
   Char* maybe_br     = ( is_HTML ? "<br>" : "" );
   Char* maybe_indent = ( is_HTML ? "&nbsp;&nbsp;" : "" );

   tl_assert(NULL != xpt);

   while (True) {
      rev_eips[i] = xpt->eip;
      n++;
      if (alloc_xpt == xpt->parent) break;
      i++;
      xpt = xpt->parent;
   }

   for (i = n-1; i >= 0; i--) {
      // -1 means point to calling line
      VG_(describe_eip)(rev_eips[i]-1, buf2, BUF_LEN);
      SPRINTF(buf, "  %s%s%s\n", maybe_indent, buf2, maybe_br);
   }

   return n;
}

// Important point:  for HTML, each XPt must be identified uniquely for the
// HTML links to all match up correctly.  Using xpt->eip is not
// sufficient, because function pointers mean that you can call more than
// one other function from a single code location.  So instead we use the
// address of the xpt struct itself, which is guaranteed to be unique.

static void pp_all_XPts2(Int fd, Queue* q, ULong heap_spacetime,
                         ULong total_spacetime)
{
   UInt  i;
   XPt  *xpt, *child;
   UInt  L   = 0;
   UInt  c1  = 1;
   UInt  c2  = 0;
   ULong sum = 0;
   UInt  n;
   Char *eip_desc, *perc;
   Bool  is_HTML   = ( XHTML == clo_format );
   Char* maybe_br  = ( is_HTML ?  "<br>" : "" );
   Char* maybe_p   = ( is_HTML ?   "<p>" : "" );
   Char* maybe_ul  = ( is_HTML ?  "<ul>" : "" );
   Char* maybe_li  = ( is_HTML ?  "<li>" : "" );
   Char* maybe_fli = ( is_HTML ? "</li>" : "" );
   Char* maybe_ful = ( is_HTML ? "</ul>" : "" );
   Char* end_hr    = ( is_HTML ? "<hr>"  : 
                                 "=================================" );
   Char* depth     = ( is_HTML ? "<code>--depth</code>" : "--depth" );

   if (total_spacetime == 0) {
      SPRINTF(buf, "(No heap memory allocated)\n");
      return;
   }


   SPRINTF(buf, "== %d ===========================%s\n", L, maybe_br);

   while (NULL != (xpt = (XPt*)dequeue(q))) {
      // Check that non-top-level XPts have a zero .approx_ST field.
      if (xpt->parent != alloc_xpt) tl_assert( 0 == xpt->approx_ST );

      // Check that the sum of all children .exact_ST_dbld fields equals
      // parent's (unless alloc_xpt, when it should == 0).
      if (alloc_xpt == xpt) {
         tl_assert(0 == xpt->exact_ST_dbld);
      } else {
         sum = 0;
         for (i = 0; i < xpt->n_children; i++) {
            sum += xpt->children[i]->exact_ST_dbld;
         }
         //tl_assert(sum == xpt->exact_ST_dbld);
         // It's possible that not all the children were included in the
         // exact_ST_dbld calculations.  Hopefully almost all of them were, and
         // all the important ones.
//         tl_assert(sum <= xpt->exact_ST_dbld);
//         tl_assert(sum * 1.05 > xpt->exact_ST_dbld );
//         if (sum != xpt->exact_ST_dbld) {
//            VG_(printf)("%ld, %ld\n", sum, xpt->exact_ST_dbld);
//         }
      }

      if (xpt == alloc_xpt) {
         SPRINTF(buf, "Heap allocation functions accounted for "
                      "%s of measured spacetime%s\n", 
                      make_perc(heap_spacetime, total_spacetime), maybe_br);
      } else {
         // Remember: exact_ST_dbld is space.time *doubled*
         perc = make_perc(xpt->exact_ST_dbld / 2, total_spacetime);
         if (is_HTML) {
            SPRINTF(buf, "<a name=\"b%x\"></a>"
                         "Context accounted for "
                         "<a href=\"#a%x\">%s</a> of measured spacetime<br>\n",
                         xpt, xpt, perc);
         } else {
            SPRINTF(buf, "Context accounted for %s of measured spacetime\n",
                         perc);
         }
         n = pp_XCon(fd, xpt);
         tl_assert(n == L);
      }

      // Sort children by exact_ST_dbld
      VG_(ssort)(xpt->children, xpt->n_children, sizeof(XPt*),
                 XPt_cmp_exact_ST_dbld);

      SPRINTF(buf, "%s\nCalled from:%s\n", maybe_p, maybe_ul);
      for (i = 0; i < xpt->n_children; i++) {
         child = xpt->children[i];

         // Stop when <1% of total spacetime
         if (child->exact_ST_dbld * 1000 / (total_spacetime * 2) < 5) {
            UInt  n_insig = xpt->n_children - i;
            Char* s       = ( n_insig == 1 ? "" : "s" );
            Char* and     = ( 0 == i ? "" : "and "   );
            Char* other   = ( 0 == i ? "" : "other " );
            SPRINTF(buf, "  %s%s%d %sinsignificant place%s%s\n\n",
                    maybe_li, and, n_insig, other, s, maybe_fli);
            break;
         }

         // Remember: exact_ST_dbld is space.time *doubled*
         perc     = make_perc(child->exact_ST_dbld / 2, total_spacetime);
         eip_desc = VG_(describe_eip)(child->eip-1, buf2, BUF_LEN);
         if (is_HTML) {
            SPRINTF(buf, "<li><a name=\"a%x\"></a>", child );

            if (child->n_children > 0) {
               SPRINTF(buf, "<a href=\"#b%x\">%s</a>", child, perc);
            } else {
               SPRINTF(buf, "%s", perc);
            }
            SPRINTF(buf, ": %s\n", eip_desc);
         } else {
            SPRINTF(buf, "  %6s: %s\n\n", perc, eip_desc);
         }

         if (child->n_children > 0) {
            enqueue(q, (void*)child);
            c2++;
         }
      }
      SPRINTF(buf, "%s%s", maybe_ful, maybe_p);
      c1--;
      
      // Putting markers between levels of the structure:
      // c1 tracks how many to go on this level, c2 tracks how many we've
      // queued up for the next level while finishing off this level.  
      // When c1 gets to zero, we've changed levels, so print a marker,
      // move c2 into c1, and zero c2.
      if (0 == c1) {
         L++;
         c1 = c2;
         c2 = 0;
         if (! is_empty_queue(q) ) {      // avoid empty one at end
            SPRINTF(buf, "== %d ===========================%s\n", L, maybe_br);
         }
      } else {
         SPRINTF(buf, "---------------------------------%s\n", maybe_br);
      }
   }
   SPRINTF(buf, "%s\n\nEnd of information.  Rerun with a bigger "
                "%s value for more.\n", end_hr, depth);
}

static void pp_all_XPts(Int fd, XPt* xpt, ULong heap_spacetime,
                        ULong total_spacetime)
{
   Queue* q = construct_queue(100);

   enqueue(q, xpt);
   pp_all_XPts2(fd, q, heap_spacetime, total_spacetime);
   destruct_queue(q);
}

static void
write_text_file(ULong total_ST, ULong heap_ST)
{
   Int   fd, i;
   Char* text_file;
   Char* maybe_p = ( XHTML == clo_format ? "<p>" : "" );

   VGP_PUSHCC(VgpPrintXPts);

   // Open file
   text_file = make_filename( base_dir, 
                              ( XText == clo_format ? ".txt" : ".html" ) );

   fd = VG_(open)(text_file, VKI_O_CREAT|VKI_O_TRUNC|VKI_O_WRONLY,
                             VKI_S_IRUSR|VKI_S_IWUSR);
   if (fd < 0) {
      file_err( text_file );
      VGP_POPCC(VgpPrintXPts);
      return;
   }

   // Header
   if (XHTML == clo_format) {
      SPRINTF(buf, "<html>\n"
                   "<head>\n"
                   "<title>%s</title>\n"
                   "</head>\n"
                   "<body>\n",
                   text_file);
   }

   // Command line
   SPRINTF(buf, "Command: ");
   for (i = 0; i < VG_(client_argc); i++)
      SPRINTF(buf, "%s ", VG_(client_argv)[i]);
   SPRINTF(buf, "\n%s\n", maybe_p);

   if (clo_heap)
      pp_all_XPts(fd, alloc_xpt, heap_ST, total_ST);

   tl_assert(fd >= 0);
   VG_(close)(fd);

   VGP_POPCC(VgpPrintXPts);
}

/*------------------------------------------------------------*/
/*--- Finalisation                                         ---*/
/*------------------------------------------------------------*/

static void
print_summary(ULong total_ST, ULong heap_ST, ULong heap_admin_ST,
              ULong stack_ST)
{
   VG_(message)(Vg_UserMsg, "Total spacetime:   %,ld ms.B", total_ST);

   // Heap --------------------------------------------------------------
   if (clo_heap)
      VG_(message)(Vg_UserMsg, "heap:              %s",
                   ( 0 == total_ST ? (Char*)"(n/a)"
                                   : make_perc(heap_ST, total_ST) ) );

   // Heap admin --------------------------------------------------------
   if (clo_heap_admin)
      VG_(message)(Vg_UserMsg, "heap admin:        %s", 
                   ( 0 == total_ST ? (Char*)"(n/a)"
                                   : make_perc(heap_admin_ST, total_ST) ) );

   tl_assert( VG_(HT_count_nodes)(malloc_list) == n_heap_blocks );

   // Stack(s) ----------------------------------------------------------
   if (clo_stacks) {
      tl_assert(0 != total_ST);
      VG_(message)(Vg_UserMsg, "stack(s):          %s", 
                   make_perc(stack_ST, total_ST) );
   }

   if (VG_(clo_verbosity) > 1) {
      tl_assert(n_xpts > 0);  // always have alloc_xpt
      VG_(message)(Vg_DebugMsg, "    allocs: %u", n_allocs);
      VG_(message)(Vg_DebugMsg, "zeroallocs: %u (%d%%)", n_zero_allocs,
                                n_zero_allocs * 100 / n_allocs );
      VG_(message)(Vg_DebugMsg, "     frees: %u", n_frees);
      VG_(message)(Vg_DebugMsg, "      XPts: %u (%d B)", n_xpts,
                                                         n_xpts*sizeof(XPt));
      VG_(message)(Vg_DebugMsg, "  bot-XPts: %u (%d%%)", n_bot_xpts,
                                n_bot_xpts * 100 / n_xpts);
      VG_(message)(Vg_DebugMsg, "  top-XPts: %u (%d%%)", alloc_xpt->n_children,
                                alloc_xpt->n_children * 100 / n_xpts);
      VG_(message)(Vg_DebugMsg, "c-reallocs: %u", n_children_reallocs);
      VG_(message)(Vg_DebugMsg, "snap-frees: %u", n_snapshot_frees);
      VG_(message)(Vg_DebugMsg, "atmp censi: %u", n_attempted_censi);
      VG_(message)(Vg_DebugMsg, "fake censi: %u", n_fake_censi);
      VG_(message)(Vg_DebugMsg, "real censi: %u", n_real_censi);
      VG_(message)(Vg_DebugMsg, "  halvings: %u", n_halvings);
   }
}

void TL_(fini)(Int exit_status)
{
   ULong total_ST      = 0;
   ULong heap_ST       = 0;
   ULong heap_admin_ST = 0;
   ULong stack_ST      = 0;

   // Do a final (empty) sample to show program's end
   hp_census();

   // Redo spacetimes of significant contexts to match the .hp file.
   calc_exact_ST_dbld(&heap_ST, &heap_admin_ST, &stack_ST);
   total_ST = heap_ST + heap_admin_ST + stack_ST; 
   write_hp_file  ( );
   write_text_file( total_ST, heap_ST );
   print_summary  ( total_ST, heap_ST, heap_admin_ST, stack_ST );
}

VG_DETERMINE_INTERFACE_VERSION(TL_(pre_clo_init), 0)

/*--------------------------------------------------------------------*/
/*--- end                                                ms_main.c ---*/
/*--------------------------------------------------------------------*/

