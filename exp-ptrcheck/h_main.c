
/*--------------------------------------------------------------------*/
/*--- Ptrcheck: a pointer-use checker.                             ---*/
/*--- This file checks heap accesses.                              ---*/
/*---                                                     h_main.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Ptrcheck, a Valgrind tool for checking pointer
   use in programs.

   Initial version (Annelid):

   Copyright (C) 2003-2010 Nicholas Nethercote
      njn@valgrind.org

   Valgrind-3.X port:

   Copyright (C) 2008-2010 OpenWorks Ltd
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

// FIXME: 64-bit cleanness, check the following
// struct _ISNode.ownerCount is 32-bit
// struct _ISNode.topLevel is 32-bit
// or is that not really right now?  add assertion checks about
// the max size of a node

// FIXME: should we shadow %RIP?  Maybe not.

// FIXME: shadows of temporaries created in preamble, a la memcheck?

// FIXME: result of add_new_segment is always ignored

// FIXME: the mechanism involving last_seg_added is really ugly.
// Do something cleaner.

// FIXME: post_reg_write_clientcall: check function pointer comparisons
// are safe on toc-afflicted platforms

// FIXME: tidy up findShadowTmp

// FIXME: post_reg_write_demux(Vg_CoreSysCall) is redundant w.r.t.
// the default 'NONPTR' behaviour of post_syscall.  post_reg_write_demux
// is called first, then post_syscall.

// FIXME: check nothing is mapped in the lowest 1M of memory at
// startup, or quit (to do with nonptr_or_unknown, also sync 1M
// magic value with PIE default load address in m_ume.c.

// FIXME: consider whether we could paint memory acquired from
// sys_read etc as NONPTR rather than UNKNOWN.

// XXX: recycle freed segments

//--------------------------------------------------------------
// Metadata:
//   HeapBlock.id :: Seg (stored as heap shadowchunk; always non-zero)
//   MemLoc.aseg  :: Seg (implicitly stored)
//   MemLoc.vseg  :: Seg (explicitly stored as the shadow memory)
//   RegLoc.vseg  :: Seg (explicitly stored as shadow registers)
//
// A Seg is made when new memory is created, eg. with malloc() or mmap().
// There are two other Segs:
//  - NONPTR:  for something that's definitely not a pointer
//  - UNKNOWN: for something that could be a pointer
//  - BOTTOM:  used with pointer differences (see below)
//
// MemLoc.vseg is done at word granularity.  If a pointer is written
// to memory misaligned, the information about it will be lost -- it's
// treated as two sub-word writes to two adjacent words.  This avoids
// certain nasty cases that could arise if we tried to track unaligned
// pointers.  Fortunately, misalignment is rare so we don't lose much
// information this way.
//
// MemLoc.aseg is done at byte granularity, and *implicitly* -- ie. not
// directly accessible like MemLoc.vseg, but only by searching through all
// the segments.  Fortunately, it's mostly checked at LOADs/STOREs;  at that
// point we have a pointer p to the MemLoc m as the other arg of the
// LOAD/STORE, so we can check to see if the p.vseg's range includes m.  If
// not, it's an error and we have to search through all segments to find out
// what m.aseg really is.  That's still pretty fast though, thanks to the
// interval skip-list used.  With syscalls we must also do the skip-list
// search, but only on the first and last bytes touched.
//--------------------------------------------------------------

//--------------------------------------------------------------
// Assumptions, etc:
// - see comment at top of SK_(instrument)() for how sub-word ops are
//   handled.
//
// - ioctl(), socketcall() (and ipc() will be) assumed to return non-pointers
//
// - FPU_W is assumed to never write pointers.
//
// - Assuming none of the post_mem_writes create segments worth tracking.
//
// - Treating mmap'd segments (all! including code) like heap segments.  But
//   their ranges can change, new ones can be created by unmapping parts of
//   old segments, etc.  But this nasty behaviour seems to never happen -- 
//   there are assertions checking it.
//--------------------------------------------------------------

//--------------------------------------------------------------
// What I am checking:
// - Type errors:
//    * ADD, OR, LEA2: error if two pointer inputs.
//    * ADC, SBB: error if one or two pointer inputs.
//    * AND, OR: error if two unequal pointer inputs.
//    * NEG: error if pointer input.
//    * {,i}mul_32_64 if either input is a pointer.
//    * shldl/shrdl, bsf/bsr if any inputs are pointers.
//
// - LOAD, STORE:
//    * ptr.vseg must match ptee.aseg.
//    * ptee.aseg must not be a freed segment.
//
// - syscalls: for those accessing memory, look at first and last bytes:
//    * check first.aseg == last.aseg
//    * check first.aseg and last.aseg are not freed segments.
//
// What I am not checking, that I expected to when I started:
// - AND, XOR: allowing two pointers to be used if both from the same segment,
//   because "xor %r,%r" is commonly used to zero %r, and "test %r,%r"
//   (which is translated with an AND) is common too.
//
// - div_64_32/idiv_64_32 can take pointer inputs for the dividend;
//   division doesn't make sense, but modulo does, and they're done with the
//   same instruction.  (Could try to be super-clever and watch the outputs
//   to see if the quotient is used, but not worth it.)
//
// - mul_64_32/imul_64_32 can take pointers inputs for one arg or the
//   other, but not both.  This is because some programs (eg. Mozilla
//   Firebird) multiply pointers in hash routines.
//
// - NEG: can take a pointer.  It happens in glibc in a few places.  I've
//   seen the code, didn't understand it, but it's done deliberately.
//
// What I am not checking/doing, but could, but it would require more
// instrumentation and/or slow things down a bit:
// - SUB: when differencing two pointers, result is BOTTOM, ie. "don't
//   check".  Could link segments instead, slower but a bit more accurate.
//   Also use BOTTOM when doing (ptr - unknown), which could be a pointer
//   difference with a stack/static pointer.
//
// - PUTF: input should be non-pointer
//
// - arithmetic error messages: eg. for adding two pointers, just giving the
//   segments, not the actual pointers.
//
// What I am not checking, and would be difficult:
// - mmap(...MAP_FIXED...) is not handled specially.  It might be used in
//   ways that fool Ptrcheck into giving false positives.
//
// - syscalls: for those accessing memory, not checking that the asegs of the
//   accessed words match the vseg of the accessing pointer, because the
//   vseg is not easily accessible at the required time (would required
//   knowing for every syscall which register each arg came in, and looking
//   there).
//
// What I am not checking, and would be difficult, but doesn't matter:
// - free(p): similar to syscalls, not checking that the p.vseg matches the
//   aseg of the first byte in the block.  However, Memcheck does an
//   equivalent "bad free" check using shadow_chunks;  indeed, Ptrcheck could
//   do the same check, but there's no point duplicating functionality.  So
//   no loss, really.
//
// Other:
// - not doing anything with mprotect();  probably not worth the effort.
//--------------------------------------------------------------

//--------------------------------------------------------------
// Todo:
// - Segments for stack frames.  Would detect (some, large) stack
//   over/under-runs, dangling pointers.
//
// - Segments for static data.  Would detect over/under-runs.  Requires
//   reading debug info.
//--------------------------------------------------------------

//--------------------------------------------------------------
// Some profiling results:
//                                                 twolf   konq    date sz
// 1. started                                              35.0s   14.7
// 2. introduced GETV/PUTV                                 30.2s   10.1
// 3. inlined check_load_or_store                  5.6s    27.5s   10.1
// 4. (made check_load, check_store4 regparm(0))          (27.9s) (11.0)
// 5. um, not sure                                 5.3s    27.3s   10.6
//    ...
// 6. after big changes, corrections              11.2s    32.8s   14.0
// 7. removed link-segment chasing in check/L/S    8.9s    30.8s   14.0
// 8. avoiding do_lea1 if k is a nonptr            8.0s    28.0s   12.9
//--------------------------------------------------------------

//#include "vg_skin.h"

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

/* Set to 1 to do sanity checks on Seg values in many places, which
   checks if bogus Segs are in circulation.  Quite expensive from a
   performance point of view. */
#define SC_SEGS 0

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

inline Bool Seg__is_freed(Seg* seg)
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

// So that post_reg_write_clientcall knows the segment just allocated.
static Seg* last_seg_added = NULL;

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

   last_seg_added = seg;

   return seg;
}

// Forward declarations
static void copy_mem( Addr from, Addr to, SizeT len );
static void set_mem_unknown ( Addr a, SizeT len );

static inline VG_REGPARM(1) Seg* nonptr_or_unknown(UWord x); /*fwds*/

static
void* alloc_and_new_mem_heap ( ThreadId tid,
                               SizeT size, SizeT alignment, Bool is_zeroed )
{
   Addr p;

   if ( ((SSizeT)size) < 0) return NULL;

   p = (Addr)VG_(cli_malloc)(alignment, size);
   if (is_zeroed) VG_(memset)((void*)p, 0, size);

   set_mem_unknown( p, size );
   add_new_segment( tid, p, size );

   stats__client_mallocs++;
   return (void*)p;
}

static void die_and_free_mem_heap ( ThreadId tid, Seg* seg )
{
   // Empty and free the actual block
   tl_assert(!Seg__is_freed(seg));
   set_mem_unknown( seg->addr, seg->szB );

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
/*--- Shadow memory                                        ---*/
/*------------------------------------------------------------*/

/* Shadow memory holds one Seg for each naturally aligned (guest)
   word.  For a 32 bit target (assuming host word size == guest word
   size) that means one Seg per 4 bytes, and each Seg occupies 4
   bytes.  For a 64 bit target that means one Seg per 8 bytes, and
   each Seg occupies 8 bytes.  Hence in each case the overall space
   overhead for shadow memory is 1:1.

   This does however make it a bit tricky to size SecMap.vseg[], simce
   it needs to hold 16384 entries for 32 bit targets but only 8192
   entries for 64 bit targets. */

#if 0
__attribute__((unused))
static void pp_curr_ExeContext(void)
{
   VG_(pp_ExeContext)(
      VG_(get_ExeContext)(
         VG_(get_current_or_recent_tid)() ) );
   VG_(message)(Vg_UserMsg, "");
}
#endif

#if defined(VGA_x86) || defined(VGA_ppc32) || defined(VGA_arm)
#  define SHMEM_SECMAP_MASK         0xFFFC
#  define SHMEM_SECMAP_SHIFT        2
#  define SHMEM_IS_WORD_ALIGNED(_a) VG_IS_4_ALIGNED(_a)
#  define SEC_MAP_WORDS             (0x10000UL / 4UL) /* 16k */
#elif defined(VGA_amd64) || defined(VGA_ppc64)
#  define SHMEM_SECMAP_MASK         0xFFF8
#  define SHMEM_SECMAP_SHIFT        3
#  define SHMEM_IS_WORD_ALIGNED(_a) VG_IS_8_ALIGNED(_a)
#  define SEC_MAP_WORDS             (0x10000UL / 8UL) /* 8k */
#else
#  error "Unknown arch"
#endif

typedef
   struct {
      Seg* vseg[SEC_MAP_WORDS];
   }
   SecMap;

static SecMap  distinguished_secondary_map;

/* An entry in the primary map.  base must be a 64k-aligned value, and
   sm points at the relevant secondary map.  The secondary may be
   either a real secondary, or the distinguished secondary.  DO NOT
   CHANGE THIS LAYOUT: the first word has to be the key for OSet fast
   lookups.
*/
typedef
   struct {
      Addr    base;
      SecMap* sm;
   }
   PriMapEnt;

/* Primary map is an OSet of PriMapEnt (primap_L2), "fronted" by a
   cache (primap_L1). */

/* Tunable parameter: How big is the L1 queue? */
#define N_PRIMAP_L1 24

/* Tunable parameter: How far along the L1 queue to insert
   entries resulting from L2 lookups? */
#define PRIMAP_L1_INSERT_IX 12

static struct {
          Addr       base; // must be 64k aligned
          PriMapEnt* ent; // pointer to the matching primap_L2 node
       }
       primap_L1[N_PRIMAP_L1];

static OSet* primap_L2 = NULL;


/* # searches initiated in auxmap_L1, and # base cmps required */
static ULong n_primap_L1_searches  = 0;
static ULong n_primap_L1_cmps      = 0;
/* # of searches that missed in auxmap_L1 and therefore had to
   be handed to auxmap_L2. And the number of nodes inserted. */
static ULong n_primap_L2_searches  = 0;
static ULong n_primap_L2_nodes     = 0;


static void init_shadow_memory ( void )
{
   Int i;

   for (i = 0; i < SEC_MAP_WORDS; i++)
      distinguished_secondary_map.vseg[i] = NONPTR;

   for (i = 0; i < N_PRIMAP_L1; i++) {
      primap_L1[i].base = 1; /* not 64k aligned, so doesn't match any
                                request ==> slot is empty */
      primap_L1[i].ent  = NULL;
   }

   tl_assert(0 == offsetof(PriMapEnt,base));
   tl_assert(sizeof(Addr) == sizeof(void*));
   primap_L2 = VG_(OSetGen_Create)( /*keyOff*/  offsetof(PriMapEnt,base),
                                    /*fastCmp*/ NULL,
                                    VG_(malloc), "pc.h_main.ism.1",
                                    VG_(free) );
   tl_assert(primap_L2);
}

static void insert_into_primap_L1_at ( Word rank, PriMapEnt* ent )
{
   Word i;
   tl_assert(ent);
   tl_assert(rank >= 0 && rank < N_PRIMAP_L1);
   for (i = N_PRIMAP_L1-1; i > rank; i--)
      primap_L1[i] = primap_L1[i-1];
   primap_L1[rank].base = ent->base;
   primap_L1[rank].ent  = ent;
}

static inline PriMapEnt* maybe_find_in_primap ( Addr a )
{
   PriMapEnt  key;
   PriMapEnt* res;
   Word       i;

   a &= ~(Addr)0xFFFF;

   /* First search the front-cache, which is a self-organising
      list containing the most popular entries. */

   if (LIKELY(primap_L1[0].base == a))
      return primap_L1[0].ent;
   if (LIKELY(primap_L1[1].base == a)) {
      Addr       t_base = primap_L1[0].base;
      PriMapEnt* t_ent  = primap_L1[0].ent;
      primap_L1[0].base = primap_L1[1].base;
      primap_L1[0].ent  = primap_L1[1].ent;
      primap_L1[1].base = t_base;
      primap_L1[1].ent  = t_ent;
      return primap_L1[0].ent;
   }

   n_primap_L1_searches++;

   for (i = 0; i < N_PRIMAP_L1; i++) {
      if (primap_L1[i].base == a) {
         break;
      }
   }
   tl_assert(i >= 0 && i <= N_PRIMAP_L1);

   n_primap_L1_cmps += (ULong)(i+1);

   if (i < N_PRIMAP_L1) {
      if (i > 0) {
         Addr       t_base = primap_L1[i-1].base;
         PriMapEnt* t_ent  = primap_L1[i-1].ent;
         primap_L1[i-1].base = primap_L1[i-0].base;
         primap_L1[i-1].ent  = primap_L1[i-0].ent;
         primap_L1[i-0].base = t_base;
         primap_L1[i-0].ent  = t_ent;
         i--;
      }
      return primap_L1[i].ent;
   }

   n_primap_L2_searches++;

   /* First see if we already have it. */
   key.base = a;
   key.sm   = 0;

   res = VG_(OSetGen_Lookup)(primap_L2, &key);
   if (res)
      insert_into_primap_L1_at( PRIMAP_L1_INSERT_IX, res );
   return res;
}

static SecMap* alloc_secondary_map ( void )
{
   SecMap* map;
   UInt  i;

   // JRS 2008-June-25: what's the following assertion for?
   tl_assert(0 == (sizeof(SecMap) % VKI_MAX_PAGE_SIZE));

   map = VG_(am_shadow_alloc)( sizeof(SecMap) );
   if (map == NULL)
      VG_(out_of_memory_NORETURN)( "annelid:allocate new SecMap",
                                   sizeof(SecMap) );

   for (i = 0; i < SEC_MAP_WORDS; i++)
      map->vseg[i] = NONPTR;
   if (0) VG_(printf)("XXX new secmap %p\n", map);
   return map;
}

static PriMapEnt* find_or_alloc_in_primap ( Addr a )
{
   PriMapEnt *nyu, *res;

   /* First see if we already have it. */
   res = maybe_find_in_primap( a );
   if (LIKELY(res))
      return res;

   /* Ok, there's no entry in the secondary map, so we'll have
      to allocate one. */
   a &= ~(Addr)0xFFFF;

   nyu = (PriMapEnt*) VG_(OSetGen_AllocNode)( 
                         primap_L2, sizeof(PriMapEnt) );
   tl_assert(nyu);
   nyu->base = a;
   nyu->sm   = alloc_secondary_map();
   tl_assert(nyu->sm);
   VG_(OSetGen_Insert)( primap_L2, nyu );
   insert_into_primap_L1_at( PRIMAP_L1_INSERT_IX, nyu );
   n_primap_L2_nodes++;
   return nyu;
}

/////////////////////////////////////////////////

// Nb: 'a' must be naturally word aligned for the host.
static inline Seg* get_mem_vseg ( Addr a )
{
   SecMap* sm     = find_or_alloc_in_primap(a)->sm;
   UWord   sm_off = (a & SHMEM_SECMAP_MASK) >> SHMEM_SECMAP_SHIFT;
   tl_assert(SHMEM_IS_WORD_ALIGNED(a));
   return sm->vseg[sm_off];
}

// Nb: 'a' must be naturally word aligned for the host.
static inline void set_mem_vseg ( Addr a, Seg* vseg )
{
   SecMap* sm     = find_or_alloc_in_primap(a)->sm;
   UWord   sm_off = (a & SHMEM_SECMAP_MASK) >> SHMEM_SECMAP_SHIFT;
   tl_assert(SHMEM_IS_WORD_ALIGNED(a));
   sm->vseg[sm_off] = vseg;
}

// Find the Seg which contains the given address.
// Returns UNKNOWN if no matches.  Never returns BOTTOM or NONPTR.
// Also, only returns in-use segments, not freed ones.
/* Doing this fast is distinctly difficult when there are more than a
   few heap allocated blocks live.  Basically it is done by searching
   addr_to_seg_map for 'a'.

   First, if 'a' is the start address of a segment, then we can detect
   that by simply doing a VG_(lookupFM) of 'a', and we are done (nice
   and easy).

   If 'a' is within some segment, but does not point to the start, it
   is much more complex.  We use VG_(findBoundsFM) to find the segment
   with the largest .addr field which is <= a, and we then inspect the
   segment to see if 'a' really falls inside it or not.  This is all a
   bit complex and fragile, and so there's a lot of assertery in the
   code below.  It has been crosschecked however against the trivial
   _SLOW implementation shown after the end of this fn.
*/
static Seg* get_Seg_containing_addr( Addr a )
{
   UWord keyW, valW;
   Seg*  s2;

   /* Since we are going to poke around in it */
   addr_to_seg_map_ENSURE_INIT();

   /* first, see if 'a' is at the start of a block.  We do this both
      because it's easy and more imporantly because VG_(findBoundsFM)
      will fail in this case, so we need to exclude it first. */
   if (VG_(lookupFM)( addr_to_seg_map, &keyW, &valW, a )) {
      tl_assert(keyW == a);
      s2 = (Seg*)valW;
      tl_assert(s2->addr == a);
   } else {
      Bool  ok;
      UWord kMin, vMin, kMax, vMax;
      Seg   minSeg;
      Seg   maxSeg;
      UWord minAddr = 0;
      UWord maxAddr = ~minAddr;
      VG_(memset)(&minSeg, 0, sizeof(minSeg));
      VG_(memset)(&maxSeg, 0, sizeof(maxSeg));
      minSeg.addr = minAddr;
      maxSeg.addr = maxAddr;
      ok = VG_(findBoundsFM)( addr_to_seg_map,
                              &kMin, &vMin, &kMax, &vMax,
                              minAddr, (UWord)&minSeg,
                              maxAddr, (UWord)&maxSeg, a );
      tl_assert(ok); /* must be so, since False is only returned when
                        'a' is directly present in the map, and we
                        just established that it isn't. */
      /* At this point, either vMin points at minSeg, or it points at a
         real Seg.  In the former case, there is no live heap-allocated
         Seg which has a start address <= a, so a is not in any block.
         In the latter case, the Seg vMin points at may or may not
         actually contain 'a'; we can only tell that by inspecting the
         Seg itself. */
      s2 = (Seg*)vMin;
      tl_assert(kMin == s2->addr);
      if (s2 == &minSeg) {
         /* the former */
         s2 = UNKNOWN;
      } else {
         /* the latter */
         tl_assert(s2->addr <= a);
         /* if s2 doesn't actually contain 'a', we must forget about it. */
         if (s2->szB == 0 /* a zero sized block can't contain anything */
             || s2->addr + s2->szB < a /* the usual range check */)
            s2 = UNKNOWN;
      }
      /* while we're at it, do as much assertery as we can, since this
         is all rather complex.  Either vMax points at maxSeg, or it
         points to a real block, which must have a start address
         greater than a. */
      tl_assert(kMax == ((Seg*)vMax)->addr);
      if (vMax == (UWord)&maxSeg) {
         /* nothing we can check */
      } else {
         tl_assert(a < kMax); /* hence also a < ((Seg*)vMax)->addr */
      }
   }

   return s2;
}

/* XXXX very slow reference implementation.  Do not use.
static Seg* get_Seg_containing_addr_SLOW( Addr a )
{
   SegGroup* group;
   UWord i;
   stats__slow_searches++;
   for (group = group_list; group; group = group->admin) {
      for (i = 0; i < group->nextfree; i++) {
         stats__slow_totcmps++;
         if (Seg__is_freed(&group->segs[i]))
            continue;
         if (group->segs[i].addr <= a
             && a < group->segs[i].addr + group->segs[i].szB)
            return &group->segs[i];
      }
   }
   return UNKNOWN;
}
*/



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

      /* Notification: copy retained part */
      copy_mem       ( (Addr)p_old, p_new, new_size );

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

      /* Notification: first half kept and copied, second half new */
      copy_mem       ( (Addr)p_old, p_new, seg->szB );
      set_mem_unknown( p_new + seg->szB, new_size - seg->szB );

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


/*------------------------------------------------------------*/
/*--- Memory events                                        ---*/
/*------------------------------------------------------------*/

static inline
void set_mem ( Addr a, SizeT len, Seg* seg )
{
   Addr end;

   if (0 == len)
      return;

   if (len > 100 * 1000 * 1000)
      VG_(message)(Vg_UserMsg,
                   "Warning: set address range state: large range %lu\n",
                   len);

   a   = VG_ROUNDDN(a,       sizeof(UWord));
   end = VG_ROUNDUP(a + len, sizeof(UWord));
   for ( ; a < end; a += sizeof(UWord))
      set_mem_vseg(a, seg);
}

static void set_mem_unknown( Addr a, SizeT len )
{
   set_mem( a, len, UNKNOWN );
}

//zz static void set_mem_nonptr( Addr a, UInt len )
//zz {
//zz    set_mem( a, len, NONPTR );
//zz }

void h_new_mem_startup( Addr a, SizeT len,
                        Bool rr, Bool ww, Bool xx, ULong di_handle )
{
   if (0) VG_(printf)("new_mem_startup(%#lx,%lu)\n", a, len);
   set_mem_unknown( a, len );
   //add_new_segment( VG_(get_running_tid)(), a, len, SegMmap );
}

//zz // XXX: Currently not doing anything with brk() -- new segments, or not?
//zz // Proper way to do it would be to grow/shrink a single, special brk segment.
//zz //
//zz // brk is difficult: it defines a single segment, of changeable size.
//zz // It starts off with size zero, at the address given by brk(0).  There are
//zz // no pointers within the program to it.  Any subsequent calls by the
//zz // program to brk() (possibly growing or shrinking it) return pointers to
//zz // the *end* of the segment (nb: this is the kernel brk(), which is
//zz // different to the libc brk()).
//zz //
//zz // If fixing this, don't forget to update the brk case in SK_(post_syscall).
//zz //
//zz // Nb: not sure if the return value is the last byte addressible, or one
//zz // past the end of the segment.
//zz //
//zz static void new_mem_brk( Addr a, UInt len )
//zz {
//zz    set_mem_unknown(a, len);
//zz    //VG_(skin_panic)("can't handle new_mem_brk");
//zz }

// Not quite right:  if you mmap a segment into a specified place, it could
// be legitimate to do certain arithmetic with the pointer that it wouldn't
// otherwise.  Hopefully this is rare, though.
void h_new_mem_mmap( Addr a, SizeT len,
                     Bool rr, Bool ww, Bool xx, ULong di_handle )
{
   if (0) VG_(printf)("new_mem_mmap(%#lx,%lu)\n", a, len);
//zz #if 0
//zz    Seg seg = NULL;
//zz 
//zz    // Check for overlapping segments
//zz #if 0
//zz    is_overlapping_seg___a   = a;    // 'free' variable
//zz    is_overlapping_seg___len = len;  // 'free' variable
//zz    seg = (Seg)VG_(HT_first_match) ( mlist, is_overlapping_seg );
//zz    is_overlapping_seg___a   = 0;    // paranoia, reset
//zz    is_overlapping_seg___len = 0;    // paranoia, reset
//zz #endif
//zz 
//zz    // XXX: do this check properly with ISLists
//zz 
//zz    if ( ISList__findI( seglist, a, &seg )) {
//zz       sk_assert(SegMmap == seg->status || SegMmapFree == seg->status);
//zz       if (SegMmap == seg->status)
//zz    
//zz    }
//zz 
//zz    if (NULL != seg) {
//zz       // Right, we found an overlap
//zz       if (VG_(clo_verbosity) > 1)
//zz          VG_(message)(Vg_UserMsg, "mmap overlap:  old: %#lx, %d;  new: %#lx, %d",
//zz                                   seg->left, Seg__size(seg), a, len);
//zz       if (seg->left <= a && a <= seg->right) {
//zz          // New one truncates end of the old one.  Nb: we don't adjust its
//zz          // size, because the first segment's pointer can be (and for
//zz          // Konqueror, is) legitimately used to access parts of the second
//zz          // segment.  At least, I assume Konqueror is doing something legal.
//zz          // so that a size mismatch upon munmap isn't a problem.
//zz //         seg->size = a - seg->data;
//zz //         seg->is_truncated_map = True;
//zz //         if (VG_(clo_verbosity) > 1)
//zz //            VG_(message)(Vg_UserMsg, "old seg truncated to length %d",
//zz //                                     seg->size);
//zz       } else {
//zz          VG_(skin_panic)("Can't handle this mmap() overlap case");
//zz       }
//zz    }
   set_mem_unknown( a, len );
   //add_new_segment( VG_(get_running_tid)(), a, len, SegMmap );
//zz #endif
}

static void copy_mem( Addr from, Addr to, SizeT len )
{
   Addr fromend = from + len;

   // Must be aligned due to malloc always returning aligned objects.
   tl_assert(VG_IS_8_ALIGNED(from) && VG_IS_8_ALIGNED(to));

   // Must only be called with positive len.
   if (0 == len)
      return;

   for ( ; from < fromend; from += sizeof(UWord), to += sizeof(UWord))
      set_mem_vseg( to, get_mem_vseg(from) );
}

//zz // Similar to SK_(realloc)()
//zz static void copy_mem_remap( Addr from, Addr to, UInt len )
//zz {
//zz    VG_(skin_panic)("argh: copy_mem_remap");
//zz }
//zz 
//zz static void die_mem_brk( Addr a, UInt len )
//zz {
//zz    set_mem_unknown(a, len);
//zz //   VG_(skin_panic)("can't handle die_mem_brk()");
//zz }

void h_die_mem_munmap( Addr a, SizeT len )
{
//   handle_free_munmap( (void*)a, len );
}

// Don't need to check all addresses within the block; in the absence of
// discontiguous segments, the segments for the first and last bytes should
// be the same.  Can't easily check the pointer segment matches the block
// segment, unfortunately, but the first/last check should catch most
// errors.
static void pre_mem_access2 ( CorePart part, ThreadId tid, Char* str,
                              Addr s/*tart*/, Addr e/*nd*/ )
{
   Seg  *seglo, *seghi;

   // Don't check code being translated -- very slow, and not much point
   if (Vg_CoreTranslate == part) return;

   // Don't check the signal case -- only happens in core, no need to check
   if (Vg_CoreSignal == part) return;

   // Only expect syscalls after this point
   if (part != Vg_CoreSysCall) {
      VG_(printf)("part = %d\n", part);
      VG_(tool_panic)("unknown corepart in pre_mem_access2");
   }

   // Check first and last bytes match
   seglo = get_Seg_containing_addr( s );
   seghi = get_Seg_containing_addr( e );
   tl_assert( BOTTOM != seglo && NONPTR != seglo );
   tl_assert( BOTTOM != seghi && NONPTR != seghi );

   /* record an error if start and end are in different, but known segments */
   if (is_known_segment(seglo) && is_known_segment(seghi)
       && seglo != seghi) {
      h_record_sysparam_error(tid, part, str, s, e, seglo, seghi);
   }
   else
   /* record an error if start is in a known segment but end isn't */
   if (is_known_segment(seglo) && !is_known_segment(seghi)) {
      h_record_sysparam_error(tid, part, str, s, e, seglo, UNKNOWN);
   }
   else
   /* record an error if end is in a known segment but start isn't */
   if (!is_known_segment(seglo) && is_known_segment(seghi)) {
      h_record_sysparam_error(tid, part, str, s, e, UNKNOWN, seghi);
   }
}

void h_pre_mem_access ( CorePart part, ThreadId tid, Char* s,
                        Addr base, SizeT size )
{
   pre_mem_access2( part, tid, s, base, base + size - 1 );
}

void h_pre_mem_read_asciiz ( CorePart part, ThreadId tid, 
                             Char* s, Addr lo )
{
   Addr hi = lo;

   // Nb: the '\0' must be included in the lo...hi range
   while ('\0' != *(Char*)hi) hi++;
   pre_mem_access2( part, tid, s, lo, hi );
}

//zz static void post_mem_write(Addr a, UInt len)
//zz {
//zz    set_mem_unknown(a, len);
//zz }


/*------------------------------------------------------------*/
/*--- Register event handlers                              ---*/
/*------------------------------------------------------------*/

//zz static void post_regs_write_init ( void )
//zz {
//zz    UInt i;
//zz    for (i = R_EAX; i <= R_EDI; i++)
//zz       VG_(set_shadow_archreg)( i, (UInt)UNKNOWN );
//zz 
//zz    // Don't bother about eflags
//zz }

// BEGIN move this uglyness to pc_machine.c

static inline Bool host_is_big_endian ( void ) {
   UInt x = 0x11223344;
   return 0x1122 == *(UShort*)(&x);
}
static inline Bool host_is_little_endian ( void ) {
   UInt x = 0x11223344;
   return 0x3344 == *(UShort*)(&x);
}

#define N_INTREGINFO_OFFSETS 4

/* Holds the result of a query to 'get_IntRegInfo'.  Valid values for
   n_offsets are:

   -1: means the queried guest state slice exactly matches
       one integer register

   0: means the queried guest state slice does not overlap any
      integer registers

   1 .. N_INTREGINFO_OFFSETS: means the queried guest state offset
      overlaps n_offsets different integer registers, and their base
      offsets are placed in the offsets array.
*/
typedef
   struct {
      Int offsets[N_INTREGINFO_OFFSETS];
      Int n_offsets;
   }
   IntRegInfo;


#if defined(VGA_x86)
# include "libvex_guest_x86.h"
# define PC_SIZEOF_GUEST_STATE sizeof(VexGuestX86State)
#endif

#if defined(VGA_amd64)
# include "libvex_guest_amd64.h"
# define PC_SIZEOF_GUEST_STATE sizeof(VexGuestAMD64State)
# define PC_OFF_FS_ZERO offsetof(VexGuestAMD64State,guest_FS_ZERO)
# define PC_SZB_FS_ZERO sizeof( ((VexGuestAMD64State*)0)->guest_FS_ZERO)
#endif

#if defined(VGA_ppc32)
# include "libvex_guest_ppc32.h"
# define PC_SIZEOF_GUEST_STATE sizeof(VexGuestPPC32State)
#endif

#if defined(VGA_ppc64)
# include "libvex_guest_ppc64.h"
# define PC_SIZEOF_GUEST_STATE sizeof(VexGuestPPC64State)
#endif

#if defined(VGA_arm)
# include "libvex_guest_arm.h"
# define PC_SIZEOF_GUEST_STATE sizeof(VexGuestARMState)
#endif


/* See description on definition of type IntRegInfo. */
static void get_IntRegInfo ( /*OUT*/IntRegInfo* iii, Int offset, Int szB )
{
   /* --------------------- x86 --------------------- */

#  if defined(VGA_x86)

#  define GOF(_fieldname) \
      (offsetof(VexGuestX86State,guest_##_fieldname))

   Int  o    = offset;
   Int  sz   = szB;
   Bool is4  = sz == 4;
   Bool is21 = sz == 2 || sz == 1;

   tl_assert(sz > 0);
   tl_assert(host_is_little_endian());

   /* Set default state to "does not intersect any int register". */
   VG_(memset)( iii, 0, sizeof(*iii) );

   /* Exact accesses to integer registers */
   if (o == GOF(EAX)     && is4) goto exactly1;
   if (o == GOF(ECX)     && is4) goto exactly1;
   if (o == GOF(EDX)     && is4) goto exactly1;
   if (o == GOF(EBX)     && is4) goto exactly1;
   if (o == GOF(ESP)     && is4) goto exactly1;
   if (o == GOF(EBP)     && is4) goto exactly1;
   if (o == GOF(ESI)     && is4) goto exactly1;
   if (o == GOF(EDI)     && is4) goto exactly1;
   if (o == GOF(EIP)     && is4) goto none;
   if (o == GOF(IP_AT_SYSCALL) && is4) goto none;
   if (o == GOF(CC_OP)   && is4) goto none;
   if (o == GOF(CC_DEP1) && is4) goto none;
   if (o == GOF(CC_DEP2) && is4) goto none;
   if (o == GOF(CC_NDEP) && is4) goto none;
   if (o == GOF(DFLAG)   && is4) goto none;
   if (o == GOF(IDFLAG)  && is4) goto none;
   if (o == GOF(ACFLAG)  && is4) goto none;

   /* Partial accesses to integer registers */
   if (o == GOF(EAX)     && is21) {         o -= 0; goto contains_o; }
   if (o == GOF(EAX)+1   && is21) { o -= 1; o -= 0; goto contains_o; }
   if (o == GOF(ECX)     && is21) {         o -= 0; goto contains_o; }
   if (o == GOF(ECX)+1   && is21) { o -= 1; o -= 0; goto contains_o; }
   if (o == GOF(EBX)     && is21) {         o -= 0; goto contains_o; }
   if (o == GOF(EBX)+1   && is21) { o -= 1; o -= 0; goto contains_o; }
   if (o == GOF(EDX)     && is21) {         o -= 0; goto contains_o; }
   if (o == GOF(EDX)+1   && is21) { o -= 1; o -= 0; goto contains_o; }
   if (o == GOF(ESI)     && is21) {         o -= 0; goto contains_o; }
   if (o == GOF(EDI)     && is21) {         o -= 0; goto contains_o; }

   /* Segment related guff */
   if (o == GOF(GS)  && sz == 2) goto none;
   if (o == GOF(LDT) && is4) goto none;
   if (o == GOF(GDT) && is4) goto none;

   /* FP admin related */
   if (o == GOF(SSEROUND) && is4) goto none;
   if (o == GOF(FPROUND)  && is4) goto none;
   if (o == GOF(EMWARN)   && is4) goto none;
   if (o == GOF(FTOP)     && is4) goto none;
   if (o == GOF(FPTAG)    && sz == 8) goto none;
   if (o == GOF(FC3210)   && is4) goto none;

   /* xmm registers, including arbitrary sub-parts */
   if (o >= GOF(XMM0) && o+sz <= GOF(XMM0)+16) goto none;
   if (o >= GOF(XMM1) && o+sz <= GOF(XMM1)+16) goto none;
   if (o >= GOF(XMM2) && o+sz <= GOF(XMM2)+16) goto none;
   if (o >= GOF(XMM3) && o+sz <= GOF(XMM3)+16) goto none;
   if (o >= GOF(XMM4) && o+sz <= GOF(XMM4)+16) goto none;
   if (o >= GOF(XMM5) && o+sz <= GOF(XMM5)+16) goto none;
   if (o >= GOF(XMM6) && o+sz <= GOF(XMM6)+16) goto none;
   if (o >= GOF(XMM7) && o+sz <= GOF(XMM7)+16) goto none;

   /* mmx/x87 registers (a bit of a kludge, since 'o' is not checked
      to be exactly equal to one of FPREG[0] .. FPREG[7]) */
   if (o >= GOF(FPREG[0]) && o < GOF(FPREG[7])+8 && sz == 8) goto none;

   /* the entire mmx/x87 register bank in one big piece */
   if (o == GOF(FPREG) && sz == 64) goto none;

   VG_(printf)("get_IntRegInfo(x86):failing on (%d,%d)\n", o, sz);
   tl_assert(0);
#  undef GOF

   /* -------------------- amd64 -------------------- */

#  elif defined(VGA_amd64)

#  define GOF(_fieldname) \
      (offsetof(VexGuestAMD64State,guest_##_fieldname))

   Int  o     = offset;
   Int  sz    = szB;
   Bool is421 = sz == 4 || sz == 2 || sz == 1;
   Bool is8   = sz == 8;

   tl_assert(sz > 0);
   tl_assert(host_is_little_endian());

   /* Set default state to "does not intersect any int register". */
   VG_(memset)( iii, 0, sizeof(*iii) );

   /* Exact accesses to integer registers */
   if (o == GOF(RAX)     && is8) goto exactly1;
   if (o == GOF(RCX)     && is8) goto exactly1;
   if (o == GOF(RDX)     && is8) goto exactly1;
   if (o == GOF(RBX)     && is8) goto exactly1;
   if (o == GOF(RSP)     && is8) goto exactly1;
   if (o == GOF(RBP)     && is8) goto exactly1;
   if (o == GOF(RSI)     && is8) goto exactly1;
   if (o == GOF(RDI)     && is8) goto exactly1;
   if (o == GOF(R8)      && is8) goto exactly1;
   if (o == GOF(R9)      && is8) goto exactly1;
   if (o == GOF(R10)     && is8) goto exactly1;
   if (o == GOF(R11)     && is8) goto exactly1;
   if (o == GOF(R12)     && is8) goto exactly1;
   if (o == GOF(R13)     && is8) goto exactly1;
   if (o == GOF(R14)     && is8) goto exactly1;
   if (o == GOF(R15)     && is8) goto exactly1;
   if (o == GOF(RIP)     && is8) goto exactly1;
   if (o == GOF(IP_AT_SYSCALL) && is8) goto none;
   if (o == GOF(CC_OP)   && is8) goto none;
   if (o == GOF(CC_DEP1) && is8) goto none;
   if (o == GOF(CC_DEP2) && is8) goto none;
   if (o == GOF(CC_NDEP) && is8) goto none;
   if (o == GOF(DFLAG)   && is8) goto none;
   if (o == GOF(IDFLAG)  && is8) goto none;

   /* Partial accesses to integer registers */
   if (o == GOF(RAX)     && is421) {         o -= 0; goto contains_o; }
   if (o == GOF(RAX)+1   && is421) { o -= 1; o -= 0; goto contains_o; }
   if (o == GOF(RCX)     && is421) {         o -= 0; goto contains_o; }
   if (o == GOF(RCX)+1   && is421) { o -= 1; o -= 0; goto contains_o; }
   if (o == GOF(RDX)     && is421) {         o -= 0; goto contains_o; }
   if (o == GOF(RDX)+1   && is421) { o -= 1; o -= 0; goto contains_o; }
   if (o == GOF(RBX)     && is421) {         o -= 0; goto contains_o; }
   if (o == GOF(RBX)+1   && is421) { o -= 1; o -= 0; goto contains_o; }
   if (o == GOF(RBP)     && is421) {         o -= 0; goto contains_o; }
   if (o == GOF(RSI)     && is421) {         o -= 0; goto contains_o; }
   if (o == GOF(RDI)     && is421) {         o -= 0; goto contains_o; }
   if (o == GOF(R8)      && is421) {         o -= 0; goto contains_o; }
   if (o == GOF(R9)      && is421) {         o -= 0; goto contains_o; }
   if (o == GOF(R10)     && is421) {         o -= 0; goto contains_o; }
   if (o == GOF(R11)     && is421) {         o -= 0; goto contains_o; }
   if (o == GOF(R12)     && is421) {         o -= 0; goto contains_o; }
   if (o == GOF(R13)     && is421) {         o -= 0; goto contains_o; }
   if (o == GOF(R14)     && is421) {         o -= 0; goto contains_o; }
   if (o == GOF(R15)     && is421) {         o -= 0; goto contains_o; }

   /* Segment related guff */
   if (o == GOF(FS_ZERO) && is8) goto exactly1;

   /* FP admin related */
   if (o == GOF(SSEROUND) && is8) goto none;
   if (o == GOF(FPROUND)  && is8) goto none;
   if (o == GOF(EMWARN)   && sz == 4) goto none;
   if (o == GOF(FTOP)     && sz == 4) goto none;
   if (o == GOF(FPTAG)    && is8) goto none;
   if (o == GOF(FC3210)   && is8) goto none;

   /* xmm registers, including arbitrary sub-parts */
   if (o >= GOF(XMM0)  && o+sz <= GOF(XMM0)+16)  goto none;
   if (o >= GOF(XMM1)  && o+sz <= GOF(XMM1)+16)  goto none;
   if (o >= GOF(XMM2)  && o+sz <= GOF(XMM2)+16)  goto none;
   if (o >= GOF(XMM3)  && o+sz <= GOF(XMM3)+16)  goto none;
   if (o >= GOF(XMM4)  && o+sz <= GOF(XMM4)+16)  goto none;
   if (o >= GOF(XMM5)  && o+sz <= GOF(XMM5)+16)  goto none;
   if (o >= GOF(XMM6)  && o+sz <= GOF(XMM6)+16)  goto none;
   if (o >= GOF(XMM7)  && o+sz <= GOF(XMM7)+16)  goto none;
   if (o >= GOF(XMM8)  && o+sz <= GOF(XMM8)+16)  goto none;
   if (o >= GOF(XMM9)  && o+sz <= GOF(XMM9)+16)  goto none;
   if (o >= GOF(XMM10) && o+sz <= GOF(XMM10)+16) goto none;
   if (o >= GOF(XMM11) && o+sz <= GOF(XMM11)+16) goto none;
   if (o >= GOF(XMM12) && o+sz <= GOF(XMM12)+16) goto none;
   if (o >= GOF(XMM13) && o+sz <= GOF(XMM13)+16) goto none;
   if (o >= GOF(XMM14) && o+sz <= GOF(XMM14)+16) goto none;
   if (o >= GOF(XMM15) && o+sz <= GOF(XMM15)+16) goto none;

   /* mmx/x87 registers (a bit of a kludge, since 'o' is not checked
      to be exactly equal to one of FPREG[0] .. FPREG[7]) */
   if (o >= GOF(FPREG[0]) && o < GOF(FPREG[7])+8 && sz == 8) goto none;

   VG_(printf)("get_IntRegInfo(amd64):failing on (%d,%d)\n", o, sz);
   tl_assert(0);
#  undef GOF

   /* -------------------- ppc32 -------------------- */

#  elif defined(VGA_ppc32)

#  define GOF(_fieldname) \
      (offsetof(VexGuestPPC32State,guest_##_fieldname))

   Int  o    = offset;
   Int  sz   = szB;
   Bool is4  = sz == 4;
   Bool is8  = sz == 8;

   tl_assert(sz > 0);
   tl_assert(host_is_big_endian());

   /* Set default state to "does not intersect any int register". */
   VG_(memset)( iii, 0, sizeof(*iii) );

   /* Exact accesses to integer registers */
   if (o == GOF(GPR0)  && is4) goto exactly1;
   if (o == GOF(GPR1)  && is4) goto exactly1;
   if (o == GOF(GPR2)  && is4) goto exactly1;
   if (o == GOF(GPR3)  && is4) goto exactly1;
   if (o == GOF(GPR4)  && is4) goto exactly1;
   if (o == GOF(GPR5)  && is4) goto exactly1;
   if (o == GOF(GPR6)  && is4) goto exactly1;
   if (o == GOF(GPR7)  && is4) goto exactly1;
   if (o == GOF(GPR8)  && is4) goto exactly1;
   if (o == GOF(GPR9)  && is4) goto exactly1;
   if (o == GOF(GPR10) && is4) goto exactly1;
   if (o == GOF(GPR11) && is4) goto exactly1;
   if (o == GOF(GPR12) && is4) goto exactly1;
   if (o == GOF(GPR13) && is4) goto exactly1;
   if (o == GOF(GPR14) && is4) goto exactly1;
   if (o == GOF(GPR15) && is4) goto exactly1;
   if (o == GOF(GPR16) && is4) goto exactly1;
   if (o == GOF(GPR17) && is4) goto exactly1;
   if (o == GOF(GPR18) && is4) goto exactly1;
   if (o == GOF(GPR19) && is4) goto exactly1;
   if (o == GOF(GPR20) && is4) goto exactly1;
   if (o == GOF(GPR21) && is4) goto exactly1;
   if (o == GOF(GPR22) && is4) goto exactly1;
   if (o == GOF(GPR23) && is4) goto exactly1;
   if (o == GOF(GPR24) && is4) goto exactly1;
   if (o == GOF(GPR25) && is4) goto exactly1;
   if (o == GOF(GPR26) && is4) goto exactly1;
   if (o == GOF(GPR27) && is4) goto exactly1;
   if (o == GOF(GPR28) && is4) goto exactly1;
   if (o == GOF(GPR29) && is4) goto exactly1;
   if (o == GOF(GPR30) && is4) goto exactly1;
   if (o == GOF(GPR31) && is4) goto exactly1;

   /* Misc integer reg and condition code accesses */
   if (o == GOF(LR)        && is4) goto exactly1;
   if (o == GOF(CTR)       && is4) goto exactly1;
   if (o == GOF(CIA)       && is4) goto none;
   if (o == GOF(IP_AT_SYSCALL) && is4) goto none;
   if (o == GOF(TISTART)   && is4) goto none;
   if (o == GOF(TILEN)     && is4) goto none;
   if (o == GOF(REDIR_SP)  && is4) goto none;

   if (sz == 1) {
      if (o == GOF(XER_SO))  goto none;
      if (o == GOF(XER_OV))  goto none;
      if (o == GOF(XER_CA))  goto none;
      if (o == GOF(XER_BC))  goto none;
      if (o == GOF(CR0_321)) goto none;
      if (o == GOF(CR0_0))   goto none;
      if (o == GOF(CR1_321)) goto none;
      if (o == GOF(CR1_0))   goto none;
      if (o == GOF(CR2_321)) goto none;
      if (o == GOF(CR2_0))   goto none;
      if (o == GOF(CR3_321)) goto none;
      if (o == GOF(CR3_0))   goto none;
      if (o == GOF(CR4_321)) goto none;
      if (o == GOF(CR4_0))   goto none;
      if (o == GOF(CR5_321)) goto none;
      if (o == GOF(CR5_0))   goto none;
      if (o == GOF(CR6_321)) goto none;
      if (o == GOF(CR6_0))   goto none;
      if (o == GOF(CR7_321)) goto none;
      if (o == GOF(CR7_0))   goto none;
   }

   /* Exact accesses to FP registers */
   if (o == GOF(FPR0)  && is8) goto none;
   if (o == GOF(FPR1)  && is8) goto none;
   if (o == GOF(FPR2)  && is8) goto none;
   if (o == GOF(FPR3)  && is8) goto none;
   if (o == GOF(FPR4)  && is8) goto none;
   if (o == GOF(FPR5)  && is8) goto none;
   if (o == GOF(FPR6)  && is8) goto none;
   if (o == GOF(FPR7)  && is8) goto none;
   if (o == GOF(FPR8)  && is8) goto none;
   if (o == GOF(FPR9)  && is8) goto none;
   if (o == GOF(FPR10) && is8) goto none;
   if (o == GOF(FPR11) && is8) goto none;
   if (o == GOF(FPR12) && is8) goto none;
   if (o == GOF(FPR13) && is8) goto none;
   if (o == GOF(FPR14) && is8) goto none;
   if (o == GOF(FPR15) && is8) goto none;
   if (o == GOF(FPR16) && is8) goto none;
   if (o == GOF(FPR17) && is8) goto none;
   if (o == GOF(FPR18) && is8) goto none;
   if (o == GOF(FPR19) && is8) goto none;
   if (o == GOF(FPR20) && is8) goto none;
   if (o == GOF(FPR21) && is8) goto none;
   if (o == GOF(FPR22) && is8) goto none;
   if (o == GOF(FPR23) && is8) goto none;
   if (o == GOF(FPR24) && is8) goto none;
   if (o == GOF(FPR25) && is8) goto none;
   if (o == GOF(FPR26) && is8) goto none;
   if (o == GOF(FPR27) && is8) goto none;
   if (o == GOF(FPR28) && is8) goto none;
   if (o == GOF(FPR29) && is8) goto none;
   if (o == GOF(FPR30) && is8) goto none;
   if (o == GOF(FPR31) && is8) goto none;

   /* FP admin related */
   if (o == GOF(FPROUND) && is4) goto none;
   if (o == GOF(EMWARN)  && is4) goto none;

   /* Altivec registers */
   if (o == GOF(VR0)  && sz == 16) goto none;
   if (o == GOF(VR1)  && sz == 16) goto none;
   if (o == GOF(VR2)  && sz == 16) goto none;
   if (o == GOF(VR3)  && sz == 16) goto none;
   if (o == GOF(VR4)  && sz == 16) goto none;
   if (o == GOF(VR5)  && sz == 16) goto none;
   if (o == GOF(VR6)  && sz == 16) goto none;
   if (o == GOF(VR7)  && sz == 16) goto none;
   if (o == GOF(VR8)  && sz == 16) goto none;
   if (o == GOF(VR9)  && sz == 16) goto none;
   if (o == GOF(VR10) && sz == 16) goto none;
   if (o == GOF(VR11) && sz == 16) goto none;
   if (o == GOF(VR12) && sz == 16) goto none;
   if (o == GOF(VR13) && sz == 16) goto none;
   if (o == GOF(VR14) && sz == 16) goto none;
   if (o == GOF(VR15) && sz == 16) goto none;
   if (o == GOF(VR16) && sz == 16) goto none;
   if (o == GOF(VR17) && sz == 16) goto none;
   if (o == GOF(VR18) && sz == 16) goto none;
   if (o == GOF(VR19) && sz == 16) goto none;
   if (o == GOF(VR20) && sz == 16) goto none;
   if (o == GOF(VR21) && sz == 16) goto none;
   if (o == GOF(VR22) && sz == 16) goto none;
   if (o == GOF(VR23) && sz == 16) goto none;
   if (o == GOF(VR24) && sz == 16) goto none;
   if (o == GOF(VR25) && sz == 16) goto none;
   if (o == GOF(VR26) && sz == 16) goto none;
   if (o == GOF(VR27) && sz == 16) goto none;
   if (o == GOF(VR28) && sz == 16) goto none;
   if (o == GOF(VR29) && sz == 16) goto none;
   if (o == GOF(VR30) && sz == 16) goto none;
   if (o == GOF(VR31) && sz == 16) goto none;

   /* Altivec admin related */
   if (o == GOF(VRSAVE) && is4) goto none;

   VG_(printf)("get_IntRegInfo(ppc32):failing on (%d,%d)\n", o, sz);
   tl_assert(0);
#  undef GOF

   /* -------------------- ppc64 -------------------- */

#  elif defined(VGA_ppc64)

#  define GOF(_fieldname) \
      (offsetof(VexGuestPPC64State,guest_##_fieldname))

   Int  o    = offset;
   Int  sz   = szB;
   Bool is4  = sz == 4;
   Bool is8  = sz == 8;

   tl_assert(sz > 0);
   tl_assert(host_is_big_endian());

   /* Set default state to "does not intersect any int register". */
   VG_(memset)( iii, 0, sizeof(*iii) );

   /* Exact accesses to integer registers */
   if (o == GOF(GPR0)  && is8) goto exactly1;
   if (o == GOF(GPR1)  && is8) goto exactly1;
   if (o == GOF(GPR2)  && is8) goto exactly1;
   if (o == GOF(GPR3)  && is8) goto exactly1;
   if (o == GOF(GPR4)  && is8) goto exactly1;
   if (o == GOF(GPR5)  && is8) goto exactly1;
   if (o == GOF(GPR6)  && is8) goto exactly1;
   if (o == GOF(GPR7)  && is8) goto exactly1;
   if (o == GOF(GPR8)  && is8) goto exactly1;
   if (o == GOF(GPR9)  && is8) goto exactly1;
   if (o == GOF(GPR10) && is8) goto exactly1;
   if (o == GOF(GPR11) && is8) goto exactly1;
   if (o == GOF(GPR12) && is8) goto exactly1;
   if (o == GOF(GPR13) && is8) goto exactly1;
   if (o == GOF(GPR14) && is8) goto exactly1;
   if (o == GOF(GPR15) && is8) goto exactly1;
   if (o == GOF(GPR16) && is8) goto exactly1;
   if (o == GOF(GPR17) && is8) goto exactly1;
   if (o == GOF(GPR18) && is8) goto exactly1;
   if (o == GOF(GPR19) && is8) goto exactly1;
   if (o == GOF(GPR20) && is8) goto exactly1;
   if (o == GOF(GPR21) && is8) goto exactly1;
   if (o == GOF(GPR22) && is8) goto exactly1;
   if (o == GOF(GPR23) && is8) goto exactly1;
   if (o == GOF(GPR24) && is8) goto exactly1;
   if (o == GOF(GPR25) && is8) goto exactly1;
   if (o == GOF(GPR26) && is8) goto exactly1;
   if (o == GOF(GPR27) && is8) goto exactly1;
   if (o == GOF(GPR28) && is8) goto exactly1;
   if (o == GOF(GPR29) && is8) goto exactly1;
   if (o == GOF(GPR30) && is8) goto exactly1;
   if (o == GOF(GPR31) && is8) goto exactly1;

   /* Misc integer reg and condition code accesses */
   if (o == GOF(LR)        && is8) goto exactly1;
   if (o == GOF(CTR)       && is8) goto exactly1;
   if (o == GOF(CIA)       && is8) goto none;
   if (o == GOF(IP_AT_SYSCALL) && is8) goto none;
   if (o == GOF(TISTART)   && is8) goto none;
   if (o == GOF(TILEN)     && is8) goto none;
   if (o == GOF(REDIR_SP)  && is8) goto none;

   if (sz == 1) {
      if (o == GOF(XER_SO))  goto none;
      if (o == GOF(XER_OV))  goto none;
      if (o == GOF(XER_CA))  goto none;
      if (o == GOF(XER_BC))  goto none;
      if (o == GOF(CR0_321)) goto none;
      if (o == GOF(CR0_0))   goto none;
      if (o == GOF(CR1_321)) goto none;
      if (o == GOF(CR1_0))   goto none;
      if (o == GOF(CR2_321)) goto none;
      if (o == GOF(CR2_0))   goto none;
      if (o == GOF(CR3_321)) goto none;
      if (o == GOF(CR3_0))   goto none;
      if (o == GOF(CR4_321)) goto none;
      if (o == GOF(CR4_0))   goto none;
      if (o == GOF(CR5_321)) goto none;
      if (o == GOF(CR5_0))   goto none;
      if (o == GOF(CR6_321)) goto none;
      if (o == GOF(CR6_0))   goto none;
      if (o == GOF(CR7_321)) goto none;
      if (o == GOF(CR7_0))   goto none;
   }

   /* Exact accesses to FP registers */
   if (o == GOF(FPR0)  && is8) goto none;
   if (o == GOF(FPR1)  && is8) goto none;
   if (o == GOF(FPR2)  && is8) goto none;
   if (o == GOF(FPR3)  && is8) goto none;
   if (o == GOF(FPR4)  && is8) goto none;
   if (o == GOF(FPR5)  && is8) goto none;
   if (o == GOF(FPR6)  && is8) goto none;
   if (o == GOF(FPR7)  && is8) goto none;
   if (o == GOF(FPR8)  && is8) goto none;
   if (o == GOF(FPR9)  && is8) goto none;
   if (o == GOF(FPR10) && is8) goto none;
   if (o == GOF(FPR11) && is8) goto none;
   if (o == GOF(FPR12) && is8) goto none;
   if (o == GOF(FPR13) && is8) goto none;
   if (o == GOF(FPR14) && is8) goto none;
   if (o == GOF(FPR15) && is8) goto none;
   if (o == GOF(FPR16) && is8) goto none;
   if (o == GOF(FPR17) && is8) goto none;
   if (o == GOF(FPR18) && is8) goto none;
   if (o == GOF(FPR19) && is8) goto none;
   if (o == GOF(FPR20) && is8) goto none;
   if (o == GOF(FPR21) && is8) goto none;
   if (o == GOF(FPR22) && is8) goto none;
   if (o == GOF(FPR23) && is8) goto none;
   if (o == GOF(FPR24) && is8) goto none;
   if (o == GOF(FPR25) && is8) goto none;
   if (o == GOF(FPR26) && is8) goto none;
   if (o == GOF(FPR27) && is8) goto none;
   if (o == GOF(FPR28) && is8) goto none;
   if (o == GOF(FPR29) && is8) goto none;
   if (o == GOF(FPR30) && is8) goto none;
   if (o == GOF(FPR31) && is8) goto none;

   /* FP admin related */
   if (o == GOF(FPROUND) && is4) goto none;
   if (o == GOF(EMWARN)  && is4) goto none;

   /* Altivec registers */
   if (o == GOF(VR0)  && sz == 16) goto none;
   if (o == GOF(VR1)  && sz == 16) goto none;
   if (o == GOF(VR2)  && sz == 16) goto none;
   if (o == GOF(VR3)  && sz == 16) goto none;
   if (o == GOF(VR4)  && sz == 16) goto none;
   if (o == GOF(VR5)  && sz == 16) goto none;
   if (o == GOF(VR6)  && sz == 16) goto none;
   if (o == GOF(VR7)  && sz == 16) goto none;
   if (o == GOF(VR8)  && sz == 16) goto none;
   if (o == GOF(VR9)  && sz == 16) goto none;
   if (o == GOF(VR10) && sz == 16) goto none;
   if (o == GOF(VR11) && sz == 16) goto none;
   if (o == GOF(VR12) && sz == 16) goto none;
   if (o == GOF(VR13) && sz == 16) goto none;
   if (o == GOF(VR14) && sz == 16) goto none;
   if (o == GOF(VR15) && sz == 16) goto none;
   if (o == GOF(VR16) && sz == 16) goto none;
   if (o == GOF(VR17) && sz == 16) goto none;
   if (o == GOF(VR18) && sz == 16) goto none;
   if (o == GOF(VR19) && sz == 16) goto none;
   if (o == GOF(VR20) && sz == 16) goto none;
   if (o == GOF(VR21) && sz == 16) goto none;
   if (o == GOF(VR22) && sz == 16) goto none;
   if (o == GOF(VR23) && sz == 16) goto none;
   if (o == GOF(VR24) && sz == 16) goto none;
   if (o == GOF(VR25) && sz == 16) goto none;
   if (o == GOF(VR26) && sz == 16) goto none;
   if (o == GOF(VR27) && sz == 16) goto none;
   if (o == GOF(VR28) && sz == 16) goto none;
   if (o == GOF(VR29) && sz == 16) goto none;
   if (o == GOF(VR30) && sz == 16) goto none;
   if (o == GOF(VR31) && sz == 16) goto none;

   /* Altivec admin related */
   if (o == GOF(VRSAVE) && is4) goto none;

   VG_(printf)("get_IntRegInfo(ppc64):failing on (%d,%d)\n", o, sz);
   tl_assert(0);
#  undef GOF

   /* -------------------- arm -------------------- */

#  elif defined(VGA_arm)

#  define GOF(_fieldname) \
      (offsetof(VexGuestARMState,guest_##_fieldname))

   Int  o    = offset;
   Int  sz   = szB;
   Bool is4  = sz == 4;
   Bool is8  = sz == 8;

   tl_assert(sz > 0);
   tl_assert(host_is_big_endian());

   /* Set default state to "does not intersect any int register". */
   VG_(memset)( iii, 0, sizeof(*iii) );

   VG_(printf)("get_IntRegInfo(arm):failing on (%d,%d)\n", o, sz);
   tl_assert(0);


#  else
#    error "FIXME: not implemented for this architecture"
#  endif

  exactly1:
   iii->n_offsets = -1;
   return;
  none:
   iii->n_offsets = 0;
   return;
  contains_o:
   tl_assert(o >= 0 && 0 == (o % sizeof(UWord)));
   iii->n_offsets = 1;
   iii->offsets[0] = o;
   return;
}


/* Does 'arr' describe an indexed guest state section containing host
   words, that we want to shadow? */

static Bool is_integer_guest_reg_array ( IRRegArray* arr )
{
   /* --------------------- x86 --------------------- */
#  if defined(VGA_x86)
   /* The x87 tag array. */
   if (arr->base == offsetof(VexGuestX86State,guest_FPTAG[0])
       && arr->elemTy == Ity_I8 && arr->nElems == 8)
      return False;
   /* The x87 register array. */
   if (arr->base == offsetof(VexGuestX86State,guest_FPREG[0])
       && arr->elemTy == Ity_F64 && arr->nElems == 8)
      return False;

   VG_(printf)("is_integer_guest_reg_array(x86): unhandled: ");
   ppIRRegArray(arr);
   VG_(printf)("\n");
   tl_assert(0);

   /* -------------------- amd64 -------------------- */
#  elif defined(VGA_amd64)
   /* The x87 tag array. */
   if (arr->base == offsetof(VexGuestAMD64State,guest_FPTAG[0])
       && arr->elemTy == Ity_I8 && arr->nElems == 8)
      return False;
   /* The x87 register array. */
   if (arr->base == offsetof(VexGuestAMD64State,guest_FPREG[0])
       && arr->elemTy == Ity_F64 && arr->nElems == 8)
      return False;

   VG_(printf)("is_integer_guest_reg_array(amd64): unhandled: ");
   ppIRRegArray(arr);
   VG_(printf)("\n");
   tl_assert(0);

   /* -------------------- ppc32 -------------------- */
#  elif defined(VGA_ppc32)
   /* The redir stack. */
   if (arr->base == offsetof(VexGuestPPC32State,guest_REDIR_STACK[0])
       && arr->elemTy == Ity_I32
       && arr->nElems == VEX_GUEST_PPC32_REDIR_STACK_SIZE)
      return True;

   VG_(printf)("is_integer_guest_reg_array(ppc32): unhandled: ");
   ppIRRegArray(arr);
   VG_(printf)("\n");
   tl_assert(0);

   /* -------------------- ppc64 -------------------- */
#  elif defined(VGA_ppc64)
   /* The redir stack. */
   if (arr->base == offsetof(VexGuestPPC64State,guest_REDIR_STACK[0])
       && arr->elemTy == Ity_I64
       && arr->nElems == VEX_GUEST_PPC64_REDIR_STACK_SIZE)
      return True;

   VG_(printf)("is_integer_guest_reg_array(ppc64): unhandled: ");
   ppIRRegArray(arr);
   VG_(printf)("\n");
   tl_assert(0);

   /* -------------------- arm -------------------- */
#  elif defined(VGA_arm)
   /* There are no rotating register sections on ARM. */
   VG_(printf)("is_integer_guest_reg_array(arm): unhandled: ");
   ppIRRegArray(arr);
   VG_(printf)("\n");
   tl_assert(0);

#  else
#    error "FIXME: not implemented for this architecture"
#  endif
}


// END move this uglyness to pc_machine.c

/* returns True iff given slice exactly matches an int reg.  Merely
   a convenience wrapper around get_IntRegInfo. */
static Bool is_integer_guest_reg ( Int offset, Int szB )
{
   IntRegInfo iii;
   get_IntRegInfo( &iii, offset, szB );
   tl_assert(iii.n_offsets >= -1 && iii.n_offsets <= N_INTREGINFO_OFFSETS);
   return iii.n_offsets == -1;
}

/* these assume guest and host have the same endianness and
   word size (probably). */
static UWord get_guest_intreg ( ThreadId tid, Int shadowNo,
                                PtrdiffT offset, SizeT size )
{
   UChar tmp[ 2 + sizeof(UWord) ];
   tl_assert(size == sizeof(UWord));
   tl_assert(0 == (offset % sizeof(UWord)));
   VG_(memset)(tmp, 0, sizeof(tmp));
   tmp[0] = 0x31;
   tmp[ sizeof(tmp)-1 ] = 0x27;
   VG_(get_shadow_regs_area)(tid, &tmp[1], shadowNo, offset, size);
   tl_assert(tmp[0] == 0x31);
   tl_assert(tmp[ sizeof(tmp)-1 ] == 0x27);
   return * ((UWord*) &tmp[1] ); /* MISALIGNED LOAD */
}
static void put_guest_intreg ( ThreadId tid, Int shadowNo,
                               PtrdiffT offset, SizeT size, UWord w )
{
   tl_assert(size == sizeof(UWord));
   tl_assert(0 == (offset % sizeof(UWord)));
   VG_(set_shadow_regs_area)(tid, shadowNo, offset, size,
                             (const UChar*)&w);
}

/* Initialise the integer shadow registers to UNKNOWN.  This is a bit
   of a nasty kludge, but it does mean we don't need to know which
   registers we really need to initialise -- simply assume that all
   integer registers will be naturally aligned w.r.t. the start of the
   guest state, and fill in all possible entries. */
static void init_shadow_registers ( ThreadId tid )
{
   Int i, wordSzB = sizeof(UWord);
   for (i = 0; i < PC_SIZEOF_GUEST_STATE-wordSzB; i += wordSzB) {
      put_guest_intreg( tid, 1, i, wordSzB, (UWord)UNKNOWN );
   }
}

static void post_reg_write_nonptr ( ThreadId tid, PtrdiffT offset, SizeT size )
{
   // syscall_return: Default is non-pointer.  If it really is a pointer
   // (eg. for mmap()), SK_(post_syscall) sets it again afterwards.
   //
   // clientreq_return: All the global client requests return non-pointers
   // (except possibly CLIENT_CALL[0123], but they're handled by
   // post_reg_write_clientcall, not here).
   //
   if (is_integer_guest_reg( (Int)offset, (Int)size )) {
      put_guest_intreg( tid, 1, offset, size, (UWord)NONPTR );
   } 
   else 
   if (size == 1 || size == 2) {
      /* can't possibly be an integer guest reg.  Ignore. */
   }
   else {
      // DDD: on Darwin, this assertion fails because we currently do a
      // 'post_reg_write' on the 'guest_CC_DEP1' pseudo-register.
      // JRS 2009July13: we should change is_integer_guest_reg()
      // to accept guest_CC_DEP* and guest_CC_NDEP
      // as legitimate pointer-holding registers
      tl_assert(0);
   }
   //   VG_(set_thread_shadow_archreg)( tid, reg, (UInt)NONPTR );
}

static void post_reg_write_nonptr_or_unknown ( ThreadId tid,
                                               PtrdiffT offset, SizeT size )
{
   // deliver_signal: called from two places; one sets the reg to zero, the
   // other sets the stack pointer.
   //
   if (is_integer_guest_reg( (Int)offset, (Int)size )) {
      put_guest_intreg(
         tid, 1/*shadowno*/, offset, size,
         (UWord)nonptr_or_unknown( 
                   get_guest_intreg( tid, 0/*shadowno*/,
                                     offset, size )));
   } else {
      tl_assert(0);
   }
}

void h_post_reg_write_demux ( CorePart part, ThreadId tid,
                              PtrdiffT guest_state_offset, SizeT size)
{
   if (0)
   VG_(printf)("post_reg_write_demux: tid %d part %d off %ld size %ld\n",
               (Int)tid, (Int)part,
              guest_state_offset, size);
   switch (part) {
      case Vg_CoreStartup:
         /* This is a bit of a kludge since for any Vg_CoreStartup
            event we overwrite the entire shadow register set.  But
            that's ok - we're only called once with
            part==Vg_CoreStartup event, and in that case the supplied
            offset & size cover the entire guest state anyway. */
         init_shadow_registers(tid);
         break;
      case Vg_CoreSysCall:
         if (0) VG_(printf)("ZZZZZZZ p_r_w    -> NONPTR\n");
         post_reg_write_nonptr( tid, guest_state_offset, size );
         break;
      case Vg_CoreClientReq:
         post_reg_write_nonptr( tid, guest_state_offset, size );
         break;
      case Vg_CoreSignal:
         post_reg_write_nonptr_or_unknown( tid, guest_state_offset, size );
         break;
      default:
         tl_assert(0);
   }
}

void h_post_reg_write_clientcall(ThreadId tid, PtrdiffT guest_state_offset,
                                 SizeT size, Addr f )
{
   UWord p;

   // Having to do this is a bit nasty...
   if (f == (Addr)h_replace_malloc
       || f == (Addr)h_replace___builtin_new
       || f == (Addr)h_replace___builtin_vec_new
       || f == (Addr)h_replace_calloc
       || f == (Addr)h_replace_memalign
       || f == (Addr)h_replace_realloc)
   {
      // We remembered the last added segment;  make sure it's the right one.
      /* What's going on: at this point, the scheduler has just called
         'f' -- one of our malloc replacement functions -- and it has
         returned.  The return value has been written to the guest
         state of thread 'tid', offset 'guest_state_offset' length
         'size'.  We need to look at that return value and set the
         shadow return value accordingly.  The shadow return value
         required is handed to us "under the counter" through the
         global variable 'last_seg_added'.  This is all very ugly, not
         to mention, non-thread-safe should V ever become
         multithreaded. */
      /* assert the place where the return value is is a legit int reg */
      tl_assert(is_integer_guest_reg(guest_state_offset, size));
      /* Now we need to look at the returned value, to see whether the
         malloc succeeded or not. */
      p = get_guest_intreg(tid, 0/*non-shadow*/, guest_state_offset, size);
      if ((UWord)NULL == p) {
         // if alloc failed, eg. realloc on bogus pointer
         put_guest_intreg(tid, 1/*first-shadow*/,
                          guest_state_offset, size, (UWord)NONPTR );
      } else {
         // alloc didn't fail.  Check we have the correct segment.
         tl_assert(p == last_seg_added->addr);
         put_guest_intreg(tid, 1/*first-shadow*/,
                          guest_state_offset, size, (UWord)last_seg_added );
      }
   } 
   else if (f == (Addr)h_replace_free
            || f == (Addr)h_replace___builtin_delete
            || f == (Addr)h_replace___builtin_vec_delete
         // || f == (Addr)VG_(cli_block_size)
            || f == (Addr)VG_(message))
   {
      // Probably best to set the (non-existent!) return value to
      // non-pointer.
      tl_assert(is_integer_guest_reg(guest_state_offset, size));
      put_guest_intreg(tid, 1/*first-shadow*/,
                       guest_state_offset, size, (UWord)NONPTR );
   }
   else {
      // Anything else, probably best to set return value to non-pointer.
      //VG_(set_thread_shadow_archreg)(tid, reg, (UInt)UNKNOWN);
      Char fbuf[100];
      VG_(printf)("f = %#lx\n", f);
      VG_(get_fnname)(f, fbuf, 100);
      VG_(printf)("name = %s\n", fbuf);
      VG_(tool_panic)("argh: clientcall");
   }
}


//zz /*--------------------------------------------------------------------*/
//zz /*--- Sanity checking                                              ---*/
//zz /*--------------------------------------------------------------------*/
//zz 
//zz /* Check that nobody has spuriously claimed that the first or last 16
//zz    pages (64 KB) of address space have become accessible.  Failure of
//zz    the following do not per se indicate an internal consistency
//zz    problem, but they are so likely to that we really want to know
//zz    about it if so. */
//zz Bool pc_replace_cheap_sanity_check) ( void )
//zz {
//zz    if (IS_DISTINGUISHED_SM(primary_map[0])
//zz        /* kludge: kernel drops a page up at top of address range for
//zz           magic "optimized syscalls", so we can no longer check the
//zz           highest page */
//zz        /* && IS_DISTINGUISHED_SM(primary_map[65535]) */
//zz       )
//zz       return True;
//zz    else
//zz       return False;
//zz }
//zz 
//zz Bool SK_(expensive_sanity_check) ( void )
//zz {
//zz    Int i;
//zz 
//zz    /* Make sure nobody changed the distinguished secondary. */
//zz    for (i = 0; i < SEC_MAP_WORDS; i++)
//zz       if (distinguished_secondary_map.vseg[i] != UNKNOWN)
//zz          return False;
//zz 
//zz    return True;
//zz }


/*--------------------------------------------------------------------*/
/*--- System calls                                                 ---*/
/*--------------------------------------------------------------------*/

void h_pre_syscall ( ThreadId tid, UInt sysno,
                     UWord* args, UInt nArgs )
{
   /* we don't do anything at the pre-syscall point */
}

/* The post-syscall table is a table of pairs (number, flag).

   'flag' is only ever zero or one.  If it is zero, it indicates that
   default handling for that syscall is required -- namely that the
   syscall is deemed to return NONPTR.  This is the case for the vast
   majority of syscalls.  If it is one then some special
   syscall-specific handling is is required.  No further details of it
   are stored in the table.

   On Linux and Darwin, 'number' is a __NR_xxx constant.

   On AIX5, 'number' is an Int*, which points to the Int variable
   holding the currently assigned number for this syscall.

   When querying the table, we compare the supplied syscall number
   with the 'number' field (directly on Linux and Darwin, after
   dereferencing on AIX5), to find the relevant entry.  This requires a
   linear search of the table.  To stop the costs getting too high, the
   table is incrementally rearranged after each search, to move commonly
   requested items a bit closer to the front.

   The table is built once, the first time it is used.  After that we
   merely query it (and reorder the entries as a result). */

static XArray* /* of UWordPair */ post_syscall_table = NULL;

static void setup_post_syscall_table ( void )
{
   tl_assert(!post_syscall_table);
   post_syscall_table = VG_(newXA)( VG_(malloc), "pc.h_main.spst.1",
                                    VG_(free), sizeof(UWordPair) );
   tl_assert(post_syscall_table);

   /* --------------- LINUX --------------- */

#  if defined(VGO_linux)

#     define ADD(_flag, _syscallname) \
         do { UWordPair p; p.uw1 = (_syscallname); p.uw2 = (_flag); \
              VG_(addToXA)( post_syscall_table, &p ); \
         } while (0)

      /* These ones definitely don't return pointers.  They're not
         particularly grammatical, either. */

#     if defined(__NR__llseek)
      ADD(0, __NR__llseek);
#     endif
      ADD(0, __NR__sysctl);
#     if defined(__NR__newselect)
      ADD(0, __NR__newselect);
#     endif
#     if defined(__NR_accept)
      ADD(0, __NR_accept);
#     endif
      ADD(0, __NR_access);
      ADD(0, __NR_alarm);
#     if defined(__NR_bind)
      ADD(0, __NR_bind);
#     endif
#     if defined(__NR_chdir)
      ADD(0, __NR_chdir);
#     endif
      ADD(0, __NR_chmod);
      ADD(0, __NR_chown);
#     if defined(__NR_chown32)
      ADD(0, __NR_chown32);
#     endif
      ADD(0, __NR_clock_getres);
      ADD(0, __NR_clock_gettime);
      ADD(0, __NR_clone);
      ADD(0, __NR_close);
#     if defined(__NR_connect)
      ADD(0, __NR_connect);
#     endif
      ADD(0, __NR_creat);
      ADD(0, __NR_dup);
      ADD(0, __NR_dup2);
      ADD(0, __NR_epoll_create);
#     if defined(__NR_epoll_create1)
      ADD(0, __NR_epoll_create1);
#     endif
      ADD(0, __NR_epoll_ctl);
#     if defined(__NR_epoll_pwait)
      ADD(0, __NR_epoll_pwait);
#     endif
      ADD(0, __NR_epoll_wait);
      ADD(0, __NR_execve); /* presumably we see this because the call failed? */
      ADD(0, __NR_exit); /* hmm, why are we still alive? */
      ADD(0, __NR_exit_group);
      ADD(0, __NR_fadvise64);
      ADD(0, __NR_fallocate);
      ADD(0, __NR_fchmod);
      ADD(0, __NR_fchown);
#     if defined(__NR_fchown32)
      ADD(0, __NR_fchown32);
#     endif
      ADD(0, __NR_fcntl);
#     if defined(__NR_fcntl64)
      ADD(0, __NR_fcntl64);
#     endif
      ADD(0, __NR_fdatasync);
      ADD(0, __NR_flock);
      ADD(0, __NR_fstat);
#     if defined(__NR_fstat64)
      ADD(0, __NR_fstat64);
#     endif
      ADD(0, __NR_fstatfs);
 #     if defined(__NR_fstatfs64)
      ADD(0, __NR_fstatfs64);
#     endif
     ADD(0, __NR_fsync);
      ADD(0, __NR_ftruncate);
#     if defined(__NR_ftruncate64)
      ADD(0, __NR_ftruncate64);
#     endif
      ADD(0, __NR_futex);
      ADD(0, __NR_getcwd);
      ADD(0, __NR_getdents); // something to do with teeth
      ADD(0, __NR_getdents64);
      ADD(0, __NR_getegid);
#     if defined(__NR_getegid32)
      ADD(0, __NR_getegid32);
#     endif
      ADD(0, __NR_geteuid);
#     if defined(__NR_geteuid32)
      ADD(0, __NR_geteuid32);
#     endif
      ADD(0, __NR_getgid);
#     if defined(__NR_getgid32)
      ADD(0, __NR_getgid32);
#     endif
      ADD(0, __NR_getgroups);
#     if defined(__NR_getgroups32)
      ADD(0, __NR_getgroups32);
#     endif
      ADD(0, __NR_getitimer);
#     if defined(__NR_getpeername)
      ADD(0, __NR_getpeername);
#     endif
      ADD(0, __NR_getpid);
      ADD(0, __NR_getpgrp);
      ADD(0, __NR_getppid);
      ADD(0, __NR_getpriority);
      ADD(0, __NR_getresgid);
#     if defined(__NR_getresgid32)
      ADD(0, __NR_getresgid32);
#     endif
      ADD(0, __NR_getresuid);
#     if defined(__NR_getresuid32)
      ADD(0, __NR_getresuid32);
#     endif
      ADD(0, __NR_getrlimit);
      ADD(0, __NR_getrusage);
      ADD(0, __NR_getsid);
#     if defined(__NR_getsockname)
      ADD(0, __NR_getsockname);
#     endif
#     if defined(__NR_getsockopt)
      ADD(0, __NR_getsockopt);
#     endif
      ADD(0, __NR_gettid);
      ADD(0, __NR_gettimeofday);
      ADD(0, __NR_getuid);
#     if defined(__NR_getuid32)
      ADD(0, __NR_getuid32);
#     endif
      ADD(0, __NR_getxattr);
#     if defined(__NR_ioperm)
      ADD(0, __NR_ioperm);
#     endif
      ADD(0, __NR_inotify_add_watch);
      ADD(0, __NR_inotify_init);
#     if defined(__NR_inotify_init1)
      ADD(0, __NR_inotify_init1);
#     endif
      ADD(0, __NR_inotify_rm_watch);
      ADD(0, __NR_ioctl); // ioctl -- assuming no pointers returned
      ADD(0, __NR_ioprio_get);
      ADD(0, __NR_kill);
      ADD(0, __NR_lgetxattr);
      ADD(0, __NR_link);
#     if defined(__NR_listen)
      ADD(0, __NR_listen);
#     endif
      ADD(0, __NR_lseek);
      ADD(0, __NR_lstat);
#     if defined(__NR_lstat64)
      ADD(0, __NR_lstat64);
#     endif
      ADD(0, __NR_madvise);
      ADD(0, __NR_mkdir);
      ADD(0, __NR_mlock);
      ADD(0, __NR_mlockall);
      ADD(0, __NR_mprotect);
#     if defined(__NR_mq_open)
      ADD(0, __NR_mq_open);
      ADD(0, __NR_mq_unlink);
      ADD(0, __NR_mq_timedsend);
      ADD(0, __NR_mq_timedreceive);
      ADD(0, __NR_mq_notify);
      ADD(0, __NR_mq_getsetattr);
#     endif
      ADD(0, __NR_munmap); // die_mem_munmap already called, segment remove);
      ADD(0, __NR_nanosleep);
      ADD(0, __NR_open);
      ADD(0, __NR_personality);
      ADD(0, __NR_pipe);
#     if defined(__NR_pipe2)
      ADD(0, __NR_pipe2);
#     endif
      ADD(0, __NR_poll);
      ADD(0, __NR_prctl);
      ADD(0, __NR_pread64);
      ADD(0, __NR_pwrite64);
      ADD(0, __NR_read);
      ADD(0, __NR_readlink);
      ADD(0, __NR_readv);
#     if defined(__NR_recvfrom)
      ADD(0, __NR_recvfrom);
#     endif
#     if defined(__NR_recvmsg)
      ADD(0, __NR_recvmsg);
#     endif
      ADD(0, __NR_rename);
      ADD(0, __NR_rmdir);
      ADD(0, __NR_rt_sigaction);
      ADD(0, __NR_rt_sigprocmask);
      ADD(0, __NR_rt_sigreturn); /* not sure if we should see this or not */
      ADD(0, __NR_rt_sigsuspend);
      ADD(0, __NR_rt_sigtimedwait);
      ADD(0, __NR_sched_get_priority_max);
      ADD(0, __NR_sched_get_priority_min);
      ADD(0, __NR_sched_getaffinity);
      ADD(0, __NR_sched_getparam);
      ADD(0, __NR_sched_getscheduler);
      ADD(0, __NR_sched_setaffinity);
      ADD(0, __NR_sched_setscheduler);
      ADD(0, __NR_sched_yield);
      ADD(0, __NR_select);
#     if defined(__NR_semctl)
      ADD(0, __NR_semctl);
#     endif
#     if defined(__NR_semget)
      ADD(0, __NR_semget);
#     endif
#     if defined(__NR_semop)
      ADD(0, __NR_semop);
#     endif
      ADD(0, __NR_sendfile);
#     if defined(__NR_sendto)
      ADD(0, __NR_sendto);
#     endif
#     if defined(__NR_sendmsg)
      ADD(0, __NR_sendmsg);
#     endif
      ADD(0, __NR_set_robust_list);
#     if defined(__NR_set_thread_area)
      ADD(0, __NR_set_thread_area);
#     endif
      ADD(0, __NR_set_tid_address);
      ADD(0, __NR_setgid);
      ADD(0, __NR_setfsgid);
      ADD(0, __NR_setfsuid);
      ADD(0, __NR_setgid);
      ADD(0, __NR_setgroups);
      ADD(0, __NR_setitimer);
      ADD(0, __NR_setpgid);
      ADD(0, __NR_setpriority);
      ADD(0, __NR_setregid);
      ADD(0, __NR_setresgid);
      ADD(0, __NR_setresuid);
      ADD(0, __NR_setreuid);
      ADD(0, __NR_setrlimit);
      ADD(0, __NR_setsid);
#     if defined(__NR_setsockopt)
      ADD(0, __NR_setsockopt);
#     endif
      ADD(0, __NR_setuid);
#     if defined(__NR_shmctl)
      ADD(0, __NR_shmctl);
      ADD(0, __NR_shmdt);
#     endif
#     if defined(__NR_shutdown)
      ADD(0, __NR_shutdown);
#     endif
      ADD(0, __NR_sigaltstack);
#     if defined(__NR_socket)
      ADD(0, __NR_socket);
#     endif
#     if defined(__NR_socketcall)
      ADD(0, __NR_socketcall); /* the nasty x86-linux socket multiplexor */
#     endif
#     if defined(__NR_socketpair)
      ADD(0, __NR_socketpair);
#     endif
#     if defined(__NR_statfs64)
      ADD(0, __NR_statfs64);
#     endif
#     if defined(__NR_sigreturn)
      ADD(0, __NR_sigreturn); /* not sure if we should see this or not */
#     endif
#     if defined(__NR_stat64)
      ADD(0, __NR_stat64);
#     endif
      ADD(0, __NR_stat);
      ADD(0, __NR_statfs);
      ADD(0, __NR_symlink);
      ADD(0, __NR_sysinfo);
      ADD(0, __NR_tgkill);
      ADD(0, __NR_time);
      ADD(0, __NR_times);
      ADD(0, __NR_truncate);
#     if defined(__NR_truncate64)
      ADD(0, __NR_truncate64);
#     endif
#     if defined(__NR_ugetrlimit)
      ADD(0, __NR_ugetrlimit);
#     endif
      ADD(0, __NR_umask);
      ADD(0, __NR_uname);
      ADD(0, __NR_unlink);
      ADD(0, __NR_utime);
      ADD(0, __NR_vfork);
#     if defined(__NR_waitpid)
      ADD(0, __NR_waitpid);
#     endif
      ADD(0, __NR_wait4);
      ADD(0, __NR_write);
      ADD(0, __NR_writev);

      /* Whereas the following need special treatment */
#     if defined(__NR_arch_prctl)
      ADD(1, __NR_arch_prctl);
#     endif
      ADD(1, __NR_brk);
      ADD(1, __NR_mmap);
#     if defined(__NR_mmap2)
      ADD(1, __NR_mmap2);
#     endif
#     if defined(__NR_shmat)
      ADD(1, __NR_shmat);
#     endif
#     if defined(__NR_shmget)
      ADD(1, __NR_shmget);
#     endif
#     if defined(__NR_ipc) && defined(VKI_SHMAT)
      ADD(1, __NR_ipc); /* ppc{32,64}-linux horrors */
#     endif

   /* --------------- AIX5 --------------- */

#  elif defined(VGO_aix5)

#     define ADD(_flag, _syscallname) \
         do { \
            UWordPair p; \
            if ((_syscallname) != __NR_AIX5_UNKNOWN) { \
               p.uw1 = (UWord)&(_syscallname); p.uw2 = (_flag); \
               VG_(addToXA)( post_syscall_table, &p ); \
            } \
         } while (0)

      /* Just a minimal set of handlers, enough to make
         a 32- and 64-bit hello-world program run. */
      ADD(1, __NR_AIX5___loadx); /* not sure what to do here */
      ADD(0, __NR_AIX5__exit);
      ADD(0, __NR_AIX5_access);
      ADD(0, __NR_AIX5_getgidx);
      ADD(0, __NR_AIX5_getuidx);
      ADD(0, __NR_AIX5_kfcntl);
      ADD(0, __NR_AIX5_kioctl);
      ADD(1, __NR_AIX5_kload); /* not sure what to do here */
      ADD(0, __NR_AIX5_kwrite);

   /* --------------- DARWIN ------------- */

#  elif defined(VGO_darwin)

#     define ADD(_flag, _syscallname) \
         do { UWordPair p; p.uw1 = (_syscallname); p.uw2 = (_flag); \
              VG_(addToXA)( post_syscall_table, &p ); \
         } while (0)

      // DDD: a desultory attempt thus far...

      // Unix/BSD syscalls.

      // Mach traps.
      ADD(0, __NR_host_self_trap);
      ADD(0, __NR_mach_msg_trap);
      ADD(0, __NR_mach_reply_port);
      ADD(0, __NR_task_self_trap);

      // Machine-dependent syscalls.
      ADD(0, __NR_thread_fast_set_cthread_self);

   /* ------------------------------------ */

#  else
#     error "Unsupported OS"
#  endif

#  undef ADD
}


void h_post_syscall ( ThreadId tid, UInt sysno, 
                      UWord* args, UInt nArgs, SysRes res )
{
   Word i, n;
   UWordPair* pair;

   if (!post_syscall_table)
      setup_post_syscall_table();

   /* search for 'sysno' in the post_syscall_table */
   n = VG_(sizeXA)( post_syscall_table );
   for (i = 0; i < n; i++) {
      pair = VG_(indexXA)( post_syscall_table, i );
#     if defined(VGO_linux) || defined(VGO_darwin)
      if (pair->uw1 == (UWord)sysno)
         break;
#     elif defined(VGO_aix5)
      if (*(Int*)(pair->uw1) == (Int)sysno)
         break;
#     else
#        error "Unsupported OS"
#     endif
   }

   tl_assert(i >= 0 && i <= n);

   if (i == n) {
      VG_(printf)("sysno == %s", VG_SYSNUM_STRING_EXTRA(sysno));
      VG_(tool_panic)("unhandled syscall");
   }

   /* So we found the relevant entry.  Move it one step
      forward so as to speed future accesses to it. */
   if (i > 0) {
      UWordPair tmp, *p, *q;
      p = VG_(indexXA)( post_syscall_table, i-1 );
      q = VG_(indexXA)( post_syscall_table, i-0 );
      tmp = *p;
      *p = *q;
      *q = tmp;
      i--;
   }

   /* Deal with the common case */
   pair = VG_(indexXA)( post_syscall_table, i );
   if (pair->uw2 == 0)
      /* the common case */
      goto res_NONPTR_err_NONPTR;

   /* Special handling for all remaining cases */
   tl_assert(pair->uw2 == 1);

#  if defined(__NR_arch_prctl)
   if (sysno == __NR_arch_prctl) {
      /* This is nasty.  On amd64-linux, arch_prctl may write a
         value to guest_FS_ZERO, and we need to shadow that value.
         Hence apply nonptr_or_unknown to it here, after the
         syscall completes. */
      post_reg_write_nonptr_or_unknown( tid, PC_OFF_FS_ZERO, 
                                             PC_SZB_FS_ZERO );
      goto res_NONPTR_err_NONPTR;
   }
#  endif

#  if defined(__NR_brk)
   // With brk(), result (of kernel syscall, not glibc wrapper) is a heap
   // pointer.  Make the shadow UNKNOWN.
   if (sysno == __NR_brk)
      goto res_UNKNOWN_err_NONPTR;
#  endif

   // With mmap, new_mem_mmap() has already been called and added the
   // segment (we did it there because we had the result address and size
   // handy).  So just set the return value shadow.
   if (sysno == __NR_mmap
#      if defined(__NR_mmap2)
       || sysno == __NR_mmap2
#      endif
#      if defined(__NR_AIX5___loadx)
       || (sysno == __NR_AIX5___loadx && __NR_AIX5___loadx != __NR_AIX5_UNKNOWN)
#      endif
#      if defined(__NR_AIX5_kload)
       || (sysno == __NR_AIX5_kload && __NR_AIX5_kload != __NR_AIX5_UNKNOWN)
#      endif
      ) {
      if (sr_isError(res)) {
         // mmap() had an error, return value is a small negative integer
         goto res_NONPTR_err_NONPTR;
      } else {
         goto res_UNKNOWN_err_NONPTR;
      }
      return;
   }

   // shmat uses the same scheme.  We will just have had a
   // notification via new_mem_mmap.  Just set the return value shadow.
#  if defined(__NR_shmat)
   if (sysno == __NR_shmat) {
      if (sr_isError(res)) {
         goto res_NONPTR_err_NONPTR;
      } else {
         goto res_UNKNOWN_err_NONPTR;
      }
   }
#  endif

#  if defined(__NR_shmget)
   if (sysno == __NR_shmget)
      // FIXME: is this correct?
      goto res_UNKNOWN_err_NONPTR;
#  endif

#  if defined(__NR_ipc) && defined(VKI_SHMAT)
   /* perhaps this should be further conditionalised with
      && (defined(VGP_ppc32_linux) || defined(VGP_ppc64_linux)
      Note, this just copies the behaviour of __NR_shmget above.

      JRS 2009 June 02: it seems that the return value from
      sys_ipc(VKI_SHMAT, ...) doesn't have much relationship to the
      result returned by the originating user-level shmat call.  It's
      different (and much lower) by a large but integral number of
      pages.  I don't have time to chase this right now.  Observed on
      ppc{32,64}-linux.  Result appears to be false errors from apps
      using shmat.  Confusion though -- shouldn't be related to the
      actual numeric values returned by the syscall, though, should
      it?  Confused.  Maybe some bad interaction with a
      nonpointer-or-unknown heuristic? */
   if (sysno == __NR_ipc) {
      if (args[0] == VKI_SHMAT) {
         goto res_UNKNOWN_err_NONPTR;
      } else {
         goto res_NONPTR_err_NONPTR;
      }
   }
#  endif

   /* If we get here, it implies the corresponding entry in
      post_syscall_table has .w2 == 1, which in turn implies there
      should be special-case code for it above. */
   tl_assert(0);

  res_NONPTR_err_NONPTR:
   VG_(set_syscall_return_shadows)( tid, /* retval */ (UWord)NONPTR, 0,
                                         /* error */  (UWord)NONPTR, 0 );
   return;

  res_UNKNOWN_err_NONPTR:
   VG_(set_syscall_return_shadows)( tid, /* retval */ (UWord)UNKNOWN, 0,
                                         /* error */  (UWord)NONPTR, 0 );
   return;
}


/*--------------------------------------------------------------------*/
/*--- Functions called from generated code                         ---*/
/*--------------------------------------------------------------------*/

#if SC_SEGS
static void checkSeg ( Seg vseg ) {
   tl_assert(vseg == UNKNOWN || vseg == NONPTR || vseg == BOTTOM
             || Seg__plausible(vseg) );
}
#endif

// XXX: could be more sophisticated -- actually track the lowest/highest
// valid address used by the program, and then return False for anything
// below that (using a suitable safety margin).  Also, nothing above
// 0xc0000000 is valid [unless you've changed that in your kernel]
static inline Bool looks_like_a_pointer(Addr a)
{
#  if defined(VGA_x86) || defined(VGA_ppc32)
   tl_assert(sizeof(UWord) == 4);
   return (a > 0x800000UL && a < 0xFF000000UL);

#  elif defined(VGA_amd64) || defined(VGA_ppc64)
   tl_assert(sizeof(UWord) == 8);
   return (a >= 16 * 0x10000UL && a < 0xFF00000000000000UL);

#  elif defined(VGA_arm)
   /* Unfortunately arm-linux seems to load the exe at very low, at
      0x8000, so we have to assume any value above that is a pointer,
      which is pretty dismal. */
   tl_assert(sizeof(UWord) == 4);
   return (a >= 0x00008000UL && a < 0xFF000000UL);

#  else
#    error "Unsupported architecture"
#  endif
}

static inline VG_REGPARM(1)
Seg* nonptr_or_unknown(UWord x)
{
   Seg* res = looks_like_a_pointer(x) ? UNKNOWN : NONPTR;
   if (0) VG_(printf)("nonptr_or_unknown %s %#lx\n", 
                      res==UNKNOWN ? "UUU" : "nnn", x);
   return res;
}

//zz static __attribute__((regparm(1)))
//zz void print_BB_entry(UInt bb)
//zz {
//zz    VG_(printf)("%u =\n", bb);
//zz }

//static ULong stats__tot_mem_refs  = 0;
//static ULong stats__refs_in_a_seg = 0;
//static ULong stats__refs_lost_seg = 0;

typedef
   struct { ExeContext* ec; UWord count; }
   Lossage;

static OSet* lossage = NULL;

//static void inc_lossage ( ExeContext* ec ) 
//{
//   Lossage key, *res, *nyu;
//   key.ec = ec;
//   key.count = 0; /* frivolous */
//   res = VG_(OSetGen_Lookup)(lossage, &key);
//   if (res) {
//      tl_assert(res->ec == ec);
//      res->count++;
//   } else {
//      nyu = (Lossage*)VG_(OSetGen_AllocNode)(lossage, sizeof(Lossage));
//      tl_assert(nyu);
//      nyu->ec = ec;
//      nyu->count = 1;
//      VG_(OSetGen_Insert)( lossage, nyu );
//   }
//}

static void init_lossage ( void )
{
   lossage = VG_(OSetGen_Create)( /*keyOff*/ offsetof(Lossage,ec),
                                  /*fastCmp*/NULL,
                                  VG_(malloc), "pc.h_main.il.1",
                                  VG_(free) );
   tl_assert(lossage);
}

//static void show_lossage ( void )
//{
//   Lossage* elem;
//   VG_(OSetGen_ResetIter)( lossage );
//   while ( (elem = VG_(OSetGen_Next)(lossage)) ) {
//      if (elem->count < 10) continue;
//      //Char buf[100];
//      //(void)VG_(describe_IP)(elem->ec, buf, sizeof(buf)-1);
//      //buf[sizeof(buf)-1] = 0;
//      //VG_(printf)("  %,8lu  %s\n", elem->count, buf);
//      VG_(message)(Vg_UserMsg, "Lossage count %'lu at", elem->count);
//      VG_(pp_ExeContext)(elem->ec);
//   }
//}

// This function is called *a lot*; inlining it sped up Konqueror by 20%.
static inline
void check_load_or_store(Bool is_write, Addr m, UWord sz, Seg* mptr_vseg)
{
#if 0
   tl_assert(0);
   if (h_clo_lossage_check) {
      Seg* seg;
      stats__tot_mem_refs++;
      if (ISList__findI0( seglist, (Addr)m, &seg )) {
         /* m falls inside 'seg' (that is, we are making a memory
            reference inside 'seg').  Now, really mptr_vseg should be
            a tracked segment of some description.  Badness is when
            mptr_vseg is UNKNOWN, BOTTOM or NONPTR at this point,
            since that means we've lost the type of it somehow: it
            shoud say that m points into a real segment (preferable
            'seg'), but it doesn't. */
         if (Seg__status_is_SegHeap(seg)) {
            stats__refs_in_a_seg++;
            if (UNKNOWN == mptr_vseg
                || BOTTOM == mptr_vseg || NONPTR == mptr_vseg) {
               ExeContext* ec;
               Char buf[100];
               static UWord xx = 0;
               stats__refs_lost_seg++;
               ec = VG_(record_ExeContext)( VG_(get_running_tid)(), 0 );
               inc_lossage(ec);
               if (0) {
                  VG_(message)(Vg_DebugMsg, "");
                  VG_(message)(Vg_DebugMsg,
                               "Lossage %s %#lx sz %lu inside block alloc'd",
                               is_write ? "wr" : "rd", m, (UWord)sz);
                  VG_(pp_ExeContext)(Seg__where(seg));
               }
               if (xx++ < 0) {
                  Addr ip = VG_(get_IP)( VG_(get_running_tid)() );
                  (void)VG_(describe_IP)( ip, buf, sizeof(buf)-1);
                  buf[sizeof(buf)-1] = 0;
                  VG_(printf)("lossage at %p %s\n", ec, buf );
               }
            }
         }
      }
   } /* clo_lossage_check */
#endif

#  if SC_SEGS
   checkSeg(mptr_vseg);
#  endif

   if (UNKNOWN == mptr_vseg) {
      // do nothing

   } else if (BOTTOM == mptr_vseg) {
      // do nothing

   } else if (NONPTR == mptr_vseg) {
      h_record_heap_error( m, sz, mptr_vseg, is_write );

   } else {
      // check all segment ranges in the circle
      // if none match, warn about 1st seg
      // else,          check matching one isn't freed
      Bool is_ok = False;
      Seg* curr  = mptr_vseg;
      Addr mhi;

      // Accesses partly outside range are an error, unless it's an aligned
      // word-sized read, and --partial-loads-ok=yes.  This is to cope with
      // gcc's/glibc's habits of doing word-sized accesses that read past
      // the ends of arrays/strings.
      // JRS 2008-sept-11: couldn't this be moved off the critical path?
      if (!is_write && sz == sizeof(UWord)
          && h_clo_partial_loads_ok && SHMEM_IS_WORD_ALIGNED(m)) {
         mhi = m;
      } else {
         mhi = m+sz-1;
      }

      if (0) VG_(printf)("calling seg_ci %p %#lx %#lx\n", curr,m,mhi);
      is_ok = curr->addr <= m && mhi < curr->addr + curr->szB;

      // If it's an overrun/underrun of a freed block, don't give both
      // warnings, since the first one mentions that the block has been
      // freed.
      if ( ! is_ok || Seg__is_freed(curr) )
         h_record_heap_error( m, sz, mptr_vseg, is_write );
   }
}

// ------------------ Load handlers ------------------ //

/* On 32 bit targets, we will use:
      check_load1 check_load2 check_load4_P
      check_load4  (for 32-bit FP reads)
      check_load8  (for 64-bit FP reads)
      check_load16 (for xmm/altivec reads)
   On 64 bit targets, we will use:
      check_load1 check_load2 check_load4 check_load8_P
      check_load8  (for 64-bit FP reads)
      check_load16 (for xmm/altivec reads)

   A "_P" handler reads a pointer from memory, and so returns a value
   to the generated code -- the pointer's shadow value.  That implies
   that check_load4_P is only to be called on a 32 bit host and
   check_load8_P is only to be called on a 64 bit host.  For all other
   cases no shadow value is returned; we merely check that the pointer
   (m) matches the block described by its shadow value (mptr_vseg).
*/

// This handles 128 bit loads on both 32 bit and 64 bit targets.
static VG_REGPARM(2)
void check_load16(Addr m, Seg* mptr_vseg)
{
#  if SC_SEGS
   checkSeg(mptr_vseg);
#  endif
   check_load_or_store(/*is_write*/False, m, 16, mptr_vseg);
}

// This handles 64 bit FP-or-otherwise-nonpointer loads on both
// 32 bit and 64 bit targets.
static VG_REGPARM(2)
void check_load8(Addr m, Seg* mptr_vseg)
{
#  if SC_SEGS
   checkSeg(mptr_vseg);
#  endif
   check_load_or_store(/*is_write*/False, m, 8, mptr_vseg);
}

// This handles 64 bit loads on 64 bit targets.  It must
// not be called on 32 bit targets.
// return m.vseg
static VG_REGPARM(2)
Seg* check_load8_P(Addr m, Seg* mptr_vseg)
{
   Seg* vseg;
   tl_assert(sizeof(UWord) == 8); /* DO NOT REMOVE */
#  if SC_SEGS
   checkSeg(mptr_vseg);
#  endif
   check_load_or_store(/*is_write*/False, m, 8, mptr_vseg);
   if (VG_IS_8_ALIGNED(m)) {
      vseg = get_mem_vseg(m);
   } else {
      vseg = nonptr_or_unknown( *(ULong*)m );
   }
   return vseg;
}

// This handles 32 bit loads on 32 bit targets.  It must
// not be called on 64 bit targets.
// return m.vseg
static VG_REGPARM(2)
Seg* check_load4_P(Addr m, Seg* mptr_vseg)
{
   Seg* vseg;
   tl_assert(sizeof(UWord) == 4); /* DO NOT REMOVE */
#  if SC_SEGS
   checkSeg(mptr_vseg);
#  endif
   check_load_or_store(/*is_write*/False, m, 4, mptr_vseg);
   if (VG_IS_4_ALIGNED(m)) {
      vseg = get_mem_vseg(m);
   } else {
      vseg = nonptr_or_unknown( *(UInt*)m );
   }
   return vseg;
}

// Used for both 32 bit and 64 bit targets.
static VG_REGPARM(2)
void check_load4(Addr m, Seg* mptr_vseg)
{
#  if SC_SEGS
   checkSeg(mptr_vseg);
#  endif
   check_load_or_store(/*is_write*/False, m, 4, mptr_vseg);
}

// Used for both 32 bit and 64 bit targets.
static VG_REGPARM(2)
void check_load2(Addr m, Seg* mptr_vseg)
{
#  if SC_SEGS
   checkSeg(mptr_vseg);
#  endif
   check_load_or_store(/*is_write*/False, m, 2, mptr_vseg);
}

// Used for both 32 bit and 64 bit targets.
static VG_REGPARM(2)
void check_load1(Addr m, Seg* mptr_vseg)
{
#  if SC_SEGS
   checkSeg(mptr_vseg);
#  endif
   check_load_or_store(/*is_write*/False, m, 1, mptr_vseg);
}

// ------------------ Store handlers ------------------ //

/* On 32 bit targets, we will use:
      check_store1 check_store2 check_store4_P check_store4C_P
      check_store4 (for 32-bit nonpointer stores)
      check_store8_ms4B_ls4B (for 64-bit stores)
      check_store16_ms4B_4B_4B_ls4B (for xmm/altivec stores)

   On 64 bit targets, we will use:
      check_store1 check_store2 check_store4 check_store4C
      check_store8_P check_store_8C_P
      check_store8_all8B (for 64-bit nonpointer stores)
      check_store16_ms8B_ls8B (for xmm/altivec stores)

   A "_P" handler writes a pointer to memory, and so has an extra
   argument -- the pointer's shadow value.  That implies that
   check_store4{,C}_P is only to be called on a 32 bit host and
   check_store8{,C}_P is only to be called on a 64 bit host.  For all
   other cases, and for the misaligned _P cases, the strategy is to
   let the store go through, and then snoop around with
   nonptr_or_unknown to fix up the shadow values of any affected
   words. */

/* Helpers for store-conditionals.  Ugly kludge :-(
   They all return 1 if the SC was successful and 0 if it failed. */
static inline UWord do_store_conditional_32( Addr m/*dst*/, UInt t/*val*/ )
{
#  if defined(VGA_ppc32) || defined(VGA_ppc64)
   UWord success;
   /* If this assertion fails, the underlying IR is (semantically) ill-formed
      as per the IR spec for IRStmt_Store. */
   tl_assert(VG_IS_4_ALIGNED(m));
   __asm__ __volatile__(
      "stwcx. %2,0,%1"    "\n\t" /* data,0,addr */
      "mfcr   %0"         "\n\t"
      "srwi   %0,%0,29"   "\n\t" /* move relevant CR bit to LSB */
      : /*out*/"=b"(success) 
      : /*in*/ "b"(m), "b"( (UWord)t ) 
      : /*trash*/ "memory", "cc"
        /* Note: srwi is OK even on 64-bit host because the we're
           after bit 29 (normal numbering) and we mask off all the
           other junk just below. */
   );
   return success & (UWord)1;
#  else
   tl_assert(0); /* not implemented on other platforms */
#  endif
}

static inline UWord do_store_conditional_64( Addr m/*dst*/, ULong t/*val*/ )
{
#  if defined(VGA_ppc64)
   UWord success;
   /* If this assertion fails, the underlying IR is (semantically) ill-formed
      as per the IR spec for IRStmt_Store. */
   tl_assert(VG_IS_8_ALIGNED(m));
   __asm__ __volatile__(
      "stdcx. %2,0,%1"    "\n\t" /* data,0,addr */
      "mfcr   %0"         "\n\t"
      "srdi   %0,%0,29"   "\n\t" /* move relevant CR bit to LSB */
      : /*out*/"=b"(success) 
      : /*in*/ "b"(m), "b"( (UWord)t ) 
      : /*trash*/ "memory", "cc"
   );
   return success & (UWord)1;
#  else
   tl_assert(0); /* not implemented on other platforms */
#  endif
}

/* Apply nonptr_or_unknown to all the words intersecting
   [a, a+len). */
static inline VG_REGPARM(2)
void nonptr_or_unknown_range ( Addr a, SizeT len )
{
   const SizeT wszB = sizeof(UWord);
   Addr wfirst = VG_ROUNDDN(a,       wszB);
   Addr wlast  = VG_ROUNDDN(a+len-1, wszB);
   Addr a2;
   tl_assert(wfirst <= wlast);
   for (a2 = wfirst ; a2 <= wlast; a2 += wszB) {
      set_mem_vseg( a2, nonptr_or_unknown( *(UWord*)a2 ));
   }
}

// Write to shadow memory, for a 32-bit store.  Must only
// be used on 32-bit targets.
static inline VG_REGPARM(2)
void do_shadow_store4_P( Addr m, Seg* vseg )
{
   tl_assert(sizeof(UWord) == 4); /* DO NOT REMOVE */
   if (VG_IS_4_ALIGNED(m)) {
      set_mem_vseg( m, vseg );
   } else {
      // straddling two words
      nonptr_or_unknown_range(m, 4);
   }
}

// Write to shadow memory, for a 64-bit store.  Must only
// be used on 64-bit targets.
static inline VG_REGPARM(2)
void do_shadow_store8_P( Addr m, Seg* vseg )
{
   tl_assert(sizeof(UWord) == 8); /* DO NOT REMOVE */
   if (VG_IS_8_ALIGNED(m)) {
      set_mem_vseg( m, vseg );
   } else {
      // straddling two words
      nonptr_or_unknown_range(m, 8);
   }
}

// This handles 128 bit stores on 64 bit targets.  The
// store data is passed in 2 pieces, the most significant
// bits first.
static VG_REGPARM(3)
void check_store16_ms8B_ls8B(Addr m, Seg* mptr_vseg,
                             UWord ms8B, UWord ls8B)
{
   tl_assert(sizeof(UWord) == 8); /* DO NOT REMOVE */
#  if SC_SEGS
   checkSeg(mptr_vseg);
#  endif
   check_load_or_store(/*is_write*/True, m, 16, mptr_vseg);
   // Actually *do* the STORE here
   if (host_is_little_endian()) {
      // FIXME: aren't we really concerned whether the guest
      // is little endian, not whether the host is?
      *(ULong*)(m + 0) = ls8B;
      *(ULong*)(m + 8) = ms8B;
   } else {
      *(ULong*)(m + 0) = ms8B;
      *(ULong*)(m + 8) = ls8B;
   }
   nonptr_or_unknown_range(m, 16);
}

// This handles 128 bit stores on 64 bit targets.  The
// store data is passed in 2 pieces, the most significant
// bits first.
static VG_REGPARM(3)
void check_store16_ms4B_4B_4B_ls4B(Addr m, Seg* mptr_vseg,
                                   UWord ms4B, UWord w2,
                                   UWord w1,   UWord ls4B)
{
   tl_assert(sizeof(UWord) == 4); /* DO NOT REMOVE */
#  if SC_SEGS
   checkSeg(mptr_vseg);
#  endif
   check_load_or_store(/*is_write*/True, m, 16, mptr_vseg);
   // Actually *do* the STORE here
   if (host_is_little_endian()) {
      // FIXME: aren't we really concerned whether the guest
      // is little endian, not whether the host is?
      *(UInt*)(m +  0) = ls4B;
      *(UInt*)(m +  4) = w1;
      *(UInt*)(m +  8) = w2;
      *(UInt*)(m + 12) = ms4B;
   } else {
      *(UInt*)(m +  0) = ms4B;
      *(UInt*)(m +  4) = w2;
      *(UInt*)(m +  8) = w1;
      *(UInt*)(m + 12) = ls4B;
   }
   nonptr_or_unknown_range(m, 16);
}

// This handles 64 bit stores on 32 bit targets.  The
// store data is passed in 2 pieces, the most significant
// bits first.
static VG_REGPARM(3)
void check_store8_ms4B_ls4B(Addr m, Seg* mptr_vseg,
                            UWord ms4B, UWord ls4B)
{
   tl_assert(sizeof(UWord) == 4); /* DO NOT REMOVE */
#  if SC_SEGS
   checkSeg(mptr_vseg);
#  endif
   check_load_or_store(/*is_write*/True, m, 8, mptr_vseg);
   // Actually *do* the STORE here
   if (host_is_little_endian()) {
      // FIXME: aren't we really concerned whether the guest
      // is little endian, not whether the host is?
      *(UInt*)(m + 0) = ls4B;
      *(UInt*)(m + 4) = ms4B;
   } else {
      *(UInt*)(m + 0) = ms4B;
      *(UInt*)(m + 4) = ls4B;
   }
   nonptr_or_unknown_range(m, 8);
}

// This handles 64 bit non pointer stores on 64 bit targets.
// It must not be called on 32 bit targets.
static VG_REGPARM(3)
void check_store8_all8B(Addr m, Seg* mptr_vseg, UWord all8B)
{
   tl_assert(sizeof(UWord) == 8); /* DO NOT REMOVE */
#  if SC_SEGS
   checkSeg(mptr_vseg);
#  endif
   check_load_or_store(/*is_write*/True, m, 8, mptr_vseg);
   // Actually *do* the STORE here
   *(ULong*)m = all8B;
   nonptr_or_unknown_range(m, 8);
}

// This handles 64 bit stores on 64 bit targets.  It must
// not be called on 32 bit targets.
static VG_REGPARM(3)
void check_store8_P(Addr m, Seg* mptr_vseg, UWord t, Seg* t_vseg)
{
   tl_assert(sizeof(UWord) == 8); /* DO NOT REMOVE */
#  if SC_SEGS
   checkSeg(t_vseg);
   checkSeg(mptr_vseg);
#  endif
   check_load_or_store(/*is_write*/True, m, 8, mptr_vseg);
   // Actually *do* the STORE here
   *(ULong*)m = t;
   do_shadow_store8_P( m, t_vseg );
}

// This handles 64 bit store-conditionals on 64 bit targets.  It must
// not be called on 32 bit targets.
static VG_REGPARM(3)
UWord check_store8C_P(Addr m, Seg* mptr_vseg, UWord t, Seg* t_vseg)
{
   UWord success;
   tl_assert(sizeof(UWord) == 8); /* DO NOT REMOVE */
#  if SC_SEGS
   checkSeg(t_vseg);
   checkSeg(mptr_vseg);
#  endif
   check_load_or_store(/*is_write*/True, m, 8, mptr_vseg);
   // Actually *do* the STORE here
   success = do_store_conditional_64( m, t );
   if (success)
      do_shadow_store8_P( m, t_vseg );
   return success;
}

// This handles 32 bit stores on 32 bit targets.  It must
// not be called on 64 bit targets.
static VG_REGPARM(3)
void check_store4_P(Addr m, Seg* mptr_vseg, UWord t, Seg* t_vseg)
{
   tl_assert(sizeof(UWord) == 4); /* DO NOT REMOVE */
#  if SC_SEGS
   checkSeg(t_vseg);
   checkSeg(mptr_vseg);
#  endif
   check_load_or_store(/*is_write*/True, m, 4, mptr_vseg);
   // Actually *do* the STORE here
   *(UInt*)m = t;
   do_shadow_store4_P( m, t_vseg );
}

// This handles 32 bit store-conditionals on 32 bit targets.  It must
// not be called on 64 bit targets.
static VG_REGPARM(3)
UWord check_store4C_P(Addr m, Seg* mptr_vseg, UWord t, Seg* t_vseg)
{
   UWord success;
   tl_assert(sizeof(UWord) == 4); /* DO NOT REMOVE */
#  if SC_SEGS
   checkSeg(t_vseg);
   checkSeg(mptr_vseg);
#  endif
   check_load_or_store(/*is_write*/True, m, 4, mptr_vseg);
   // Actually *do* the STORE here
   success = do_store_conditional_32( m, t );
   if (success)
      do_shadow_store4_P( m, t_vseg );
   return success;
}

// Used for both 32 bit and 64 bit targets.
static VG_REGPARM(3)
void check_store4(Addr m, Seg* mptr_vseg, UWord t)
{
#  if SC_SEGS
   checkSeg(mptr_vseg);
#  endif
   check_load_or_store(/*is_write*/True, m, 4, mptr_vseg);
   // Actually *do* the STORE here  (Nb: cast must be to 4-byte type!)
   *(UInt*)m = t;
   nonptr_or_unknown_range(m, 4);
}

// Used for 32-bit store-conditionals on 64 bit targets only.  It must
// not be called on 32 bit targets.
static VG_REGPARM(3)
UWord check_store4C(Addr m, Seg* mptr_vseg, UWord t)
{
   UWord success;
   tl_assert(sizeof(UWord) == 8); /* DO NOT REMOVE */
#  if SC_SEGS
   checkSeg(mptr_vseg);
#  endif
   check_load_or_store(/*is_write*/True, m, 4, mptr_vseg);
   // Actually *do* the STORE here
   success = do_store_conditional_32( m, t );
   if (success)
      nonptr_or_unknown_range(m, 4);
   return success;
}

// Used for both 32 bit and 64 bit targets.
static VG_REGPARM(3)
void check_store2(Addr m, Seg* mptr_vseg, UWord t)
{
#  if SC_SEGS
   checkSeg(mptr_vseg);
#  endif
   check_load_or_store(/*is_write*/True, m, 2, mptr_vseg);
   // Actually *do* the STORE here  (Nb: cast must be to 2-byte type!)
   *(UShort*)m = t;
   nonptr_or_unknown_range(m, 2);
}

// Used for both 32 bit and 64 bit targets.
static VG_REGPARM(3)
void check_store1(Addr m, Seg* mptr_vseg, UWord t)
{
#  if SC_SEGS
   checkSeg(mptr_vseg);
#  endif
   check_load_or_store(/*is_write*/True, m, 1, mptr_vseg);
   // Actually *do* the STORE here  (Nb: cast must be to 1-byte type!)
   *(UChar*)m = t;
   nonptr_or_unknown_range(m, 1);
}


// Nb: if the result is BOTTOM, return immedately -- don't let BOTTOM
//     be changed to NONPTR by a range check on the result.
#define BINOP(bt, nn, nu, np, un, uu, up, pn, pu, pp) \
   if (BOTTOM == seg1 || BOTTOM == seg2) { bt;                   \
   } else if (NONPTR == seg1)  { if      (NONPTR == seg2)  { nn; }  \
                                 else if (UNKNOWN == seg2) { nu; }    \
                                 else                      { np; }    \
   } else if (UNKNOWN == seg1) { if      (NONPTR == seg2)  { un; }    \
                                 else if (UNKNOWN == seg2) { uu; }    \
                                 else                      { up; }    \
   } else                      { if      (NONPTR == seg2)  { pn; }    \
                                 else if (UNKNOWN == seg2) { pu; }    \
                                 else                      { pp; }    \
   }

#define BINERROR(opname)                    \
   h_record_arith_error(seg1, seg2, opname);  \
   out = NONPTR


// -------------
//  + | n  ?  p
// -------------
//  n | n  ?  p
//  ? | ?  ?  ?
//  p | p  ?  e   (all results become n if they look like a non-pointer)
// -------------
static Seg* do_addW_result(Seg* seg1, Seg* seg2, UWord result, HChar* opname)
{
   Seg* out;
#  if SC_SEGS
   checkSeg(seg1);
   checkSeg(seg2);
#  endif
   BINOP(
      return BOTTOM,
      out = NONPTR,  out = UNKNOWN, out = seg2,
      out = UNKNOWN, out = UNKNOWN, out = UNKNOWN,
      out = seg1,    out = UNKNOWN,       BINERROR(opname)
   );
   return ( looks_like_a_pointer(result) ? out : NONPTR );
}

static VG_REGPARM(3) Seg* do_addW(Seg* seg1, Seg* seg2, UWord result)
{
   Seg* out;
#  if SC_SEGS
   checkSeg(seg1);
   checkSeg(seg2);
#  endif
   out = do_addW_result(seg1, seg2, result, "Add32/Add64");
#  if SC_SEGS
   checkSeg(out);
#  endif
   return out;
}

// -------------
//  - | n  ?  p      (Nb: operation is seg1 - seg2)
// -------------
//  n | n  ?  n+     (+) happens a lot due to "cmp", but result should never
//  ? | ?  ?  n/B        be used, so give 'n'
//  p | p  p? n*/B   (*) and possibly link the segments
// -------------
static VG_REGPARM(3) Seg* do_subW(Seg* seg1, Seg* seg2, UWord result)
{
   Seg* out;
#  if SC_SEGS
   checkSeg(seg1);
   checkSeg(seg2);
#  endif
   // Nb: when returning BOTTOM, don't let it go through the range-check;
   //     a segment linking offset can easily look like a nonptr.
   BINOP(
      return BOTTOM,
      out = NONPTR,  out = UNKNOWN,    out = NONPTR,
      out = UNKNOWN, out = UNKNOWN,    return BOTTOM,
      out = seg1,    out = seg1/*??*/, return BOTTOM
   );
   #if 0
         // This is for the p-p segment-linking case
         Seg end2 = seg2;
         while (end2->links != seg2) end2 = end2->links;
         end2->links = seg1->links;
         seg1->links = seg2;
         return NONPTR;
   #endif
   return ( looks_like_a_pointer(result) ? out : NONPTR );
}

// -------------
//  & | n  ?  p
// -------------
//  n | n  ?  p
//  ? | ?  ?  ?
//  p | p  ?  *  (*) if p1==p2 then p else e (see comment)
// -------------
/* Seems to be OK to And two pointers:
     testq %ptr1,%ptr2
     jnz ..
   which possibly derives from
     if (ptr1 & ptr2) { A } else { B }
   not sure what that means
*/
static VG_REGPARM(3) Seg* do_andW(Seg* seg1, Seg* seg2, 
                                  UWord result, UWord args_diff)
{
   Seg* out;
   if (0 == args_diff) {
      // p1==p2
      out = seg1;
   } else {
      BINOP(
         return BOTTOM,
         out = NONPTR,  out = UNKNOWN, out = seg2,
         out = UNKNOWN, out = UNKNOWN, out = UNKNOWN,
         out = seg1,    out = UNKNOWN, out = NONPTR
                                       /*BINERROR("And32/And64")*/
      );
   }
   out = ( looks_like_a_pointer(result) ? out : NONPTR );
   return out;
}

// -------------
// `|`| n  ?  p
// -------------
//  n | n  ?  p
//  ? | ?  ?  ?
//  p | p  ?  n
// -------------
/* It's OK to Or two pointers together, but the result definitely
   isn't a pointer.  Why would you want to do that?  Because of this:
     char* p1 = malloc(..);
     char* p2 = malloc(..);
     ...
     if (p1 || p2) { .. }
   In this case gcc on x86/amd64 quite literally or-s the two pointers
   together and throws away the result, the purpose of which is merely
   to sets %eflags.Z/%rflags.Z.  So we have to allow it.
*/
static VG_REGPARM(3) Seg* do_orW(Seg* seg1, Seg* seg2, UWord result)
{
   Seg* out;
   BINOP(
      return BOTTOM,
      out = NONPTR,  out = UNKNOWN, out = seg2,
      out = UNKNOWN, out = UNKNOWN, out = UNKNOWN,
      out = seg1,    out = UNKNOWN, out = NONPTR
   );
   out = ( looks_like_a_pointer(result) ? out : NONPTR );
   return out;
}

// -------------
//  ~ | n  ?  p
// -------------
//    | n  n  n
// -------------
static VG_REGPARM(2) Seg* do_notW(Seg* seg1, UWord result)
{
#  if SC_SEGS
   checkSeg(seg1);
#  endif
   if (BOTTOM == seg1) return BOTTOM;
   return NONPTR;
}

// Pointers are rarely multiplied, but sometimes legitimately, eg. as hash
// function inputs.  But two pointers args --> error.
// Pretend it always returns a nonptr.  Maybe improve later.
static VG_REGPARM(2) Seg* do_mulW(Seg* seg1, Seg* seg2)
{
#  if SC_SEGS
   checkSeg(seg1);
   checkSeg(seg2);
#  endif
   if (is_known_segment(seg1) && is_known_segment(seg2))
      h_record_arith_error(seg1, seg2, "Mul32/Mul64");
   return NONPTR;
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

/* Find the tmp currently shadowing the given original tmp.  If none
   so far exists, allocate one.  */
static IRTemp findShadowTmp ( PCEnv* pce, IRTemp orig )
{
   TempMapEnt* ent;
   /* VG_(indexXA) range-checks 'orig', hence no need to check
      here. */
   ent = (TempMapEnt*)VG_(indexXA)( pce->qmpMap, (Word)orig );
   tl_assert(ent->kind == NonShad);
   if (ent->shadow == IRTemp_INVALID) {
      IRTemp shadow = newTemp( pce, pce->gWordTy, Shad );
      /* newTemp may cause pce->tmpMap to resize, hence previous results
         from VG_(indexXA) are invalid. */
      ent = (TempMapEnt*)VG_(indexXA)( pce->qmpMap, (Word)orig );
      tl_assert(ent->kind == NonShad);
      tl_assert(ent->shadow == IRTemp_INVALID);
      ent->shadow = shadow;
   }
   return ent->shadow;
}

/* Allocate a new shadow for the given original tmp.  This means any
   previous shadow is abandoned.  This is needed because it is
   necessary to give a new value to a shadow once it has been tested
   for undefinedness, but unfortunately IR's SSA property disallows
   this.  Instead we must abandon the old shadow, allocate a new one
   and use that instead.

   This is the same as findShadowTmp, except we don't bother to see
   if a shadow temp already existed -- we simply allocate a new one
   regardless. */
static IRTemp newShadowTmp ( PCEnv* pce, IRTemp orig )
{
   TempMapEnt* ent;
   /* VG_(indexXA) range-checks 'orig', hence no need to check
      here. */
   ent = (TempMapEnt*)VG_(indexXA)( pce->qmpMap, (Word)orig );
   tl_assert(ent->kind == NonShad);
   if (1) {
      IRTemp shadow = newTemp( pce, pce->gWordTy, Shad );
      /* newTemp may cause pce->tmpMap to resize, hence previous results
         from VG_(indexXA) are invalid. */
      ent = (TempMapEnt*)VG_(indexXA)( pce->qmpMap, (Word)orig );
      tl_assert(ent->kind == NonShad);
      ent->shadow = shadow;
      return shadow;
   }
   /* NOTREACHED */
   tl_assert(0);
}


/*------------------------------------------------------------*/
/*--- IRAtoms -- a subset of IRExprs                       ---*/
/*------------------------------------------------------------*/

/* An atom is either an IRExpr_Const or an IRExpr_Tmp, as defined by
   isIRAtom() in libvex_ir.h.  Because this instrumenter expects flat
   input, most of this code deals in atoms.  Usefully, a value atom
   always has a V-value which is also an atom: constants are shadowed
   by constants, and temps are shadowed by the corresponding shadow
   temporary. */

typedef  IRExpr  IRAtom;

//zz /* (used for sanity checks only): is this an atom which looks
//zz    like it's from original code? */
//zz static Bool isOriginalAtom ( PCEnv* pce, IRAtom* a1 )
//zz {
//zz    if (a1->tag == Iex_Const)
//zz       return True;
//zz    if (a1->tag == Iex_RdTmp && a1->Iex.RdTmp.tmp < pce->n_originalTmps)
//zz       return True;
//zz    return False;
//zz }
//zz 
//zz /* (used for sanity checks only): is this an atom which looks
//zz    like it's from shadow code? */
//zz static Bool isShadowAtom ( PCEnv* pce, IRAtom* a1 )
//zz {
//zz    if (a1->tag == Iex_Const)
//zz       return True;
//zz    if (a1->tag == Iex_RdTmp && a1->Iex.RdTmp.tmp >= pce->n_originalTmps)
//zz       return True;
//zz    return False;
//zz }
//zz 
//zz /* (used for sanity checks only): check that both args are atoms and
//zz    are identically-kinded. */
//zz static Bool sameKindedAtoms ( IRAtom* a1, IRAtom* a2 )
//zz {
//zz    if (a1->tag == Iex_RdTmp && a2->tag == Iex_RdTmp)
//zz       return True;
//zz    if (a1->tag == Iex_Const && a2->tag == Iex_Const)
//zz       return True;
//zz    return False;
//zz }


/*------------------------------------------------------------*/
/*--- Constructing IR fragments                            ---*/
/*------------------------------------------------------------*/

/* add stmt to a bb */
static inline void stmt ( HChar cat, PCEnv* pce, IRStmt* st ) {
   if (pce->trace) {
      VG_(printf)("  %c: ", cat);
      ppIRStmt(st);
      VG_(printf)("\n");
   }
   addStmtToIRSB(pce->sb, st);
}

/* assign value to tmp */
static inline
void assign ( HChar cat, PCEnv* pce, IRTemp tmp, IRExpr* expr ) {
   stmt(cat, pce, IRStmt_WrTmp(tmp,expr));
}

/* build various kinds of expressions */
#define binop(_op, _arg1, _arg2) IRExpr_Binop((_op),(_arg1),(_arg2))
#define unop(_op, _arg)          IRExpr_Unop((_op),(_arg))
#define mkU8(_n)                 IRExpr_Const(IRConst_U8(_n))
#define mkU16(_n)                IRExpr_Const(IRConst_U16(_n))
#define mkU32(_n)                IRExpr_Const(IRConst_U32(_n))
#define mkU64(_n)                IRExpr_Const(IRConst_U64(_n))
#define mkV128(_n)               IRExpr_Const(IRConst_V128(_n))
#define mkexpr(_tmp)             IRExpr_RdTmp((_tmp))

/* Bind the given expression to a new temporary, and return the
   temporary.  This effectively converts an arbitrary expression into
   an atom.

   'ty' is the type of 'e' and hence the type that the new temporary
   needs to be.  But passing it is redundant, since we can deduce the
   type merely by inspecting 'e'.  So at least that fact to assert
   that the two types agree. */
static IRAtom* assignNew ( HChar cat, PCEnv* pce, IRType ty, IRExpr* e ) {
   IRTemp t;
   IRType tyE = typeOfIRExpr(pce->sb->tyenv, e);
   tl_assert(tyE == ty); /* so 'ty' is redundant (!) */
   t = newTemp(pce, ty, Shad);
   assign(cat, pce, t, e);
   return mkexpr(t);
}



//-----------------------------------------------------------------------
// Approach taken for range-checking for NONPTR/UNKNOWN-ness as follows.
//
// Range check (NONPTR/seg): 
// - after modifying a word-sized value in/into a TempReg:
//    - {ADD, SUB, ADC, SBB, AND, OR, XOR, LEA, LEA2, NEG, NOT}L
//    - BSWAP
// 
// Range check (NONPTR/UNKNOWN):
// - when introducing a new word-sized value into a TempReg:
//    - MOVL l, t2
//
// - when copying a word-sized value which lacks a corresponding segment
//   into a TempReg:
//    - straddled LDL
//
// - when a sub-word of a word (or two) is updated:
//    - SHROTL
//    - {ADD, SUB, ADC, SBB, AND, OR, XOR, SHROT, NEG, NOT}[WB]
//    - PUT[WB]
//    - straddled   STL (2 range checks)
//    - straddled   STW (2 range checks)
//    - unstraddled STW
//    - STB
//    
// Just copy:
// - when copying word-sized values:
//    - MOVL t1, t2 (--optimise=no only)
//    - CMOV
//    - GETL, PUTL
//    - unstraddled LDL, unstraddled STL
//
// - when barely changing
//    - INC[LWB]/DEC[LWB]
// 
// Set to NONPTR:
// - after copying a sub-word value into a TempReg:
//    - MOV[WB] l, t2
//    - GET[WB]
//    - unstraddled LDW
//    - straddled   LDW
//    - LDB
//    - POP[WB]
//
// - after copying an obvious non-ptr into a TempReg:
//    - GETF
//    - CC2VAL
//    - POPL
//
// - after copying an obvious non-ptr into a memory word:
//    - FPU_W
// 
// Do nothing:
// - LOCK, INCEIP
// - WIDEN[WB]
// - JMP, JIFZ
// - CALLM_[SE], PUSHL, CALLM, CLEAR
// - FPU, FPU_R (and similar MMX/SSE ones)
//




/* Call h_fn (name h_nm) with the given arg, and return a new IRTemp
   holding the result.  The arg must be a word-typed atom.  Callee
   must be a VG_REGPARM(1) function. */
__attribute__((noinline))
static IRTemp gen_dirty_W_W ( PCEnv* pce, void* h_fn, HChar* h_nm, 
                              IRExpr* a1 )
{
   IRTemp   res;
   IRDirty* di;
   tl_assert(isIRAtom(a1));
   tl_assert(typeOfIRExpr(pce->sb->tyenv, a1) == pce->gWordTy);
   res = newTemp(pce, pce->gWordTy, Shad);
   di = unsafeIRDirty_1_N( res, 1/*regparms*/,
                           h_nm, VG_(fnptr_to_fnentry)( h_fn ),
                           mkIRExprVec_1( a1 ) );
   stmt( 'I', pce, IRStmt_Dirty(di) );
   return res;
}

/* Two-arg version of gen_dirty_W_W.  Callee must be a VG_REGPARM(2)
   function.*/
static IRTemp gen_dirty_W_WW ( PCEnv* pce, void* h_fn, HChar* h_nm, 
                               IRExpr* a1, IRExpr* a2 )
{
   IRTemp   res;
   IRDirty* di;
   tl_assert(isIRAtom(a1));
   tl_assert(isIRAtom(a2));
   tl_assert(typeOfIRExpr(pce->sb->tyenv, a1) == pce->gWordTy);
   tl_assert(typeOfIRExpr(pce->sb->tyenv, a2) == pce->gWordTy);
   res = newTemp(pce, pce->gWordTy, Shad);
   di = unsafeIRDirty_1_N( res, 2/*regparms*/,
                           h_nm, VG_(fnptr_to_fnentry)( h_fn ),
                           mkIRExprVec_2( a1, a2 ) );
   stmt( 'I', pce, IRStmt_Dirty(di) );
   return res;
}

/* Three-arg version of gen_dirty_W_W.  Callee must be a VG_REGPARM(3)
   function.*/
static IRTemp gen_dirty_W_WWW ( PCEnv* pce, void* h_fn, HChar* h_nm, 
                                IRExpr* a1, IRExpr* a2, IRExpr* a3 )
{
   IRTemp   res;
   IRDirty* di;
   tl_assert(isIRAtom(a1));
   tl_assert(isIRAtom(a2));
   tl_assert(isIRAtom(a3));
   tl_assert(typeOfIRExpr(pce->sb->tyenv, a1) == pce->gWordTy);
   tl_assert(typeOfIRExpr(pce->sb->tyenv, a2) == pce->gWordTy);
   tl_assert(typeOfIRExpr(pce->sb->tyenv, a3) == pce->gWordTy);
   res = newTemp(pce, pce->gWordTy, Shad);
   di = unsafeIRDirty_1_N( res, 3/*regparms*/,
                           h_nm, VG_(fnptr_to_fnentry)( h_fn ),
                           mkIRExprVec_3( a1, a2, a3 ) );
   stmt( 'I', pce, IRStmt_Dirty(di) );
   return res;
}

/* Four-arg version of gen_dirty_W_W.  Callee must be a VG_REGPARM(3)
   function.*/
static IRTemp gen_dirty_W_WWWW ( PCEnv* pce, void* h_fn, HChar* h_nm, 
                                 IRExpr* a1, IRExpr* a2,
                                 IRExpr* a3, IRExpr* a4 )
{
   IRTemp   res;
   IRDirty* di;
   tl_assert(isIRAtom(a1));
   tl_assert(isIRAtom(a2));
   tl_assert(isIRAtom(a3));
   tl_assert(isIRAtom(a4));
   tl_assert(typeOfIRExpr(pce->sb->tyenv, a1) == pce->gWordTy);
   tl_assert(typeOfIRExpr(pce->sb->tyenv, a2) == pce->gWordTy);
   tl_assert(typeOfIRExpr(pce->sb->tyenv, a3) == pce->gWordTy);
   tl_assert(typeOfIRExpr(pce->sb->tyenv, a4) == pce->gWordTy);
   res = newTemp(pce, pce->gWordTy, Shad);
   di = unsafeIRDirty_1_N( res, 3/*regparms*/,
                           h_nm, VG_(fnptr_to_fnentry)( h_fn ),
                           mkIRExprVec_4( a1, a2, a3, a4 ) );
   stmt( 'I', pce, IRStmt_Dirty(di) );
   return res;
}

/* Version of gen_dirty_W_WW with no return value.  Callee must be a
   VG_REGPARM(2) function.  If guard is non-NULL then it is used to
   conditionalise the call. */
static void gen_dirty_v_WW ( PCEnv* pce, IRExpr* guard,
                             void* h_fn, HChar* h_nm, 
                             IRExpr* a1, IRExpr* a2 )
{
   IRDirty* di;
   tl_assert(isIRAtom(a1));
   tl_assert(isIRAtom(a2));
   tl_assert(typeOfIRExpr(pce->sb->tyenv, a1) == pce->gWordTy);
   tl_assert(typeOfIRExpr(pce->sb->tyenv, a2) == pce->gWordTy);
   di = unsafeIRDirty_0_N( 2/*regparms*/,
                           h_nm, VG_(fnptr_to_fnentry)( h_fn ),
                           mkIRExprVec_2( a1, a2 ) );
   if (guard)
      di->guard = guard;
   stmt( 'I', pce, IRStmt_Dirty(di) );
}

/* Version of gen_dirty_W_WWW with no return value.  Callee must be a
   VG_REGPARM(3) function.*/
static void gen_dirty_v_WWW ( PCEnv* pce, void* h_fn, HChar* h_nm, 
                              IRExpr* a1, IRExpr* a2, IRExpr* a3 )
{
   IRDirty* di;
   tl_assert(isIRAtom(a1));
   tl_assert(isIRAtom(a2));
   tl_assert(isIRAtom(a3));
   tl_assert(typeOfIRExpr(pce->sb->tyenv, a1) == pce->gWordTy);
   tl_assert(typeOfIRExpr(pce->sb->tyenv, a2) == pce->gWordTy);
   tl_assert(typeOfIRExpr(pce->sb->tyenv, a3) == pce->gWordTy);
   di = unsafeIRDirty_0_N( 3/*regparms*/,
                           h_nm, VG_(fnptr_to_fnentry)( h_fn ),
                           mkIRExprVec_3( a1, a2, a3 ) );
   stmt( 'I', pce, IRStmt_Dirty(di) );
}

/* Version of gen_dirty_v_WWW for 4 arguments.  Callee must be a
   VG_REGPARM(3) function.*/
static void gen_dirty_v_WWWW ( PCEnv* pce, void* h_fn, HChar* h_nm, 
                               IRExpr* a1, IRExpr* a2,
                               IRExpr* a3, IRExpr* a4 )
{
   IRDirty* di;
   tl_assert(isIRAtom(a1));
   tl_assert(isIRAtom(a2));
   tl_assert(isIRAtom(a3));
   tl_assert(isIRAtom(a4));
   tl_assert(typeOfIRExpr(pce->sb->tyenv, a1) == pce->gWordTy);
   tl_assert(typeOfIRExpr(pce->sb->tyenv, a2) == pce->gWordTy);
   tl_assert(typeOfIRExpr(pce->sb->tyenv, a3) == pce->gWordTy);
   tl_assert(typeOfIRExpr(pce->sb->tyenv, a4) == pce->gWordTy);
   di = unsafeIRDirty_0_N( 3/*regparms*/,
                           h_nm, VG_(fnptr_to_fnentry)( h_fn ),
                           mkIRExprVec_4( a1, a2, a3, a4 ) );
   stmt( 'I', pce, IRStmt_Dirty(di) );
}

/* Version of gen_dirty_v_WWW for 6 arguments.  Callee must be a
   VG_REGPARM(3) function.*/
static void gen_dirty_v_6W ( PCEnv* pce, void* h_fn, HChar* h_nm, 
                             IRExpr* a1, IRExpr* a2, IRExpr* a3,
                             IRExpr* a4, IRExpr* a5, IRExpr* a6 )
{
   IRDirty* di;
   tl_assert(isIRAtom(a1));
   tl_assert(isIRAtom(a2));
   tl_assert(isIRAtom(a3));
   tl_assert(isIRAtom(a4));
   tl_assert(isIRAtom(a5));
   tl_assert(isIRAtom(a6));
   tl_assert(typeOfIRExpr(pce->sb->tyenv, a1) == pce->gWordTy);
   tl_assert(typeOfIRExpr(pce->sb->tyenv, a2) == pce->gWordTy);
   tl_assert(typeOfIRExpr(pce->sb->tyenv, a3) == pce->gWordTy);
   tl_assert(typeOfIRExpr(pce->sb->tyenv, a4) == pce->gWordTy);
   tl_assert(typeOfIRExpr(pce->sb->tyenv, a5) == pce->gWordTy);
   tl_assert(typeOfIRExpr(pce->sb->tyenv, a6) == pce->gWordTy);
   di = unsafeIRDirty_0_N( 3/*regparms*/,
                           h_nm, VG_(fnptr_to_fnentry)( h_fn ),
                           mkIRExprVec_6( a1, a2, a3, a4, a5, a6 ) );
   stmt( 'I', pce, IRStmt_Dirty(di) );
}

static IRAtom* uwiden_to_host_word ( PCEnv* pce, IRAtom* a )
{
   IRType a_ty = typeOfIRExpr(pce->sb->tyenv, a);
   tl_assert(isIRAtom(a));
   if (pce->hWordTy == Ity_I32) {
      switch (a_ty) {
         case Ity_I8:
            return assignNew( 'I', pce, Ity_I32, unop(Iop_8Uto32, a) );
         case Ity_I16:
            return assignNew( 'I', pce, Ity_I32, unop(Iop_16Uto32, a) );
         default:
            ppIRType(a_ty);
            tl_assert(0);
      }
   } else {
      tl_assert(pce->hWordTy == Ity_I64);
      switch (a_ty) {
         case Ity_I8:
            return assignNew( 'I', pce, Ity_I64, unop(Iop_8Uto64, a) );
         case Ity_I16:
            return assignNew( 'I', pce, Ity_I64, unop(Iop_16Uto64, a) );
         case Ity_I32:
            return assignNew( 'I', pce, Ity_I64, unop(Iop_32Uto64, a) );
         default:
            ppIRType(a_ty);
            tl_assert(0);
      }
   }
}

/* 'e' is a word-sized atom.  Call nonptr_or_unknown with it, bind the
   results to a new temporary, and return the temporary.  Note this
   takes an original expression but returns a shadow value. */
static IRTemp gen_call_nonptr_or_unknown_w ( PCEnv* pce, IRExpr* e )
{
   return gen_dirty_W_W( pce, &nonptr_or_unknown, 
                              "nonptr_or_unknown", e );
}


/* Generate the shadow value for an IRExpr which is an atom and
   guaranteed to be word-sized. */
static IRAtom* schemeEw_Atom ( PCEnv* pce, IRExpr* e )
{
   if (pce->gWordTy == Ity_I32) {
      if (e->tag == Iex_Const && e->Iex.Const.con->tag == Ico_U32) {
         IRTemp t;
         tl_assert(sizeof(UWord) == 4);
         t = gen_call_nonptr_or_unknown_w(pce, e);
         return mkexpr(t);
      }
      if (e->tag == Iex_RdTmp
          && typeOfIRExpr(pce->sb->tyenv, e) == Ity_I32) {
         return mkexpr( findShadowTmp(pce, e->Iex.RdTmp.tmp) );
      }
      /* there are no other word-sized atom cases */
   } else {
      if (e->tag == Iex_Const && e->Iex.Const.con->tag == Ico_U64) {
         IRTemp t;
         tl_assert(sizeof(UWord) == 8);
         //return mkU64( (ULong)(UWord)NONPTR );
         t = gen_call_nonptr_or_unknown_w(pce, e);
         return mkexpr(t);
      }
      if (e->tag == Iex_RdTmp
          && typeOfIRExpr(pce->sb->tyenv, e) == Ity_I64) {
         return mkexpr( findShadowTmp(pce, e->Iex.RdTmp.tmp) );
      }
      /* there are no other word-sized atom cases */
   }
   ppIRExpr(e);
   tl_assert(0);
}


static
void instrument_arithop ( PCEnv* pce,
                          IRTemp dst, /* already holds result */
                          IRTemp dstv, /* generate an assignment to this */
                          IROp op,
                          /* original args, guaranteed to be atoms */
                          IRExpr* a1, IRExpr* a2, IRExpr* a3, IRExpr* a4 )
{
   HChar*  nm  = NULL;
   void*   fn  = NULL;
   IRExpr* a1v = NULL;
   IRExpr* a2v = NULL;
   //IRExpr* a3v = NULL;
   //IRExpr* a4v = NULL;
   IRTemp  res = IRTemp_INVALID;

   if (pce->gWordTy == Ity_I32) {

      tl_assert(pce->hWordTy == Ity_I32);
      switch (op) {

         /* For these cases, pass Segs for both arguments, and the
            result value. */
         case Iop_Add32: nm = "do_addW"; fn = &do_addW; goto ssr32;
         case Iop_Sub32: nm = "do_subW"; fn = &do_subW; goto ssr32;
         case Iop_Or32:  nm = "do_orW";  fn = &do_orW;  goto ssr32;
         ssr32:
            a1v = schemeEw_Atom( pce, a1 );
            a2v = schemeEw_Atom( pce, a2 );
            res = gen_dirty_W_WWW( pce, fn, nm, a1v, a2v, mkexpr(dst) );
            assign( 'I', pce, dstv, mkexpr(res) );
            break;

         /* In this case, pass Segs for both arguments, the result
            value, and the difference between the (original) values of
            the arguments. */
         case Iop_And32:
            nm = "do_andW"; fn = &do_andW;
            a1v = schemeEw_Atom( pce, a1 );
            a2v = schemeEw_Atom( pce, a2 );
            res = gen_dirty_W_WWWW( 
                     pce, fn, nm, a1v, a2v, mkexpr(dst),
                     assignNew( 'I', pce, Ity_I32,
                                binop(Iop_Sub32,a1,a2) ) );
            assign( 'I', pce, dstv, mkexpr(res) );
            break;

         /* Pass one shadow arg and the result to the helper. */
         case Iop_Not32: nm = "do_notW"; fn = &do_notW; goto vr32;
         vr32:
            a1v = schemeEw_Atom( pce, a1 );
            res = gen_dirty_W_WW( pce, fn, nm, a1v, mkexpr(dst) );
            assign( 'I', pce, dstv, mkexpr(res) );
            break;

         /* Pass two shadow args only to the helper. */
         case Iop_Mul32: nm = "do_mulW"; fn = &do_mulW; goto vv32;
         vv32:
            a1v = schemeEw_Atom( pce, a1 );
            a2v = schemeEw_Atom( pce, a2 );
            res = gen_dirty_W_WW( pce, fn, nm, a1v, a2v );
            assign( 'I', pce, dstv, mkexpr(res) );
            break;

         /* We don't really know what the result could be; test at run
            time. */
         case Iop_64HIto32: goto n_or_u_32;
         case Iop_64to32:   goto n_or_u_32;
         case Iop_Xor32:    goto n_or_u_32;
         n_or_u_32:
            assign( 'I', pce, dstv,
                    mkexpr(
                       gen_call_nonptr_or_unknown_w( pce, 
                                                     mkexpr(dst) ) ) );
            break;

         /* Cases where it's very obvious that the result cannot be a
            pointer.  Hence declare directly that it's NONPTR; don't
            bother with the overhead of calling nonptr_or_unknown. */

         /* cases where it makes no sense for the result to be a ptr */
         /* FIXME: for Shl/Shr/Sar, really should do a test on the 2nd
            arg, so that shift by zero preserves the original
            value. */
         case Iop_Shl32:     goto n32;
         case Iop_Sar32:     goto n32;
         case Iop_Shr32:     goto n32;
         case Iop_16Uto32:   goto n32;
         case Iop_16Sto32:   goto n32;
         case Iop_F64toI32S: goto n32;
         case Iop_16HLto32:  goto n32;
         case Iop_MullS16:   goto n32;
         case Iop_MullU16:   goto n32;
         case Iop_PRemC3210F64: goto n32;
         case Iop_DivU32:    goto n32;
         case Iop_DivS32:    goto n32;
         case Iop_V128to32:  goto n32;

         /* cases where result range is very limited and clearly cannot
            be a pointer */
         case Iop_1Uto32: goto n32;
         case Iop_1Sto32: goto n32;
         case Iop_8Uto32: goto n32;
         case Iop_8Sto32: goto n32;
         case Iop_Clz32:  goto n32;
         case Iop_Ctz32:  goto n32;
         case Iop_CmpF64: goto n32;
         case Iop_CmpORD32S: goto n32;
         case Iop_CmpORD32U: goto n32;
         n32:
            assign( 'I', pce, dstv, mkU32( (UWord)NONPTR ));
            break;

         default:
            VG_(printf)("instrument_arithop(32-bit): unhandled: ");
            ppIROp(op);
            tl_assert(0);
      }

   } else {

      tl_assert(pce->gWordTy == Ity_I64);
      switch (op) {

         /* For these cases, pass Segs for both arguments, and the
            result value. */
         case Iop_Add64: nm = "do_addW"; fn = &do_addW; goto ssr64;
         case Iop_Sub64: nm = "do_subW"; fn = &do_subW; goto ssr64;
         case Iop_Or64:  nm = "do_orW";  fn = &do_orW;  goto ssr64;
         ssr64:
            a1v = schemeEw_Atom( pce, a1 );
            a2v = schemeEw_Atom( pce, a2 );
            res = gen_dirty_W_WWW( pce, fn, nm, a1v, a2v, mkexpr(dst) );
            assign( 'I', pce, dstv, mkexpr(res) );
            break;

         /* In this case, pass Segs for both arguments, the result
            value, and the difference between the (original) values of
            the arguments. */
         case Iop_And64:
            nm = "do_andW"; fn = &do_andW;
            a1v = schemeEw_Atom( pce, a1 );
            a2v = schemeEw_Atom( pce, a2 );
            res = gen_dirty_W_WWWW( 
                     pce, fn, nm, a1v, a2v, mkexpr(dst),
                     assignNew( 'I', pce, Ity_I64,
                                binop(Iop_Sub64,a1,a2) ) );
            assign( 'I', pce, dstv, mkexpr(res) );
            break;

         /* Pass one shadow arg and the result to the helper. */
         case Iop_Not64: nm = "do_notW"; fn = &do_notW; goto vr64;
         vr64:
            a1v = schemeEw_Atom( pce, a1 );
            res = gen_dirty_W_WW( pce, fn, nm, a1v, mkexpr(dst) );
            assign( 'I', pce, dstv, mkexpr(res) );
            break;

         /* Pass two shadow args only to the helper. */
         case Iop_Mul64: nm = "do_mulW"; fn = &do_mulW; goto vv64;
         vv64:
            a1v = schemeEw_Atom( pce, a1 );
            a2v = schemeEw_Atom( pce, a2 );
            res = gen_dirty_W_WW( pce, fn, nm, a1v, a2v );
            assign( 'I', pce, dstv, mkexpr(res) );
            break;

         /* We don't really know what the result could be; test at run
            time. */
         case Iop_Xor64:      goto n_or_u_64;
         case Iop_128HIto64:  goto n_or_u_64;
         case Iop_128to64:    goto n_or_u_64;
         case Iop_V128HIto64: goto n_or_u_64;
         case Iop_V128to64:   goto n_or_u_64;
         n_or_u_64:
            assign( 'I', pce, dstv,
                    mkexpr(
                       gen_call_nonptr_or_unknown_w( pce, 
                                                     mkexpr(dst) ) ) );
            break;

         /* Cases where it's very obvious that the result cannot be a
            pointer.  Hence declare directly that it's NONPTR; don't
            bother with the overhead of calling nonptr_or_unknown. */

         /* cases where it makes no sense for the result to be a ptr */
         /* FIXME: for Shl/Shr/Sar, really should do a test on the 2nd
            arg, so that shift by zero preserves the original
            value. */
         case Iop_Shl64:      goto n64;
         case Iop_Sar64:      goto n64;
         case Iop_Shr64:      goto n64;
         case Iop_32Uto64:    goto n64;
         case Iop_32Sto64:    goto n64;
         case Iop_16Uto64:    goto n64;
         case Iop_16Sto64:    goto n64;
         case Iop_32HLto64:   goto n64;
         case Iop_DivModU64to32: goto n64;
         case Iop_DivModS64to32: goto n64;
         case Iop_F64toI64S:     goto n64;
         case Iop_MullS32:    goto n64;
         case Iop_MullU32:    goto n64;
         case Iop_DivU64:     goto n64;
         case Iop_DivS64:     goto n64;
         case Iop_ReinterpF64asI64: goto n64;

         /* cases where result range is very limited and clearly cannot
            be a pointer */
         case Iop_1Uto64:        goto n64;
         case Iop_8Uto64:        goto n64;
         case Iop_8Sto64:        goto n64;
         case Iop_Ctz64:         goto n64;
         case Iop_Clz64:         goto n64;
         case Iop_CmpORD64S:     goto n64;
         case Iop_CmpORD64U:     goto n64;
         /* 64-bit simd */
         case Iop_Avg8Ux8: case Iop_Avg16Ux4:
         case Iop_Max16Sx4: case Iop_Max8Ux8: case Iop_Min16Sx4:
         case Iop_Min8Ux8: case Iop_MulHi16Ux4:
         case Iop_QNarrow32Sx2: case Iop_QNarrow16Sx4:
         case Iop_QNarrow16Ux4: case Iop_Add8x8: case Iop_Add32x2:
         case Iop_QAdd8Sx8: case Iop_QAdd16Sx4: case Iop_QAdd8Ux8:
         case Iop_QAdd16Ux4: case Iop_Add16x4: case Iop_CmpEQ8x8:
         case Iop_CmpEQ32x2: case Iop_CmpEQ16x4: case Iop_CmpGT8Sx8:
         case Iop_CmpGT32Sx2: case Iop_CmpGT16Sx4: case Iop_MulHi16Sx4:
         case Iop_Mul16x4: case Iop_ShlN32x2: case Iop_ShlN16x4:
         case Iop_SarN32x2: case Iop_SarN16x4: case Iop_ShrN32x2: case Iop_ShrN8x8:
         case Iop_ShrN16x4: case Iop_Sub8x8: case Iop_Sub32x2:
         case Iop_QSub8Sx8: case Iop_QSub16Sx4: case Iop_QSub8Ux8:
         case Iop_QSub16Ux4: case Iop_Sub16x4: case Iop_InterleaveHI8x8:
         case Iop_InterleaveHI32x2: case Iop_InterleaveHI16x4:
         case Iop_InterleaveLO8x8: case Iop_InterleaveLO32x2:
         case Iop_InterleaveLO16x4: case Iop_SarN8x8:
         case Iop_Perm8x8: case Iop_ShlN8x8: case Iop_Mul32x2:
         case Iop_CatEvenLanes16x4: case Iop_CatOddLanes16x4:
         n64:
            assign( 'I', pce, dstv, mkU64( (UWord)NONPTR ));
            break;

         default:
            VG_(printf)("instrument_arithop(64-bit): unhandled: ");
            ppIROp(op);
            tl_assert(0);
      }
   }
}

static 
void gen_call_nonptr_or_unknown_range ( PCEnv* pce,
                                        IRExpr* guard,
                                        IRAtom* addr, IRAtom* len )
{
   gen_dirty_v_WW( pce, guard,
                   &nonptr_or_unknown_range,
                   "nonptr_or_unknown_range",
                   addr, len );
}

/* iii describes zero or more non-exact integer register updates.  For
   each one, generate IR to get the containing register, apply
   nonptr_or_unknown to it, and write it back again. */
static void gen_nonptr_or_unknown_for_III( PCEnv* pce, IntRegInfo* iii )
{
   Int i;
   tl_assert(iii && iii->n_offsets >= 0);
   for (i = 0; i < iii->n_offsets; i++) {
      IRAtom* a1 = assignNew( 'I', pce, pce->gWordTy, 
                              IRExpr_Get( iii->offsets[i], pce->gWordTy ));
      IRTemp a2 = gen_call_nonptr_or_unknown_w( pce, a1 );
      stmt( 'I', pce, IRStmt_Put( iii->offsets[i] 
                                     + pce->guest_state_sizeB,
                                  mkexpr(a2) ));
   }
}


/* schemeS helper for doing stores, pulled out into a function because
   it needs to handle both normal stores and store-conditionals.
   Returns False if we see a case we don't know how to handle.
*/
static Bool schemeS_store ( PCEnv* pce,
                            IRExpr* data, IRExpr* addr, IRTemp resSC )
{
   /* We have: STle(addr) = data
      if data is int-word sized, do
      check_store4(addr, addr#, data, data#)
      for all other stores
      check_store{1,2}(addr, addr#, data)

      The helper actually *does* the store, so that it can do the
      post-hoc ugly hack of inspecting and "improving" the shadow data
      after the store, in the case where it isn't an aligned word
      store.

      Only word-sized values are shadowed.  If this is a
      store-conditional, .resSC will denote a non-word-typed temp, and
      so we don't need to shadow it.  Assert about the type, tho.
      However, since we're not re-emitting the original IRStmt_Store,
      but rather doing it as part of the helper function, we need to
      actually do a SC in the helper, and assign the result bit to
      .resSC.  Ugly.
   */
   IRType  d_ty  = typeOfIRExpr(pce->sb->tyenv, data);
   IRExpr* addrv = schemeEw_Atom( pce, addr );
   if (resSC != IRTemp_INVALID) {
      tl_assert(typeOfIRTemp(pce->sb->tyenv, resSC) == Ity_I1);
      /* viz, not something we want to shadow */
      /* also, throw out all store-conditional cases that
         we can't handle */
      if (pce->gWordTy == Ity_I32 && d_ty != Ity_I32)
         return False;
      if (pce->gWordTy == Ity_I64 && d_ty != Ity_I32 && d_ty != Ity_I64)
         return False;
   }
   if (pce->gWordTy == Ity_I32) {
      /* ------ 32 bit host/guest (cough, cough) ------ */
      switch (d_ty) {
         /* Integer word case */
         case Ity_I32: {
            IRExpr* datav = schemeEw_Atom( pce, data );
            if (resSC == IRTemp_INVALID) {
               /* "normal" store */
               gen_dirty_v_WWWW( pce,
                                 &check_store4_P, "check_store4_P",
                                 addr, addrv, data, datav );
            } else {
               /* store-conditional; need to snarf the success bit */
               IRTemp resSC32
                   = gen_dirty_W_WWWW( pce,
                                       &check_store4C_P,
                                       "check_store4C_P",
                                       addr, addrv, data, datav );
               /* presumably resSC32 will really be Ity_I32.  In
                  any case we'll get jumped by the IR sanity
                  checker if it's not, when it sees the
                  following statement. */
               assign( 'I', pce, resSC, unop(Iop_32to1, mkexpr(resSC32)) );
            }
            break;
         }
         /* Integer subword cases */
         case Ity_I16:
            gen_dirty_v_WWW( pce,
                             &check_store2, "check_store2",
                             addr, addrv,
                             uwiden_to_host_word( pce, data ));
            break;
         case Ity_I8:
            gen_dirty_v_WWW( pce,
                             &check_store1, "check_store1",
                             addr, addrv,
                             uwiden_to_host_word( pce, data ));
            break;
         /* 64-bit float.  Pass store data in 2 32-bit pieces. */
         case Ity_F64: {
            IRAtom* d64 = assignNew( 'I', pce, Ity_I64,
                                     unop(Iop_ReinterpF64asI64, data) );
            IRAtom* dLo32 = assignNew( 'I', pce, Ity_I32,
                                       unop(Iop_64to32, d64) );
            IRAtom* dHi32 = assignNew( 'I', pce, Ity_I32,
                                       unop(Iop_64HIto32, d64) );
            gen_dirty_v_WWWW( pce,
                              &check_store8_ms4B_ls4B, 
                              "check_store8_ms4B_ls4B",
                              addr, addrv, dHi32, dLo32 );
            break;
         }
         /* 32-bit float.  We can just use _store4, but need
            to futz with the argument type. */
         case Ity_F32: {
            IRAtom* i32 = assignNew( 'I', pce, Ity_I32, 
                                     unop(Iop_ReinterpF32asI32,
                                          data ) );
            gen_dirty_v_WWW( pce,
                             &check_store4,
                             "check_store4",
                             addr, addrv, i32 );
            break;
         }
         /* 64-bit int.  Pass store data in 2 32-bit pieces. */
         case Ity_I64: {
            IRAtom* dLo32 = assignNew( 'I', pce, Ity_I32,
                                       unop(Iop_64to32, data) );
            IRAtom* dHi32 = assignNew( 'I', pce, Ity_I32,
                                       unop(Iop_64HIto32, data) );
            gen_dirty_v_WWWW( pce,
                              &check_store8_ms4B_ls4B, 
                              "check_store8_ms4B_ls4B",
                              addr, addrv, dHi32, dLo32 );
            break;
         }
         /* 128-bit vector.  Pass store data in 4 32-bit pieces.
            This is all very ugly and inefficient, but it is
            hard to better without considerably complicating the
            store-handling schemes. */
         case Ity_V128: {
            IRAtom* dHi64 = assignNew( 'I', pce, Ity_I64,
                                       unop(Iop_V128HIto64, data) );
            IRAtom* dLo64 = assignNew( 'I', pce, Ity_I64,
                                       unop(Iop_V128to64, data) );
            IRAtom* w3    = assignNew( 'I', pce, Ity_I32,
                                       unop(Iop_64HIto32, dHi64) );
            IRAtom* w2    = assignNew( 'I', pce, Ity_I32,
                                       unop(Iop_64to32, dHi64) );
            IRAtom* w1    = assignNew( 'I', pce, Ity_I32,
                                       unop(Iop_64HIto32, dLo64) );
            IRAtom* w0    = assignNew( 'I', pce, Ity_I32,
                                       unop(Iop_64to32, dLo64) );
            gen_dirty_v_6W( pce,
                            &check_store16_ms4B_4B_4B_ls4B, 
                            "check_store16_ms4B_4B_4B_ls4B",
                            addr, addrv, w3, w2, w1, w0 );
            break;
         }
         default:
            ppIRType(d_ty); tl_assert(0);
      }
   } else {
      /* ------ 64 bit host/guest (cough, cough) ------ */
      switch (d_ty) {
         /* Integer word case */
         case Ity_I64: {
            IRExpr* datav = schemeEw_Atom( pce, data );
            if (resSC == IRTemp_INVALID) {
               /* "normal" store */
               gen_dirty_v_WWWW( pce,
                                 &check_store8_P, "check_store8_P",
                                 addr, addrv, data, datav );
            } else {
               IRTemp resSC64
                   = gen_dirty_W_WWWW( pce,
                                       &check_store8C_P,
                                       "check_store8C_P",
                                       addr, addrv, data, datav );
               assign( 'I', pce, resSC, unop(Iop_64to1, mkexpr(resSC64)) );
            }
            break;
         }
         /* Integer subword cases */
         case Ity_I32:
            if (resSC == IRTemp_INVALID) {
               /* "normal" store */
               gen_dirty_v_WWW( pce,
                                &check_store4, "check_store4",
                                addr, addrv,
                                uwiden_to_host_word( pce, data ));
            } else {
               /* store-conditional; need to snarf the success bit */
               IRTemp resSC64
                   = gen_dirty_W_WWW( pce,
                                      &check_store4C,
                                      "check_store4C",
                                      addr, addrv,
                                      uwiden_to_host_word( pce, data ));
               assign( 'I', pce, resSC, unop(Iop_64to1, mkexpr(resSC64)) );
            }
            break;
         case Ity_I16:
            gen_dirty_v_WWW( pce,
                             &check_store2, "check_store2",
                             addr, addrv,
                             uwiden_to_host_word( pce, data ));
            break;
         case Ity_I8:
            gen_dirty_v_WWW( pce,
                             &check_store1, "check_store1",
                             addr, addrv,
                             uwiden_to_host_word( pce, data ));
            break;
         /* 128-bit vector.  Pass store data in 2 64-bit pieces. */
         case Ity_V128: {
            IRAtom* dHi64 = assignNew( 'I', pce, Ity_I64,
                                       unop(Iop_V128HIto64, data) );
            IRAtom* dLo64 = assignNew( 'I', pce, Ity_I64,
                                       unop(Iop_V128to64, data) );
            gen_dirty_v_WWWW( pce,
                              &check_store16_ms8B_ls8B, 
                              "check_store16_ms8B_ls8B",
                              addr, addrv, dHi64, dLo64 );
            break;
         }
         /* 64-bit float. */
         case Ity_F64: {
            IRAtom* dI = assignNew( 'I', pce, Ity_I64, 
                                     unop(Iop_ReinterpF64asI64,
                                          data ) );
            gen_dirty_v_WWW( pce,
                             &check_store8_all8B,
                             "check_store8_all8B",
                             addr, addrv, dI );
            break;
         }
         /* 32-bit float.  We can just use _store4, but need
            to futz with the argument type. */
         case Ity_F32: {
            IRAtom* i32 = assignNew( 'I', pce, Ity_I32, 
                                     unop(Iop_ReinterpF32asI32,
                                          data ) );
            IRAtom* i64 = assignNew( 'I', pce, Ity_I64, 
                                     unop(Iop_32Uto64,
                                          i32 ) );
            gen_dirty_v_WWW( pce,
                             &check_store4,
                             "check_store4",
                             addr, addrv, i64 );
            break;
         }
         default:
            ppIRType(d_ty); tl_assert(0);
      }
   }
   /* And don't copy the original, since the helper does the store.
      Ick. */
   return True; /* store was successfully instrumented */
}


/* schemeS helper for doing loads, pulled out into a function because
   it needs to handle both normal loads and load-linked's.
*/
static void schemeS_load ( PCEnv* pce, IRExpr* addr, IRType e_ty, IRTemp dstv )
{
   HChar*  h_nm  = NULL;
   void*   h_fn  = NULL;
   IRExpr* addrv = NULL;
   if (e_ty == pce->gWordTy) {
      tl_assert(dstv != IRTemp_INVALID);
   } else {
      tl_assert(dstv == IRTemp_INVALID);
   }
   if (pce->gWordTy == Ity_I32) {
      /* 32 bit host/guest (cough, cough) */
      switch (e_ty) {
         /* Ity_I32: helper returns shadow value. */
         case Ity_I32:  h_fn = &check_load4_P;
                        h_nm = "check_load4_P"; break;
         /* all others: helper does not return a shadow
            value. */
         case Ity_V128: h_fn = &check_load16;
                        h_nm = "check_load16"; break;
         case Ity_I64:
         case Ity_F64:  h_fn = &check_load8;
                        h_nm = "check_load8"; break;
         case Ity_F32:  h_fn = &check_load4;
                        h_nm = "check_load4"; break;
         case Ity_I16:  h_fn = &check_load2;
                        h_nm = "check_load2"; break;
         case Ity_I8:   h_fn = &check_load1;
                        h_nm = "check_load1"; break;
         default: ppIRType(e_ty); tl_assert(0);
      }
      addrv = schemeEw_Atom( pce, addr );
      if (e_ty == Ity_I32) {
         assign( 'I', pce, dstv, 
                  mkexpr( gen_dirty_W_WW( pce, h_fn, h_nm,
                                               addr, addrv )) );
      } else {
         gen_dirty_v_WW( pce, NULL, h_fn, h_nm, addr, addrv );
      }
   } else {
      /* 64 bit host/guest (cough, cough) */
      switch (e_ty) {
         /* Ity_I64: helper returns shadow value. */
         case Ity_I64:  h_fn = &check_load8_P;
                        h_nm = "check_load8_P"; break;
         /* all others: helper does not return a shadow
            value. */
         case Ity_V128: h_fn = &check_load16;
                        h_nm = "check_load16"; break;
         case Ity_F64:  h_fn = &check_load8;
                        h_nm = "check_load8"; break;
         case Ity_F32:
         case Ity_I32:  h_fn = &check_load4;
                        h_nm = "check_load4"; break;
         case Ity_I16:  h_fn = &check_load2;
                        h_nm = "check_load2"; break;
         case Ity_I8:   h_fn = &check_load1;
                        h_nm = "check_load1"; break;
         default: ppIRType(e_ty); tl_assert(0);
      }
      addrv = schemeEw_Atom( pce, addr );
      if (e_ty == Ity_I64) {
         assign( 'I', pce, dstv, 
                  mkexpr( gen_dirty_W_WW( pce, h_fn, h_nm,
                                               addr, addrv )) );
      } else {
         gen_dirty_v_WW( pce, NULL, h_fn, h_nm, addr, addrv );
      }
   }
}


/* Generate into 'pce', instrumentation for 'st'.  Also copy 'st'
   itself into 'pce' (the caller does not do so).  This is somewhat
   complex and relies heavily on the assumption that the incoming IR
   is in flat form.

   Generally speaking, the instrumentation is placed after the
   original statement, so that results computed by the original can be
   used in the instrumentation.  However, that isn't safe for memory
   references, since we need the instrumentation (hence bounds check
   and potential error message) to happen before the reference itself,
   as the latter could cause a fault. */
static void schemeS ( PCEnv* pce, IRStmt* st )
{
   tl_assert(st);
   tl_assert(isFlatIRStmt(st));

   switch (st->tag) {

      case Ist_CAS: {
         /* In all these CAS cases, the did-we-succeed? comparison is
            done using Iop_CasCmpEQ{8,16,32,64} rather than the plain
            Iop_CmpEQ equivalents.  This isn't actually necessary,
            since the generated IR is not going to be subsequently
            instrumented by Memcheck.  But it's done for consistency.
            See COMMENT_ON_CasCmpEQ in memcheck/mc_translate.c for
            background/rationale. */
         IRCAS* cas = st->Ist.CAS.details;
         IRType elTy = typeOfIRExpr(pce->sb->tyenv, cas->expdLo);
         if (cas->oldHi == IRTemp_INVALID) {
            /* ------------ SINGLE CAS ------------ */
            /* -- single cas -- 32 bits, on 32-bit host -- */
            /* -- single cas -- 64 bits, on 64-bit host -- */
            /* -- viz, single cas, native-word case -- */
            if ( (pce->gWordTy == Ity_I32 && elTy == Ity_I32)
                 || (pce->gWordTy == Ity_I64 && elTy == Ity_I64) ) {
               // 32 bit host translation scheme; 64-bit is analogous
               // old#    = check_load4_P(addr, addr#)
               // old     = CAS(addr:expd->new) [COPY]
               // success = CasCmpEQ32(old,expd)
               // if (success) do_shadow_store4_P(addr, new#)
               IRTemp  success;
               Bool    is64  = elTy == Ity_I64;
               IROp    cmpEQ = is64 ? Iop_CasCmpEQ64 : Iop_CasCmpEQ32;
               void*   r_fn  = is64 ? &check_load8_P  : &check_load4_P;
               HChar*  r_nm  = is64 ? "check_load8_P" : "check_load4_P";
               void*   w_fn  = is64 ? &do_shadow_store8_P  : &do_shadow_store4_P;
               void*   w_nm  = is64 ? "do_shadow_store8_P" : "do_shadow_store4_P";
               IRExpr* addr  = cas->addr;
               IRExpr* addrV = schemeEw_Atom(pce, addr);
               IRTemp  old   = cas->oldLo;
               IRTemp  oldV  = newShadowTmp(pce, old);
               IRExpr* nyu   = cas->dataLo;
               IRExpr* nyuV  = schemeEw_Atom(pce, nyu);
               IRExpr* expd  = cas->expdLo;
               assign( 'I', pce, oldV, 
                       mkexpr( gen_dirty_W_WW( pce, r_fn, r_nm, addr, addrV )));
               stmt( 'C', pce, st );
               success = newTemp(pce, Ity_I1, NonShad);
               assign('I', pce, success, binop(cmpEQ, mkexpr(old), expd));
               gen_dirty_v_WW( pce, mkexpr(success), w_fn, w_nm, addr, nyuV );
            }
            else
            /* -- single cas -- 8 or 16 bits, on 32-bit host -- */
            /* -- viz, single cas, 32-bit subword cases -- */
            if (pce->gWordTy == Ity_I32
                && (elTy == Ity_I8 || elTy == Ity_I16)) {
               // 8-bit translation scheme; 16-bit is analogous
               // check_load1(addr, addr#)
               // old     = CAS(addr:expd->new) [COPY]
               // success = CasCmpEQ8(old,expd)
               // if (success) nonptr_or_unknown_range(addr, 1)
               IRTemp  success;
               Bool    is16  = elTy == Ity_I16;
               IRExpr* addr  = cas->addr;
               IRExpr* addrV = schemeEw_Atom(pce, addr);
               IRTemp  old   = cas->oldLo;
               IRExpr* expd  = cas->expdLo;
               void*   h_fn  = is16 ? &check_load2  : &check_load1;
               HChar*  h_nm  = is16 ? "check_load2" : "check_load1";
               IROp    cmpEQ = is16 ? Iop_CasCmpEQ16 : Iop_CasCmpEQ8;
               Int     szB   = is16 ? 2 : 1;
               gen_dirty_v_WW( pce, NULL, h_fn, h_nm, addr, addrV );
               stmt( 'C', pce, st );
               success = newTemp(pce, Ity_I1, NonShad);
               assign('I', pce, success,
                           binop(cmpEQ, mkexpr(old), expd));
               gen_call_nonptr_or_unknown_range( pce, mkexpr(success),
                                                 addr, mkIRExpr_HWord(szB) );
            }
            else
            /* -- single cas -- 8, 16 or 32 bits, on 64-bit host -- */
            /* -- viz, single cas, 64-bit subword cases -- */
            if (pce->gWordTy == Ity_I64
                && (elTy == Ity_I8 || elTy == Ity_I16 || elTy == Ity_I32)) {
               // 8-bit translation scheme; 16/32-bit are analogous
               // check_load1(addr, addr#)
               // old     = CAS(addr:expd->new) [COPY]
               // success = CasCmpEQ8(old,expd)
               // if (success) nonptr_or_unknown_range(addr, 1)
               IRTemp  success;
               Bool    is16  = elTy == Ity_I16;
               Bool    is32  = elTy == Ity_I32;
               IRExpr* addr  = cas->addr;
               IRExpr* addrV = schemeEw_Atom(pce, addr);
               IRTemp  old   = cas->oldLo;
               IRExpr* expd  = cas->expdLo;
               void*   h_fn  = is32 ? &check_load4
                                    : (is16 ? &check_load2 : &check_load1);
               HChar*  h_nm  = is32 ? "check_load4" 
                                    : (is16 ? "check_load2" : "check_load1");
               IROp    cmpEQ = is32 ? Iop_CasCmpEQ32
                                    : (is16 ? Iop_CasCmpEQ16 : Iop_CasCmpEQ8);
               Int     szB   = is32 ? 4 : (is16 ? 2 : 1);
               gen_dirty_v_WW( pce, NULL, h_fn, h_nm, addr, addrV );
               stmt( 'C', pce, st );
               success = newTemp(pce, Ity_I1, NonShad);
               assign('I', pce, success,
                           binop(cmpEQ, mkexpr(old), expd));
               gen_call_nonptr_or_unknown_range( pce, mkexpr(success),
                                                 addr, mkIRExpr_HWord(szB) );
            }
            else
               goto unhandled;
         } else {
            /* ------------ DOUBLE CAS ------------ */
            /* Punt on bigendian DCAS.  In fact it's probably trivial
               to do; just swap the individual shadow loads/stores
               around in memory, but we'd have to verify it, and there
               is no use case.  So punt. */
            if (cas->end != Iend_LE)
               goto unhandled;
            /* -- double cas -- 2 x 32 bits, on 32-bit host -- */
            /* -- double cas -- 2 x 64 bits, on 64-bit host -- */
            /* -- viz, double cas, native-word case -- */
            if ( (pce->gWordTy == Ity_I32 && elTy == Ity_I32)
                 || (pce->gWordTy == Ity_I64 && elTy == Ity_I64) ) {
               // 32 bit host translation scheme; 64-bit is analogous
               // oldHi#    = check_load4_P(addr+4, addr#)
               // oldLo#    = check_load4_P(addr+0, addr#)
               // oldHi/Lo  = DCAS(addr:expdHi/Lo->newHi/Lo) [COPY]
               // success   = CasCmpEQ32(oldHi,expdHi) && CasCmpEQ32(oldLo,expdLo)
               //           = ((oldHi ^ expdHi) | (oldLo ^ expdLo)) == 0
               // if (success) do_shadow_store4_P(addr+4, newHi#)
               // if (success) do_shadow_store4_P(addr+0, newLo#)
               IRTemp  diffHi, diffLo, diff, success, addrpp;
               Bool    is64       = elTy == Ity_I64;
               void*   r_fn       = is64 ? &check_load8_P  : &check_load4_P;
               HChar*  r_nm       = is64 ? "check_load8_P" : "check_load4_P";
               void*   w_fn       = is64 ? &do_shadow_store8_P
                                         : &do_shadow_store4_P;
               void*   w_nm       = is64 ? "do_shadow_store8_P"
                                         : "do_shadow_store4_P";
               IROp    opADD      = is64 ? Iop_Add64 : Iop_Add32;
               IROp    opXOR      = is64 ? Iop_Xor64 : Iop_Xor32;
               IROp    opOR       = is64 ? Iop_Or64 : Iop_Or32;
               IROp    opCasCmpEQ = is64 ? Iop_CasCmpEQ64 : Iop_CasCmpEQ32;
               IRExpr* step       = is64 ? mkU64(8) : mkU32(4);
               IRExpr* zero       = is64 ? mkU64(0) : mkU32(0);
               IRExpr* addr       = cas->addr;
               IRExpr* addrV      = schemeEw_Atom(pce, addr);
               IRTemp  oldLo      = cas->oldLo;
               IRTemp  oldLoV     = newShadowTmp(pce, oldLo);
               IRTemp  oldHi      = cas->oldHi;
               IRTemp  oldHiV     = newShadowTmp(pce, oldHi);
               IRExpr* nyuLo      = cas->dataLo;
               IRExpr* nyuLoV     = schemeEw_Atom(pce, nyuLo);
               IRExpr* nyuHi      = cas->dataHi;
               IRExpr* nyuHiV     = schemeEw_Atom(pce, nyuHi);
               IRExpr* expdLo     = cas->expdLo;
               IRExpr* expdHi     = cas->expdHi;
               tl_assert(elTy == Ity_I32 || elTy == Ity_I64);
               tl_assert(pce->gWordTy == elTy);
               addrpp = newTemp(pce, elTy, NonShad);
               assign('I', pce, addrpp, binop(opADD, addr, step));
               assign('I', pce, oldHiV,
                      mkexpr( gen_dirty_W_WW( pce, r_fn, r_nm,
                                                   mkexpr(addrpp), addrV ))
               );
               assign('I', pce, oldLoV,
                      mkexpr( gen_dirty_W_WW( pce, r_fn, r_nm,
                                                   addr, addrV ))
               );
               stmt( 'C', pce, st );
               diffHi = newTemp(pce, elTy, NonShad);
               assign('I', pce, diffHi,
                           binop(opXOR, mkexpr(oldHi), expdHi));
               diffLo = newTemp(pce, elTy, NonShad);
               assign('I', pce, diffLo,
                           binop(opXOR, mkexpr(oldLo), expdLo));
               diff = newTemp(pce, elTy, NonShad);
               assign('I', pce, diff,
                      binop(opOR, mkexpr(diffHi), mkexpr(diffLo)));
               success = newTemp(pce, Ity_I1, NonShad);
               assign('I', pce, success,
                      binop(opCasCmpEQ, mkexpr(diff), zero));
               gen_dirty_v_WW( pce, mkexpr(success),
                                     w_fn, w_nm, mkexpr(addrpp), nyuHiV );
               gen_dirty_v_WW( pce, mkexpr(success),
                                    w_fn, w_nm, addr, nyuLoV );
            }
            else
            /* -- double cas -- 2 x 32 bits, on 64-bit host -- */
            if (pce->gWordTy == Ity_I64 && elTy == Ity_I32) {
               // check_load8(addr, addr#)
               // oldHi/Lo  = DCAS(addr:expdHi/Lo->newHi/Lo) [COPY]
               // success   = CasCmpEQ32(oldHi,expdHi) && CasCmpEQ32(oldLo,expdLo)
               //           = ((oldHi ^ expdHi) | (oldLo ^ expdLo)) == 0
               // if (success) nonptr_or_unknown_range(addr, 8)
               IRTemp  diffHi, diffLo, diff, success;
               IRExpr* addr   = cas->addr;
               IRExpr* addrV  = schemeEw_Atom(pce, addr);
               IRTemp  oldLo  = cas->oldLo;
               IRTemp  oldHi  = cas->oldHi;
               IRExpr* expdLo = cas->expdLo;
               IRExpr* expdHi = cas->expdHi;
               gen_dirty_v_WW( pce, NULL, &check_load8, "check_load8",
                               addr, addrV );
               stmt( 'C', pce, st );
               diffHi = newTemp(pce, Ity_I32, NonShad);
               assign('I', pce, diffHi,
                           binop(Iop_Xor32, mkexpr(oldHi), expdHi));
               diffLo = newTemp(pce, Ity_I32, NonShad);
               assign('I', pce, diffLo,
                           binop(Iop_Xor32, mkexpr(oldLo), expdLo));
               diff = newTemp(pce, Ity_I32, NonShad);
               assign('I', pce, diff,
                      binop(Iop_Or32, mkexpr(diffHi), mkexpr(diffLo)));
               success = newTemp(pce, Ity_I1, NonShad);
               assign('I', pce, success,
                      binop(Iop_CasCmpEQ32, mkexpr(diff), mkU32(0)));
               gen_call_nonptr_or_unknown_range( pce, mkexpr(success),
                                                 addr, mkU64(8) );
            }
            else
               goto unhandled;
         }
         break;
      }

      case Ist_LLSC: {
         if (st->Ist.LLSC.storedata == NULL) {
            /* LL */
            IRTemp dst    = st->Ist.LLSC.result;
            IRType dataTy = typeOfIRTemp(pce->sb->tyenv, dst);
            Bool   isWord = dataTy == pce->gWordTy;
            IRTemp dstv   = isWord ? newShadowTmp( pce, dst )
                                   : IRTemp_INVALID;
            schemeS_load( pce, st->Ist.LLSC.addr, dataTy, dstv );
            /* copy the original -- must happen after the helper call */
            stmt( 'C', pce, st );
         } else {
            /* SC */
            schemeS_store( pce,
                           st->Ist.LLSC.storedata,
                           st->Ist.LLSC.addr,
                           st->Ist.LLSC.result );
            /* Don't copy the original, since the helper does the
               store itself. */
         }
         break;
      }

      case Ist_Dirty: {
         Int i;
         IRDirty* di;
         stmt( 'C', pce, st );
         /* nasty.  assumes that (1) all helpers are unconditional,
            and (2) all outputs are non-ptr */
         di = st->Ist.Dirty.details;
         /* deal with the return tmp, if any */
         if (di->tmp != IRTemp_INVALID
             && typeOfIRTemp(pce->sb->tyenv, di->tmp) == pce->gWordTy) {
            /* di->tmp is shadowed.  Set it to NONPTR. */
            IRTemp dstv = newShadowTmp( pce, di->tmp );
            if (pce->gWordTy == Ity_I32) {
              assign( 'I', pce, dstv, mkU32( (UWord)NONPTR ));
            } else {
              assign( 'I', pce, dstv, mkU64( (UWord)NONPTR ));
            }
         }
         /* apply the nonptr_or_unknown technique to any parts of
            the guest state that happen to get written */
         for (i = 0; i < di->nFxState; i++) {
            IntRegInfo iii;
            tl_assert(di->fxState[i].fx != Ifx_None);
            if (di->fxState[i].fx == Ifx_Read)
               continue; /* this bit is only read -- not interesting */
            get_IntRegInfo( &iii, di->fxState[i].offset,
                                  di->fxState[i].size );
            tl_assert(iii.n_offsets >= -1 
                      && iii.n_offsets <= N_INTREGINFO_OFFSETS);
            /* Deal with 3 possible cases, same as with Ist_Put
               elsewhere in this function. */
            if (iii.n_offsets == -1) {
               /* case (1): exact write of an integer register. */
               IRAtom* a1
                  = assignNew( 'I', pce, pce->gWordTy, 
                               IRExpr_Get( iii.offsets[i], pce->gWordTy ));
               IRTemp a2 = gen_call_nonptr_or_unknown_w( pce, a1 );
               stmt( 'I', pce, IRStmt_Put( iii.offsets[i] 
                                              + pce->guest_state_sizeB,
                                           mkexpr(a2) ));
            } else {
               /* when == 0: case (3): no instrumentation needed */
               /* when > 0: case (2) .. complex case.  Fish out the
                  stored value for the whole register, heave it
                  through nonptr_or_unknown, and use that as the new
                  shadow value. */
               tl_assert(iii.n_offsets >= 0 
                         && iii.n_offsets <= N_INTREGINFO_OFFSETS);
               gen_nonptr_or_unknown_for_III( pce, &iii );
            }
         } /* for (i = 0; i < di->nFxState; i++) */
         /* finally, deal with memory outputs */
         if (di->mFx != Ifx_None) {
            tl_assert(di->mAddr && isIRAtom(di->mAddr));
            tl_assert(di->mSize > 0);
            gen_call_nonptr_or_unknown_range( pce, NULL, di->mAddr,
                                              mkIRExpr_HWord(di->mSize));
         }
         break;
      }

      case Ist_NoOp:
         break;

      /* nothing interesting in these; just copy them through */
      case Ist_AbiHint:
      case Ist_MBE:
      case Ist_Exit:
      case Ist_IMark:
         stmt( 'C', pce, st );
         break;

      case Ist_PutI: {
         IRRegArray* descr = st->Ist.PutI.descr;
         stmt( 'C', pce, st );
         tl_assert(descr && descr->elemTy);
         if (is_integer_guest_reg_array(descr)) {
            /* if this fails, is_integer_guest_reg_array is returning
               bogus results */
            tl_assert(descr->elemTy == pce->gWordTy);
            stmt(
               'I', pce,
               IRStmt_PutI(
                  mkIRRegArray(descr->base + pce->guest_state_sizeB,
                               descr->elemTy, descr->nElems),
                  st->Ist.PutI.ix,
                  st->Ist.PutI.bias,
                  schemeEw_Atom( pce, st->Ist.PutI.data)
               )
            );
         }
         break;
      }

      case Ist_Put: {
         /* PUT(offset) = atom */
         /* 3 cases:
            1. It's a complete write of an integer register.  Get hold of
               'atom's shadow value and write it in the shadow state.
            2. It's a partial write of an integer register.  Let the write
               happen, then fish out the complete register value and see if,
               via range checking, consultation of tea leaves, etc, its
               shadow value can be upgraded to anything useful.
            3. It is none of the above.  Generate no instrumentation. */
         IntRegInfo iii;
         IRType     ty;
         stmt( 'C', pce, st );
         ty = typeOfIRExpr(pce->sb->tyenv, st->Ist.Put.data);
         get_IntRegInfo( &iii, st->Ist.Put.offset,
                         sizeofIRType(ty) );
         if (iii.n_offsets == -1) {
            /* case (1): exact write of an integer register. */
            tl_assert(ty == pce->gWordTy);
            stmt( 'I', pce,
                       IRStmt_Put( st->Ist.Put.offset
                                      + pce->guest_state_sizeB,
                                   schemeEw_Atom( pce, st->Ist.Put.data)) );
         } else {
            /* when == 0: case (3): no instrumentation needed */
            /* when > 0: case (2) .. complex case.  Fish out the
               stored value for the whole register, heave it through
               nonptr_or_unknown, and use that as the new shadow
               value. */
            tl_assert(iii.n_offsets >= 0 
                      && iii.n_offsets <= N_INTREGINFO_OFFSETS);
            gen_nonptr_or_unknown_for_III( pce, &iii );
         }
         break;
      } /* case Ist_Put */

      case Ist_Store: {
         Bool ok = schemeS_store( pce,
                                  st->Ist.Store.data,
                                  st->Ist.Store.addr,
                                  IRTemp_INVALID/*not a SC*/ );
         if (!ok) goto unhandled;
         /* Don't copy the original, since the helper does the store
            itself. */
         break;
      }

      case Ist_WrTmp: {
         /* This is the only place we have to deal with the full
            IRExpr range.  In all other places where an IRExpr could
            appear, we in fact only get an atom (Iex_RdTmp or
            Iex_Const). */
         IRExpr* e      = st->Ist.WrTmp.data;
         IRType  e_ty   = typeOfIRExpr( pce->sb->tyenv, e );
         Bool    isWord = e_ty == pce->gWordTy;
         IRTemp  dst    = st->Ist.WrTmp.tmp;
         IRTemp  dstv   = isWord ? newShadowTmp( pce, dst )
                                 : IRTemp_INVALID;

         switch (e->tag) {

            case Iex_Const: {
               stmt( 'C', pce, st );
               if (isWord)
                  assign( 'I', pce, dstv, schemeEw_Atom( pce, e ) );
               break;
            }

            case Iex_CCall: {
               stmt( 'C', pce, st );
               if (isWord)
                  assign( 'I', pce, dstv,
                          mkexpr( gen_call_nonptr_or_unknown_w( 
                                     pce, mkexpr(dst)))); 
               break;
            }

            case Iex_Mux0X: {
               /* Just steer the shadow values in the same way as the
                  originals. */
               stmt( 'C', pce, st );
               if (isWord)
                  assign( 'I', pce, dstv, 
                          IRExpr_Mux0X(
                             e->Iex.Mux0X.cond,
                             schemeEw_Atom( pce, e->Iex.Mux0X.expr0 ),
                             schemeEw_Atom( pce, e->Iex.Mux0X.exprX ) ));
               break;
            }

            case Iex_RdTmp: {
               stmt( 'C', pce, st );
               if (isWord)
                  assign( 'I', pce, dstv, schemeEw_Atom( pce, e ));
               break;
            }

            case Iex_Load: {
               schemeS_load( pce, e->Iex.Load.addr, e_ty, dstv );
               /* copy the original -- must happen after the helper call */
               stmt( 'C', pce, st );
               break;
            }

            case Iex_GetI: {
               IRRegArray* descr = e->Iex.GetI.descr;
               stmt( 'C', pce, st );
               tl_assert(descr && descr->elemTy);
               if (is_integer_guest_reg_array(descr)) {
                  /* if this fails, is_integer_guest_reg_array is
                     returning bogus results */
                  tl_assert(isWord);
                  assign(
                     'I', pce, dstv,
                     IRExpr_GetI(
                        mkIRRegArray(descr->base + pce->guest_state_sizeB,
                                     descr->elemTy, descr->nElems),
                        e->Iex.GetI.ix,
                        e->Iex.GetI.bias
                     )
                  );
               }
               break;
            }

            case Iex_Get: {
               stmt( 'C', pce, st );
               if (isWord) {
                  /* guest-word-typed tmp assignment, so it will have a
                     shadow tmp, and we must make an assignment to
                     that */
                  if (is_integer_guest_reg(e->Iex.Get.offset,
                                           sizeofIRType(e->Iex.Get.ty))) {
                     assign( 'I', pce, dstv,
                             IRExpr_Get( e->Iex.Get.offset 
                                            + pce->guest_state_sizeB,
                                         e->Iex.Get.ty) );
                  } else {
                     if (pce->hWordTy == Ity_I32) {
                        assign( 'I', pce, dstv, mkU32( (UWord)NONPTR ));
                     } else {
                       assign( 'I', pce, dstv, mkU64( (UWord)NONPTR ));
                     }
                  }
               } else {
                  /* tmp isn't guest-word-typed, so isn't shadowed, so
                     generate no instrumentation */
               }
               break;
            }

            case Iex_Unop: {
               stmt( 'C', pce, st );
               tl_assert(isIRAtom(e->Iex.Unop.arg));
               if (isWord)
                  instrument_arithop( pce, dst, dstv, e->Iex.Unop.op,
                                      e->Iex.Unop.arg,
                                      NULL, NULL, NULL );
               break;
            }

            case Iex_Binop: {
               stmt( 'C', pce, st );
               tl_assert(isIRAtom(e->Iex.Binop.arg1));
               tl_assert(isIRAtom(e->Iex.Binop.arg2));
               if (isWord)
                  instrument_arithop( pce, dst, dstv, e->Iex.Binop.op,
                                      e->Iex.Binop.arg1, e->Iex.Binop.arg2,
                                      NULL, NULL );
               break;
            }

            case Iex_Triop: {
               stmt( 'C', pce, st );
               tl_assert(isIRAtom(e->Iex.Triop.arg1));
               tl_assert(isIRAtom(e->Iex.Triop.arg2));
               tl_assert(isIRAtom(e->Iex.Triop.arg3));
               if (isWord)
                  instrument_arithop( pce, dst, dstv, e->Iex.Triop.op,
                                      e->Iex.Triop.arg1, e->Iex.Triop.arg2,
                                      e->Iex.Triop.arg3, NULL );
               break;
            }

            case Iex_Qop: {
               stmt( 'C', pce, st );
               tl_assert(isIRAtom(e->Iex.Qop.arg1));
               tl_assert(isIRAtom(e->Iex.Qop.arg2));
               tl_assert(isIRAtom(e->Iex.Qop.arg3));
               tl_assert(isIRAtom(e->Iex.Qop.arg4));
               if (isWord)
                  instrument_arithop( pce, dst, dstv, e->Iex.Qop.op,
                                      e->Iex.Qop.arg1, e->Iex.Qop.arg2,
                                      e->Iex.Qop.arg3, e->Iex.Qop.arg4 );
               break;
            }

            default:
               goto unhandled;
         } /* switch (e->tag) */

         break;

      } /* case Ist_WrTmp */

      default:
      unhandled:
         ppIRStmt(st);
         tl_assert(0);
   }
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

   /* Stay sane.  These two should agree! */
   tl_assert(layout->total_sizeB == PC_SIZEOF_GUEST_STATE);

   /* Copy verbatim any IR preamble preceding the first IMark */

   i = 0;
   while (i < sbIn->stmts_used && sbIn->stmts[i]->tag != Ist_IMark) {
      IRStmt* st = sbIn->stmts[i];
      tl_assert(st);
      tl_assert(isFlatIRStmt(st));
      stmt( 'C', &pce, sbIn->stmts[i] );
      i++;
   }

   /* Nasty problem.  IR optimisation of the pre-instrumented IR may
      cause the IR following the preamble to contain references to IR
      temporaries defined in the preamble.  Because the preamble isn't
      instrumented, these temporaries don't have any shadows.
      Nevertheless uses of them following the preamble will cause
      memcheck to generate references to their shadows.  End effect is
      to cause IR sanity check failures, due to references to
      non-existent shadows.  This is only evident for the complex
      preambles used for function wrapping on TOC-afflicted platforms
      (ppc64-linux, ppc32-aix5, ppc64-aix5).

      The following loop therefore scans the preamble looking for
      assignments to temporaries.  For each one found it creates an
      assignment to the corresponding shadow temp, marking it as
      'defined'.  This is the same resulting IR as if the main
      instrumentation loop before had been applied to the statement
      'tmp = CONSTANT'.
   */
#if 0
   // FIXME: this isn't exactly right; only needs to generate shadows
   // for guest-word-typed temps
   for (j = 0; j < i; j++) {
      if (sbIn->stmts[j]->tag == Ist_WrTmp) {
         /* findShadowTmpV checks its arg is an original tmp;
            no need to assert that here. */
         IRTemp tmp_o = sbIn->stmts[j]->Ist.WrTmp.tmp;
         IRTemp tmp_s = findShadowTmp(&pce, tmp_o);
         IRType ty_s  = typeOfIRTemp(sbIn->tyenv, tmp_s);
         assign( 'V', &pce, tmp_s, definedOfType( ty_s ) );
         if (0) {
            VG_(printf)("create shadow tmp for preamble tmp [%d] ty ", j);
            ppIRType( ty_s );
            VG_(printf)("\n");
         }
      }
   }
#endif

   /* Iterate over the remaining stmts to generate instrumentation. */

   tl_assert(sbIn->stmts_used > 0);
   tl_assert(i >= 0);
   tl_assert(i < sbIn->stmts_used);
   tl_assert(sbIn->stmts[i]->tag == Ist_IMark);

   for (/*use current i*/; i < sbIn->stmts_used; i++) {
      /* generate sg_ instrumentation for this stmt */
      sg_instrument_IRStmt( sgenv, pce.sb, sbIn->stmts[i],
                            layout, gWordTy, hWordTy );
      /* generate h_ instrumentation for this stmt */
      schemeS( &pce, sbIn->stmts[i] );
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
/*--- Initialisation                                               ---*/
/*--------------------------------------------------------------------*/

void h_pre_clo_init ( void )
{
   // Other initialisation
   init_shadow_memory();
   init_lossage();
}

void h_post_clo_init ( void )
{
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

#if 0
   if (h_clo_lossage_check) {
      VG_(message)(Vg_UserMsg, "\n");
      VG_(message)(Vg_UserMsg, "%12lld total memory references\n",
                               stats__tot_mem_refs);
      VG_(message)(Vg_UserMsg, "%12lld   of which are in a known segment\n",
                               stats__refs_in_a_seg);
      VG_(message)(Vg_UserMsg, "%12lld   of which are 'lost' w.r.t the seg\n",
                               stats__refs_lost_seg);
      VG_(message)(Vg_UserMsg, "\n");
      show_lossage();
      VG_(message)(Vg_UserMsg, "\n");
   } else {
      tl_assert( 0 == VG_(OSetGen_Size)(lossage) );
   }
#endif
}


/*--------------------------------------------------------------------*/
/*--- end                                                 h_main.c ---*/
/*--------------------------------------------------------------------*/
