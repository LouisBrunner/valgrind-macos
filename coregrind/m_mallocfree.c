
/*--------------------------------------------------------------------*/
/*--- An implementation of malloc/free which doesn't use sbrk.     ---*/
/*---                                               m_mallocfree.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2017 Julian Seward 
      jseward@acm.org

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

#include "pub_core_basics.h"
#include "pub_core_vki.h"
#include "pub_core_debuglog.h"
#include "pub_core_libcbase.h"
#include "pub_core_aspacemgr.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcprint.h"
#include "pub_core_mallocfree.h"
#include "pub_core_options.h"
#include "pub_core_threadstate.h"   // For VG_INVALID_THREADID
#include "pub_core_syscall.h"       // For VG_(strerror)
#include "pub_core_gdbserver.h"
#include "pub_core_transtab.h"
#include "pub_core_tooliface.h"

#include "pub_core_inner.h"
#if defined(ENABLE_INNER_CLIENT_REQUEST)
#include "memcheck/memcheck.h"
#endif

// #define DEBUG_MALLOC      // turn on heavyweight debugging machinery
// #define VERBOSE_MALLOC    // make verbose, esp. in debugging machinery

/* Number and total size of blocks in free queue. Used by mallinfo(). */
Long VG_(free_queue_volume) = 0;
Long VG_(free_queue_length) = 0;

static void cc_analyse_alloc_arena ( ArenaId aid ); /* fwds */

/*------------------------------------------------------------*/
/*--- Main types                                           ---*/
/*------------------------------------------------------------*/

#define N_MALLOC_LISTS     112    // do not change this

// The amount you can ask for is limited only by sizeof(SizeT)...
#define MAX_PSZB              (~((SizeT)0x0))

// Each arena has a sorted array of superblocks, which expands
// dynamically.  This is its initial size.
#define SBLOCKS_SIZE_INITIAL 50

typedef UChar UByte;

/* Layout of an in-use block:

      cost center (OPTIONAL)   (VG_MIN_MALLOC_SZB bytes, only when h-p enabled)
      this block total szB     (sizeof(SizeT) bytes)
      red zone bytes           (depends on Arena.rz_szB, but >= sizeof(void*))
      (payload bytes)
      red zone bytes           (depends on Arena.rz_szB, but >= sizeof(void*))
      this block total szB     (sizeof(SizeT) bytes)

   Layout of a block on the free list:

      cost center (OPTIONAL)   (VG_MIN_MALLOC_SZB bytes, only when h-p enabled)
      this block total szB     (sizeof(SizeT) bytes)
      freelist previous ptr    (sizeof(void*) bytes)
      excess red zone bytes    (if Arena.rz_szB > sizeof(void*))
      (payload bytes)
      excess red zone bytes    (if Arena.rz_szB > sizeof(void*))
      freelist next ptr        (sizeof(void*) bytes)         
      this block total szB     (sizeof(SizeT) bytes)         

   Total size in bytes (bszB) and payload size in bytes (pszB)
   are related by:

      bszB == pszB + 2*sizeof(SizeT) + 2*a->rz_szB

   when heap profiling is not enabled, and

      bszB == pszB + 2*sizeof(SizeT) + 2*a->rz_szB + VG_MIN_MALLOC_SZB

   when it is enabled.  It follows that the minimum overhead per heap
   block for arenas used by the core is:

      32-bit platforms:  2*4 + 2*4 == 16 bytes
      64-bit platforms:  2*8 + 2*8 == 32 bytes

   when heap profiling is not enabled, and

      32-bit platforms:  2*4 + 2*4 + 8  == 24 bytes
      64-bit platforms:  2*8 + 2*8 + 16 == 48 bytes

   when it is enabled.  In all cases, extra overhead may be incurred
   when rounding the payload size up to VG_MIN_MALLOC_SZB.

   Furthermore, both size fields in the block have their least-significant
   bit set if the block is not in use, and unset if it is in use.
   (The bottom 3 or so bits are always free for this because of alignment.)
   A block size of zero is not possible, because a block always has at
   least two SizeTs and two pointers of overhead.  

   Nb: All Block payloads must be VG_MIN_MALLOC_SZB-aligned.  This is
   achieved by ensuring that Superblocks are VG_MIN_MALLOC_SZB-aligned
   (see newSuperblock() for how), and that the lengths of the following
   things are a multiple of VG_MIN_MALLOC_SZB:
   - Superblock admin section lengths (due to elastic padding)
   - Block admin section (low and high) lengths (due to elastic redzones)
   - Block payload lengths (due to req_pszB rounding up)

   The heap-profile cost-center field is 8 bytes even on 32 bit
   platforms.  This is so as to keep the payload field 8-aligned.  On
   a 64-bit platform, this cc-field contains a pointer to a const
   HChar*, which is the cost center name.  On 32-bit platforms, the
   pointer lives in the lower-addressed half of the field, regardless
   of the endianness of the host.
*/
typedef
   struct {
      // No fields are actually used in this struct, because a Block has
      // many variable sized fields and so can't be accessed
      // meaningfully with normal fields.  So we use access functions all
      // the time.  This struct gives us a type to use, though.  Also, we
      // make sizeof(Block) 1 byte so that we can do arithmetic with the
      // Block* type in increments of 1!
      UByte dummy;
   } 
   Block;

/* Ensure that Block payloads can be safely cast to various pointers below. */
STATIC_ASSERT(VG_MIN_MALLOC_SZB % sizeof(void *) == 0);

// A superblock.  'padding' is never used, it just ensures that if the
// entire Superblock is aligned to VG_MIN_MALLOC_SZB, then payload_bytes[]
// will be too.  It can add small amounts of padding unnecessarily -- eg.
// 8-bytes on 32-bit machines with an 8-byte VG_MIN_MALLOC_SZB -- because
// it's too hard to make a constant expression that works perfectly in all
// cases.
// 'unsplittable' is set to NULL if superblock can be split, otherwise
// it is set to the address of the superblock. An unsplittable superblock
// will contain only one allocated block. An unsplittable superblock will
// be unmapped when its (only) allocated block is freed.
// The free space at the end of an unsplittable superblock is not used to
// make a free block. Note that this means that an unsplittable superblock can
// have up to slightly less than 1 page of unused bytes at the end of the
// superblock.
// 'unsplittable' is used to avoid quadratic memory usage for linear
// reallocation of big structures
// (see http://bugs.kde.org/show_bug.cgi?id=250101).
// ??? unsplittable replaces 'void *padding2'. Choosed this
// ??? to avoid changing the alignment logic. Maybe something cleaner
// ??? can be done.
// A splittable block can be reclaimed when all its blocks are freed :
// the reclaim of such a block is deferred till either another superblock
// of the same arena can be reclaimed or till a new superblock is needed
// in any arena.
// payload_bytes[] is made a single big Block when the Superblock is
// created, and then can be split and the splittings remerged, but Blocks
// always cover its entire length -- there's never any unused bytes at the
// end, for example.
typedef
   struct _Superblock {
      SizeT n_payload_bytes;
      struct _Superblock* unsplittable;
      UByte padding[ VG_MIN_MALLOC_SZB -
                        ((sizeof(struct _Superblock*) + sizeof(SizeT)) %
                         VG_MIN_MALLOC_SZB) ];
      UByte payload_bytes[0];
   }
   Superblock;

// An arena. 'freelist' is a circular, doubly-linked list.  'rz_szB' is
// elastic, in that it can be bigger than asked-for to ensure alignment.
typedef
   struct {
      const HChar* name;
      Bool         clientmem;        // Allocates in the client address space?
      SizeT        rz_szB;           // Red zone size in bytes
      SizeT        min_sblock_szB;   // Minimum superblock size in bytes
      SizeT        min_unsplittable_sblock_szB;
      // Minimum unsplittable superblock size in bytes. To be marked as
      // unsplittable, a superblock must have a 
      // size >= min_unsplittable_sblock_szB and cannot be split.
      // So, to avoid big overhead, superblocks used to provide aligned
      // blocks on big alignments are splittable.
      // Unsplittable superblocks will be reclaimed when their (only) 
      // allocated block is freed.
      // Smaller size superblocks are splittable and can be reclaimed when all
      // their blocks are freed.
      Block*       freelist[N_MALLOC_LISTS];
      // A dynamically expanding, ordered array of (pointers to)
      // superblocks in the arena.  If this array is expanded, which
      // is rare, the previous space it occupies is simply abandoned.
      // To avoid having to get yet another block from m_aspacemgr for
      // the first incarnation of this array, the first allocation of
      // it is within this struct.  If it has to be expanded then the
      // new space is acquired from m_aspacemgr as you would expect.
      Superblock** sblocks;
      SizeT        sblocks_size;
      SizeT        sblocks_used;
      Superblock*  sblocks_initial[SBLOCKS_SIZE_INITIAL];
      Superblock*  deferred_reclaimed_sb;

      // VG_(arena_perm_malloc) returns memory from superblocks
      // only used for permanent blocks. No overhead. These superblocks
      // are not stored in sblocks array above.
      Addr         perm_malloc_current; // first byte free in perm_malloc sb.
      Addr         perm_malloc_limit; // maximum usable byte in perm_malloc sb.

      // Stats only
      SizeT        stats__perm_bytes_on_loan;
      SizeT        stats__perm_blocks;

      ULong        stats__nreclaim_unsplit;
      ULong        stats__nreclaim_split;
      /* total # of reclaim executed for unsplittable/splittable superblocks */
      SizeT        stats__bytes_on_loan;
      SizeT        stats__bytes_mmaped;
      SizeT        stats__bytes_on_loan_max;
      ULong        stats__tot_blocks; /* total # blocks alloc'd */
      ULong        stats__tot_bytes; /* total # bytes alloc'd */
      ULong        stats__nsearches; /* total # freelist checks */
      // If profiling, when should the next profile happen at
      // (in terms of stats__bytes_on_loan_max) ?
      SizeT        next_profile_at;
      SizeT        stats__bytes_mmaped_max;
   }
   Arena;


/*------------------------------------------------------------*/
/*--- Low-level functions for working with Blocks.         ---*/
/*------------------------------------------------------------*/

#define SIZE_T_0x1      ((SizeT)0x1)

static const char* probably_your_fault =
   "This is probably caused by your program erroneously writing past the\n"
   "end of a heap block and corrupting heap metadata.  If you fix any\n"
   "invalid writes reported by Memcheck, this assertion failure will\n"
   "probably go away.  Please try that before reporting this as a bug.\n";

// Mark a bszB as in-use, and not in-use, and remove the in-use attribute.
static __inline__
SizeT mk_inuse_bszB ( SizeT bszB )
{
   vg_assert2(bszB != 0, probably_your_fault);
   return bszB & (~SIZE_T_0x1);
}
static __inline__
SizeT mk_free_bszB ( SizeT bszB )
{
   vg_assert2(bszB != 0, probably_your_fault);
   return bszB | SIZE_T_0x1;
}
static __inline__
SizeT mk_plain_bszB ( SizeT bszB )
{
   vg_assert2(bszB != 0, probably_your_fault);
   return bszB & (~SIZE_T_0x1);
}

// Forward definition.
static
void ensure_mm_init ( ArenaId aid );

// return either 0 or sizeof(ULong) depending on whether or not
// heap profiling is engaged
#define hp_overhead_szB() set_at_init_hp_overhead_szB
static SizeT set_at_init_hp_overhead_szB = -1000000; 
// startup value chosen to very likely cause a problem if used before
// a proper value is given by ensure_mm_init.

//---------------------------------------------------------------------------

// Get a block's size as stored, ie with the in-use/free attribute.
static __inline__
SizeT get_bszB_as_is ( Block* b )
{
   UByte* b2     = (UByte*)b;
   SizeT bszB_lo = *ASSUME_ALIGNED(SizeT*, &b2[0 + hp_overhead_szB()]);
   SizeT bszB_hi = *ASSUME_ALIGNED(SizeT*,
                                   &b2[mk_plain_bszB(bszB_lo) - sizeof(SizeT)]);
   vg_assert2(bszB_lo == bszB_hi, 
      "Heap block lo/hi size mismatch: lo = %llu, hi = %llu.\n%s",
      (ULong)bszB_lo, (ULong)bszB_hi, probably_your_fault);
   return bszB_lo;
}

// Get a block's plain size, ie. remove the in-use/free attribute.
static __inline__
SizeT get_bszB ( Block* b )
{
   return mk_plain_bszB(get_bszB_as_is(b));
}

// Set the size fields of a block.  bszB may have the in-use/free attribute.
static __inline__
void set_bszB ( Block* b, SizeT bszB )
{
   UByte* b2 = (UByte*)b;
   *ASSUME_ALIGNED(SizeT*, &b2[0 + hp_overhead_szB()])               = bszB;
   *ASSUME_ALIGNED(SizeT*, &b2[mk_plain_bszB(bszB) - sizeof(SizeT)]) = bszB;
}

//---------------------------------------------------------------------------

// Does this block have the in-use attribute?
static __inline__
Bool is_inuse_block ( Block* b )
{
   SizeT bszB = get_bszB_as_is(b);
   vg_assert2(bszB != 0, probably_your_fault);
   return (0 != (bszB & SIZE_T_0x1)) ? False : True;
}

//---------------------------------------------------------------------------

// Return the lower, upper and total overhead in bytes for a block.
// These are determined purely by which arena the block lives in.
static __inline__
SizeT overhead_szB_lo ( Arena* a )
{
   return hp_overhead_szB() + sizeof(SizeT) + a->rz_szB;
}
static __inline__
SizeT overhead_szB_hi ( Arena* a )
{
   return a->rz_szB + sizeof(SizeT);
}
static __inline__
SizeT overhead_szB ( Arena* a )
{
   return overhead_szB_lo(a) + overhead_szB_hi(a);
}

//---------------------------------------------------------------------------

// Return the minimum bszB for a block in this arena.  Can have zero-length
// payloads, so it's the size of the admin bytes.
static __inline__
SizeT min_useful_bszB ( Arena* a )
{
   return overhead_szB(a);
}

//---------------------------------------------------------------------------

// Convert payload size <--> block size (both in bytes).
static __inline__
SizeT pszB_to_bszB ( Arena* a, SizeT pszB )
{
   return pszB + overhead_szB(a);
}
static __inline__
SizeT bszB_to_pszB ( Arena* a, SizeT bszB )
{
   vg_assert2(bszB >= overhead_szB(a), probably_your_fault);
   return bszB - overhead_szB(a);
}

//---------------------------------------------------------------------------

// Get a block's payload size.
static __inline__
SizeT get_pszB ( Arena* a, Block* b )
{
   return bszB_to_pszB(a, get_bszB(b));
}

//---------------------------------------------------------------------------

// Given the addr of a block, return the addr of its payload, and vice versa.
static __inline__
UByte* get_block_payload ( Arena* a, Block* b )
{
   UByte* b2 = (UByte*)b;
   return & b2[ overhead_szB_lo(a) ];
}
// Given the addr of a block's payload, return the addr of the block itself.
static __inline__
Block* get_payload_block ( Arena* a, UByte* payload )
{
   return (Block*)&payload[ -overhead_szB_lo(a) ];
}

//---------------------------------------------------------------------------

// Set and get the next and previous link fields of a block.
static __inline__
void set_prev_b ( Block* b, Block* prev_p )
{ 
   UByte* b2 = (UByte*)b;
   *ASSUME_ALIGNED(Block**, &b2[hp_overhead_szB() + sizeof(SizeT)]) = prev_p;
}
static __inline__
void set_next_b ( Block* b, Block* next_p )
{
   UByte* b2 = (UByte*)b;
   *ASSUME_ALIGNED(Block**,
                   &b2[get_bszB(b) - sizeof(SizeT) - sizeof(void*)]) = next_p;
}
static __inline__
Block* get_prev_b ( Block* b )
{ 
   UByte* b2 = (UByte*)b;
   return *ASSUME_ALIGNED(Block**, &b2[hp_overhead_szB() + sizeof(SizeT)]);
}
static __inline__
Block* get_next_b ( Block* b )
{ 
   UByte* b2 = (UByte*)b;
   return *ASSUME_ALIGNED(Block**,
                          &b2[get_bszB(b) - sizeof(SizeT) - sizeof(void*)]);
}

//---------------------------------------------------------------------------

// Set and get the cost-center field of a block.
static __inline__
void set_cc ( Block* b, const HChar* cc )
{ 
   UByte* b2 = (UByte*)b;
   vg_assert( VG_(clo_profile_heap) );
   *ASSUME_ALIGNED(const HChar**, &b2[0]) = cc;
}
static __inline__
const HChar* get_cc ( Block* b )
{
   UByte* b2 = (UByte*)b;
   vg_assert( VG_(clo_profile_heap) );
   return *ASSUME_ALIGNED(const HChar**, &b2[0]);
}

//---------------------------------------------------------------------------

// Get the block immediately preceding this one in the Superblock.
static __inline__
Block* get_predecessor_block ( Block* b )
{
   UByte* b2 = (UByte*)b;
   SizeT  bszB = mk_plain_bszB(*ASSUME_ALIGNED(SizeT*, &b2[-sizeof(SizeT)]));
   return (Block*)&b2[-bszB];
}

//---------------------------------------------------------------------------

// Read and write the lower and upper red-zone bytes of a block.
static __inline__
void set_rz_lo_byte ( Block* b, UInt rz_byteno, UByte v )
{
   UByte* b2 = (UByte*)b;
   b2[hp_overhead_szB() + sizeof(SizeT) + rz_byteno] = v;
}
static __inline__
void set_rz_hi_byte ( Block* b, UInt rz_byteno, UByte v )
{
   UByte* b2 = (UByte*)b;
   b2[get_bszB(b) - sizeof(SizeT) - rz_byteno - 1] = v;
}
static __inline__
UByte get_rz_lo_byte ( Block* b, UInt rz_byteno )
{
   UByte* b2 = (UByte*)b;
   return b2[hp_overhead_szB() + sizeof(SizeT) + rz_byteno];
}
static __inline__
UByte get_rz_hi_byte ( Block* b, UInt rz_byteno )
{
   UByte* b2 = (UByte*)b;
   return b2[get_bszB(b) - sizeof(SizeT) - rz_byteno - 1];
}

#if defined(ENABLE_INNER_CLIENT_REQUEST)
/* When running as an inner, the block headers before and after
   (see 'Layout of an in-use block:' above) are made non accessible
   by VALGRIND_MALLOCLIKE_BLOCK/VALGRIND_FREELIKE_BLOCK
   to allow the outer to detect block overrun.
   The below two functions are used when these headers must be
   temporarily accessed. */
static void mkBhdrAccess( Arena* a, Block* b )
{
   VALGRIND_MAKE_MEM_DEFINED (b,
                              hp_overhead_szB() + sizeof(SizeT) + a->rz_szB);
   VALGRIND_MAKE_MEM_DEFINED (b + get_bszB(b) - a->rz_szB - sizeof(SizeT),
                              a->rz_szB + sizeof(SizeT));
}

/* Mark block hdr as not accessible.
   !!! Currently, we do not mark the cost center and szB fields unaccessible
   as these are accessed at too many places. */
static void mkBhdrNoAccess( Arena* a, Block* b )
{
   VALGRIND_MAKE_MEM_NOACCESS (b + hp_overhead_szB() + sizeof(SizeT),
                               a->rz_szB);
   VALGRIND_MAKE_MEM_NOACCESS (b + get_bszB(b) - sizeof(SizeT) - a->rz_szB,
                               a->rz_szB);
}

/* Make the cc+szB fields accessible. */
static void mkBhdrSzAccess( Arena* a, Block* b )
{
   VALGRIND_MAKE_MEM_DEFINED (b,
                              hp_overhead_szB() + sizeof(SizeT));
   /* We cannot use  get_bszB(b), as this reads the 'hi' szB we want
      to mark accessible. So, we only access the 'lo' szB. */
   SizeT bszB_lo = mk_plain_bszB(*(SizeT*)&b[0 + hp_overhead_szB()]);
   VALGRIND_MAKE_MEM_DEFINED (b + bszB_lo - sizeof(SizeT),
                              sizeof(SizeT));
}
#endif

/*------------------------------------------------------------*/
/*--- Arena management                                     ---*/
/*------------------------------------------------------------*/

#define CORE_ARENA_MIN_SZB    1048576

// The arena structures themselves.
static Arena vg_arena[VG_N_ARENAS];

// Functions external to this module identify arenas using ArenaIds,
// not Arena*s.  This fn converts the former to the latter.
static Arena* arenaId_to_ArenaP ( ArenaId arena )
{
   vg_assert(arena >= 0 && arena < VG_N_ARENAS);
   return & vg_arena[arena];
}

static ArenaId arenaP_to_ArenaId ( Arena *a )
{
   ArenaId arena = a -vg_arena; 
   vg_assert(arena >= 0 && arena < VG_N_ARENAS);
   return arena;
}

// Initialise an arena.  rz_szB is the (default) minimum redzone size;
// It might be overridden by VG_(clo_redzone_size) or VG_(clo_core_redzone_size).
// it might be made bigger to ensure that VG_MIN_MALLOC_SZB is observed.
static
void arena_init ( ArenaId aid, const HChar* name, SizeT rz_szB,
                  SizeT min_sblock_szB, SizeT min_unsplittable_sblock_szB )
{
   SizeT  i;
   Arena* a = arenaId_to_ArenaP(aid);

   // Ensure default redzones are a reasonable size.  
   vg_assert(rz_szB <= MAX_REDZONE_SZB);
   
   /* Override the default redzone size if a clo value was given.
      Note that the clo value can be significantly bigger than MAX_REDZONE_SZB
      to allow the user to chase horrible bugs using up to 1 page
      of protection. */
   if (VG_AR_CLIENT == aid) {
      if (VG_(clo_redzone_size) != -1)
         rz_szB = VG_(clo_redzone_size);
   } else {
      if (VG_(clo_core_redzone_size) != rz_szB)
         rz_szB = VG_(clo_core_redzone_size);
   }

   // Redzones must always be at least the size of a pointer, for holding the
   // prev/next pointer (see the layout details at the top of this file).
   if (rz_szB < sizeof(void*)) rz_szB = sizeof(void*);

   // The size of the low and high admin sections in a block must be a
   // multiple of VG_MIN_MALLOC_SZB.  So we round up the asked-for
   // redzone size if necessary to achieve this.
   a->rz_szB = rz_szB;
   while (0 != overhead_szB_lo(a) % VG_MIN_MALLOC_SZB) a->rz_szB++;
   vg_assert(overhead_szB_lo(a) - hp_overhead_szB() == overhead_szB_hi(a));

   // Here we have established the effective redzone size.


   vg_assert((min_sblock_szB % VKI_PAGE_SIZE) == 0);
   a->name      = name;
   a->clientmem = ( VG_AR_CLIENT == aid ? True : False );

   a->min_sblock_szB = min_sblock_szB;
   a->min_unsplittable_sblock_szB = min_unsplittable_sblock_szB;
   for (i = 0; i < N_MALLOC_LISTS; i++) a->freelist[i] = NULL;

   a->sblocks                  = & a->sblocks_initial[0];
   a->sblocks_size             = SBLOCKS_SIZE_INITIAL;
   a->sblocks_used             = 0;
   a->deferred_reclaimed_sb    = 0;
   a->perm_malloc_current      = 0;
   a->perm_malloc_limit        = 0;
   a->stats__perm_bytes_on_loan= 0;
   a->stats__perm_blocks       = 0;
   a->stats__nreclaim_unsplit  = 0;
   a->stats__nreclaim_split    = 0;
   a->stats__bytes_on_loan     = 0;
   a->stats__bytes_mmaped      = 0;
   a->stats__bytes_on_loan_max = 0;
   a->stats__bytes_mmaped_max  = 0;
   a->stats__tot_blocks        = 0;
   a->stats__tot_bytes         = 0;
   a->stats__nsearches         = 0;
   a->next_profile_at          = 25 * 1000 * 1000;
   vg_assert(sizeof(a->sblocks_initial) 
             == SBLOCKS_SIZE_INITIAL * sizeof(Superblock*));
}

/* Print vital stats for an arena. */
void VG_(print_all_arena_stats) ( void )
{
   UInt i;
   for (i = 0; i < VG_N_ARENAS; i++) {
      Arena* a = arenaId_to_ArenaP(i);
      VG_(message)(Vg_DebugMsg,
                   "%-8s: %'13lu/%'13lu max/curr mmap'd, "
                   "%llu/%llu unsplit/split sb unmmap'd,  "
                   "%'13lu/%'13lu max/curr,  "
                   "%10llu/%10llu totalloc-blocks/bytes,"
                   "  %10llu searches %lu rzB\n",
                   a->name,
                   a->stats__bytes_mmaped_max, a->stats__bytes_mmaped,
                   a->stats__nreclaim_unsplit, a->stats__nreclaim_split,
                   a->stats__bytes_on_loan_max,
                   a->stats__bytes_on_loan,
                   a->stats__tot_blocks, a->stats__tot_bytes,
                   a->stats__nsearches,
                   a->rz_szB
      );
   }
}

void VG_(print_arena_cc_analysis) ( void )
{
   UInt i;
   vg_assert( VG_(clo_profile_heap) );
   for (i = 0; i < VG_N_ARENAS; i++) {
      cc_analyse_alloc_arena(i);
   }
}


/* This library is self-initialising, as it makes this more self-contained,
   less coupled with the outside world.  Hence VG_(arena_malloc)() and
   VG_(arena_free)() below always call ensure_mm_init() to ensure things are
   correctly initialised.  

   We initialise the client arena separately (and later) because the core
   must do non-client allocation before the tool has a chance to set the
   client arena's redzone size.
*/
static Bool     client_inited = False;
static Bool  nonclient_inited = False;

static
void ensure_mm_init ( ArenaId aid )
{
   static SizeT client_rz_szB = 8;     // default: be paranoid

   /* We use checked red zones (of various sizes) for our internal stuff,
      and an unchecked zone of arbitrary size for the client.  Of
      course the client's red zone can be checked by the tool, eg. 
      by using addressibility maps, but not by the mechanism implemented
      here, which merely checks at the time of freeing that the red 
      zone bytes are unchanged.

      Nb: redzone sizes are *minimums*;  they could be made bigger to ensure
      alignment.  Eg. with 8 byte alignment, on 32-bit machines 4 stays as
      4, but 16 becomes 20;  but on 64-bit machines 4 becomes 8, and 16
      stays as 16 --- the extra 4 bytes in both are accounted for by the
      larger prev/next ptr.
   */
   if (VG_AR_CLIENT == aid) {
      Int ar_client_sbszB;
      if (client_inited) {
         // This assertion ensures that a tool cannot try to change the client
         // redzone size with VG_(needs_malloc_replacement)() after this module
         // has done its first allocation from the client arena.
         if (VG_(needs).malloc_replacement)
            vg_assert(client_rz_szB == VG_(tdict).tool_client_redzone_szB);
         return;
      }

      // Check and set the client arena redzone size
      if (VG_(needs).malloc_replacement) {
         client_rz_szB = VG_(tdict).tool_client_redzone_szB;
         if (client_rz_szB > MAX_REDZONE_SZB) {
            VG_(printf)( "\nTool error:\n"
                         "  specified redzone size is too big (%llu)\n", 
                         (ULong)client_rz_szB);
            VG_(exit)(1);
         }
      }
      // Initialise the client arena.  On all platforms,
      // increasing the superblock size reduces the number of superblocks
      // in the client arena, which makes findSb cheaper.
      ar_client_sbszB = 4194304;
      // superblocks with a size > ar_client_sbszB will be unsplittable
      // (unless used for providing memalign-ed blocks).
      arena_init ( VG_AR_CLIENT,    "client",   client_rz_szB, 
                   ar_client_sbszB, ar_client_sbszB+1);
      client_inited = True;

   } else {
      if (nonclient_inited) {
         return;
      }
      set_at_init_hp_overhead_szB = 
         VG_(clo_profile_heap)  ? VG_MIN_MALLOC_SZB  : 0;
      // Initialise the non-client arenas
      // Similarly to client arena, big allocations will be unsplittable.
      arena_init ( VG_AR_CORE,      "core",     CORE_REDZONE_DEFAULT_SZB,
                   4194304, 4194304+1 );
      arena_init ( VG_AR_DINFO,     "dinfo",    CORE_REDZONE_DEFAULT_SZB,
                   1048576, 1048576+1 );
      arena_init ( VG_AR_DEMANGLE,  "demangle", CORE_REDZONE_DEFAULT_SZB,
                   65536,   65536+1 );
      arena_init ( VG_AR_TTAUX,     "ttaux",    CORE_REDZONE_DEFAULT_SZB,
                   65536,   65536+1 );
      nonclient_inited = True;
   }

#  ifdef DEBUG_MALLOC
   VG_(printf)("ZZZ1\n");
   VG_(sanity_check_malloc_all)();
   VG_(printf)("ZZZ2\n");
#  endif
}


/*------------------------------------------------------------*/
/*--- Superblock management                                ---*/
/*------------------------------------------------------------*/

__attribute__((noreturn))
void VG_(out_of_memory_NORETURN) ( const HChar* who, SizeT szB, UWord err )
{
   static Int outputTrial = 0;
   // We try once to output the full memory state followed by the below message.
   // If that fails (due to out of memory during first trial), we try to just
   // output the below message.
   // And then we abandon.
   
   ULong tot_alloc = VG_(am_get_anonsize_total)();
   const HChar* s1 = 
      "\n"
      "    Valgrind's memory management: out of memory: %s\n"
      "       %s's request for %llu bytes failed.\n"
      "       %'13llu bytes have already been mmap-ed ANONYMOUS.\n"
      "    Valgrind cannot continue.  Sorry.\n\n"
      "    There are several possible reasons for this.\n"
      "    - You have some kind of memory limit in place.  Look at the\n"
      "      output of 'ulimit -a'.  Is there a limit on the size of\n"
      "      virtual memory or address space?\n"
      "    - You have run out of swap space.\n"
      "    - You have some policy enabled that denies memory to be\n"
      "      executable (for example selinux deny_execmem) that causes\n"
      "      mmap to fail with Permission denied.\n"
      "    - Valgrind has a bug.  If you think this is the case or you are\n"
      "    not sure, please let us know and we'll try to fix it.\n"
      "    Please note that programs can take substantially more memory than\n"
      "    normal when running under Valgrind tools, eg. up to twice or\n"
      "    more, depending on the tool.  On a 64-bit machine, Valgrind\n"
      "    should be able to make use of up 32GB memory.  On a 32-bit\n"
      "    machine, Valgrind should be able to use all the memory available\n"
      "    to a single process, up to 4GB if that's how you have your\n"
      "    kernel configured.  Most 32-bit Linux setups allow a maximum of\n"
      "    3GB per process.\n\n"
      "    Whatever the reason, Valgrind cannot continue.  Sorry.\n";

   if (outputTrial <= 1) {
      if (outputTrial == 0) {
         outputTrial++;
         // First print the memory stats with the aspacemgr data.
         VG_(am_show_nsegments) (0, "out_of_memory");
         VG_(print_all_arena_stats) ();
         if (VG_(clo_profile_heap))
            VG_(print_arena_cc_analysis) ();
         // And then print some other information that might help.
         VG_(print_all_stats) (False, /* Memory stats */
                               True /* Tool stats */);
         VG_(show_sched_status) (True,  // host_stacktrace
                                 True,  // valgrind_stack_usage
                                 True); // exited_threads
        /* In case we are an inner valgrind, asks the outer to report
            its memory state in its log output. */
         INNER_REQUEST(VALGRIND_MONITOR_COMMAND("v.set log_output"));
         INNER_REQUEST(VALGRIND_MONITOR_COMMAND("v.info memory aspacemgr"));
      }
      outputTrial++;
      VG_(message)(Vg_UserMsg, s1,
		   ((err == 0 || err == VKI_ENOMEM)
                    ? "" : VG_(strerror) (err)),
                   who, (ULong)szB, tot_alloc);
   } else {
      VG_(debugLog)(0,"mallocfree", s1,
		    ((err == 0 || err == VKI_ENOMEM)
                     ? "" : VG_(strerror) (err)),
                    who, (ULong)szB, tot_alloc);
   }

   VG_(exit)(1);
}


// Align ptr p upwards to an align-sized boundary.
static
void* align_upwards ( void* p, SizeT align )
{
   Addr a = (Addr)p;
   if ((a % align) == 0) return (void*)a;
   return (void*)(a - (a % align) + align);
}

// Forward definition.
static
void deferred_reclaimSuperblock ( Arena* a, Superblock* sb);

// If not enough memory available, either aborts (for non-client memory)
// or returns 0 (for client memory).
static
Superblock* newSuperblock ( Arena* a, SizeT cszB )
{
   Superblock* sb;
   SysRes      sres;
   Bool        unsplittable;
   ArenaId     aid;

   // A new superblock is needed for arena a. We will execute the deferred
   // reclaim in all arenas in order to minimise fragmentation and
   // peak memory usage.
   for (aid = 0; aid < VG_N_ARENAS; aid++) {
      Arena* arena = arenaId_to_ArenaP(aid);
      if (arena->deferred_reclaimed_sb != NULL)
         deferred_reclaimSuperblock (arena, NULL);
   }

   // Take into account admin bytes in the Superblock.
   cszB += sizeof(Superblock);

   if (cszB < a->min_sblock_szB) cszB = a->min_sblock_szB;
   cszB = VG_PGROUNDUP(cszB);

   if (cszB >= a->min_unsplittable_sblock_szB)
      unsplittable = True;
   else
      unsplittable = False;   


   if (a->clientmem) {
      // client allocation -- return 0 to client if it fails
      sres = VG_(am_mmap_client_heap)
         ( cszB, VKI_PROT_READ|VKI_PROT_WRITE|VKI_PROT_EXEC );
      if (sr_isError(sres))
         return 0;
      sb = (Superblock*)(Addr)sr_Res(sres);
   } else {
      // non-client allocation -- abort if it fails
      sres = VG_(am_mmap_anon_float_valgrind)( cszB );
      if (sr_isError(sres)) {
         VG_(out_of_memory_NORETURN)("newSuperblock", cszB, sr_Err(sres));
         /* NOTREACHED */
         sb = NULL; /* keep gcc happy */
      } else {
         sb = (Superblock*)(Addr)sr_Res(sres);
      }
   }
   vg_assert(NULL != sb);
   INNER_REQUEST(VALGRIND_MAKE_MEM_UNDEFINED(sb, cszB));
   vg_assert(0 == (Addr)sb % VG_MIN_MALLOC_SZB);
   sb->n_payload_bytes = cszB - sizeof(Superblock);
   sb->unsplittable = (unsplittable ? sb : NULL);
   a->stats__bytes_mmaped += cszB;
   if (a->stats__bytes_mmaped > a->stats__bytes_mmaped_max)
      a->stats__bytes_mmaped_max = a->stats__bytes_mmaped;
   VG_(debugLog)(1, "mallocfree",
                    "newSuperblock at %p (pszB %7lu) %s owner %s/%s\n", 
                    sb, sb->n_payload_bytes,
                    (unsplittable ? "unsplittable" : ""),
                    a->clientmem ? "CLIENT" : "VALGRIND", a->name );
   return sb;
}

// Reclaims the given superblock:
//  * removes sb from arena sblocks list.
//  * munmap the superblock segment.
static
void reclaimSuperblock ( Arena* a, Superblock* sb)
{
   SysRes sres;
   SizeT  cszB;
   UInt   i, j;

   VG_(debugLog)(1, "mallocfree",
                    "reclaimSuperblock at %p (pszB %7lu) %s owner %s/%s\n", 
                    sb, sb->n_payload_bytes,
                    (sb->unsplittable ? "unsplittable" : ""),
                    a->clientmem ? "CLIENT" : "VALGRIND", a->name );

   // Take into account admin bytes in the Superblock.
   cszB = sizeof(Superblock) + sb->n_payload_bytes;

   // removes sb from superblock list.
   for (i = 0U; i < a->sblocks_used; i++) {
      if (a->sblocks[i] == sb)
         break;
   }
   vg_assert(i < a->sblocks_used);
   for (j = i; j < a->sblocks_used; j++)
      a->sblocks[j] = a->sblocks[j+1];
   a->sblocks_used--;
   a->sblocks[a->sblocks_used] = NULL;
   // paranoia: NULLify ptr to reclaimed sb or NULLify copy of ptr to last sb.

   a->stats__bytes_mmaped -= cszB;
   if (sb->unsplittable)
      a->stats__nreclaim_unsplit++;
   else
      a->stats__nreclaim_split++;

   // Now that the sb is removed from the list, mnumap its space.
   if (a->clientmem) {
      // reclaimable client allocation 
      Bool need_discard = False;
      sres = VG_(am_munmap_client)(&need_discard, (Addr) sb, cszB);
      vg_assert2(! sr_isError(sres), "superblock client munmap failure\n");
      /* We somewhat help the client by discarding the range.
         Note however that if the client has JITted some code in
         a small block that was freed, we do not provide this
         'discard support' */
      /* JRS 2011-Sept-26: it would be nice to move the discard
         outwards somewhat (in terms of calls) so as to make it easier
         to verify that there will be no nonterminating recursive set
         of calls a result of calling VG_(discard_translations).
         Another day, perhaps. */
      if (need_discard)
         VG_(discard_translations) ((Addr) sb, cszB, "reclaimSuperblock");
   } else {
      // reclaimable non-client allocation
      sres = VG_(am_munmap_valgrind)((Addr) sb, cszB);
      vg_assert2(! sr_isError(sres), "superblock valgrind munmap failure\n");
   }

}

// Find the superblock containing the given chunk.
static
Superblock* findSb ( Arena* a, Block* b )
{
   SizeT min = 0;
   SizeT max = a->sblocks_used;

   while (min <= max) {
      Superblock * sb; 
      SizeT pos = min + (max - min)/2;

      vg_assert(pos < a->sblocks_used);
      sb = a->sblocks[pos];
      if ((Block*)&sb->payload_bytes[0] <= b
          && b < (Block*)&sb->payload_bytes[sb->n_payload_bytes])
      {
         return sb;
      } else if ((Block*)&sb->payload_bytes[0] <= b) {
         min = pos + 1;
      } else {
         max = pos - 1;
      }
   }
   VG_(printf)("findSb: can't find pointer %p in arena '%s'\n",
                b, a->name );
   VG_(core_panic)("findSb: VG_(arena_free)() in wrong arena?");
   return NULL; /*NOTREACHED*/
}


// Find the superblock containing the given address.
// If superblock not found, return NULL.
static
Superblock* maybe_findSb ( Arena* a, Addr ad )
{
   SizeT min = 0;
   SizeT max = a->sblocks_used;

   while (min <= max) {
      Superblock * sb; 
      SizeT pos = min + (max - min)/2;
      if (pos >= a->sblocks_used)
         return NULL;
      sb = a->sblocks[pos];
      if ((Addr)&sb->payload_bytes[0] <= ad
          && ad < (Addr)&sb->payload_bytes[sb->n_payload_bytes]) {
         return sb;
      } else if ((Addr)&sb->payload_bytes[0] <= ad) {
         min = pos + 1;
      } else {
         max = pos - 1;
      }
   }
   return NULL;
}


/*------------------------------------------------------------*/
/*--- Functions for working with freelists.                ---*/
/*------------------------------------------------------------*/

#if defined(__clang__)
/* The nicely aligned 'returns' in the function below produce
 * misleading indentation warnings. Rather than turn the
 * warning off globally, just turn it off for the block of code. */
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wmisleading-indentation"
#endif

// Nb: Determination of which freelist a block lives on is based on the
// payload size, not block size.

// Convert a payload size in bytes to a freelist number.
static __attribute__((noinline))
UInt pszB_to_listNo_SLOW ( SizeT pszB__divided_by__VG_MIN_MALLOC_SZB )
{
   SizeT n = pszB__divided_by__VG_MIN_MALLOC_SZB;

   if (n < 299) {
      if (n < 114) {
         if (n < 85) {
            if (n < 74) {
               /* -- Exponential slope up, factor 1.05 -- */
               if (n < 67) return 64;
               if (n < 70) return 65;
               /* else */  return 66;
            } else {
               if (n < 77) return 67;
               if (n < 81) return 68;
               /* else */  return 69;
            }
         } else {
            if (n < 99) {
               if (n < 90) return 70;
               if (n < 94) return 71;
               /* else */  return 72;
            } else {
               if (n < 104) return 73;
               if (n < 109) return 74;
               /* else */   return 75;
            }
         }
      } else {
         if (n < 169) {
            if (n < 133) {
               if (n < 120) return 76;
               if (n < 126) return 77;
               /* else */   return 78;
            } else {
               if (n < 139) return 79;
               /* -- Exponential slope up, factor 1.10 -- */
               if (n < 153) return 80;
               /* else */   return 81;
            }
         } else {
            if (n < 224) {
               if (n < 185) return 82;
               if (n < 204) return 83;
               /* else */   return 84;
            } else {
               if (n < 247) return 85;
               if (n < 272) return 86;
               /* else */   return 87;
            }
         }
      }
   } else {
      if (n < 1331) {
         if (n < 530) {
            if (n < 398) {
               if (n < 329) return 88;
               if (n < 362) return 89;
               /* else */   return 90;
            } else {
               if (n < 438) return 91;
               if (n < 482) return 92;
               /* else */   return 93;
            }
         } else {
            if (n < 770) {
               if (n < 583) return 94;
               if (n < 641) return 95;
               /* -- Exponential slope up, factor 1.20 -- */
               /* else */   return 96;
            } else {
               if (n < 924) return 97;
               if (n < 1109) return 98;
               /* else */    return 99;
            }
         }
      } else {
         if (n < 3974) {
            if (n < 2300) {
               if (n < 1597) return 100;
               if (n < 1916) return 101;
               return 102;
            } else {
               if (n < 2760) return 103;
               if (n < 3312) return 104;
               /* else */    return 105;
            }
         } else {
            if (n < 6868) {
               if (n < 4769) return 106;
               if (n < 5723) return 107;
               /* else */    return 108;
            } else {
               if (n < 8241) return 109;
               if (n < 9890) return 110;
               /* else */    return 111;
            }
         }
      }
   }
   /*NOTREACHED*/
   vg_assert(0);
}

#if defined(__clang__)
#pragma clang diagnostic pop
#endif

static inline
UInt pszB_to_listNo ( SizeT pszB )
{
   SizeT n = pszB / VG_MIN_MALLOC_SZB;
   vg_assert(0 == (pszB % VG_MIN_MALLOC_SZB));

   // The first 64 lists hold blocks of size VG_MIN_MALLOC_SZB * list_num.
   // The final 48 hold bigger blocks and are dealt with by the _SLOW
   // case.
   if (LIKELY(n < 64)) {
      return (UInt)n;
   } else {
      return pszB_to_listNo_SLOW(n);
   }
}

// What is the minimum payload size for a given list?
static
SizeT listNo_to_pszB_min ( UInt listNo )
{
   /* Repeatedly computing this function at every request is
      expensive.  Hence at the first call just cache the result for
      every possible argument. */
   static SizeT cache[N_MALLOC_LISTS];
   static Bool  cache_valid = False;
   if (!cache_valid) {
      UInt i;
      for (i = 0; i < N_MALLOC_LISTS; i++) {
         SizeT pszB = 0;
         while (pszB_to_listNo(pszB) < i)
            pszB += VG_MIN_MALLOC_SZB;
         cache[i] = pszB;
      }
      cache_valid = True;
   }
   /* Returned cached answer. */
   vg_assert(listNo <= N_MALLOC_LISTS);
   return cache[listNo];
}

// What is the maximum payload size for a given list?
static
SizeT listNo_to_pszB_max ( UInt listNo )
{
   vg_assert(listNo <= N_MALLOC_LISTS);
   if (listNo == N_MALLOC_LISTS-1) {
      return MAX_PSZB;
   } else {
      return listNo_to_pszB_min(listNo+1) - 1;
   }
}


/* A nasty hack to try and reduce fragmentation.  Try and replace
   a->freelist[lno] with another block on the same list but with a
   lower address, with the idea of attempting to recycle the same
   blocks rather than cruise through the address space. */
static 
void swizzle ( Arena* a, UInt lno )
{
   Block* p_best;
   Block* pp;
   Block* pn;
   UInt   i;

   p_best = a->freelist[lno];
   if (p_best == NULL) return;

   pn = pp = p_best;

   // This loop bound was 20 for a long time, but experiments showed that
   // reducing it to 10 gave the same result in all the tests, and 5 got the
   // same result in 85--100% of cases.  And it's called often enough to be
   // noticeable in programs that allocated a lot.
   for (i = 0; i < 5; i++) {
      pn = get_next_b(pn);
      pp = get_prev_b(pp);
      if (pn < p_best) p_best = pn;
      if (pp < p_best) p_best = pp;
   }
   if (p_best < a->freelist[lno]) {
#     ifdef VERBOSE_MALLOC
      VG_(printf)("retreat by %ld\n", (Word)(a->freelist[lno] - p_best));
#     endif
      a->freelist[lno] = p_best;
   }
}


/*------------------------------------------------------------*/
/*--- Sanity-check/debugging machinery.                    ---*/
/*------------------------------------------------------------*/

#define REDZONE_LO_MASK    0x31
#define REDZONE_HI_MASK    0x7c

// Do some crude sanity checks on a Block.
static 
Bool blockSane ( Arena* a, Block* b )
{
#  define BLEAT(str) VG_(printf)("blockSane: fail -- %s\n",str)
   UInt i;
   // The lo and hi size fields will be checked (indirectly) by the call
   // to get_rz_hi_byte().
   if (!a->clientmem && is_inuse_block(b)) {
      // In the inner, for memcheck sake, temporarily mark redzone accessible.
      INNER_REQUEST(mkBhdrAccess(a,b));
      for (i = 0; i < a->rz_szB; i++) {
         if (get_rz_lo_byte(b, i) != 
            (UByte)(((Addr)b&0xff) ^ REDZONE_LO_MASK))
               {BLEAT("redzone-lo");return False;}
         if (get_rz_hi_byte(b, i) != 
            (UByte)(((Addr)b&0xff) ^ REDZONE_HI_MASK))
               {BLEAT("redzone-hi");return False;}
      }
      INNER_REQUEST(mkBhdrNoAccess(a,b));
   }
   return True;
#  undef BLEAT
}

// Sanity checks on a Block inside an unsplittable superblock
static 
Bool unsplittableBlockSane ( Arena* a, Superblock *sb, Block* b )
{
#  define BLEAT(str) VG_(printf)("unsplittableBlockSane: fail -- %s\n",str)
   Block*      other_b;
   UByte* sb_start;
   UByte* sb_end;

   if (!blockSane (a, b))
      {BLEAT("blockSane");return False;}
   
   if (sb->unsplittable != sb)
      {BLEAT("unsplittable");return False;}

   sb_start = &sb->payload_bytes[0];
   sb_end   = &sb->payload_bytes[sb->n_payload_bytes - 1];

   // b must be first block (i.e. no unused bytes at the beginning)
   if ((Block*)sb_start != b)
      {BLEAT("sb_start");return False;}

   // b must be last block (i.e. no unused bytes at the end)
   other_b = b + get_bszB(b);
   if (other_b-1 != (Block*)sb_end)
      {BLEAT("sb_end");return False;}
   
   return True;
#  undef BLEAT
}

// Print superblocks (only for debugging).
static 
void ppSuperblocks ( Arena* a )
{
   UInt i, j, blockno = 1;
   SizeT b_bszB;

   for (j = 0; j < a->sblocks_used; ++j) {
      Superblock * sb = a->sblocks[j];

      VG_(printf)( "\n" );
      VG_(printf)( "superblock %u at %p %s, sb->n_pl_bs = %lu\n",
                   blockno++, sb, (sb->unsplittable ? "unsplittable" : ""),
                   sb->n_payload_bytes);
      for (i = 0; i < sb->n_payload_bytes; i += b_bszB) {
         Block* b = (Block*)&sb->payload_bytes[i];
         b_bszB   = get_bszB(b);
         VG_(printf)( "   block at %u, bszB %lu: ", i, b_bszB );
         VG_(printf)( "%s, ", is_inuse_block(b) ? "inuse" : "free");
         VG_(printf)( "%s\n", blockSane(a, b) ? "ok" : "BAD" );
      }
      vg_assert(i == sb->n_payload_bytes);   // no overshoot at end of Sb
   }
   VG_(printf)( "end of superblocks\n\n" );
}

// Sanity check both the superblocks and the chains.
static void sanity_check_malloc_arena ( ArenaId aid )
{
   UInt        i, j, superblockctr, blockctr_sb, blockctr_li;
   UInt        blockctr_sb_free, listno;
   SizeT       b_bszB, b_pszB, list_min_pszB, list_max_pszB;
   Bool        thisFree, lastWasFree, sblockarrOK;
   Block*      b;
   Block*      b_prev;
   SizeT       arena_bytes_on_loan;
   Arena*      a;

#  define BOMB VG_(core_panic)("sanity_check_malloc_arena")

   a = arenaId_to_ArenaP(aid);

   // Check the superblock array.
   sblockarrOK
      = a->sblocks != NULL
        && a->sblocks_size >= SBLOCKS_SIZE_INITIAL
        && a->sblocks_used <= a->sblocks_size
        && (a->sblocks_size == SBLOCKS_SIZE_INITIAL 
            ? (a->sblocks == &a->sblocks_initial[0])
            : (a->sblocks != &a->sblocks_initial[0]));
   if (!sblockarrOK) {
      VG_(printf)("sanity_check_malloc_arena: sblock array BAD\n");
      BOMB;
   }

   // First, traverse all the superblocks, inspecting the Blocks in each.
   superblockctr = blockctr_sb = blockctr_sb_free = 0;
   arena_bytes_on_loan = 0;
   for (j = 0; j < a->sblocks_used; ++j) {
      Superblock * sb = a->sblocks[j];
      lastWasFree = False;
      superblockctr++;
      for (i = 0; i < sb->n_payload_bytes; i += mk_plain_bszB(b_bszB)) {
         blockctr_sb++;
         b     = (Block*)&sb->payload_bytes[i];
         b_bszB = get_bszB_as_is(b);
         if (!blockSane(a, b)) {
            VG_(printf)("sanity_check_malloc_arena: sb %p, block %u "
                        "(bszB %lu):  BAD\n", sb, i, b_bszB );
            BOMB;
         }
         thisFree = !is_inuse_block(b);
         if (thisFree && lastWasFree) {
            VG_(printf)("sanity_check_malloc_arena: sb %p, block %u "
                        "(bszB %lu): UNMERGED FREES\n", sb, i, b_bszB );
            BOMB;
         }
         if (thisFree) blockctr_sb_free++;
         if (!thisFree)
            arena_bytes_on_loan += bszB_to_pszB(a, b_bszB);
         lastWasFree = thisFree;
      }
      if (i > sb->n_payload_bytes) {
         VG_(printf)( "sanity_check_malloc_arena: sb %p: last block "
                      "overshoots end\n", sb);
         BOMB;
      }
   }

   arena_bytes_on_loan += a->stats__perm_bytes_on_loan;

   if (arena_bytes_on_loan != a->stats__bytes_on_loan) {
#     ifdef VERBOSE_MALLOC
      VG_(printf)( "sanity_check_malloc_arena: a->bytes_on_loan %lu, "
                   "arena_bytes_on_loan %lu: "
                   "MISMATCH\n", a->stats__bytes_on_loan, arena_bytes_on_loan);
#     endif
      ppSuperblocks(a);
      BOMB;
   }

   /* Second, traverse each list, checking that the back pointers make
      sense, counting blocks encountered, and checking that each block
      is an appropriate size for this list. */
   blockctr_li = 0;
   for (listno = 0; listno < N_MALLOC_LISTS; listno++) {
      list_min_pszB = listNo_to_pszB_min(listno);
      list_max_pszB = listNo_to_pszB_max(listno);
      b = a->freelist[listno];
      if (b == NULL) continue;
      while (True) {
         b_prev = b;
         b = get_next_b(b);
         if (get_prev_b(b) != b_prev) {
            VG_(printf)( "sanity_check_malloc_arena: list %u at %p: "
                         "BAD LINKAGE\n",
                         listno, b );
            BOMB;
         }
         b_pszB = get_pszB(a, b);
         if (b_pszB < list_min_pszB || b_pszB > list_max_pszB) {
            VG_(printf)(
               "sanity_check_malloc_arena: list %u at %p: "
               "WRONG CHAIN SIZE %luB (%luB, %luB)\n",
               listno, b, b_pszB, list_min_pszB, list_max_pszB );
            BOMB;
         }
         blockctr_li++;
         if (b == a->freelist[listno]) break;
      }
   }

   if (blockctr_sb_free != blockctr_li) {
#     ifdef VERBOSE_MALLOC
      VG_(printf)( "sanity_check_malloc_arena: BLOCK COUNT MISMATCH "
                   "(via sbs %d, via lists %d)\n",
                   blockctr_sb_free, blockctr_li );
#     endif
      ppSuperblocks(a);
      BOMB;
   }

   if (VG_(clo_verbosity) > 2) 
      VG_(message)(Vg_DebugMsg,
                   "%-8s: %2u sbs, %5u bs, %2u/%-2u free bs, "
                   "%7lu mmap, %7lu loan\n",
                   a->name,
                   superblockctr,
                   blockctr_sb, blockctr_sb_free, blockctr_li, 
                   a->stats__bytes_mmaped, a->stats__bytes_on_loan);   
#  undef BOMB
}


#define N_AN_CCS 1000

typedef struct {
   ULong nBytes;
   ULong nBlocks;
   const HChar* cc;
} AnCC;

static AnCC anCCs[N_AN_CCS];

/* Sorting by decreasing cost center nBytes, to have the biggest
   cost centres at the top. */
static Int cmp_AnCC_by_vol ( const void* v1, const void* v2 ) {
   const AnCC* ancc1 = v1;
   const AnCC* ancc2 = v2;
   if (ancc1->nBytes < ancc2->nBytes) return 1;
   if (ancc1->nBytes > ancc2->nBytes) return -1;
   return 0;
}

static void cc_analyse_alloc_arena ( ArenaId aid )
{
   Word i, j, k;
   Arena*      a;
   Block*      b;
   Bool        thisFree, lastWasFree;
   SizeT       b_bszB;

   const HChar* cc;
   UInt n_ccs = 0;
   //return;
   a = arenaId_to_ArenaP(aid);
   if (a->name == NULL) {
      /* arena is not in use, is not initialised and will fail the
         sanity check that follows. */
      return;
   }

   sanity_check_malloc_arena(aid);

   VG_(printf)(
      "-------- Arena \"%s\": %'lu/%'lu max/curr mmap'd, "
      "%llu/%llu unsplit/split sb unmmap'd, "
      "%'lu/%'lu max/curr on_loan %lu rzB --------\n",
      a->name, a->stats__bytes_mmaped_max, a->stats__bytes_mmaped,
      a->stats__nreclaim_unsplit, a->stats__nreclaim_split,
      a->stats__bytes_on_loan_max, a->stats__bytes_on_loan,
      a->rz_szB
   );

   for (j = 0; j < a->sblocks_used; ++j) {
      Superblock * sb = a->sblocks[j];
      lastWasFree = False;
      for (i = 0; i < sb->n_payload_bytes; i += mk_plain_bszB(b_bszB)) {
         b     = (Block*)&sb->payload_bytes[i];
         b_bszB = get_bszB_as_is(b);
         if (!blockSane(a, b)) {
            VG_(printf)("sanity_check_malloc_arena: sb %p, block %ld "
                        "(bszB %lu):  BAD\n", sb, i, b_bszB );
            vg_assert(0);
         }
         thisFree = !is_inuse_block(b);
         if (thisFree && lastWasFree) {
            VG_(printf)("sanity_check_malloc_arena: sb %p, block %ld "
                        "(bszB %lu): UNMERGED FREES\n", sb, i, b_bszB );
            vg_assert(0);
         }
         lastWasFree = thisFree;

         if (thisFree) continue;

         if (VG_(clo_profile_heap))
            cc = get_cc(b);
         else
            cc = "(--profile-heap=yes for details)";
         if (0)
         VG_(printf)("block: inUse=%d pszB=%d cc=%s\n", 
                     (Int)(!thisFree), 
                     (Int)bszB_to_pszB(a, b_bszB),
                     get_cc(b));
         vg_assert(cc);
         for (k = 0; k < n_ccs; k++) {
           vg_assert(anCCs[k].cc);
            if (0 == VG_(strcmp)(cc, anCCs[k].cc))
               break;
         }
         vg_assert(k >= 0 && k <= n_ccs);

         if (k == n_ccs) {
            vg_assert(n_ccs < N_AN_CCS-1);
            n_ccs++;
            anCCs[k].nBytes  = 0;
            anCCs[k].nBlocks = 0;
            anCCs[k].cc      = cc;
         }

         vg_assert(k >= 0 && k < n_ccs && k < N_AN_CCS);
         anCCs[k].nBytes += (ULong)bszB_to_pszB(a, b_bszB);
         anCCs[k].nBlocks++;
      }
      if (i > sb->n_payload_bytes) {
         VG_(printf)( "sanity_check_malloc_arena: sb %p: last block "
                      "overshoots end\n", sb);
         vg_assert(0);
      }
   }

   if (a->stats__perm_bytes_on_loan > 0) {
      vg_assert(n_ccs < N_AN_CCS-1);
      anCCs[n_ccs].nBytes  = a->stats__perm_bytes_on_loan;
      anCCs[n_ccs].nBlocks = a->stats__perm_blocks;
      anCCs[n_ccs].cc      = "perm_malloc";
      n_ccs++;
   }

   VG_(ssort)( &anCCs[0], n_ccs, sizeof(anCCs[0]), cmp_AnCC_by_vol );

   for (k = 0; k < n_ccs; k++) {
      VG_(printf)("%'13llu in %'9llu: %s\n",
                  anCCs[k].nBytes, anCCs[k].nBlocks, anCCs[k].cc );
   }

   VG_(printf)("\n");
}


void VG_(sanity_check_malloc_all) ( void )
{
   UInt i;
   for (i = 0; i < VG_N_ARENAS; i++) {
      if (i == VG_AR_CLIENT && !client_inited)
         continue;
      sanity_check_malloc_arena ( i );
   }
}

void VG_(describe_arena_addr) ( Addr a, AddrArenaInfo* aai )
{
   UInt i;
   Superblock *sb;
   Arena      *arena;

   for (i = 0; i < VG_N_ARENAS; i++) {
      if (i == VG_AR_CLIENT && !client_inited)
         continue;
      arena = arenaId_to_ArenaP(i);
      sb = maybe_findSb( arena, a );
      if (sb != NULL) {
         Word   j;
         SizeT  b_bszB;
         Block *b = NULL;

         aai->aid = i;
         aai->name = arena->name;
         for (j = 0; j < sb->n_payload_bytes; j += mk_plain_bszB(b_bszB)) {
            b     = (Block*)&sb->payload_bytes[j];
            b_bszB = get_bszB_as_is(b);
            if (a < (Addr)b + mk_plain_bszB(b_bszB))
               break;
         }
         vg_assert (b);
         aai->block_szB = get_pszB(arena, b);
         aai->rwoffset = a - (Addr)get_block_payload(arena, b);
         aai->free = !is_inuse_block(b);
         return;
      }
   }
   aai->aid = 0;
   aai->name = NULL;
   aai->block_szB = 0;
   aai->rwoffset = 0;
   aai->free = False;
}

/*------------------------------------------------------------*/
/*--- Creating and deleting blocks.                        ---*/
/*------------------------------------------------------------*/

// Mark the bytes at b .. b+bszB-1 as not in use, and add them to the
// relevant free list.

static
void mkFreeBlock ( Arena* a, Block* b, SizeT bszB, UInt b_lno )
{
   SizeT pszB = bszB_to_pszB(a, bszB);
   vg_assert(b_lno == pszB_to_listNo(pszB));
   INNER_REQUEST(VALGRIND_MAKE_MEM_UNDEFINED(b, bszB));
   // Set the size fields and indicate not-in-use.
   set_bszB(b, mk_free_bszB(bszB));

   // Add to the relevant list.
   if (a->freelist[b_lno] == NULL) {
      set_prev_b(b, b);
      set_next_b(b, b);
      a->freelist[b_lno] = b;
   } else {
      Block* b_prev = get_prev_b(a->freelist[b_lno]);
      Block* b_next = a->freelist[b_lno];
      set_next_b(b_prev, b);
      set_prev_b(b_next, b);
      set_next_b(b, b_next);
      set_prev_b(b, b_prev);
   }
#  ifdef DEBUG_MALLOC
   (void)blockSane(a,b);
#  endif
}

// Mark the bytes at b .. b+bszB-1 as in use, and set up the block
// appropriately.
static
void mkInuseBlock ( Arena* a, Block* b, SizeT bszB )
{
   UInt i;
   vg_assert(bszB >= min_useful_bszB(a));
   INNER_REQUEST(VALGRIND_MAKE_MEM_UNDEFINED(b, bszB));
   set_bszB(b, mk_inuse_bszB(bszB));
   set_prev_b(b, NULL);    // Take off freelist
   set_next_b(b, NULL);    // ditto
   if (!a->clientmem) {
      for (i = 0; i < a->rz_szB; i++) {
         set_rz_lo_byte(b, i, (UByte)(((Addr)b&0xff) ^ REDZONE_LO_MASK));
         set_rz_hi_byte(b, i, (UByte)(((Addr)b&0xff) ^ REDZONE_HI_MASK));
      }
   }
#  ifdef DEBUG_MALLOC
   (void)blockSane(a,b);
#  endif
}

// Mark the bytes at b .. b+bszB-1 as being part of a block that has been shrunk.
static
void shrinkInuseBlock ( Arena* a, Block* b, SizeT bszB )
{
   UInt i;

   vg_assert(bszB >= min_useful_bszB(a));
   INNER_REQUEST(mkBhdrAccess(a,b));
   set_bszB(b, mk_inuse_bszB(bszB));
   if (!a->clientmem) {
      for (i = 0; i < a->rz_szB; i++) {
         set_rz_lo_byte(b, i, (UByte)(((Addr)b&0xff) ^ REDZONE_LO_MASK));
         set_rz_hi_byte(b, i, (UByte)(((Addr)b&0xff) ^ REDZONE_HI_MASK));
      }
   }
   INNER_REQUEST(mkBhdrNoAccess(a,b));
   
#  ifdef DEBUG_MALLOC
   (void)blockSane(a,b);
#  endif
}

// Remove a block from a given list.  Does no sanity checking.
static
void unlinkBlock ( Arena* a, Block* b, UInt listno )
{
   vg_assert(listno < N_MALLOC_LISTS);
   if (get_prev_b(b) == b) {
      // Only one element in the list; treat it specially.
      vg_assert(get_next_b(b) == b);
      a->freelist[listno] = NULL;
   } else {
      Block* b_prev = get_prev_b(b);
      Block* b_next = get_next_b(b);
      a->freelist[listno] = b_prev;
      set_next_b(b_prev, b_next);
      set_prev_b(b_next, b_prev);
      swizzle ( a, listno );
   }
   set_prev_b(b, NULL);
   set_next_b(b, NULL);
}


/*------------------------------------------------------------*/
/*--- Core-visible functions.                              ---*/
/*------------------------------------------------------------*/

// Align the request size.
static __inline__
SizeT align_req_pszB ( SizeT req_pszB )
{
   SizeT n = VG_MIN_MALLOC_SZB-1;
   return ((req_pszB + n) & (~n));
}

static
void add_one_block_to_stats (Arena* a, SizeT loaned)
{
   a->stats__bytes_on_loan += loaned;
   if (a->stats__bytes_on_loan > a->stats__bytes_on_loan_max) {
      a->stats__bytes_on_loan_max = a->stats__bytes_on_loan;
      if (a->stats__bytes_on_loan_max >= a->next_profile_at) {
         /* next profile after 10% more growth */
         a->next_profile_at 
            = (SizeT)( 
                 (((ULong)a->stats__bytes_on_loan_max) * 105ULL) / 100ULL );
         if (VG_(clo_profile_heap))
            cc_analyse_alloc_arena(arenaP_to_ArenaId (a));
      }
   }
   a->stats__tot_blocks += (ULong)1;
   a->stats__tot_bytes  += (ULong)loaned;
}

/* Allocate a piece of memory of req_pszB bytes on the given arena.
   The function may return NULL if (and only if) aid == VG_AR_CLIENT.
   Otherwise, the function returns a non-NULL value. */
void* VG_(arena_malloc) ( ArenaId aid, const HChar* cc, SizeT req_pszB )
{
   SizeT       req_bszB, frag_bszB, b_bszB;
   UInt        lno, i;
   Superblock* new_sb = NULL;
   Block*      b = NULL;
   Arena*      a;
   void*       v;
   UWord       stats__nsearches = 0;

   ensure_mm_init(aid);
   a = arenaId_to_ArenaP(aid);

   vg_assert(req_pszB < MAX_PSZB);
   req_pszB = align_req_pszB(req_pszB);
   req_bszB = pszB_to_bszB(a, req_pszB);

   // You must provide a cost-center name against which to charge
   // this allocation; it isn't optional.
   vg_assert(cc);

   // Scan through all the big-enough freelists for a block.
   //
   // Nb: this scanning might be expensive in some cases.  Eg. if you
   // allocate lots of small objects without freeing them, but no
   // medium-sized objects, it will repeatedly scanning through the whole
   // list, and each time not find any free blocks until the last element.
   //
   // If this becomes a noticeable problem... the loop answers the question
   // "where is the first nonempty list above me?"  And most of the time,
   // you ask the same question and get the same answer.  So it would be
   // good to somehow cache the results of previous searches.
   // One possibility is an array (with N_MALLOC_LISTS elements) of
   // shortcuts.  shortcut[i] would give the index number of the nearest
   // larger list above list i which is non-empty.  Then this loop isn't
   // necessary.  However, we'd have to modify some section [ .. i-1] of the
   // shortcut array every time a list [i] changes from empty to nonempty or
   // back.  This would require care to avoid pathological worst-case
   // behaviour.
   //
   for (lno = pszB_to_listNo(req_pszB); lno < N_MALLOC_LISTS; lno++) {
      UWord nsearches_this_level = 0;
      b = a->freelist[lno];
      if (NULL == b) continue;   // If this list is empty, try the next one.
      while (True) {
         stats__nsearches++;
         nsearches_this_level++;
         if (UNLIKELY(nsearches_this_level >= 100) 
             && lno < N_MALLOC_LISTS-1) {
            /* Avoid excessive scanning on this freelist, and instead
               try the next one up.  But first, move this freelist's
               start pointer one element along, so as to ensure that
               subsequent searches of this list don't endlessly
               revisit only these 100 elements, but in fact slowly
               progress through the entire list. */
            b = a->freelist[lno];
            vg_assert(b); // this list must be nonempty!
            a->freelist[lno] = get_next_b(b); // step one along
            break;
         }
         b_bszB = get_bszB(b);
         if (b_bszB >= req_bszB) goto obtained_block;    // success!
         b = get_next_b(b);
         if (b == a->freelist[lno]) break;   // traversed entire freelist
      }
   }

   // If we reach here, no suitable block found, allocate a new superblock
   vg_assert(lno == N_MALLOC_LISTS);
   new_sb = newSuperblock(a, req_bszB);
   if (NULL == new_sb) {
      // Should only fail if for client, otherwise, should have aborted
      // already.
      vg_assert(VG_AR_CLIENT == aid);
      return NULL;
   }

   vg_assert(a->sblocks_used <= a->sblocks_size);
   if (a->sblocks_used == a->sblocks_size) {
      Superblock ** array;
      SysRes sres = VG_(am_mmap_anon_float_valgrind)(sizeof(Superblock *) *
                                                     a->sblocks_size * 2);
      if (sr_isError(sres)) {
         VG_(out_of_memory_NORETURN)("arena_init", sizeof(Superblock *) * 
                                                   a->sblocks_size * 2,
                                                   sr_Err(sres));
         /* NOTREACHED */
      }
      array = (Superblock**)(Addr)sr_Res(sres);
      for (i = 0; i < a->sblocks_used; ++i) array[i] = a->sblocks[i];

      a->sblocks_size *= 2;
      a->sblocks = array;
      VG_(debugLog)(1, "mallocfree", 
                       "sblock array for arena `%s' resized to %lu\n", 
                       a->name, a->sblocks_size);
   }

   vg_assert(a->sblocks_used < a->sblocks_size);
   
   i = a->sblocks_used;
   while (i > 0) {
      if (a->sblocks[i-1] > new_sb) {
         a->sblocks[i] = a->sblocks[i-1];
      } else {
         break;
      }
      --i;
   }   
   a->sblocks[i] = new_sb;
   a->sblocks_used++;

   b = (Block*)&new_sb->payload_bytes[0];
   lno = pszB_to_listNo(bszB_to_pszB(a, new_sb->n_payload_bytes));
   mkFreeBlock ( a, b, new_sb->n_payload_bytes, lno);
   if (VG_(clo_profile_heap))
      set_cc(b, "admin.free-new-sb-1");
   // fall through

  obtained_block:
   // Ok, we can allocate from b, which lives in list lno.
   vg_assert(b != NULL);
   vg_assert(lno < N_MALLOC_LISTS);
   vg_assert(a->freelist[lno] != NULL);
   b_bszB = get_bszB(b);
   // req_bszB is the size of the block we are after.  b_bszB is the
   // size of what we've actually got. */
   vg_assert(b_bszB >= req_bszB);

   // Could we split this block and still get a useful fragment?
   // A block in an unsplittable superblock can never be split.
   frag_bszB = b_bszB - req_bszB;
   if (frag_bszB >= min_useful_bszB(a)
       && (NULL == new_sb || ! new_sb->unsplittable)) {
      // Yes, split block in two, put the fragment on the appropriate free
      // list, and update b_bszB accordingly.
      // printf( "split %dB into %dB and %dB\n", b_bszB, req_bszB, frag_bszB );
      unlinkBlock(a, b, lno);
      mkInuseBlock(a, b, req_bszB);
      if (VG_(clo_profile_heap))
         set_cc(b, cc);
      mkFreeBlock(a, &b[req_bszB], frag_bszB, 
                     pszB_to_listNo(bszB_to_pszB(a, frag_bszB)));
      if (VG_(clo_profile_heap))
         set_cc(&b[req_bszB], "admin.fragmentation-1");
      b_bszB = get_bszB(b);
   } else {
      // No, mark as in use and use as-is.
      unlinkBlock(a, b, lno);
      mkInuseBlock(a, b, b_bszB);
      if (VG_(clo_profile_heap))
         set_cc(b, cc);
   }

   // Update stats
   SizeT loaned = bszB_to_pszB(a, b_bszB);
   add_one_block_to_stats (a, loaned);
   a->stats__nsearches  += (ULong)stats__nsearches;

#  ifdef DEBUG_MALLOC
   sanity_check_malloc_arena(aid);
#  endif

   v = get_block_payload(a, b);
   vg_assert( (((Addr)v) & (VG_MIN_MALLOC_SZB-1)) == 0 );

   // Which size should we pass to VALGRIND_MALLOCLIKE_BLOCK ?
   // We have 2 possible options:
   // 1. The final resulting usable size.
   // 2. The initial (non-aligned) req_pszB.
   // Memcheck implements option 2 easily, as the initial requested size
   // is maintained in the mc_chunk data structure.
   // This is not as easy in the core, as there is no such structure.
   // (note: using the aligned req_pszB is not simpler than 2, as
   //  requesting an aligned req_pszB might still be satisfied by returning
   // a (slightly) bigger block than requested if the remaining part of 
   // of a free block is not big enough to make a free block by itself).
   // Implement Sol 2 can be done the following way:
   // After having called VALGRIND_MALLOCLIKE_BLOCK, the non accessible
   // redzone just after the block can be used to determine the
   // initial requested size.
   // Currently, not implemented => we use Option 1.
   INNER_REQUEST
      (VALGRIND_MALLOCLIKE_BLOCK(v, 
                                 VG_(arena_malloc_usable_size)(aid, v), 
                                 a->rz_szB, False));

   /* For debugging/testing purposes, fill the newly allocated area
      with a definite value in an attempt to shake out any
      uninitialised uses of the data (by V core / V tools, not by the
      client).  Testing on 25 Nov 07 with the values 0x00, 0xFF, 0x55,
      0xAA showed no differences in the regression tests on
      amd64-linux.  Note, is disabled by default. */
   if (0 && aid != VG_AR_CLIENT)
      VG_(memset)(v, 0xAA, (SizeT)req_pszB);

   return v;
}

// If arena has already a deferred reclaimed superblock and
// this superblock is still reclaimable, then this superblock is first
// reclaimed.
// sb becomes then the new arena deferred superblock.
// Passing NULL as sb allows to reclaim a deferred sb without setting a new
// deferred reclaim.
static
void deferred_reclaimSuperblock ( Arena* a, Superblock* sb)
{
   
   if (sb == NULL) {
      if (!a->deferred_reclaimed_sb)
         // no deferred sb to reclaim now, nothing to do in the future =>
         // return directly.
         return;

      VG_(debugLog)(1, "mallocfree",
                    "deferred_reclaimSuperblock NULL "
                    "(prev %p) owner %s/%s\n",
                    a->deferred_reclaimed_sb,
                    a->clientmem ? "CLIENT" : "VALGRIND", a->name );
   } else
      VG_(debugLog)(1, "mallocfree",
                    "deferred_reclaimSuperblock at %p (pszB %7lu) %s "
                    "(prev %p) owner %s/%s\n",
                    sb, sb->n_payload_bytes,
                    (sb->unsplittable ? "unsplittable" : ""),
                    a->deferred_reclaimed_sb,
                    a->clientmem ? "CLIENT" : "VALGRIND", a->name );

   if (a->deferred_reclaimed_sb && a->deferred_reclaimed_sb != sb) {
      // If we are deferring another block that the current block deferred,
      // then if this block can stil be reclaimed, reclaim it now.
      // Note that we might have a re-deferred reclaim of the same block
      // with a sequence: free (causing a deferred reclaim of sb)
      //                  alloc (using a piece of memory of the deferred sb)
      //                  free of the just alloc-ed block (causing a re-defer).
      UByte*      def_sb_start;
      UByte*      def_sb_end;
      Superblock* def_sb;
      Block*      b;

      def_sb = a->deferred_reclaimed_sb;
      def_sb_start = &def_sb->payload_bytes[0];
      def_sb_end   = &def_sb->payload_bytes[def_sb->n_payload_bytes - 1];
      b = (Block *)def_sb_start;
      vg_assert (blockSane(a, b));

      // Check if the deferred_reclaimed_sb is still reclaimable.
      // If yes, we will execute the reclaim.
      if (!is_inuse_block(b)) {
         // b (at the beginning of def_sb) is not in use.
         UInt        b_listno;
         SizeT       b_bszB, b_pszB;
         b_bszB   = get_bszB(b);
         b_pszB   = bszB_to_pszB(a, b_bszB);
         if (b + b_bszB-1 == (Block*)def_sb_end) {
            // b (not in use) covers the full superblock.
            // => def_sb is still reclaimable
            // => execute now the reclaim of this def_sb.
            b_listno = pszB_to_listNo(b_pszB);
            unlinkBlock( a, b, b_listno );
            reclaimSuperblock (a, def_sb);
            a->deferred_reclaimed_sb = NULL;
         }
      }
   }

   // sb (possibly NULL) becomes the new deferred reclaimed superblock.
   a->deferred_reclaimed_sb = sb;
}

/* b must be a free block, of size b_bszB.
   If b is followed by another free block, merge them.
   If b is preceded by another free block, merge them.
   If the merge results in the superblock being fully free,
   deferred_reclaimSuperblock the superblock. */
static void mergeWithFreeNeighbours (Arena* a, Superblock* sb,
                                     Block* b, SizeT b_bszB)
{
   UByte*      sb_start;
   UByte*      sb_end;
   Block*      other_b;
   SizeT       other_bszB;
   UInt        b_listno;

   sb_start = &sb->payload_bytes[0];
   sb_end   = &sb->payload_bytes[sb->n_payload_bytes - 1];

   b_listno = pszB_to_listNo(bszB_to_pszB(a, b_bszB));

   // See if this block can be merged with its successor.
   // First test if we're far enough before the superblock's end to possibly
   // have a successor.
   other_b = b + b_bszB;
   if (other_b+min_useful_bszB(a)-1 <= (Block*)sb_end) {
      // Ok, we have a successor, merge if it's not in use.
      other_bszB = get_bszB(other_b);
      if (!is_inuse_block(other_b)) {
         // VG_(printf)( "merge-successor\n");
#        ifdef DEBUG_MALLOC
         vg_assert(blockSane(a, other_b));
#        endif
         unlinkBlock( a, b, b_listno );
         unlinkBlock( a, other_b,
                      pszB_to_listNo(bszB_to_pszB(a,other_bszB)) );
         b_bszB += other_bszB;
         b_listno = pszB_to_listNo(bszB_to_pszB(a, b_bszB));
         mkFreeBlock( a, b, b_bszB, b_listno );
         if (VG_(clo_profile_heap))
            set_cc(b, "admin.free-2");
      }
   } else {
      // Not enough space for successor: check that b is the last block
      // ie. there are no unused bytes at the end of the Superblock.
      vg_assert(other_b-1 == (Block*)sb_end);
   }

   // Then see if this block can be merged with its predecessor.
   // First test if we're far enough after the superblock's start to possibly
   // have a predecessor.
   if (b >= (Block*)sb_start + min_useful_bszB(a)) {
      // Ok, we have a predecessor, merge if it's not in use.
      other_b = get_predecessor_block( b );
      other_bszB = get_bszB(other_b);
      if (!is_inuse_block(other_b)) {
         // VG_(printf)( "merge-predecessor\n");
         unlinkBlock( a, b, b_listno );
         unlinkBlock( a, other_b,
                      pszB_to_listNo(bszB_to_pszB(a, other_bszB)) );
         b = other_b;
         b_bszB += other_bszB;
         b_listno = pszB_to_listNo(bszB_to_pszB(a, b_bszB));
         mkFreeBlock( a, b, b_bszB, b_listno );
         if (VG_(clo_profile_heap))
            set_cc(b, "admin.free-3");
      }
   } else {
      // Not enough space for predecessor: check that b is the first block,
      // ie. there are no unused bytes at the start of the Superblock.
      vg_assert((Block*)sb_start == b);
   }

   /* If the block b just merged is the only block of the superblock sb,
      then we defer reclaim sb. */
   if ( ((Block*)sb_start == b) && (b + b_bszB-1 == (Block*)sb_end) ) {
      deferred_reclaimSuperblock (a, sb);
   }
}
 
void VG_(arena_free) ( ArenaId aid, void* ptr )
{
   Superblock* sb;
   Block*      b;
   SizeT       b_bszB, b_pszB;
   UInt        b_listno;
   Arena*      a;

   ensure_mm_init(aid);
   a = arenaId_to_ArenaP(aid);

   if (ptr == NULL) {
      return;
   }
      
   b = get_payload_block(a, ptr);

   /* If this is one of V's areas, check carefully the block we're
      getting back.  This picks up simple block-end overruns. */
   if (aid != VG_AR_CLIENT)
      vg_assert(is_inuse_block(b) && blockSane(a, b));

   b_bszB   = get_bszB(b);
   b_pszB   = bszB_to_pszB(a, b_bszB);
   sb       = findSb( a, b );

   a->stats__bytes_on_loan -= b_pszB;

   /* If this is one of V's areas, fill it up with junk to enhance the
      chances of catching any later reads of it.  Note, 0xDD is
      carefully chosen junk :-), in that: (1) 0xDDDDDDDD is an invalid
      and non-word-aligned address on most systems, and (2) 0xDD is a
      value which is unlikely to be generated by the new compressed
      Vbits representation for memcheck. */
   if (aid != VG_AR_CLIENT)
      VG_(memset)(ptr, 0xDD, (SizeT)b_pszB);

   if (! sb->unsplittable) {
      // Put this chunk back on a list somewhere.
      b_listno = pszB_to_listNo(b_pszB);
      mkFreeBlock( a, b, b_bszB, b_listno );
      if (VG_(clo_profile_heap))
         set_cc(b, "admin.free-1");

      /* Possibly merge b with its predecessor or successor. */
      mergeWithFreeNeighbours (a, sb, b, b_bszB);

      // Inform that ptr has been released. We give redzone size 
      // 0 instead of a->rz_szB as proper accessibility is done just after.
      INNER_REQUEST(VALGRIND_FREELIKE_BLOCK(ptr, 0));
      
      // We need to (re-)establish the minimum accessibility needed
      // for free list management. E.g. if block ptr has been put in a free
      // list and a neighbour block is released afterwards, the
      // "lo" and "hi" portions of the block ptr will be accessed to
      // glue the 2 blocks together.
      // We could mark the whole block as not accessible, and each time
      // transiently mark accessible the needed lo/hi parts. Not done as this
      // is quite complex, for very little expected additional bug detection.
      // fully unaccessible. Note that the below marks the (possibly) merged
      // block, not the block corresponding to the ptr argument.

      // First mark the whole block unaccessible.
      INNER_REQUEST(VALGRIND_MAKE_MEM_NOACCESS(b, b_bszB));
      // Then mark the relevant administrative headers as defined.
      // No need to mark the heap profile portion as defined, this is not
      // used for free blocks.
      INNER_REQUEST(VALGRIND_MAKE_MEM_DEFINED(b + hp_overhead_szB(),
                                              sizeof(SizeT) + sizeof(void*)));
      INNER_REQUEST(VALGRIND_MAKE_MEM_DEFINED(b + b_bszB
                                              - sizeof(SizeT) - sizeof(void*),
                                              sizeof(SizeT) + sizeof(void*)));
   } else {
      vg_assert(unsplittableBlockSane(a, sb, b));

      // Inform that ptr has been released. Redzone size value
      // is not relevant (so we give  0 instead of a->rz_szB)
      // as it is expected that the aspacemgr munmap will be used by
      //  outer to mark the whole superblock as unaccessible.
      INNER_REQUEST(VALGRIND_FREELIKE_BLOCK(ptr, 0));

      // Reclaim immediately the unsplittable superblock sb.
      reclaimSuperblock (a, sb);
   }

#  ifdef DEBUG_MALLOC
   sanity_check_malloc_arena(aid);
#  endif

}


/*
   The idea for malloc_aligned() is to allocate a big block, base, and
   then split it into two parts: frag, which is returned to the free
   pool, and align, which is the bit we're really after.  Here's
   a picture.  L and H denote the block lower and upper overheads, in
   bytes.  The details are gruesome.  Note it is slightly complicated
   because the initial request to generate base may return a bigger
   block than we asked for, so it is important to distinguish the base
   request size and the base actual size.

   frag_b                   align_b
   |                        |
   |    frag_p              |    align_p
   |    |                   |    |
   v    v                   v    v

   +---+                +---+---+               +---+
   | L |----------------| H | L |---------------| H |
   +---+                +---+---+               +---+

   ^    ^                        ^
   |    |                        :
   |    base_p                   this addr must be aligned
   |
   base_b

   .    .               .   .   .               .   .
   <------ frag_bszB ------->   .               .   .
   .    <------------- base_pszB_act ----------->   .
   .    .               .   .   .               .   .

*/
void* VG_(arena_memalign) ( ArenaId aid, const HChar* cc, 
                            SizeT req_alignB, SizeT req_pszB )
{
   SizeT  base_pszB_req, base_pszB_act, frag_bszB;
   Block  *base_b, *align_b;
   UByte  *base_p, *align_p;
   SizeT  saved_bytes_on_loan;
   Arena* a;

   ensure_mm_init(aid);
   a = arenaId_to_ArenaP(aid);

   vg_assert(req_pszB < MAX_PSZB);

   // You must provide a cost-center name against which to charge
   // this allocation; it isn't optional.
   vg_assert(cc);

   // Check that the requested alignment has a plausible size.
   // Check that the requested alignment seems reasonable; that is, is
   // a power of 2.
   if (req_alignB < VG_MIN_MALLOC_SZB) {
      VG_(printf)("VG_(arena_memalign)(%p, %lu, %lu)\n"
                  "bad alignment value %lu\n"
                  "(it is too small, below the lower limit of %d)",
                  a, req_alignB, req_pszB, req_alignB, VG_MIN_MALLOC_SZB );
      VG_(core_panic)("VG_(arena_memalign)");
      /*NOTREACHED*/
   }
   if (req_alignB > 16 * 1024 * 1024) {
      VG_(printf)("VG_(arena_memalign)(%p, %lu, %lu)\n"
                  "bad alignment value %lu\n"
                  "(it is too big, larger than the upper limit of %d)",
                  a, req_alignB, req_pszB, req_alignB, 16 * 1024 * 1024 );
      VG_(core_panic)("VG_(arena_memalign)");
      /*NOTREACHED*/
   }
   if (VG_(log2)( req_alignB ) == -1 /* not a power of 2 */) {
      VG_(printf)("VG_(arena_memalign)(%p, %lu, %lu)\n"
                  "bad alignment value %lu\n"
                  "(it is not a power of two)",
                  a, req_alignB, req_pszB, req_alignB );
      VG_(core_panic)("VG_(arena_memalign)");
      /*NOTREACHED*/
   }
   // Paranoid
   vg_assert(req_alignB % VG_MIN_MALLOC_SZB == 0);

   /* Required payload size for the aligned chunk. */
   req_pszB = align_req_pszB(req_pszB);
   
   /* Payload size to request for the big block that we will split up. */
   base_pszB_req = req_pszB + min_useful_bszB(a) + req_alignB;

   /* Payload ptr for the block we are going to split.  Note this
      changes a->bytes_on_loan; we save and restore it ourselves. */
   saved_bytes_on_loan = a->stats__bytes_on_loan;
   {
      /* As we will split the block given back by VG_(arena_malloc),
         we have to (temporarily) disable unsplittable for this arena,
         as unsplittable superblocks cannot be split. */
      const SizeT save_min_unsplittable_sblock_szB 
         = a->min_unsplittable_sblock_szB;
      a->min_unsplittable_sblock_szB = MAX_PSZB;
      base_p = VG_(arena_malloc) ( aid, cc, base_pszB_req );
      a->min_unsplittable_sblock_szB = save_min_unsplittable_sblock_szB;
   }
   a->stats__bytes_on_loan = saved_bytes_on_loan;

   /* Give up if we couldn't allocate enough space */
   if (base_p == 0)
      return 0;
   /* base_p was marked as allocated by VALGRIND_MALLOCLIKE_BLOCK
      inside VG_(arena_malloc). We need to indicate it is free, then
      we need to mark it undefined to allow the below code to access is. */
   INNER_REQUEST(VALGRIND_FREELIKE_BLOCK(base_p, a->rz_szB));
   INNER_REQUEST(VALGRIND_MAKE_MEM_UNDEFINED(base_p, base_pszB_req));

   /* Block ptr for the block we are going to split. */
   base_b = get_payload_block ( a, base_p );

   /* Pointer to the payload of the aligned block we are going to
      return.  This has to be suitably aligned. */
   align_p = align_upwards ( base_b + 2 * overhead_szB_lo(a)
                                    + overhead_szB_hi(a),
                             req_alignB );
   align_b = get_payload_block(a, align_p);

   /* The block size of the fragment we will create.  This must be big
      enough to actually create a fragment. */
   frag_bszB = align_b - base_b;

   vg_assert(frag_bszB >= min_useful_bszB(a));

   /* The actual payload size of the block we are going to split. */
   base_pszB_act = get_pszB(a, base_b);

   /* Create the fragment block, and put it back on the relevant free list. */
   mkFreeBlock ( a, base_b, frag_bszB,
                 pszB_to_listNo(bszB_to_pszB(a, frag_bszB)) );
   if (VG_(clo_profile_heap))
      set_cc(base_b, "admin.frag-memalign-1");

   /* Create the aligned block. */
   mkInuseBlock ( a, align_b,
                  base_p + base_pszB_act 
                         + overhead_szB_hi(a) - (UByte*)align_b );
   if (VG_(clo_profile_heap))
      set_cc(align_b, cc);

   /* Final sanity checks. */
   vg_assert( is_inuse_block(get_payload_block(a, align_p)) );

   vg_assert(req_pszB <= get_pszB(a, get_payload_block(a, align_p)));

   a->stats__bytes_on_loan += get_pszB(a, get_payload_block(a, align_p));
   if (a->stats__bytes_on_loan > a->stats__bytes_on_loan_max) {
      a->stats__bytes_on_loan_max = a->stats__bytes_on_loan;
   }
   /* a->stats__tot_blocks, a->stats__tot_bytes, a->stats__nsearches
      are updated by the call to VG_(arena_malloc) just a few lines
      above.  So we don't need to update them here. */

#  ifdef DEBUG_MALLOC
   sanity_check_malloc_arena(aid);
#  endif

   vg_assert( (((Addr)align_p) % req_alignB) == 0 );

   INNER_REQUEST(VALGRIND_MALLOCLIKE_BLOCK(align_p,
                                           req_pszB, a->rz_szB, False));

   return align_p;
}


SizeT VG_(arena_malloc_usable_size) ( ArenaId aid, void* ptr )
{
   Arena* a = arenaId_to_ArenaP(aid);
   Block* b = get_payload_block(a, ptr);
   return get_pszB(a, b);
}


// Implementation of mallinfo(). There is no recent standard that defines
// the behavior of mallinfo(). The meaning of the fields in struct mallinfo
// is as follows:
//
//     struct mallinfo  {
//                int arena;     /* total space in arena            */
//                int ordblks;   /* number of ordinary blocks       */
//                int smblks;    /* number of small blocks          */
//                int hblks;     /* number of holding blocks        */
//                int hblkhd;    /* space in holding block headers  */
//                int usmblks;   /* space in small blocks in use    */
//                int fsmblks;   /* space in free small blocks      */
//                int uordblks;  /* space in ordinary blocks in use */
//                int fordblks;  /* space in free ordinary blocks   */
//                int keepcost;  /* space penalty if keep option    */
//                               /* is used                         */
//        };
//
// The glibc documentation about mallinfo (which is somewhat outdated) can
// be found here:
// http://www.gnu.org/software/libtool/manual/libc/Statistics-of-Malloc.html
//
// See also http://bugs.kde.org/show_bug.cgi?id=160956.
//
// Regarding the implementation of VG_(mallinfo)(): we cannot return the
// whole struct as the library function does, because this is called by a
// client request.  So instead we use a pointer to do call by reference.
void VG_(mallinfo) ( ThreadId tid, struct vg_mallinfo* mi )
{
   UWord  i, free_blocks, free_blocks_size;
   Arena* a = arenaId_to_ArenaP(VG_AR_CLIENT);

   // Traverse free list and calculate free blocks statistics.
   // This may seem slow but glibc works the same way.
   free_blocks_size = free_blocks = 0;
   for (i = 0; i < N_MALLOC_LISTS; i++) {
      Block* b = a->freelist[i];
      if (b == NULL) continue;
      for (;;) {
         free_blocks++;
         free_blocks_size += (UWord)get_pszB(a, b);
         b = get_next_b(b);
         if (b == a->freelist[i]) break;
      }
   }

   // We don't have fastbins so smblks & fsmblks are always 0. Also we don't
   // have a separate mmap allocator so set hblks & hblkhd to 0.
   mi->arena    = a->stats__bytes_mmaped;
   mi->ordblks  = free_blocks + VG_(free_queue_length);
   mi->smblks   = 0;
   mi->hblks    = 0;
   mi->hblkhd   = 0;
   mi->usmblks  = 0;
   mi->fsmblks  = 0;
   mi->uordblks = a->stats__bytes_on_loan - VG_(free_queue_volume);
   mi->fordblks = free_blocks_size + VG_(free_queue_volume);
   mi->keepcost = 0; // may want some value in here
}

SizeT VG_(arena_redzone_size) ( ArenaId aid )
{
   ensure_mm_init (VG_AR_CLIENT);
   /*  ensure_mm_init will call arena_init if not yet done.
       This then ensures that the arena redzone size is properly
       initialised. */
   return arenaId_to_ArenaP(aid)->rz_szB;
}

/*------------------------------------------------------------*/
/*--- Services layered on top of malloc/free.              ---*/
/*------------------------------------------------------------*/

void* VG_(arena_calloc) ( ArenaId aid, const HChar* cc,
                          SizeT nmemb, SizeT bytes_per_memb )
{
   SizeT  size;
   void*  p;

   size = nmemb * bytes_per_memb;
   vg_assert(size >= nmemb && size >= bytes_per_memb);// check against overflow

   p = VG_(arena_malloc) ( aid, cc, size );

   if (p != NULL)
     VG_(memset)(p, 0, size);

   return p;
}


void* VG_(arena_realloc) ( ArenaId aid, const HChar* cc, 
                           void* ptr, SizeT req_pszB )
{
   Arena* a;
   SizeT  old_pszB;
   void*  p_new;
   Block* b;

   ensure_mm_init(aid);
   a = arenaId_to_ArenaP(aid);

   vg_assert(req_pszB < MAX_PSZB);

   if (NULL == ptr) {
      return VG_(arena_malloc)(aid, cc, req_pszB);
   }

   if (req_pszB == 0) {
      VG_(arena_free)(aid, ptr);
      return NULL;
   }

   b = get_payload_block(a, ptr);
   vg_assert(blockSane(a, b));

   vg_assert(is_inuse_block(b));
   old_pszB = get_pszB(a, b);

   if (req_pszB <= old_pszB) {
      return ptr;
   }

   p_new = VG_(arena_malloc) ( aid, cc, req_pszB );
      
   VG_(memcpy)(p_new, ptr, old_pszB);

   VG_(arena_free)(aid, ptr);

   return p_new;
}


void VG_(arena_realloc_shrink) ( ArenaId aid,
                                 void* ptr, SizeT req_pszB )
{
   SizeT  req_bszB, frag_bszB, b_bszB;
   Superblock* sb;
   Arena* a;
   SizeT  old_pszB;
   Block* b;

   ensure_mm_init(aid);

   a = arenaId_to_ArenaP(aid);
   b = get_payload_block(a, ptr);
   vg_assert(blockSane(a, b));
   vg_assert(is_inuse_block(b));

   old_pszB = get_pszB(a, b);
   req_pszB = align_req_pszB(req_pszB);
   vg_assert(old_pszB >= req_pszB);
   if (old_pszB == req_pszB)
      return;

   sb = findSb( a, b );
   if (sb->unsplittable) {
      const UByte* sb_start = &sb->payload_bytes[0];
      const UByte* sb_end = &sb->payload_bytes[sb->n_payload_bytes - 1];
      Addr  frag;

      vg_assert(unsplittableBlockSane(a, sb, b));

      frag = VG_PGROUNDUP((Addr) sb 
                          + sizeof(Superblock) + pszB_to_bszB(a, req_pszB));
      frag_bszB = (Addr)sb_end - frag + 1;
      
      if (frag_bszB >= VKI_PAGE_SIZE) {
         SysRes sres;
         
         a->stats__bytes_on_loan -= old_pszB;
         b_bszB = (UByte*)frag - sb_start;
         shrinkInuseBlock(a, b, b_bszB);
         INNER_REQUEST
            (VALGRIND_RESIZEINPLACE_BLOCK(ptr,
                                          old_pszB,
                                          VG_(arena_malloc_usable_size)(aid, ptr),
                                          a->rz_szB));
         /* Have the minimum admin headers needed accessibility. */
         INNER_REQUEST(mkBhdrSzAccess(a, b));
         a->stats__bytes_on_loan += bszB_to_pszB(a, b_bszB);

         sb->n_payload_bytes -= frag_bszB;
         VG_(debugLog)(1, "mallocfree",
                       "shrink superblock %p to (pszB %7lu) "
                       "owner %s/%s (munmap-ing %p %7lu)\n",
                       sb, sb->n_payload_bytes,
                       a->clientmem ? "CLIENT" : "VALGRIND", a->name,
                       (void*) frag, frag_bszB);
         if (a->clientmem) {
            Bool need_discard = False;
            sres = VG_(am_munmap_client)(&need_discard,
                                         frag,
                                         frag_bszB);
            vg_assert (!need_discard);
         } else {
            sres = VG_(am_munmap_valgrind)(frag,
                                           frag_bszB);
         }
         vg_assert2(! sr_isError(sres), "shrink superblock munmap failure\n");
         a->stats__bytes_mmaped -= frag_bszB;

         vg_assert(unsplittableBlockSane(a, sb, b));
      }
   } else {
      req_bszB = pszB_to_bszB(a, req_pszB);
      b_bszB = get_bszB(b);
      frag_bszB = b_bszB - req_bszB;
      if (frag_bszB < min_useful_bszB(a))
         return;
      
      a->stats__bytes_on_loan -= old_pszB;
      shrinkInuseBlock(a, b, req_bszB);
      INNER_REQUEST
         (VALGRIND_RESIZEINPLACE_BLOCK(ptr,
                                       old_pszB,
                                       VG_(arena_malloc_usable_size)(aid, ptr),
                                       a->rz_szB));
      /* Have the minimum admin headers needed accessibility. */
      INNER_REQUEST(mkBhdrSzAccess(a, b));

      mkFreeBlock(a, &b[req_bszB], frag_bszB,
                  pszB_to_listNo(bszB_to_pszB(a, frag_bszB)));
      /* Mark the admin headers as accessible. */
      INNER_REQUEST(mkBhdrAccess(a, &b[req_bszB]));
      if (VG_(clo_profile_heap))
         set_cc(&b[req_bszB], "admin.fragmentation-2");
      /* Possibly merge &b[req_bszB] with its free neighbours. */
      mergeWithFreeNeighbours(a, sb, &b[req_bszB], frag_bszB);
      
      b_bszB = get_bszB(b);
      a->stats__bytes_on_loan += bszB_to_pszB(a, b_bszB);
   }

   vg_assert (blockSane(a, b));
#  ifdef DEBUG_MALLOC
   sanity_check_malloc_arena(aid);
#  endif
}

/* Inline just for the wrapper VG_(strdup) below */
__inline__ HChar* VG_(arena_strdup) ( ArenaId aid, const HChar* cc, 
                                      const HChar* s )
{
   Int   i;
   Int   len;
   HChar* res;

   if (s == NULL)
      return NULL;

   len = VG_(strlen)(s) + 1;
   res = VG_(arena_malloc) (aid, cc, len);

   for (i = 0; i < len; i++)
      res[i] = s[i];
   return res;
}

void* VG_(arena_perm_malloc) ( ArenaId aid, SizeT size, Int align  )
{
   Arena*      a;

   ensure_mm_init(aid);
   a = arenaId_to_ArenaP(aid);

   align = align - 1;
   size = (size + align) & ~align;

   if (UNLIKELY(a->perm_malloc_current + size > a->perm_malloc_limit)) {
      // Get a superblock, but we will not insert it into the superblock list.
      // The superblock structure is not needed, so we will use the full
      // memory range of it. This superblock is however counted in the
      // mmaped statistics.
      Superblock* new_sb = newSuperblock (a, size);
      a->perm_malloc_limit = (Addr)&new_sb->payload_bytes[new_sb->n_payload_bytes - 1];

      // We do not mind starting allocating from the beginning of the superblock
      // as afterwards, we "lose" it as a superblock.
      a->perm_malloc_current = (Addr)new_sb;
   }

   a->stats__perm_blocks += 1;
   a->stats__perm_bytes_on_loan  += size;
   add_one_block_to_stats (a, size);

   a->perm_malloc_current        += size;
   return (void*)(a->perm_malloc_current - size);
}

/*------------------------------------------------------------*/
/*--- Tool-visible functions.                              ---*/
/*------------------------------------------------------------*/

// All just wrappers to avoid exposing arenas to tools.

// This function never returns NULL.
void* VG_(malloc) ( const HChar* cc, SizeT nbytes )
{
   return VG_(arena_malloc) ( VG_AR_CORE, cc, nbytes );
}

void  VG_(free) ( void* ptr )
{
   VG_(arena_free) ( VG_AR_CORE, ptr );
}

void* VG_(calloc) ( const HChar* cc, SizeT nmemb, SizeT bytes_per_memb )
{
   return VG_(arena_calloc) ( VG_AR_CORE, cc, nmemb, bytes_per_memb );
}

void* VG_(realloc) ( const HChar* cc, void* ptr, SizeT size )
{
   return VG_(arena_realloc) ( VG_AR_CORE, cc, ptr, size );
}

void VG_(realloc_shrink) ( void* ptr, SizeT size )
{
   VG_(arena_realloc_shrink) ( VG_AR_CORE, ptr, size );
}

HChar* VG_(strdup) ( const HChar* cc, const HChar* s )
{
   return VG_(arena_strdup) ( VG_AR_CORE, cc, s ); 
}

void* VG_(perm_malloc) ( SizeT size, Int align  )
{
   return VG_(arena_perm_malloc) ( VG_AR_CORE, size, align );
}


/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
