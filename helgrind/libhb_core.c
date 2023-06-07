
/*--------------------------------------------------------------------*/
/*--- LibHB: a library for implementing and checking               ---*/
/*--- the happens-before relationship in concurrent programs.      ---*/
/*---                                                 libhb_main.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of LibHB, a library for implementing and checking
   the happens-before relationship in concurrent programs.

   Copyright (C) 2008-2017 OpenWorks Ltd
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
#include "pub_tool_poolalloc.h"
#include "pub_tool_libcassert.h"
#include "pub_tool_libcbase.h"
#include "pub_tool_libcprint.h"
#include "pub_tool_machine.h"
#include "pub_tool_mallocfree.h"
#include "pub_tool_wordfm.h"
#include "pub_tool_hashtable.h"
#include "pub_tool_xarray.h"
#include "pub_tool_oset.h"
#include "pub_tool_threadstate.h"
#include "pub_tool_aspacemgr.h"
#include "pub_tool_stacktrace.h"
#include "pub_tool_execontext.h"
#include "pub_tool_errormgr.h"
#include "pub_tool_debuginfo.h"
#include "pub_tool_gdbserver.h"
#include "pub_tool_options.h"        // VG_(clo_stats)
#include "hg_basics.h"
#include "hg_wordset.h"
#include "hg_lock_n_thread.h"
#include "hg_errors.h"

#include "libhb.h"


/////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////
//                                                             //
// Debugging #defines                                          //
//                                                             //
/////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////

/* Check the sanity of shadow values in the core memory state
   machine.  Change #if 0 to #if 1 to enable this. */
#if 0
#  define CHECK_MSM 1
#else
#  define CHECK_MSM 0
#endif


/* Check sanity (reference counts, etc) in the conflicting access
   machinery.  Change #if 0 to #if 1 to enable this. */
#if 0
#  define CHECK_CEM 1
#else
#  define CHECK_CEM 0
#endif


/* Check sanity in the compressed shadow memory machinery,
   particularly in its caching innards.  Unfortunately there's no
   almost-zero-cost way to make them selectable at run time.  Hence
   set the #if 0 to #if 1 and rebuild if you want them. */
#if 0
#  define CHECK_ZSM 1  /* do sanity-check CacheLine stuff */
#  define inline __attribute__((noinline))
   /* probably want to ditch -fomit-frame-pointer too */
#else
#  define CHECK_ZSM 0   /* don't sanity-check CacheLine stuff */
#endif

/* Define to 1 to activate tracing cached rcec. */
#define DEBUG_CACHED_RCEC 0

/////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////
//                                                             //
// data decls: VtsID                                           //
//                                                             //
/////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////

/* VtsIDs: Unique small-integer IDs for VTSs.  VtsIDs can't exceed 30
   bits, since they have to be packed into the lowest 30 bits of an
   SVal. */
typedef  UInt  VtsID;
#define VtsID_INVALID 0xFFFFFFFF



/////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////
//                                                             //
// data decls: SVal                                            //
//                                                             //
/////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////

typedef  ULong  SVal;

/* This value has special significance to the implementation, and callers
   may not store it in the shadow memory. */
#define SVal_INVALID (3ULL << 62)

/* This is the default value for shadow memory.  Initially the shadow
   memory contains no accessible areas and so all reads produce this
   value.  TODO: make this caller-defineable. */
#define SVal_NOACCESS (2ULL << 62)



/////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////
//                                                             //
// data decls: ScalarTS                                        //
//                                                             //
/////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////

/* Scalar Timestamp.  We have to store a lot of these, so there is
   some effort to make them as small as possible.  Logically they are
   a pair, (Thr*, ULong), but that takes 16 bytes on a 64-bit target.
   We pack it into 64 bits by representing the Thr* using a ThrID, a
   small integer (18 bits), and a 46 bit integer for the timestamp
   number.  The 46/18 split is arbitrary, but has the effect that
   Helgrind can only handle programs that create 2^18 or fewer threads
   over their entire lifetime, and have no more than 2^46 timestamp
   ticks (synchronisation operations on the same thread).

   This doesn't seem like much of a limitation.  2^46 ticks is
   7.06e+13, and if each tick (optimistically) takes the machine 1000
   cycles to process, then the minimum time to process that many ticks
   at a clock rate of 5 GHz is 162.9 days.  And that's doing nothing
   but VTS ticks, which isn't realistic.

   NB1: SCALARTS_N_THRBITS must be 27 or lower.  The obvious limit is
   32 since a ThrID is a UInt.  27 comes from the fact that
   'Thr_n_RCEC', which records information about old accesses, packs
   in tsw not only a ThrID but also minimum 4+1 other bits (access size
   and writeness) in a UInt, hence limiting size to 32-(4+1) == 27.

   NB2: thrid values are issued upwards from 1024, and values less
   than that aren't valid.  This isn't per se necessary (any order
   will do, so long as they are unique), but it does help ensure they
   are less likely to get confused with the various other kinds of
   small-integer thread ids drifting around (eg, TId).
   So, SCALARTS_N_THRBITS must be 11 or more.
   See also NB5.

   NB3: this probably also relies on the fact that Thr's are never
   deallocated -- they exist forever.  Hence the 1-1 mapping from
   Thr's to thrid values (set up in Thr__new) persists forever.

   NB4: temp_max_sized_VTS is allocated at startup and never freed.
   It is a maximum sized VTS, so has (1 << SCALARTS_N_TYMBITS)
   ScalarTSs.  So we can't make SCALARTS_N_THRBITS too large without
   making the memory use for this go sky-high.  With
   SCALARTS_N_THRBITS at 18, it occupies 2MB of memory, which seems
   like an OK tradeoff.  If more than 256k threads need to be
   supported, we could change SCALARTS_N_THRBITS to 20, which would
   facilitate supporting 1 million threads at the cost of 8MB storage
   for temp_max_sized_VTS.

   NB5: the conflicting-map mechanism (Thr_n_RCEC, specifically) uses
   ThrID == 0 to denote an empty Thr_n_RCEC record.  So ThrID == 0
   must never be a valid ThrID.  Given NB2 that's OK.
*/
#define SCALARTS_N_THRBITS 18  /* valid range: 11 to 27 inclusive,
                                  See NB1 and NB2 above. */

#define SCALARTS_N_TYMBITS (64 - SCALARTS_N_THRBITS)
typedef
   struct {
      ThrID thrid : SCALARTS_N_THRBITS;
      ULong tym   : SCALARTS_N_TYMBITS;
   }
   ScalarTS;

#define ThrID_MAX_VALID ((1 << SCALARTS_N_THRBITS) - 1)



/////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////
//                                                             //
// data decls: Filter                                          //
//                                                             //
/////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////

// baseline: 5, 9
#define FI_LINE_SZB_LOG2  5
#define FI_NUM_LINES_LOG2 10

#define FI_LINE_SZB       (1 << FI_LINE_SZB_LOG2)
#define FI_NUM_LINES      (1 << FI_NUM_LINES_LOG2)

#define FI_TAG_MASK        (~(Addr)(FI_LINE_SZB - 1))
#define FI_GET_TAG(_a)     ((_a) & FI_TAG_MASK)

#define FI_GET_LINENO(_a)  ( ((_a) >> FI_LINE_SZB_LOG2) \
                             & (Addr)(FI_NUM_LINES-1) )


/* In the lines, each 8 bytes are treated individually, and are mapped
   to a UShort.  Regardless of endianness of the underlying machine,
   bits 1 and 0 pertain to the lowest address and bits 15 and 14 to
   the highest address.

   Of each bit pair, the higher numbered bit is set if a R has been
   seen, so the actual layout is:

   15 14             ...  01 00

   R  W  for addr+7  ...  R  W  for addr+0

   So a mask for the R-bits is 0xAAAA and for the W bits is 0x5555.
*/

/* tags are separated from lines.  tags are Addrs and are
   the base address of the line. */
typedef
   struct {
      UShort u16s[FI_LINE_SZB / 8]; /* each UShort covers 8 bytes */
   }
   FiLine;

typedef
   struct {
      Addr   tags[FI_NUM_LINES];
      FiLine lines[FI_NUM_LINES];
   }
   Filter;



/////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////
//                                                             //
// data decls: Thr, ULong_n_EC                                 //
//                                                             //
/////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////

// Records stacks for H1 history mechanism (DRD-style)
typedef
   struct { ULong ull; ExeContext* ec; }
   ULong_n_EC;


/* How many of the above records to collect for each thread?  Older
   ones are dumped when we run out of space.  62.5k requires 1MB per
   thread, since each ULong_n_EC record is 16 bytes long.  When more
   than N_KWs_N_STACKs_PER_THREAD are present, the older half are
   deleted to make space.  Hence in the worst case we will be able to
   produce a stack at least for the last N_KWs_N_STACKs_PER_THREAD / 2
   Kw transitions (segments in this thread).  For the current setting
   that gives a guaranteed stack for at least the last 31.25k
   segments. */
#define N_KWs_N_STACKs_PER_THREAD 62500


UInt HG_(clo_history_backtrace_size) = 8;

// (UInt) `echo "Reference Counted Execution Context" | md5sum`
#define RCEC_MAGIC 0xab88abb2UL

/* RCEC usage is commented more in details in the section 'Change-event map2'
   later in this file */
typedef
   struct _RCEC {
      UWord magic;  /* sanity check only */
      struct _RCEC* next;
      UWord rc;
      UWord rcX; /* used for crosschecking */
      UWord frames_hash;          /* hash of all the frames */
      UWord frames[0];
      /* Variable-length array.
         The size depends on HG_(clo_history_backtrace_size). */
   }
   RCEC;

struct _Thr {
   /* Current VTSs for this thread.  They change as we go along.  viR
      is the VTS to be used for reads, viW for writes.  Usually they
      are the same, but can differ when we deal with reader-writer
      locks.  It is always the case that
         VtsID__cmpLEQ(viW,viR) == True
      that is, viW must be the same, or lagging behind, viR. */
   VtsID viR;
   VtsID viW;

   /* Is initially False, and is set to True after the thread really
      has done a low-level exit.  When True, we expect to never see
      any more memory references done by this thread. */
   Bool llexit_done;

   /* Is initially False, and is set to True after the thread has been
      joined with (reaped by some other thread).  After this point, we
      do not expect to see any uses of .viR or .viW, so it is safe to
      set them to VtsID_INVALID. */
   Bool joinedwith_done;

   /* A small integer giving a unique identity to this Thr.  See
      comments on the definition of ScalarTS for details. */
   ThrID thrid : SCALARTS_N_THRBITS;

   /* A filter that removes references for which we believe that
      msmcread/msmcwrite will not change the state, nor report a
      race. */
   Filter* filter;

   /* A pointer back to the top level Thread structure.  There is a
      1-1 mapping between Thread and Thr structures -- each Thr points
      at its corresponding Thread, and vice versa.  Really, Thr and
      Thread should be merged into a single structure. */
   Thread* hgthread;

   /* The ULongs (scalar Kws) in this accumulate in strictly
      increasing order, without duplicates.  This is important because
      we need to be able to find a given scalar Kw in this array
      later, by binary search. */
   XArray* /* ULong_n_EC */ local_Kws_n_stacks;

   /* cached_rcec maintains the last RCEC that was retrieved for this thread. */
   RCEC cached_rcec; 
   // cached_rcec value, not ref-counted.
   // As the last member of an RCEC is a variable length array, this must be
   // the last element of the  _Thr struct.

   /* The shadow register vex_shadow1 SP register (SP_s1) is used to maintain
      the validity of the cached rcec.
      If SP_s1 is 0, then the cached rcec is invalid (cannot be used).
      If SP_S1 is != 0, then the cached rcec is valid. The valid cached rcec
      can be used to generate a new RCEC by changing just the last frame. */

};



/////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////
//                                                             //
// data decls: SO                                              //
//                                                             //
/////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////

// (UInt) `echo "Synchronisation object" | md5sum`
#define SO_MAGIC 0x56b3c5b0U

struct _SO {
   struct _SO* admin_prev;
   struct _SO* admin_next;
   VtsID viR; /* r-clock of sender */
   VtsID viW; /* w-clock of sender */
   UInt  magic;
};



/////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////
//                                                             //
// Forward declarations                                        //
//                                                             //
/////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////

/* fwds for
   Globals needed by other parts of the library.  These are set
   once at startup and then never changed. */
static void        (*main_get_stacktrace)( Thr*, Addr*, UWord ) = NULL;
static ExeContext* (*main_get_EC)( Thr* ) = NULL;

/* misc fn and data fwdses */
static void VtsID__rcinc ( VtsID ii );
static void VtsID__rcdec ( VtsID ii );

static inline Bool SVal__isC ( SVal s );
static inline VtsID SVal__unC_Rmin ( SVal s );
static inline VtsID SVal__unC_Wmin ( SVal s );
static inline SVal SVal__mkC ( VtsID rmini, VtsID wmini );
static inline void SVal__rcinc ( SVal s );
static inline void SVal__rcdec ( SVal s );
/* SVal in LineZ are used to store various pointers. */
static inline void *SVal2Ptr (SVal s);
static inline SVal Ptr2SVal (void* ptr);

/* A double linked list of all the SO's. */
SO* admin_SO;



/////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////
//                                                             //
// SECTION BEGIN compressed shadow memory                      //
//                                                             //
/////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////

#ifndef __HB_ZSM_H
#define __HB_ZSM_H

/* Initialise the library.  Once initialised, it will (or may) call
   SVal__rcinc and SVal__rcdec in response to all the calls below, in order to
   allow the user to do reference counting on the SVals stored herein.
   It is important to understand, however, that due to internal
   caching, the reference counts are in general inaccurate, and can be
   both above or below the true reference count for an item.  In
   particular, the library may indicate that the reference count for
   an item is zero, when in fact it is not.

   To make the reference counting exact and therefore non-pointless,
   call zsm_flush_cache.  Immediately after it returns, the reference
   counts for all items, as deduced by the caller by observing calls
   to SVal__rcinc and SVal__rcdec, will be correct, and so any items with a
   zero reference count may be freed (or at least considered to be
   unreferenced by this library).
*/
static void zsm_init ( void );

static void zsm_sset_range  ( Addr, SizeT, SVal );
static void zsm_sset_range_SMALL ( Addr a, SizeT len, SVal svNew );
static void zsm_scopy_range ( Addr, Addr, SizeT );
static void zsm_flush_cache ( void );

#endif /* ! __HB_ZSM_H */


/* Round a up to the next multiple of N.  N must be a power of 2 */
#define ROUNDUP(a, N)   ((a + N - 1) & ~(N-1))
/* Round a down to the next multiple of N.  N must be a power of 2 */
#define ROUNDDN(a, N)   ((a) & ~(N-1))

/* True if a belongs in range [start, start + szB[
   (i.e. start + szB is excluded). */
static inline Bool address_in_range (Addr a, Addr start,  SizeT szB)
{
   /* Checking start <= a && a < start + szB.
      As start and a are unsigned addresses, the condition can
      be simplified. */
   if (CHECK_ZSM)
      tl_assert ((a - start < szB)
                 == (start <= a
                     &&       a < start + szB));
   return a - start < szB;
}

/* ------ CacheLine ------ */

#define N_LINE_BITS      6 /* must be >= 3 */
#define N_LINE_ARANGE    (1 << N_LINE_BITS)
#define N_LINE_TREES     (N_LINE_ARANGE >> 3)

typedef
   struct {
      UShort descrs[N_LINE_TREES];
      SVal   svals[N_LINE_ARANGE]; // == N_LINE_TREES * 8
   }
   CacheLine;

#define TREE_DESCR_16_0 (1<<0)
#define TREE_DESCR_32_0 (1<<1)
#define TREE_DESCR_16_1 (1<<2)
#define TREE_DESCR_64   (1<<3)
#define TREE_DESCR_16_2 (1<<4)
#define TREE_DESCR_32_1 (1<<5)
#define TREE_DESCR_16_3 (1<<6)
#define TREE_DESCR_8_0  (1<<7)
#define TREE_DESCR_8_1  (1<<8)
#define TREE_DESCR_8_2  (1<<9)
#define TREE_DESCR_8_3  (1<<10)
#define TREE_DESCR_8_4  (1<<11)
#define TREE_DESCR_8_5  (1<<12)
#define TREE_DESCR_8_6  (1<<13)
#define TREE_DESCR_8_7  (1<<14)
#define TREE_DESCR_DTY  (1<<15)

typedef
   struct {
      SVal  dict[4]; /* can represent up to 4 diff values in the line */
      UChar ix2s[N_LINE_ARANGE/4]; /* array of N_LINE_ARANGE 2-bit
                                      dict indexes */
      /* if dict[0] == SVal_INVALID then dict[1] is a pointer to the
         LineF to use, and dict[2..] are also SVal_INVALID. */
   }
   LineZ; /* compressed rep for a cache line */

/* LineZ.dict[1] is used to store various pointers:
   * In the first lineZ of a free SecMap, it points to the next free SecMap.
   * In a lineZ for which we need to use a lineF, it points to the lineF. */


typedef
   struct {
      SVal w64s[N_LINE_ARANGE];
   }
   LineF; /* full rep for a cache line */

/* We use a pool allocator for LineF, as LineF is relatively small,
   and we will often alloc/release such lines. */
static PoolAlloc* LineF_pool_allocator;

/* SVal in a lineZ are used to store various pointers.
   Below are conversion functions to support that. */
static inline LineF *LineF_Ptr (LineZ *lineZ)
{
   tl_assert(lineZ->dict[0] == SVal_INVALID);
   return SVal2Ptr (lineZ->dict[1]);
}

/* Shadow memory.
   Primary map is a WordFM Addr SecMap*.  
   SecMaps cover some page-size-ish section of address space and hold
     a compressed representation.
   CacheLine-sized chunks of SecMaps are copied into a Cache, being
   decompressed when moved into the cache and recompressed on the
   way out.  Because of this, the cache must operate as a writeback
   cache, not a writethrough one.

   Each SecMap must hold a power-of-2 number of CacheLines.  Hence
   N_SECMAP_BITS must >= N_LINE_BITS.
*/
#define N_SECMAP_BITS   13
#define N_SECMAP_ARANGE (1 << N_SECMAP_BITS)

// # CacheLines held by a SecMap
#define N_SECMAP_ZLINES (N_SECMAP_ARANGE / N_LINE_ARANGE)

/* The data in the SecMap is held in the array of LineZs.  Each LineZ
   either carries the required data directly, in a compressed
   representation, or it holds (in .dict[1]) a pointer to a LineF
   that holds the full representation.

   As each in-use LineF is referred to by exactly one LineZ,
   the number of .linesZ[] that refer to a lineF should equal
   the number of used lineF.

   RC obligations: the RCs presented to the user include exactly
   the values in:
   * direct Z reps, that is, ones for which .dict[0] != SVal_INVALID
   * F reps that are in use

   Hence the following actions at the following transitions are required:

   F rep: alloc'd       -> freed                -- rcdec_LineF
   F rep:               -> alloc'd              -- rcinc_LineF
   Z rep: .dict[0] from other to SVal_INVALID   -- rcdec_LineZ
   Z rep: .dict[0] from SVal_INVALID to other   -- rcinc_LineZ
*/

typedef
   struct {
      UInt   magic;
      LineZ  linesZ[N_SECMAP_ZLINES];
   }
   SecMap;

#define SecMap_MAGIC   0x571e58cbU

// (UInt) `echo "Free SecMap" | md5sum`
#define SecMap_free_MAGIC 0x5a977f30U

__attribute__((unused))
static inline Bool is_sane_SecMap ( SecMap* sm ) {
   return sm != NULL && sm->magic == SecMap_MAGIC;
}

/* ------ Cache ------ */

#define N_WAY_BITS 16
#define N_WAY_NENT (1 << N_WAY_BITS)

/* Each tag is the address of the associated CacheLine, rounded down
   to a CacheLine address boundary.  A CacheLine size must be a power
   of 2 and must be 8 or more.  Hence an easy way to initialise the
   cache so it is empty is to set all the tag values to any value % 8
   != 0, eg 1.  This means all queries in the cache initially miss.
   It does however require us to detect and not writeback, any line
   with a bogus tag. */
typedef
   struct {
      CacheLine lyns0[N_WAY_NENT];
      Addr      tags0[N_WAY_NENT];
   }
   Cache;

static inline Bool is_valid_scache_tag ( Addr tag ) {
   /* a valid tag should be naturally aligned to the start of
      a CacheLine. */
   return 0 == (tag & (N_LINE_ARANGE - 1));
}


/* --------- Primary data structures --------- */

/* Shadow memory primary map */
static WordFM* map_shmem = NULL; /* WordFM Addr SecMap* */
static Cache   cache_shmem;


static UWord stats__secmaps_search       = 0; // # SM finds
static UWord stats__secmaps_search_slow  = 0; // # SM lookupFMs
static UWord stats__secmaps_allocd       = 0; // # SecMaps issued
static UWord stats__secmaps_in_map_shmem = 0; // # SecMaps 'live'
static UWord stats__secmaps_scanGC       = 0; // # nr of scan GC done.
static UWord stats__secmaps_scanGCed     = 0; // # SecMaps GC-ed via scan
static UWord stats__secmaps_ssetGCed     = 0; // # SecMaps GC-ed via setnoaccess
static UWord stats__secmap_ga_space_covered = 0; // # ga bytes covered
static UWord stats__secmap_linesZ_allocd = 0; // # LineZ's issued
static UWord stats__secmap_linesZ_bytes  = 0; // .. using this much storage
static UWord stats__cache_Z_fetches      = 0; // # Z lines fetched
static UWord stats__cache_Z_wbacks       = 0; // # Z lines written back
static UWord stats__cache_F_fetches      = 0; // # F lines fetched
static UWord stats__cache_F_wbacks       = 0; // # F lines written back
static UWord stats__cache_flushes_invals = 0; // # cache flushes and invals
static UWord stats__cache_totrefs        = 0; // # total accesses
static UWord stats__cache_totmisses      = 0; // # misses
static ULong stats__cache_make_New_arange = 0; // total arange made New
static ULong stats__cache_make_New_inZrep = 0; // arange New'd on Z reps
static UWord stats__cline_normalises     = 0; // # calls to cacheline_normalise
static UWord stats__cline_cread64s       = 0; // # calls to s_m_read64
static UWord stats__cline_cread32s       = 0; // # calls to s_m_read32
static UWord stats__cline_cread16s       = 0; // # calls to s_m_read16
static UWord stats__cline_cread08s       = 0; // # calls to s_m_read8
static UWord stats__cline_cwrite64s      = 0; // # calls to s_m_write64
static UWord stats__cline_cwrite32s      = 0; // # calls to s_m_write32
static UWord stats__cline_cwrite16s      = 0; // # calls to s_m_write16
static UWord stats__cline_cwrite08s      = 0; // # calls to s_m_write8
static UWord stats__cline_sread08s       = 0; // # calls to s_m_set8
static UWord stats__cline_swrite08s      = 0; // # calls to s_m_get8
static UWord stats__cline_swrite16s      = 0; // # calls to s_m_get8
static UWord stats__cline_swrite32s      = 0; // # calls to s_m_get8
static UWord stats__cline_swrite64s      = 0; // # calls to s_m_get8
static UWord stats__cline_scopy08s       = 0; // # calls to s_m_copy8
static UWord stats__cline_64to32splits   = 0; // # 64-bit accesses split
static UWord stats__cline_32to16splits   = 0; // # 32-bit accesses split
static UWord stats__cline_16to8splits    = 0; // # 16-bit accesses split
static UWord stats__cline_64to32pulldown = 0; // # calls to pulldown_to_32
static UWord stats__cline_32to16pulldown = 0; // # calls to pulldown_to_16
static UWord stats__cline_16to8pulldown  = 0; // # calls to pulldown_to_8
static UWord stats__vts__tick            = 0; // # calls to VTS__tick
static UWord stats__vts__join            = 0; // # calls to VTS__join
static UWord stats__vts__cmpLEQ          = 0; // # calls to VTS__cmpLEQ
static UWord stats__vts__cmp_structural  = 0; // # calls to VTS__cmp_structural
static UWord stats__vts_tab_GC           = 0; // # nr of vts_tab GC
static UWord stats__vts_pruning          = 0; // # nr of vts pruning

// # calls to VTS__cmp_structural w/ slow case
static UWord stats__vts__cmp_structural_slow = 0;

// # calls to VTS__indexAt_SLOW
static UWord stats__vts__indexat_slow = 0;

// # calls to vts_set__find__or__clone_and_add
static UWord stats__vts_set__focaa    = 0;

// # calls to vts_set__find__or__clone_and_add that lead to an
// allocation
static UWord stats__vts_set__focaa_a  = 0;


static inline Addr shmem__round_to_SecMap_base ( Addr a ) {
   return a & ~(N_SECMAP_ARANGE - 1);
}
static inline UWord shmem__get_SecMap_offset ( Addr a ) {
   return a & (N_SECMAP_ARANGE - 1);
}


/*----------------------------------------------------------------*/
/*--- map_shmem :: WordFM Addr SecMap                          ---*/
/*--- shadow memory (low level handlers) (shmem__* fns)        ---*/
/*----------------------------------------------------------------*/

/*--------------- SecMap allocation --------------- */

static HChar* shmem__bigchunk_next = NULL;
static HChar* shmem__bigchunk_end1 = NULL;

static void* shmem__bigchunk_alloc ( SizeT n )
{
   const SizeT sHMEM__BIGCHUNK_SIZE = 4096 * 256 * 4;
   tl_assert(n > 0);
   n = VG_ROUNDUP(n, 16);
   tl_assert(shmem__bigchunk_next <= shmem__bigchunk_end1);
   tl_assert(shmem__bigchunk_end1 - shmem__bigchunk_next
             <= (SSizeT)sHMEM__BIGCHUNK_SIZE);
   if (shmem__bigchunk_next + n > shmem__bigchunk_end1) {
      if (0)
      VG_(printf)("XXXXX bigchunk: abandoning %d bytes\n",
                  (Int)(shmem__bigchunk_end1 - shmem__bigchunk_next));
      SysRes sres = VG_(am_shadow_alloc)( sHMEM__BIGCHUNK_SIZE );
      if (sr_isError(sres)) {
         VG_(out_of_memory_NORETURN)(
             "helgrind:shmem__bigchunk_alloc", sHMEM__BIGCHUNK_SIZE,
             sr_Err(sres));
      }
      shmem__bigchunk_next = (void*)(Addr)sr_Res(sres);;
      shmem__bigchunk_end1 = shmem__bigchunk_next + sHMEM__BIGCHUNK_SIZE;
   }
   tl_assert(shmem__bigchunk_next);
   tl_assert( 0 == (((Addr)shmem__bigchunk_next) & (16-1)) );
   tl_assert(shmem__bigchunk_next + n <= shmem__bigchunk_end1);
   shmem__bigchunk_next += n;
   return shmem__bigchunk_next - n;
}

/* SecMap changed to be fully SVal_NOACCESS are inserted in a list of
   recycled SecMap. When a new SecMap is needed, a recycled SecMap
   will be used in preference to allocating a new SecMap. */
/* We make a linked list of SecMap. The first LineZ is re-used to
   implement the linked list. */
/* Returns the SecMap following sm in the free list.
   NULL if sm is the last SecMap. sm must be on the free list. */
static inline SecMap *SecMap_freelist_next ( SecMap* sm )
{
   tl_assert (sm);
   tl_assert (sm->magic == SecMap_free_MAGIC);
   return SVal2Ptr (sm->linesZ[0].dict[1]);
}
static inline void set_SecMap_freelist_next ( SecMap* sm, SecMap* next )
{
   tl_assert (sm);
   tl_assert (sm->magic == SecMap_free_MAGIC);
   tl_assert (next == NULL || next->magic == SecMap_free_MAGIC);
   sm->linesZ[0].dict[1] = Ptr2SVal (next);
}

static SecMap *SecMap_freelist = NULL;
static UWord SecMap_freelist_length(void)
{
   SecMap *sm;
   UWord n = 0;

   sm = SecMap_freelist;
   while (sm) {
     n++;
     sm = SecMap_freelist_next (sm);
   }
   return n;
}

static void push_SecMap_on_freelist(SecMap* sm)
{
   if (0) VG_(message)(Vg_DebugMsg, "%p push\n", sm);
   sm->magic = SecMap_free_MAGIC;
   set_SecMap_freelist_next(sm, SecMap_freelist);
   SecMap_freelist = sm;
}
/* Returns a free SecMap if there is one.
   Otherwise, returns NULL. */
static SecMap *pop_SecMap_from_freelist(void)
{
   SecMap *sm;

   sm = SecMap_freelist;
   if (sm) {
      tl_assert (sm->magic == SecMap_free_MAGIC);
      SecMap_freelist = SecMap_freelist_next (sm);
      if (0) VG_(message)(Vg_DebugMsg, "%p pop\n", sm);
   }
   return sm;
}

static SecMap* shmem__alloc_or_recycle_SecMap ( void )
{
   Word    i, j;
   SecMap* sm = pop_SecMap_from_freelist();

   if (!sm) {
      sm = shmem__bigchunk_alloc( sizeof(SecMap) );
      stats__secmaps_allocd++;
      stats__secmap_ga_space_covered += N_SECMAP_ARANGE;
      stats__secmap_linesZ_allocd += N_SECMAP_ZLINES;
      stats__secmap_linesZ_bytes += N_SECMAP_ZLINES * sizeof(LineZ);
   }
   if (0) VG_(printf)("alloc_SecMap %p\n",sm);
   tl_assert(sm);
   sm->magic = SecMap_MAGIC;
   for (i = 0; i < N_SECMAP_ZLINES; i++) {
      sm->linesZ[i].dict[0] = SVal_NOACCESS;
      sm->linesZ[i].dict[1] = SVal_INVALID;
      sm->linesZ[i].dict[2] = SVal_INVALID;
      sm->linesZ[i].dict[3] = SVal_INVALID;
      for (j = 0; j < N_LINE_ARANGE/4; j++)
         sm->linesZ[i].ix2s[j] = 0; /* all reference dict[0] */
   }
   return sm;
}

typedef struct { Addr gaKey; SecMap* sm; } SMCacheEnt;
static SMCacheEnt smCache[3] = { {1,NULL}, {1,NULL}, {1,NULL} };

static SecMap* shmem__find_SecMap ( Addr ga ) 
{
   SecMap* sm    = NULL;
   Addr    gaKey = shmem__round_to_SecMap_base(ga);
   // Cache
   stats__secmaps_search++;
   if (LIKELY(gaKey == smCache[0].gaKey))
      return smCache[0].sm;
   if (LIKELY(gaKey == smCache[1].gaKey)) {
      SMCacheEnt tmp = smCache[0];
      smCache[0] = smCache[1];
      smCache[1] = tmp;
      return smCache[0].sm;
   }
   if (gaKey == smCache[2].gaKey) {
      SMCacheEnt tmp = smCache[1];
      smCache[1] = smCache[2];
      smCache[2] = tmp;
      return smCache[1].sm;
   }
   // end Cache
   stats__secmaps_search_slow++;
   if (VG_(lookupFM)( map_shmem,
                      NULL/*keyP*/, (UWord*)&sm, (UWord)gaKey )) {
      tl_assert(sm != NULL);
      smCache[2] = smCache[1];
      smCache[1] = smCache[0];
      smCache[0].gaKey = gaKey;
      smCache[0].sm    = sm;
   } else {
      tl_assert(sm == NULL);
   }
   return sm;
}

/* Scan the SecMap and count the SecMap that can be GC-ed.
   If really, really does the GC of the SecMap. */
/* NOT TO BE CALLED FROM WITHIN libzsm. */
static UWord next_SecMap_GC_at = 1000;
__attribute__((noinline))
static UWord shmem__SecMap_do_GC(Bool really)
{
   UWord secmapW = 0;
   Addr  gaKey;
   UWord examined = 0;
   UWord ok_GCed = 0;

   /* First invalidate the smCache */
   smCache[0].gaKey = 1;
   smCache[1].gaKey = 1;
   smCache[2].gaKey = 1;
   STATIC_ASSERT (3 == sizeof(smCache)/sizeof(smCache[0]));

   VG_(initIterFM)( map_shmem );
   while (VG_(nextIterFM)( map_shmem, &gaKey, &secmapW )) {
      UWord   i;
      UWord   j;
      UWord   n_linesF = 0;
      SecMap* sm = (SecMap*)secmapW;
      tl_assert(sm->magic == SecMap_MAGIC);
      Bool ok_to_GC = True;

      examined++;

      /* Deal with the LineZs and the possible LineF of a LineZ. */
      for (i = 0; i < N_SECMAP_ZLINES && ok_to_GC; i++) {
         LineZ* lineZ = &sm->linesZ[i];
         if (lineZ->dict[0] != SVal_INVALID) {
            ok_to_GC = lineZ->dict[0] == SVal_NOACCESS
               && !SVal__isC (lineZ->dict[1])
               && !SVal__isC (lineZ->dict[2])
               && !SVal__isC (lineZ->dict[3]);
         } else {
            LineF *lineF = LineF_Ptr(lineZ);
            n_linesF++;
            for (j = 0; j < N_LINE_ARANGE && ok_to_GC; j++)
               ok_to_GC = lineF->w64s[j] == SVal_NOACCESS;
         }
      }
      if (ok_to_GC)
         ok_GCed++;
      if (ok_to_GC && really) {
        SecMap *fm_sm;
        Addr fm_gaKey;
        /* We cannot remove a SecMap from map_shmem while iterating.
           So, stop iteration, remove from map_shmem, recreate the iteration
           on the next SecMap. */
        VG_(doneIterFM) ( map_shmem );
        /* No need to rcdec linesZ or linesF, these are all SVal_NOACCESS.
           We just need to free the lineF referenced by the linesZ. */
        if (n_linesF > 0) {
           for (i = 0; i < N_SECMAP_ZLINES && n_linesF > 0; i++) {
              LineZ* lineZ = &sm->linesZ[i];
              if (lineZ->dict[0] == SVal_INVALID) {
                 VG_(freeEltPA)( LineF_pool_allocator, LineF_Ptr(lineZ) );
                 n_linesF--;
              }
           }
        }
        if (!VG_(delFromFM)(map_shmem, &fm_gaKey, (UWord*)&fm_sm, gaKey))
          tl_assert (0);
        stats__secmaps_in_map_shmem--;
        tl_assert (gaKey == fm_gaKey);
        tl_assert (sm == fm_sm);
        stats__secmaps_scanGCed++;
        push_SecMap_on_freelist (sm);
        VG_(initIterAtFM) (map_shmem, gaKey + N_SECMAP_ARANGE);
      }
   }
   VG_(doneIterFM)( map_shmem );

   if (really) {
      stats__secmaps_scanGC++;
      /* Next GC when we approach the max allocated */
      next_SecMap_GC_at = stats__secmaps_allocd - 1000;
      /* Unless we GCed less than 10%. We then allow to alloc 10%
         more before GCing. This avoids doing a lot of costly GC
         for the worst case : the 'growing phase' of an application
         that allocates a lot of memory.
         Worst can can be reproduced e.g. by
             perf/memrw -t 30000000 -b 1000 -r 1 -l 1 
         that allocates around 30Gb of memory. */
      if (ok_GCed < stats__secmaps_allocd/10)
         next_SecMap_GC_at = stats__secmaps_allocd + stats__secmaps_allocd/10;

   }

   if (VG_(clo_stats) && really) {
      VG_(message)(Vg_DebugMsg,
                  "libhb: SecMap GC: #%lu scanned %lu, GCed %lu,"
                   " next GC at %lu\n",
                   stats__secmaps_scanGC, examined, ok_GCed,
                   next_SecMap_GC_at);
   }

   return ok_GCed;
}

static SecMap* shmem__find_or_alloc_SecMap ( Addr ga )
{
   SecMap* sm = shmem__find_SecMap ( ga );
   if (LIKELY(sm)) {
      if (CHECK_ZSM) tl_assert(is_sane_SecMap(sm));
      return sm;
   } else {
      /* create a new one */
      Addr gaKey = shmem__round_to_SecMap_base(ga);
      sm = shmem__alloc_or_recycle_SecMap();
      tl_assert(sm);
      VG_(addToFM)( map_shmem, (UWord)gaKey, (UWord)sm );
      stats__secmaps_in_map_shmem++;
      if (CHECK_ZSM) tl_assert(is_sane_SecMap(sm));
      return sm;
   }
}

/* Returns the nr of linesF which are in use. Note: this is scanning
   the secmap wordFM. So, this is to be used for statistics only. */
__attribute__((noinline))
static UWord shmem__SecMap_used_linesF(void)
{
   UWord secmapW = 0;
   Addr  gaKey;
   UWord inUse = 0;

   VG_(initIterFM)( map_shmem );
   while (VG_(nextIterFM)( map_shmem, &gaKey, &secmapW )) {
      UWord   i;
      SecMap* sm = (SecMap*)secmapW;
      tl_assert(sm->magic == SecMap_MAGIC);

      for (i = 0; i < N_SECMAP_ZLINES; i++) {
         LineZ* lineZ = &sm->linesZ[i];
         if (lineZ->dict[0] == SVal_INVALID)
            inUse++;
      }
   }
   VG_(doneIterFM)( map_shmem );

   return inUse;
}

/* ------------ LineF and LineZ related ------------ */

static void rcinc_LineF ( LineF* lineF ) {
   UWord i;
   for (i = 0; i < N_LINE_ARANGE; i++)
      SVal__rcinc(lineF->w64s[i]);
}

static void rcdec_LineF ( LineF* lineF ) {
   UWord i;
   for (i = 0; i < N_LINE_ARANGE; i++)
      SVal__rcdec(lineF->w64s[i]);
}

static void rcinc_LineZ ( LineZ* lineZ ) {
   tl_assert(lineZ->dict[0] != SVal_INVALID);
   SVal__rcinc(lineZ->dict[0]);
   if (lineZ->dict[1] != SVal_INVALID) SVal__rcinc(lineZ->dict[1]);
   if (lineZ->dict[2] != SVal_INVALID) SVal__rcinc(lineZ->dict[2]);
   if (lineZ->dict[3] != SVal_INVALID) SVal__rcinc(lineZ->dict[3]);
}

static void rcdec_LineZ ( LineZ* lineZ ) {
   tl_assert(lineZ->dict[0] != SVal_INVALID);
   SVal__rcdec(lineZ->dict[0]);
   if (lineZ->dict[1] != SVal_INVALID) SVal__rcdec(lineZ->dict[1]);
   if (lineZ->dict[2] != SVal_INVALID) SVal__rcdec(lineZ->dict[2]);
   if (lineZ->dict[3] != SVal_INVALID) SVal__rcdec(lineZ->dict[3]);
}

inline
static void write_twobit_array ( UChar* arr, UWord ix, UWord b2 ) {
   Word bix, shft, mask, prep;
   tl_assert(ix >= 0);
   bix  = ix >> 2;
   shft = 2 * (ix & 3); /* 0, 2, 4 or 6 */
   mask = 3 << shft;
   prep = b2 << shft;
   arr[bix] = (arr[bix] & ~mask) | prep;
}

inline
static UWord read_twobit_array ( UChar* arr, UWord ix ) {
   Word bix, shft;
   tl_assert(ix >= 0);
   bix  = ix >> 2;
   shft = 2 * (ix & 3); /* 0, 2, 4 or 6 */
   return (arr[bix] >> shft) & 3;
}

/* We cache one free lineF, to avoid pool allocator calls.
   Measurement on firefox has shown that this avoids more than 90%
   of the PA calls. */
static LineF *free_lineF = NULL;

/* Allocates a lineF for LineZ. Sets lineZ in a state indicating
   lineF has to be used. */
static inline LineF *alloc_LineF_for_Z (LineZ *lineZ)
{
   LineF *lineF;

   tl_assert(lineZ->dict[0] == SVal_INVALID);

   if (LIKELY(free_lineF)) {
      lineF = free_lineF;
      free_lineF = NULL;
   } else {
      lineF = VG_(allocEltPA) ( LineF_pool_allocator );
   }
   lineZ->dict[0] = lineZ->dict[2] = lineZ->dict[3] = SVal_INVALID;
   lineZ->dict[1] = Ptr2SVal (lineF);

   return lineF;
}

/* rcdec the LineF of lineZ, frees the lineF, and sets lineZ
   back to its initial state SVal_NOACCESS (i.e. ready to be
   read or written just after SecMap allocation). */
static inline void clear_LineF_of_Z (LineZ *lineZ)
{
   LineF *lineF = LineF_Ptr(lineZ);

   rcdec_LineF(lineF);
   if (UNLIKELY(free_lineF)) {
      VG_(freeEltPA)( LineF_pool_allocator, lineF );
   } else {
      free_lineF = lineF;
   }
   lineZ->dict[0] = SVal_NOACCESS;
   lineZ->dict[1] = SVal_INVALID;
}

/* Given address 'tag', find either the Z or F line containing relevant
   data, so it can be read into the cache.
*/
static void find_ZF_for_reading ( /*OUT*/LineZ** zp,
                                  /*OUT*/LineF** fp, Addr tag ) {
   LineZ* lineZ;
   LineF* lineF;
   UWord   zix;
   SecMap* sm    = shmem__find_or_alloc_SecMap(tag);
   UWord   smoff = shmem__get_SecMap_offset(tag);
   /* since smoff is derived from a valid tag, it should be
      cacheline-aligned. */
   tl_assert(0 == (smoff & (N_LINE_ARANGE - 1)));
   zix = smoff >> N_LINE_BITS;
   tl_assert(zix < N_SECMAP_ZLINES);
   lineZ = &sm->linesZ[zix];
   lineF = NULL;
   if (lineZ->dict[0] == SVal_INVALID) {
      lineF = LineF_Ptr (lineZ);
      lineZ = NULL;
   }
   *zp = lineZ;
   *fp = lineF;
}

/* Given address 'tag', return the relevant SecMap and the index of
   the LineZ within it, in the expectation that the line is to be
   overwritten.  Regardless of whether 'tag' is currently associated
   with a Z or F representation, to rcdec on the current
   representation, in recognition of the fact that the contents are
   just about to be overwritten. */
static __attribute__((noinline))
void find_Z_for_writing ( /*OUT*/SecMap** smp,
                          /*OUT*/Word* zixp,
                          Addr tag ) {
   LineZ* lineZ;
   UWord   zix;
   SecMap* sm    = shmem__find_or_alloc_SecMap(tag);
   UWord   smoff = shmem__get_SecMap_offset(tag);
   /* since smoff is derived from a valid tag, it should be
      cacheline-aligned. */
   tl_assert(0 == (smoff & (N_LINE_ARANGE - 1)));
   zix = smoff >> N_LINE_BITS;
   tl_assert(zix < N_SECMAP_ZLINES);
   lineZ = &sm->linesZ[zix];
   /* re RCs, we are rcdec_LineZ/clear_LineF_of_Z this LineZ so that new data
      can be parked in it.  Hence have to rcdec it accordingly. */
   /* If lineZ has an associated lineF, free it up. */
   if (lineZ->dict[0] == SVal_INVALID)
      clear_LineF_of_Z(lineZ);
   else
      rcdec_LineZ(lineZ);
   *smp  = sm;
   *zixp = zix;
}

/* ------------ CacheLine and implicit-tree related ------------ */

__attribute__((unused))
static void pp_CacheLine ( CacheLine* cl ) {
   Word i;
   if (!cl) {
      VG_(printf)("%s","pp_CacheLine(NULL)\n");
      return;
   }
   for (i = 0; i < N_LINE_TREES; i++) 
      VG_(printf)("   descr: %04lx\n", (UWord)cl->descrs[i]);
   for (i = 0; i < N_LINE_ARANGE; i++) 
      VG_(printf)("    sval: %08lx\n", (UWord)cl->svals[i]);
}

static UChar descr_to_validbits ( UShort descr )
{
   /* a.k.a Party Time for gcc's constant folder */
#  define DESCR(b8_7, b8_6, b8_5, b8_4, b8_3, b8_2, b8_1, b8_0, \
                b16_3, b32_1, b16_2, b64, b16_1, b32_0, b16_0)  \
             ( (UShort) ( ( (b8_7)  << 14) | ( (b8_6)  << 13) | \
                          ( (b8_5)  << 12) | ( (b8_4)  << 11) | \
                          ( (b8_3)  << 10) | ( (b8_2)  << 9)  | \
                          ( (b8_1)  << 8)  | ( (b8_0)  << 7)  | \
                          ( (b16_3) << 6)  | ( (b32_1) << 5)  | \
                          ( (b16_2) << 4)  | ( (b64)   << 3)  | \
                          ( (b16_1) << 2)  | ( (b32_0) << 1)  | \
                          ( (b16_0) << 0) ) )

#  define BYTE(bit7, bit6, bit5, bit4, bit3, bit2, bit1, bit0) \
             ( (UChar) ( ( (bit7) << 7) | ( (bit6) << 6) | \
                         ( (bit5) << 5) | ( (bit4) << 4) | \
                         ( (bit3) << 3) | ( (bit2) << 2) | \
                         ( (bit1) << 1) | ( (bit0) << 0) ) )

   /* these should all get folded out at compile time */
   tl_assert(DESCR(1,0,0,0,0,0,0,0, 0,0,0, 0, 0,0,0) == TREE_DESCR_8_7);
   tl_assert(DESCR(0,0,0,0,0,0,0,1, 0,0,0, 0, 0,0,0) == TREE_DESCR_8_0);
   tl_assert(DESCR(0,0,0,0,0,0,0,0, 1,0,0, 0, 0,0,0) == TREE_DESCR_16_3);
   tl_assert(DESCR(0,0,0,0,0,0,0,0, 0,1,0, 0, 0,0,0) == TREE_DESCR_32_1);
   tl_assert(DESCR(0,0,0,0,0,0,0,0, 0,0,1, 0, 0,0,0) == TREE_DESCR_16_2);
   tl_assert(DESCR(0,0,0,0,0,0,0,0, 0,0,0, 1, 0,0,0) == TREE_DESCR_64);
   tl_assert(DESCR(0,0,0,0,0,0,0,0, 0,0,0, 0, 1,0,0) == TREE_DESCR_16_1);
   tl_assert(DESCR(0,0,0,0,0,0,0,0, 0,0,0, 0, 0,1,0) == TREE_DESCR_32_0);
   tl_assert(DESCR(0,0,0,0,0,0,0,0, 0,0,0, 0, 0,0,1) == TREE_DESCR_16_0);

   switch (descr) {
   /*
              +--------------------------------- TREE_DESCR_8_7
              |             +------------------- TREE_DESCR_8_0
              |             |  +---------------- TREE_DESCR_16_3
              |             |  | +-------------- TREE_DESCR_32_1
              |             |  | | +------------ TREE_DESCR_16_2
              |             |  | | |  +--------- TREE_DESCR_64
              |             |  | | |  |  +------ TREE_DESCR_16_1
              |             |  | | |  |  | +---- TREE_DESCR_32_0
              |             |  | | |  |  | | +-- TREE_DESCR_16_0
              |             |  | | |  |  | | |
              |             |  | | |  |  | | |   GRANULARITY, 7 -> 0 */
   case DESCR(1,1,1,1,1,1,1,1, 0,0,0, 0, 0,0,0): /* 8 8 8 8  8 8 8 8 */
                                                 return BYTE(1,1,1,1,1,1,1,1);
   case DESCR(1,1,0,0,1,1,1,1, 0,0,1, 0, 0,0,0): /* 8 8 16   8 8 8 8 */
                                                 return BYTE(1,1,0,1,1,1,1,1);
   case DESCR(0,0,1,1,1,1,1,1, 1,0,0, 0, 0,0,0): /* 16  8 8  8 8 8 8 */ 
                                                 return BYTE(0,1,1,1,1,1,1,1);
   case DESCR(0,0,0,0,1,1,1,1, 1,0,1, 0, 0,0,0): /* 16  16   8 8 8 8 */
                                                 return BYTE(0,1,0,1,1,1,1,1);

   case DESCR(1,1,1,1,1,1,0,0, 0,0,0, 0, 0,0,1): /* 8 8 8 8  8 8 16 */ 
                                                 return BYTE(1,1,1,1,1,1,0,1);
   case DESCR(1,1,0,0,1,1,0,0, 0,0,1, 0, 0,0,1): /* 8 8 16   8 8 16 */
                                                 return BYTE(1,1,0,1,1,1,0,1);
   case DESCR(0,0,1,1,1,1,0,0, 1,0,0, 0, 0,0,1): /* 16  8 8  8 8 16 */
                                                 return BYTE(0,1,1,1,1,1,0,1);
   case DESCR(0,0,0,0,1,1,0,0, 1,0,1, 0, 0,0,1): /* 16  16   8 8 16 */
                                                 return BYTE(0,1,0,1,1,1,0,1);

   case DESCR(1,1,1,1,0,0,1,1, 0,0,0, 0, 1,0,0): /* 8 8 8 8  16 8 8 */
                                                 return BYTE(1,1,1,1,0,1,1,1);
   case DESCR(1,1,0,0,0,0,1,1, 0,0,1, 0, 1,0,0): /* 8 8 16   16 8 8 */
                                                 return BYTE(1,1,0,1,0,1,1,1);
   case DESCR(0,0,1,1,0,0,1,1, 1,0,0, 0, 1,0,0): /* 16  8 8  16 8 8 */
                                                 return BYTE(0,1,1,1,0,1,1,1);
   case DESCR(0,0,0,0,0,0,1,1, 1,0,1, 0, 1,0,0): /* 16  16   16 8 8 */
                                                 return BYTE(0,1,0,1,0,1,1,1);

   case DESCR(1,1,1,1,0,0,0,0, 0,0,0, 0, 1,0,1): /* 8 8 8 8  16 16 */
                                                 return BYTE(1,1,1,1,0,1,0,1);
   case DESCR(1,1,0,0,0,0,0,0, 0,0,1, 0, 1,0,1): /* 8 8 16   16 16 */
                                                 return BYTE(1,1,0,1,0,1,0,1);
   case DESCR(0,0,1,1,0,0,0,0, 1,0,0, 0, 1,0,1): /* 16  8 8  16 16 */
                                                 return BYTE(0,1,1,1,0,1,0,1);
   case DESCR(0,0,0,0,0,0,0,0, 1,0,1, 0, 1,0,1): /* 16  16   16 16 */
                                                 return BYTE(0,1,0,1,0,1,0,1);

   case DESCR(0,0,0,0,1,1,1,1, 0,1,0, 0, 0,0,0): /* 32  8 8 8 8 */
                                                 return BYTE(0,0,0,1,1,1,1,1);
   case DESCR(0,0,0,0,1,1,0,0, 0,1,0, 0, 0,0,1): /* 32  8 8 16  */
                                                 return BYTE(0,0,0,1,1,1,0,1);
   case DESCR(0,0,0,0,0,0,1,1, 0,1,0, 0, 1,0,0): /* 32  16  8 8 */
                                                 return BYTE(0,0,0,1,0,1,1,1);
   case DESCR(0,0,0,0,0,0,0,0, 0,1,0, 0, 1,0,1): /* 32  16  16  */
                                                 return BYTE(0,0,0,1,0,1,0,1);

   case DESCR(1,1,1,1,0,0,0,0, 0,0,0, 0, 0,1,0): /* 8 8 8 8  32 */
                                                 return BYTE(1,1,1,1,0,0,0,1);
   case DESCR(1,1,0,0,0,0,0,0, 0,0,1, 0, 0,1,0): /* 8 8 16   32 */
                                                 return BYTE(1,1,0,1,0,0,0,1);
   case DESCR(0,0,1,1,0,0,0,0, 1,0,0, 0, 0,1,0): /* 16  8 8  32 */
                                                 return BYTE(0,1,1,1,0,0,0,1);
   case DESCR(0,0,0,0,0,0,0,0, 1,0,1, 0, 0,1,0): /* 16  16   32 */
                                                 return BYTE(0,1,0,1,0,0,0,1);

   case DESCR(0,0,0,0,0,0,0,0, 0,1,0, 0, 0,1,0): /* 32 32 */
                                                 return BYTE(0,0,0,1,0,0,0,1);

   case DESCR(0,0,0,0,0,0,0,0, 0,0,0, 1, 0,0,0): /* 64 */
                                                 return BYTE(0,0,0,0,0,0,0,1);

   default: return BYTE(0,0,0,0,0,0,0,0); 
                   /* INVALID - any valid descr produces at least one
                      valid bit in tree[0..7]*/
   }
   /* NOTREACHED*/
   tl_assert(0);

#  undef DESCR
#  undef BYTE
}

__attribute__((unused))
static Bool is_sane_Descr ( UShort descr ) {
   return descr_to_validbits(descr) != 0;
}

static void sprintf_Descr ( /*OUT*/HChar* dst, UShort descr ) {
   VG_(sprintf)(dst, 
                "%d%d%d%d%d%d%d%d %d%d%d %d %d%d%d",
                (Int)((descr & TREE_DESCR_8_7) ? 1 : 0),
                (Int)((descr & TREE_DESCR_8_6) ? 1 : 0),
                (Int)((descr & TREE_DESCR_8_5) ? 1 : 0),
                (Int)((descr & TREE_DESCR_8_4) ? 1 : 0),
                (Int)((descr & TREE_DESCR_8_3) ? 1 : 0),
                (Int)((descr & TREE_DESCR_8_2) ? 1 : 0),
                (Int)((descr & TREE_DESCR_8_1) ? 1 : 0),
                (Int)((descr & TREE_DESCR_8_0) ? 1 : 0),
                (Int)((descr & TREE_DESCR_16_3) ? 1 : 0),
                (Int)((descr & TREE_DESCR_32_1) ? 1 : 0),
                (Int)((descr & TREE_DESCR_16_2) ? 1 : 0),
                (Int)((descr & TREE_DESCR_64)   ? 1 : 0),
                (Int)((descr & TREE_DESCR_16_1) ? 1 : 0),
                (Int)((descr & TREE_DESCR_32_0) ? 1 : 0),
                (Int)((descr & TREE_DESCR_16_0) ? 1 : 0)
   );
}
static void sprintf_Byte ( /*OUT*/HChar* dst, UChar byte ) {
   VG_(sprintf)(dst, "%d%d%d%d%d%d%d%d",
                     (Int)((byte & 128) ? 1 : 0),
                     (Int)((byte &  64) ? 1 : 0),
                     (Int)((byte &  32) ? 1 : 0),
                     (Int)((byte &  16) ? 1 : 0),
                     (Int)((byte &   8) ? 1 : 0),
                     (Int)((byte &   4) ? 1 : 0),
                     (Int)((byte &   2) ? 1 : 0),
                     (Int)((byte &   1) ? 1 : 0)
   );
}

static Bool is_sane_Descr_and_Tree ( UShort descr, SVal* tree ) {
   Word  i;
   UChar validbits = descr_to_validbits(descr);
   HChar buf[128], buf2[128];    // large enough
   if (validbits == 0)
      goto bad;
   for (i = 0; i < 8; i++) {
      if (validbits & (1<<i)) {
         if (tree[i] == SVal_INVALID)
            goto bad;
      } else {
         if (tree[i] != SVal_INVALID)
            goto bad;
      }
   }
   return True;
  bad:
   sprintf_Descr( buf, descr );
   sprintf_Byte( buf2, validbits );
   VG_(printf)("%s","is_sane_Descr_and_Tree: bad tree {\n");
   VG_(printf)("   validbits 0x%02lx    %s\n", (UWord)validbits, buf2);
   VG_(printf)("       descr 0x%04lx  %s\n", (UWord)descr, buf);
   for (i = 0; i < 8; i++)
      VG_(printf)("   [%ld] 0x%016llx\n", i, tree[i]);
   VG_(printf)("%s","}\n");
   return 0;
}

static Bool is_sane_CacheLine ( CacheLine* cl )
{
   Word tno, cloff;

   if (!cl) goto bad;

   for (tno = 0, cloff = 0;  tno < N_LINE_TREES;  tno++, cloff += 8) {
      UShort descr = cl->descrs[tno];
      SVal*  tree  = &cl->svals[cloff];
      if (!is_sane_Descr_and_Tree(descr, tree))
         goto bad;
   }
   tl_assert(cloff == N_LINE_ARANGE);
   return True;
  bad:
   pp_CacheLine(cl);
   return False;
}

static UShort normalise_tree ( /*MOD*/SVal* tree )
{
   UShort descr;
   /* pre: incoming tree[0..7] does not have any invalid shvals, in
      particular no zeroes. */
   if (CHECK_ZSM
       && UNLIKELY(tree[7] == SVal_INVALID || tree[6] == SVal_INVALID
                   || tree[5] == SVal_INVALID || tree[4] == SVal_INVALID
                   || tree[3] == SVal_INVALID || tree[2] == SVal_INVALID
                   || tree[1] == SVal_INVALID || tree[0] == SVal_INVALID))
      tl_assert(0);
   
   descr = TREE_DESCR_8_7 | TREE_DESCR_8_6 | TREE_DESCR_8_5
           | TREE_DESCR_8_4 | TREE_DESCR_8_3 | TREE_DESCR_8_2
           | TREE_DESCR_8_1 | TREE_DESCR_8_0;
   /* build 16-bit layer */
   if (tree[1] == tree[0]) {
      tree[1] = SVal_INVALID;
      descr &= ~(TREE_DESCR_8_1 | TREE_DESCR_8_0);
      descr |= TREE_DESCR_16_0;
   }
   if (tree[3] == tree[2]) {
      tree[3] = SVal_INVALID;
      descr &= ~(TREE_DESCR_8_3 | TREE_DESCR_8_2);
      descr |= TREE_DESCR_16_1;
   }
   if (tree[5] == tree[4]) {
      tree[5] = SVal_INVALID;
      descr &= ~(TREE_DESCR_8_5 | TREE_DESCR_8_4);
      descr |= TREE_DESCR_16_2;
   }
   if (tree[7] == tree[6]) {
      tree[7] = SVal_INVALID;
      descr &= ~(TREE_DESCR_8_7 | TREE_DESCR_8_6);
      descr |= TREE_DESCR_16_3;
   }
   /* build 32-bit layer */
   if (tree[2] == tree[0]
       && (descr & TREE_DESCR_16_1) && (descr & TREE_DESCR_16_0)) {
      tree[2] = SVal_INVALID; /* [3,1] must already be SVal_INVALID */
      descr &= ~(TREE_DESCR_16_1 | TREE_DESCR_16_0);
      descr |= TREE_DESCR_32_0;
   }
   if (tree[6] == tree[4]
       && (descr & TREE_DESCR_16_3) && (descr & TREE_DESCR_16_2)) {
      tree[6] = SVal_INVALID; /* [7,5] must already be SVal_INVALID */
      descr &= ~(TREE_DESCR_16_3 | TREE_DESCR_16_2);
      descr |= TREE_DESCR_32_1;
   }
   /* build 64-bit layer */
   if (tree[4] == tree[0]
       && (descr & TREE_DESCR_32_1) && (descr & TREE_DESCR_32_0)) {
      tree[4] = SVal_INVALID; /* [7,6,5,3,2,1] must already be SVal_INVALID */
      descr &= ~(TREE_DESCR_32_1 | TREE_DESCR_32_0);
      descr |= TREE_DESCR_64;
   }
   return descr;
}

/* This takes a cacheline where all the data is at the leaves
   (w8[..]) and builds a correctly normalised tree. */
static void normalise_CacheLine ( /*MOD*/CacheLine* cl )
{
   Word tno, cloff;
   for (tno = 0, cloff = 0;  tno < N_LINE_TREES;  tno++, cloff += 8) {
      SVal* tree = &cl->svals[cloff];
      cl->descrs[tno] = normalise_tree( tree );
   }
   tl_assert(cloff == N_LINE_ARANGE);
   if (CHECK_ZSM)
      tl_assert(is_sane_CacheLine(cl)); /* EXPENSIVE */
   stats__cline_normalises++;
}


typedef struct { UChar count; SVal sval; } CountedSVal;

static
void sequentialise_CacheLine ( /*OUT*/CountedSVal* dst,
                               /*OUT*/Word* dstUsedP,
                               Word nDst, CacheLine* src )
{
   Word  tno, cloff, dstUsed;

   tl_assert(nDst == N_LINE_ARANGE);
   dstUsed = 0;

   for (tno = 0, cloff = 0;  tno < N_LINE_TREES;  tno++, cloff += 8) {
      UShort descr = src->descrs[tno];
      SVal*  tree  = &src->svals[cloff];

      /* sequentialise the tree described by (descr,tree). */
#     define PUT(_n,_v)                                \
         do { dst[dstUsed  ].count = (_n);             \
              dst[dstUsed++].sval  = (_v);             \
         } while (0)

      /* byte 0 */
      if (descr & TREE_DESCR_64)   PUT(8, tree[0]); else
      if (descr & TREE_DESCR_32_0) PUT(4, tree[0]); else
      if (descr & TREE_DESCR_16_0) PUT(2, tree[0]); else
      if (descr & TREE_DESCR_8_0)  PUT(1, tree[0]);
      /* byte 1 */
      if (descr & TREE_DESCR_8_1)  PUT(1, tree[1]);
      /* byte 2 */
      if (descr & TREE_DESCR_16_1) PUT(2, tree[2]); else
      if (descr & TREE_DESCR_8_2)  PUT(1, tree[2]);
      /* byte 3 */
      if (descr & TREE_DESCR_8_3)  PUT(1, tree[3]);
      /* byte 4 */
      if (descr & TREE_DESCR_32_1) PUT(4, tree[4]); else
      if (descr & TREE_DESCR_16_2) PUT(2, tree[4]); else
      if (descr & TREE_DESCR_8_4)  PUT(1, tree[4]);
      /* byte 5 */
      if (descr & TREE_DESCR_8_5)  PUT(1, tree[5]);
      /* byte 6 */
      if (descr & TREE_DESCR_16_3) PUT(2, tree[6]); else
      if (descr & TREE_DESCR_8_6)  PUT(1, tree[6]);
      /* byte 7 */
      if (descr & TREE_DESCR_8_7)  PUT(1, tree[7]);

#     undef PUT
      /* END sequentialise the tree described by (descr,tree). */

   }
   tl_assert(cloff == N_LINE_ARANGE);
   tl_assert(dstUsed <= nDst);

   *dstUsedP = dstUsed;
}

/* Write the cacheline 'wix' to backing store.  Where it ends up
   is determined by its tag field. */
static __attribute__((noinline)) void cacheline_wback ( UWord wix )
{
   Word        i, j, k, m;
   Addr        tag;
   SecMap*     sm;
   CacheLine*  cl;
   LineZ* lineZ;
   LineF* lineF;
   Word        zix, fix, csvalsUsed;
   CountedSVal csvals[N_LINE_ARANGE];
   SVal        sv;

   if (0)
   VG_(printf)("scache wback line %d\n", (Int)wix);

   tl_assert(wix >= 0 && wix < N_WAY_NENT);

   tag =  cache_shmem.tags0[wix];
   cl  = &cache_shmem.lyns0[wix];

   /* The cache line may have been invalidated; if so, ignore it. */
   if (!is_valid_scache_tag(tag))
      return;

   /* Where are we going to put it? */
   sm         = NULL;
   lineZ      = NULL;
   lineF      = NULL;
   zix = fix = -1;

   /* find the Z line to write in and rcdec it or the associated F
      line. */
   find_Z_for_writing( &sm, &zix, tag );

   tl_assert(sm);
   tl_assert(zix >= 0 && zix < N_SECMAP_ZLINES);
   lineZ = &sm->linesZ[zix];

   /* Generate the data to be stored */
   if (CHECK_ZSM)
      tl_assert(is_sane_CacheLine(cl)); /* EXPENSIVE */

   csvalsUsed = -1;
   sequentialise_CacheLine( csvals, &csvalsUsed, 
                            N_LINE_ARANGE, cl );
   tl_assert(csvalsUsed >= 1 && csvalsUsed <= N_LINE_ARANGE);
   if (0) VG_(printf)("%ld ", csvalsUsed);

   lineZ->dict[0] = lineZ->dict[1] 
                  = lineZ->dict[2] = lineZ->dict[3] = SVal_INVALID;

   /* i indexes actual shadow values, k is cursor in csvals */
   i = 0;
   for (k = 0; k < csvalsUsed; k++) {

      sv = csvals[k].sval;
      if (CHECK_ZSM)
         tl_assert(csvals[k].count >= 1 && csvals[k].count <= 8);
      /* do we already have it? */
      if (sv == lineZ->dict[0]) { j = 0; goto dict_ok; }
      if (sv == lineZ->dict[1]) { j = 1; goto dict_ok; }
      if (sv == lineZ->dict[2]) { j = 2; goto dict_ok; }
      if (sv == lineZ->dict[3]) { j = 3; goto dict_ok; }
      /* no.  look for a free slot. */
      if (CHECK_ZSM)
         tl_assert(sv != SVal_INVALID);
      if (lineZ->dict[0] 
          == SVal_INVALID) { lineZ->dict[0] = sv; j = 0; goto dict_ok; }
      if (lineZ->dict[1]
          == SVal_INVALID) { lineZ->dict[1] = sv; j = 1; goto dict_ok; }
      if (lineZ->dict[2]
          == SVal_INVALID) { lineZ->dict[2] = sv; j = 2; goto dict_ok; }
      if (lineZ->dict[3]
          == SVal_INVALID) { lineZ->dict[3] = sv; j = 3; goto dict_ok; }
      break; /* we'll have to use the f rep */
     dict_ok:
      m = csvals[k].count;
      if (m == 8) {
         write_twobit_array( lineZ->ix2s, i+0, j );
         write_twobit_array( lineZ->ix2s, i+1, j );
         write_twobit_array( lineZ->ix2s, i+2, j );
         write_twobit_array( lineZ->ix2s, i+3, j );
         write_twobit_array( lineZ->ix2s, i+4, j );
         write_twobit_array( lineZ->ix2s, i+5, j );
         write_twobit_array( lineZ->ix2s, i+6, j );
         write_twobit_array( lineZ->ix2s, i+7, j );
         i += 8;
      }
      else if (m == 4) {
         write_twobit_array( lineZ->ix2s, i+0, j );
         write_twobit_array( lineZ->ix2s, i+1, j );
         write_twobit_array( lineZ->ix2s, i+2, j );
         write_twobit_array( lineZ->ix2s, i+3, j );
         i += 4;
      }
      else if (m == 1) {
         write_twobit_array( lineZ->ix2s, i+0, j );
         i += 1;
      }
      else if (m == 2) {
         write_twobit_array( lineZ->ix2s, i+0, j );
         write_twobit_array( lineZ->ix2s, i+1, j );
         i += 2;
      }
      else {
         tl_assert(0); /* 8 4 2 or 1 are the only legitimate values for m */
      }

   }

   if (LIKELY(i == N_LINE_ARANGE)) {
      /* Construction of the compressed representation was
         successful. */
      rcinc_LineZ(lineZ);
      stats__cache_Z_wbacks++;
   } else {
      /* Cannot use the compressed(z) representation.  Use the full(f)
         rep instead. */
      tl_assert(i >= 0 && i < N_LINE_ARANGE);
      lineZ->dict[0] = lineZ->dict[2] = lineZ->dict[3] = SVal_INVALID;
      lineF = alloc_LineF_for_Z (lineZ);
      i = 0;
      for (k = 0; k < csvalsUsed; k++) {
         if (CHECK_ZSM)
            tl_assert(csvals[k].count >= 1 && csvals[k].count <= 8);
         sv = csvals[k].sval;
         if (CHECK_ZSM)
            tl_assert(sv != SVal_INVALID);
         for (m = csvals[k].count; m > 0; m--) {
            lineF->w64s[i] = sv;
            i++;
         }
      }
      tl_assert(i == N_LINE_ARANGE);
      rcinc_LineF(lineF);
      stats__cache_F_wbacks++;
   }
}

/* Fetch the cacheline 'wix' from the backing store.  The tag
   associated with 'wix' is assumed to have already been filled in;
   hence that is used to determine where in the backing store to read
   from. */
static __attribute__((noinline)) void cacheline_fetch ( UWord wix )
{
   Word       i;
   Addr       tag;
   CacheLine* cl;
   LineZ*     lineZ;
   LineF*     lineF;

   if (0)
   VG_(printf)("scache fetch line %d\n", (Int)wix);

   tl_assert(wix >= 0 && wix < N_WAY_NENT);

   tag =  cache_shmem.tags0[wix];
   cl  = &cache_shmem.lyns0[wix];

   /* reject nonsense requests */
   tl_assert(is_valid_scache_tag(tag));

   lineZ = NULL;
   lineF = NULL;
   find_ZF_for_reading( &lineZ, &lineF, tag );
   tl_assert( (lineZ && !lineF) || (!lineZ && lineF) );

   /* expand the data into the bottom layer of the tree, then get
      cacheline_normalise to build the descriptor array. */
   if (lineF) {
      for (i = 0; i < N_LINE_ARANGE; i++) {
         cl->svals[i] = lineF->w64s[i];
      }
      stats__cache_F_fetches++;
   } else {
      for (i = 0; i < N_LINE_ARANGE; i++) {
         UWord ix = read_twobit_array( lineZ->ix2s, i );
         if (CHECK_ZSM) tl_assert(ix >= 0 && ix <= 3);
         cl->svals[i] = lineZ->dict[ix];
         if (CHECK_ZSM) tl_assert(cl->svals[i] != SVal_INVALID);
      }
      stats__cache_Z_fetches++;
   }
   normalise_CacheLine( cl );
}

/* Invalid the cachelines corresponding to the given range, which
   must start and end on a cacheline boundary. */
static void shmem__invalidate_scache_range (Addr ga, SizeT szB)
{
   Word wix;

   /* ga must be on a cacheline boundary. */
   tl_assert (is_valid_scache_tag (ga));
   /* szB must be a multiple of cacheline size. */
   tl_assert (0 == (szB & (N_LINE_ARANGE - 1)));
   

   Word ga_ix = (ga >> N_LINE_BITS) & (N_WAY_NENT - 1);
   Word nwix = szB / N_LINE_ARANGE;

   if (nwix > N_WAY_NENT)
      nwix = N_WAY_NENT; // no need to check several times the same entry.

   for (wix = 0; wix < nwix; wix++) {
      if (address_in_range(cache_shmem.tags0[ga_ix], ga, szB))
         cache_shmem.tags0[ga_ix] = 1/*INVALID*/;
      ga_ix++;
      if (UNLIKELY(ga_ix == N_WAY_NENT))
         ga_ix = 0;
   }
}


static void shmem__flush_and_invalidate_scache ( void ) {
   Word wix;
   Addr tag;
   if (0) VG_(printf)("%s","scache flush and invalidate\n");
   tl_assert(!is_valid_scache_tag(1));
   for (wix = 0; wix < N_WAY_NENT; wix++) {
      tag = cache_shmem.tags0[wix];
      if (tag == 1/*INVALID*/) {
         /* already invalid; nothing to do */
      } else {
         tl_assert(is_valid_scache_tag(tag));
         cacheline_wback( wix );
      }
      cache_shmem.tags0[wix] = 1/*INVALID*/;
   }
   stats__cache_flushes_invals++;
}


static inline Bool aligned16 ( Addr a ) {
   return 0 == (a & 1);
}
static inline Bool aligned32 ( Addr a ) {
   return 0 == (a & 3);
}
static inline Bool aligned64 ( Addr a ) {
   return 0 == (a & 7);
}
static inline UWord get_cacheline_offset ( Addr a ) {
   return (UWord)(a & (N_LINE_ARANGE - 1));
}
static inline Addr cacheline_ROUNDUP ( Addr a ) {
   return ROUNDUP(a, N_LINE_ARANGE);
}
static inline Addr cacheline_ROUNDDN ( Addr a ) {
   return ROUNDDN(a, N_LINE_ARANGE);
}
static inline UWord get_treeno ( Addr a ) {
   return get_cacheline_offset(a) >> 3;
}
static inline UWord get_tree_offset ( Addr a ) {
   return a & 7;
}

static __attribute__((noinline))
       CacheLine* get_cacheline_MISS ( Addr a ); /* fwds */
static inline CacheLine* get_cacheline ( Addr a )
{
   /* tag is 'a' with the in-line offset masked out, 
      eg a[31]..a[4] 0000 */
   Addr       tag = a & ~(N_LINE_ARANGE - 1);
   UWord      wix = (a >> N_LINE_BITS) & (N_WAY_NENT - 1);
   stats__cache_totrefs++;
   if (LIKELY(tag == cache_shmem.tags0[wix])) {
      return &cache_shmem.lyns0[wix];
   } else {
      return get_cacheline_MISS( a );
   }
}

static __attribute__((noinline))
       CacheLine* get_cacheline_MISS ( Addr a )
{
   /* tag is 'a' with the in-line offset masked out, 
      eg a[31]..a[4] 0000 */

   CacheLine* cl;
   Addr*      tag_old_p;
   Addr       tag = a & ~(N_LINE_ARANGE - 1);
   UWord      wix = (a >> N_LINE_BITS) & (N_WAY_NENT - 1);

   tl_assert(tag != cache_shmem.tags0[wix]);

   /* Dump the old line into the backing store. */
   stats__cache_totmisses++;

   cl        = &cache_shmem.lyns0[wix];
   tag_old_p = &cache_shmem.tags0[wix];

   if (is_valid_scache_tag( *tag_old_p )) {
      /* EXPENSIVE and REDUNDANT: callee does it */
      if (CHECK_ZSM)
         tl_assert(is_sane_CacheLine(cl)); /* EXPENSIVE */
      cacheline_wback( wix );
   }
   /* and reload the new one */
   *tag_old_p = tag;
   cacheline_fetch( wix );
   if (CHECK_ZSM)
      tl_assert(is_sane_CacheLine(cl)); /* EXPENSIVE */
   return cl;
}

static UShort pulldown_to_32 ( /*MOD*/SVal* tree, UWord toff, UShort descr ) {
   stats__cline_64to32pulldown++;
   switch (toff) {
      case 0: case 4:
         tl_assert(descr & TREE_DESCR_64);
         tree[4] = tree[0];
         descr &= ~TREE_DESCR_64;
         descr |= (TREE_DESCR_32_1 | TREE_DESCR_32_0);
         break;
      default:
         tl_assert(0);
   }
   return descr;
}

static UShort pulldown_to_16 ( /*MOD*/SVal* tree, UWord toff, UShort descr ) {
   stats__cline_32to16pulldown++;
   switch (toff) {
      case 0: case 2:
         if (!(descr & TREE_DESCR_32_0)) {
            descr = pulldown_to_32(tree, 0, descr);
         }
         tl_assert(descr & TREE_DESCR_32_0);
         tree[2] = tree[0];
         descr &= ~TREE_DESCR_32_0;
         descr |= (TREE_DESCR_16_1 | TREE_DESCR_16_0);
         break;
      case 4: case 6:
         if (!(descr & TREE_DESCR_32_1)) {
            descr = pulldown_to_32(tree, 4, descr);
         }
         tl_assert(descr & TREE_DESCR_32_1);
         tree[6] = tree[4];
         descr &= ~TREE_DESCR_32_1;
         descr |= (TREE_DESCR_16_3 | TREE_DESCR_16_2);
         break;
      default:
         tl_assert(0);
   }
   return descr;
}

static UShort pulldown_to_8 ( /*MOD*/SVal* tree, UWord toff, UShort descr ) {
   stats__cline_16to8pulldown++;
   switch (toff) {
      case 0: case 1:
         if (!(descr & TREE_DESCR_16_0)) {
            descr = pulldown_to_16(tree, 0, descr);
         }
         tl_assert(descr & TREE_DESCR_16_0);
         tree[1] = tree[0];
         descr &= ~TREE_DESCR_16_0;
         descr |= (TREE_DESCR_8_1 | TREE_DESCR_8_0);
         break;
      case 2: case 3:
         if (!(descr & TREE_DESCR_16_1)) {
            descr = pulldown_to_16(tree, 2, descr);
         }
         tl_assert(descr & TREE_DESCR_16_1);
         tree[3] = tree[2];
         descr &= ~TREE_DESCR_16_1;
         descr |= (TREE_DESCR_8_3 | TREE_DESCR_8_2);
         break;
      case 4: case 5:
         if (!(descr & TREE_DESCR_16_2)) {
            descr = pulldown_to_16(tree, 4, descr);
         }
         tl_assert(descr & TREE_DESCR_16_2);
         tree[5] = tree[4];
         descr &= ~TREE_DESCR_16_2;
         descr |= (TREE_DESCR_8_5 | TREE_DESCR_8_4);
         break;
      case 6: case 7:
         if (!(descr & TREE_DESCR_16_3)) {
            descr = pulldown_to_16(tree, 6, descr);
         }
         tl_assert(descr & TREE_DESCR_16_3);
         tree[7] = tree[6];
         descr &= ~TREE_DESCR_16_3;
         descr |= (TREE_DESCR_8_7 | TREE_DESCR_8_6);
         break;
      default:
         tl_assert(0);
   }
   return descr;
}


static UShort pullup_descr_to_16 ( UShort descr, UWord toff ) {
   UShort mask;
   switch (toff) {
      case 0:
         mask = TREE_DESCR_8_1 | TREE_DESCR_8_0;
         tl_assert( (descr & mask) == mask );
         descr &= ~mask;
         descr |= TREE_DESCR_16_0;
         break;
      case 2:
         mask = TREE_DESCR_8_3 | TREE_DESCR_8_2;
         tl_assert( (descr & mask) == mask );
         descr &= ~mask;
         descr |= TREE_DESCR_16_1;
         break;
      case 4:
         mask = TREE_DESCR_8_5 | TREE_DESCR_8_4;
         tl_assert( (descr & mask) == mask );
         descr &= ~mask;
         descr |= TREE_DESCR_16_2;
         break;
      case 6:
         mask = TREE_DESCR_8_7 | TREE_DESCR_8_6;
         tl_assert( (descr & mask) == mask );
         descr &= ~mask;
         descr |= TREE_DESCR_16_3;
         break;
      default:
         tl_assert(0);
   }
   return descr;
}

static UShort pullup_descr_to_32 ( UShort descr, UWord toff ) {
   UShort mask;
   switch (toff) {
      case 0:
         if (!(descr & TREE_DESCR_16_0))
            descr = pullup_descr_to_16(descr, 0);
         if (!(descr & TREE_DESCR_16_1))
            descr = pullup_descr_to_16(descr, 2);
         mask = TREE_DESCR_16_1 | TREE_DESCR_16_0;
         tl_assert( (descr & mask) == mask );
         descr &= ~mask;
         descr |= TREE_DESCR_32_0;
         break;
      case 4:
         if (!(descr & TREE_DESCR_16_2))
            descr = pullup_descr_to_16(descr, 4);
         if (!(descr & TREE_DESCR_16_3))
            descr = pullup_descr_to_16(descr, 6);
         mask = TREE_DESCR_16_3 | TREE_DESCR_16_2;
         tl_assert( (descr & mask) == mask );
         descr &= ~mask;
         descr |= TREE_DESCR_32_1;
         break;
      default:
         tl_assert(0);
   }
   return descr;
}

static Bool valid_value_is_above_me_32 ( UShort descr, UWord toff ) {
   switch (toff) {
      case 0: case 4:
         return 0 != (descr & TREE_DESCR_64);
      default:
         tl_assert(0);
   }
}

static Bool valid_value_is_below_me_16 ( UShort descr, UWord toff ) {
   switch (toff) {
      case 0:
         return 0 != (descr & (TREE_DESCR_8_1 | TREE_DESCR_8_0));
      case 2:
         return 0 != (descr & (TREE_DESCR_8_3 | TREE_DESCR_8_2));
      case 4:
         return 0 != (descr & (TREE_DESCR_8_5 | TREE_DESCR_8_4));
      case 6:
         return 0 != (descr & (TREE_DESCR_8_7 | TREE_DESCR_8_6));
      default:
         tl_assert(0);
   }
}

/* ------------ Cache management ------------ */

static void zsm_flush_cache ( void )
{
   shmem__flush_and_invalidate_scache();
}


static void zsm_init ( void )
{
   tl_assert( sizeof(UWord) == sizeof(Addr) );

   tl_assert(map_shmem == NULL);
   map_shmem = VG_(newFM)( HG_(zalloc), "libhb.zsm_init.1 (map_shmem)",
                           HG_(free), 
                           NULL/*unboxed UWord cmp*/);
   /* Invalidate all cache entries. */
   tl_assert(!is_valid_scache_tag(1));
   for (UWord wix = 0; wix < N_WAY_NENT; wix++) {
      cache_shmem.tags0[wix] = 1/*INVALID*/;
   }

   LineF_pool_allocator = VG_(newPA) (
                             sizeof(LineF),
                             /* Nr elements/pool to fill a core arena block
                                taking some arena overhead into account. */
                             (4 * 1024 * 1024 - 200)/sizeof(LineF),
                             HG_(zalloc),
                             "libhb.LineF_storage.pool",
                             HG_(free)
                          );

   /* a SecMap must contain an integral number of CacheLines */
   tl_assert(0 == (N_SECMAP_ARANGE % N_LINE_ARANGE));
   /* also ... a CacheLine holds an integral number of trees */
   tl_assert(0 == (N_LINE_ARANGE % 8));
}

/////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////
//                                                             //
// SECTION END compressed shadow memory                        //
//                                                             //
/////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////



/////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////
//                                                             //
// SECTION BEGIN vts primitives                                //
//                                                             //
/////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////


/* There's a 1-1 mapping between Thr and ThrIDs -- the latter merely
   being compact stand-ins for Thr*'s.  Use these functions to map
   between them. */
static ThrID Thr__to_ThrID   ( Thr*  thr   ); /* fwds */
static Thr*  Thr__from_ThrID ( ThrID thrid ); /* fwds */

__attribute__((noreturn))
static void scalarts_limitations_fail_NORETURN ( Bool due_to_nThrs )
{
   if (due_to_nThrs) {
      const HChar* s =
         "\n"
         "Helgrind: cannot continue, run aborted: too many threads.\n"
         "Sorry.  Helgrind can only handle programs that create\n"
         "%'llu or fewer threads over their entire lifetime.\n"
         "\n";
      VG_(umsg)(s, (ULong)(ThrID_MAX_VALID - 1024));
   } else {
      const HChar* s =
         "\n"
         "Helgrind: cannot continue, run aborted: too many\n"
         "synchronisation events.  Sorry. Helgrind can only handle\n"
         "programs which perform %'llu or fewer\n"
         "inter-thread synchronisation events (locks, unlocks, etc).\n"
         "\n";
      VG_(umsg)(s, (1ULL << SCALARTS_N_TYMBITS) - 1);
   }
   VG_(exit)(1);
   /*NOTREACHED*/
   tl_assert(0); /*wtf?!*/
}


/* The dead thread (ThrID, actually) tables.  A thread may only be
   listed here if we have been notified thereof by libhb_async_exit.
   New entries are added at the end.  The order isn't important, but
   the ThrID values must be unique.
   verydead_thread_table_not_pruned lists the identity of the threads
   that died since the previous round of pruning.
   Once pruning is done, these ThrID are added in verydead_thread_table.
   We don't actually need to keep the set of threads that have ever died --
   only the threads that have died since the previous round of
   pruning.  But it's useful for sanity check purposes to keep the
   entire set, so we do. */
static XArray* /* of ThrID */ verydead_thread_table_not_pruned = NULL;
static XArray* /* of ThrID */ verydead_thread_table = NULL;

/* Arbitrary total ordering on ThrIDs. */
static Int cmp__ThrID ( const void* v1, const void* v2 ) {
   ThrID id1 = *(const ThrID*)v1;
   ThrID id2 = *(const ThrID*)v2;
   if (id1 < id2) return -1;
   if (id1 > id2) return 1;
   return 0;
}

static void verydead_thread_tables_init ( void )
{
   tl_assert(!verydead_thread_table);
   tl_assert(!verydead_thread_table_not_pruned);
   verydead_thread_table
     = VG_(newXA)( HG_(zalloc),
                   "libhb.verydead_thread_table_init.1",
                   HG_(free), sizeof(ThrID) );
   VG_(setCmpFnXA)(verydead_thread_table, cmp__ThrID);
   verydead_thread_table_not_pruned
     = VG_(newXA)( HG_(zalloc),
                   "libhb.verydead_thread_table_init.2",
                   HG_(free), sizeof(ThrID) );
   VG_(setCmpFnXA)(verydead_thread_table_not_pruned, cmp__ThrID);
}

static void verydead_thread_table_sort_and_check (XArray* thrids)
{
   UWord i;

   VG_(sortXA)( thrids );
   /* Sanity check: check for unique .sts.thr values. */
   UWord nBT = VG_(sizeXA)( thrids );
   if (nBT > 0) {
      ThrID thrid1, thrid2;
      thrid2 = *(ThrID*)VG_(indexXA)( thrids, 0 );
      for (i = 1; i < nBT; i++) {
         thrid1 = thrid2;
         thrid2 = *(ThrID*)VG_(indexXA)( thrids, i );
         tl_assert(thrid1 < thrid2);
      }
   }
   /* Ok, so the dead thread table thrids has unique and in-order keys. */
}

/* A VTS contains .ts, its vector clock, and also .id, a field to hold
   a backlink for the caller's convenience.  Since we have no idea
   what to set that to in the library, it always gets set to
   VtsID_INVALID. */
typedef
   struct {
      VtsID    id;
      UInt     usedTS;
      UInt     sizeTS;
      ScalarTS ts[0];
   }
   VTS;

/* Allocate a VTS capable of storing 'sizeTS' entries. */
static VTS* VTS__new ( const HChar* who, UInt sizeTS );

/* Make a clone of 'vts', sizing the new array to exactly match the
   number of ScalarTSs present. */
static VTS* VTS__clone ( const HChar* who, VTS* vts );

/* Make a clone of 'vts' with the thrids in 'thrids' removed.  The new
   array is sized exactly to hold the number of required elements.
   'thridsToDel' is an array of ThrIDs to be omitted in the clone, and
   must be in strictly increasing order. */
static VTS* VTS__subtract ( const HChar* who, VTS* vts, XArray* thridsToDel );

/* Delete this VTS in its entirety. */
static void VTS__delete ( VTS* vts );

/* Create a new singleton VTS in 'out'.  Caller must have
   pre-allocated 'out' sufficiently big to hold the result in all
   possible cases. */
static void VTS__singleton ( /*OUT*/VTS* out, Thr* thr, ULong tym );

/* Create in 'out' a VTS which is the same as 'vts' except with
   vts[me]++, so to speak.  Caller must have pre-allocated 'out'
   sufficiently big to hold the result in all possible cases. */
static void VTS__tick ( /*OUT*/VTS* out, Thr* me, VTS* vts );

/* Create in 'out' a VTS which is the join (max) of 'a' and
   'b'. Caller must have pre-allocated 'out' sufficiently big to hold
   the result in all possible cases. */
static void VTS__join ( /*OUT*/VTS* out, VTS* a, VTS* b );

/* Compute the partial ordering relation of the two args.  Although we
   could be completely general and return an enumeration value (EQ,
   LT, GT, UN), in fact we only need LEQ, and so we may as well
   hardwire that fact.

   Returns zero iff LEQ(A,B), or a valid ThrID if not (zero is an
   invald ThrID).  In the latter case, the returned ThrID indicates
   the discovered point for which they are not.  There may be more
   than one such point, but we only care about seeing one of them, not
   all of them.  This rather strange convention is used because
   sometimes we want to know the actual index at which they first
   differ. */
static UInt VTS__cmpLEQ ( VTS* a, VTS* b );

/* Compute an arbitrary structural (total) ordering on the two args,
   based on their VCs, so they can be looked up in a table, tree, etc.
   Returns -1, 0 or 1. */
static Word VTS__cmp_structural ( VTS* a, VTS* b );

/* Debugging only.  Display the given VTS. */
static void VTS__show ( const VTS* vts );

/* Debugging only.  Return vts[index], so to speak. */
static ULong VTS__indexAt_SLOW ( VTS* vts, Thr* idx );

/* Notify the VTS machinery that a thread has been declared
   comprehensively dead: that is, it has done an async exit AND it has
   been joined with.  This should ensure that its local clocks (.viR
   and .viW) will never again change, and so all mentions of this
   thread from all VTSs in the system may be removed. */
static void VTS__declare_thread_very_dead ( Thr* idx );

/*--------------- to do with Vector Timestamps ---------------*/

static Bool is_sane_VTS ( VTS* vts )
{
   UWord     i, n;
   ScalarTS  *st1, *st2;
   if (!vts) return False;
   if (vts->usedTS > vts->sizeTS) return False;
   n = vts->usedTS;
   if (n == 1) {
      st1 = &vts->ts[0];
      if (st1->tym == 0)
         return False;
   }
   else
   if (n >= 2) {
      for (i = 0; i < n-1; i++) {
         st1 = &vts->ts[i];
         st2 = &vts->ts[i+1];
         if (st1->thrid >= st2->thrid)
            return False;
         if (st1->tym == 0 || st2->tym == 0)
            return False;
      }
   }
   return True;
}


/* Create a new, empty VTS.
*/
static VTS* VTS__new ( const HChar* who, UInt sizeTS )
{
   VTS* vts = HG_(zalloc)(who, sizeof(VTS) + (sizeTS+1) * sizeof(ScalarTS));
   tl_assert(vts->usedTS == 0);
   vts->sizeTS = sizeTS;
   *(ULong*)(&vts->ts[sizeTS]) = 0x0ddC0ffeeBadF00dULL;
   return vts;
}

/* Clone this VTS.
*/
static VTS* VTS__clone ( const HChar* who, VTS* vts )
{
   tl_assert(vts);
   tl_assert( *(ULong*)(&vts->ts[vts->sizeTS]) == 0x0ddC0ffeeBadF00dULL);
   UInt nTS = vts->usedTS;
   VTS* clone = VTS__new(who, nTS);
   clone->id = vts->id;
   clone->sizeTS = nTS;
   clone->usedTS = nTS;
   UInt i;
   for (i = 0; i < nTS; i++) {
      clone->ts[i] = vts->ts[i];
   }
   tl_assert( *(ULong*)(&clone->ts[clone->sizeTS]) == 0x0ddC0ffeeBadF00dULL);
   return clone;
}


/* Make a clone of a VTS with specified ThrIDs removed.  'thridsToDel'
   must be in strictly increasing order.  We could obviously do this
   much more efficiently (in linear time) if necessary.
*/
static VTS* VTS__subtract ( const HChar* who, VTS* vts, XArray* thridsToDel )
{
   UInt i, j;
   tl_assert(vts);
   tl_assert(thridsToDel);
   tl_assert( *(ULong*)(&vts->ts[vts->sizeTS]) == 0x0ddC0ffeeBadF00dULL);
   UInt nTS = vts->usedTS;
   /* Figure out how many ScalarTSs will remain in the output. */
   UInt nReq = nTS;
   for (i = 0; i < nTS; i++) {
      ThrID thrid = vts->ts[i].thrid;
      if (VG_(lookupXA)(thridsToDel, &thrid, NULL, NULL))
         nReq--;
   }
   tl_assert(nReq <= nTS);
   /* Copy the ones that will remain. */
   VTS* res = VTS__new(who, nReq);
   j = 0;
   for (i = 0; i < nTS; i++) {
      ThrID thrid = vts->ts[i].thrid;
      if (VG_(lookupXA)(thridsToDel, &thrid, NULL, NULL))
         continue;
      res->ts[j++] = vts->ts[i];
   }
   tl_assert(j == nReq);
   tl_assert(j == res->sizeTS);
   res->usedTS = j;
   tl_assert( *(ULong*)(&res->ts[j]) == 0x0ddC0ffeeBadF00dULL);
   return res;
}


/* Delete this VTS in its entirety.
*/
static void VTS__delete ( VTS* vts )
{
   tl_assert(vts);
   tl_assert(vts->usedTS <= vts->sizeTS);
   tl_assert( *(ULong*)(&vts->ts[vts->sizeTS]) == 0x0ddC0ffeeBadF00dULL);
   HG_(free)(vts);
}


/* Create a new singleton VTS. 
*/
static void VTS__singleton ( /*OUT*/VTS* out, Thr* thr, ULong tym )
{
   tl_assert(thr);
   tl_assert(tym >= 1);
   tl_assert(out);
   tl_assert(out->usedTS == 0);
   tl_assert(out->sizeTS >= 1);
   UInt hi = out->usedTS++;
   out->ts[hi].thrid = Thr__to_ThrID(thr);
   out->ts[hi].tym   = tym;
}


/* Return a new VTS in which vts[me]++, so to speak.  'vts' itself is
   not modified.
*/
static void VTS__tick ( /*OUT*/VTS* out, Thr* me, VTS* vts )
{
   UInt      i, n;
   ThrID     me_thrid;
   Bool      found = False;

   stats__vts__tick++;

   tl_assert(out);
   tl_assert(out->usedTS == 0);
   if (vts->usedTS >= ThrID_MAX_VALID)
      scalarts_limitations_fail_NORETURN( True/*due_to_nThrs*/ );
   tl_assert(out->sizeTS >= 1 + vts->usedTS);

   tl_assert(me);
   me_thrid = Thr__to_ThrID(me);
   tl_assert(is_sane_VTS(vts));
   n = vts->usedTS;

   /* Copy all entries which precede 'me'. */
   for (i = 0; i < n; i++) {
      ScalarTS* here = &vts->ts[i];
      if (UNLIKELY(here->thrid >= me_thrid))
         break;
      UInt hi = out->usedTS++;
      out->ts[hi] = *here;
   }

   /* 'i' now indicates the next entry to copy, if any.
       There are 3 possibilities:
       (a) there is no next entry (we used them all up already):
           add (me_thrid,1) to the output, and quit
       (b) there is a next entry, and its thrid > me_thrid:
           add (me_thrid,1) to the output, then copy the remaining entries
       (c) there is a next entry, and its thrid == me_thrid:
           copy it to the output but increment its timestamp value.
           Then copy the remaining entries.  (c) is the common case.
   */
   tl_assert(i >= 0 && i <= n);
   if (i == n) { /* case (a) */
      UInt hi = out->usedTS++;
      out->ts[hi].thrid = me_thrid;
      out->ts[hi].tym   = 1;
   } else {
      /* cases (b) and (c) */
      ScalarTS* here = &vts->ts[i];
      if (me_thrid == here->thrid) { /* case (c) */
         if (UNLIKELY(here->tym >= (1ULL << SCALARTS_N_TYMBITS) - 2ULL)) {
            /* We're hosed.  We have to stop. */
            scalarts_limitations_fail_NORETURN( False/*!due_to_nThrs*/ );
         }
         UInt hi = out->usedTS++;
         out->ts[hi].thrid = here->thrid;
         out->ts[hi].tym   = here->tym + 1;
         i++;
         found = True;
      } else { /* case (b) */
         UInt hi = out->usedTS++;
         out->ts[hi].thrid = me_thrid;
         out->ts[hi].tym   = 1;
      }
      /* And copy any remaining entries. */
      for (/*keepgoing*/; i < n; i++) {
         ScalarTS* here2 = &vts->ts[i];
         UInt hi = out->usedTS++;
         out->ts[hi] = *here2;
      }
   }

   tl_assert(is_sane_VTS(out));
   tl_assert(out->usedTS == vts->usedTS + (found ? 0 : 1));
   tl_assert(out->usedTS <= out->sizeTS);
}


/* Return a new VTS constructed as the join (max) of the 2 args.
   Neither arg is modified.
*/
static void VTS__join ( /*OUT*/VTS* out, VTS* a, VTS* b )
{
   UInt     ia, ib, useda, usedb;
   ULong    tyma, tymb, tymMax;
   ThrID    thrid;
   UInt     ncommon = 0;

   stats__vts__join++;

   tl_assert(a);
   tl_assert(b);
   useda = a->usedTS;
   usedb = b->usedTS;

   tl_assert(out);
   tl_assert(out->usedTS == 0);
   /* overly conservative test, but doing better involves comparing
      the two VTSs, which we don't want to do at this point. */
   if (useda + usedb >= ThrID_MAX_VALID)
      scalarts_limitations_fail_NORETURN( True/*due_to_nThrs*/ );
   tl_assert(out->sizeTS >= useda + usedb);

   ia = ib = 0;

   while (1) {

      /* This logic is to enumerate triples (thrid, tyma, tymb) drawn
         from a and b in order, where thrid is the next ThrID
         occurring in either a or b, and tyma/b are the relevant
         scalar timestamps, taking into account implicit zeroes. */
      tl_assert(ia >= 0 && ia <= useda);
      tl_assert(ib >= 0 && ib <= usedb);

      if        (ia == useda && ib == usedb) {
         /* both empty - done */
         break;

      } else if (ia == useda && ib != usedb) {
         /* a empty, use up b */
         ScalarTS* tmpb = &b->ts[ib];
         thrid = tmpb->thrid;
         tyma  = 0;
         tymb  = tmpb->tym;
         ib++;

      } else if (ia != useda && ib == usedb) {
         /* b empty, use up a */
         ScalarTS* tmpa = &a->ts[ia];
         thrid = tmpa->thrid;
         tyma  = tmpa->tym;
         tymb  = 0;
         ia++;

      } else {
         /* both not empty; extract lowest-ThrID'd triple */
         ScalarTS* tmpa = &a->ts[ia];
         ScalarTS* tmpb = &b->ts[ib];
         if (tmpa->thrid < tmpb->thrid) {
            /* a has the lowest unconsidered ThrID */
            thrid = tmpa->thrid;
            tyma  = tmpa->tym;
            tymb  = 0;
            ia++;
         } else if (tmpa->thrid > tmpb->thrid) {
            /* b has the lowest unconsidered ThrID */
            thrid = tmpb->thrid;
            tyma  = 0;
            tymb  = tmpb->tym;
            ib++;
         } else {
            /* they both next mention the same ThrID */
            tl_assert(tmpa->thrid == tmpb->thrid);
            thrid = tmpa->thrid; /* == tmpb->thrid */
            tyma  = tmpa->tym;
            tymb  = tmpb->tym;
            ia++;
            ib++;
            ncommon++;
         }
      }

      /* having laboriously determined (thr, tyma, tymb), do something
         useful with it. */
      tymMax = tyma > tymb ? tyma : tymb;
      if (tymMax > 0) {
         UInt hi = out->usedTS++;
         out->ts[hi].thrid = thrid;
         out->ts[hi].tym   = tymMax;
      }

   }

   tl_assert(is_sane_VTS(out));
   tl_assert(out->usedTS <= out->sizeTS);
   tl_assert(out->usedTS == useda + usedb - ncommon);
}


/* Determine if 'a' <= 'b', in the partial ordering.  Returns zero if
   they are, or the first ThrID for which they are not (no valid ThrID
   has the value zero).  This rather strange convention is used
   because sometimes we want to know the actual index at which they
   first differ. */
static UInt/*ThrID*/ VTS__cmpLEQ ( VTS* a, VTS* b )
{
   Word  ia, ib, useda, usedb;
   ULong tyma, tymb;

   stats__vts__cmpLEQ++;

   tl_assert(a);
   tl_assert(b);
   useda = a->usedTS;
   usedb = b->usedTS;

   ia = ib = 0;

   while (1) {

      /* This logic is to enumerate doubles (tyma, tymb) drawn
         from a and b in order, and tyma/b are the relevant
         scalar timestamps, taking into account implicit zeroes. */
      ThrID thrid;

      tl_assert(ia >= 0 && ia <= useda);
      tl_assert(ib >= 0 && ib <= usedb);

      if        (ia == useda && ib == usedb) {
         /* both empty - done */
         break;

      } else if (ia == useda && ib != usedb) {
         /* a empty, use up b */
         ScalarTS* tmpb = &b->ts[ib];
         tyma  = 0;
         tymb  = tmpb->tym;
         thrid = tmpb->thrid;
         ib++;

      } else if (ia != useda && ib == usedb) {
         /* b empty, use up a */
         ScalarTS* tmpa = &a->ts[ia];
         tyma  = tmpa->tym;
         thrid = tmpa->thrid;
         tymb  = 0;
         ia++;

      } else {
         /* both not empty; extract lowest-ThrID'd triple */
         ScalarTS* tmpa = &a->ts[ia];
         ScalarTS* tmpb = &b->ts[ib];
         if (tmpa->thrid < tmpb->thrid) {
            /* a has the lowest unconsidered ThrID */
            tyma  = tmpa->tym;
            thrid = tmpa->thrid;
            tymb  = 0;
            ia++;
         }
         else
         if (tmpa->thrid > tmpb->thrid) {
            /* b has the lowest unconsidered ThrID */
            tyma  = 0;
            tymb  = tmpb->tym;
            thrid = tmpb->thrid;
            ib++;
         } else {
            /* they both next mention the same ThrID */
            tl_assert(tmpa->thrid == tmpb->thrid);
            tyma  = tmpa->tym;
            thrid = tmpa->thrid;
            tymb  = tmpb->tym;
            ia++;
            ib++;
         }
      }

      /* having laboriously determined (tyma, tymb), do something
         useful with it. */
      if (tyma > tymb) {
         /* not LEQ at this index.  Quit, since the answer is
            determined already. */
         tl_assert(thrid >= 1024);
         return thrid;
      }
   }

   return 0; /* all points are LEQ => return an invalid ThrID */
}


/* Compute an arbitrary structural (total) ordering on the two args,
   based on their VCs, so they can be looked up in a table, tree, etc.
   Returns -1, 0 or 1.  (really just 'deriving Ord' :-) This can be
   performance critical so there is some effort expended to make it sa
   fast as possible.
*/
Word VTS__cmp_structural ( VTS* a, VTS* b )
{
   /* We just need to generate an arbitrary total ordering based on
      a->ts and b->ts.  Preferably do it in a way which comes across likely
      differences relatively quickly. */
   Word     i;
   Word     useda = 0,    usedb = 0;
   ScalarTS *ctsa = NULL, *ctsb = NULL;

   stats__vts__cmp_structural++;

   tl_assert(a);
   tl_assert(b);

   ctsa = &a->ts[0]; useda = a->usedTS;
   ctsb = &b->ts[0]; usedb = b->usedTS;

   if (LIKELY(useda == usedb)) {
      ScalarTS *tmpa = NULL, *tmpb = NULL;
      stats__vts__cmp_structural_slow++;
      /* Same length vectors.  Find the first difference, if any, as
         fast as possible. */
      for (i = 0; i < useda; i++) {
         tmpa = &ctsa[i];
         tmpb = &ctsb[i];
         if (LIKELY(tmpa->tym == tmpb->tym
                    && tmpa->thrid == tmpb->thrid))
            continue;
         else
            break;
      }
      if (UNLIKELY(i == useda)) {
         /* They're identical. */
         return 0;
      } else {
         tl_assert(i >= 0 && i < useda);
         if (tmpa->tym < tmpb->tym) return -1;
         if (tmpa->tym > tmpb->tym) return 1;
         if (tmpa->thrid < tmpb->thrid) return -1;
         if (tmpa->thrid > tmpb->thrid) return 1;
         /* we just established them as non-identical, hence: */
      }
      /*NOTREACHED*/
      tl_assert(0);
   }

   if (useda < usedb) return -1;
   if (useda > usedb) return 1;
   /*NOTREACHED*/
   tl_assert(0);
}


/* Debugging only.  Display the given VTS.
*/
static void VTS__show ( const VTS* vts )
{
   Word      i, n;
   tl_assert(vts);

   VG_(printf)("[");
   n =  vts->usedTS;
   for (i = 0; i < n; i++) {
      const ScalarTS *st = &vts->ts[i];
      VG_(printf)(i < n-1 ? "%d:%llu " : "%d:%llu", st->thrid, (ULong)st->tym);
   }
   VG_(printf)("]");
}


/* Debugging only.  Return vts[index], so to speak.
*/
ULong VTS__indexAt_SLOW ( VTS* vts, Thr* idx )
{
   UWord i, n;
   ThrID idx_thrid = Thr__to_ThrID(idx);
   stats__vts__indexat_slow++;
   tl_assert(vts);
   n = vts->usedTS;
   for (i = 0; i < n; i++) {
      ScalarTS* st = &vts->ts[i];
      if (st->thrid == idx_thrid)
         return st->tym;
   }
   return 0;
}


/* See comment on prototype above.
*/
static void VTS__declare_thread_very_dead ( Thr* thr )
{
   if (0) VG_(printf)("VTQ:  tae %p\n", thr);

   tl_assert(thr->llexit_done);
   tl_assert(thr->joinedwith_done);

   ThrID nyu;
   nyu = Thr__to_ThrID(thr);
   VG_(addToXA)( verydead_thread_table_not_pruned, &nyu );

   /* We can only get here if we're assured that we'll never again
      need to look at this thread's ::viR or ::viW.  Set them to
      VtsID_INVALID, partly so as to avoid holding on to the VTSs, but
      mostly so that we don't wind up pruning them (as that would be
      nonsensical: the only interesting ScalarTS entry for a dead
      thread is its own index, and the pruning will remove that.). */
   VtsID__rcdec(thr->viR);
   VtsID__rcdec(thr->viW);
   thr->viR = VtsID_INVALID;
   thr->viW = VtsID_INVALID;
}


/////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////
//                                                             //
// SECTION END vts primitives                                  //
//                                                             //
/////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////



/////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////
//                                                             //
// SECTION BEGIN main library                                  //
//                                                             //
/////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////


/////////////////////////////////////////////////////////
//                                                     //
// VTS set                                             //
//                                                     //
/////////////////////////////////////////////////////////

static WordFM* /* WordFM VTS* void */ vts_set = NULL;

static void vts_set_init ( void )
{
   tl_assert(!vts_set);
   vts_set = VG_(newFM)( HG_(zalloc), "libhb.vts_set_init.1",
                         HG_(free),
                         (Word(*)(UWord,UWord))VTS__cmp_structural );
}

/* Given a VTS, look in vts_set to see if we already have a
   structurally identical one.  If yes, return the pair (True, pointer
   to the existing one).  If no, clone this one, add the clone to the
   set, and return (False, pointer to the clone). */
static Bool vts_set__find__or__clone_and_add ( /*OUT*/VTS** res, VTS* cand )
{
   UWord keyW, valW;
   stats__vts_set__focaa++;
   tl_assert(cand->id == VtsID_INVALID);
   /* lookup cand (by value) */
   if (VG_(lookupFM)( vts_set, &keyW, &valW, (UWord)cand )) {
      /* found it */
      tl_assert(valW == 0);
      /* if this fails, cand (by ref) was already present (!) */
      tl_assert(keyW != (UWord)cand);
      *res = (VTS*)keyW;
      return True;
   } else {
      /* not present.  Clone, add and return address of clone. */
      stats__vts_set__focaa_a++;
      VTS* clone = VTS__clone( "libhb.vts_set_focaa.1", cand );
      tl_assert(clone != cand);
      VG_(addToFM)( vts_set, (UWord)clone, 0/*val is unused*/ );
      *res = clone;
      return False;
   }
}


/////////////////////////////////////////////////////////
//                                                     //
// VTS table                                           //
//                                                     //
/////////////////////////////////////////////////////////

static void VtsID__invalidate_caches ( void ); /* fwds */

/* A type to hold VTS table entries.  Invariants:
   If .vts == NULL, then this entry is not in use, so:
   - .rc == 0
   - this entry is on the freelist (unfortunately, does not imply
     any constraints on value for u.freelink)
   If .vts != NULL, then this entry is in use:
   - .vts is findable in vts_set
   - .vts->id == this entry number
   - no specific value for .rc (even 0 is OK)
   - this entry is not on freelist, so u.freelink == VtsID_INVALID
*/
typedef
   struct {
      VTS*  vts;      /* vts, in vts_set */
      UWord rc;       /* reference count - enough for entire aspace */
      union {
         VtsID freelink; /* chain for free entries, VtsID_INVALID at end */
         VtsID remap;    /* used only during pruning, for used entries */
      } u; 
      /* u.freelink only used when vts == NULL,
         u.remap only used when vts != NULL, during pruning. */
   }
   VtsTE;

/* The VTS table. */
static XArray* /* of VtsTE */ vts_tab = NULL;

/* An index into the VTS table, indicating the start of the list of
   free (available for use) entries.  If the list is empty, this is
   VtsID_INVALID. */
static VtsID vts_tab_freelist = VtsID_INVALID;

/* Do a GC of vts_tab when the freelist becomes empty AND the size of
   vts_tab equals or exceeds this size.  After GC, the value here is
   set appropriately so as to check for the next GC point. */
static Word vts_next_GC_at = 1000;

static void vts_tab_init ( void )
{
   vts_tab = VG_(newXA)( HG_(zalloc), "libhb.vts_tab_init.1",
                         HG_(free), sizeof(VtsTE) );
   vts_tab_freelist = VtsID_INVALID;
}

/* Add ii to the free list, checking that it looks out-of-use. */
static void add_to_free_list ( VtsID ii )
{
   VtsTE* ie = VG_(indexXA)( vts_tab, ii );
   tl_assert(ie->vts == NULL);
   tl_assert(ie->rc == 0);
   tl_assert(ie->u.freelink == VtsID_INVALID);
   ie->u.freelink = vts_tab_freelist;
   vts_tab_freelist = ii;
}

/* Get an entry from the free list.  This will return VtsID_INVALID if
   the free list is empty. */
static VtsID get_from_free_list ( void )
{
   VtsID  ii;
   VtsTE* ie;
   if (vts_tab_freelist == VtsID_INVALID)
      return VtsID_INVALID;
   ii = vts_tab_freelist;
   ie = VG_(indexXA)( vts_tab, ii );
   tl_assert(ie->vts == NULL);
   tl_assert(ie->rc == 0);
   vts_tab_freelist = ie->u.freelink;
   return ii;
}

/* Produce a new VtsID that can be used, either by getting it from
   the freelist, or, if that is empty, by expanding vts_tab. */
static VtsID get_new_VtsID ( void )
{
   VtsID ii;
   VtsTE te;
   ii = get_from_free_list();
   if (ii != VtsID_INVALID)
      return ii;
   te.vts = NULL;
   te.rc = 0;
   te.u.freelink = VtsID_INVALID;
   ii = (VtsID)VG_(addToXA)( vts_tab, &te );
   return ii;
}


/* Indirect callback from lib_zsm. */
static void VtsID__rcinc ( VtsID ii )
{
   VtsTE* ie;
   /* VG_(indexXA) does a range check for us */
   ie = VG_(indexXA)( vts_tab, ii );
   tl_assert(ie->vts); /* else it's not in use */
   tl_assert(ie->rc < ~0UL); /* else we can't continue */
   tl_assert(ie->vts->id == ii);
   ie->rc++;
}

/* Indirect callback from lib_zsm. */
static void VtsID__rcdec ( VtsID ii )
{
   VtsTE* ie;
   /* VG_(indexXA) does a range check for us */
   ie = VG_(indexXA)( vts_tab, ii );
   tl_assert(ie->vts); /* else it's not in use */
   tl_assert(ie->rc > 0); /* else RC snafu */
   tl_assert(ie->vts->id == ii);
   ie->rc--;
}


/* Look up 'cand' in our collection of VTSs.  If present, return the
   VtsID for the pre-existing version.  If not present, clone it, add
   the clone to both vts_tab and vts_set, allocate a fresh VtsID for
   it, and return that. */
static VtsID vts_tab__find__or__clone_and_add ( VTS* cand )
{
   VTS* in_tab = NULL;
   tl_assert(cand->id == VtsID_INVALID);
   Bool already_have = vts_set__find__or__clone_and_add( &in_tab, cand );
   tl_assert(in_tab);
   if (already_have) {
      /* We already have a copy of 'cand'.  Use that. */
      VtsTE* ie;
      tl_assert(in_tab->id != VtsID_INVALID);
      ie = VG_(indexXA)( vts_tab, in_tab->id );
      tl_assert(ie->vts == in_tab);
      return in_tab->id;
   } else {
      VtsID  ii = get_new_VtsID();
      VtsTE* ie = VG_(indexXA)( vts_tab, ii );
      ie->vts = in_tab;
      ie->rc = 0;
      ie->u.freelink = VtsID_INVALID;
      in_tab->id = ii;
      return ii;
   }
}


static void show_vts_stats ( const HChar* caller )
{
   UWord nSet, nTab, nLive;
   ULong totrc;
   UWord n, i;
   nSet = VG_(sizeFM)( vts_set );
   nTab = VG_(sizeXA)( vts_tab );
   totrc = 0;
   nLive = 0;
   n = VG_(sizeXA)( vts_tab );
   for (i = 0; i < n; i++) {
      VtsTE* ie = VG_(indexXA)( vts_tab, i );
      if (ie->vts) {
         nLive++;
         totrc += (ULong)ie->rc;
      } else {
         tl_assert(ie->rc == 0);
      }
   }
   VG_(printf)("  show_vts_stats %s\n", caller);
   VG_(printf)("    vts_tab size %4lu\n", nTab);
   VG_(printf)("    vts_tab live %4lu\n", nLive);
   VG_(printf)("    vts_set size %4lu\n", nSet);
   VG_(printf)("        total rc %4llu\n", totrc);
}


/* --- Helpers for VtsID pruning --- */

static
void remap_VtsID ( /*MOD*/XArray* /* of VtsTE */ old_tab,
                   /*MOD*/XArray* /* of VtsTE */ new_tab,
                   VtsID* ii )
{
   VtsTE *old_te, *new_te;
   VtsID old_id, new_id;
   /* We're relying here on VG_(indexXA)'s range checking to assert on
      any stupid values, in particular *ii == VtsID_INVALID. */
   old_id = *ii;
   old_te = VG_(indexXA)( old_tab, old_id );
   old_te->rc--;
   new_id = old_te->u.remap;
   new_te = VG_(indexXA)( new_tab, new_id );
   new_te->rc++;
   *ii = new_id;
}

static
void remap_VtsIDs_in_SVal ( /*MOD*/XArray* /* of VtsTE */ old_tab,
                            /*MOD*/XArray* /* of VtsTE */ new_tab,
                            SVal* s )
{
   SVal old_sv, new_sv;
   old_sv = *s;
   if (SVal__isC(old_sv)) {
      VtsID rMin, wMin;
      rMin = SVal__unC_Rmin(old_sv);
      wMin = SVal__unC_Wmin(old_sv);
      remap_VtsID( old_tab, new_tab, &rMin );
      remap_VtsID( old_tab, new_tab, &wMin );
      new_sv = SVal__mkC( rMin, wMin );
      *s = new_sv;
  }
}


/* NOT TO BE CALLED FROM WITHIN libzsm. */
__attribute__((noinline))
static void vts_tab__do_GC ( Bool show_stats )
{
   UWord i, nTab, nLive, nFreed;

   /* ---------- BEGIN VTS GC ---------- */
   /* check this is actually necessary. */
   tl_assert(vts_tab_freelist == VtsID_INVALID);

   /* empty the caches for partial order checks and binary joins.  We
      could do better and prune out the entries to be deleted, but it
      ain't worth the hassle. */
   VtsID__invalidate_caches();

   /* First, make the reference counts up to date. */
   zsm_flush_cache();

   nTab = VG_(sizeXA)( vts_tab );

   if (show_stats) {
      VG_(printf)("<<GC begins at vts_tab size %lu>>\n", nTab);
      show_vts_stats("before GC");
   }

   /* Now we can inspect the entire vts_tab.  Any entries with zero
      .rc fields are now no longer in use and can be put back on the
      free list, removed from vts_set, and deleted. */
   nFreed = 0;
   for (i = 0; i < nTab; i++) {
      Bool present;
      UWord oldK = 0, oldV = 12345;
      VtsTE* te = VG_(indexXA)( vts_tab, i );
      if (te->vts == NULL) {
         tl_assert(te->rc == 0);
         continue; /* already on the free list (presumably) */
      }
      if (te->rc > 0)
         continue; /* in use */
      /* Ok, we got one we can free. */
      tl_assert(te->vts->id == i);
      /* first, remove it from vts_set. */
      present = VG_(delFromFM)( vts_set,
                                &oldK, &oldV, (UWord)te->vts );
      tl_assert(present); /* else it isn't in vts_set ?! */
      tl_assert(oldV == 0); /* no info stored in vts_set val fields */
      tl_assert(oldK == (UWord)te->vts); /* else what did delFromFM find?! */
      /* now free the VTS itself */
      VTS__delete(te->vts);
      te->vts = NULL;
      /* and finally put this entry on the free list */
      tl_assert(te->u.freelink == VtsID_INVALID); /* can't already be on it */
      add_to_free_list( i );
      nFreed++;
   }

   /* Now figure out when the next GC should be.  We'll allow the
      number of VTSs to double before GCing again.  Except of course
      that since we can't (or, at least, don't) shrink vts_tab, we
      can't set the threshold value smaller than it. */
   tl_assert(nFreed <= nTab);
   nLive = nTab - nFreed;
   tl_assert(nLive >= 0 && nLive <= nTab);
   vts_next_GC_at = 2 * nLive;
   if (vts_next_GC_at < nTab)
      vts_next_GC_at = nTab;

   if (show_stats) {
      show_vts_stats("after GC");
      VG_(printf)("<<GC ends, next gc at %ld>>\n", vts_next_GC_at);
   }

   stats__vts_tab_GC++;
   if (VG_(clo_stats)) {
      tl_assert(nTab > 0);
      VG_(message)(Vg_DebugMsg,
                   "libhb: VTS GC: #%lu  old size %lu  live %lu  (%2llu%%)\n",
                   stats__vts_tab_GC, 
                   nTab, nLive, (100ULL * (ULong)nLive) / (ULong)nTab);
   }
   /* ---------- END VTS GC ---------- */

   /* Decide whether to do VTS pruning.  We have one of three
      settings. */
   static UInt pruning_auto_ctr = 0; /* do not make non-static */

   Bool do_pruning = False;
   switch (HG_(clo_vts_pruning)) {
      case 0: /* never */
         break;
      case 1: /* auto */
         do_pruning = (++pruning_auto_ctr % 5) == 0;
         break;
      case 2: /* always */
         do_pruning = True;
         break;
      default:
         tl_assert(0);
   }

   /* The rest of this routine only handles pruning, so we can
      quit at this point if it is not to be done. */
   if (!do_pruning)
      return;
   /* No need to do pruning if no thread died since the last pruning as
      no VtsTE can be pruned. */
   if (VG_(sizeXA)( verydead_thread_table_not_pruned) == 0)
      return;

   /* ---------- BEGIN VTS PRUNING ---------- */
   /* Sort and check the very dead threads that died since the last pruning.
      Sorting is used for the check and so that we can quickly look
      up the dead-thread entries as we work through the VTSs. */
   verydead_thread_table_sort_and_check (verydead_thread_table_not_pruned);

   /* We will run through the old table, and create a new table and
      set, at the same time setting the u.remap entries in the old
      table to point to the new entries.  Then, visit every VtsID in
      the system, and replace all of them with new ones, using the
      u.remap entries in the old table.  Finally, we can delete the old
      table and set. */

   XArray* /* of VtsTE */ new_tab
      = VG_(newXA)( HG_(zalloc), "libhb.vts_tab__do_GC.new_tab",
                    HG_(free), sizeof(VtsTE) );

   /* WordFM VTS* void */
   WordFM* new_set
      = VG_(newFM)( HG_(zalloc), "libhb.vts_tab__do_GC.new_set",
                    HG_(free),
                    (Word(*)(UWord,UWord))VTS__cmp_structural );

   /* Visit each old VTS.  For each one:

      * make a pruned version

      * search new_set for the pruned version, yielding either
        Nothing (not present) or the new VtsID for it.

      * if not present, allocate a new VtsID for it, insert (pruned
        VTS, new VtsID) in the tree, and set
        remap_table[old VtsID] = new VtsID.

      * if present, set remap_table[old VtsID] = new VtsID, where
        new VtsID was determined by the tree lookup.  Then free up
        the clone.
   */

   UWord nBeforePruning = 0, nAfterPruning = 0;
   UWord nSTSsBefore = 0, nSTSsAfter = 0;
   VtsID new_VtsID_ctr = 0;

   for (i = 0; i < nTab; i++) {

      /* For each old VTS .. */
      VtsTE* old_te  = VG_(indexXA)( vts_tab, i );
      VTS*   old_vts = old_te->vts;

      /* Skip it if not in use */
      if (old_te->rc == 0) {
         tl_assert(old_vts == NULL);
         continue;
      }
      tl_assert(old_te->u.remap == VtsID_INVALID);
      tl_assert(old_vts != NULL);
      tl_assert(old_vts->id == i);
      tl_assert(old_vts->ts != NULL);

      /* It is in use. Make a pruned version. */
      nBeforePruning++;
      nSTSsBefore += old_vts->usedTS;
      VTS* new_vts = VTS__subtract("libhb.vts_tab__do_GC.new_vts",
                                   old_vts, verydead_thread_table_not_pruned);
      tl_assert(new_vts->sizeTS == new_vts->usedTS);
      tl_assert(*(ULong*)(&new_vts->ts[new_vts->usedTS])
                == 0x0ddC0ffeeBadF00dULL);

      /* Get rid of the old VTS and the tree entry.  It's a bit more
         complex to incrementally delete the VTSs now than to nuke
         them all after we're done, but the upside is that we don't
         wind up temporarily storing potentially two complete copies
         of each VTS and hence spiking memory use. */
      UWord oldK = 0, oldV = 12345;
      Bool  present = VG_(delFromFM)( vts_set,
                                      &oldK, &oldV, (UWord)old_vts );
      tl_assert(present); /* else it isn't in vts_set ?! */
      tl_assert(oldV == 0); /* no info stored in vts_set val fields */
      tl_assert(oldK == (UWord)old_vts); /* else what did delFromFM find?! */
      /* now free the VTS itself */
      VTS__delete(old_vts);
      old_te->vts = NULL;
      old_vts = NULL;

      /* NO MENTIONS of old_vts allowed beyond this point. */

      /* Ok, we have the pruned copy in new_vts.  See if a
         structurally identical version is already present in new_set.
         If so, delete the one we just made and move on; if not, add
         it. */
      VTS*  identical_version = NULL;
      UWord valW = 12345;
      if (VG_(lookupFM)(new_set, (UWord*)&identical_version, &valW,
                        (UWord)new_vts)) {
         // already have it
         tl_assert(valW == 0);
         tl_assert(identical_version != NULL);
         tl_assert(identical_version != new_vts);
         VTS__delete(new_vts);
         new_vts = identical_version;
         tl_assert(new_vts->id != VtsID_INVALID);
      } else {
         tl_assert(valW == 12345);
         tl_assert(identical_version == NULL);
         new_vts->id = new_VtsID_ctr++;
         Bool b = VG_(addToFM)(new_set, (UWord)new_vts, 0);
         tl_assert(!b);
         VtsTE new_te;
         new_te.vts      = new_vts;
         new_te.rc       = 0;
         new_te.u.freelink = VtsID_INVALID;
         Word j = VG_(addToXA)( new_tab, &new_te );
         tl_assert(j <= i);
         tl_assert(j == new_VtsID_ctr - 1);
         // stats
         nAfterPruning++;
         nSTSsAfter += new_vts->usedTS;
      }
      old_te->u.remap = new_vts->id;

   } /* for (i = 0; i < nTab; i++) */

   /* Move very dead thread from verydead_thread_table_not_pruned to
      verydead_thread_table. Sort and check verydead_thread_table
      to verify a thread was reported very dead only once. */
   {
      UWord nBT = VG_(sizeXA)( verydead_thread_table_not_pruned);

      for (i = 0; i < nBT; i++) {
         ThrID thrid = 
            *(ThrID*)VG_(indexXA)( verydead_thread_table_not_pruned, i );
         VG_(addToXA)( verydead_thread_table, &thrid );
      }
      verydead_thread_table_sort_and_check (verydead_thread_table);
      VG_(dropHeadXA) (verydead_thread_table_not_pruned, nBT);
   }

   /* At this point, we have:
      * the old VTS table, with its u.remap entries set,
        and with all .vts == NULL.
      * the old VTS tree should be empty, since it and the old VTSs
        it contained have been incrementally deleted was we worked
        through the old table.
      * the new VTS table, with all .rc == 0, all u.freelink and u.remap
        == VtsID_INVALID. 
      * the new VTS tree.
   */
   tl_assert( VG_(sizeFM)(vts_set) == 0 );

   /* Now actually apply the mapping. */
   /* Visit all the VtsIDs in the entire system.  Where do we expect
      to find them?
      (a) in shadow memory -- the LineZs and LineFs
      (b) in our collection of struct _Thrs.
      (c) in our collection of struct _SOs.
      Nowhere else, AFAICS.  Not in the zsm cache, because that just
      got invalidated.

      Using the u.remap fields in vts_tab, map each old VtsID to a new
      VtsID.  For each old VtsID, dec its rc; and for each new one,
      inc it.  This sets up the new refcounts, and it also gives a
      cheap sanity check of the old ones: all old refcounts should be
      zero after this operation.
   */

   /* Do the mappings for (a) above: iterate over the Primary shadow
      mem map (WordFM Addr SecMap*). */
   UWord secmapW = 0;
   VG_(initIterFM)( map_shmem );
   while (VG_(nextIterFM)( map_shmem, NULL, &secmapW )) {
      UWord   j;
      SecMap* sm = (SecMap*)secmapW;
      tl_assert(sm->magic == SecMap_MAGIC);
      /* Deal with the LineZs */
      for (i = 0; i < N_SECMAP_ZLINES; i++) {
         LineZ* lineZ = &sm->linesZ[i];
         if (lineZ->dict[0] != SVal_INVALID) {
            for (j = 0; j < 4; j++)
               remap_VtsIDs_in_SVal(vts_tab, new_tab, &lineZ->dict[j]);
         } else {
            LineF* lineF = SVal2Ptr (lineZ->dict[1]);
            for (j = 0; j < N_LINE_ARANGE; j++)
               remap_VtsIDs_in_SVal(vts_tab, new_tab, &lineF->w64s[j]);
         }
      }
   }
   VG_(doneIterFM)( map_shmem );

   /* Do the mappings for (b) above: visit our collection of struct
      _Thrs. */
   Thread* hgthread = get_admin_threads();
   tl_assert(hgthread);
   while (hgthread) {
      Thr* hbthr = hgthread->hbthr;
      tl_assert(hbthr);
      /* Threads that are listed in the prunable set have their viR
         and viW set to VtsID_INVALID, so we can't mess with them. */
      if (hbthr->llexit_done && hbthr->joinedwith_done) {
         tl_assert(hbthr->viR == VtsID_INVALID);
         tl_assert(hbthr->viW == VtsID_INVALID);
         hgthread = hgthread->admin;
         continue;
      }
      remap_VtsID( vts_tab, new_tab, &hbthr->viR );
      remap_VtsID( vts_tab, new_tab, &hbthr->viW );
      hgthread = hgthread->admin;
   }

   /* Do the mappings for (c) above: visit the struct _SOs. */
   SO* so = admin_SO;
   while (so) {
      if (so->viR != VtsID_INVALID)
         remap_VtsID( vts_tab, new_tab, &so->viR );
      if (so->viW != VtsID_INVALID)
         remap_VtsID( vts_tab, new_tab, &so->viW );
      so = so->admin_next;
   }

   /* So, we're nearly done (with this incredibly complex operation).
      Check the refcounts for the old VtsIDs all fell to zero, as
      expected.  Any failure is serious. */
   for (i = 0; i < nTab; i++) {
      VtsTE* te = VG_(indexXA)( vts_tab, i );
      tl_assert(te->vts == NULL);
      /* This is the assert proper.  Note we're also asserting
         zeroness for old entries which are unmapped.  That's OK. */
      tl_assert(te->rc == 0);
   }

   /* Install the new table and set. */
   VG_(deleteFM)(vts_set, NULL/*kFin*/, NULL/*vFin*/);
   vts_set = new_set;
   VG_(deleteXA)( vts_tab );
   vts_tab = new_tab;

   /* The freelist of vts_tab entries is empty now, because we've
      compacted all of the live entries at the low end of the
      table. */
   vts_tab_freelist = VtsID_INVALID;

   /* Sanity check vts_set and vts_tab. */

   /* Because all the live entries got slid down to the bottom of vts_tab: */
   tl_assert( VG_(sizeXA)( vts_tab ) == VG_(sizeFM)( vts_set ));

   /* Assert that the vts_tab and vts_set entries point at each other
      in the required way */
   UWord wordK = 0, wordV = 0;
   VG_(initIterFM)( vts_set );
   while (VG_(nextIterFM)( vts_set, &wordK, &wordV )) {
      tl_assert(wordK != 0);
      tl_assert(wordV == 0);
      VTS* vts = (VTS*)wordK;
      tl_assert(vts->id != VtsID_INVALID);
      VtsTE* te = VG_(indexXA)( vts_tab, vts->id );
      tl_assert(te->vts == vts);
   }
   VG_(doneIterFM)( vts_set );

   /* Also iterate over the table, and check each entry is
      plausible. */
   nTab = VG_(sizeXA)( vts_tab );
   for (i = 0; i < nTab; i++) {
      VtsTE* te = VG_(indexXA)( vts_tab, i );
      tl_assert(te->vts);
      tl_assert(te->vts->id == i);
      tl_assert(te->rc > 0); /* 'cos we just GC'd */
      tl_assert(te->u.freelink == VtsID_INVALID); /* in use */
      /* value of te->u.remap  not relevant */
   }

   /* And we're done.  Bwahahaha. Ha. Ha. Ha. */
   stats__vts_pruning++;
   if (VG_(clo_stats)) {
      tl_assert(nTab > 0);
      VG_(message)(
         Vg_DebugMsg,
         "libhb: VTS PR: #%lu  before %lu (avg sz %lu)  "
            "after %lu (avg sz %lu)\n",
         stats__vts_pruning,
         nBeforePruning, nSTSsBefore / (nBeforePruning ? nBeforePruning : 1),
         nAfterPruning, nSTSsAfter / (nAfterPruning ? nAfterPruning : 1)
      );
   }
   /* ---------- END VTS PRUNING ---------- */
}


/////////////////////////////////////////////////////////
//                                                     //
// Vts IDs                                             //
//                                                     //
/////////////////////////////////////////////////////////

//////////////////////////
/* A temporary, max-sized VTS which is used as a temporary (the first
   argument) in VTS__singleton, VTS__tick and VTS__join operations. */
static VTS* temp_max_sized_VTS = NULL;

//////////////////////////
static ULong stats__cmpLEQ_queries = 0;
static ULong stats__cmpLEQ_misses  = 0;
static ULong stats__join2_queries  = 0;
static ULong stats__join2_misses   = 0;

static inline UInt ROL32 ( UInt w, Int n ) {
   w = (w << n) | (w >> (32-n));
   return w;
}
static inline UInt hash_VtsIDs ( VtsID vi1, VtsID vi2, UInt nTab ) {
   UInt hash = ROL32(vi1,19) ^ ROL32(vi2,13);
   return hash % nTab;
}

#define N_CMPLEQ_CACHE 1023
static
   struct { VtsID vi1; VtsID vi2; Bool leq; }
   cmpLEQ_cache[N_CMPLEQ_CACHE];

#define N_JOIN2_CACHE 1023
static
   struct { VtsID vi1; VtsID vi2; VtsID res; }
   join2_cache[N_JOIN2_CACHE];

static void VtsID__invalidate_caches ( void ) {
   Int i;
   for (i = 0; i < N_CMPLEQ_CACHE; i++) {
      cmpLEQ_cache[i].vi1 = VtsID_INVALID;
      cmpLEQ_cache[i].vi2 = VtsID_INVALID;
      cmpLEQ_cache[i].leq = False;
   }
   for (i = 0; i < N_JOIN2_CACHE; i++) {
     join2_cache[i].vi1 = VtsID_INVALID;
     join2_cache[i].vi2 = VtsID_INVALID;
     join2_cache[i].res = VtsID_INVALID;
   }
}
//////////////////////////

//static Bool VtsID__is_valid ( VtsID vi ) {
//   VtsTE* ve;
//   if (vi >= (VtsID)VG_(sizeXA)( vts_tab ))
//      return False;
//   ve = VG_(indexXA)( vts_tab, vi );
//   if (!ve->vts)
//      return False;
//   tl_assert(ve->vts->id == vi);
//   return True;
//}

static VTS* VtsID__to_VTS ( VtsID vi ) {
   VtsTE* te = VG_(indexXA)( vts_tab, vi );
   tl_assert(te->vts);
   return te->vts;
}

static void VtsID__pp ( VtsID vi ) {
   VTS* vts = VtsID__to_VTS(vi);
   VTS__show( vts );
}

/* compute partial ordering relation of vi1 and vi2. */
__attribute__((noinline))
static Bool VtsID__cmpLEQ_WRK ( VtsID vi1, VtsID vi2 ) {
   UInt hash;
   Bool leq;
   VTS  *v1, *v2;
   //if (vi1 == vi2) return True;
   tl_assert(vi1 != vi2);
   ////++
   stats__cmpLEQ_queries++;
   hash = hash_VtsIDs(vi1, vi2, N_CMPLEQ_CACHE);
   if (cmpLEQ_cache[hash].vi1 == vi1
       && cmpLEQ_cache[hash].vi2 == vi2)
      return cmpLEQ_cache[hash].leq;
   stats__cmpLEQ_misses++;
   ////--
   v1  = VtsID__to_VTS(vi1);
   v2  = VtsID__to_VTS(vi2);
   leq = VTS__cmpLEQ( v1, v2 ) == 0;
   ////++
   cmpLEQ_cache[hash].vi1 = vi1;
   cmpLEQ_cache[hash].vi2 = vi2;
   cmpLEQ_cache[hash].leq = leq;
   ////--
   return leq;
}
static inline Bool VtsID__cmpLEQ ( VtsID vi1, VtsID vi2 ) {
   return LIKELY(vi1 == vi2)  ? True  : VtsID__cmpLEQ_WRK(vi1, vi2);
}

/* compute binary join */
__attribute__((noinline))
static VtsID VtsID__join2_WRK ( VtsID vi1, VtsID vi2 ) {
   UInt  hash;
   VtsID res;
   VTS   *vts1, *vts2;
   //if (vi1 == vi2) return vi1;
   tl_assert(vi1 != vi2);
   ////++
   stats__join2_queries++;
   hash = hash_VtsIDs(vi1, vi2, N_JOIN2_CACHE);
   if (join2_cache[hash].vi1 == vi1
       && join2_cache[hash].vi2 == vi2)
      return join2_cache[hash].res;
   stats__join2_misses++;
   ////--
   vts1 = VtsID__to_VTS(vi1);
   vts2 = VtsID__to_VTS(vi2);
   temp_max_sized_VTS->usedTS = 0;
   VTS__join(temp_max_sized_VTS, vts1,vts2);
   res = vts_tab__find__or__clone_and_add(temp_max_sized_VTS);
   ////++
   join2_cache[hash].vi1 = vi1;
   join2_cache[hash].vi2 = vi2;
   join2_cache[hash].res = res;
   ////--
   return res;
}
static inline VtsID VtsID__join2 ( VtsID vi1, VtsID vi2 ) {
   return LIKELY(vi1 == vi2)  ? vi1  : VtsID__join2_WRK(vi1, vi2);
}

/* create a singleton VTS, namely [thr:1] */
static VtsID VtsID__mk_Singleton ( Thr* thr, ULong tym ) {
   temp_max_sized_VTS->usedTS = 0;
   VTS__singleton(temp_max_sized_VTS, thr,tym);
   return vts_tab__find__or__clone_and_add(temp_max_sized_VTS);
}

/* tick operation, creates value 1 if specified index is absent */
static VtsID VtsID__tick ( VtsID vi, Thr* idx ) {
   VTS* vts = VtsID__to_VTS(vi);
   temp_max_sized_VTS->usedTS = 0;
   VTS__tick(temp_max_sized_VTS, idx,vts);
   return vts_tab__find__or__clone_and_add(temp_max_sized_VTS);
}

/* index into a VTS (only for assertions) */
static ULong VtsID__indexAt ( VtsID vi, Thr* idx ) {
   VTS* vts = VtsID__to_VTS(vi);
   return VTS__indexAt_SLOW( vts, idx );
}

/* Assuming that !cmpLEQ(vi1, vi2), find the index of the first (or
   any, really) element in vi1 which is pointwise greater-than the
   corresponding element in vi2.  If no such element exists, return
   NULL.  This needs to be fairly quick since it is called every time
   a race is detected. */
static Thr* VtsID__findFirst_notLEQ ( VtsID vi1, VtsID vi2 )
{
   VTS  *vts1, *vts2;
   Thr*  diffthr;
   ThrID diffthrid;
   tl_assert(vi1 != vi2);
   vts1 = VtsID__to_VTS(vi1);
   vts2 = VtsID__to_VTS(vi2);
   tl_assert(vts1 != vts2);
   diffthrid = VTS__cmpLEQ(vts1, vts2);
   diffthr = Thr__from_ThrID(diffthrid);
   tl_assert(diffthr); /* else they are LEQ ! */
   return diffthr;
}


/////////////////////////////////////////////////////////
//                                                     //
// Filters                                             //
//                                                     //
/////////////////////////////////////////////////////////

/* Forget everything we know -- clear the filter and let everything
   through.  This needs to be as fast as possible, since it is called
   every time the running thread changes, and every time a thread's
   vector clocks change, which can be quite frequent.  The obvious
   fast way to do this is simply to stuff in tags which we know are
   not going to match anything, since they're not aligned to the start
   of a line. */
static void Filter__clear ( Filter* fi, const HChar* who )
{
   UWord i;
   if (0) VG_(printf)("  Filter__clear(%p, %s)\n", fi, who);
   for (i = 0; i < FI_NUM_LINES; i += 8) {
      fi->tags[i+0] = 1; /* impossible value -- cannot match */
      fi->tags[i+1] = 1;
      fi->tags[i+2] = 1;
      fi->tags[i+3] = 1;
      fi->tags[i+4] = 1;
      fi->tags[i+5] = 1;
      fi->tags[i+6] = 1;
      fi->tags[i+7] = 1;
   }
   tl_assert(i == FI_NUM_LINES);
}

/* Clearing an arbitrary range in the filter.  Unfortunately
   we have to do this due to core-supplied new/die-mem events. */

static void Filter__clear_1byte ( Filter* fi, Addr a )
{
   Addr    atag   = FI_GET_TAG(a);     /* tag of 'a' */
   UWord   lineno = FI_GET_LINENO(a);  /* lineno for 'a' */
   FiLine* line   = &fi->lines[lineno];
   UWord   loff   = (a - atag) / 8;
   UShort  mask   = 0x3 << (2 * (a & 7));
   /* mask is C000, 3000, 0C00, 0300, 00C0, 0030, 000C or 0003 */
   if (LIKELY( fi->tags[lineno] == atag )) {
      /* hit.  clear the bits. */
      UShort  u16  = line->u16s[loff];
      line->u16s[loff] = u16 & ~mask; /* clear them */
   } else {
      /* miss.  The filter doesn't hold this address, so ignore. */
   }
}

static void Filter__clear_8bytes_aligned ( Filter* fi, Addr a )
{
   Addr    atag   = FI_GET_TAG(a);     /* tag of 'a' */
   UWord   lineno = FI_GET_LINENO(a);  /* lineno for 'a' */
   FiLine* line   = &fi->lines[lineno];
   UWord   loff   = (a - atag) / 8;
   if (LIKELY( fi->tags[lineno] == atag )) {
      line->u16s[loff] = 0;
   } else {
    /* miss.  The filter doesn't hold this address, so ignore. */
   }
}

/* Only used to verify the fast Filter__clear_range */
__attribute__((unused))
static void Filter__clear_range_SLOW ( Filter* fi, Addr a, UWord len )
{
   tl_assert (CHECK_ZSM);

   /* slowly do part preceding 8-alignment */
   while (UNLIKELY(!VG_IS_8_ALIGNED(a)) && LIKELY(len > 0)) {
      Filter__clear_1byte( fi, a );
      a++;
      len--;
   }
   /* vector loop */
   while (len >= 8) {
      Filter__clear_8bytes_aligned( fi, a );
      a += 8;
      len -= 8;
   }
   /* slowly do tail */
   while (UNLIKELY(len > 0)) {
      Filter__clear_1byte( fi, a );
      a++;
      len--;
   }
}

static void Filter__clear_range ( Filter* fi, Addr a, UWord len )
{
#  if CHECK_ZSM > 0
   /* We check the below more complex algorithm with the simple one.
      This check is very expensive : we do first the slow way on a
      copy of the data, then do it the fast way. On RETURN, we check
      the two values are equal. */
   Filter fi_check = *fi;
   Filter__clear_range_SLOW(&fi_check, a, len);
#  define RETURN goto check_and_return
#  else
#  define RETURN return
#  endif

   Addr    begtag = FI_GET_TAG(a);       /* tag of range begin */

   Addr    end = a + len - 1;
   Addr    endtag = FI_GET_TAG(end); /* tag of range end. */

   UWord rlen = len; /* remaining length to clear */

   Addr    c = a; /* Current position we are clearing. */
   UWord   clineno = FI_GET_LINENO(c); /* Current lineno we are clearing */
   FiLine* cline; /* Current line we are clearing */
   UWord   cloff; /* Current offset in line we are clearing, when clearing
                     partial lines. */

   UShort u16;

   STATIC_ASSERT (FI_LINE_SZB == 32);
   // Below assumes filter lines are 32 bytes

   if (LIKELY(fi->tags[clineno] == begtag)) {
      /* LIKELY for the heavy caller VG_(unknown_SP_update). */
      /* First filter line matches begtag.
         If c is not at the filter line begin, the below will clear
         the filter line bytes starting from c. */
      cline = &fi->lines[clineno];
      cloff = (c - begtag) / 8;

      /* First the byte(s) needed to reach 8-alignment */
      if (UNLIKELY(!VG_IS_8_ALIGNED(c))) {
         /* hiB is the nr of bytes (higher addresses) from c to reach
            8-aligment. */
         UWord hiB = 8 - (c & 7);
         /* Compute 2-bit/byte mask representing hiB bytes [c..c+hiB[
            mask is  C000 , F000, FC00, FF00, FFC0, FFF0 or FFFC for the byte
            range    7..7   6..7  5..7  4..7  3..7  2..7    1..7 */
         UShort mask = 0xFFFF << (16 - 2*hiB);

         u16  = cline->u16s[cloff];
         if (LIKELY(rlen >= hiB)) {
            cline->u16s[cloff] = u16 & ~mask; /* clear all hiB from c */
            rlen -= hiB;
            c += hiB;
            cloff += 1;
         } else {
            /* Only have the bits for rlen bytes bytes. */
            mask = mask & ~(0xFFFF << (16 - 2*(hiB-rlen)));
            cline->u16s[cloff] = u16 & ~mask; /* clear rlen bytes from c. */
            RETURN;  // We have cleared all what we can.
         }
      }
      /* c is now 8 aligned. Clear by 8 aligned bytes, 
         till c is filter-line aligned */
      while (!VG_IS_32_ALIGNED(c) && rlen >= 8) {
         cline->u16s[cloff] = 0;
         c += 8;
         rlen -= 8;
         cloff += 1;
      }
   } else {
      c = begtag + FI_LINE_SZB;
      if (c > end)
         RETURN;   // We have cleared all what we can.
      rlen -= c - a;
   }
   // We have changed c, so re-establish clineno.
   clineno = FI_GET_LINENO(c);

   if (rlen >= FI_LINE_SZB) {
      /* Here, c is filter line-aligned. Clear all full lines that
         overlap with the range starting at c, made of a full lines */
      UWord nfull = rlen / FI_LINE_SZB;
      UWord full_len = nfull * FI_LINE_SZB;
      rlen -= full_len;
      if (nfull > FI_NUM_LINES)
         nfull = FI_NUM_LINES; // no need to check several times the same entry.

      for (UWord n = 0; n < nfull; n++) {
         if (UNLIKELY(address_in_range(fi->tags[clineno], c, full_len))) {
            cline = &fi->lines[clineno];
            cline->u16s[0] = 0;
            cline->u16s[1] = 0;
            cline->u16s[2] = 0;
            cline->u16s[3] = 0;
            STATIC_ASSERT (4 == sizeof(cline->u16s)/sizeof(cline->u16s[0]));
         }
         clineno++;
         if (UNLIKELY(clineno == FI_NUM_LINES))
            clineno = 0;
      }

      c += full_len;
      clineno = FI_GET_LINENO(c);
   }

   if (CHECK_ZSM) {
      tl_assert(VG_IS_8_ALIGNED(c));
      tl_assert(clineno == FI_GET_LINENO(c));
   }

   /* Do the last filter line, if it was not cleared as a full filter line */
   if (UNLIKELY(rlen > 0) && fi->tags[clineno] == endtag) {
      cline = &fi->lines[clineno];
      cloff = (c - endtag) / 8;
      if (CHECK_ZSM) tl_assert(FI_GET_TAG(c) == endtag);

      /* c is 8 aligned. Clear by 8 aligned bytes, till we have less than
         8 bytes. */
      while (rlen >= 8) {
         cline->u16s[cloff] = 0;
         c += 8;
         rlen -= 8;
         cloff += 1;
      }
      /* Then the remaining byte(s) */
      if (rlen > 0) {
         /* nr of bytes from c to reach end. */
         UWord loB = rlen;
         /* Compute mask representing loB bytes [c..c+loB[ :
            mask is 0003, 000F, 003F, 00FF, 03FF, 0FFF or 3FFF */
         UShort mask = 0xFFFF >> (16 - 2*loB);

         u16  = cline->u16s[cloff];
         cline->u16s[cloff] = u16 & ~mask; /* clear all loB from c */
      }
   }

#  if CHECK_ZSM > 0
   check_and_return:
   tl_assert (VG_(memcmp)(&fi_check, fi, sizeof(fi_check)) == 0);
#  endif
#  undef RETURN
}

/* ------ Read handlers for the filter. ------ */

static inline Bool Filter__ok_to_skip_crd64 ( Filter* fi, Addr a )
{
   if (UNLIKELY( !VG_IS_8_ALIGNED(a) ))
      return False;
   { 
     Addr    atag   = FI_GET_TAG(a);     /* tag of 'a' */
     UWord   lineno = FI_GET_LINENO(a);  /* lineno for 'a' */
     FiLine* line   = &fi->lines[lineno];
     UWord   loff   = (a - atag) / 8;
     UShort  mask   = 0xAAAA;
     if (LIKELY( fi->tags[lineno] == atag )) {
        /* hit.  check line and update. */
        UShort u16  = line->u16s[loff];
        Bool   ok   = (u16 & mask) == mask; /* all R bits set? */
        line->u16s[loff] = u16 | mask; /* set them */
        return ok;
     } else {
        /* miss.  nuke existing line and re-use it. */
        UWord i;
        fi->tags[lineno] = atag;
        for (i = 0; i < FI_LINE_SZB / 8; i++)
           line->u16s[i] = 0;
        line->u16s[loff] = mask;
        return False;
     }
   }
}

static inline Bool Filter__ok_to_skip_crd32 ( Filter* fi, Addr a )
{
   if (UNLIKELY( !VG_IS_4_ALIGNED(a) ))
      return False;
   {
     Addr    atag   = FI_GET_TAG(a);     /* tag of 'a' */
     UWord   lineno = FI_GET_LINENO(a);  /* lineno for 'a' */
     FiLine* line   = &fi->lines[lineno];
     UWord   loff   = (a - atag) / 8;
     UShort  mask   = 0xAA << (2 * (a & 4)); /* 0xAA00 or 0x00AA */
     if (LIKELY( fi->tags[lineno] == atag )) {
        /* hit.  check line and update. */
        UShort  u16  = line->u16s[loff];
        Bool    ok   = (u16 & mask) == mask; /* 4 x R bits set? */
        line->u16s[loff] = u16 | mask; /* set them */
        return ok;
     } else {
        /* miss.  nuke existing line and re-use it. */
        UWord   i;
        fi->tags[lineno] = atag;
        for (i = 0; i < FI_LINE_SZB / 8; i++)
           line->u16s[i] = 0;
        line->u16s[loff] = mask;
        return False;
     }
   }
}

static inline Bool Filter__ok_to_skip_crd16 ( Filter* fi, Addr a )
{
   if (UNLIKELY( !VG_IS_2_ALIGNED(a) ))
      return False;
   {
     Addr    atag   = FI_GET_TAG(a);     /* tag of 'a' */
     UWord   lineno = FI_GET_LINENO(a);  /* lineno for 'a' */
     FiLine* line   = &fi->lines[lineno];
     UWord   loff   = (a - atag) / 8;
     UShort  mask   = 0xA << (2 * (a & 6));
     /* mask is A000, 0A00, 00A0 or 000A */
     if (LIKELY( fi->tags[lineno] == atag )) {
        /* hit.  check line and update. */
        UShort  u16  = line->u16s[loff];
        Bool    ok   = (u16 & mask) == mask; /* 2 x R bits set? */
        line->u16s[loff] = u16 | mask; /* set them */
        return ok;
     } else {
        /* miss.  nuke existing line and re-use it. */
        UWord   i;
        fi->tags[lineno] = atag;
        for (i = 0; i < FI_LINE_SZB / 8; i++)
           line->u16s[i] = 0;
        line->u16s[loff] = mask;
        return False;
     }
   }
}

static inline Bool Filter__ok_to_skip_crd08 ( Filter* fi, Addr a )
{
   {
     Addr    atag   = FI_GET_TAG(a);     /* tag of 'a' */
     UWord   lineno = FI_GET_LINENO(a);  /* lineno for 'a' */
     FiLine* line   = &fi->lines[lineno];
     UWord   loff   = (a - atag) / 8;
     UShort  mask   = 0x2 << (2 * (a & 7));
     /* mask is 8000, 2000, 0800, 0200, 0080, 0020, 0008 or 0002 */
     if (LIKELY( fi->tags[lineno] == atag )) {
        /* hit.  check line and update. */
        UShort  u16  = line->u16s[loff];
        Bool    ok   = (u16 & mask) == mask; /* 1 x R bits set? */
        line->u16s[loff] = u16 | mask; /* set them */
        return ok;
     } else {
        /* miss.  nuke existing line and re-use it. */
        UWord   i;
        fi->tags[lineno] = atag;
        for (i = 0; i < FI_LINE_SZB / 8; i++)
           line->u16s[i] = 0;
        line->u16s[loff] = mask;
        return False;
     }
   }
}


/* ------ Write handlers for the filter. ------ */

static inline Bool Filter__ok_to_skip_cwr64 ( Filter* fi, Addr a )
{
   if (UNLIKELY( !VG_IS_8_ALIGNED(a) ))
      return False;
   { 
     Addr    atag   = FI_GET_TAG(a);     /* tag of 'a' */
     UWord   lineno = FI_GET_LINENO(a);  /* lineno for 'a' */
     FiLine* line   = &fi->lines[lineno];
     UWord   loff   = (a - atag) / 8;
     UShort  mask   = 0xFFFF;
     if (LIKELY( fi->tags[lineno] == atag )) {
        /* hit.  check line and update. */
        UShort u16  = line->u16s[loff];
        Bool   ok   = (u16 & mask) == mask; /* all R & W bits set? */
        line->u16s[loff] = u16 | mask; /* set them */
        return ok;
     } else {
        /* miss.  nuke existing line and re-use it. */
        UWord i;
        fi->tags[lineno] = atag;
        for (i = 0; i < FI_LINE_SZB / 8; i++)
           line->u16s[i] = 0;
        line->u16s[loff] = mask;
        return False;
     }
   }
}

static inline Bool Filter__ok_to_skip_cwr32 ( Filter* fi, Addr a )
{
   if (UNLIKELY( !VG_IS_4_ALIGNED(a) ))
      return False;
   {
     Addr    atag   = FI_GET_TAG(a);     /* tag of 'a' */
     UWord   lineno = FI_GET_LINENO(a);  /* lineno for 'a' */
     FiLine* line   = &fi->lines[lineno];
     UWord   loff   = (a - atag) / 8;
     UShort  mask   = 0xFF << (2 * (a & 4)); /* 0xFF00 or 0x00FF */
     if (LIKELY( fi->tags[lineno] == atag )) {
        /* hit.  check line and update. */
        UShort  u16  = line->u16s[loff];
        Bool    ok   = (u16 & mask) == mask; /* 4 x R & W bits set? */
        line->u16s[loff] = u16 | mask; /* set them */
        return ok;
     } else {
        /* miss.  nuke existing line and re-use it. */
        UWord   i;
        fi->tags[lineno] = atag;
        for (i = 0; i < FI_LINE_SZB / 8; i++)
           line->u16s[i] = 0;
        line->u16s[loff] = mask;
        return False;
     }
   }
}

static inline Bool Filter__ok_to_skip_cwr16 ( Filter* fi, Addr a )
{
   if (UNLIKELY( !VG_IS_2_ALIGNED(a) ))
      return False;
   {
     Addr    atag   = FI_GET_TAG(a);     /* tag of 'a' */
     UWord   lineno = FI_GET_LINENO(a);  /* lineno for 'a' */
     FiLine* line   = &fi->lines[lineno];
     UWord   loff   = (a - atag) / 8;
     UShort  mask   = 0xF << (2 * (a & 6));
     /* mask is F000, 0F00, 00F0 or 000F */
     if (LIKELY( fi->tags[lineno] == atag )) {
        /* hit.  check line and update. */
        UShort  u16  = line->u16s[loff];
        Bool    ok   = (u16 & mask) == mask; /* 2 x R & W bits set? */
        line->u16s[loff] = u16 | mask; /* set them */
        return ok;
     } else {
        /* miss.  nuke existing line and re-use it. */
        UWord   i;
        fi->tags[lineno] = atag;
        for (i = 0; i < FI_LINE_SZB / 8; i++)
           line->u16s[i] = 0;
        line->u16s[loff] = mask;
        return False;
     }
   }
}

static inline Bool Filter__ok_to_skip_cwr08 ( Filter* fi, Addr a )
{
   {
     Addr    atag   = FI_GET_TAG(a);     /* tag of 'a' */
     UWord   lineno = FI_GET_LINENO(a);  /* lineno for 'a' */
     FiLine* line   = &fi->lines[lineno];
     UWord   loff   = (a - atag) / 8;
     UShort  mask   = 0x3 << (2 * (a & 7));
     /* mask is C000, 3000, 0C00, 0300, 00C0, 0030, 000C or 0003 */
     if (LIKELY( fi->tags[lineno] == atag )) {
        /* hit.  check line and update. */
        UShort  u16  = line->u16s[loff];
        Bool    ok   = (u16 & mask) == mask; /* 1 x R bits set? */
        line->u16s[loff] = u16 | mask; /* set them */
        return ok;
     } else {
        /* miss.  nuke existing line and re-use it. */
        UWord   i;
        fi->tags[lineno] = atag;
        for (i = 0; i < FI_LINE_SZB / 8; i++)
           line->u16s[i] = 0;
        line->u16s[loff] = mask;
        return False;
     }
   }
}


/////////////////////////////////////////////////////////
//                                                     //
// Threads                                             //
//                                                     //
/////////////////////////////////////////////////////////

/* Maps ThrID values to their Thr*s (which contain ThrID values that
   should point back to the relevant slot in the array.  Lowest
   numbered slot (0) is for thrid = 1024, (1) is for 1025, etc. */
static XArray* /* of Thr* */ thrid_to_thr_map = NULL;

/* And a counter to dole out ThrID values.  For rationale/background,
   see comments on definition of ScalarTS (far) above. */
static ThrID thrid_counter = 1024; /* runs up to ThrID_MAX_VALID */

static ThrID Thr__to_ThrID ( Thr* thr ) {
   return thr->thrid;
}
static Thr* Thr__from_ThrID ( UInt thrid ) {
   Thr* thr = *(Thr**)VG_(indexXA)( thrid_to_thr_map, thrid - 1024 );
   tl_assert(thr->thrid == thrid);
   return thr;
}

/* True if the cached rcec for thr is valid and can be used to build the
   current stack trace just by changing the last frame to the current IP. */
static inline Bool cached_rcec_valid(Thr *thr)
{
   UWord cached_stackvalid = VG_(get_SP_s1) (thr->hgthread->coretid);
   return cached_stackvalid != 0;
}
/* Set the validity of the cached rcec of thr. */
static inline void set_cached_rcec_validity(Thr *thr, Bool valid)
{
   VG_(set_SP_s1) (thr->hgthread->coretid, valid);
}

static Thr* Thr__new ( void )
{
   Thr* thr = HG_(zalloc)
      ( "libhb.Thr__new.1", 
        sizeof(Thr) + HG_(clo_history_backtrace_size) * sizeof(UWord));
   // We need to add the size of the frames in the cached_rcec (last member of
   // _Thr).

   thr->viR = VtsID_INVALID;
   thr->viW = VtsID_INVALID;
   thr->llexit_done = False;
   thr->joinedwith_done = False;
   thr->filter = HG_(zalloc)( "libhb.Thr__new.2", sizeof(Filter) );
   if (HG_(clo_history_level) == 1)
      thr->local_Kws_n_stacks
         = VG_(newXA)( HG_(zalloc),
                       "libhb.Thr__new.3 (local_Kws_and_stacks)",
                       HG_(free), sizeof(ULong_n_EC) );
   /* Make an 'empty' cached rcec in thr. */
   thr->cached_rcec.magic = RCEC_MAGIC;
   thr->cached_rcec.rc = 0;
   thr->cached_rcec.rcX = 0;
   thr->cached_rcec.next = NULL;

   /* Add this Thr* <-> ThrID binding to the mapping, and
      cross-check */
   if (!thrid_to_thr_map) {
      thrid_to_thr_map = VG_(newXA)( HG_(zalloc), "libhb.Thr__new.4",
                                     HG_(free), sizeof(Thr*) );
   }

   if (thrid_counter >= ThrID_MAX_VALID) {
      /* We're hosed.  We have to stop. */
      scalarts_limitations_fail_NORETURN( True/*due_to_nThrs*/ );
   }

   thr->thrid = thrid_counter++;
   Word ix = VG_(addToXA)( thrid_to_thr_map, &thr );
   tl_assert(ix + 1024 == thr->thrid);

   return thr;
}

static void note_local_Kw_n_stack_for ( Thr* thr )
{
   Word       nPresent;
   ULong_n_EC pair;
   tl_assert(thr);

   // We only collect this info at history level 1 (approx)
   if (HG_(clo_history_level) != 1) 
      return;

   /* This is the scalar Kw for thr. */
   pair.ull = VtsID__indexAt( thr->viW, thr );
   pair.ec  = main_get_EC( thr );
   tl_assert(pair.ec);
   tl_assert(thr->local_Kws_n_stacks);

   /* check that we're not adding duplicates */
   nPresent = VG_(sizeXA)( thr->local_Kws_n_stacks );

   /* Throw away old stacks, if necessary.  We can't accumulate stuff
      indefinitely. */
   if (nPresent >= N_KWs_N_STACKs_PER_THREAD) {
      VG_(dropHeadXA)( thr->local_Kws_n_stacks, nPresent / 2 );
      nPresent = VG_(sizeXA)( thr->local_Kws_n_stacks );
      if (0)
         VG_(printf)("LOCAL Kw: thr %p,  Kw %llu,  ec %p (!!! gc !!!)\n",
                     thr, pair.ull, pair.ec );
   }

   if (nPresent > 0) {
      ULong_n_EC* prevPair
         = (ULong_n_EC*)VG_(indexXA)( thr->local_Kws_n_stacks, nPresent-1 );
      tl_assert( prevPair->ull <= pair.ull );
   }

   if (nPresent == 0)
      pair.ec = NULL;

   VG_(addToXA)( thr->local_Kws_n_stacks, &pair );

   if (0)
      VG_(printf)("LOCAL Kw: thr %p,  Kw %llu,  ec %p\n",
                  thr, pair.ull, pair.ec );
   if (0)
      VG_(pp_ExeContext)(pair.ec);
}

static Int cmp__ULong_n_EC__by_ULong ( const ULong_n_EC* pair1,
                                       const ULong_n_EC* pair2 )
{
   if (pair1->ull < pair2->ull) return -1;
   if (pair1->ull > pair2->ull) return 1;
   return 0;
}


/////////////////////////////////////////////////////////
//                                                     //
// Shadow Values                                       //
//                                                     //
/////////////////////////////////////////////////////////

// type SVal, SVal_INVALID and SVal_NOACCESS are defined by
// hb_zsm.h.  We have to do everything else here.

/* SVal is 64 bit unsigned int.

      <---------30--------->    <---------30--------->
   00 X-----Rmin-VtsID-----X 00 X-----Wmin-VtsID-----X   C(Rmin,Wmin)
   10 X--------------------X XX X--------------------X   A: SVal_NOACCESS
   11 0--------------------0 00 0--------------------0   A: SVal_INVALID

*/
#define SVAL_TAGMASK (3ULL << 62)

static inline Bool SVal__isC ( SVal s ) {
   return (0ULL << 62) == (s & SVAL_TAGMASK);
}
static inline SVal SVal__mkC ( VtsID rmini, VtsID wmini ) {
   //tl_assert(VtsID__is_valid(rmini));
   //tl_assert(VtsID__is_valid(wmini));
   return (((ULong)rmini) << 32) | ((ULong)wmini);
}
static inline VtsID SVal__unC_Rmin ( SVal s ) {
   tl_assert(SVal__isC(s));
   return (VtsID)(s >> 32);
}
static inline VtsID SVal__unC_Wmin ( SVal s ) {
   tl_assert(SVal__isC(s));
   return (VtsID)(s & 0xFFFFFFFFULL);
}

static inline Bool SVal__isA ( SVal s ) {
   return (2ULL << 62) == (s & SVAL_TAGMASK);
}
__attribute__((unused))
static inline SVal SVal__mkA ( void ) {
   return 2ULL << 62;
}

/* Direct callback from lib_zsm. */
static inline void SVal__rcinc ( SVal s ) {
   if (SVal__isC(s)) {
      VtsID__rcinc( SVal__unC_Rmin(s) );
      VtsID__rcinc( SVal__unC_Wmin(s) );
   }
}

/* Direct callback from lib_zsm. */
static inline void SVal__rcdec ( SVal s ) {
   if (SVal__isC(s)) {
      VtsID__rcdec( SVal__unC_Rmin(s) );
      VtsID__rcdec( SVal__unC_Wmin(s) );
   }
}

static inline void *SVal2Ptr (SVal s)
{
   return (void*)(UWord)s;
}

static inline SVal Ptr2SVal (void* ptr)
{
   return (SVal)(UWord)ptr;
}



/////////////////////////////////////////////////////////
//                                                     //
// Change-event map2                                   //
//                                                     //
/////////////////////////////////////////////////////////

/* This is in two parts:

   1. A hash table of RCECs.  This is a set of reference-counted stack
      traces.  When the reference count of a stack trace becomes zero,
      it is removed from the set and freed up.  The intent is to have
      a set of stack traces which can be referred to from (2), but to
      only represent each one once.  The set is indexed/searched by
      ordering on the stack trace vectors.

   2. A Hash table of OldRefs.  These store information about each old
      ref that we need to record.  Hash table key is the address of the
      location for which the information is recorded.  For LRU
      purposes, each OldRef in the hash table is also on a doubly
      linked list maintaining the order in which the OldRef were most
      recently accessed.
      Each OldRef also maintains the stamp at which it was last accessed.
      With these stamps, we can quickly check which of 2 OldRef is the
      'newest', without having to scan the full list of LRU OldRef.

      The important part of an OldRef is, however, its acc component.
      This binds a TSW triple (thread, size, R/W) to an RCEC.

      We allocate a maximum of VG_(clo_conflict_cache_size) OldRef.
      Then we do exact LRU discarding.  For each discarded OldRef we must
      of course decrement the reference count on the RCEC it
      refers to, in order that entries from (1) eventually get
      discarded too.
*/

static UWord stats__evm__lookup_found = 0;
static UWord stats__evm__lookup_notfound = 0;

static UWord stats__ctxt_eq_tsw_eq_rcec = 0;
static UWord stats__ctxt_eq_tsw_neq_rcec = 0;
static UWord stats__ctxt_neq_tsw_neq_rcec = 0;
static UWord stats__ctxt_rcdec_calls = 0;
static UWord stats__ctxt_rcec_gc_discards = 0;

static UWord stats__ctxt_tab_curr = 0;
static UWord stats__ctxt_tab_max  = 0;

static UWord stats__ctxt_tab_qs   = 0;
static UWord stats__ctxt_tab_cmps = 0;


///////////////////////////////////////////////////////
//// Part (1): A hash table of RCECs
///

//#define N_RCEC_TAB 98317 /* prime */
#define N_RCEC_TAB 196613 /* prime */

//////////// BEGIN RCEC pool allocator
static PoolAlloc* rcec_pool_allocator;
static RCEC* alloc_RCEC ( void ) {
   return VG_(allocEltPA) ( rcec_pool_allocator );
}

static void free_RCEC ( RCEC* rcec ) {
   tl_assert(rcec->magic == RCEC_MAGIC);
   VG_(freeEltPA)( rcec_pool_allocator, rcec );
}
//////////// END RCEC pool allocator

static RCEC** contextTab = NULL; /* hash table of RCEC*s */

/* Count of allocated RCEC having ref count > 0 */
static UWord RCEC_referenced = 0;

/* True if the frames of ec1 and ec2 are different. */
static Bool RCEC__differs_by_frames ( RCEC* ec1, RCEC* ec2 ) {
   Word i;
   if (CHECK_CEM) {
      tl_assert(ec1 && ec1->magic == RCEC_MAGIC);
      tl_assert(ec2 && ec2->magic == RCEC_MAGIC);
   }
   if (ec1->frames_hash != ec2->frames_hash) return True;
   for (i = 0; i < HG_(clo_history_backtrace_size); i++) {
      if (ec1->frames[i] != ec2->frames[i]) return True;
   }
   return False;
}

/* Dec the ref of this RCEC. */
static void ctxt__rcdec ( RCEC* ec )
{
   stats__ctxt_rcdec_calls++;
   if (CHECK_CEM)
      tl_assert(ec && ec->magic == RCEC_MAGIC);
   tl_assert(ec->rc > 0);
   ec->rc--;
   if (ec->rc == 0) 
      RCEC_referenced--;
}

static void ctxt__rcinc ( RCEC* ec )
{
   if (CHECK_CEM)
      tl_assert(ec && ec->magic == RCEC_MAGIC);
   if (ec->rc == 0) 
      RCEC_referenced++;
   ec->rc++;
}


/* Find 'ec' in the RCEC list whose head pointer lives at 'headp' and
   move it one step closer to the front of the list, so as to make
   subsequent searches for it cheaper. */
static void move_RCEC_one_step_forward ( RCEC** headp, RCEC* ec )
{
   RCEC *ec0, *ec1, *ec2;
   if (ec == *headp)
      tl_assert(0); /* already at head of list */
   tl_assert(ec != NULL);
   ec0 = *headp;
   ec1 = NULL;
   ec2 = NULL;
   while (True) {
      if (ec0 == NULL || ec0 == ec) break;
      ec2 = ec1;
      ec1 = ec0;
      ec0 = ec0->next;
   }
   tl_assert(ec0 == ec);
   if (ec0 != NULL && ec1 != NULL && ec2 != NULL) {
      RCEC* tmp;
      /* ec0 points to ec, ec1 to its predecessor, and ec2 to ec1's
         predecessor.  Swap ec0 and ec1, that is, move ec0 one step
         closer to the start of the list. */
      tl_assert(ec2->next == ec1);
      tl_assert(ec1->next == ec0);
      tmp = ec0->next;
      ec2->next = ec0;
      ec0->next = ec1;
      ec1->next = tmp;
   }
   else
   if (ec0 != NULL && ec1 != NULL && ec2 == NULL) {
      /* it's second in the list. */
      tl_assert(*headp == ec1);
      tl_assert(ec1->next == ec0);
      ec1->next = ec0->next;
      ec0->next = ec1;
      *headp = ec0;
   }
}


/* Find the given RCEC in the tree, and return a pointer to it.  Or,
   if not present, add the given one to the tree (by making a copy of
   it, so the caller can immediately deallocate the original) and
   return a pointer to the copy.  The caller can safely have 'example'
   on its stack, since we will always return a pointer to a copy of
   it, not to the original.  Note that the inserted node will have .rc
   of zero and so the caller must immediately increment it. */
__attribute__((noinline))
static RCEC* ctxt__find_or_add ( RCEC* example )
{
   UWord hent;
   RCEC* copy;

   if (CHECK_CEM) {
      /* Note that the single caller of ctxt__find_or_add always provides
         &thr->cached_rcec as argument. The sanity of thr->cached_rcec is always
         checked with a thread terminates. */
      tl_assert(example && example->magic == RCEC_MAGIC);
      tl_assert(example->rc == 0);
   }

   /* Search the hash table to see if we already have it. */
   stats__ctxt_tab_qs++;
   hent = example->frames_hash % N_RCEC_TAB;
   copy = contextTab[hent];
   while (1) {
      if (!copy) break;
      if (CHECK_CEM)
         tl_assert(copy->magic == RCEC_MAGIC);
      stats__ctxt_tab_cmps++;
      if (!RCEC__differs_by_frames(copy, example)) break;
      copy = copy->next;
   }

   if (copy) {
      tl_assert(copy != example);
      /* optimisation: if it's not at the head of its list, move 1
         step fwds, to make future searches cheaper */
      if (copy != contextTab[hent]) {
         move_RCEC_one_step_forward( &contextTab[hent], copy );
      }
   } else {
      copy = alloc_RCEC();
      tl_assert(copy != example);
      *copy = *example;
      for (Word i = 0; i < HG_(clo_history_backtrace_size); i++)
         copy->frames[i] = example->frames[i];
      copy->next = contextTab[hent];
      contextTab[hent] = copy;
      stats__ctxt_tab_curr++;
      if (stats__ctxt_tab_curr > stats__ctxt_tab_max)
         stats__ctxt_tab_max = stats__ctxt_tab_curr;
   }
   return copy;
}

static inline UWord ROLW ( UWord w, Int n )
{
   Int bpw = 8 * sizeof(UWord);
   w = (w << n) | (w >> (bpw-n));
   return w;
}

static UWord stats__cached_rcec_identical = 0;
static UWord stats__cached_rcec_updated = 0;
static UWord stats__cached_rcec_fresh = 0;
static UWord stats__cached_rcec_diff = 0;
static UWord stats__cached_rcec_diff_known_reason = 0;

/* Check if the cached rcec in thr corresponds to the current
   stacktrace of the thread. Returns True if ok, False otherwise.
   This is just used for debugging the cached rcec logic, activated
   using --hg-sanity-flags=xx1xxx i.e. SCE_ACCESS flag.
   When this flag is activated, a call to this function will happen each time
   a stack trace is needed for a memory access. */
__attribute__((noinline))
static Bool check_cached_rcec_ok (Thr* thr, Addr previous_frame0)
{
   Bool  ok = True;
   UInt  i;
   UWord frames[HG_(clo_history_backtrace_size)];
   UWord sps[HG_(clo_history_backtrace_size)];
   UWord fps[HG_(clo_history_backtrace_size)];
   const DiEpoch cur_ep = VG_(current_DiEpoch)();

   for (i = 0; i < HG_(clo_history_backtrace_size); i++)
      frames[i] = sps[i] = fps[i] = 0;
   VG_(get_StackTrace)( thr->hgthread->coretid, &frames[0], 
                        HG_(clo_history_backtrace_size),
                        &sps[0], &fps[0], 0);
   for (i = 0; i < HG_(clo_history_backtrace_size); i++) {
      if ( thr->cached_rcec.frames[i] != frames[i] ) {
         /* There are a bunch of "normal" reasons for which a stack
            derived from the cached rcec differs from frames. */
         const HChar *reason = NULL;

         /* Old linkers (e.g. RHEL5) gave no cfi unwind information in the PLT
            section (fix was added in binutils around June 2011).
            Without PLT unwind info, stacktrace in the PLT section are
            missing an entry. E.g. the cached stacktrace is:
              ==4463==    at 0x2035C0: ___tls_get_addr (dl-tls.c:753)
              ==4463==    by 0x33B7F9: __libc_thread_freeres
                                                (in /lib/libc-2.11.2.so)
              ==4463==    by 0x39BA4F: start_thread (pthread_create.c:307)
              ==4463==    by 0x2F107D: clone (clone.S:130)
           while the 'check stacktrace' is
              ==4463==    at 0x2035C0: ___tls_get_addr (dl-tls.c:753)
              ==4463==    by 0x33B82D: strerror_thread_freeres
                                                (in /lib/libc-2.11.2.so)
              ==4463==    by 0x33B7F9: __libc_thread_freeres
                                                (in /lib/libc-2.11.2.so)
              ==4463==    by 0x39BA4F: start_thread (pthread_create.c:307)
              ==4463==    by 0x2F107D: clone (clone.S:130)
           No cheap/easy way to detect or fix that. */

         /* It seems that sometimes, the CFI unwind info looks wrong
            for a 'ret' instruction. E.g. here is the unwind info
            for a 'retq' on gcc20 (amd64, Debian 7)
                [0x4e3ddfe .. 0x4e3ddfe]: let cfa=oldSP+48 in RA=*(cfa+-8)
                                                      SP=cfa+0 BP=*(cfa+-24)
            This unwind info looks doubtful, as the RA should be at oldSP.
            No easy way to detect this problem.
            This gives a difference between cached rcec and
            current stack trace: the cached rcec is correct. */

         /* When returning from main, unwind info becomes erratic.
            So, by default, only report errors for main and above,
            unless asked to show below main. */
         if (reason == NULL) {
            UInt fr_main;
            Vg_FnNameKind fr_kind = Vg_FnNameNormal;
            for (fr_main = 0; 
                 fr_main < HG_(clo_history_backtrace_size); 
                 fr_main++) {
               fr_kind = VG_(get_fnname_kind_from_IP)
                                (cur_ep, frames[fr_main]);
               if (fr_kind == Vg_FnNameMain || fr_kind == Vg_FnNameBelowMain)
                  break;
            }
            UInt kh_main;
            Vg_FnNameKind kh_kind = Vg_FnNameNormal;
            for (kh_main = 0; 
                 kh_main < HG_(clo_history_backtrace_size); 
                 kh_main++) {
               kh_kind = VG_(get_fnname_kind_from_IP)
                                (cur_ep, thr->cached_rcec.frames[kh_main]);
               if (kh_kind == Vg_FnNameMain || kh_kind == Vg_FnNameBelowMain)
                  break;
            }
            if (kh_main == fr_main
                && kh_kind == fr_kind
                && (kh_main < i || (kh_main == i
                                    && kh_kind == Vg_FnNameBelowMain))) {
               // found main or below main before the difference
               reason = "Below main";
            }
         }

         /* We have places where the stack is missing some internal
            pthread functions. For such stacktraces, GDB reports only
            one function, telling:
               #0  0xf7fa81fe in _L_unlock_669 ()
                              from /lib/i386-linux-gnu/libpthread.so.0
               Backtrace stopped: previous frame identical to
                                            this frame (corrupt stack?)

            This is when sps and fps are identical.
            The cached stack trace is then
               ==3336==    at 0x40641FE: _L_unlock_669
                                              (pthread_mutex_unlock.c:310)
               ==3336==    by 0x40302BE: pthread_mutex_unlock
                                              (hg_intercepts.c:710)
               ==3336==    by 0x80486AF: main (cond_timedwait_test.c:14)
           while the 'check stacktrace' is
               ==3336==    at 0x40641FE: _L_unlock_669
                                              (pthread_mutex_unlock.c:310)
               ==3336==    by 0x4064206: _L_unlock_669
                                              (pthread_mutex_unlock.c:310)
               ==3336==    by 0x4064132: __pthread_mutex_unlock_usercnt
                                              (pthread_mutex_unlock.c:57)
               ==3336==    by 0x40302BE: pthread_mutex_unlock
                                               (hg_intercepts.c:710)
               ==3336==    by 0x80486AF: main (cond_timedwait_test.c:14) */
         if (reason == NULL) {
            if ((i > 0
                      && sps[i] == sps[i-1] && fps[i] == fps[i-1])
                || (i < HG_(clo_history_backtrace_size)-1
                      && sps[i] == sps[i+1] && fps[i] == fps[i+1])) {
               reason = "previous||next frame: identical sp and fp";
            }
         }
         if (reason == NULL) {
            if ((i > 0
                      && fps[i] == fps[i-1])
                || (i < HG_(clo_history_backtrace_size)-1
                      && fps[i] == fps[i+1])) {
               reason = "previous||next frame: identical fp";
            }
         }

         /* When we have a read or write 'in the middle of a push instruction',
            then the normal backtrace is not very good, while the helgrind
            stacktrace is better, as it undoes the not yet fully finished
            push instruction before getting the stacktrace. */
         if (reason == NULL && thr->hgthread->first_sp_delta != 0) {
            reason = "fixupSP probably needed for check stacktrace";
         }

         /* Unwinding becomes hectic when running the exit handlers.
            None of GDB, cached stacktrace and check stacktrace corresponds.
            So, if we find __run_exit_handlers, ignore the difference. */
         if (reason == NULL) {
            const HChar *fnname;
            for (UInt f = 0; f < HG_(clo_history_backtrace_size); f++) {
               if (VG_(get_fnname)( cur_ep, frames[f], &fnname)
                   && VG_(strcmp) ("__run_exit_handlers", fnname) == 0) {
                  reason = "exit handlers";
                  break;
               }
            }
         }

         // Show what we have found for this difference
         if (reason == NULL) {
            ok = False;
            stats__cached_rcec_diff++;
         } else {
            ok = True;
            stats__cached_rcec_diff_known_reason++;
         }
         if (!ok || VG_(clo_verbosity) > 2) {
            Bool save_show_below_main = VG_(clo_show_below_main);
            VG_(clo_show_below_main) = True;
            /* The below error msg reports an unexpected diff in 'frame %d'.
               The (maybe wrong) pc found in the cached stacktrace is
               'cached_pc %p' while an unwind gives the (maybe wrong)
               'check_pc %p'.
               After, 'previous_frame0 %p' tells where the cached stacktrace
               was taken.
               This is then followed by the full resulting cache stack trace
               and the full stack trace found doing unwind.
               Such a diff can have various origins:
                 * a bug in the unwinder, when the cached stack trace was taken
                   at 'previous_frame0'
                 * a bug in the unwinder, when the check stack trace was taken
                   (i.e. at current pc).
                 * a missing 'invalidate cache stack trace' somewhere in the
                   instructions between 'previous_frame0' and current_pc.
               To investigate the last case, typically, disass the range of
               instructions where an invalidate cached stack might miss. */
            VG_(printf)("%s diff tid %u frame %u "
                        "cached_pc %p check_pc %p\n",
                        reason ? reason : "unexpected",
                        thr->hgthread->coretid,
                        i,
                        (void*)thr->cached_rcec.frames[i],
                        (void*)frames[i]);
            VG_(printf)("cached stack trace previous_frame0 %p\n",
                        (void*)previous_frame0);
            VG_(pp_StackTrace)(cur_ep, &previous_frame0, 1);
            VG_(printf)("resulting cached stack trace:\n");
            VG_(pp_StackTrace)(cur_ep, thr->cached_rcec.frames, 
                               HG_(clo_history_backtrace_size));
            VG_(printf)("check stack trace:\n");
            VG_(pp_StackTrace)(cur_ep, frames, HG_(clo_history_backtrace_size));

            VG_(show_sched_status) (False,  // host_stacktrace
                                    False,  // stack_usage
                                    False); // exited_threads
            if (VG_(clo_vgdb_error) == 1234567890) // HACK TO ALLOW TO DEBUG
               VG_(gdbserver) ( thr->hgthread->coretid );
            VG_(clo_show_below_main) = save_show_below_main;
         }
         break; // Stop giving more errors for this stacktrace.
      }
   }
   return ok;
}

__attribute__((noinline))
static RCEC* get_RCEC ( Thr* thr )
{
   UInt  i;
   UWord hash;
   Addr  previous_frame0 = 0; // Assignment needed to silence gcc
   RCEC  *res;
   const Bool thr_cached_rcec_valid = cached_rcec_valid(thr);
   const Addr cur_ip = VG_(get_IP)(thr->hgthread->coretid);

   if (DEBUG_CACHED_RCEC)
      VG_(printf)("get rcec tid %u at IP %p SP %p"
                  " first_sp_delta %ld cached valid %d\n",
                  thr->hgthread->coretid,
                  (void*)cur_ip,
                  (void*)VG_(get_SP)(thr->hgthread->coretid),
                  thr->hgthread->first_sp_delta, thr_cached_rcec_valid);

   /* If we have a valid cached rcec, derive the new rcec from the cached one
      and update the cached one.
      Otherwise, compute a fresh rcec. */

   if (thr_cached_rcec_valid) {
      /* Update the stacktrace of the cached rcec with the current IP */
      previous_frame0 = thr->cached_rcec.frames[0];
      thr->cached_rcec.frames[0] = cur_ip;

#     if defined(VGP_x86_linux)
      // See m_stacktrace.c kludge
      extern Addr VG_(client__dl_sysinfo_int80);
      /// #include pub_core_clientstate needed for the above ????
      /// or move the above into a pub_tool_??? tool_stacktrace.h maybe ????
      if (VG_(client__dl_sysinfo_int80) != 0 /* we know its address */
          && cur_ip >= VG_(client__dl_sysinfo_int80)
          && cur_ip < VG_(client__dl_sysinfo_int80)+3
          ) {
         thr->cached_rcec.frames[0]
            = (ULong) *(Addr*)(UWord)VG_(get_SP)(thr->hgthread->coretid);
      }
#     endif

      if (previous_frame0 == thr->cached_rcec.frames[0])
         stats__cached_rcec_identical++;
      else
         stats__cached_rcec_updated++;
   } else {
      /* Compute a fresh stacktrace. */
      main_get_stacktrace( thr, &thr->cached_rcec.frames[0], 
                           HG_(clo_history_backtrace_size) );
      if (DEBUG_CACHED_RCEC) {
         Bool save_show_below_main = VG_(clo_show_below_main);
         VG_(clo_show_below_main) = True;
         VG_(printf)("caching stack trace:\n");
         VG_(pp_StackTrace)(VG_(current_DiEpoch)(),
                            &thr->cached_rcec.frames[0], 
                            HG_(clo_history_backtrace_size));
         VG_(clo_show_below_main) = save_show_below_main;
      }
      stats__cached_rcec_fresh++;
   }

   hash = 0;
   for (i = 0; i < HG_(clo_history_backtrace_size); i++) {
      hash ^= thr->cached_rcec.frames[i];
      hash = ROLW(hash, 19);
   }
   thr->cached_rcec.frames_hash = hash;
   res = ctxt__find_or_add( &thr->cached_rcec );

   if (UNLIKELY(HG_(clo_sanity_flags) & SCE_ACCESS)
       && thr_cached_rcec_valid) {
      /* In case the cached and check differ, invalidate the cached rcec.
         We have less duplicated diffs reported afterwards. */
      if (!check_cached_rcec_ok (thr, previous_frame0))
         set_cached_rcec_validity(thr, False);
   } else {
      if (HG_(clo_delta_stacktrace) && !thr_cached_rcec_valid)
            set_cached_rcec_validity(thr, True);
   }

   return res;
}

///////////////////////////////////////////////////////
//// Part (2):
///  A hashtable guest-addr -> OldRef, that refers to (1)
///  Note: we use the guest address as key. This means that the entries
///  for multiple threads accessing the same address will land in the same
///  bucket. It might be nice to have a better distribution of the
///  OldRef in the hashtable by using ask key the guestaddress ^ tsw.
///  The problem is that when a race is reported on a ga, we need to retrieve
///  efficiently the accesses to ga by other threads, only using the ga.
///  Measurements on firefox have shown that the chain length is reasonable.

/* Records an access: a thread, a context (size & writeness) and the
   number of held locks. The size (1,2,4,8) is stored as is in szB.
   Note that szB uses more bits than needed to store a size up to 8.
   This allows to use a TSW as a fully initialised UInt e.g. in
   cmp_oldref_tsw. If needed, a more compact representation of szB
   can be done (e.g. use only 4 bits, or use only 2 bits and encode the
   size (1,2,4,8) as 00 = 1, 01 = 2, 10 = 4, 11 = 8. */
typedef 
   struct {
      UInt      thrid  : SCALARTS_N_THRBITS;
      UInt      szB    : 32 - SCALARTS_N_THRBITS - 1;
      UInt      isW    : 1;
   } TSW; // Thread+Size+Writeness
typedef
   struct {
      TSW       tsw;
      WordSetID locksHeldW;
      RCEC*     rcec;
   }
   Thr_n_RCEC;

typedef
   struct OldRef {
      struct OldRef *ht_next; // to link hash table nodes together.
      UWord  ga; // hash_table key, == address for which we record an access.
      struct OldRef *prev; // to refs older than this one
      struct OldRef *next; // to refs newer that this one
      UWord stamp; // allows to order (by time of access) 2 OldRef
      Thr_n_RCEC acc;
   }
   OldRef;

/* Returns the or->tsw as an UInt */
static inline UInt oldref_tsw (const OldRef* or)
{
   return *(const UInt*)(&or->acc.tsw);
}

/* Compare the tsw component for 2 OldRef.
   Used for OldRef hashtable (which already verifies equality of the
   'key' part. */
static Word cmp_oldref_tsw (const void* node1, const void* node2 )
{
   const UInt tsw1 = oldref_tsw(node1);
   const UInt tsw2 = oldref_tsw(node2);

   if (tsw1 < tsw2) return -1;
   if (tsw1 > tsw2) return  1;
   return 0;
}


//////////// BEGIN OldRef pool allocator
static PoolAlloc* oldref_pool_allocator;
// Note: We only allocate elements in this pool allocator, we never free them.
// We stop allocating elements at VG_(clo_conflict_cache_size).
//////////// END OldRef pool allocator

static OldRef mru; 
static OldRef lru; 
// A double linked list, chaining all OldREf in a mru/lru order.
// mru/lru are sentinel nodes.
// Whenever an oldref is re-used, its position is changed as the most recently
// used (i.e. pointed to by mru.prev).
// When a new oldref is needed, it is allocated from the pool
//  if we have not yet reached --conflict-cache-size.
// Otherwise, if all oldref have already been allocated,
// the least recently used (i.e. pointed to by lru.next) is re-used.
// When an OldRef is used, it is moved as the most recently used entry
// (i.e. pointed to by mru.prev).

// Removes r from the double linked list
// Note: we do not need to test for special cases such as
// NULL next or prev pointers, because we have sentinel nodes
// at both sides of the list. So, a node is always forward and
// backward linked.
static inline void OldRef_unchain(OldRef *r)
{
   r->next->prev = r->prev;
   r->prev->next = r->next;
}

// Insert new as the newest OldRef
// Similarly to OldRef_unchain, no need to test for NULL
// pointers, as e.g. mru.prev is always guaranteed to point
// to a non NULL node (lru when the list is empty).
static inline void OldRef_newest(OldRef *new)
{
   new->next = &mru;
   new->prev = mru.prev;
   mru.prev = new;
   new->prev->next = new;
}


static VgHashTable* oldrefHT    = NULL; /* Hash table* OldRef* */
static UWord     oldrefHTN    = 0;    /* # elems in oldrefHT */
/* Note: the nr of ref in the oldrefHT will always be equal to
   the nr of elements that were allocated from the OldRef pool allocator
   as we never free an OldRef : we just re-use them. */


/* allocates a new OldRef or re-use the lru one if all allowed OldRef
   have already been allocated. */
static OldRef* alloc_or_reuse_OldRef ( void )
{
   if (oldrefHTN < HG_(clo_conflict_cache_size)) {
      oldrefHTN++;
      return VG_(allocEltPA) ( oldref_pool_allocator );
   } else {
      OldRef *oldref_ht;
      OldRef *oldref = lru.next;

      OldRef_unchain(oldref);
      oldref_ht = VG_(HT_gen_remove) (oldrefHT, oldref, cmp_oldref_tsw);
      tl_assert (oldref == oldref_ht);
      ctxt__rcdec( oldref->acc.rcec );
      return oldref;
   }
}


inline static UInt min_UInt ( UInt a, UInt b ) {
   return a < b ? a : b;
}

/* Compare the intervals [a1,a1+n1) and [a2,a2+n2).  Return -1 if the
   first interval is lower, 1 if the first interval is higher, and 0
   if there is any overlap.  Redundant paranoia with casting is there
   following what looked distinctly like a bug in gcc-4.1.2, in which
   some of the comparisons were done signedly instead of
   unsignedly. */
/* Copied from exp-ptrcheck/sg_main.c */
static inline Word cmp_nonempty_intervals ( Addr a1, SizeT n1,
                                            Addr a2, SizeT n2 ) {
   UWord a1w = (UWord)a1;
   UWord n1w = (UWord)n1;
   UWord a2w = (UWord)a2;
   UWord n2w = (UWord)n2;
   tl_assert(n1w > 0 && n2w > 0);
   if (a1w + n1w <= a2w) return -1L;
   if (a2w + n2w <= a1w) return 1L;
   return 0;
}

static UWord event_map_stamp = 0; // Used to stamp each OldRef when touched.

static void event_map_bind ( Addr a, SizeT szB, Bool isW, Thr* thr )
{
   OldRef  example;
   OldRef* ref;
   RCEC*   rcec;

   tl_assert(thr);
   ThrID thrid = thr->thrid;
   tl_assert(thrid != 0); /* zero is used to denote an empty slot. */

   WordSetID locksHeldW = thr->hgthread->locksetW;

   rcec = get_RCEC( thr );

   /* Look in the oldrefHT to see if we already have a record for this
      address/thr/sz/isW. */
   example.ga = a;
   example.acc.tsw = (TSW) {.thrid = thrid,
                            .szB = szB,
                            .isW = (UInt)(isW & 1)};
   ref = VG_(HT_gen_lookup) (oldrefHT, &example, cmp_oldref_tsw);

   if (ref) {
      /* We already have a record for this address and this (thrid, R/W,
         size) triple. */
      tl_assert (ref->ga == a);

      /* thread 'thr' has an entry.  Update its RCEC, if it differs. */
      if (rcec == ref->acc.rcec)
         stats__ctxt_eq_tsw_eq_rcec++;
      else {
         stats__ctxt_eq_tsw_neq_rcec++;
         ctxt__rcdec( ref->acc.rcec );
         ctxt__rcinc(rcec);
         ref->acc.rcec       = rcec;
      }
      tl_assert(ref->acc.tsw.thrid == thrid);
      /* Update the stamp, RCEC and the W-held lockset. */
      ref->stamp = event_map_stamp;
      ref->acc.locksHeldW = locksHeldW;

      OldRef_unchain(ref);
      OldRef_newest(ref);

   } else {
      tl_assert (szB == 4 || szB == 8 ||szB == 1 || szB == 2);
      // We only need to check the size the first time we insert a ref.
      // Check for most frequent cases first
      // Note: we could support a szB up to 1 << (32 - SCALARTS_N_THRBITS - 1)

      /* We don't have a record for this address+triple.  Create a new one. */
      stats__ctxt_neq_tsw_neq_rcec++;
      ref = alloc_or_reuse_OldRef();
      ref->ga = a;
      ref->acc.tsw = (TSW) {.thrid  = thrid,
                            .szB    = szB,
                            .isW    = (UInt)(isW & 1)};
      ref->stamp = event_map_stamp;
      ref->acc.locksHeldW = locksHeldW;
      ref->acc.rcec       = rcec;
      ctxt__rcinc(rcec);

      VG_(HT_add_node) ( oldrefHT, ref );
      OldRef_newest (ref);
   }
   event_map_stamp++;
}


/* Extract info from the conflicting-access machinery.
   Returns the most recent conflicting access with thr/[a, a+szB[/isW. */
Bool libhb_event_map_lookup ( /*OUT*/ExeContext** resEC,
                              /*OUT*/Thr**        resThr,
                              /*OUT*/SizeT*       resSzB,
                              /*OUT*/Bool*        resIsW,
                              /*OUT*/WordSetID*   locksHeldW,
                              Thr* thr, Addr a, SizeT szB, Bool isW )
{
   Word    i, j;
   OldRef *ref = NULL;
   SizeT  ref_szB = 0;

   OldRef *cand_ref;
   SizeT  cand_ref_szB;
   Addr   cand_a;

   Addr toCheck[15];
   Int  nToCheck = 0;

   tl_assert(thr);
   tl_assert(szB == 8 || szB == 4 || szB == 2 || szB == 1);

   ThrID thrid = thr->thrid;

   toCheck[nToCheck++] = a;
   for (i = -7; i < (Word)szB; i++) {
      if (i != 0)
         toCheck[nToCheck++] = a + i;
   }
   tl_assert(nToCheck <= 15);

   /* Now see if we can find a suitable matching event for
      any of the addresses in toCheck[0 .. nToCheck-1]. */
   for (j = 0; j < nToCheck; j++) {

      cand_a = toCheck[j];
      //      VG_(printf)("test %ld %p\n", j, cand_a);

      /* Find the first HT element for this address.
         We might have several of these. They will be linked via ht_next.
         We however need to check various elements as the list contains
         all elements that map to the same bucket. */
      for (cand_ref = VG_(HT_lookup)( oldrefHT, cand_a ); 
           cand_ref; cand_ref = cand_ref->ht_next) {
         if (cand_ref->ga != cand_a)
            /* OldRef for another address in this HT bucket. Ignore. */
            continue;

         if (cand_ref->acc.tsw.thrid == thrid)
            /* This is an access by the same thread, but we're only
               interested in accesses from other threads.  Ignore. */
            continue;

         if ((!cand_ref->acc.tsw.isW) && (!isW))
            /* We don't want to report a read racing against another
               read; that's stupid.  So in this case move on. */
            continue;

         cand_ref_szB        = cand_ref->acc.tsw.szB;
         if (cmp_nonempty_intervals(a, szB, cand_a, cand_ref_szB) != 0)
            /* No overlap with the access we're asking about.  Ignore. */
            continue;

         /* We have a match. Keep this match if it is newer than
            the previous match. Note that stamp are Unsigned Words, and
            for long running applications, event_map_stamp might have cycled.
            So, 'roll' each stamp using event_map_stamp to have the
            stamps in the good order, in case event_map_stamp recycled. */
         if (!ref 
             || (ref->stamp - event_map_stamp) 
                   < (cand_ref->stamp - event_map_stamp)) {
            ref = cand_ref;
            ref_szB = cand_ref_szB;
         }
      }

      if (ref) {
         /* return with success */
         Int n, maxNFrames;
         RCEC*     ref_rcec = ref->acc.rcec;
         tl_assert(ref->acc.tsw.thrid);
         tl_assert(ref_rcec);
         tl_assert(ref_rcec->magic == RCEC_MAGIC);
         tl_assert(ref_szB >= 1);
         /* Count how many non-zero frames we have. */
         maxNFrames = min_UInt(HG_(clo_history_backtrace_size), 
                               VG_(clo_backtrace_size));
         for (n = 0; n < maxNFrames; n++) {
            if (0 == ref_rcec->frames[n]) break;
         }
         *resEC      = VG_(make_ExeContext_from_StackTrace)(&ref_rcec->frames[0],
                                                            n);
         *resThr     = Thr__from_ThrID(ref->acc.tsw.thrid);
         *resSzB     = ref_szB;
         *resIsW     = ref->acc.tsw.isW;
         *locksHeldW = ref->acc.locksHeldW;
         stats__evm__lookup_found++;
         return True;
      }

      /* consider next address in toCheck[] */
   } /* for (j = 0; j < nToCheck; j++) */

   /* really didn't find anything. */
   stats__evm__lookup_notfound++;
   return False;
}


void libhb_event_map_access_history ( Addr a, SizeT szB, Access_t fn )
{
   OldRef *ref = lru.next;
   SizeT ref_szB;
   Int n;

   while (ref != &mru) {
      ref_szB = ref->acc.tsw.szB;
      if (cmp_nonempty_intervals(a, szB, ref->ga, ref_szB) == 0) {
         RCEC* ref_rcec = ref->acc.rcec;
         for (n = 0; n < HG_(clo_history_backtrace_size); n++) {
            if (0 == ref_rcec->frames[n]) {
               break;
            }
         }
         (*fn)(&ref_rcec->frames[0], n,
               Thr__from_ThrID(ref->acc.tsw.thrid),
               ref->ga,
               ref_szB,
               ref->acc.tsw.isW,
               ref->acc.locksHeldW);
      }
      tl_assert (ref->next == &mru
                 || ((ref->stamp - event_map_stamp)
                        < ref->next->stamp - event_map_stamp));
      ref = ref->next;
   }
}

static void event_map_init ( void )
{
   Word i;

   /* Context (RCEC) pool allocator */
   rcec_pool_allocator 
      = VG_(newPA) (
         sizeof(RCEC) + 2 * HG_(clo_history_backtrace_size) * sizeof(UWord),
         1000 /* RCECs per pool */,
         HG_(zalloc),
         "libhb.event_map_init.1 (RCEC pools)",
         HG_(free)
         );

   /* Context table */
   tl_assert(!contextTab);
   contextTab = HG_(zalloc)( "libhb.event_map_init.2 (context table)",
                             N_RCEC_TAB * sizeof(RCEC*) );
   for (i = 0; i < N_RCEC_TAB; i++)
      contextTab[i] = NULL;

   /* Oldref pool allocator */
   oldref_pool_allocator = VG_(newPA)(
                               sizeof(OldRef),
                               1000 /* OldRefs per pool */,
                               HG_(zalloc),
                               "libhb.event_map_init.3 (OldRef pools)",
                               HG_(free)
                            );

   /* Oldref hashtable */
   tl_assert(!oldrefHT);
   oldrefHT = VG_(HT_construct) ("libhb.event_map_init.4 (oldref hashtable)");

   oldrefHTN = 0;
   mru.prev = &lru;
   mru.next = NULL;
   lru.prev = NULL;
   lru.next = &mru;
   mru.acc = (Thr_n_RCEC) {.tsw = {.thrid = 0, 
                                   .szB = 0, 
                                   .isW = 0},
                           .locksHeldW = 0, 
                           .rcec = NULL};
   lru.acc = mru.acc;
}

static void event_map__check_reference_counts ( void )
{
   RCEC*   rcec;
   OldRef* oldref;
   Word    i;
   UWord   nEnts = 0;

   /* Set the 'check' reference counts to zero.  Also, optionally
      check that the real reference counts are non-zero.  We allow
      these to fall to zero before a GC, but the GC must get rid of
      all those that are zero, hence none should be zero after a
      GC. */
   for (i = 0; i < N_RCEC_TAB; i++) {
      for (rcec = contextTab[i]; rcec; rcec = rcec->next) {
         nEnts++;
         tl_assert(rcec);
         tl_assert(rcec->magic == RCEC_MAGIC);
         rcec->rcX = 0;
      }
   }

   /* check that the stats are sane */
   tl_assert(nEnts == stats__ctxt_tab_curr);
   tl_assert(stats__ctxt_tab_curr <= stats__ctxt_tab_max);

   /* visit all the referencing points, inc check ref counts */
   VG_(HT_ResetIter)( oldrefHT );
   oldref = VG_(HT_Next)( oldrefHT );
   while (oldref) {
      tl_assert (oldref->acc.tsw.thrid);
      tl_assert (oldref->acc.rcec);
      tl_assert (oldref->acc.rcec->magic == RCEC_MAGIC);
      oldref->acc.rcec->rcX++;
      oldref = VG_(HT_Next)( oldrefHT );
   }

   /* compare check ref counts with actual */
   for (i = 0; i < N_RCEC_TAB; i++) {
      for (rcec = contextTab[i]; rcec; rcec = rcec->next) {
         tl_assert(rcec->rc == rcec->rcX);
      }
   }
}

__attribute__((noinline))
static void do_RCEC_GC ( void )
{
   UInt i;

   if (VG_(clo_stats)) {
      static UInt ctr = 1;
      VG_(message)(Vg_DebugMsg,
                  "libhb: RCEC GC: #%u  %lu slots,"
                   " %lu cur ents(ref'd %lu),"
                   " %lu max ents\n",
                   ctr++,
                   (UWord)N_RCEC_TAB,
                   stats__ctxt_tab_curr, RCEC_referenced,
                   stats__ctxt_tab_max );
   }
   tl_assert (stats__ctxt_tab_curr > RCEC_referenced);

   /* Throw away all RCECs with zero reference counts */
   for (i = 0; i < N_RCEC_TAB; i++) {
      RCEC** pp = &contextTab[i];
      RCEC*  p  = *pp;
      while (p) {
         if (p->rc == 0) {
            *pp = p->next;
            free_RCEC(p);
            p = *pp;
            tl_assert(stats__ctxt_tab_curr > 0);
            stats__ctxt_rcec_gc_discards++;
            stats__ctxt_tab_curr--;
         } else {
            pp = &p->next;
            p = p->next;
         }
      }
   }

   tl_assert (stats__ctxt_tab_curr == RCEC_referenced);
}

/////////////////////////////////////////////////////////
//                                                     //
// Core MSM                                            //
//                                                     //
/////////////////////////////////////////////////////////

/* Logic in msmcread/msmcwrite updated/verified after re-analysis, 19
   Nov 08, and again after [...],
   June 09. */

static ULong stats__msmcread         = 0;
static ULong stats__msmcread_change  = 0;
static ULong stats__msmcwrite        = 0;
static ULong stats__msmcwrite_change = 0;

/* Some notes on the H1 history mechanism:

   Transition rules are:

   read_{Kr,Kw}(Cr,Cw)  = (Cr,           Cr `join` Kw)
   write_{Kr,Kw}(Cr,Cw) = (Cr `join` Kw, Cr `join` Kw)

   After any access by a thread T to a location L, L's constraint pair
   (Cr,Cw) has Cw[T] == T's Kw[T], that is, == T's scalar W-clock.

   After a race by thread T conflicting with some previous access by
   some other thread U, for a location with constraint (before
   processing the later access) (Cr,Cw), then Cw[U] is the segment in
   which the previously access lies.

   Hence in record_race_info, we pass in Cfailed and Kfailed, which
   are compared so as to find out which thread(s) this access
   conflicts with.  Once that is established, we also require the
   pre-update Cw for the location, so we can index into it for those
   threads, to get the scalar clock values for the point at which the
   former accesses were made.  (In fact we only bother to do any of
   this for an arbitrarily chosen one of the conflicting threads, as
   that's simpler, it avoids flooding the user with vast amounts of
   mostly useless information, and because the program is wrong if it
   contains any races at all -- so we don't really need to show all
   conflicting access pairs initially, so long as we only show none if
   none exist).

   ---

   That requires the auxiliary proof that 

      (Cr `join` Kw)[T] == Kw[T]

   Why should that be true?  Because for any thread T, Kw[T] >= the
   scalar clock value for T known by any other thread.  In other
   words, because T's value for its own scalar clock is at least as up
   to date as the value for it known by any other thread (that is true
   for both the R- and W- scalar clocks).  Hence no other thread will
   be able to feed in a value for that element (indirectly via a
   constraint) which will exceed Kw[T], and hence the join cannot
   cause that particular element to advance.
*/

__attribute__((noinline))
static void record_race_info ( Thr* acc_thr, 
                               Addr acc_addr, SizeT szB, Bool isWrite,
                               VtsID Cfailed,
                               VtsID Kfailed,
                               VtsID Cw )
{
   /* Call here to report a race.  We just hand it onwards to
      HG_(record_error_Race).  If that in turn discovers that the
      error is going to be collected, then, at history_level 2, that
      queries the conflicting-event map.  The alternative would be to
      query it right here.  But that causes a lot of pointless queries
      for errors which will shortly be discarded as duplicates, and
      can become a performance overhead; so we defer the query until
      we know the error is not a duplicate. */

   /* Stacks for the bounds of the (or one of the) conflicting
      segment(s).  These are only set at history_level 1. */
   ExeContext* hist1_seg_start = NULL;
   ExeContext* hist1_seg_end   = NULL;
   Thread*     hist1_conf_thr  = NULL;

   tl_assert(acc_thr);
   tl_assert(acc_thr->hgthread);
   tl_assert(acc_thr->hgthread->hbthr == acc_thr);
   tl_assert(HG_(clo_history_level) >= 0 && HG_(clo_history_level) <= 2);

   if (HG_(clo_history_level) == 1) {
      Bool found;
      Word firstIx, lastIx;
      ULong_n_EC key;

      /* At history_level 1, we must round up the relevant stack-pair
         for the conflicting segment right now.  This is because
         deferring it is complex; we can't (easily) put Kfailed and
         Cfailed into the XError and wait for later without
         getting tied up in difficulties with VtsID reference
         counting.  So just do it now. */
      Thr*  confThr;
      ULong confTym = 0;
      /* Which thread are we in conflict with?  There may be more than
         one, in which case VtsID__findFirst_notLEQ selects one arbitrarily
         (in fact it's the one with the lowest Thr* value). */
      confThr = VtsID__findFirst_notLEQ( Cfailed, Kfailed );
      /* This must exist!  since if it was NULL then there's no
         conflict (semantics of return value of
         VtsID__findFirst_notLEQ), and msmc{read,write}, which has
         called us, just checked exactly this -- that there was in
         fact a race. */
      tl_assert(confThr);

      /* Get the scalar clock value that the conflicting thread
         introduced into the constraint.  A careful examination of the
         base machine rules shows that this must be the same as the
         conflicting thread's scalar clock when it created this
         constraint.  Hence we know the scalar clock of the
         conflicting thread when the conflicting access was made. */
      confTym = VtsID__indexAt( Cfailed, confThr );

      /* Using this scalar clock, index into the conflicting thread's
         collection of stack traces made each time its vector clock
         (hence its scalar clock) changed.  This gives the stack
         traces at the start and end of the conflicting segment (well,
         as per comment just above, of one of the conflicting
         segments, if there are more than one). */
      key.ull = confTym;
      key.ec  = NULL;
      /* tl_assert(confThr); -- asserted just above */
      tl_assert(confThr->local_Kws_n_stacks);
      firstIx = lastIx = 0;
      found = VG_(lookupXA_UNSAFE)(
                 confThr->local_Kws_n_stacks,
                 &key, &firstIx, &lastIx,
                 (XACmpFn_t)cmp__ULong_n_EC__by_ULong
              );
      if (0) VG_(printf)("record_race_info %u %u %u  confThr %p "
                         "confTym %llu found %d (%ld,%ld)\n", 
                         Cfailed, Kfailed, Cw,
                         confThr, confTym, found, firstIx, lastIx);
      /* We can't indefinitely collect stack traces at VTS
         transitions, since we'd eventually run out of memory.  Hence
         note_local_Kw_n_stack_for will eventually throw away old
         ones, which in turn means we might fail to find index value
         confTym in the array. */
      if (found) {
         ULong_n_EC *pair_start, *pair_end;
         pair_start 
            = (ULong_n_EC*)VG_(indexXA)( confThr->local_Kws_n_stacks, lastIx );
         hist1_seg_start = pair_start->ec;
         if (lastIx+1 < VG_(sizeXA)( confThr->local_Kws_n_stacks )) {
            pair_end
               = (ULong_n_EC*)VG_(indexXA)( confThr->local_Kws_n_stacks,
                                            lastIx+1 );
            /* from properties of VG_(lookupXA) and the comparison fn used: */
            tl_assert(pair_start->ull < pair_end->ull);
            hist1_seg_end = pair_end->ec;
            /* Could do a bit better here.  It may be that pair_end
               doesn't have a stack, but the following entries in the
               array have the same scalar Kw and to have a stack.  So
               we should search a bit further along the array than
               lastIx+1 if hist1_seg_end is NULL. */
         } else {
            if (!confThr->llexit_done)
               hist1_seg_end = main_get_EC( confThr );
         }
         // seg_start could be NULL iff this is the first stack in the thread
         //if (seg_start) VG_(pp_ExeContext)(seg_start);
         //if (seg_end)   VG_(pp_ExeContext)(seg_end);
         hist1_conf_thr = confThr->hgthread;
      }
   }

   HG_(record_error_Race)( acc_thr->hgthread, acc_addr,
                           szB, isWrite,
                           hist1_conf_thr, hist1_seg_start, hist1_seg_end );
}

static Bool is_sane_SVal_C ( SVal sv ) {
   Bool leq;
   if (!SVal__isC(sv)) return True;
   leq = VtsID__cmpLEQ( SVal__unC_Rmin(sv), SVal__unC_Wmin(sv) );
   return leq;
}


/* Compute new state following a read */
static inline SVal msmcread ( SVal svOld,
                              /* The following are only needed for 
                                 creating error reports. */
                              Thr* acc_thr,
                              Addr acc_addr, SizeT szB )
{
   SVal svNew = SVal_INVALID;
   stats__msmcread++;

   /* Redundant sanity check on the constraints */
   if (CHECK_MSM) {
      tl_assert(is_sane_SVal_C(svOld));
   }

   if (LIKELY(SVal__isC(svOld))) {
      VtsID tviR  = acc_thr->viR;
      VtsID tviW  = acc_thr->viW;
      VtsID rmini = SVal__unC_Rmin(svOld);
      VtsID wmini = SVal__unC_Wmin(svOld);
      Bool  leq   = VtsID__cmpLEQ(rmini,tviR);
      if (LIKELY(leq)) {
         /* no race */
         /* Note: RWLOCK subtlety: use tviW, not tviR */
         svNew = SVal__mkC( rmini, VtsID__join2(wmini, tviW) );
         goto out;
      } else {
         /* assert on sanity of constraints. */
         Bool leqxx = VtsID__cmpLEQ(rmini,wmini);
         tl_assert(leqxx);
         // same as in non-race case
         svNew = SVal__mkC( rmini, VtsID__join2(wmini, tviW) );
         record_race_info( acc_thr, acc_addr, szB, False/*!isWrite*/,
                           rmini, /* Cfailed */
                           tviR,  /* Kfailed */
                           wmini  /* Cw */ );
         goto out;
      }
   }
   if (SVal__isA(svOld)) {
      /* reading no-access memory (sigh); leave unchanged */
      /* check for no pollution */
      tl_assert(svOld == SVal_NOACCESS);
      svNew = SVal_NOACCESS;
      goto out;
   }
   if (0) VG_(printf)("msmcread: bad svOld: 0x%016llx\n", svOld);
   tl_assert(0);

  out:
   if (CHECK_MSM) {
      tl_assert(is_sane_SVal_C(svNew));
   }
   if (UNLIKELY(svNew != svOld)) {
      tl_assert(svNew != SVal_INVALID);
      if (HG_(clo_history_level) >= 2
          && SVal__isC(svOld) && SVal__isC(svNew)) {
         event_map_bind( acc_addr, szB, False/*!isWrite*/, acc_thr );
         stats__msmcread_change++;
      }
   }
   return svNew;
}


/* Compute new state following a write */
static inline SVal msmcwrite ( SVal svOld,
                              /* The following are only needed for 
                                 creating error reports. */
                              Thr* acc_thr,
                              Addr acc_addr, SizeT szB )
{
   SVal svNew = SVal_INVALID;
   stats__msmcwrite++;

   /* Redundant sanity check on the constraints */
   if (CHECK_MSM) {
      tl_assert(is_sane_SVal_C(svOld));
   }

   if (LIKELY(SVal__isC(svOld))) {
      VtsID tviW  = acc_thr->viW;
      VtsID wmini = SVal__unC_Wmin(svOld);
      Bool  leq   = VtsID__cmpLEQ(wmini,tviW);
      if (LIKELY(leq)) {
         /* no race */
         svNew = SVal__mkC( tviW, tviW );
         goto out;
      } else {
         VtsID rmini = SVal__unC_Rmin(svOld);
         /* assert on sanity of constraints. */
         Bool leqxx = VtsID__cmpLEQ(rmini,wmini);
         tl_assert(leqxx);
         // same as in non-race case
         // proof: in the non-race case, we have
         //    rmini <= wmini (invar on constraints)
         //    tviW <= tviR (invar on thread clocks)
         //    wmini <= tviW (from run-time check)
         // hence from transitivity of <= we have
         //    rmini <= wmini <= tviW
         // and so join(rmini,tviW) == tviW
         // and    join(wmini,tviW) == tviW
         // qed.
         svNew = SVal__mkC( VtsID__join2(rmini, tviW),
                            VtsID__join2(wmini, tviW) );
         record_race_info( acc_thr, acc_addr, szB, True/*isWrite*/,
                           wmini, /* Cfailed */
                           tviW,  /* Kfailed */
                           wmini  /* Cw */ );
         goto out;
      }
   }
   if (SVal__isA(svOld)) {
      /* writing no-access memory (sigh); leave unchanged */
      /* check for no pollution */
      tl_assert(svOld == SVal_NOACCESS);
      svNew = SVal_NOACCESS;
      goto out;
   }
   if (0) VG_(printf)("msmcwrite: bad svOld: 0x%016llx\n", svOld);
   tl_assert(0);

  out:
   if (CHECK_MSM) {
      tl_assert(is_sane_SVal_C(svNew));
   }
   if (UNLIKELY(svNew != svOld)) {
      tl_assert(svNew != SVal_INVALID);
      if (HG_(clo_history_level) >= 2
          && SVal__isC(svOld) && SVal__isC(svNew)) {
         event_map_bind( acc_addr, szB, True/*isWrite*/, acc_thr );
         stats__msmcwrite_change++;
      }
   }
   return svNew;
}


/////////////////////////////////////////////////////////
//                                                     //
// Apply core MSM to specific memory locations         //
//                                                     //
/////////////////////////////////////////////////////////

/*------------- ZSM accesses: 8 bit sapply ------------- */

static void zsm_sapply08__msmcread ( Thr* thr, Addr a ) {
   CacheLine* cl; 
   UWord      cloff, tno, toff;
   SVal       svOld, svNew;
   UShort     descr;
   stats__cline_cread08s++;
   cl    = get_cacheline(a);
   cloff = get_cacheline_offset(a);
   tno   = get_treeno(a);
   toff  = get_tree_offset(a); /* == 0 .. 7 */
   descr = cl->descrs[tno];
   if (UNLIKELY( !(descr & (TREE_DESCR_8_0 << toff)) )) {
      SVal* tree = &cl->svals[tno << 3];
      cl->descrs[tno] = pulldown_to_8(tree, toff, descr);
      if (CHECK_ZSM)
         tl_assert(is_sane_CacheLine(cl)); /* EXPENSIVE */
   }
   svOld = cl->svals[cloff];
   svNew = msmcread( svOld, thr,a,1 );
   if (CHECK_ZSM)
      tl_assert(svNew != SVal_INVALID);
   cl->svals[cloff] = svNew;
}

static void zsm_sapply08__msmcwrite ( Thr* thr, Addr a ) {
   CacheLine* cl; 
   UWord      cloff, tno, toff;
   SVal       svOld, svNew;
   UShort     descr;
   stats__cline_cwrite08s++;
   cl    = get_cacheline(a);
   cloff = get_cacheline_offset(a);
   tno   = get_treeno(a);
   toff  = get_tree_offset(a); /* == 0 .. 7 */
   descr = cl->descrs[tno];
   if (UNLIKELY( !(descr & (TREE_DESCR_8_0 << toff)) )) {
      SVal* tree = &cl->svals[tno << 3];
      cl->descrs[tno] = pulldown_to_8(tree, toff, descr);
      if (CHECK_ZSM)
         tl_assert(is_sane_CacheLine(cl)); /* EXPENSIVE */
   }
   svOld = cl->svals[cloff];
   svNew = msmcwrite( svOld, thr,a,1 );
   if (CHECK_ZSM)
      tl_assert(svNew != SVal_INVALID);
   cl->svals[cloff] = svNew;
}

/*------------- ZSM accesses: 16 bit sapply ------------- */

static void zsm_sapply16__msmcread ( Thr* thr, Addr a ) {
   CacheLine* cl; 
   UWord      cloff, tno, toff;
   SVal       svOld, svNew;
   UShort     descr;
   stats__cline_cread16s++;
   if (UNLIKELY(!aligned16(a))) goto slowcase;
   cl    = get_cacheline(a);
   cloff = get_cacheline_offset(a);
   tno   = get_treeno(a);
   toff  = get_tree_offset(a); /* == 0, 2, 4 or 6 */
   descr = cl->descrs[tno];
   if (UNLIKELY( !(descr & (TREE_DESCR_16_0 << toff)) )) {
      if (valid_value_is_below_me_16(descr, toff)) {
         goto slowcase;
      } else {
         SVal* tree = &cl->svals[tno << 3];
         cl->descrs[tno] = pulldown_to_16(tree, toff, descr);
      }
      if (CHECK_ZSM)
         tl_assert(is_sane_CacheLine(cl)); /* EXPENSIVE */
   }
   svOld = cl->svals[cloff];
   svNew = msmcread( svOld, thr,a,2 );
   if (CHECK_ZSM)
      tl_assert(svNew != SVal_INVALID);
   cl->svals[cloff] = svNew;
   return;
  slowcase: /* misaligned, or must go further down the tree */
   stats__cline_16to8splits++;
   zsm_sapply08__msmcread( thr, a + 0 );
   zsm_sapply08__msmcread( thr, a + 1 );
}

static void zsm_sapply16__msmcwrite ( Thr* thr, Addr a ) {
   CacheLine* cl; 
   UWord      cloff, tno, toff;
   SVal       svOld, svNew;
   UShort     descr;
   stats__cline_cwrite16s++;
   if (UNLIKELY(!aligned16(a))) goto slowcase;
   cl    = get_cacheline(a);
   cloff = get_cacheline_offset(a);
   tno   = get_treeno(a);
   toff  = get_tree_offset(a); /* == 0, 2, 4 or 6 */
   descr = cl->descrs[tno];
   if (UNLIKELY( !(descr & (TREE_DESCR_16_0 << toff)) )) {
      if (valid_value_is_below_me_16(descr, toff)) {
         goto slowcase;
      } else {
         SVal* tree = &cl->svals[tno << 3];
         cl->descrs[tno] = pulldown_to_16(tree, toff, descr);
      }
      if (CHECK_ZSM)
         tl_assert(is_sane_CacheLine(cl)); /* EXPENSIVE */
   }
   svOld = cl->svals[cloff];
   svNew = msmcwrite( svOld, thr,a,2 );
   if (CHECK_ZSM)
      tl_assert(svNew != SVal_INVALID);
   cl->svals[cloff] = svNew;
   return;
  slowcase: /* misaligned, or must go further down the tree */
   stats__cline_16to8splits++;
   zsm_sapply08__msmcwrite( thr, a + 0 );
   zsm_sapply08__msmcwrite( thr, a + 1 );
}

/*------------- ZSM accesses: 32 bit sapply ------------- */

static void zsm_sapply32__msmcread ( Thr* thr, Addr a ) {
   CacheLine* cl; 
   UWord      cloff, tno, toff;
   SVal       svOld, svNew;
   UShort     descr;
   stats__cline_cread32s++;
   if (UNLIKELY(!aligned32(a))) goto slowcase;
   cl    = get_cacheline(a);
   cloff = get_cacheline_offset(a);
   tno   = get_treeno(a);
   toff  = get_tree_offset(a); /* == 0 or 4 */
   descr = cl->descrs[tno];
   if (UNLIKELY( !(descr & (TREE_DESCR_32_0 << toff)) )) {
      if (valid_value_is_above_me_32(descr, toff)) {
         SVal* tree = &cl->svals[tno << 3];
         cl->descrs[tno] = pulldown_to_32(tree, toff, descr);
      } else {
         goto slowcase;
      }
      if (CHECK_ZSM)
         tl_assert(is_sane_CacheLine(cl)); /* EXPENSIVE */
   }
   svOld = cl->svals[cloff];
   svNew = msmcread( svOld, thr,a,4 );
   if (CHECK_ZSM)
      tl_assert(svNew != SVal_INVALID);
   cl->svals[cloff] = svNew;
   return;
  slowcase: /* misaligned, or must go further down the tree */
   stats__cline_32to16splits++;
   zsm_sapply16__msmcread( thr, a + 0 );
   zsm_sapply16__msmcread( thr, a + 2 );
}

static void zsm_sapply32__msmcwrite ( Thr* thr, Addr a ) {
   CacheLine* cl; 
   UWord      cloff, tno, toff;
   SVal       svOld, svNew;
   UShort     descr;
   stats__cline_cwrite32s++;
   if (UNLIKELY(!aligned32(a))) goto slowcase;
   cl    = get_cacheline(a);
   cloff = get_cacheline_offset(a);
   tno   = get_treeno(a);
   toff  = get_tree_offset(a); /* == 0 or 4 */
   descr = cl->descrs[tno];
   if (UNLIKELY( !(descr & (TREE_DESCR_32_0 << toff)) )) {
      if (valid_value_is_above_me_32(descr, toff)) {
         SVal* tree = &cl->svals[tno << 3];
         cl->descrs[tno] = pulldown_to_32(tree, toff, descr);
      } else {
         goto slowcase;
      }
      if (CHECK_ZSM)
         tl_assert(is_sane_CacheLine(cl)); /* EXPENSIVE */
   }
   svOld = cl->svals[cloff];
   svNew = msmcwrite( svOld, thr,a,4 );
   if (CHECK_ZSM)
      tl_assert(svNew != SVal_INVALID);
   cl->svals[cloff] = svNew;
   return;
  slowcase: /* misaligned, or must go further down the tree */
   stats__cline_32to16splits++;
   zsm_sapply16__msmcwrite( thr, a + 0 );
   zsm_sapply16__msmcwrite( thr, a + 2 );
}

/*------------- ZSM accesses: 64 bit sapply ------------- */

static void zsm_sapply64__msmcread ( Thr* thr, Addr a ) {
   CacheLine* cl; 
   UWord      cloff, tno;
   //UWord      toff;
   SVal       svOld, svNew;
   UShort     descr;
   stats__cline_cread64s++;
   if (UNLIKELY(!aligned64(a))) goto slowcase;
   cl    = get_cacheline(a);
   cloff = get_cacheline_offset(a);
   tno   = get_treeno(a);
   //toff  = get_tree_offset(a); /* == 0, unused */
   descr = cl->descrs[tno];
   if (UNLIKELY( !(descr & TREE_DESCR_64) )) {
      goto slowcase;
   }
   svOld = cl->svals[cloff];
   svNew = msmcread( svOld, thr,a,8 );
   if (CHECK_ZSM)
      tl_assert(svNew != SVal_INVALID);
   cl->svals[cloff] = svNew;
   return;
  slowcase: /* misaligned, or must go further down the tree */
   stats__cline_64to32splits++;
   zsm_sapply32__msmcread( thr, a + 0 );
   zsm_sapply32__msmcread( thr, a + 4 );
}

static void zsm_sapply64__msmcwrite ( Thr* thr, Addr a ) {
   CacheLine* cl; 
   UWord      cloff, tno;
   //UWord      toff;
   SVal       svOld, svNew;
   UShort     descr;
   stats__cline_cwrite64s++;
   if (UNLIKELY(!aligned64(a))) goto slowcase;
   cl    = get_cacheline(a);
   cloff = get_cacheline_offset(a);
   tno   = get_treeno(a);
   //toff  = get_tree_offset(a); /* == 0, unused */
   descr = cl->descrs[tno];
   if (UNLIKELY( !(descr & TREE_DESCR_64) )) {
      goto slowcase;
   }
   svOld = cl->svals[cloff];
   svNew = msmcwrite( svOld, thr,a,8 );
   if (CHECK_ZSM)
      tl_assert(svNew != SVal_INVALID);
   cl->svals[cloff] = svNew;
   return;
  slowcase: /* misaligned, or must go further down the tree */
   stats__cline_64to32splits++;
   zsm_sapply32__msmcwrite( thr, a + 0 );
   zsm_sapply32__msmcwrite( thr, a + 4 );
}

/*--------------- ZSM accesses: 8 bit swrite --------------- */

static
void zsm_swrite08 ( Addr a, SVal svNew ) {
   CacheLine* cl; 
   UWord      cloff, tno, toff;
   UShort     descr;
   stats__cline_swrite08s++;
   cl    = get_cacheline(a);
   cloff = get_cacheline_offset(a);
   tno   = get_treeno(a);
   toff  = get_tree_offset(a); /* == 0 .. 7 */
   descr = cl->descrs[tno];
   if (UNLIKELY( !(descr & (TREE_DESCR_8_0 << toff)) )) {
      SVal* tree = &cl->svals[tno << 3];
      cl->descrs[tno] = pulldown_to_8(tree, toff, descr);
      if (CHECK_ZSM)
         tl_assert(is_sane_CacheLine(cl)); /* EXPENSIVE */
   }
   tl_assert(svNew != SVal_INVALID);
   cl->svals[cloff] = svNew;
}

/*--------------- ZSM accesses: 16 bit swrite --------------- */

static
void zsm_swrite16 ( Addr a, SVal svNew ) {
   CacheLine* cl; 
   UWord      cloff, tno, toff;
   UShort     descr;
   stats__cline_swrite16s++;
   if (UNLIKELY(!aligned16(a))) goto slowcase;
   cl    = get_cacheline(a);
   cloff = get_cacheline_offset(a);
   tno   = get_treeno(a);
   toff  = get_tree_offset(a); /* == 0, 2, 4 or 6 */
   descr = cl->descrs[tno];
   if (UNLIKELY( !(descr & (TREE_DESCR_16_0 << toff)) )) {
      if (valid_value_is_below_me_16(descr, toff)) {
         /* Writing at this level.  Need to fix up 'descr'. */
         cl->descrs[tno] = pullup_descr_to_16(descr, toff);
         /* At this point, the tree does not match cl->descr[tno] any
            more.  The assignments below will fix it up. */
      } else {
         /* We can't indiscriminately write on the w16 node as in the
            w64 case, as that might make the node inconsistent with
            its parent.  So first, pull down to this level. */
         SVal* tree = &cl->svals[tno << 3];
         cl->descrs[tno] = pulldown_to_16(tree, toff, descr);
      if (CHECK_ZSM)
         tl_assert(is_sane_CacheLine(cl)); /* EXPENSIVE */
      }
   }
   tl_assert(svNew != SVal_INVALID);
   cl->svals[cloff + 0] = svNew;
   cl->svals[cloff + 1] = SVal_INVALID;
   return;
  slowcase: /* misaligned */
   stats__cline_16to8splits++;
   zsm_swrite08( a + 0, svNew );
   zsm_swrite08( a + 1, svNew );
}

/*--------------- ZSM accesses: 32 bit swrite --------------- */

static
void zsm_swrite32 ( Addr a, SVal svNew ) {
   CacheLine* cl; 
   UWord      cloff, tno, toff;
   UShort     descr;
   stats__cline_swrite32s++;
   if (UNLIKELY(!aligned32(a))) goto slowcase;
   cl    = get_cacheline(a);
   cloff = get_cacheline_offset(a);
   tno   = get_treeno(a);
   toff  = get_tree_offset(a); /* == 0 or 4 */
   descr = cl->descrs[tno];
   if (UNLIKELY( !(descr & (TREE_DESCR_32_0 << toff)) )) {
      if (valid_value_is_above_me_32(descr, toff)) {
         /* We can't indiscriminately write on the w32 node as in the
            w64 case, as that might make the node inconsistent with
            its parent.  So first, pull down to this level. */
         SVal* tree = &cl->svals[tno << 3];
         cl->descrs[tno] = pulldown_to_32(tree, toff, descr);
         if (CHECK_ZSM)
            tl_assert(is_sane_CacheLine(cl)); /* EXPENSIVE */
      } else {
         /* Writing at this level.  Need to fix up 'descr'. */
         cl->descrs[tno] = pullup_descr_to_32(descr, toff);
         /* At this point, the tree does not match cl->descr[tno] any
            more.  The assignments below will fix it up. */
      }
   }
   tl_assert(svNew != SVal_INVALID);
   cl->svals[cloff + 0] = svNew;
   cl->svals[cloff + 1] = SVal_INVALID;
   cl->svals[cloff + 2] = SVal_INVALID;
   cl->svals[cloff + 3] = SVal_INVALID;
   return;
  slowcase: /* misaligned */
   stats__cline_32to16splits++;
   zsm_swrite16( a + 0, svNew );
   zsm_swrite16( a + 2, svNew );
}

/*--------------- ZSM accesses: 64 bit swrite --------------- */

static
void zsm_swrite64 ( Addr a, SVal svNew ) {
   CacheLine* cl; 
   UWord      cloff, tno;
   //UWord    toff;
   stats__cline_swrite64s++;
   if (UNLIKELY(!aligned64(a))) goto slowcase;
   cl    = get_cacheline(a);
   cloff = get_cacheline_offset(a);
   tno   = get_treeno(a);
   //toff  = get_tree_offset(a); /* == 0, unused */
   cl->descrs[tno] = TREE_DESCR_64;
   if (CHECK_ZSM)
      tl_assert(svNew != SVal_INVALID); /* EXPENSIVE */
   cl->svals[cloff + 0] = svNew;
   cl->svals[cloff + 1] = SVal_INVALID;
   cl->svals[cloff + 2] = SVal_INVALID;
   cl->svals[cloff + 3] = SVal_INVALID;
   cl->svals[cloff + 4] = SVal_INVALID;
   cl->svals[cloff + 5] = SVal_INVALID;
   cl->svals[cloff + 6] = SVal_INVALID;
   cl->svals[cloff + 7] = SVal_INVALID;
   return;
  slowcase: /* misaligned */
   stats__cline_64to32splits++;
   zsm_swrite32( a + 0, svNew );
   zsm_swrite32( a + 4, svNew );
}

/*------------- ZSM accesses: 8 bit sread/scopy ------------- */

static
SVal zsm_sread08 ( Addr a ) {
   CacheLine* cl; 
   UWord      cloff, tno, toff;
   UShort     descr;
   stats__cline_sread08s++;
   cl    = get_cacheline(a);
   cloff = get_cacheline_offset(a);
   tno   = get_treeno(a);
   toff  = get_tree_offset(a); /* == 0 .. 7 */
   descr = cl->descrs[tno];
   if (UNLIKELY( !(descr & (TREE_DESCR_8_0 << toff)) )) {
      SVal* tree = &cl->svals[tno << 3];
      cl->descrs[tno] = pulldown_to_8(tree, toff, descr);
   }
   return cl->svals[cloff];
}

static void zsm_scopy08 ( Addr src, Addr dst, Bool uu_normalise ) {
   SVal       sv;
   stats__cline_scopy08s++;
   sv = zsm_sread08( src );
   zsm_swrite08( dst, sv );
}


/* Block-copy states (needed for implementing realloc()).  Note this
   doesn't change the filtering arrangements.  The caller of
   zsm_scopy_range needs to attend to that. */

static void zsm_scopy_range ( Addr src, Addr dst, SizeT len )
{
   SizeT i;
   if (len == 0)
      return;

   /* assert for non-overlappingness */
   tl_assert(src+len <= dst || dst+len <= src);

   /* To be simple, just copy byte by byte.  But so as not to wreck
      performance for later accesses to dst[0 .. len-1], normalise
      destination lines as we finish with them, and also normalise the
      line containing the first and last address. */
   for (i = 0; i < len; i++) {
      Bool normalise
         = get_cacheline_offset( dst+i+1 ) == 0 /* last in line */
           || i == 0       /* first in range */
           || i == len-1;  /* last in range */
      zsm_scopy08( src+i, dst+i, normalise );
   }
}


/* For setting address ranges to a given value.  Has considerable
   sophistication so as to avoid generating large numbers of pointless
   cache loads/writebacks for large ranges. */

/* Do small ranges in-cache, in the obvious way. */
static
void zsm_sset_range_SMALL ( Addr a, SizeT len, SVal svNew )
{
   /* fast track a couple of common cases */
   if (len == 4 && aligned32(a)) {
      zsm_swrite32( a, svNew );
      return;
   }
   if (len == 8 && aligned64(a)) {
      zsm_swrite64( a, svNew );
      return;
   }

   /* be completely general (but as efficient as possible) */
   if (len == 0) return;

   if (!aligned16(a) && len >= 1) {
      zsm_swrite08( a, svNew );
      a += 1;
      len -= 1;
      tl_assert(aligned16(a));
   }
   if (len == 0) return;

   if (!aligned32(a) && len >= 2) {
      zsm_swrite16( a, svNew );
      a += 2;
      len -= 2;
      tl_assert(aligned32(a));
   }
   if (len == 0) return;

   if (!aligned64(a) && len >= 4) {
      zsm_swrite32( a, svNew );
      a += 4;
      len -= 4;
      tl_assert(aligned64(a));
   }
   if (len == 0) return;

   if (len >= 8) {
      tl_assert(aligned64(a));
      while (len >= 8) {
         zsm_swrite64( a, svNew );
         a += 8;
         len -= 8;
      }
      tl_assert(aligned64(a));
   }
   if (len == 0) return;

   if (len >= 4)
      tl_assert(aligned32(a));
   if (len >= 4) {
      zsm_swrite32( a, svNew );
      a += 4;
      len -= 4;
   }
   if (len == 0) return;

   if (len >= 2)
      tl_assert(aligned16(a));
   if (len >= 2) {
      zsm_swrite16( a, svNew );
      a += 2;
      len -= 2;
   }
   if (len == 0) return;

   if (len >= 1) {
      zsm_swrite08( a, svNew );
      //a += 1;
      len -= 1;
   }
   tl_assert(len == 0);
}


/* If we're doing a small range, hand off to zsm_sset_range_SMALL.  But
   for larger ranges, try to operate directly on the out-of-cache
   representation, rather than dragging lines into the cache,
   overwriting them, and forcing them out.  This turns out to be an
   important performance optimisation.

   Note that this doesn't change the filtering arrangements.  The
   caller of zsm_sset_range needs to attend to that. */

static void zsm_sset_range ( Addr a, SizeT len, SVal svNew )
{
   tl_assert(svNew != SVal_INVALID);
   stats__cache_make_New_arange += (ULong)len;

   if (0 && len > 500)
      VG_(printf)("make New      ( %#lx, %lu )\n", a, len );

   if (0) {
      static UWord n_New_in_cache = 0;
      static UWord n_New_not_in_cache = 0;
      /* tag is 'a' with the in-line offset masked out, 
         eg a[31]..a[4] 0000 */
      Addr       tag = a & ~(N_LINE_ARANGE - 1);
      UWord      wix = (a >> N_LINE_BITS) & (N_WAY_NENT - 1);
      if (LIKELY(tag == cache_shmem.tags0[wix])) {
         n_New_in_cache++;
      } else {
         n_New_not_in_cache++;
      }
      if (0 == ((n_New_in_cache + n_New_not_in_cache) % 100000))
         VG_(printf)("shadow_mem_make_New: IN %lu OUT %lu\n",
                     n_New_in_cache, n_New_not_in_cache );
   }

   if (LIKELY(len < 2 * N_LINE_ARANGE)) {
      zsm_sset_range_SMALL( a, len, svNew );
   } else {
      Addr  before_start  = a;
      Addr  aligned_start = cacheline_ROUNDUP(a);
      Addr  after_start   = cacheline_ROUNDDN(a + len);
      UWord before_len    = aligned_start - before_start;
      UWord aligned_len   = after_start - aligned_start;
      UWord after_len     = a + len - after_start;
      tl_assert(before_start <= aligned_start);
      tl_assert(aligned_start <= after_start);
      tl_assert(before_len < N_LINE_ARANGE);
      tl_assert(after_len < N_LINE_ARANGE);
      tl_assert(get_cacheline_offset(aligned_start) == 0);
      if (get_cacheline_offset(a) == 0) {
         tl_assert(before_len == 0);
         tl_assert(a == aligned_start);
      }
      if (get_cacheline_offset(a+len) == 0) {
         tl_assert(after_len == 0);
         tl_assert(after_start == a+len);
      }
      if (before_len > 0) {
         zsm_sset_range_SMALL( before_start, before_len, svNew );
      }
      if (after_len > 0) {
         zsm_sset_range_SMALL( after_start, after_len, svNew );
      }
      stats__cache_make_New_inZrep += (ULong)aligned_len;

      while (1) {
         Addr tag;
         UWord wix;
         if (aligned_start >= after_start)
            break;
         tl_assert(get_cacheline_offset(aligned_start) == 0);
         tag = aligned_start & ~(N_LINE_ARANGE - 1);
         wix = (aligned_start >> N_LINE_BITS) & (N_WAY_NENT - 1);
         if (tag == cache_shmem.tags0[wix]) {
            UWord i;
            for (i = 0; i < N_LINE_ARANGE / 8; i++)
               zsm_swrite64( aligned_start + i * 8, svNew );
         } else {
            UWord i;
            Word zix;
            SecMap* sm;
            LineZ* lineZ;
            /* This line is not in the cache.  Do not force it in; instead
               modify it in-place. */
            /* find the Z line to write in and rcdec it or the
               associated F line. */
            find_Z_for_writing( &sm, &zix, tag );
            tl_assert(sm);
            tl_assert(zix >= 0 && zix < N_SECMAP_ZLINES);
            lineZ = &sm->linesZ[zix];
            lineZ->dict[0] = svNew;
            lineZ->dict[1] = lineZ->dict[2] = lineZ->dict[3] = SVal_INVALID;
            for (i = 0; i < N_LINE_ARANGE/4; i++)
               lineZ->ix2s[i] = 0; /* all refer to dict[0] */
            rcinc_LineZ(lineZ);
         }
         aligned_start += N_LINE_ARANGE;
         aligned_len -= N_LINE_ARANGE;
      }
      tl_assert(aligned_start == after_start);
      tl_assert(aligned_len == 0);
   }
}


/////////////////////////////////////////////////////////
//                                                     //
// Front-filtering accesses                            //
//                                                     //
/////////////////////////////////////////////////////////

static UWord stats__f_ac = 0;
static UWord stats__f_sk = 0;

#if 0
#  define STATS__F_SHOW \
     do { \
        if (UNLIKELY(0 == (stats__f_ac & 0xFFFFFF))) \
           VG_(printf)("filters: ac %lu sk %lu\n",   \
           stats__f_ac, stats__f_sk); \
     } while (0)
#else
#  define STATS__F_SHOW /* */
#endif

void zsm_sapply08_f__msmcwrite ( Thr* thr, Addr a ) {
   stats__f_ac++;
   STATS__F_SHOW;
   if (LIKELY(Filter__ok_to_skip_cwr08(thr->filter, a))) {
      stats__f_sk++;
      return;
   }
   zsm_sapply08__msmcwrite(thr, a);
}

void zsm_sapply16_f__msmcwrite ( Thr* thr, Addr a ) {
   stats__f_ac++;
   STATS__F_SHOW;
   if (LIKELY(Filter__ok_to_skip_cwr16(thr->filter, a))) {
      stats__f_sk++;
      return;
   }
   zsm_sapply16__msmcwrite(thr, a);
}

void zsm_sapply32_f__msmcwrite ( Thr* thr, Addr a ) {
   stats__f_ac++;
   STATS__F_SHOW;
   if (LIKELY(Filter__ok_to_skip_cwr32(thr->filter, a))) {
      stats__f_sk++;
      return;
   }
   zsm_sapply32__msmcwrite(thr, a);
}

void zsm_sapply64_f__msmcwrite ( Thr* thr, Addr a ) {
   stats__f_ac++;
   STATS__F_SHOW;
   if (LIKELY(Filter__ok_to_skip_cwr64(thr->filter, a))) {
      stats__f_sk++;
      return;
   }
   zsm_sapply64__msmcwrite(thr, a);
}

void zsm_sapplyNN_f__msmcwrite ( Thr* thr, Addr a, SizeT len )
{
   /* fast track a couple of common cases */
   if (len == 4 && aligned32(a)) {
      zsm_sapply32_f__msmcwrite( thr, a );
      return;
   }
   if (len == 8 && aligned64(a)) {
      zsm_sapply64_f__msmcwrite( thr, a );
      return;
   }

   /* be completely general (but as efficient as possible) */
   if (len == 0) return;

   if (!aligned16(a) && len >= 1) {
      zsm_sapply08_f__msmcwrite( thr, a );
      a += 1;
      len -= 1;
      tl_assert(aligned16(a));
   }
   if (len == 0) return;

   if (!aligned32(a) && len >= 2) {
      zsm_sapply16_f__msmcwrite( thr, a );
      a += 2;
      len -= 2;
      tl_assert(aligned32(a));
   }
   if (len == 0) return;

   if (!aligned64(a) && len >= 4) {
      zsm_sapply32_f__msmcwrite( thr, a );
      a += 4;
      len -= 4;
      tl_assert(aligned64(a));
   }
   if (len == 0) return;

   if (len >= 8) {
      tl_assert(aligned64(a));
      while (len >= 8) {
         zsm_sapply64_f__msmcwrite( thr, a );
         a += 8;
         len -= 8;
      }
      tl_assert(aligned64(a));
   }
   if (len == 0) return;

   if (len >= 4)
      tl_assert(aligned32(a));
   if (len >= 4) {
      zsm_sapply32_f__msmcwrite( thr, a );
      a += 4;
      len -= 4;
   }
   if (len == 0) return;

   if (len >= 2)
      tl_assert(aligned16(a));
   if (len >= 2) {
      zsm_sapply16_f__msmcwrite( thr, a );
      a += 2;
      len -= 2;
   }
   if (len == 0) return;

   if (len >= 1) {
      zsm_sapply08_f__msmcwrite( thr, a );
      //a += 1;
      len -= 1;
   }
   tl_assert(len == 0);
}

void zsm_sapply08_f__msmcread ( Thr* thr, Addr a ) {
   stats__f_ac++;
   STATS__F_SHOW;
   if (LIKELY(Filter__ok_to_skip_crd08(thr->filter, a))) {
      stats__f_sk++;
      return;
   }
   zsm_sapply08__msmcread(thr, a);
}

void zsm_sapply16_f__msmcread ( Thr* thr, Addr a ) {
   stats__f_ac++;
   STATS__F_SHOW;
   if (LIKELY(Filter__ok_to_skip_crd16(thr->filter, a))) {
      stats__f_sk++;
      return;
   }
   zsm_sapply16__msmcread(thr, a);
}

void zsm_sapply32_f__msmcread ( Thr* thr, Addr a ) {
   stats__f_ac++;
   STATS__F_SHOW;
   if (LIKELY(Filter__ok_to_skip_crd32(thr->filter, a))) {
      stats__f_sk++;
      return;
   }
   zsm_sapply32__msmcread(thr, a);
}

void zsm_sapply64_f__msmcread ( Thr* thr, Addr a ) {
   stats__f_ac++;
   STATS__F_SHOW;
   if (LIKELY(Filter__ok_to_skip_crd64(thr->filter, a))) {
      stats__f_sk++;
      return;
   }
   zsm_sapply64__msmcread(thr, a);
}

void zsm_sapplyNN_f__msmcread ( Thr* thr, Addr a, SizeT len )
{
   /* fast track a couple of common cases */
   if (len == 4 && aligned32(a)) {
      zsm_sapply32_f__msmcread( thr, a );
      return;
   }
   if (len == 8 && aligned64(a)) {
      zsm_sapply64_f__msmcread( thr, a );
      return;
   }

   /* be completely general (but as efficient as possible) */
   if (len == 0) return;

   if (!aligned16(a) && len >= 1) {
      zsm_sapply08_f__msmcread( thr, a );
      a += 1;
      len -= 1;
      tl_assert(aligned16(a));
   }
   if (len == 0) return;

   if (!aligned32(a) && len >= 2) {
      zsm_sapply16_f__msmcread( thr, a );
      a += 2;
      len -= 2;
      tl_assert(aligned32(a));
   }
   if (len == 0) return;

   if (!aligned64(a) && len >= 4) {
      zsm_sapply32_f__msmcread( thr, a );
      a += 4;
      len -= 4;
      tl_assert(aligned64(a));
   }
   if (len == 0) return;

   if (len >= 8) {
      tl_assert(aligned64(a));
      while (len >= 8) {
         zsm_sapply64_f__msmcread( thr, a );
         a += 8;
         len -= 8;
      }
      tl_assert(aligned64(a));
   }
   if (len == 0) return;

   if (len >= 4)
      tl_assert(aligned32(a));
   if (len >= 4) {
      zsm_sapply32_f__msmcread( thr, a );
      a += 4;
      len -= 4;
   }
   if (len == 0) return;

   if (len >= 2)
      tl_assert(aligned16(a));
   if (len >= 2) {
      zsm_sapply16_f__msmcread( thr, a );
      a += 2;
      len -= 2;
   }
   if (len == 0) return;

   if (len >= 1) {
      zsm_sapply08_f__msmcread( thr, a );
      //a += 1;
      len -= 1;
   }
   tl_assert(len == 0);
}

void libhb_Thr_resumes ( Thr* thr )
{
   if (0) VG_(printf)("resume %p\n", thr);
   tl_assert(thr);
   tl_assert(!thr->llexit_done);
   Filter__clear(thr->filter, "libhb_Thr_resumes");
   /* A kludge, but .. if this thread doesn't have any marker stacks
      at all, get one right now.  This is easier than figuring out
      exactly when at thread startup we can and can't take a stack
      snapshot. */
   if (HG_(clo_history_level) == 1) {
      tl_assert(thr->local_Kws_n_stacks);
      if (VG_(sizeXA)( thr->local_Kws_n_stacks ) == 0)
         note_local_Kw_n_stack_for(thr);
   }
}


/////////////////////////////////////////////////////////
//                                                     //
// Synchronisation objects                             //
//                                                     //
/////////////////////////////////////////////////////////

/* A double linked list of all the SO's. */
SO* admin_SO = NULL;

static SO* SO__Alloc ( void )
{
   SO* so = HG_(zalloc)( "libhb.SO__Alloc.1", sizeof(SO) );
   so->viR   = VtsID_INVALID;
   so->viW   = VtsID_INVALID;
   so->magic = SO_MAGIC;
   /* Add to double linked list */
   if (admin_SO) {
      tl_assert(admin_SO->admin_prev == NULL);
      admin_SO->admin_prev = so;
      so->admin_next = admin_SO;
   } else {
      so->admin_next = NULL;
   }
   so->admin_prev = NULL;
   admin_SO = so;
   /* */
   return so;
}

static void SO__Dealloc ( SO* so )
{
   tl_assert(so);
   tl_assert(so->magic == SO_MAGIC);
   if (so->viR == VtsID_INVALID) {
      tl_assert(so->viW == VtsID_INVALID);
   } else {
      tl_assert(so->viW != VtsID_INVALID);
      VtsID__rcdec(so->viR);
      VtsID__rcdec(so->viW);
   }
   so->magic = 0;
   /* Del from double linked list */
   if (so->admin_prev)
      so->admin_prev->admin_next = so->admin_next;
   if (so->admin_next)
      so->admin_next->admin_prev = so->admin_prev;
   if (so == admin_SO)
      admin_SO = so->admin_next;
   /* */
   HG_(free)( so );
}


/////////////////////////////////////////////////////////
//                                                     //
// Top Level API                                       //
//                                                     //
/////////////////////////////////////////////////////////

static void show_thread_state ( const HChar* str, Thr* t ) 
{
   if (1) return;
   if (t->viR == t->viW) {
      VG_(printf)("thr \"%s\" %p has vi* %u==", str, t, t->viR );
      VtsID__pp( t->viR );
      VG_(printf)("%s","\n");
   } else {
      VG_(printf)("thr \"%s\" %p has viR %u==", str, t, t->viR );
      VtsID__pp( t->viR );
      VG_(printf)(" viW %u==", t->viW);
      VtsID__pp( t->viW );
      VG_(printf)("%s","\n");
   }
}


Thr* libhb_init (
        void        (*get_stacktrace)( Thr*, Addr*, UWord ),
        ExeContext* (*get_EC)( Thr* )
     )
{
   Thr*  thr;
   VtsID vi;

   // We will have to have to store a large number of these,
   // so make sure they're the size we expect them to be.
   STATIC_ASSERT(sizeof(ScalarTS) == 8);

   /* because first 1024 unusable */
   STATIC_ASSERT(SCALARTS_N_THRBITS >= 11);
   /* so as to fit in a UInt w/ 5 bits to spare (see defn of
      Thr_n_RCEC and TSW). */
   STATIC_ASSERT(SCALARTS_N_THRBITS <= 27);

   /* Need to be sure that Thr_n_RCEC is 2 words (64-bit) or 3 words
      (32-bit).  It's not correctness-critical, but there are a lot of
      them, so it's important from a space viewpoint.  Unfortunately
      we simply can't pack it into 2 words on a 32-bit target. */
   STATIC_ASSERT(   (sizeof(UWord) == 8 && sizeof(Thr_n_RCEC) == 16)
                 || (sizeof(UWord) == 4 && sizeof(Thr_n_RCEC) == 12));
   STATIC_ASSERT(sizeof(TSW) == sizeof(UInt));

   /* Word sets really are 32 bits.  Even on a 64 bit target. */
   STATIC_ASSERT(sizeof(WordSetID) == 4);
   STATIC_ASSERT(sizeof(WordSet) == sizeof(WordSetID));

   tl_assert(get_stacktrace);
   tl_assert(get_EC);
   main_get_stacktrace   = get_stacktrace;
   main_get_EC           = get_EC;

   // No need to initialise hg_wordfm.
   // No need to initialise hg_wordset.

   /* Allocated once and never deallocated.  Used as a temporary in
      VTS singleton, tick and join operations. */
   temp_max_sized_VTS = VTS__new( "libhb.libhb_init.1", ThrID_MAX_VALID );
   temp_max_sized_VTS->id = VtsID_INVALID;
   verydead_thread_tables_init();
   vts_set_init();
   vts_tab_init();
   event_map_init();
   VtsID__invalidate_caches();

   // initialise shadow memory
   zsm_init( );

   thr = Thr__new();
   vi  = VtsID__mk_Singleton( thr, 1 );
   thr->viR = vi;
   thr->viW = vi;
   VtsID__rcinc(thr->viR);
   VtsID__rcinc(thr->viW);

   show_thread_state("  root", thr);
   return thr;
}


Thr* libhb_create ( Thr* parent )
{
   /* The child's VTSs are copies of the parent's VTSs, but ticked at
      the child's index.  Since the child's index is guaranteed
      unique, it has never been seen before, so the implicit value
      before the tick is zero and after that is one. */
   Thr* child = Thr__new();

   child->viR = VtsID__tick( parent->viR, child );
   child->viW = VtsID__tick( parent->viW, child );
   Filter__clear(child->filter, "libhb_create(child)");
   VtsID__rcinc(child->viR);
   VtsID__rcinc(child->viW);
   /* We need to do note_local_Kw_n_stack_for( child ), but it's too
      early for that - it may not have a valid TId yet.  So, let
      libhb_Thr_resumes pick it up the first time the thread runs. */

   tl_assert(VtsID__indexAt( child->viR, child ) == 1);
   tl_assert(VtsID__indexAt( child->viW, child ) == 1);

   /* and the parent has to move along too */
   VtsID__rcdec(parent->viR);
   VtsID__rcdec(parent->viW);
   parent->viR = VtsID__tick( parent->viR, parent );
   parent->viW = VtsID__tick( parent->viW, parent );
   Filter__clear(parent->filter, "libhb_create(parent)");
   VtsID__rcinc(parent->viR);
   VtsID__rcinc(parent->viW);
   note_local_Kw_n_stack_for( parent );

   show_thread_state(" child", child);
   show_thread_state("parent", parent);

   return child;
}

/* Shut down the library, and print stats (in fact that's _all_
   this is for. */
void libhb_shutdown ( Bool show_stats )
{
   if (show_stats) {
      VG_(printf)("%s","<<< BEGIN libhb stats >>>\n");
      VG_(printf)(" secmaps: %'10lu allocd (%'12lu g-a-range)\n",
                  stats__secmaps_allocd,
                  stats__secmap_ga_space_covered);
      VG_(printf)("  linesZ: %'10lu allocd (%'12lu bytes occupied)\n",
                  stats__secmap_linesZ_allocd,
                  stats__secmap_linesZ_bytes);
      VG_(printf)("  linesF: %'10lu allocd (%'12lu bytes occupied)"
                  " (%'10lu used)\n",
                  VG_(sizePA) (LineF_pool_allocator),
                  VG_(sizePA) (LineF_pool_allocator) * sizeof(LineF),
                  shmem__SecMap_used_linesF());
      VG_(printf)(" secmaps: %'10lu in map (can be scanGCed %'5lu)"
                  " #%lu scanGC \n",
                  stats__secmaps_in_map_shmem,
                  shmem__SecMap_do_GC(False /* really do GC */),
                  stats__secmaps_scanGC);
      tl_assert (VG_(sizeFM) (map_shmem) == stats__secmaps_in_map_shmem);
      VG_(printf)(" secmaps: %'10lu in freelist,"
                  " total (scanGCed %'lu, ssetGCed %'lu)\n",
                  SecMap_freelist_length(),
                  stats__secmaps_scanGCed,
                  stats__secmaps_ssetGCed);
      VG_(printf)(" secmaps: %'10lu searches (%'12lu slow)\n",
                  stats__secmaps_search, stats__secmaps_search_slow);

      VG_(printf)("%s","\n");
      VG_(printf)("   cache: %'lu totrefs (%'lu misses)\n",
                  stats__cache_totrefs, stats__cache_totmisses );
      VG_(printf)("   cache: %'14lu Z-fetch,    %'14lu F-fetch\n",
                  stats__cache_Z_fetches, stats__cache_F_fetches );
      VG_(printf)("   cache: %'14lu Z-wback,    %'14lu F-wback\n",
                  stats__cache_Z_wbacks, stats__cache_F_wbacks );
      VG_(printf)("   cache: %'14lu flushes_invals\n",
                  stats__cache_flushes_invals );
      VG_(printf)("   cache: %'14llu arange_New  %'14llu direct-to-Zreps\n",
                  stats__cache_make_New_arange,
                  stats__cache_make_New_inZrep);

      VG_(printf)("%s","\n");
      VG_(printf)("   cline: %'10lu normalises\n",
                  stats__cline_normalises );
      VG_(printf)("   cline: c rds 8/4/2/1: %'13lu %'13lu %'13lu %'13lu\n",
                  stats__cline_cread64s,
                  stats__cline_cread32s,
                  stats__cline_cread16s,
                  stats__cline_cread08s );
      VG_(printf)("   cline: c wrs 8/4/2/1: %'13lu %'13lu %'13lu %'13lu\n",
                  stats__cline_cwrite64s,
                  stats__cline_cwrite32s,
                  stats__cline_cwrite16s,
                  stats__cline_cwrite08s );
      VG_(printf)("   cline: s wrs 8/4/2/1: %'13lu %'13lu %'13lu %'13lu\n",
                  stats__cline_swrite64s,
                  stats__cline_swrite32s,
                  stats__cline_swrite16s,
                  stats__cline_swrite08s );
      VG_(printf)("   cline: s rd1s %'lu, s copy1s %'lu\n",
                  stats__cline_sread08s, stats__cline_scopy08s );
      VG_(printf)("   cline:    splits: 8to4 %'12lu    4to2 %'12lu"
                  "    2to1 %'12lu\n",
                  stats__cline_64to32splits, stats__cline_32to16splits,
                  stats__cline_16to8splits );
      VG_(printf)("   cline: pulldowns: 8to4 %'12lu    4to2 %'12lu"
                  "    2to1 %'12lu\n",
                  stats__cline_64to32pulldown, stats__cline_32to16pulldown,
                  stats__cline_16to8pulldown );
      if (0)
      VG_(printf)("   cline: sizeof(CacheLineZ) %ld,"
                  " covers %ld bytes of arange\n",
                  (Word)sizeof(LineZ),
                  (Word)N_LINE_ARANGE);

      VG_(printf)("%s","\n");

      VG_(printf)("   libhb: %'13llu msmcread  (%'llu dragovers)\n",
                  stats__msmcread, stats__msmcread_change);
      VG_(printf)("   libhb: %'13llu msmcwrite (%'llu dragovers)\n",
                  stats__msmcwrite, stats__msmcwrite_change);
      VG_(printf)("   libhb: %'13llu cmpLEQ queries (%'llu misses)\n",
                  stats__cmpLEQ_queries, stats__cmpLEQ_misses);
      VG_(printf)("   libhb: %'13llu join2  queries (%'llu misses)\n",
                  stats__join2_queries, stats__join2_misses);

      VG_(printf)("%s","\n");
      VG_(printf)("   libhb: VTSops: tick %'lu,  join %'lu,  cmpLEQ %'lu\n",
                  stats__vts__tick, stats__vts__join,  stats__vts__cmpLEQ );
      VG_(printf)("   libhb: VTSops: cmp_structural %'lu (%'lu slow)\n",
                  stats__vts__cmp_structural, stats__vts__cmp_structural_slow);
      VG_(printf)("   libhb: VTSset: find__or__clone_and_add %'lu"
                  " (%'lu allocd)\n",
                   stats__vts_set__focaa, stats__vts_set__focaa_a );
      VG_(printf)( "   libhb: VTSops: indexAt_SLOW %'lu\n",
                   stats__vts__indexat_slow );

      VG_(printf)("%s","\n");
      VG_(printf)(
         "   libhb: %ld entries in vts_table (approximately %lu bytes)\n",
         VG_(sizeXA)( vts_tab ), VG_(sizeXA)( vts_tab ) * sizeof(VtsTE)
      );
      VG_(printf)("   libhb: #%lu vts_tab GC    #%lu vts pruning\n",
                  stats__vts_tab_GC, stats__vts_pruning);
      VG_(printf)( "   libhb: %lu entries in vts_set\n",
                   VG_(sizeFM)( vts_set ) );

      VG_(printf)("%s","\n");
      {
         UInt live = 0;
         UInt llexit_done = 0;
         UInt joinedwith_done = 0;
         UInt llexit_and_joinedwith_done = 0;

         Thread* hgthread = get_admin_threads();
         tl_assert(hgthread);
         while (hgthread) {
            Thr* hbthr = hgthread->hbthr;
            tl_assert(hbthr);
            if (hbthr->llexit_done && hbthr->joinedwith_done)
               llexit_and_joinedwith_done++;
            else if (hbthr->llexit_done)
               llexit_done++;
            else if (hbthr->joinedwith_done)
               joinedwith_done++;
            else
               live++;
            hgthread = hgthread->admin;
         }
         VG_(printf)("   libhb: threads live: %u exit_and_joinedwith %u"
                     " exit %u joinedwith %u\n",
                     live, llexit_and_joinedwith_done,
                     llexit_done, joinedwith_done);
         VG_(printf)("   libhb: %d verydead_threads, "
                     "%d verydead_threads_not_pruned\n",
                     (int) VG_(sizeXA)( verydead_thread_table),
                     (int) VG_(sizeXA)( verydead_thread_table_not_pruned));
         tl_assert (VG_(sizeXA)( verydead_thread_table)
                    + VG_(sizeXA)( verydead_thread_table_not_pruned)
                    == llexit_and_joinedwith_done);
      }

      VG_(printf)("%s","\n");
      VG_(printf)( "   libhb: oldrefHTN %lu (%'d bytes)\n",
                   oldrefHTN, (int)(oldrefHTN * sizeof(OldRef)));
      tl_assert (oldrefHTN == VG_(HT_count_nodes) (oldrefHT));
      VG_(printf)( "   libhb: oldref lookup found=%lu notfound=%lu\n",
                   stats__evm__lookup_found, stats__evm__lookup_notfound);
      if (VG_(clo_verbosity) > 1)
         VG_(HT_print_stats) (oldrefHT, cmp_oldref_tsw);
      VG_(printf)( "   libhb: oldref bind tsw/rcec "
                   "==/==:%'lu ==/!=:%'lu !=/!=:%'lu\n",
                   stats__ctxt_eq_tsw_eq_rcec, stats__ctxt_eq_tsw_neq_rcec,
                   stats__ctxt_neq_tsw_neq_rcec);
      VG_(printf)( "   libhb: ctxt__rcdec calls %'lu. rcec gc discards %'lu\n",
                   stats__ctxt_rcdec_calls, stats__ctxt_rcec_gc_discards);
      VG_(printf)( "   libhb: contextTab: %lu slots,"
                   " %lu cur ents(ref'd %lu),"
                   " %lu max ents\n",
                   (UWord)N_RCEC_TAB,
                   stats__ctxt_tab_curr, RCEC_referenced,
                   stats__ctxt_tab_max );
      VG_(printf) ("   libhb: stats__cached_rcec "
                   "identical %'lu updated %'lu fresh %'lu\n",
                   stats__cached_rcec_identical, stats__cached_rcec_updated,
                   stats__cached_rcec_fresh);
      if (stats__cached_rcec_diff > 0)
         VG_(printf) ("   libhb: stats__cached_rcec diff unk reason%'lu\n",
                      stats__cached_rcec_diff);
      if (stats__cached_rcec_diff_known_reason > 0)
         VG_(printf) ("   libhb: stats__cached_rcec diff known reason %'lu\n",
                      stats__cached_rcec_diff_known_reason);

      {
#        define  MAXCHAIN 10
         UInt chains[MAXCHAIN+1]; // [MAXCHAIN] gets all chains >= MAXCHAIN
         UInt non0chain = 0;
         UInt n;
         UInt i;
         RCEC *p;

         for (i = 0; i <= MAXCHAIN; i++) chains[i] = 0;
         for (i = 0; i < N_RCEC_TAB; i++) {
            n = 0;
            for (p = contextTab[i]; p; p = p->next)
               n++;
            if (n < MAXCHAIN)
               chains[n]++;
            else
               chains[MAXCHAIN]++;
            if (n > 0)
               non0chain++;
         }
         VG_(printf)( "   libhb: contextTab chain of [length]=nchain."
                      " Avg chain len %3.1f\n"
                      "        ",
                      (Double)stats__ctxt_tab_curr
                      / (Double)(non0chain ? non0chain : 1));
         for (i = 0; i <= MAXCHAIN; i++) {
            if (chains[i] != 0)
                VG_(printf)( "[%u%s]=%u ",
                             i, i == MAXCHAIN ? "+" : "",
                             chains[i]);
         }
         VG_(printf)( "\n");
#        undef MAXCHAIN
      }
      VG_(printf)( "   libhb: contextTab: %lu queries, %lu cmps\n",
                   stats__ctxt_tab_qs,
                   stats__ctxt_tab_cmps );
#if 0
      VG_(printf)("sizeof(CacheLine)   = %zu\n", sizeof(CacheLine));
      VG_(printf)("sizeof(LineZ)       = %zu\n", sizeof(LineZ));
      VG_(printf)("sizeof(LineF)       = %zu\n", sizeof(LineF));
      VG_(printf)("sizeof(SecMap)      = %zu\n", sizeof(SecMap));
      VG_(printf)("sizeof(Cache)       = %zu\n", sizeof(Cache));
      VG_(printf)("sizeof(SMCacheEnt)  = %zu\n", sizeof(SMCacheEnt));
      VG_(printf)("sizeof(CountedSVal) = %zu\n", sizeof(CountedSVal));
      VG_(printf)("sizeof(VTS)         = %zu\n", sizeof(VTS));
      VG_(printf)("sizeof(ScalarTS)    = %zu\n", sizeof(ScalarTS));
      VG_(printf)("sizeof(VtsTE)       = %zu\n", sizeof(VtsTE));

      VG_(printf)("sizeof(struct _Thr)     = %zu\n", sizeof(struct _Thr));
      VG_(printf)("sizeof(RCEC)     = %zu\n", sizeof(RCEC));
      VG_(printf)("sizeof(struct _SO)     = %zu\n", sizeof(struct _SO));
#endif

      VG_(printf)("%s","<<< END libhb stats >>>\n");
      VG_(printf)("%s","\n");

   }
}

/* Receive notification that a thread has low level exited.  The
   significance here is that we do not expect to see any more memory
   references from it. */
void libhb_async_exit ( Thr* thr )
{
   tl_assert(thr);
   tl_assert(!thr->llexit_done);
   thr->llexit_done = True;

   /* Check nobody messed up with the cached_rcec */
   tl_assert (thr->cached_rcec.magic == RCEC_MAGIC);
   tl_assert (thr->cached_rcec.rc == 0);
   tl_assert (thr->cached_rcec.rcX == 0);
   tl_assert (thr->cached_rcec.next == NULL);

   /* Just to be sure, declare the cached stack invalid. */
   set_cached_rcec_validity(thr, False);

   /* free up Filter and local_Kws_n_stacks (well, actually not the
      latter ..) */
   tl_assert(thr->filter);
   HG_(free)(thr->filter);
   thr->filter = NULL;

   /* Tell the VTS mechanism this thread has exited, so it can
      participate in VTS pruning.  Note this can only happen if the
      thread has both ll_exited and has been joined with. */
   if (thr->joinedwith_done)
      VTS__declare_thread_very_dead(thr);

   /* Another space-accuracy tradeoff.  Do we want to be able to show
      H1 history for conflicts in threads which have since exited?  If
      yes, then we better not free up thr->local_Kws_n_stacks.  The
      downside is a potential per-thread leak of up to
      N_KWs_N_STACKs_PER_THREAD * sizeof(ULong_n_EC) * whatever the
      XArray average overcommit factor is (1.5 I'd guess). */
   // hence:
   // VG_(deleteXA)(thr->local_Kws_n_stacks);
   // thr->local_Kws_n_stacks = NULL;
}

/* Receive notification that a thread has been joined with.  The
   significance here is that we do not expect to see any further
   references to its vector clocks (Thr::viR and Thr::viW). */
void libhb_joinedwith_done ( Thr* thr )
{
   tl_assert(thr);
   /* Caller must ensure that this is only ever called once per Thr. */
   tl_assert(!thr->joinedwith_done);
   thr->joinedwith_done = True;
   if (thr->llexit_done)
      VTS__declare_thread_very_dead(thr);
}


/* Both Segs and SOs point to VTSs.  However, there is no sharing, so
   a Seg that points at a VTS is its one-and-only owner, and ditto for
   a SO that points at a VTS. */

SO* libhb_so_alloc ( void )
{
   return SO__Alloc();
}

void libhb_so_dealloc ( SO* so )
{
   tl_assert(so);
   tl_assert(so->magic == SO_MAGIC);
   SO__Dealloc(so);
}

/* See comments in libhb.h for details on the meaning of 
   strong vs weak sends and strong vs weak receives. */
void libhb_so_send ( Thr* thr, SO* so, Bool strong_send )
{
   /* Copy the VTSs from 'thr' into the sync object, and then move
      the thread along one step. */

   tl_assert(so);
   tl_assert(so->magic == SO_MAGIC);

   /* stay sane .. a thread's read-clock must always lead or be the
      same as its write-clock */
   { Bool leq = VtsID__cmpLEQ(thr->viW, thr->viR);
     tl_assert(leq);
   }

   /* since we're overwriting the VtsIDs in the SO, we need to drop
      any references made by the previous contents thereof */
   if (so->viR == VtsID_INVALID) {
      tl_assert(so->viW == VtsID_INVALID);
      so->viR = thr->viR;
      so->viW = thr->viW;
      VtsID__rcinc(so->viR);
      VtsID__rcinc(so->viW);
   } else {
      /* In a strong send, we dump any previous VC in the SO and
         install the sending thread's VC instead.  For a weak send we
         must join2 with what's already there. */
      tl_assert(so->viW != VtsID_INVALID);
      VtsID__rcdec(so->viR);
      VtsID__rcdec(so->viW);
      so->viR = strong_send ? thr->viR : VtsID__join2( so->viR, thr->viR );
      so->viW = strong_send ? thr->viW : VtsID__join2( so->viW, thr->viW );
      VtsID__rcinc(so->viR);
      VtsID__rcinc(so->viW);
   }

   /* move both parent clocks along */
   VtsID__rcdec(thr->viR);
   VtsID__rcdec(thr->viW);
   thr->viR = VtsID__tick( thr->viR, thr );
   thr->viW = VtsID__tick( thr->viW, thr );
   if (!thr->llexit_done) {
      Filter__clear(thr->filter, "libhb_so_send");
      note_local_Kw_n_stack_for(thr);
   }
   VtsID__rcinc(thr->viR);
   VtsID__rcinc(thr->viW);

   if (strong_send)
      show_thread_state("s-send", thr);
   else
      show_thread_state("w-send", thr);
}

void libhb_so_recv ( Thr* thr, SO* so, Bool strong_recv )
{
   tl_assert(so);
   tl_assert(so->magic == SO_MAGIC);

   if (so->viR != VtsID_INVALID) {
      tl_assert(so->viW != VtsID_INVALID);

      /* Weak receive (basically, an R-acquisition of a R-W lock).
         This advances the read-clock of the receiver, but not the
         write-clock. */
      VtsID__rcdec(thr->viR);
      thr->viR = VtsID__join2( thr->viR, so->viR );
      VtsID__rcinc(thr->viR);

      /* At one point (r10589) it seemed safest to tick the clocks for
         the receiving thread after the join.  But on reflection, I
         wonder if that might cause it to 'overtake' constraints,
         which could lead to missing races.  So, back out that part of
         r10589. */
      //VtsID__rcdec(thr->viR);
      //thr->viR = VtsID__tick( thr->viR, thr );
      //VtsID__rcinc(thr->viR);

      /* For a strong receive, we also advance the receiver's write
         clock, which means the receive as a whole is essentially
         equivalent to a W-acquisition of a R-W lock. */
      if (strong_recv) {
         VtsID__rcdec(thr->viW);
         thr->viW = VtsID__join2( thr->viW, so->viW );
         VtsID__rcinc(thr->viW);

         /* See comment just above, re r10589. */
         //VtsID__rcdec(thr->viW);
         //thr->viW = VtsID__tick( thr->viW, thr );
         //VtsID__rcinc(thr->viW);
      }

      if (thr->filter)
         Filter__clear(thr->filter, "libhb_so_recv");
      note_local_Kw_n_stack_for(thr);

      if (strong_recv) 
         show_thread_state("s-recv", thr);
      else 
         show_thread_state("w-recv", thr);

   } else {
      tl_assert(so->viW == VtsID_INVALID);
      /* Deal with degenerate case: 'so' has no vts, so there has been
         no message posted to it.  Just ignore this case. */
      show_thread_state("d-recv", thr);
   }
}

Bool libhb_so_everSent ( SO* so )
{
   if (so->viR == VtsID_INVALID) {
      tl_assert(so->viW == VtsID_INVALID);
      return False;
   } else {
      tl_assert(so->viW != VtsID_INVALID);
      return True;
   }
}

#define XXX1 0 // 0x67a106c
#define XXX2 0

static inline Bool TRACEME(Addr a, SizeT szB) {
   if (XXX1 && a <= XXX1 && XXX1 <= a+szB) return True;
   if (XXX2 && a <= XXX2 && XXX2 <= a+szB) return True;
   return False;
}
static void trace ( Thr* thr, Addr a, SizeT szB, const HChar* s ) 
{
  SVal sv = zsm_sread08(a);
  VG_(printf)("thr %p (%#lx,%lu) %s: 0x%016llx ", thr,a,szB,s,sv);
  show_thread_state("", thr);
  VG_(printf)("%s","\n");
}

void libhb_srange_new ( Thr* thr, Addr a, SizeT szB )
{
   SVal sv = SVal__mkC(thr->viW, thr->viW);
   tl_assert(is_sane_SVal_C(sv));
   if (0 && TRACEME(a,szB)) trace(thr,a,szB,"nw-before");
   zsm_sset_range( a, szB, sv );
   Filter__clear_range( thr->filter, a, szB );
   if (0 && TRACEME(a,szB)) trace(thr,a,szB,"nw-after ");
}

void libhb_srange_noaccess_NoFX ( Thr* thr, Addr a, SizeT szB )
{
   /* do nothing */
}


/* Set the lines zix_start till zix_end to NOACCESS. */
static void zsm_secmap_line_range_noaccess (SecMap *sm,
                                            UInt zix_start, UInt zix_end)
{
   for (UInt lz = zix_start; lz <= zix_end; lz++) {
      LineZ* lineZ;
      lineZ = &sm->linesZ[lz];
      if (lineZ->dict[0] != SVal_INVALID) {
         rcdec_LineZ(lineZ);
         lineZ->dict[0] = SVal_NOACCESS;
         lineZ->dict[1] = lineZ->dict[2] = lineZ->dict[3] = SVal_INVALID;
      } else {
         clear_LineF_of_Z(lineZ);
      }
      for (UInt i = 0; i < N_LINE_ARANGE/4; i++)
         lineZ->ix2s[i] = 0; /* all refer to dict[0] */
   }
}

/* Set the given range to SVal_NOACCESS in-place in the secmap.
   a must be cacheline aligned. len must be a multiple of a cacheline
   and must be < N_SECMAP_ARANGE. */
static void zsm_sset_range_noaccess_in_secmap(Addr a, SizeT len)
{
   tl_assert (is_valid_scache_tag (a));
   tl_assert (0 == (len & (N_LINE_ARANGE - 1)));
   tl_assert (len < N_SECMAP_ARANGE);

   SecMap *sm1 = shmem__find_SecMap (a);
   SecMap *sm2 = shmem__find_SecMap (a + len - 1);
   UWord zix_start = shmem__get_SecMap_offset(a          ) >> N_LINE_BITS;
   UWord zix_end   = shmem__get_SecMap_offset(a + len - 1) >> N_LINE_BITS;

   if (sm1) {
      if (CHECK_ZSM) tl_assert(is_sane_SecMap(sm1));
      zsm_secmap_line_range_noaccess (sm1, zix_start,
                                      sm1 == sm2 ? zix_end : N_SECMAP_ZLINES-1);
   }
   if (sm2 && sm1 != sm2) {
      if (CHECK_ZSM) tl_assert(is_sane_SecMap(sm2));
      zsm_secmap_line_range_noaccess (sm2, 0, zix_end);
   }
}

/* Set the given address range to SVal_NOACCESS.
   The SecMaps fully set to SVal_NOACCESS will be pushed in SecMap_freelist. */
static void zsm_sset_range_noaccess (Addr addr, SizeT len)
{
   /*
       BPC = Before, Partial Cacheline, = addr 
             (i.e. starting inside a cacheline/inside a SecMap)
       BFC = Before, Full Cacheline(s), but not full SecMap
             (i.e. starting inside a SecMap)
       FSM = Full SecMap(s)
             (i.e. starting a SecMap)
       AFC = After, Full Cacheline(s), but not full SecMap
             (i.e. first address after the full SecMap(s))
       APC = After, Partial Cacheline, i.e. first address after the
             full CacheLines).
       ARE = After Range End = addr+len = first address not part of the range.

       If addr     starts a Cacheline, then BPC == BFC.
       If addr     starts a SecMap,    then BPC == BFC == FSM.
       If addr+len starts a SecMap,    then APC == ARE == AFC
       If addr+len starts a Cacheline, then APC == ARE
   */
   Addr ARE = addr + len;
   Addr BPC = addr;
   Addr BFC = ROUNDUP(BPC, N_LINE_ARANGE);
   Addr FSM = ROUNDUP(BPC, N_SECMAP_ARANGE);
   Addr AFC = ROUNDDN(ARE, N_SECMAP_ARANGE);
   Addr APC = ROUNDDN(ARE, N_LINE_ARANGE);
   SizeT Plen = len; // Plen will be split between the following:
   SizeT BPClen;
   SizeT BFClen;
   SizeT FSMlen;
   SizeT AFClen;
   SizeT APClen;

   /* Consumes from Plen the nr of bytes between from and to.
      from and to must be aligned on a multiple of round.
      The length consumed will be a multiple of round, with
      a maximum of Plen. */
#  define PlenCONSUME(from, to, round, consumed) \
   do {                                          \
   if (from < to) {                              \
      if (to - from < Plen)                      \
         consumed = to - from;                   \
      else                                       \
         consumed = ROUNDDN(Plen, round);        \
   } else {                                      \
      consumed = 0;                              \
   }                                             \
   Plen -= consumed; } while (0)
      
   PlenCONSUME(BPC, BFC, 1,               BPClen);
   PlenCONSUME(BFC, FSM, N_LINE_ARANGE,   BFClen);
   PlenCONSUME(FSM, AFC, N_SECMAP_ARANGE, FSMlen);
   PlenCONSUME(AFC, APC, N_LINE_ARANGE,   AFClen);
   PlenCONSUME(APC, ARE, 1,               APClen);

   if (0)
      VG_(printf) ("addr %p[%lu] ARE %p"
                   " BPC %p[%lu] BFC %p[%lu] FSM %p[%lu]"
                   " AFC %p[%lu] APC %p[%lu]\n",
                   (void*)addr, len, (void*)ARE,
                   (void*)BPC, BPClen, (void*)BFC, BFClen, (void*)FSM, FSMlen,
                   (void*)AFC, AFClen, (void*)APC, APClen);

   tl_assert (Plen == 0);

   /* Set to NOACCESS pieces before and after not covered by entire SecMaps. */

   /* First we set the partial cachelines. This is done through the cache. */
   if (BPClen > 0)
      zsm_sset_range_SMALL (BPC, BPClen, SVal_NOACCESS);
   if (APClen > 0)
      zsm_sset_range_SMALL (APC, APClen, SVal_NOACCESS);

   /* After this, we will not use the cache anymore. We will directly work
      in-place on the z shadow memory in SecMap(s).
      So, we invalidate the cachelines for the whole range we are setting
      to NOACCESS below. */
   shmem__invalidate_scache_range (BFC, APC - BFC);

   if (BFClen > 0)
      zsm_sset_range_noaccess_in_secmap (BFC, BFClen);
   if (AFClen > 0)
      zsm_sset_range_noaccess_in_secmap (AFC, AFClen);

   if (FSMlen > 0) {
      /* Set to NOACCESS all the SecMaps, pushing the SecMaps to the
         free list. */
      Addr  sm_start = FSM;
      while (sm_start < AFC) {
         SecMap *sm = shmem__find_SecMap (sm_start);
         if (sm) {
            Addr gaKey;
            SecMap *fm_sm;

            if (CHECK_ZSM) tl_assert(is_sane_SecMap(sm));
            for (UInt lz = 0; lz < N_SECMAP_ZLINES; lz++) {
               LineZ *lineZ = &sm->linesZ[lz];
               if (LIKELY(lineZ->dict[0] != SVal_INVALID))
                  rcdec_LineZ(lineZ);
               else
                  clear_LineF_of_Z(lineZ);
            }
            if (!VG_(delFromFM)(map_shmem, &gaKey, (UWord*)&fm_sm, sm_start))
               tl_assert (0);
            stats__secmaps_in_map_shmem--;
            tl_assert (gaKey == sm_start);
            tl_assert (sm == fm_sm);
            stats__secmaps_ssetGCed++;
            push_SecMap_on_freelist (sm);
         }
         sm_start += N_SECMAP_ARANGE;
      }
      tl_assert (sm_start == AFC);

      /* The above loop might have kept copies of freed SecMap in the smCache.
         => clear them. */
      if (address_in_range(smCache[0].gaKey, FSM, FSMlen)) {
         smCache[0].gaKey = 1;
         smCache[0].sm = NULL;
      }
      if (address_in_range(smCache[1].gaKey, FSM, FSMlen)) {
         smCache[1].gaKey = 1;
         smCache[1].sm = NULL;
      }
      if (address_in_range(smCache[2].gaKey, FSM, FSMlen)) {
         smCache[2].gaKey = 1;
         smCache[2].sm = NULL;
      }
      STATIC_ASSERT (3 == sizeof(smCache)/sizeof(SMCacheEnt));
   }
}

void libhb_srange_noaccess_AHAE ( Thr* thr, Addr a, SizeT szB )
{
   /* This really does put the requested range in NoAccess.  It's
      expensive though. */
   SVal sv = SVal_NOACCESS;
   tl_assert(is_sane_SVal_C(sv));
   if (LIKELY(szB < 2 * N_LINE_ARANGE))
      zsm_sset_range_SMALL (a, szB, SVal_NOACCESS);
   else
      zsm_sset_range_noaccess (a, szB);
   Filter__clear_range( thr->filter, a, szB );
}

/* Works byte at a time. Can be optimised if needed. */
UWord libhb_srange_get_abits (Addr a, UChar *abits, SizeT len)
{
   UWord anr = 0; // nr of bytes addressable.

   /* Get the accessibility of each byte. Pay attention to not
      create SecMap or LineZ when checking if a byte is addressable.

      Note: this is used for client request. Performance deemed not critical.
      So for simplicity, we work byte per byte.
      Performance could be improved  by working with full cachelines
      or with full SecMap, when reaching a cacheline or secmap boundary. */
   for (SizeT i = 0; i < len; i++) {
      SVal       sv = SVal_INVALID;
      Addr       b = a + i;
      Addr       tag = b & ~(N_LINE_ARANGE - 1);
      UWord      wix = (b >> N_LINE_BITS) & (N_WAY_NENT - 1);
      UWord      cloff = get_cacheline_offset(b);

      /* Note: we do not use get_cacheline(b) to avoid creating cachelines
         and/or SecMap for non addressable bytes. */
      if (tag == cache_shmem.tags0[wix]) {
         CacheLine copy = cache_shmem.lyns0[wix];
         /* We work on a copy of the cacheline, as we do not want to
            record the client request as a real read.
            The below is somewhat similar to zsm_sapply08__msmcread but
            avoids side effects on the cache. */
         UWord toff = get_tree_offset(b); /* == 0 .. 7 */
         UWord tno  = get_treeno(b);
         UShort descr = copy.descrs[tno];
         if (UNLIKELY( !(descr & (TREE_DESCR_8_0 << toff)) )) {
            SVal* tree = &copy.svals[tno << 3];
            copy.descrs[tno] = pulldown_to_8(tree, toff, descr);
         }
         sv = copy.svals[cloff];
      } else {
         /* Byte not found in the cacheline. Search for a SecMap. */
         SecMap *sm = shmem__find_SecMap(b);
         LineZ *lineZ;
         if (sm == NULL)
            sv = SVal_NOACCESS;
         else {
            UWord zix = shmem__get_SecMap_offset(b) >> N_LINE_BITS;
            lineZ = &sm->linesZ[zix];
            if (lineZ->dict[0] == SVal_INVALID) {
               LineF *lineF = SVal2Ptr(lineZ->dict[1]);
               sv = lineF->w64s[cloff];
            } else {
               UWord ix = read_twobit_array( lineZ->ix2s, cloff );
               sv = lineZ->dict[ix];
            }
         }
      }
      
      tl_assert (sv != SVal_INVALID);
      if (sv == SVal_NOACCESS) {
         if (abits)
            abits[i] = 0x00;
      } else {
         if (abits)
            abits[i] = 0xff;
         anr++;
      }
   }

   return anr;
}


void libhb_srange_untrack ( Thr* thr, Addr a, SizeT szB )
{
   SVal sv = SVal_NOACCESS;
   tl_assert(is_sane_SVal_C(sv));
   if (0 && TRACEME(a,szB)) trace(thr,a,szB,"untrack-before");
   if (LIKELY(szB < 2 * N_LINE_ARANGE))
      zsm_sset_range_SMALL (a, szB, SVal_NOACCESS);
   else
      zsm_sset_range_noaccess (a, szB);
   Filter__clear_range( thr->filter, a, szB );
   if (0 && TRACEME(a,szB)) trace(thr,a,szB,"untrack-after ");
}

Thread* libhb_get_Thr_hgthread ( Thr* thr ) {
   tl_assert(thr);
   return thr->hgthread;
}

void libhb_set_Thr_hgthread ( Thr* thr, Thread* hgthread ) {
   tl_assert(thr);
   thr->hgthread = hgthread;
}

void libhb_copy_shadow_state ( Thr* thr, Addr src, Addr dst, SizeT len )
{
   zsm_scopy_range(src, dst, len);
   Filter__clear_range( thr->filter, dst, len ); 
}

void libhb_maybe_GC ( void )
{
   /* GC the unreferenced (zero rc) RCECs when
         (1) reaching a significant nr of RCECs (to avoid scanning a contextTab
             with mostly NULL ptr)
     and (2) approaching the max nr of RCEC (as we have in any case
             at least that amount of RCEC in the pool allocator)
             Note: the margin allows to avoid a small but constant increase
             of the max nr of RCEC due to the fact that libhb_maybe_GC is
             not called when the current nr of RCEC exactly reaches the max.
     and (3) the nr of referenced RCECs is less than 75% than total nr RCECs.
     Avoid growing too much the nr of RCEC keeps the memory use low,
     and avoids to have too many elements in the (fixed) contextTab hashtable.
   */
   if (UNLIKELY(stats__ctxt_tab_curr > N_RCEC_TAB/2
                && stats__ctxt_tab_curr + 1000 >= stats__ctxt_tab_max
                && (stats__ctxt_tab_curr * 3)/4 > RCEC_referenced))
      do_RCEC_GC();

   /* If there are still no entries available (all the table entries are full),
      and we hit the threshold point, then do a GC */
   Bool vts_tab_GC = vts_tab_freelist == VtsID_INVALID
      && VG_(sizeXA)( vts_tab ) >= vts_next_GC_at;
   if (UNLIKELY (vts_tab_GC))
      vts_tab__do_GC( False/*don't show stats*/ );

   /* scan GC the SecMaps when
          (1) no SecMap in the freelist
      and (2) the current nr of live secmaps exceeds the threshold. */
   if (UNLIKELY(SecMap_freelist == NULL
                && stats__secmaps_in_map_shmem >= next_SecMap_GC_at)) {
      // If we did a vts tab GC, then no need to flush the cache again.
      if (!vts_tab_GC)
         zsm_flush_cache();
      shmem__SecMap_do_GC(True);
   }

   /* Check the reference counts (expensive) */
   if (CHECK_CEM)
      event_map__check_reference_counts();
}


/////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////
//                                                             //
// SECTION END main library                                    //
//                                                             //
/////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////

/*--------------------------------------------------------------------*/
/*--- end                                             libhb_main.c ---*/
/*--------------------------------------------------------------------*/
