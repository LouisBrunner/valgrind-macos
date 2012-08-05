
/*--------------------------------------------------------------------*/
/*--- Management of the translation table and cache.               ---*/
/*---                                                 m_transtab.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2012 Julian Seward 
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
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307, USA.

   The GNU General Public License is contained in the file COPYING.
*/

#include "pub_core_basics.h"
#include "pub_core_debuglog.h"
#include "pub_core_machine.h"    // For VG_(machine_get_VexArchInfo)
#include "pub_core_libcbase.h"
#include "pub_core_vki.h"        // to keep pub_core_libproc.h happy, sigh
#include "pub_core_libcproc.h"   // VG_(invalidate_icache)
#include "pub_core_libcassert.h"
#include "pub_core_libcprint.h"
#include "pub_core_options.h"
#include "pub_core_tooliface.h"  // For VG_(details).avg_translation_sizeB
#include "pub_core_transtab.h"
#include "pub_core_aspacemgr.h"
#include "pub_core_mallocfree.h" // VG_(out_of_memory_NORETURN)
#include "pub_core_xarray.h"
#include "pub_core_dispatch.h"   // For VG_(disp_cp*) addresses


/* #define DEBUG_TRANSTAB */


/*-------------------------------------------------------------*/
/*--- Management of the FIFO-based translation table+cache. ---*/
/*-------------------------------------------------------------*/

/*------------------ CONSTANTS ------------------*/

/* Number of sectors the TC is divided into.  If you need a larger
   overall translation cache, increase this value. */
#define N_SECTORS 8

/* Number of TC entries in each sector.  This needs to be a prime
   number to work properly, it must be <= 65535 (so that a TT index
   fits in a UShort, leaving room for 0xFFFF(EC2TTE_DELETED) to denote
   'deleted') and it is strongly recommended not to change this.
   65521 is the largest prime <= 65535. */
#define N_TTES_PER_SECTOR /*10007*/ /*30011*/ /*40009*/ 65521

/* Because each sector contains a hash table of TTEntries, we need to
   specify the maximum allowable loading, after which the sector is
   deemed full. */
#define SECTOR_TT_LIMIT_PERCENT 65

/* The sector is deemed full when this many entries are in it. */
#define N_TTES_PER_SECTOR_USABLE \
           ((N_TTES_PER_SECTOR * SECTOR_TT_LIMIT_PERCENT) / 100)

/* Equivalence classes for fast address range deletion.  There are 1 +
   2^ECLASS_WIDTH bins.  The highest one, ECLASS_MISC, describes an
   address range which does not fall cleanly within any specific bin.
   Note that ECLASS_SHIFT + ECLASS_WIDTH must be < 32. */
#define ECLASS_SHIFT 11
#define ECLASS_WIDTH 8
#define ECLASS_MISC  (1 << ECLASS_WIDTH)
#define ECLASS_N     (1 + ECLASS_MISC)

#define EC2TTE_DELETED  0xFFFF /* 16-bit special value */


/*------------------ TYPES ------------------*/

/* In edges ("to-me") in the graph created by chaining. */
typedef
   struct {
      UInt from_sNo;   /* sector number */
      UInt from_tteNo; /* TTE number in given sector */
      UInt from_offs;  /* code offset from TCEntry::tcptr where the patch is */
      Bool to_fastEP;  /* Is the patch to a fast or slow entry point? */
   }
   InEdge;


/* Out edges ("from-me") in the graph created by chaining. */
typedef
   struct {
      UInt to_sNo;    /* sector number */
      UInt to_tteNo;  /* TTE number in given sector */
      UInt from_offs; /* code offset in owning translation where patch is */
   }
   OutEdge;


#define N_FIXED_IN_EDGE_ARR 3
typedef
   struct {
      UInt     n_fixed; /* 0 .. N_FIXED_IN_EDGE_ARR */
      InEdge   fixed[N_FIXED_IN_EDGE_ARR];
      XArray*  var; /* XArray* of InEdgeArr */
   }
   InEdgeArr;

#define N_FIXED_OUT_EDGE_ARR 2
typedef
   struct {
      UInt    n_fixed; /* 0 .. N_FIXED_OUT_EDGE_ARR */
      OutEdge fixed[N_FIXED_OUT_EDGE_ARR];
      XArray* var; /* XArray* of OutEdgeArr */
   }
   OutEdgeArr;


/* A translation-table entry.  This indicates precisely which areas of
   guest code are included in the translation, and contains all other
   auxiliary info too.  */
typedef
   struct {
      /* Profiling only: the count and weight (arbitrary meaning) for
         this translation.  Weight is a property of the translation
         itself and computed once when the translation is created.
         Count is an entry count for the translation and is
         incremented by 1 every time the translation is used, if we
         are profiling. */
      ULong    count;
      UShort   weight;

      /* Status of the slot.  Note, we need to be able to do lazy
         deletion, hence the Deleted state. */
      enum { InUse, Deleted, Empty } status;

      /* 64-bit aligned pointer to one or more 64-bit words containing
         the corresponding host code (must be in the same sector!)
         This is a pointer into the sector's tc (code) area. */
      ULong* tcptr;

      /* This is the original guest address that purportedly is the
         entry point of the translation.  You might think that .entry
         should be the same as .vge->base[0], and most of the time it
         is.  However, when doing redirections, that is not the case.
         .vge must always correctly describe the guest code sections
         from which this translation was made.  However, .entry may or
         may not be a lie, depending on whether or not we're doing
         redirection. */
      Addr64 entry;

      /* This structure describes precisely what ranges of guest code
         the translation covers, so we can decide whether or not to
         delete it when translations of a given address range are
         invalidated. */
      VexGuestExtents vge;

      /* Address range summary info: these are pointers back to
         eclass[] entries in the containing Sector.  Those entries in
         turn point back here -- the two structures are mutually
         redundant but both necessary to make fast deletions work.
         The eclass info is similar to, and derived from, this entry's
         'vge' field, but it is not the same */
      UShort n_tte2ec;      // # tte2ec pointers (1 to 3)
      UShort tte2ec_ec[3];  // for each, the eclass #
      UInt   tte2ec_ix[3];  // and the index within the eclass.
      // for i in 0 .. n_tte2ec-1
      //    sec->ec2tte[ tte2ec_ec[i] ][ tte2ec_ix[i] ] 
      // should be the index 
      // of this TTEntry in the containing Sector's tt array.

      /* Admin information for chaining.  'in_edges' is a set of the
         patch points which jump to this translation -- hence are
         predecessors in the control flow graph.  'out_edges' points
         to successors in the control flow graph -- translations to
         which this one has a patched jump.  In short these are just
         backwards and forwards edges in the graph of patched-together
         blocks.  The 'in_edges' contain slightly more info, enough
         that we can undo the chaining of each mentioned patch point.
         The 'out_edges' list exists only so that we can visit the
         'in_edges' entries of all blocks we're patched through to, in
         order to remove ourselves from then when we're deleted. */

      /* A translation can disappear for two reasons:
          1. erased (as part of the oldest sector cleanup) when the
             youngest sector is full.
          2. discarded due to calls to VG_(discard_translations).
             VG_(discard_translations) sets the status of the
             translation to 'Deleted'.
             A.o., the gdbserver discards one or more translations
             when a breakpoint is inserted or removed at an Addr,
             or when single stepping mode is enabled/disabled
             or when a translation is instrumented for gdbserver
             (all the target jumps of this translation are
              invalidated).

         So, it is possible that the translation A to be patched
         (to obtain a patched jump from A to B) is invalidated
         after B is translated and before A is patched.
         In case a translation is erased or discarded, the patching
         cannot be done.  VG_(tt_tc_do_chaining) and find_TTEntry_from_hcode
         are checking the 'from' translation still exists before
         doing the patching.

         Is it safe to erase or discard the current translation E being 
         executed ? Amazing, but yes, it is safe.
         Here is the explanation:

         The translation E being executed can only be erased if a new
         translation N is being done. A new translation is done only
         if the host addr is a not yet patched jump to another
         translation. In such a case, the guest address of N is
         assigned to the PC in the VEX state. Control is returned
         to the scheduler. N will be translated. This can erase the
         translation E (in case of sector full). VG_(tt_tc_do_chaining)
         will not do the chaining to a non found translation E.
         The execution will continue at the current guest PC
         (i.e. the translation N).
         => it is safe to erase the current translation being executed.
         
         The current translation E being executed can also be discarded
         (e.g. by gdbserver). VG_(discard_translations) will mark
         this translation E as Deleted, but the translation itself
         is not erased. In particular, its host code can only
         be overwritten or erased in case a new translation is done.
         A new translation will only be done if a not yet translated
         jump is to be executed. The execution of the Deleted translation
         E will continue till a non patched jump is encountered.
         This situation is then similar to the 'erasing' case above :
         the current translation E can be erased or overwritten, as the
         execution will continue at the new translation N.

      */

      /* It is possible, although very unlikely, that a block A has
         more than one patched jump to block B.  This could happen if
         (eg) A finishes "jcond B; jmp B".

         This means in turn that B's in_edges set can list A more than
         once (twice in this example).  However, each such entry must
         have a different from_offs, since a patched jump can only
         jump to one place at once (it's meaningless for it to have
         multiple destinations.)  IOW, the successor and predecessor
         edges in the graph are not uniquely determined by a 
         TTEntry --> TTEntry pair, but rather by a 
         (TTEntry,offset) --> TTEntry triple.

         If A has multiple edges to B then B will mention A multiple
         times in its in_edges.  To make things simpler, we then
         require that A mentions B exactly the same number of times in
         its out_edges.  Furthermore, a matching out-in pair must have
         the same offset (from_offs).  This facilitates sanity
         checking, and it facilitates establishing the invariant that
         a out_edges set may not have duplicates when using the
         equality defined by (TTEntry,offset).  Hence the out_edges
         and in_edges sets really do have both have set semantics.

         eg if  A has been patched to B at offsets 42 and 87 (in A)
         then   A.out_edges = { (B,42), (B,87) }   (in any order)
         and    B.in_edges  = { (A,42), (A,87) }   (in any order)

         Hence for each node pair P->Q in the graph, there's a 1:1
         mapping between P.out_edges and Q.in_edges.
      */
      InEdgeArr  in_edges;
      OutEdgeArr out_edges;
   }
   TTEntry;


/* A structure used for mapping host code addresses back to the
   relevant TTEntry.  Used when doing chaining, for finding the
   TTEntry to which some arbitrary patch address belongs. */
typedef
   struct {
      UChar* start;
      UInt   len;
      UInt   tteNo;
   }
   HostExtent;

/* Finally, a sector itself.  Each sector contains an array of
   TCEntries, which hold code, and an array of TTEntries, containing
   all required administrative info.  Profiling is supported using the
   TTEntry .count and .weight fields, if required.

   If the sector is not in use, all three pointers are NULL and
   tt_n_inuse is zero.  
*/
typedef
   struct {
      /* The TCEntry area.  Size of this depends on the average
         translation size.  We try and size it so it becomes full
         precisely when this sector's translation table (tt) reaches
         its load limit (SECTOR_TT_LIMIT_PERCENT). */
      ULong* tc;

      /* The TTEntry array.  This is a fixed size, always containing
         exactly N_TTES_PER_SECTOR entries. */
      TTEntry* tt;

      /* This points to the current allocation point in tc. */
      ULong* tc_next;

      /* The count of tt entries with state InUse. */
      Int tt_n_inuse;

      /* Expandable arrays of tt indices for each of the ECLASS_N
         address range equivalence classes.  These hold indices into
         the containing sector's tt array, which in turn should point
         back here. */
      Int     ec2tte_size[ECLASS_N];
      Int     ec2tte_used[ECLASS_N];
      UShort* ec2tte[ECLASS_N];

      /* The host extents.  The [start, +len) ranges are constructed
         in strictly non-overlapping order, so we can binary search
         them at any time. */
      XArray* host_extents; /* XArray* of HostExtent */
   }
   Sector;


/*------------------ DECLS ------------------*/

/* The root data structure is an array of sectors.  The index of the
   youngest sector is recorded, and new translations are put into that
   sector.  When it fills up, we move along to the next sector and
   start to fill that up, wrapping around at the end of the array.
   That way, once all N_TC_SECTORS have been bought into use for the
   first time, and are full, we then re-use the oldest sector,
   endlessly. 

   When running, youngest sector should be between >= 0 and <
   N_TC_SECTORS.  The initial -1 value indicates the TT/TC system is
   not yet initialised. 
*/
static Sector sectors[N_SECTORS];
static Int    youngest_sector = -1;

/* The number of ULongs in each TCEntry area.  This is computed once
   at startup and does not change. */
static Int    tc_sector_szQ;


/* A list of sector numbers, in the order which they should be
   searched to find translations.  This is an optimisation to be used
   when searching for translations and should not affect
   correctness.  -1 denotes "no entry". */
static Int sector_search_order[N_SECTORS];


/* Fast helper for the TC.  A direct-mapped cache which holds a set of
   recently used (guest address, host address) pairs.  This array is
   referred to directly from m_dispatch/dispatch-<platform>.S.

   Entries in tt_fast may refer to any valid TC entry, regardless of
   which sector it's in.  Consequently we must be very careful to
   invalidate this cache when TC entries are changed or disappear.

   A special .guest address - TRANSTAB_BOGUS_GUEST_ADDR -- must be
   pointed at to cause that cache entry to miss.  This relies on the
   assumption that no guest code actually has that address, hence a
   value 0x1 seems good.  m_translate gives the client a synthetic
   segfault if it tries to execute at this address.
*/
/*
typedef
   struct { 
      Addr guest;
      Addr host;
   }
   FastCacheEntry;
*/
/*global*/ __attribute__((aligned(16)))
           FastCacheEntry VG_(tt_fast)[VG_TT_FAST_SIZE];

/* Make sure we're not used before initialisation. */
static Bool init_done = False;


/*------------------ STATS DECLS ------------------*/

/* Number of fast-cache updates and flushes done. */
static ULong n_fast_flushes = 0;
static ULong n_fast_updates = 0;

/* Number of full lookups done. */
static ULong n_full_lookups = 0;
static ULong n_lookup_probes = 0;

/* Number/osize/tsize of translations entered; also the number of
   those for which self-checking was requested. */
static ULong n_in_count    = 0;
static ULong n_in_osize    = 0;
static ULong n_in_tsize    = 0;
static ULong n_in_sc_count = 0;

/* Number/osize of translations discarded due to lack of space. */
static ULong n_dump_count = 0;
static ULong n_dump_osize = 0;

/* Number/osize of translations discarded due to requests to do so. */
static ULong n_disc_count = 0;
static ULong n_disc_osize = 0;


/*-------------------------------------------------------------*/
/*--- Misc                                                  ---*/
/*-------------------------------------------------------------*/

static void* ttaux_malloc ( HChar* tag, SizeT n )
{
   return VG_(arena_malloc)(VG_AR_TTAUX, tag, n);
}

static void ttaux_free ( void* p )
{
   VG_(arena_free)(VG_AR_TTAUX, p);
}


/*-------------------------------------------------------------*/
/*--- Chaining support                                      ---*/
/*-------------------------------------------------------------*/

static inline TTEntry* index_tte ( UInt sNo, UInt tteNo )
{
   vg_assert(sNo < N_SECTORS);
   vg_assert(tteNo < N_TTES_PER_SECTOR);
   Sector* s = &sectors[sNo];
   vg_assert(s->tt);
   TTEntry* tte = &s->tt[tteNo];
   vg_assert(tte->status == InUse);
   return tte;
}

static void InEdge__init ( InEdge* ie )
{
   ie->from_sNo   = -1; /* invalid */
   ie->from_tteNo = 0;
   ie->from_offs  = 0;
   ie->to_fastEP  = False;
}

static void OutEdge__init ( OutEdge* oe )
{
   oe->to_sNo    = -1; /* invalid */
   oe->to_tteNo  = 0;
   oe->from_offs = 0;
}

static void TTEntry__init ( TTEntry* tte )
{
   VG_(memset)(tte, 0, sizeof(*tte));
}

static UWord InEdgeArr__size ( InEdgeArr* iea )
{
   if (iea->var) {
      vg_assert(iea->n_fixed == 0);
      return VG_(sizeXA)(iea->var);
   } else {
      vg_assert(iea->n_fixed <= N_FIXED_IN_EDGE_ARR);
      return iea->n_fixed;
   }
}

static void InEdgeArr__makeEmpty ( InEdgeArr* iea )
{
   if (iea->var) {
      vg_assert(iea->n_fixed == 0);
      VG_(deleteXA)(iea->var);
      iea->var = NULL;
   } else {
      vg_assert(iea->n_fixed <= N_FIXED_IN_EDGE_ARR);
      iea->n_fixed = 0;
   }
}

static
InEdge* InEdgeArr__index ( InEdgeArr* iea, UWord i )
{
   if (iea->var) {
      vg_assert(iea->n_fixed == 0);
      return (InEdge*)VG_(indexXA)(iea->var, i);
   } else {
      vg_assert(i < iea->n_fixed);
      return &iea->fixed[i];
   }
}

static
void InEdgeArr__deleteIndex ( InEdgeArr* iea, UWord i )
{
   if (iea->var) {
      vg_assert(iea->n_fixed == 0);
      VG_(removeIndexXA)(iea->var, i);
   } else {
      vg_assert(i < iea->n_fixed);
      for (; i+1 < iea->n_fixed; i++) {
         iea->fixed[i] = iea->fixed[i+1];
      }
      iea->n_fixed--;
   }
}

static
void InEdgeArr__add ( InEdgeArr* iea, InEdge* ie )
{
   if (iea->var) {
      vg_assert(iea->n_fixed == 0);
      VG_(addToXA)(iea->var, ie);
   } else {
      vg_assert(iea->n_fixed <= N_FIXED_IN_EDGE_ARR);
      if (iea->n_fixed == N_FIXED_IN_EDGE_ARR) {
         /* The fixed array is full, so we have to initialise an
            XArray and copy the fixed array into it. */
         iea->var = VG_(newXA)(ttaux_malloc, "transtab.IEA__add",
                               ttaux_free,
                               sizeof(InEdge));
         UWord i;
         for (i = 0; i < iea->n_fixed; i++) {
            VG_(addToXA)(iea->var, &iea->fixed[i]);
         }
         VG_(addToXA)(iea->var, ie);
         iea->n_fixed = 0;
      } else {
         /* Just add to the fixed array. */
         iea->fixed[iea->n_fixed++] = *ie;
      }
   }
}

static UWord OutEdgeArr__size ( OutEdgeArr* oea )
{
   if (oea->var) {
      vg_assert(oea->n_fixed == 0);
      return VG_(sizeXA)(oea->var);
   } else {
      vg_assert(oea->n_fixed <= N_FIXED_OUT_EDGE_ARR);
      return oea->n_fixed;
   }
}

static void OutEdgeArr__makeEmpty ( OutEdgeArr* oea )
{
   if (oea->var) {
      vg_assert(oea->n_fixed == 0);
      VG_(deleteXA)(oea->var);
      oea->var = NULL;
   } else {
      vg_assert(oea->n_fixed <= N_FIXED_OUT_EDGE_ARR);
      oea->n_fixed = 0;
   }
}

static
OutEdge* OutEdgeArr__index ( OutEdgeArr* oea, UWord i )
{
   if (oea->var) {
      vg_assert(oea->n_fixed == 0);
      return (OutEdge*)VG_(indexXA)(oea->var, i);
   } else {
      vg_assert(i < oea->n_fixed);
      return &oea->fixed[i];
   }
}

static
void OutEdgeArr__deleteIndex ( OutEdgeArr* oea, UWord i )
{
   if (oea->var) {
      vg_assert(oea->n_fixed == 0);
      VG_(removeIndexXA)(oea->var, i);
   } else {
      vg_assert(i < oea->n_fixed);
      for (; i+1 < oea->n_fixed; i++) {
         oea->fixed[i] = oea->fixed[i+1];
      }
      oea->n_fixed--;
   }
}

static
void OutEdgeArr__add ( OutEdgeArr* oea, OutEdge* oe )
{
   if (oea->var) {
      vg_assert(oea->n_fixed == 0);
      VG_(addToXA)(oea->var, oe);
   } else {
      vg_assert(oea->n_fixed <= N_FIXED_OUT_EDGE_ARR);
      if (oea->n_fixed == N_FIXED_OUT_EDGE_ARR) {
         /* The fixed array is full, so we have to initialise an
            XArray and copy the fixed array into it. */
         oea->var = VG_(newXA)(ttaux_malloc, "transtab.OEA__add",
                               ttaux_free,
                               sizeof(OutEdge));
         UWord i;
         for (i = 0; i < oea->n_fixed; i++) {
            VG_(addToXA)(oea->var, &oea->fixed[i]);
         }
         VG_(addToXA)(oea->var, oe);
         oea->n_fixed = 0;
      } else {
         /* Just add to the fixed array. */
         oea->fixed[oea->n_fixed++] = *oe;
      }
   }
}

static
Int HostExtent__cmpOrd ( void* v1, void* v2 )
{
   HostExtent* hx1 = (HostExtent*)v1;
   HostExtent* hx2 = (HostExtent*)v2;
   if (hx1->start + hx1->len <= hx2->start) return -1;
   if (hx2->start + hx2->len <= hx1->start) return 1;
   return 0; /* partial overlap */
}

static __attribute__((noinline))
Bool find_TTEntry_from_hcode( /*OUT*/UInt* from_sNo,
                              /*OUT*/UInt* from_tteNo,
                              void* hcode )
{
   Int i;

   /* Search order logic copied from VG_(search_transtab). */
   for (i = 0; i < N_SECTORS; i++) {
      Int sno = sector_search_order[i];
      if (UNLIKELY(sno == -1))
         return False; /* run out of sectors to search */

      Sector* sec = &sectors[sno];
      XArray* /* of HostExtent */ host_extents = sec->host_extents;
      vg_assert(host_extents);

      HostExtent key;
      VG_(memset)(&key, 0, sizeof(key));
      key.start = hcode;
      key.len = 1;
      Word firstW = -1, lastW = -1;
      Bool found  = VG_(lookupXA_UNSAFE)(
                       host_extents, &key, &firstW, &lastW,
                       (Int(*)(void*,void*))HostExtent__cmpOrd
                    );
      vg_assert(firstW == lastW); // always true, even if not found
      if (found) {
         HostExtent* hx = VG_(indexXA)(host_extents, firstW);
         UInt tteNo = hx->tteNo;
         /* Do some additional sanity checks. */
         vg_assert(tteNo <= N_TTES_PER_SECTOR);
         /* Entry might have been invalidated. Consider this
            as not found. */
         if (sec->tt[tteNo].status == Deleted)
            return False;
         vg_assert(sec->tt[tteNo].status == InUse);
         /* Can only half check that the found TTEntry contains hcode,
            due to not having a length value for the hcode in the
            TTEntry. */
         vg_assert((UChar*)sec->tt[tteNo].tcptr <= (UChar*)hcode);
         /* Looks plausible */
         *from_sNo   = sno;
         *from_tteNo = (UInt)tteNo;
         return True;
      }
   }
   return False;
}


/* Figure out whether or not hcode is jitted code present in the main
   code cache (but not in the no-redir cache).  Used for sanity
   checking. */
static Bool is_in_the_main_TC ( void* hcode )
{
   Int i, sno;
   for (i = 0; i < N_SECTORS; i++) {
      sno = sector_search_order[i];
      if (sno == -1)
         break; /* run out of sectors to search */
      if ((UChar*)hcode >= (UChar*)sectors[sno].tc
          && (UChar*)hcode <= (UChar*)sectors[sno].tc_next
                              + sizeof(ULong) - 1)
         return True;
   }
   return False;
}


/* Fulfill a chaining request, and record admin info so we
   can undo it later, if required.
*/
void VG_(tt_tc_do_chaining) ( void* from__patch_addr,
                              UInt  to_sNo,
                              UInt  to_tteNo,
                              Bool  to_fastEP )
{
   /* Get the CPU info established at startup. */
   VexArch vex_arch = VexArch_INVALID;
   VG_(machine_get_VexArchInfo)( &vex_arch, NULL );

   // host_code is where we're patching to.  So it needs to
   // take into account, whether we're jumping to the slow
   // or fast entry point.  By definition, the fast entry point
   // is exactly one event check's worth of code along from
   // the slow (tcptr) entry point.
   TTEntry* to_tte    = index_tte(to_sNo, to_tteNo);
   void*    host_code = ((UChar*)to_tte->tcptr)
                        + (to_fastEP ? LibVEX_evCheckSzB(vex_arch) : 0);

   // stay sane -- the patch point (dst) is in this sector's code cache
   vg_assert( (UChar*)host_code >= (UChar*)sectors[to_sNo].tc );
   vg_assert( (UChar*)host_code <= (UChar*)sectors[to_sNo].tc_next
                                   + sizeof(ULong) - 1 );

   /* Find the TTEntry for the from__ code.  This isn't simple since
      we only know the patch address, which is going to be somewhere
      inside the from_ block. */
   UInt from_sNo   = (UInt)-1;
   UInt from_tteNo = (UInt)-1;
   Bool from_found
      = find_TTEntry_from_hcode( &from_sNo, &from_tteNo,
                                 from__patch_addr );
   if (!from_found) {
      // The from code might have been discarded due to sector re-use
      // or marked Deleted due to translation invalidation.
      // In such a case, don't do the chaining.
      VG_(debugLog)(1,"transtab",
                    "host code %p not found (discarded? sector recycled?)"
                    " => no chaining done\n",
                    from__patch_addr);
      return;
   }

   TTEntry* from_tte = index_tte(from_sNo, from_tteNo);

   /* Get VEX to do the patching itself.  We have to hand it off
      since it is host-dependent. */
   VexInvalRange vir
      = LibVEX_Chain(
           vex_arch,
           from__patch_addr,
           VG_(fnptr_to_fnentry)(
              to_fastEP ? &VG_(disp_cp_chain_me_to_fastEP)
                        : &VG_(disp_cp_chain_me_to_slowEP)),
           (void*)host_code
        );
   VG_(invalidate_icache)( (void*)vir.start, vir.len );

   /* Now do the tricky bit -- update the ch_succs and ch_preds info
      for the two translations involved, so we can undo the chaining
      later, which we will have to do if the to_ block gets removed
      for whatever reason. */

   /* This is the new from_ -> to_ link to add. */
   InEdge ie;
   InEdge__init(&ie);
   ie.from_sNo   = from_sNo;
   ie.from_tteNo = from_tteNo;
   ie.to_fastEP  = to_fastEP;
   HWord from_offs = (HWord)( (UChar*)from__patch_addr
                              - (UChar*)from_tte->tcptr );
   vg_assert(from_offs < 100000/* let's say */);
   ie.from_offs  = (UInt)from_offs;

   /* This is the new to_ -> from_ backlink to add. */
   OutEdge oe;
   OutEdge__init(&oe);
   oe.to_sNo    = to_sNo;
   oe.to_tteNo  = to_tteNo;
   oe.from_offs = (UInt)from_offs;

   /* Add .. */
   InEdgeArr__add(&to_tte->in_edges, &ie);
   OutEdgeArr__add(&from_tte->out_edges, &oe);
}


/* Unchain one patch, as described by the specified InEdge.  For
   sanity check purposes only (to check that the patched location is
   as expected) it also requires the fast and slow entry point
   addresses of the destination block (that is, the block that owns
   this InEdge). */
__attribute__((noinline))
static void unchain_one ( VexArch vex_arch,
                          InEdge* ie,
                          void* to_fastEPaddr, void* to_slowEPaddr )
{
   vg_assert(ie);
   TTEntry* tte
      = index_tte(ie->from_sNo, ie->from_tteNo);
   UChar* place_to_patch
      = ((HChar*)tte->tcptr) + ie->from_offs;
   UChar* disp_cp_chain_me
      = VG_(fnptr_to_fnentry)(
           ie->to_fastEP ? &VG_(disp_cp_chain_me_to_fastEP)
                         : &VG_(disp_cp_chain_me_to_slowEP)
        );
   UChar* place_to_jump_to_EXPECTED
      = ie->to_fastEP ? to_fastEPaddr : to_slowEPaddr;

   // stay sane: both src and dst for this unchaining are
   // in the main code cache
   vg_assert( is_in_the_main_TC(place_to_patch) ); // src
   vg_assert( is_in_the_main_TC(place_to_jump_to_EXPECTED) ); // dst
   // dst check is ok because LibVEX_UnChain checks that
   // place_to_jump_to_EXPECTED really is the current dst, and
   // asserts if it isn't.
   VexInvalRange vir
       = LibVEX_UnChain( vex_arch, place_to_patch, 
                         place_to_jump_to_EXPECTED, disp_cp_chain_me );
   VG_(invalidate_icache)( (void*)vir.start, vir.len );
}


/* The specified block is about to be deleted.  Update the preds and
   succs of its associated blocks accordingly.  This includes undoing
   any chained jumps to this block. */
static
void unchain_in_preparation_for_deletion ( VexArch vex_arch,
                                           UInt here_sNo, UInt here_tteNo )
{
   if (0)
      VG_(printf)("QQQ unchain_in_prep %u.%u\n", here_sNo, here_tteNo);
   UWord    i, j, n, m;
   Int      evCheckSzB = LibVEX_evCheckSzB(vex_arch);
   TTEntry* here_tte   = index_tte(here_sNo, here_tteNo);
   vg_assert(here_tte->status == InUse);

   /* Visit all InEdges owned by here_tte. */
   n = InEdgeArr__size(&here_tte->in_edges);
   for (i = 0; i < n; i++) {
      InEdge* ie = InEdgeArr__index(&here_tte->in_edges, i);
      // Undo the chaining.
      UChar* here_slow_EP = (UChar*)here_tte->tcptr;
      UChar* here_fast_EP = here_slow_EP + evCheckSzB;
      unchain_one(vex_arch, ie, here_fast_EP, here_slow_EP);
      // Find the corresponding entry in the "from" node's out_edges,
      // and remove it.
      TTEntry* from_tte = index_tte(ie->from_sNo, ie->from_tteNo);
      m = OutEdgeArr__size(&from_tte->out_edges);
      vg_assert(m > 0); // it must have at least one entry
      for (j = 0; j < m; j++) {
         OutEdge* oe = OutEdgeArr__index(&from_tte->out_edges, j);
         if (oe->to_sNo == here_sNo && oe->to_tteNo == here_tteNo
             && oe->from_offs == ie->from_offs)
           break;
      }
      vg_assert(j < m); // "oe must be findable"
      OutEdgeArr__deleteIndex(&from_tte->out_edges, j);
   }

   /* Visit all OutEdges owned by here_tte. */
   n = OutEdgeArr__size(&here_tte->out_edges);
   for (i = 0; i < n; i++) {
      OutEdge* oe = OutEdgeArr__index(&here_tte->out_edges, i);
      // Find the corresponding entry in the "to" node's in_edges,
      // and remove it.
      TTEntry* to_tte = index_tte(oe->to_sNo, oe->to_tteNo);
      m = InEdgeArr__size(&to_tte->in_edges);
      vg_assert(m > 0); // it must have at least one entry
      for (j = 0; j < m; j++) {
         InEdge* ie = InEdgeArr__index(&to_tte->in_edges, j);
         if (ie->from_sNo == here_sNo && ie->from_tteNo == here_tteNo
             && ie->from_offs == oe->from_offs)
           break;
      }
      vg_assert(j < m); // "ie must be findable"
      InEdgeArr__deleteIndex(&to_tte->in_edges, j);
   }

   InEdgeArr__makeEmpty(&here_tte->in_edges);
   OutEdgeArr__makeEmpty(&here_tte->out_edges);
}


/*-------------------------------------------------------------*/
/*--- Address-range equivalence class stuff                 ---*/
/*-------------------------------------------------------------*/

/* Return equivalence class number for a range. */

static Int range_to_eclass ( Addr64 start, UInt len )
{
   UInt mask   = (1 << ECLASS_WIDTH) - 1;
   UInt lo     = (UInt)start;
   UInt hi     = lo + len - 1;
   UInt loBits = (lo >> ECLASS_SHIFT) & mask;
   UInt hiBits = (hi >> ECLASS_SHIFT) & mask;
   if (loBits == hiBits) {
      vg_assert(loBits < ECLASS_N-1);
      return loBits;
   } else {
      return ECLASS_MISC;
   }
}


/* Calculates the equivalence class numbers for any VexGuestExtent.
   These are written in *eclasses, which must be big enough to hold 3
   Ints.  The number written, between 1 and 3, is returned.  The
   eclasses are presented in order, and any duplicates are removed.
*/

static 
Int vexGuestExtents_to_eclasses ( /*OUT*/Int* eclasses,
                                  VexGuestExtents* vge )
{
#  define SWAP(_lv1,_lv2) \
      do { Int t = _lv1; _lv1 = _lv2; _lv2 = t; } while (0)

   Int i, j, n_ec, r;

   vg_assert(vge->n_used >= 1 && vge->n_used <= 3);

   n_ec = 0;
   for (i = 0; i < vge->n_used; i++) {
      r = range_to_eclass( vge->base[i], (UInt)vge->len[i] );
      if (r == ECLASS_MISC) 
         goto bad;
      /* only add if we haven't already seen it */
      for (j = 0; j < n_ec; j++)
         if (eclasses[j] == r)
            break;
      if (j == n_ec)
         eclasses[n_ec++] = r;
   }

   if (n_ec == 1)
      return 1;

   if (n_ec == 2) {
      /* sort */
      if (eclasses[0] > eclasses[1])
         SWAP(eclasses[0], eclasses[1]);
      return 2;
   }

   if (n_ec == 3) {
      /* sort */
      if (eclasses[0] > eclasses[2])
         SWAP(eclasses[0], eclasses[2]);
      if (eclasses[0] > eclasses[1])
         SWAP(eclasses[0], eclasses[1]);
      if (eclasses[1] > eclasses[2])
         SWAP(eclasses[1], eclasses[2]);
      return 3;
   }

   /* NOTREACHED */
   vg_assert(0);

  bad:
   eclasses[0] = ECLASS_MISC;
   return 1;

#  undef SWAP
}


/* Add tteno to the set of entries listed for equivalence class ec in
   this sector.  Returns used location in eclass array. */

static 
UInt addEClassNo ( /*MOD*/Sector* sec, Int ec, UShort tteno )
{
   Int    old_sz, new_sz, i, r;
   UShort *old_ar, *new_ar;

   vg_assert(ec >= 0 && ec < ECLASS_N);
   vg_assert(tteno < N_TTES_PER_SECTOR);

   if (0) VG_(printf)("ec %d  gets %d\n", ec, (Int)tteno);

   if (sec->ec2tte_used[ec] >= sec->ec2tte_size[ec]) {

      vg_assert(sec->ec2tte_used[ec] == sec->ec2tte_size[ec]);

      old_sz = sec->ec2tte_size[ec];
      old_ar = sec->ec2tte[ec];
      new_sz = old_sz==0 ? 8 : old_sz<64 ? 2*old_sz : (3*old_sz)/2;
      new_ar = ttaux_malloc("transtab.aECN.1",
                            new_sz * sizeof(UShort));
      for (i = 0; i < old_sz; i++)
         new_ar[i] = old_ar[i];
      if (old_ar)
         ttaux_free(old_ar);
      sec->ec2tte_size[ec] = new_sz;
      sec->ec2tte[ec] = new_ar;

      if (0) VG_(printf)("expand ec %d to %d\n", ec, new_sz);
   }

   /* Common case */
   r = sec->ec2tte_used[ec]++;
   vg_assert(r >= 0 && r < sec->ec2tte_size[ec]);
   sec->ec2tte[ec][r] = tteno;
   return (UInt)r;
}


/* 'vge' is being added to 'sec' at TT entry 'tteno'.  Add appropriate
   eclass entries to 'sec'. */

static 
void upd_eclasses_after_add ( /*MOD*/Sector* sec, Int tteno )
{
   Int i, r, eclasses[3];
   TTEntry* tte;
   vg_assert(tteno >= 0 && tteno < N_TTES_PER_SECTOR);

   tte = &sec->tt[tteno];
   r = vexGuestExtents_to_eclasses( eclasses, &tte->vge );

   vg_assert(r >= 1 && r <= 3);
   tte->n_tte2ec = r;

   for (i = 0; i < r; i++) {
      tte->tte2ec_ec[i] = eclasses[i];
      tte->tte2ec_ix[i] = addEClassNo( sec, eclasses[i], (UShort)tteno );
   }
}


/* Check the eclass info in 'sec' to ensure it is consistent.  Returns
   True if OK, False if something's not right.  Expensive. */

static Bool sanity_check_eclasses_in_sector ( Sector* sec )
{
#  define BAD(_str) do { whassup = (_str); goto bad; } while (0)

   HChar*   whassup = NULL;
   Int      i, j, k, n, ec_num, ec_idx;
   TTEntry* tte;
   UShort   tteno;
   ULong*   tce;

   /* Basic checks on this sector */
   if (sec->tt_n_inuse < 0 || sec->tt_n_inuse > N_TTES_PER_SECTOR_USABLE)
      BAD("invalid sec->tt_n_inuse");
   tce = sec->tc_next;
   if (tce < &sec->tc[0] || tce > &sec->tc[tc_sector_szQ])
      BAD("sec->tc_next points outside tc");

   /* For each eclass ... */
   for (i = 0; i < ECLASS_N; i++) {
      if (sec->ec2tte_size[i] == 0 && sec->ec2tte[i] != NULL)
         BAD("ec2tte_size/ec2tte mismatch(1)");
      if (sec->ec2tte_size[i] != 0 && sec->ec2tte[i] == NULL)
         BAD("ec2tte_size/ec2tte mismatch(2)");
      if (sec->ec2tte_used[i] < 0 
          || sec->ec2tte_used[i] > sec->ec2tte_size[i])
         BAD("implausible ec2tte_used");
      if (sec->ec2tte_used[i] == 0)
         continue;

      /* For each tt reference in each eclass .. ensure the reference
         is to a valid tt entry, and that the entry's address ranges
         really include this eclass. */

      for (j = 0; j < sec->ec2tte_used[i]; j++) {
         tteno = sec->ec2tte[i][j];
         if (tteno == EC2TTE_DELETED)
            continue;
         if (tteno >= N_TTES_PER_SECTOR)
            BAD("implausible tteno");
         tte = &sec->tt[tteno];
         if (tte->status != InUse)
            BAD("tteno points to non-inuse tte");
         if (tte->n_tte2ec < 1 || tte->n_tte2ec > 3)
            BAD("tte->n_tte2ec out of range");
         /* Exactly least one of tte->eclasses[0 .. tte->n_eclasses-1]
            must equal i.  Inspect tte's eclass info. */
         n = 0;
         for (k = 0; k < tte->n_tte2ec; k++) {
            if (k < tte->n_tte2ec-1
                && tte->tte2ec_ec[k] >= tte->tte2ec_ec[k+1])
               BAD("tte->tte2ec_ec[..] out of order");
            ec_num = tte->tte2ec_ec[k];
            if (ec_num < 0 || ec_num >= ECLASS_N)
               BAD("tte->tte2ec_ec[..] out of range");
            if (ec_num != i)
               continue;
            ec_idx = tte->tte2ec_ix[k];
            if (ec_idx < 0 || ec_idx >= sec->ec2tte_used[i])
               BAD("tte->tte2ec_ix[..] out of range");
            if (ec_idx == j)
               n++;
         }
         if (n != 1)
            BAD("tteno does not point back at eclass");
      }
   }

   /* That establishes that for each forward pointer from TTEntrys
      there is a corresponding backward pointer from the eclass[]
      arrays.  However, it doesn't rule out the possibility of other,
      bogus pointers in the eclass[] arrays.  So do those similarly:
      scan through them and check the TTEntryies they point at point
      back. */

   for (i = 0; i < N_TTES_PER_SECTOR_USABLE; i++) {

      tte = &sec->tt[i];
      if (tte->status == Empty || tte->status == Deleted) {
         if (tte->n_tte2ec != 0)
            BAD("tte->n_eclasses nonzero for unused tte");
         continue;
      }

      vg_assert(tte->status == InUse);

      if (tte->n_tte2ec < 1 || tte->n_tte2ec > 3)
         BAD("tte->n_eclasses out of range(2)");

      for (j = 0; j < tte->n_tte2ec; j++) {
         ec_num = tte->tte2ec_ec[j];
         if (ec_num < 0 || ec_num >= ECLASS_N)
            BAD("tte->eclass[..] out of range");
         ec_idx = tte->tte2ec_ix[j];
         if (ec_idx < 0 || ec_idx >= sec->ec2tte_used[ec_num])
            BAD("tte->ec_idx[..] out of range(2)");
         if (sec->ec2tte[ec_num][ec_idx] != i)
            BAD("ec2tte does not point back to tte");
      }
   }

   return True;

  bad:
   if (whassup)
      VG_(debugLog)(0, "transtab", "eclass sanity fail: %s\n", whassup);

#  if 0
   VG_(printf)("eclass = %d\n", i);
   VG_(printf)("tteno = %d\n", (Int)tteno);
   switch (tte->status) {
      case InUse:   VG_(printf)("InUse\n"); break;
      case Deleted: VG_(printf)("Deleted\n"); break;
      case Empty:   VG_(printf)("Empty\n"); break;
   }
   if (tte->status != Empty) {
      for (k = 0; k < tte->vge.n_used; k++)
         VG_(printf)("0x%llx %d\n", tte->vge.base[k], 
                                    (Int)tte->vge.len[k]);
   }
#  endif

   return False;

#  undef BAD
}


/* Sanity check absolutely everything.  True == check passed. */

/* forwards */
static Bool sanity_check_redir_tt_tc ( void );

static Bool sanity_check_sector_search_order ( void )
{
   Int i, j, nListed;
   /* assert the array is the right size */
   vg_assert(N_SECTORS == (sizeof(sector_search_order) 
                           / sizeof(sector_search_order[0])));
   /* Check it's of the form  valid_sector_numbers ++ [-1, -1, ..] */
   for (i = 0; i < N_SECTORS; i++) {
      if (sector_search_order[i] < 0 || sector_search_order[i] >= N_SECTORS)
         break;
   }
   nListed = i;
   for (/* */; i < N_SECTORS; i++) {
      if (sector_search_order[i] != -1)
         break;
   }
   if (i != N_SECTORS)
      return False;
   /* Check each sector number only appears once */
   for (i = 0; i < N_SECTORS; i++) {
      if (sector_search_order[i] == -1)
         continue;
      for (j = i+1; j < N_SECTORS; j++) {
         if (sector_search_order[j] == sector_search_order[i])
            return False;
      }
   }
   /* Check that the number of listed sectors equals the number
      in use, by counting nListed back down. */
   for (i = 0; i < N_SECTORS; i++) {
      if (sectors[i].tc != NULL)
         nListed--;
   }
   if (nListed != 0)
      return False;
   return True;
}

static Bool sanity_check_all_sectors ( void )
{
   Int     sno;
   Bool    sane;
   Sector* sec;
   for (sno = 0; sno < N_SECTORS; sno++) {
      sec = &sectors[sno];
      if (sec->tc == NULL)
         continue;
      sane = sanity_check_eclasses_in_sector( sec );
      if (!sane)
         return False;
   }
   if ( !sanity_check_redir_tt_tc() )
      return False;
   if ( !sanity_check_sector_search_order() )
      return False;
   return True;
}



/*-------------------------------------------------------------*/
/*--- Add/find translations                                 ---*/
/*-------------------------------------------------------------*/

static UInt vge_osize ( VexGuestExtents* vge )
{
   UInt i, n = 0;
   for (i = 0; i < vge->n_used; i++)
      n += (UInt)vge->len[i];
   return n;
}

static Bool isValidSector ( Int sector )
{
   if (sector < 0 || sector >= N_SECTORS)
      return False;
   return True;
}

static inline UInt HASH_TT ( Addr64 key )
{
   UInt kHi = (UInt)(key >> 32);
   UInt kLo = (UInt)key;
   UInt k32 = kHi ^ kLo;
   UInt ror = 7;
   if (ror > 0)
      k32 = (k32 >> ror) | (k32 << (32-ror));
   return k32 % N_TTES_PER_SECTOR;
}

static void setFastCacheEntry ( Addr64 key, ULong* tcptr )
{
   UInt cno = (UInt)VG_TT_FAST_HASH(key);
   VG_(tt_fast)[cno].guest = (Addr)key;
   VG_(tt_fast)[cno].host  = (Addr)tcptr;
   n_fast_updates++;
   /* This shouldn't fail.  It should be assured by m_translate
      which should reject any attempt to make translation of code
      starting at TRANSTAB_BOGUS_GUEST_ADDR. */
   vg_assert(VG_(tt_fast)[cno].guest != TRANSTAB_BOGUS_GUEST_ADDR);
}

/* Invalidate the fast cache VG_(tt_fast). */
static void invalidateFastCache ( void )
{
   UInt j;
   /* This loop is popular enough to make it worth unrolling a
      bit, at least on ppc32. */
   vg_assert(VG_TT_FAST_SIZE > 0 && (VG_TT_FAST_SIZE % 4) == 0);
   for (j = 0; j < VG_TT_FAST_SIZE; j += 4) {
      VG_(tt_fast)[j+0].guest = TRANSTAB_BOGUS_GUEST_ADDR;
      VG_(tt_fast)[j+1].guest = TRANSTAB_BOGUS_GUEST_ADDR;
      VG_(tt_fast)[j+2].guest = TRANSTAB_BOGUS_GUEST_ADDR;
      VG_(tt_fast)[j+3].guest = TRANSTAB_BOGUS_GUEST_ADDR;
   }

   vg_assert(j == VG_TT_FAST_SIZE);
   n_fast_flushes++;
}

static void initialiseSector ( Int sno )
{
   Int     i;
   SysRes  sres;
   Sector* sec;
   vg_assert(isValidSector(sno));

   { Bool sane = sanity_check_sector_search_order();
     vg_assert(sane);
   }
   sec = &sectors[sno];

   if (sec->tc == NULL) {

      /* Sector has never been used before.  Need to allocate tt and
         tc. */
      vg_assert(sec->tt == NULL);
      vg_assert(sec->tc_next == NULL);
      vg_assert(sec->tt_n_inuse == 0);
      for (i = 0; i < ECLASS_N; i++) {
         vg_assert(sec->ec2tte_size[i] == 0);
         vg_assert(sec->ec2tte_used[i] == 0);
         vg_assert(sec->ec2tte[i] == NULL);
      }
      vg_assert(sec->host_extents == NULL);

      VG_(debugLog)(1,"transtab", "allocate sector %d\n", sno);

      sres = VG_(am_mmap_anon_float_valgrind)( 8 * tc_sector_szQ );
      if (sr_isError(sres)) {
         VG_(out_of_memory_NORETURN)("initialiseSector(TC)", 
                                     8 * tc_sector_szQ );
	 /*NOTREACHED*/
      }
      sec->tc = (ULong*)(AddrH)sr_Res(sres);

      sres = VG_(am_mmap_anon_float_valgrind)
                ( N_TTES_PER_SECTOR * sizeof(TTEntry) );
      if (sr_isError(sres)) {
         VG_(out_of_memory_NORETURN)("initialiseSector(TT)", 
                                     N_TTES_PER_SECTOR * sizeof(TTEntry) );
	 /*NOTREACHED*/
      }
      sec->tt = (TTEntry*)(AddrH)sr_Res(sres);

      for (i = 0; i < N_TTES_PER_SECTOR; i++) {
         sec->tt[i].status   = Empty;
         sec->tt[i].n_tte2ec = 0;
      }

      /* Set up the host_extents array. */
      sec->host_extents
         = VG_(newXA)(ttaux_malloc, "transtab.initialiseSector(host_extents)",
                      ttaux_free,
                      sizeof(HostExtent));

      /* Add an entry in the sector_search_order */
      for (i = 0; i < N_SECTORS; i++) {
         if (sector_search_order[i] == -1)
            break;
      }
      vg_assert(i >= 0 && i < N_SECTORS);
      sector_search_order[i] = sno;

      if (VG_(clo_verbosity) > 2)
         VG_(message)(Vg_DebugMsg, "TT/TC: initialise sector %d\n", sno);

   } else {

      /* Sector has been used before.  Dump the old contents. */
      VG_(debugLog)(1,"transtab", "recycle sector %d\n", sno);
      vg_assert(sec->tt != NULL);
      vg_assert(sec->tc_next != NULL);
      n_dump_count += sec->tt_n_inuse;

      VexArch vex_arch = VexArch_INVALID;
      VG_(machine_get_VexArchInfo)( &vex_arch, NULL );

      /* Visit each just-about-to-be-abandoned translation. */
      if (0) VG_(printf)("QQQ unlink-entire-sector: %d START\n", sno);
      for (i = 0; i < N_TTES_PER_SECTOR; i++) {
         if (sec->tt[i].status == InUse) {
            vg_assert(sec->tt[i].n_tte2ec >= 1);
            vg_assert(sec->tt[i].n_tte2ec <= 3);
            n_dump_osize += vge_osize(&sec->tt[i].vge);
            /* Tell the tool too. */
            if (VG_(needs).superblock_discards) {
               VG_TDICT_CALL( tool_discard_superblock_info,
                              sec->tt[i].entry,
                              sec->tt[i].vge );
            }
            unchain_in_preparation_for_deletion(vex_arch, sno, i);
         } else {
            vg_assert(sec->tt[i].n_tte2ec == 0);
         }
         sec->tt[i].status   = Empty;
         sec->tt[i].n_tte2ec = 0;
      }
      if (0) VG_(printf)("QQQ unlink-entire-sector: %d END\n", sno);

      /* Free up the eclass structures. */
      for (i = 0; i < ECLASS_N; i++) {
         if (sec->ec2tte_size[i] == 0) {
            vg_assert(sec->ec2tte_used[i] == 0);
            vg_assert(sec->ec2tte[i] == NULL);
         } else {
            vg_assert(sec->ec2tte[i] != NULL);
            ttaux_free(sec->ec2tte[i]);
            sec->ec2tte[i] = NULL;
            sec->ec2tte_size[i] = 0;
            sec->ec2tte_used[i] = 0;
         }
      }

      /* Empty out the host extents array. */
      vg_assert(sec->host_extents != NULL);
      VG_(dropTailXA)(sec->host_extents, VG_(sizeXA)(sec->host_extents));
      vg_assert(VG_(sizeXA)(sec->host_extents) == 0);

      /* Sanity check: ensure it is already in
         sector_search_order[]. */
      for (i = 0; i < N_SECTORS; i++) {
         if (sector_search_order[i] == sno)
            break;
      }
      vg_assert(i >= 0 && i < N_SECTORS);

      if (VG_(clo_verbosity) > 2)
         VG_(message)(Vg_DebugMsg, "TT/TC: recycle sector %d\n", sno);
   }

   sec->tc_next = sec->tc;
   sec->tt_n_inuse = 0;

   invalidateFastCache();

   { Bool sane = sanity_check_sector_search_order();
     vg_assert(sane);
   }
}


/* Add a translation of vge to TT/TC.  The translation is temporarily
   in code[0 .. code_len-1].

   pre: youngest_sector points to a valid (although possibly full)
   sector.
*/
void VG_(add_to_transtab)( VexGuestExtents* vge,
                           Addr64           entry,
                           AddrH            code,
                           UInt             code_len,
                           Bool             is_self_checking,
                           Int              offs_profInc,
                           UInt             n_guest_instrs,
                           VexArch          arch_host )
{
   Int    tcAvailQ, reqdQ, y, i;
   ULong  *tcptr, *tcptr2;
   UChar* srcP;
   UChar* dstP;

   vg_assert(init_done);
   vg_assert(vge->n_used >= 1 && vge->n_used <= 3);

   /* 60000: should agree with N_TMPBUF in m_translate.c. */
   vg_assert(code_len > 0 && code_len < 60000);

   /* Generally stay sane */
   vg_assert(n_guest_instrs < 200); /* it can be zero, tho */

   if (0)
      VG_(printf)("add_to_transtab(entry = 0x%llx, len = %d)\n",
                  entry, code_len);

   n_in_count++;
   n_in_tsize += code_len;
   n_in_osize += vge_osize(vge);
   if (is_self_checking)
      n_in_sc_count++;

   y = youngest_sector;
   vg_assert(isValidSector(y));

   if (sectors[y].tc == NULL)
      initialiseSector(y);

   /* Try putting the translation in this sector. */
   reqdQ = (code_len + 7) >> 3;

   /* Will it fit in tc? */
   tcAvailQ = ((ULong*)(&sectors[y].tc[tc_sector_szQ]))
              - ((ULong*)(sectors[y].tc_next));
   vg_assert(tcAvailQ >= 0);
   vg_assert(tcAvailQ <= tc_sector_szQ);

   if (tcAvailQ < reqdQ 
       || sectors[y].tt_n_inuse >= N_TTES_PER_SECTOR_USABLE) {
      /* No.  So move on to the next sector.  Either it's never been
         used before, in which case it will get its tt/tc allocated
         now, or it has been used before, in which case it is set to be
         empty, hence throwing out the oldest sector. */
      vg_assert(tc_sector_szQ > 0);
      VG_(debugLog)(1,"transtab", 
                      "declare sector %d full "
                      "(TT loading %2d%%, TC loading %2d%%)\n",
                      y,
                      (100 * sectors[y].tt_n_inuse) 
                         / N_TTES_PER_SECTOR,
                      (100 * (tc_sector_szQ - tcAvailQ)) 
                         / tc_sector_szQ);
      youngest_sector++;
      if (youngest_sector >= N_SECTORS)
         youngest_sector = 0;
      y = youngest_sector;
      initialiseSector(y);
   }

   /* Be sure ... */
   tcAvailQ = ((ULong*)(&sectors[y].tc[tc_sector_szQ]))
              - ((ULong*)(sectors[y].tc_next));
   vg_assert(tcAvailQ >= 0);
   vg_assert(tcAvailQ <= tc_sector_szQ);
   vg_assert(tcAvailQ >= reqdQ);
   vg_assert(sectors[y].tt_n_inuse < N_TTES_PER_SECTOR_USABLE);
   vg_assert(sectors[y].tt_n_inuse >= 0);
 
   /* Copy into tc. */
   tcptr = sectors[y].tc_next;
   vg_assert(tcptr >= &sectors[y].tc[0]);
   vg_assert(tcptr <= &sectors[y].tc[tc_sector_szQ]);

   dstP = (UChar*)tcptr;
   srcP = (UChar*)code;
   VG_(memcpy)(dstP, srcP, code_len);
   sectors[y].tc_next += reqdQ;
   sectors[y].tt_n_inuse++;

   /* more paranoia */
   tcptr2 = sectors[y].tc_next;
   vg_assert(tcptr2 >= &sectors[y].tc[0]);
   vg_assert(tcptr2 <= &sectors[y].tc[tc_sector_szQ]);

   /* Find an empty tt slot, and use it.  There must be such a slot
      since tt is never allowed to get completely full. */
   i = HASH_TT(entry);
   vg_assert(i >= 0 && i < N_TTES_PER_SECTOR);
   while (True) {
      if (sectors[y].tt[i].status == Empty
          || sectors[y].tt[i].status == Deleted)
         break;
      i++;
      if (i >= N_TTES_PER_SECTOR)
         i = 0;
   }

   TTEntry__init(&sectors[y].tt[i]);
   sectors[y].tt[i].status = InUse;
   sectors[y].tt[i].tcptr  = tcptr;
   sectors[y].tt[i].count  = 0;
   sectors[y].tt[i].weight = n_guest_instrs == 0 ? 1 : n_guest_instrs;
   sectors[y].tt[i].vge    = *vge;
   sectors[y].tt[i].entry  = entry;

   /* Patch in the profile counter location, if necessary. */
   if (offs_profInc != -1) {
      vg_assert(offs_profInc >= 0 && offs_profInc < code_len);
      VexInvalRange vir
         = LibVEX_PatchProfInc( arch_host,
                                dstP + offs_profInc,
                                &sectors[y].tt[i].count );
      VG_(invalidate_icache)( (void*)vir.start, vir.len );
   }

   VG_(invalidate_icache)( dstP, code_len );

   /* Add this entry to the host_extents map, checking that we're
      adding in order. */
   { HostExtent hx;
     hx.start = (UChar*)tcptr;
     hx.len   = code_len;
     hx.tteNo = i;
     vg_assert(hx.len > 0); /* bsearch fails w/ zero length entries */
     XArray* hx_array = sectors[y].host_extents;
     vg_assert(hx_array);
     Word n = VG_(sizeXA)(hx_array);
     if (n > 0) {
        HostExtent* hx_prev = (HostExtent*)VG_(indexXA)(hx_array, n-1);
        vg_assert(hx_prev->start + hx_prev->len <= hx.start);
     }
     VG_(addToXA)(hx_array, &hx);
   }

   /* Update the fast-cache. */
   setFastCacheEntry( entry, tcptr );

   /* Note the eclass numbers for this translation. */
   upd_eclasses_after_add( &sectors[y], i );
}


/* Search for the translation of the given guest address.  If
   requested, a successful search can also cause the fast-caches to be
   updated.  
*/
Bool VG_(search_transtab) ( /*OUT*/AddrH* res_hcode,
                            /*OUT*/UInt*  res_sNo,
                            /*OUT*/UInt*  res_tteNo,
                            Addr64        guest_addr, 
                            Bool          upd_cache )
{
   Int i, j, k, kstart, sno;

   vg_assert(init_done);
   /* Find the initial probe point just once.  It will be the same in
      all sectors and avoids multiple expensive % operations. */
   n_full_lookups++;
   k      = -1;
   kstart = HASH_TT(guest_addr);
   vg_assert(kstart >= 0 && kstart < N_TTES_PER_SECTOR);

   /* Search in all the sectors,using sector_search_order[] as a
      heuristic guide as to what order to visit the sectors. */
   for (i = 0; i < N_SECTORS; i++) {

      sno = sector_search_order[i];
      if (UNLIKELY(sno == -1))
         return False; /* run out of sectors to search */

      k = kstart;
      for (j = 0; j < N_TTES_PER_SECTOR; j++) {
         n_lookup_probes++;
         if (sectors[sno].tt[k].status == InUse
             && sectors[sno].tt[k].entry == guest_addr) {
            /* found it */
            if (upd_cache)
               setFastCacheEntry( 
                  guest_addr, sectors[sno].tt[k].tcptr );
            if (res_hcode)
               *res_hcode = (AddrH)sectors[sno].tt[k].tcptr;
            if (res_sNo)
               *res_sNo = sno;
            if (res_tteNo)
               *res_tteNo = k;
            /* pull this one one step closer to the front.  For large
               apps this more or less halves the number of required
               probes. */
            if (i > 0) {
               Int tmp = sector_search_order[i-1];
               sector_search_order[i-1] = sector_search_order[i];
               sector_search_order[i] = tmp;
            }
            return True;
         }
         if (sectors[sno].tt[k].status == Empty)
            break; /* not found in this sector */
         k++;
         if (k == N_TTES_PER_SECTOR)
            k = 0;
      }

      /* If we fall off the end, all entries are InUse and not
         matching, or Deleted.  In any case we did not find it in this
         sector. */
   }

   /* Not found in any sector. */
   return False;
}


/*-------------------------------------------------------------*/
/*--- Delete translations.                                  ---*/
/*-------------------------------------------------------------*/

/* forward */
static void unredir_discard_translations( Addr64, ULong );

/* Stuff for deleting translations which intersect with a given
   address range.  Unfortunately, to make this run at a reasonable
   speed, it is complex. */

static inline
Bool overlap1 ( Addr64 s1, ULong r1, Addr64 s2, ULong r2 )
{
   Addr64 e1 = s1 + r1 - 1ULL;
   Addr64 e2 = s2 + r2 - 1ULL;
   if (e1 < s2 || e2 < s1) 
      return False;
   return True;
}

static inline
Bool overlaps ( Addr64 start, ULong range, VexGuestExtents* vge )
{
   if (overlap1(start, range, vge->base[0], (UInt)vge->len[0]))
      return True;
   if (vge->n_used < 2)
      return False;
   if (overlap1(start, range, vge->base[1], (UInt)vge->len[1]))
      return True;
   if (vge->n_used < 3)
      return False;
   if (overlap1(start, range, vge->base[2], (UInt)vge->len[2]))
      return True;
   return False;
}


/* Delete a tt entry, and update all the eclass data accordingly. */

static void delete_tte ( /*MOD*/Sector* sec, UInt secNo, Int tteno,
                         VexArch vex_arch )
{
   Int      i, ec_num, ec_idx;
   TTEntry* tte;

   /* sec and secNo are mutually redundant; cross-check. */
   vg_assert(sec == &sectors[secNo]);

   vg_assert(tteno >= 0 && tteno < N_TTES_PER_SECTOR);
   tte = &sec->tt[tteno];
   vg_assert(tte->status == InUse);
   vg_assert(tte->n_tte2ec >= 1 && tte->n_tte2ec <= 3);

   /* Unchain .. */
   unchain_in_preparation_for_deletion(vex_arch, secNo, tteno);

   /* Deal with the ec-to-tte links first. */
   for (i = 0; i < tte->n_tte2ec; i++) {
      ec_num = (Int)tte->tte2ec_ec[i];
      ec_idx = tte->tte2ec_ix[i];
      vg_assert(ec_num >= 0 && ec_num < ECLASS_N);
      vg_assert(ec_idx >= 0);
      vg_assert(ec_idx < sec->ec2tte_used[ec_num]);
      /* Assert that the two links point at each other. */
      vg_assert(sec->ec2tte[ec_num][ec_idx] == (UShort)tteno);
      /* "delete" the pointer back to here. */
      sec->ec2tte[ec_num][ec_idx] = EC2TTE_DELETED;
   }

   /* Now fix up this TTEntry. */
   tte->status   = Deleted;
   tte->n_tte2ec = 0;

   /* Stats .. */
   sec->tt_n_inuse--;
   n_disc_count++;
   n_disc_osize += vge_osize(&tte->vge);

   /* Tell the tool too. */
   if (VG_(needs).superblock_discards) {
      VG_TDICT_CALL( tool_discard_superblock_info,
                     tte->entry,
                     tte->vge );
   }
}


/* Delete translations from sec which intersect specified range, but
   only consider translations in the specified eclass. */

static 
Bool delete_translations_in_sector_eclass ( /*MOD*/Sector* sec, UInt secNo,
                                            Addr64 guest_start, ULong range,
                                            Int ec,
                                            VexArch vex_arch )
{
   Int      i;
   UShort   tteno;
   Bool     anyDeld = False;
   TTEntry* tte;

   vg_assert(ec >= 0 && ec < ECLASS_N);

   for (i = 0; i < sec->ec2tte_used[ec]; i++) {

      tteno = sec->ec2tte[ec][i];
      if (tteno == EC2TTE_DELETED) {
         /* already deleted */
         continue;
      }

      vg_assert(tteno < N_TTES_PER_SECTOR);

      tte = &sec->tt[tteno];
      vg_assert(tte->status == InUse);

      if (overlaps( guest_start, range, &tte->vge )) {
         anyDeld = True;
         delete_tte( sec, secNo, (Int)tteno, vex_arch );
      }

   }

   return anyDeld;
}


/* Delete translations from sec which intersect specified range, the
   slow way, by inspecting all translations in sec. */

static 
Bool delete_translations_in_sector ( /*MOD*/Sector* sec, UInt secNo,
                                     Addr64 guest_start, ULong range,
                                     VexArch vex_arch )
{
   Int  i;
   Bool anyDeld = False;

   for (i = 0; i < N_TTES_PER_SECTOR; i++) {
      if (sec->tt[i].status == InUse
          && overlaps( guest_start, range, &sec->tt[i].vge )) {
         anyDeld = True;
         delete_tte( sec, secNo, i, vex_arch );
      }
   }

   return anyDeld;
} 


void VG_(discard_translations) ( Addr64 guest_start, ULong range,
                                 HChar* who )
{
   Sector* sec;
   Int     sno, ec;
   Bool    anyDeleted = False;

   vg_assert(init_done);

   VG_(debugLog)(2, "transtab",
                    "discard_translations(0x%llx, %lld) req by %s\n",
                    guest_start, range, who );

   /* Pre-deletion sanity check */
   if (VG_(clo_sanity_level >= 4)) {
      Bool sane = sanity_check_all_sectors();
      vg_assert(sane);
   }

   if (range == 0)
      return;

   VexArch vex_arch = VexArch_INVALID;
   VG_(machine_get_VexArchInfo)( &vex_arch, NULL );

   /* There are two different ways to do this.

      If the range fits within a single address-range equivalence
      class, as will be the case for a cache line sized invalidation,
      then we only have to inspect the set of translations listed in
      that equivalence class, and also in the "sin-bin" equivalence
      class ECLASS_MISC.

      Otherwise, the invalidation is of a larger range and probably
      results from munmap.  In this case it's (probably!) faster just
      to inspect all translations, dump those we don't want, and
      regenerate the equivalence class information (since modifying it
      in-situ is even more expensive).
   */

   /* First off, figure out if the range falls within a single class, 
      and if so which one. */

   ec = ECLASS_MISC;
   if (range < (1ULL << ECLASS_SHIFT))
      ec = range_to_eclass( guest_start, (UInt)range );

   /* if ec is ECLASS_MISC then we aren't looking at just a single
      class, so use the slow scheme.  Else use the fast scheme,
      examining 'ec' and ECLASS_MISC. */

   if (ec != ECLASS_MISC) {

      VG_(debugLog)(2, "transtab",
                       "                    FAST, ec = %d\n", ec);

      /* Fast scheme */
      vg_assert(ec >= 0 && ec < ECLASS_MISC);

      for (sno = 0; sno < N_SECTORS; sno++) {
         sec = &sectors[sno];
         if (sec->tc == NULL)
            continue;
         anyDeleted |= delete_translations_in_sector_eclass( 
                          sec, sno, guest_start, range, ec, 
                          vex_arch
                       );
         anyDeleted |= delete_translations_in_sector_eclass( 
                          sec, sno, guest_start, range, ECLASS_MISC,
                          vex_arch
                       );
      }

   } else {

      /* slow scheme */

      VG_(debugLog)(2, "transtab",
                       "                    SLOW, ec = %d\n", ec);

      for (sno = 0; sno < N_SECTORS; sno++) {
         sec = &sectors[sno];
         if (sec->tc == NULL)
            continue;
         anyDeleted |= delete_translations_in_sector( 
                          sec, sno, guest_start, range, vex_arch );
      }

   }

   if (anyDeleted)
      invalidateFastCache();

   /* don't forget the no-redir cache */
   unredir_discard_translations( guest_start, range );

   /* Post-deletion sanity check */
   if (VG_(clo_sanity_level >= 4)) {
      Int      i;
      TTEntry* tte;
      Bool     sane = sanity_check_all_sectors();
      vg_assert(sane);
      /* But now, also check the requested address range isn't
         present anywhere. */
      for (sno = 0; sno < N_SECTORS; sno++) {
         sec = &sectors[sno];
         if (sec->tc == NULL)
            continue;
         for (i = 0; i < N_TTES_PER_SECTOR; i++) {
            tte = &sec->tt[i];
            if (tte->status != InUse)
               continue;
            vg_assert(!overlaps( guest_start, range, &tte->vge ));
         }
      }
   }
}


/*------------------------------------------------------------*/
/*--- AUXILIARY: the unredirected TT/TC                    ---*/
/*------------------------------------------------------------*/

/* A very simple translation cache which holds a small number of
   unredirected translations.  This is completely independent of the
   main tt/tc structures.  When unredir_tc or unredir_tt becomes full,
   both structures are simply dumped and we start over.

   Since these translations are unredirected, the search key is (by
   definition) the first address entry in the .vge field. */

/* Sized to hold 500 translations of average size 1000 bytes. */

#define UNREDIR_SZB   1000

#define N_UNREDIR_TT  500
#define N_UNREDIR_TCQ (N_UNREDIR_TT * UNREDIR_SZB / sizeof(ULong))

typedef
   struct {
      VexGuestExtents vge;
      Addr            hcode;
      Bool            inUse;
   }
   UTCEntry;

/* We just allocate forwards in _tc, never deleting. */
static ULong    *unredir_tc;
static Int      unredir_tc_used = N_UNREDIR_TCQ;

/* Slots in _tt can come into use and out again (.inUse).
   Nevertheless _tt_highwater is maintained so that invalidations
   don't have to scan all the slots when only a few are in use.
   _tt_highwater holds the index of the highest ever allocated
   slot. */
static UTCEntry unredir_tt[N_UNREDIR_TT];
static Int      unredir_tt_highwater;


static void init_unredir_tt_tc ( void )
{
   Int i;
   if (unredir_tc == NULL) {
      SysRes sres = VG_(am_mmap_anon_float_valgrind)
                       ( N_UNREDIR_TT * UNREDIR_SZB );
      if (sr_isError(sres)) {
         VG_(out_of_memory_NORETURN)("init_unredir_tt_tc",
                                     N_UNREDIR_TT * UNREDIR_SZB);
         /*NOTREACHED*/
      }
      unredir_tc = (ULong *)(AddrH)sr_Res(sres);
   }
   unredir_tc_used = 0;
   for (i = 0; i < N_UNREDIR_TT; i++)
      unredir_tt[i].inUse = False;
   unredir_tt_highwater = -1;
}

/* Do a sanity check; return False on failure. */
static Bool sanity_check_redir_tt_tc ( void )
{
   Int i;
   if (unredir_tt_highwater < -1) return False;
   if (unredir_tt_highwater >= N_UNREDIR_TT) return False;

   for (i = unredir_tt_highwater+1; i < N_UNREDIR_TT; i++)
      if (unredir_tt[i].inUse)
         return False;

   if (unredir_tc_used < 0) return False;
   if (unredir_tc_used > N_UNREDIR_TCQ) return False;

   return True;
}


/* Add an UNREDIRECTED translation of vge to TT/TC.  The translation
   is temporarily in code[0 .. code_len-1].
*/
void VG_(add_to_unredir_transtab)( VexGuestExtents* vge,
                                   Addr64           entry,
                                   AddrH            code,
                                   UInt             code_len )
{
   Int   i, j, code_szQ;
   HChar *srcP, *dstP;

   vg_assert(sanity_check_redir_tt_tc());

   /* This is the whole point: it's not redirected! */
   vg_assert(entry == vge->base[0]);

   /* How many unredir_tt slots are needed */   
   code_szQ = (code_len + 7) / 8;

   /* Look for an empty unredir_tc slot */
   for (i = 0; i < N_UNREDIR_TT; i++)
      if (!unredir_tt[i].inUse)
         break;

   if (i >= N_UNREDIR_TT || code_szQ > (N_UNREDIR_TCQ - unredir_tc_used)) {
      /* It's full; dump everything we currently have */
      init_unredir_tt_tc();
      i = 0;
   }

   vg_assert(unredir_tc_used >= 0);
   vg_assert(unredir_tc_used <= N_UNREDIR_TCQ);
   vg_assert(code_szQ > 0);
   vg_assert(code_szQ + unredir_tc_used <= N_UNREDIR_TCQ);
   vg_assert(i >= 0 && i < N_UNREDIR_TT);
   vg_assert(unredir_tt[i].inUse == False);

   if (i > unredir_tt_highwater)
      unredir_tt_highwater = i;

   dstP = (HChar*)&unredir_tc[unredir_tc_used];
   srcP = (HChar*)code;
   for (j = 0; j < code_len; j++)
      dstP[j] = srcP[j];

   VG_(invalidate_icache)( dstP, code_len );

   unredir_tt[i].inUse = True;
   unredir_tt[i].vge   = *vge;
   unredir_tt[i].hcode = (Addr)dstP;

   unredir_tc_used += code_szQ;
   vg_assert(unredir_tc_used >= 0);
   vg_assert(unredir_tc_used <= N_UNREDIR_TCQ);

   vg_assert(&dstP[code_len] <= (HChar*)&unredir_tc[unredir_tc_used]);
}

Bool VG_(search_unredir_transtab) ( /*OUT*/AddrH* result,
                                    Addr64        guest_addr )
{
   Int i;
   for (i = 0; i < N_UNREDIR_TT; i++) {
      if (!unredir_tt[i].inUse)
         continue;
      if (unredir_tt[i].vge.base[0] == guest_addr) {
         *result = (AddrH)unredir_tt[i].hcode;
         return True;
      }
   }
   return False;
}

static void unredir_discard_translations( Addr64 guest_start, ULong range )
{
   Int i;

   vg_assert(sanity_check_redir_tt_tc());

   for (i = 0; i <= unredir_tt_highwater; i++) {
      if (unredir_tt[i].inUse
          && overlaps( guest_start, range, &unredir_tt[i].vge))
         unredir_tt[i].inUse = False;
   }
}


/*------------------------------------------------------------*/
/*--- Initialisation.                                      ---*/
/*------------------------------------------------------------*/

void VG_(init_tt_tc) ( void )
{
   Int i, j, avg_codeszQ;

   vg_assert(!init_done);
   init_done = True;

   /* Otherwise lots of things go wrong... */
   vg_assert(sizeof(ULong) == 8);
   vg_assert(sizeof(Addr64) == 8);
   /* check fast cache entries really are 2 words long */
   vg_assert(sizeof(Addr) == sizeof(void*));
   vg_assert(sizeof(FastCacheEntry) == 2 * sizeof(Addr));
   /* check fast cache entries are packed back-to-back with no spaces */
   vg_assert(sizeof( VG_(tt_fast) ) == VG_TT_FAST_SIZE * sizeof(FastCacheEntry));
   /* check fast cache is aligned as we requested.  Not fatal if it
      isn't, but we might as well make sure. */
   vg_assert(VG_IS_16_ALIGNED( ((Addr) & VG_(tt_fast)[0]) ));

   if (VG_(clo_verbosity) > 2)
      VG_(message)(Vg_DebugMsg, 
                   "TT/TC: VG_(init_tt_tc) "
                   "(startup of code management)\n");

   /* Figure out how big each tc area should be.  */
   avg_codeszQ   = (VG_(details).avg_translation_sizeB + 7) / 8;
   tc_sector_szQ = N_TTES_PER_SECTOR_USABLE * (1 + avg_codeszQ);

   /* Ensure the calculated value is not way crazy. */
   vg_assert(tc_sector_szQ >= 2 * N_TTES_PER_SECTOR_USABLE);
   vg_assert(tc_sector_szQ <= 100 * N_TTES_PER_SECTOR_USABLE);

   /* Initialise the sectors */
   youngest_sector = 0;
   for (i = 0; i < N_SECTORS; i++) {
      sectors[i].tc = NULL;
      sectors[i].tt = NULL;
      sectors[i].tc_next = NULL;
      sectors[i].tt_n_inuse = 0;
      for (j = 0; j < ECLASS_N; j++) {
         sectors[i].ec2tte_size[j] = 0;
         sectors[i].ec2tte_used[j] = 0;
         sectors[i].ec2tte[j] = NULL;
      }
      sectors[i].host_extents = NULL;
   }

   /* Initialise the sector_search_order hint table. */
   for (i = 0; i < N_SECTORS; i++)
      sector_search_order[i] = -1;

   /* Initialise the fast cache. */
   invalidateFastCache();

   /* and the unredir tt/tc */
   init_unredir_tt_tc();

   if (VG_(clo_verbosity) > 2) {
      VG_(message)(Vg_DebugMsg,
         "TT/TC: cache: %d sectors of %d bytes each = %d total\n", 
          N_SECTORS, 8 * tc_sector_szQ,
          N_SECTORS * 8 * tc_sector_szQ );
      VG_(message)(Vg_DebugMsg,
         "TT/TC: table: %d total entries, max occupancy %d (%d%%)\n",
         N_SECTORS * N_TTES_PER_SECTOR,
         N_SECTORS * N_TTES_PER_SECTOR_USABLE, 
         SECTOR_TT_LIMIT_PERCENT );
   }

   VG_(debugLog)(2, "transtab",
      "cache: %d sectors of %d bytes each = %d total\n", 
       N_SECTORS, 8 * tc_sector_szQ,
       N_SECTORS * 8 * tc_sector_szQ );
   VG_(debugLog)(2, "transtab",
      "table: %d total entries, max occupancy %d (%d%%)\n",
      N_SECTORS * N_TTES_PER_SECTOR,
      N_SECTORS * N_TTES_PER_SECTOR_USABLE, 
      SECTOR_TT_LIMIT_PERCENT );
}


/*------------------------------------------------------------*/
/*--- Printing out statistics.                             ---*/
/*------------------------------------------------------------*/

static ULong safe_idiv( ULong a, ULong b )
{
   return (b == 0 ? 0 : a / b);
}

UInt VG_(get_bbs_translated) ( void )
{
   return n_in_count;
}

void VG_(print_tt_tc_stats) ( void )
{
   VG_(message)(Vg_DebugMsg,
      "    tt/tc: %'llu tt lookups requiring %'llu probes\n",
      n_full_lookups, n_lookup_probes );
   VG_(message)(Vg_DebugMsg,
      "    tt/tc: %'llu fast-cache updates, %'llu flushes\n",
      n_fast_updates, n_fast_flushes );

   VG_(message)(Vg_DebugMsg,
                " transtab: new        %'lld "
                "(%'llu -> %'llu; ratio %'llu:10) [%'llu scs]\n",
                n_in_count, n_in_osize, n_in_tsize,
                safe_idiv(10*n_in_tsize, n_in_osize),
                n_in_sc_count);
   VG_(message)(Vg_DebugMsg,
                " transtab: dumped     %'llu (%'llu -> ?" "?)\n",
                n_dump_count, n_dump_osize );
   VG_(message)(Vg_DebugMsg,
                " transtab: discarded  %'llu (%'llu -> ?" "?)\n",
                n_disc_count, n_disc_osize );

   if (0) {
      Int i;
      VG_(printf)("\n");
      for (i = 0; i < ECLASS_N; i++) {
         VG_(printf)(" %4d", sectors[0].ec2tte_used[i]);
         if (i % 16 == 15)
            VG_(printf)("\n");
      }
      VG_(printf)("\n\n");
   }
}

/*------------------------------------------------------------*/
/*--- Printing out of profiling results.                   ---*/
/*------------------------------------------------------------*/

static ULong score ( TTEntry* tte )
{
   return ((ULong)tte->weight) * ((ULong)tte->count);
}

ULong VG_(get_BB_profile) ( BBProfEntry tops[], UInt n_tops )
{
   Int   sno, i, r, s;
   ULong score_total;

   /* First, compute the total weighted count, and find the top N
      ttes.  tops contains pointers to the most-used n_tops blocks, in
      descending order (viz, tops[0] is the highest scorer). */
   for (i = 0; i < n_tops; i++) {
      tops[i].addr  = 0;
      tops[i].score = 0;
   }

   score_total = 0;

   for (sno = 0; sno < N_SECTORS; sno++) {
      if (sectors[sno].tc == NULL)
         continue;
      for (i = 0; i < N_TTES_PER_SECTOR; i++) {
         if (sectors[sno].tt[i].status != InUse)
            continue;
         score_total += score(&sectors[sno].tt[i]);
         /* Find the rank for sectors[sno].tt[i]. */
         r = n_tops-1;
         while (True) {
            if (r == -1)
               break;
             if (tops[r].addr == 0) {
               r--; 
               continue;
             }
             if ( score(&sectors[sno].tt[i]) > tops[r].score ) {
                r--;
                continue;
             }
             break;
         }
         r++;
         vg_assert(r >= 0 && r <= n_tops);
         /* This bb should be placed at r, and bbs above it shifted
            upwards one slot. */
         if (r < n_tops) {
            for (s = n_tops-1; s > r; s--)
               tops[s] = tops[s-1];
            tops[r].addr  = sectors[sno].tt[i].entry;
            tops[r].score = score( &sectors[sno].tt[i] );
         }
      }
   }

   return score_total;
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
