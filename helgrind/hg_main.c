
/*--------------------------------------------------------------------*/
/*--- Helgrind: checking for data races in threaded programs.      ---*/
/*---                                                    hg_main.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Helgrind, a Valgrind skin for detecting
   data races in threaded programs.

   Copyright (C) 2000-2002 Nicholas Nethercote
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

#include "vg_skin.h"
#include "helgrind.h"

VG_DETERMINE_INTERFACE_VERSION

static UInt n_eraser_warnings = 0;
static UInt n_lockorder_warnings = 0;

/*------------------------------------------------------------*/
/*--- Debug guff                                           ---*/
/*------------------------------------------------------------*/

#define DEBUG_LOCK_TABLE    1   /* Print lock table at end */

#define DEBUG_MAKE_ACCESSES 0   /* Print make_access() calls */
#define DEBUG_LOCKS         0   /* Print lock()/unlock() calls and locksets */
#define DEBUG_NEW_LOCKSETS  0   /* Print new locksets when created */
#define DEBUG_ACCESSES      0   /* Print reads, writes */
#define DEBUG_MEM_LOCKSET_CHANGES 0
                                /* Print when an address's lockset
                                   changes; only useful with
                                   DEBUG_ACCESSES */

#define DEBUG_VIRGIN_READS  0   /* Dump around address on VIRGIN reads */

/* heavyweight LockSet sanity checking:
   0 == never
   1 == after important ops
   2 == As 1 and also after pthread_mutex_* ops (excessively slow)
 */
#define LOCKSET_SANITY 0


/*------------------------------------------------------------*/
/*--- Crude profiling machinery.                           ---*/
/*------------------------------------------------------------*/

// PPP: work out if I want this

#define PROF_EVENT(x)
#if 0
#ifdef VG_PROFILE_MEMORY

#define N_PROF_EVENTS 150

static UInt event_ctr[N_PROF_EVENTS];

void VGE_(done_prof_mem) ( void )
{
   Int i;
   for (i = 0; i < N_PROF_EVENTS; i++) {
      if ((i % 10) == 0)
         VG_(printf)("\n");
      if (event_ctr[i] > 0)
         VG_(printf)( "prof mem event %2d: %d\n", i, event_ctr[i] );
   }
   VG_(printf)("\n");
}

#define PROF_EVENT(ev)                                  \
   do { sk_assert((ev) >= 0 && (ev) < N_PROF_EVENTS);   \
        event_ctr[ev]++;                                \
   } while (False);

#else

//static void init_prof_mem ( void ) { }
//       void VG_(done_prof_mem) ( void ) { }

#define PROF_EVENT(ev) /* */

#endif /* VG_PROFILE_MEMORY */

/* Event index.  If just the name of the fn is given, this means the
   number of calls to the fn.  Otherwise it is the specified event.

   [PPP: snip event numbers...]
*/
#endif /* 0 */


/*------------------------------------------------------------*/
/*--- Data defns.                                          ---*/
/*------------------------------------------------------------*/

typedef enum 
   { Vge_VirginInit, Vge_NonVirginInit, Vge_SegmentInit, Vge_Error } 
   VgeInitStatus;

/* Should add up to 32 to fit in one word */
#define OTHER_BITS      30
#define STATE_BITS      2

#define ESEC_MAP_WORDS  16384   /* Words per secondary map */

/* This is for indicating that a memory block has been initialised but not
 * really directly by a particular thread... (eg. text/data initialised
 * automatically at startup).
 * Must be different to virgin_word.other */
#define TID_INDICATING_NONVIRGIN    1

/* Magic TID used for error suppression; if word state is Excl and tid
   is this, then it means all access are OK without changing state and
   without raising any more errors  */
#define TID_INDICATING_ALL          ((1 << OTHER_BITS) - 1)

/* Number of entries must fit in STATE_BITS bits */
typedef enum { Vge_Virgin, Vge_Excl, Vge_Shar, Vge_SharMod } pth_state;

typedef
   struct {
      UInt other:OTHER_BITS;
      UInt state:STATE_BITS;
   } shadow_word;

typedef
   struct {
      shadow_word swords[ESEC_MAP_WORDS];
   }
   ESecMap;

static ESecMap* primary_map[ 65536 ];
static ESecMap  distinguished_secondary_map;

static const shadow_word virgin_sword = { 0, Vge_Virgin };
static const shadow_word error_sword = { TID_INDICATING_ALL, Vge_Excl };

#define VGE_IS_DISTINGUISHED_SM(smap) \
   ((smap) == &distinguished_secondary_map)

#define ENSURE_MAPPABLE(addr,caller)                                  \
   do {                                                               \
      if (VGE_IS_DISTINGUISHED_SM(primary_map[(addr) >> 16])) {       \
         primary_map[(addr) >> 16] = alloc_secondary_map(caller);     \
         /*VG_(printf)("new 2map because of %p\n", addr);*/           \
      } \
   } while(0)


/*------------------------------------------------------------*/
/*--- Low-level support for memory tracking.               ---*/
/*------------------------------------------------------------*/

/*
   All reads and writes are recorded in the memory map, which
   records the state of all memory in the process.  The memory map is
   organised like that for normal Valgrind, except each that everything
   is done at word-level instead of byte-level, and each word has only
   one word of shadow (instead of 36 bits).  

   As for normal Valgrind there is a distinguished secondary map.  But we're
   working at word-granularity, so it has 16k word entries instead of 64k byte
   entries.  Lookup is done as follows:

     bits 31..16:   primary map lookup
     bits 15.. 2:   secondary map lookup
     bits  1.. 0:   ignored
*/


/*------------------------------------------------------------*/
/*--- Basic bitmap management, reading and writing.        ---*/
/*------------------------------------------------------------*/

/* Allocate and initialise a secondary map, marking all words as virgin. */

/* Just a value that isn't a real pointer */
#define SEC_MAP_ACCESS  (shadow_word*)0x99    


static 
ESecMap* alloc_secondary_map ( __attribute__ ((unused)) Char* caller )
{
   ESecMap* map;
   UInt  i;
   //PROF_EVENT(10); PPP

   /* It just happens that a SecMap occupies exactly 18 pages --
      although this isn't important, so the following assert is
      spurious. (SSS: not true for ESecMaps -- they're 16 pages) */
   sk_assert(0 == (sizeof(ESecMap) % VKI_BYTES_PER_PAGE));
   map = VG_(get_memory_from_mmap)( sizeof(ESecMap), caller );

   for (i = 0; i < ESEC_MAP_WORDS; i++)
      map->swords[i] = virgin_sword;

   return map;
}


/* Set a word.  The byte give by 'a' could be anywhere in the word -- the whole
 * word gets set. */
static __inline__ 
void set_sword ( Addr a, shadow_word sword )
{
   ESecMap* sm;

   //PROF_EVENT(23); PPP
   ENSURE_MAPPABLE(a, "VGE_(set_sword)");

   /* Use bits 31..16 for primary, 15..2 for secondary lookup */
   sm     = primary_map[a >> 16];
   sk_assert(sm != &distinguished_secondary_map);
   sm->swords[(a & 0xFFFC) >> 2] = sword;

   if (VGE_IS_DISTINGUISHED_SM(sm)) {
      VG_(printf)("wrote to distinguished 2ndary map! 0x%x\n", a);
      // XXX: may be legit, but I want to know when it happens --njn
      VG_(skin_panic)("wrote to distinguished 2ndary map!");
   }
}


static __inline__ 
shadow_word* get_sword_addr ( Addr a )
{
   /* Use bits 31..16 for primary, 15..2 for secondary lookup */
   ESecMap* sm     = primary_map[a >> 16];
   UInt    sm_off = (a & 0xFFFC) >> 2;

   if (VGE_IS_DISTINGUISHED_SM(sm)) {
      VG_(printf)("accessed distinguished 2ndary map! 0x%x\n", a);
      // XXX: may be legit, but I want to know when it happens --njn
      //VG_(skin_panic)("accessed distinguished 2ndary map!");
      return SEC_MAP_ACCESS;
   }

   //PROF_EVENT(21); PPP
   return & (sm->swords[sm_off]);
}


// SSS: rename these so they're not so similar to memcheck, unless it's
// appropriate of course

static __inline__ 
void init_virgin_sword(Addr a)
{
   set_sword(a, virgin_sword);
}

static __inline__
void init_error_sword(Addr a)
{
   set_sword(a, error_sword);
}

/* 'a' is guaranteed to be 4-byte aligned here (not that that's important,
 * really) */
static 
void make_writable_aligned ( Addr a, UInt size )
{
   Addr a_past_end = a + size;

   //PROF_EVENT(??)  PPP
   sk_assert(IS_ALIGNED4_ADDR(a));

   for ( ; a < a_past_end; a += 4) {
      set_sword(a, virgin_sword);
   }
}

static __inline__ 
void init_nonvirgin_sword(Addr a)
{
   shadow_word sword;
   ThreadId tid = VG_(get_current_or_recent_tid)();

   sk_assert(tid != VG_INVALID_THREADID);
   sword.other = tid;
   sword.state = Vge_Excl;
   set_sword(a, sword);
}


/* In this case, we treat it for Eraser's sake like virgin (it hasn't
 * been inited by a particular thread, it's just done automatically upon
 * startup), but we mark its .state specially so it doesn't look like an 
 * uninited read. */
static __inline__ 
void init_magically_inited_sword(Addr a)
{
   shadow_word sword;

   sk_assert(VG_INVALID_THREADID == VG_(get_current_tid)());
   sword.other = TID_INDICATING_NONVIRGIN;
   sword.state = Vge_Virgin;
   set_sword(a, virgin_sword);
}


/*------------------------------------------------------------*/
/*--- Implementation of lock sets.                         ---*/
/*------------------------------------------------------------*/

typedef struct _Mutex Mutex; /* forward decl */
typedef struct _LockSet LockSet;

typedef enum MutexState {
   MxUnknown,			/* don't know */
   MxUnlocked,			/* unlocked */
   MxLocked,			/* locked */
   MxDead			/* destroyed */
} MutexState;

struct _Mutex {
   Addr               mutexp;
   Mutex             *next;

   MutexState         state;	/* mutex state */
   ThreadId           tid;	/* owner */
   ExeContext	     *location;	/* where the last change happened */

   const LockSet     *lockdep;	/* set of locks we depend on */
   UInt               mark;	/* mark for graph traversal */
};

static inline Int mutex_cmp(const Mutex *a, const Mutex *b)
{
   return a->mutexp - b->mutexp;
}

struct _LockSet {
   UInt		      setsize;	/* number of members */
   UInt		      hash;	/* hash code */
   LockSet           *next;	/* next in hash chain */
   const Mutex       *mutex[0];	/* locks */
};

static const LockSet *emptyset;

/* Each one is an index into the lockset table. */
static const LockSet *thread_locks[VG_N_THREADS];

#define LOCKSET_HASH_SZ	1021

static LockSet *lockset_hash[LOCKSET_HASH_SZ];

/* Pack and unpack a LockSet pointer into shadow_word.other */
static inline UInt getLockSetId(const LockSet *p)
{
   UInt id;

   sk_assert(((UInt)p & ((1 << STATE_BITS)-1)) == 0);
   id = ((UInt)p) >> STATE_BITS;

   return id;
}

static inline const LockSet *getLockSet(UInt id)
{
   return (LockSet *)(id << STATE_BITS);
}

static 
void pp_LockSet(const LockSet* p)
{
   int i;

   VG_(printf)("{ ");
   for(i = 0; i < p->setsize; i++) {
      const Mutex *mx = p->mutex[i];

      VG_(printf)("%p%(y ", mx->mutexp, mx->mutexp);
   }
   VG_(printf)("}\n");
}


static void print_LockSet(const Char *s, const LockSet *ls)
{
   VG_(printf)("%s: ", s);
   pp_LockSet(ls);
}

/* Compute the hash of a LockSet */
static inline UInt hash_LockSet_w_wo(const LockSet *ls, 
				     const Mutex *with,
				     const Mutex *without)
{
   UInt i;
   UInt hash = 0;
   
   sk_assert(with == NULL || with != without);

   for(i = 0; with != NULL || i < ls->setsize; i++) {
      const Mutex *mx = i >= ls->setsize ? NULL : ls->mutex[i];

      if (without && mutex_cmp(without, mx) == 0)
	 continue;

      if (with && (mx == NULL || mutex_cmp(with, mx) < 0)) {
	 mx = with;
	 with = NULL;
	 i--;
      }

      hash ^= (UInt)mx->mutexp;
      hash = (hash << 17) | (hash >> (32-17));
   }

   return hash % LOCKSET_HASH_SZ;
}

static inline UInt hash_LockSet_with(const LockSet *ls, const Mutex *with)
{
   UInt hash = hash_LockSet_w_wo(ls, with, NULL);

   if (0)
      VG_(printf)("hash_with %p+%p -> %d\n", ls, with->mutexp, hash);

   return hash;
}

static inline UInt hash_LockSet_without(const LockSet *ls, const Mutex *without)
{
   UInt hash = hash_LockSet_w_wo(ls, NULL, without);

   if (0)
      VG_(printf)("hash_with %p-%p -> %d\n", ls, without->mutexp, hash);

   return hash;
}

static inline UInt hash_LockSet(const LockSet *ls)
{
   UInt hash = hash_LockSet_w_wo(ls, NULL, NULL);

   if (0)
      VG_(printf)("hash %p -> %d\n", ls, hash);

   return hash;
}

static 
Bool structural_eq_LockSet(const LockSet* a, const LockSet* b)
{
   Int i;

   if (a == b)
      return True;
   if (a->setsize != b->setsize)
      return False;

   for(i = 0; i < a->setsize; i++) {
      if (mutex_cmp(a->mutex[i], b->mutex[i]) != 0)
         return False;
   }

   return True;
}


/* Tricky: equivalent to (compare(insert(missing_elem, a), b)), but
 * doesn't do the insertion.  Returns True if they match.
 */
static Bool 
weird_LockSet_equals(const LockSet* a, const LockSet* b, 
                     const Mutex *missing_mutex)
{
   static const Bool debug = False;
   Int ia, ib;

   /* Idea is to try and match each element of b against either an
      element of a, or missing_mutex. */

   if (debug) {
      print_LockSet("weird_LockSet_equals a", a);
      print_LockSet("                     b", b);
      VG_(printf)(  "               missing: %p%(y\n", 
		    missing_mutex->mutexp, missing_mutex->mutexp);
   }

   if ((a->setsize + 1) != b->setsize) {
      if (debug)
	 VG_(printf)("   fastpath length mismatch -> 0\n");
      return False;
   }

   /* There are three phases to this compare:
      1 the section from the start of a up to missing_mutex
      2 missing mutex itself
      3 the section after missing_mutex to the end of a
    */

   ia = 0;
   ib = 0;

   /* 1: up to missing_mutex */
   for(; ia < a->setsize && mutex_cmp(a->mutex[ia], missing_mutex) < 0; ia++, ib++) {
      if (debug) {
	 print_LockSet("     1:a", a);
	 print_LockSet("     1:b", b);
      }
      if (ib == b->setsize || mutex_cmp(a->mutex[ia], b->mutex[ib]) != 0)
	 return False;
   }

   /* 2: missing_mutex itself */
   if (debug) {
      VG_(printf)(  "     2:missing: %p%(y\n", 
		    missing_mutex->mutexp, missing_mutex->mutexp);
      print_LockSet("     2:      b", b);
   }

   sk_assert(ia == a->setsize || mutex_cmp(a->mutex[ia], missing_mutex) >= 0);

   if (ib == b->setsize || mutex_cmp(missing_mutex, b->mutex[ib]) != 0)
      return False;

   ib++;

   /* 3: after missing_mutex to end */

   for(; ia < a->setsize && ib < b->setsize; ia++, ib++) {
      if (debug) {
	 print_LockSet("     3:a", a);
	 print_LockSet("     3:b", b);
      }
      if (mutex_cmp(a->mutex[ia], b->mutex[ib]) != 0)
	 return False;
   }

   if (debug)
      VG_(printf)("  ia=%d ib=%d --> %d\n", ia, ib, ia == a->setsize && ib == b->setsize);

   return ia == a->setsize && ib == b->setsize;
}



static const LockSet *lookup_LockSet(const LockSet *set)
{
   UInt bucket = set->hash;
   LockSet *ret;

   for(ret = lockset_hash[bucket]; ret != NULL; ret = ret->next)
      if (set == ret || structural_eq_LockSet(set, ret))
	 return ret;

   return NULL;
}

static const LockSet *lookup_LockSet_with(const LockSet *set, Mutex *mutex)
{
   UInt bucket = hash_LockSet_with(set, mutex);
   const LockSet *ret;
   
   for(ret = lockset_hash[bucket]; ret != NULL; ret = ret->next)
      if (weird_LockSet_equals(set, ret, mutex))
	 return ret;

   return NULL;
}

static const LockSet *lookup_LockSet_without(const LockSet *set, Mutex *mutex)
{
   UInt bucket = hash_LockSet_without(set, mutex);
   const LockSet *ret;
   
   for(ret = lockset_hash[bucket]; ret != NULL; ret = ret->next)
      if (weird_LockSet_equals(ret, set, mutex))
	 return ret;

   return NULL;
}

static void insert_LockSet(LockSet *set)
{
   UInt hash = hash_LockSet(set);
   
   set->hash = hash;

   sk_assert(lookup_LockSet(set) == NULL);

   set->next = lockset_hash[hash];
   lockset_hash[hash] = set;
}

static inline
LockSet *alloc_LockSet(UInt setsize)
{
   LockSet *ret = VG_(malloc)(sizeof(*ret) + sizeof(Mutex *) * setsize);
   ret->setsize = setsize;
   return ret;
}

static inline
void free_LockSet(LockSet *p)
{
   /* assert: not present in hash */
   VG_(free)(p);
}

void pp_all_LockSets ( void )
{
   Int i;
   Int sets, buckets;

   sets = buckets = 0;
   for (i = 0; i < LOCKSET_HASH_SZ; i++) {
      const LockSet *ls = lockset_hash[i];
      Bool first = True;
      
      for(; ls != NULL; ls = ls->next) {
	 if (first) {
	    buckets++;
	    VG_(printf)("[%4d] = ", i);
	 } else
	    VG_(printf)("         ");

	 sets++;
	 first = False;
	 pp_LockSet(ls);
      }
   }

   VG_(printf)("%d distinct LockSets in %d buckets\n", sets, buckets);
}

static inline Bool isempty(const LockSet *ls)
{
   return ls == NULL || ls->setsize == 0;
}

static Bool ismember(const LockSet *ls, const Mutex *mx)
{
   Int i;

   /* XXX use binary search */
   for(i = 0; i < ls->setsize; i++)
      if (mutex_cmp(mx, ls->mutex[i]) == 0)
	 return True;

   return False;
}

/* Check invariants:
   - all locksets are unique
   - each set is an array in strictly increasing order of mutex addr 
*/
static
void sanity_check_locksets ( const Char* caller )
{
   Int              i;
   const Char *badness;
   LockSet *ls;

   for(i = 0; i < LOCKSET_HASH_SZ; i++) {

      for(ls = lockset_hash[i]; ls != NULL; ls = ls->next) {
	 const Mutex *prev;
	 Int j;

	 if (hash_LockSet(ls) != ls->hash) {
	    badness = "mismatched hash";
	    goto bad;
	 }
	 if (ls->hash != i) {
	    badness = "wrong bucket";
	    goto bad;
	 }
	 if (lookup_LockSet(ls) != ls) {
	    badness = "non-unique set";
	    goto bad;
	 }

	 prev = ls->mutex[0];
	 for(j = 1; j < ls->setsize; j++) {
	    if (mutex_cmp(prev, ls->mutex[j]) >= 0) {
	       badness = "mutexes out of order";
	       goto bad;
	    }
	 }
      }
   }
   return;

  bad:
   VG_(printf)("sanity_check_locksets: "
               "i = %d, ls=%p badness = %s, caller = %s\n", 
               i, ls, badness, caller);
   pp_all_LockSets();
   VG_(skin_panic)("sanity_check_locksets");
}

static
LockSet *add_LockSet(const LockSet *ls, const Mutex *mx)
{
   static const Bool debug = False;
   LockSet *ret = NULL;
   Int i, j;

   if (debug || DEBUG_MEM_LOCKSET_CHANGES) {
      VG_(printf)("add-IN mutex %p%(y\n", mx->mutexp, mx->mutexp);
      print_LockSet("add-IN", ls);
   }

   if (debug || LOCKSET_SANITY)
      sanity_check_locksets("add-IN");

   sk_assert(!ismember(ls, mx));

   ret = alloc_LockSet(ls->setsize+1);

   for(i = j = 0; i < ls->setsize; i++) {
      if (debug)
	 VG_(printf)("i=%d j=%d ls->mutex[i]=%p mx=%p\n",
		    i, j, ls->mutex[i]->mutexp, mx ? mx->mutexp : 0);
      if (mx && mutex_cmp(mx, ls->mutex[i]) < 0) {
	 ret->mutex[j++] = mx;
	 mx = NULL;
      }
      ret->mutex[j++] = ls->mutex[i];
   }

   /* not added in loop - must be after */
   if (mx)
      ret->mutex[j++] = mx;

   sk_assert(j == ret->setsize);

   if (debug || LOCKSET_SANITY) {
      print_LockSet("add-OUT", ret);
      sanity_check_locksets("add-OUT");
   }
   return ret;
}

/* Builds ls with mx removed.  mx should actually be in ls! 
   (a checked assertion).  Resulting set should not already
   exist in the table (unchecked).
*/
static 
LockSet *remove_LockSet ( const LockSet *ls, const Mutex *mx )
{
   static const Bool debug = False;
   LockSet   *ret = NULL;
   Int        i, j;

   if (debug || DEBUG_MEM_LOCKSET_CHANGES) {
      print_LockSet("remove-IN", ls);
   }

   if (debug || LOCKSET_SANITY)
      sanity_check_locksets("remove-IN");

   sk_assert(ismember(ls, mx));

   ret = alloc_LockSet(ls->setsize-1);

   for(i = j = 0; i < ls->setsize; i++) {
      if (mutex_cmp(ls->mutex[i], mx) == 0)
	      continue;
      ret->mutex[j++] = ls->mutex[i];
   }

   sk_assert(j == ret->setsize);

   if (debug || LOCKSET_SANITY) {
      print_LockSet("remove-OUT", ret);
      sanity_check_locksets("remove-OUT");
   }
   return ret;
}


/* Builds the intersection, and then unbuilds it if it's already in the table.
 */
static const LockSet *_intersect(const LockSet *a, const LockSet *b)
{
   static const Bool debug = False;
   Int       iret;
   Int	     ia, ib;
   Int	     size;
   LockSet   *ret;
   const LockSet   *found;

   if (debug || LOCKSET_SANITY)
      sanity_check_locksets("intersect-IN");

   if (debug || DEBUG_MEM_LOCKSET_CHANGES) {
      print_LockSet("intersect a", a);
      print_LockSet("intersect b", b);
   }

   /* count the size of the new set */
   size = 0;
   ia = ib = 0;
   for(size = ia = ib = 0; ia < a->setsize && ib < b->setsize; ) {
      if (mutex_cmp(a->mutex[ia], b->mutex[ib]) == 0) {
	 size++;
	 ia++;
	 ib++;
      } else if (mutex_cmp(a->mutex[ia], b->mutex[ib]) < 0) {
	 ia++;
      } else {
	 sk_assert(mutex_cmp(a->mutex[ia], b->mutex[ib]) > 0);
	 ib++;
      } 
   }

   /* Build the intersection of the two sets */
   ret = alloc_LockSet(size);
   for (iret = ia = ib = 0; ia < a->setsize && ib < b->setsize; ) {
      if (mutex_cmp(a->mutex[ia], b->mutex[ib]) == 0) {
	 sk_assert(iret < ret->setsize);
	 ret->mutex[iret++] = a->mutex[ia];
	 ia++;
	 ib++;
      } else if (mutex_cmp(a->mutex[ia], b->mutex[ib]) < 0) {
	 ia++;
      } else {
	 sk_assert(mutex_cmp(a->mutex[ia], b->mutex[ib]) > 0);
	 ib++;
      } 
   }

   ret->hash = hash_LockSet(ret);

   /* Now search for it in the table, adding it if not seen before */
   found = lookup_LockSet(ret);

   if (found != NULL) {
      free_LockSet(ret);
   } else {
      insert_LockSet(ret);
      found = ret;
   }

   if (debug || LOCKSET_SANITY) {
      print_LockSet("intersect-OUT", found);
      sanity_check_locksets("intersect-OUT");
   }

   return found;
}

/* inline the fastpath */
static inline const LockSet *intersect(const LockSet *a, const LockSet *b)
{
   static const Bool debug = False;

   /* Fast case -- when the two are the same */
   if (a == b) {
      if (debug || DEBUG_MEM_LOCKSET_CHANGES) {
	 print_LockSet("intersect-same fastpath", a);
      }
      return a;
   }

   if (isempty(a) || isempty(b)) {
      if (debug)
	 VG_(printf)("intersect empty fastpath\n");
      return emptyset;
   }

   return _intersect(a, b);
}


static const LockSet *ls_union(const LockSet *a, const LockSet *b)
{
   static const Bool debug = False;
   Int       iret;
   Int	     ia, ib;
   Int	     size;
   LockSet   *ret;
   const LockSet   *found;

   if (debug || LOCKSET_SANITY)
      sanity_check_locksets("union-IN");

   /* Fast case -- when the two are the same */
   if (a == b) {
      if (debug || DEBUG_MEM_LOCKSET_CHANGES) {
	 print_LockSet("union-same fastpath", a);
      }
      return a;
   }

   if (isempty(a)) {
      if (debug)
	 print_LockSet("union a=empty b", b);
      return b;
   }
   if (isempty(b)) {
      if (debug)
	 print_LockSet("union b=empty a", a);
      return a;
   }

   if (debug || DEBUG_MEM_LOCKSET_CHANGES) {
      print_LockSet("union a", a);
      print_LockSet("union b", b);
   }

   /* count the size of the new set */
   for(size = ia = ib = 0; (ia < a->setsize) || (ib < b->setsize); ) {
      Int cmp;

      if ((ia < a->setsize) && (ib < b->setsize))
	 cmp = mutex_cmp(a->mutex[ia], b->mutex[ib]);
      else if (ia == a->setsize)
	 cmp = 1;
      else 
	 cmp = -1;

      if (cmp == 0) {
	 size++;
	 ia++;
	 ib++;
      } else if (cmp < 0) {
	 size++;
	 ia++;
      } else {
	 sk_assert(cmp > 0);
	 size++;
	 ib++;
      } 
   }

   /* Build the intersection of the two sets */
   ret = alloc_LockSet(size);
   for (iret = ia = ib = 0; (ia < a->setsize) || (ib < b->setsize); ) {
      Int cmp;
      sk_assert(iret < ret->setsize);

      if ((ia < a->setsize) && (ib < b->setsize))
	 cmp = mutex_cmp(a->mutex[ia], b->mutex[ib]);
      else if (ia == a->setsize)
	 cmp = 1;
      else 
	 cmp = -1;

      if (cmp == 0) {
	 ret->mutex[iret++] = a->mutex[ia];
	 ia++;
	 ib++;
      } else if (cmp < 0) {
	 ret->mutex[iret++] = a->mutex[ia];
	 ia++;
      } else {
	 sk_assert(cmp > 0);
	 ret->mutex[iret++] = b->mutex[ib];
	 ib++;
      } 
   }

   sk_assert(iret == ret->setsize);

   ret->hash = hash_LockSet(ret);

   /* Now search for it in the table, adding it if not seen before */
   found = lookup_LockSet(ret);

   if (found != NULL) {
      if (debug)
	 print_LockSet("union found existing set", found);
      free_LockSet(ret);
   } else {
      if (debug)
	 print_LockSet("union inserting new set", ret);
      insert_LockSet(ret);
      found = ret;
   }

   if (debug || LOCKSET_SANITY) {
      print_LockSet("union-OUT", found);
      sanity_check_locksets("union-OUT");
   }

   return found;
}

/*------------------------------------------------------------*/
/*--- Shadow chunks info                                   ---*/
/*------------------------------------------------------------*/

#define SHADOW_EXTRA	2

static __inline__
void set_sc_where( ShadowChunk* sc, ExeContext* ec )
{
   sc->skin_extra[0] = (UInt)ec;
}

static __inline__
ExeContext *get_sc_where( ShadowChunk* sc )
{
   return (ExeContext*)sc->skin_extra[0];
}

static __inline__
void set_sc_tid(ShadowChunk *sc, ThreadId tid)
{
   sc->skin_extra[1] = (UInt)tid;
}

static __inline__
ThreadId get_sc_tid(ShadowChunk *sc)
{
   return (ThreadId)sc->skin_extra[1];
}

void SK_(complete_shadow_chunk) ( ShadowChunk* sc, ThreadState* tst )
{
   set_sc_where( sc, VG_(get_ExeContext) ( tst ) );
   set_sc_tid(sc, VG_(get_tid_from_ThreadState(tst)));
}

/*------------------------------------------------------------*/
/*--- Implementation of mutex structure.                   ---*/
/*------------------------------------------------------------*/

static UInt graph_mark;		/* current mark we're using for graph traversal */

static void record_mutex_error(ThreadId tid, Mutex *mutex, 
			       Char *str, ExeContext *ec);
static void record_lockgraph_error(ThreadId tid, Mutex *mutex,
				   const LockSet *lockset_holding, 
				   const LockSet *lockset_prev);

static void set_mutex_state(Mutex *mutex, MutexState state,
			    ThreadId tid, ThreadState *tst);

#define M_MUTEX_HASHSZ	1021

static Mutex *mutex_hash[M_MUTEX_HASHSZ];
static UInt total_mutexes;

static const Char *pp_MutexState(MutexState st)
{
   switch(st) {
   case MxLocked:	return "Locked";
   case MxUnlocked:	return "Unlocked";
   case MxDead:		return "Dead";
   case MxUnknown:	return "Unknown";
   }
   return "???";
}

static void pp_all_mutexes()
{
   Int i;
   Int locks, buckets;

   locks = buckets = 0;
   for(i = 0; i < M_MUTEX_HASHSZ; i++) {
      Mutex *mx;
      Bool first = True;

      for(mx = mutex_hash[i]; mx != NULL; mx = mx->next) {
	 if (first) {
	    buckets++;
	    VG_(printf)("[%4d] = ", i);
	 } else
	    VG_(printf)("         ");
	 locks++;
	 first = False;
	 VG_(printf)("%p [%8s] -> %p%(y\n",
		     mx, pp_MutexState(mx->state), mx->mutexp, mx->mutexp);
      }
   }

   VG_(printf)("%d locks in %d buckets (%d allocated)\n", 
	       locks, buckets, total_mutexes);
}

/* find or create a Mutex for a program's mutex use */
static Mutex *get_mutex(Addr mutexp)
{
   UInt bucket = ((UInt)mutexp) % M_MUTEX_HASHSZ;
   Mutex *mp;
   
   for(mp = mutex_hash[bucket]; mp != NULL; mp = mp->next)
      if (mp->mutexp == mutexp)
	 return mp;

   total_mutexes++;

   mp = VG_(malloc)(sizeof(*mp));
   mp->mutexp = mutexp;
   mp->next = mutex_hash[bucket];
   mutex_hash[bucket] = mp;

   mp->state = MxUnknown;
   mp->tid = VG_INVALID_THREADID;
   mp->location = NULL;

   mp->lockdep = emptyset;
   mp->mark = graph_mark - 1;

   return mp;
}

/* Find all mutexes in a range of memory, and call the callback.
   Remove the mutex from the hash if the callback returns True (mutex
   structure itself is not freed, because it may be pointed to by a
   LockSet. */
static void find_mutex_range(Addr start, Addr end, Bool (*action)(Mutex *))
{
   UInt first = start % M_MUTEX_HASHSZ;
   UInt last = (end+1) % M_MUTEX_HASHSZ;
   UInt i;

   /* Single pass over the hash table, looking for likely hashes */
   for(i = first; i != last; ) {
      Mutex *mx;
      Mutex **prev = &mutex_hash[i];

      for(mx = mutex_hash[i]; mx != NULL; prev = &mx->next, mx = mx->next) {
	 if (mx->mutexp >= start && mx->mutexp < end && (*action)(mx))
	     *prev = mx->next;
      }
      
      if (++i == M_MUTEX_HASHSZ)
	 i = 0;
   }
}

#define N_FREED_CHUNKS	2
static Int freechunkptr = 0;
static ShadowChunk *freechunks[N_FREED_CHUNKS];

/* They're freeing some memory; look to see if it contains any mutexes. */
void SK_(alt_free) ( ShadowChunk* sc, ThreadState* tst )
{
   ThreadId tid = VG_(get_tid_from_ThreadState)(tst);
   Addr start = sc->data;
   Addr end = start + sc->size;

   Bool deadmx(Mutex *mx) {
      if (mx->state != MxDead)
	 set_mutex_state(mx, MxDead, tid, tst);

      return False;
   }

   set_sc_where(sc, VG_(get_ExeContext)(tst));

   /* maintain a small window so that the error reporting machinery
      knows about this memory */
   if (freechunks[freechunkptr] != NULL)
      VG_(free_ShadowChunk)(freechunks[freechunkptr]);
   freechunks[freechunkptr] = sc;

   if (++freechunkptr == N_FREED_CHUNKS)
      freechunkptr = 0;

   /* mark all mutexes in range dead */
   find_mutex_range(start, end, deadmx);
}


#define MARK_LOOP	(graph_mark+0)
#define MARK_DONE	(graph_mark+1)

static Bool check_cycle(const Mutex *start, const LockSet* lockset)
{
   Bool check_cycle_inner(const Mutex *mutex, const LockSet *ls)
   {
      static const Bool debug = False;
      Int i;

      if (mutex->mark == MARK_LOOP)
	 return True;		/* found cycle */
      if (mutex->mark == MARK_DONE)
	 return False;		/* been here before, its OK */

      ((Mutex*)mutex)->mark = MARK_LOOP;
	 
      if (debug)
	 VG_(printf)("mark=%d visiting %p%(y mutex->lockset=%d\n",
		     graph_mark, mutex->mutexp, mutex->mutexp, mutex->lockdep);
      for(i = 0; i < ls->setsize; i++) {
	 const Mutex *mx = ls->mutex[i];

	 if (debug)
	    VG_(printf)("   %y ls=%p (ls->mutex=%p%(y)\n", 
			mutex->mutexp, ls,
			mx->mutexp, mx->mutexp);
	 if (check_cycle_inner(mx, mx->lockdep))
	    return True;
      }
      ((Mutex*)mutex)->mark = MARK_DONE;
	 
      return False;
   }

   graph_mark += 2;		/* clear all marks */

   return check_cycle_inner(start, lockset);
}

/* catch bad mutex state changes (though the common ones are handled
   by core) */
static void set_mutex_state(Mutex *mutex, MutexState state,
			    ThreadId tid, ThreadState *tst)
{
   static const Bool debug = False;

   if (debug)
      VG_(printf)("\ntid %d changing mutex (%p)->%p%(y state %s -> %s\n",
		  tid, mutex, mutex->mutexp, mutex->mutexp,
		  pp_MutexState(mutex->state), pp_MutexState(state));

   if (mutex->state == MxDead) {
      Char *str;

      switch(state) {
      case MxLocked:	str = "lock dead mutex"; break;
      case MxUnlocked:	str = "unlock dead mutex"; break;
      default:		str = "operate on dead mutex"; break;
      }

      /* can't do anything legal to a destroyed mutex */
      record_mutex_error(tid, mutex, str, mutex->location);
      return;
   }

   switch(state) {
   case MxLocked:
      if (mutex->state == MxLocked) {
	 if (mutex->tid != tid)
	    record_mutex_error(tid, mutex, "take lock held by someone else", 
			       mutex->location);
	 else
	    record_mutex_error(tid, mutex, "take lock we already hold", 
			       mutex->location);

	 VG_(skin_panic)("core should have checked this\n");
	 break;
      }

      sk_assert(!check_cycle(mutex, mutex->lockdep));

      if (debug)
	 print_LockSet("thread holding", thread_locks[tid]);

      if (check_cycle(mutex, thread_locks[tid]))
	 record_lockgraph_error(tid, mutex, thread_locks[tid], mutex->lockdep);
      else {
	 mutex->lockdep = ls_union(mutex->lockdep, thread_locks[tid]);

	 if (debug) {
	    VG_(printf)("giving mutex %p%(y lockdep = %p ", 
			mutex->mutexp, mutex->mutexp, mutex->lockdep);
	    print_LockSet("lockdep", mutex->lockdep);
	 }
      }
      mutex->tid = tid;
      break;

   case MxUnlocked:
      if (debug)
	 print_LockSet("thread holding", thread_locks[tid]);

      if (mutex->state != MxLocked) {
	 record_mutex_error(tid, mutex, 
			    "unlock non-locked mutex", mutex->location);
      }
      if (mutex->tid != tid) {
	 record_mutex_error(tid, mutex, 
			    "unlock someone else's mutex", mutex->location);
      }
      mutex->tid = VG_INVALID_THREADID;
      break;

   case MxDead:
      if (mutex->state == MxLocked) {
	 /* forcably remove offending lock from thread's lockset  */
	 sk_assert(ismember(thread_locks[mutex->tid], mutex));
	 thread_locks[mutex->tid] = remove_LockSet(thread_locks[mutex->tid], mutex);
	 mutex->tid = VG_INVALID_THREADID;

	 record_mutex_error(tid, mutex,
			    "free locked mutex", mutex->location);
      }
      break;

   default:
      break;
   }

   mutex->location = VG_(get_ExeContext)(tst);
   mutex->state = state;
}

/*------------------------------------------------------------*/
/*--- Setting and checking permissions.                    ---*/
/*------------------------------------------------------------*/

static
void set_address_range_state ( Addr a, UInt len /* in bytes */, 
                               VgeInitStatus status )
{
   Addr end;

   /* only clean up dead mutexes */
   Bool cleanmx(Mutex *mx) {
      return mx->state == MxDead;
   }


#  if DEBUG_MAKE_ACCESSES
   VG_(printf)("make_access: 0x%x, %u, status=%u\n", a, len, status);
#  endif
   //PROF_EVENT(30); PPP

   if (len == 0)
      return;

   if (len > 100 * 1000 * 1000)
      VG_(message)(Vg_UserMsg,
                   "Warning: set address range state: large range %d",
                   len);

   VGP_PUSHCC(VgpSARP);

   /* Remove mutexes in recycled memory range from hash */
   find_mutex_range(a, a+len, cleanmx);

   /* Memory block may not be aligned or a whole word multiple.  In neat cases,
    * we have to init len/4 words (len is in bytes).  In nasty cases, it's
    * len/4+1 words.  This works out which it is by aligning the block and
    * seeing if the end byte is in the same word as it is for the unaligned
    * block; if not, it's the awkward case. */
   end = (a + len + 3) & ~3;	/* round up */
   a   &= ~3;			/* round down */

   /* Do it ... */
   switch (status) {
   case Vge_VirginInit:
      for ( ; a < end; a += 4) {
         //PROF_EVENT(31);  PPP
         init_virgin_sword(a);
      }
      break;

   case Vge_NonVirginInit:
      for ( ; a < end; a += 4) {
         //PROF_EVENT(31);  PPP
         init_nonvirgin_sword(a);
      }
      break;

   case Vge_SegmentInit:
      for ( ; a < end; a += 4) {
         //PROF_EVENT(31);  PPP
         init_magically_inited_sword(a);
      }
      break;

   case Vge_Error:
      for ( ; a < end; a += 4) {
         //PROF_EVENT(31);  PPP
         init_error_sword(a);
      }
      break;
   
   default:
      VG_(printf)("init_status = %u\n", status);
      VG_(skin_panic)("Unexpected Vge_InitStatus");
   }
      
   /* Check that zero page and highest page have not been written to
      -- this could happen with buggy syscall wrappers.  Today
      (2001-04-26) had precisely such a problem with
      __NR_setitimer. */
   sk_assert(SK_(cheap_sanity_check)());
   VGP_POPCC(VgpSARP);
}


static void make_segment_readable ( Addr a, UInt len )
{
   //PROF_EVENT(??);    PPP
   set_address_range_state ( a, len, Vge_SegmentInit );
}

static void make_writable ( Addr a, UInt len )
{
   //PROF_EVENT(36);  PPP
   set_address_range_state( a, len, Vge_VirginInit );
}

static void make_readable ( Addr a, UInt len )
{
   //PROF_EVENT(37);  PPP
   set_address_range_state( a, len, Vge_NonVirginInit );
}


/* Block-copy states (needed for implementing realloc()). */
static void copy_address_range_state(Addr src, Addr dst, UInt len)
{
   UInt i;

   //PROF_EVENT(40); PPP
   for (i = 0; i < len; i += 4) {
      shadow_word sword = *(get_sword_addr ( src+i ));
      //PROF_EVENT(41);  PPP
      set_sword ( dst+i, sword );
   }
}

// SSS: put these somewhere better
static void eraser_mem_read (Addr a, UInt data_size, ThreadState *tst);
static void eraser_mem_write(Addr a, UInt data_size, ThreadState *tst);

#define REGPARM(x)	__attribute__((regparm (x)))

static void eraser_mem_help_read_1(Addr a) REGPARM(1);
static void eraser_mem_help_read_2(Addr a) REGPARM(1);
static void eraser_mem_help_read_4(Addr a) REGPARM(1);
static void eraser_mem_help_read_N(Addr a, UInt size) REGPARM(2);

static void eraser_mem_help_write_1(Addr a, UInt val) REGPARM(2);
static void eraser_mem_help_write_2(Addr a, UInt val) REGPARM(2);
static void eraser_mem_help_write_4(Addr a, UInt val) REGPARM(2);
static void eraser_mem_help_write_N(Addr a, UInt size) REGPARM(2);

static
void eraser_pre_mem_read(CorePart part, ThreadState* tst,
                         Char* s, UInt base, UInt size )
{
   eraser_mem_read(base, size, tst);
}

static
void eraser_pre_mem_read_asciiz(CorePart part, ThreadState* tst,
                                Char* s, UInt base )
{
   eraser_mem_read(base, VG_(strlen)((Char*)base), tst);
}

static
void eraser_pre_mem_write(CorePart part, ThreadState* tst,
                          Char* s, UInt base, UInt size )
{
   eraser_mem_write(base, size, tst);
}



static
void eraser_new_mem_startup( Addr a, UInt len, Bool rr, Bool ww, Bool xx )
{
   /* Ignore the permissions, just make it readable.  Seems to work... */
   make_segment_readable(a, len);
}


static
void eraser_new_mem_heap ( Addr a, UInt len, Bool is_inited )
{
   if (is_inited) {
      make_readable(a, len);
   } else {
      make_writable(a, len);
   }
}

static
void eraser_set_perms (Addr a, UInt len,
                       Bool rr, Bool ww, Bool xx)
{
   if      (rr) make_readable(a, len);
   else if (ww) make_writable(a, len);
   /* else do nothing */
}


/*--------------------------------------------------------------*/
/*--- Initialise the memory audit system on program startup. ---*/
/*--------------------------------------------------------------*/

static 
void init_shadow_memory(void)
{
   Int i;

   for (i = 0; i < ESEC_MAP_WORDS; i++)
      distinguished_secondary_map.swords[i] = virgin_sword;

   /* These entries gradually get overwritten as the used address
      space expands. */
   for (i = 0; i < 65536; i++)
      primary_map[i] = &distinguished_secondary_map;
}


/*--------------------------------------------------------------*/
/*--- Machinery to support sanity checking                   ---*/
/*--------------------------------------------------------------*/

/* Check that nobody has spuriously claimed that the first or last 16
   pages (64 KB) of address space have become accessible.  Failure of
   the following do not per se indicate an internal consistency
   problem, but they are so likely to that we really want to know
   about it if so. */

Bool SK_(cheap_sanity_check) ( void )
{
   if (VGE_IS_DISTINGUISHED_SM(primary_map[0]) && 
       VGE_IS_DISTINGUISHED_SM(primary_map[65535]))
      return True;
   else
      return False;
}


Bool SK_(expensive_sanity_check)(void)
{
   Int i;

   /* Make sure nobody changed the distinguished secondary. */
   for (i = 0; i < ESEC_MAP_WORDS; i++)
      if (distinguished_secondary_map.swords[i].other != virgin_sword.other ||
          distinguished_secondary_map.swords[i].state != virgin_sword.state)
         return False;

   return True;
}


/*--------------------------------------------------------------*/
/*--- Instrumentation                                        ---*/
/*--------------------------------------------------------------*/

/* Create and return an instrumented version of cb_in.  Free cb_in
   before returning. */
UCodeBlock* SK_(instrument) ( UCodeBlock* cb_in, Addr not_used )
{
   UCodeBlock* cb;
   Int         i;
   UInstr*     u_in;
   Int         t_size = INVALID_TEMPREG;

   cb = VG_(alloc_UCodeBlock)();
   cb->nextTemp = cb_in->nextTemp;

   for (i = 0; i < cb_in->used; i++) {
      u_in = &cb_in->instrs[i];

      switch (u_in->opcode) {

         case NOP: case CALLM_S: case CALLM_E:
            break;

         case LOAD: {
	    void (*help)(Addr);
	    sk_assert(1 == u_in->size || 2 == u_in->size || 4 == u_in->size);
	    
	    switch(u_in->size) {
	    case 1: help = eraser_mem_help_read_1; break;
	    case 2: help = eraser_mem_help_read_2; break;
	    case 4: help = eraser_mem_help_read_4; break;
	    default:
	       VG_(skin_panic)("bad size");
	    }
	    
	    uInstr1(cb, CCALL, 0, TempReg, u_in->val1);
	    uCCall(cb, (Addr)help, 1, 1, False);

	    VG_(copy_UInstr)(cb, u_in);
	    t_size = INVALID_TEMPREG;
	    break;
	 }

         case FPU_R: {
            sk_assert(1 == u_in->size || 2 == u_in->size || 4 == u_in->size || 
                      8 == u_in->size || 10 == u_in->size);
	    
	       t_size = newTemp(cb);
	       uInstr2(cb, MOV,   4, Literal, 0, TempReg, t_size);
	       uLiteral(cb, (UInt)u_in->size);

	       uInstr2(cb, CCALL, 0, TempReg, u_in->val2, TempReg, t_size);
	       uCCall(cb, (Addr) & eraser_mem_help_read_N, 2, 2, False);

	       VG_(copy_UInstr)(cb, u_in);
	       t_size = INVALID_TEMPREG;
	       break;
	 } 

         case STORE: {
	    void (*help)(Addr, UInt);
            sk_assert(1 == u_in->size || 2 == u_in->size || 4 == u_in->size);
	    
	    switch(u_in->size) {
	    case 1: help = eraser_mem_help_write_1; break;
	    case 2: help = eraser_mem_help_write_2; break;
	    case 4: help = eraser_mem_help_write_4; break;
	    default:
	       VG_(skin_panic)("bad size");
	    }

	    uInstr2(cb, CCALL, 0, TempReg, u_in->val2, TempReg, u_in->val1);
	    uCCall(cb, (Addr)help, 2, 2, False);

	    VG_(copy_UInstr)(cb, u_in);
	    t_size = INVALID_TEMPREG;
	    break;
	 }

         case FPU_W: {
            sk_assert(1 == u_in->size || 2 == u_in->size || 4 == u_in->size || 
                      8 == u_in->size || 10 == u_in->size);

	    t_size = newTemp(cb);
	    uInstr2(cb, MOV,   4, Literal, 0, TempReg, t_size);
	    uLiteral(cb, (UInt)u_in->size);
	    uInstr2(cb, CCALL, 0, TempReg, u_in->val2, TempReg, t_size);
	    uCCall(cb, (Addr) & eraser_mem_help_write_N, 2, 2, False);

	    VG_(copy_UInstr)(cb, u_in);
	    t_size = INVALID_TEMPREG;
	    break;
	 }

         default:
            VG_(copy_UInstr)(cb, u_in);
            break;
      }
   }

   VG_(free_UCodeBlock)(cb_in);
   return cb;
}


/*--------------------------------------------------------------------*/
/*--- Error and suppression handling                               ---*/
/*--------------------------------------------------------------------*/

typedef
   enum {
      /* Possible data race */
      EraserSupp
   }
   EraserSuppKind;

/* What kind of error it is. */
typedef
   enum { 
      EraserErr,		/* data-race */
      MutexErr,			/* mutex operations */
      LockGraphErr,		/* mutex order error */
   }
   EraserErrorKind;

/* The classification of a faulting address. */
typedef 
   enum { Undescribed, /* as-yet unclassified */
          Stack, 
          Unknown, /* classification yielded nothing useful */
          Mallocd,
	  Freed,
	  Segment
   }
   AddrKind;
/* Records info about a faulting address. */
typedef
   struct {
      /* ALL */
      AddrKind akind;
      /* Freed, Mallocd */
      Int blksize;
      /* Freed, Mallocd */
      Int rwoffset;
      /* Freed, Mallocd */
      ExeContext* lastchange;
      ThreadId lasttid;
      /* Stack */
      ThreadId stack_tid;
      /* Segment */
      const Char* filename;
      const Char* section;
      /* True if is just-below %esp -- could be a gcc bug. */
      Bool maybe_gcc;
   }
   AddrInfo;

/* What kind of memory access is involved in the error? */
typedef
   enum { ReadAxs, WriteAxs, ExecAxs }
   AxsKind;

/* Extra context for memory errors */
typedef
   struct {
      AxsKind axskind;
      Int size;
      AddrInfo addrinfo;
      Bool isWrite;
      shadow_word prevstate;
      /* MutexErr, LockGraphErr */
      Mutex      *mutex;
      ExeContext *lasttouched;
      ThreadId    lasttid;
      /* LockGraphErr */
      const LockSet    *held_lockset;
      const LockSet    *prev_lockset;
   }
   HelgrindError;

static __inline__
void clear_AddrInfo ( AddrInfo* ai )
{
   ai->akind      = Unknown;
   ai->blksize    = 0;
   ai->rwoffset   = 0;
   ai->lastchange = NULL;
   ai->lasttid    = VG_INVALID_THREADID;
   ai->filename   = NULL;
   ai->section    = "???";
   ai->stack_tid  = VG_INVALID_THREADID;
   ai->maybe_gcc  = False;
}

static __inline__
void clear_HelgrindError ( HelgrindError* err_extra )
{
   err_extra->axskind    = ReadAxs;
   err_extra->size       = 0;
   err_extra->mutex      = NULL;
   err_extra->lasttouched= NULL;
   err_extra->lasttid    = VG_INVALID_THREADID;
   err_extra->prev_lockset = 0;
   err_extra->held_lockset = 0;
   err_extra->prevstate.state  = Vge_Virgin;
   err_extra->prevstate.other  = 0;
   clear_AddrInfo ( &err_extra->addrinfo );
   err_extra->isWrite    = False;
}



/* Describe an address as best you can, for error messages,
   putting the result in ai. */

static void describe_addr ( Addr a, AddrInfo* ai )
{
   ShadowChunk* sc;
   Int i;

   /* Nested functions, yeah.  Need the lexical scoping of 'a'. */ 

   /* Closure for searching thread stacks */
   Bool addr_is_in_bounds(Addr stack_min, Addr stack_max)
   {
      return (stack_min <= a && a <= stack_max);
   }
   /* Closure for searching malloc'd and free'd lists */
   Bool addr_is_in_block(ShadowChunk *sh_ch)
   {
      return VG_(addr_is_in_block) ( a, sh_ch->data, sh_ch->size );
   }

   /* Search for it in segments */
   {
      const SegInfo *seg;

      for(seg = VG_(next_seginfo)(NULL); 
	  seg != NULL; 
	  seg = VG_(next_seginfo)(seg)) {
	 Addr base = VG_(seg_start)(seg);
	 UInt size = VG_(seg_size)(seg);
	 const UChar *filename = VG_(seg_filename)(seg);

	 if (a >= base && a < base+size) {
	    ai->akind = Segment;
	    ai->blksize = size;
	    ai->rwoffset = a - base;
	    ai->filename = filename;

	    switch(VG_(seg_sect_kind)(a)) {
	    case Vg_SectText:	ai->section = "text"; break;
	    case Vg_SectData:	ai->section = "data"; break;
	    case Vg_SectBSS:	ai->section = "BSS"; break;
	    case Vg_SectGOT:	ai->section = "GOT"; break;
	    case Vg_SectPLT:	ai->section = "PLT"; break;
	    case Vg_SectUnknown:
	    default:
	       ai->section = "???"; break;
	    }

	    return;
	 }
      }
   }

   /* Search for a currently malloc'd block which might bracket it. */
   sc = VG_(any_matching_mallocd_ShadowChunks)(addr_is_in_block);
   if (NULL != sc) {
      ai->akind      = Mallocd;
      ai->blksize    = sc->size;
      ai->rwoffset   = (Int)(a) - (Int)(sc->data);
      ai->lastchange = get_sc_where(sc);
      ai->lasttid    = get_sc_tid(sc);
      return;
   } 

   /* Look in recently freed memory */
   for(i = 0; i < N_FREED_CHUNKS; i++) {
      sc = freechunks[i];
      if (sc == NULL)
	 continue;

      if (a >= sc->data && a < sc->data+sc->size) {
	 ai->akind      = Freed;
	 ai->blksize    = sc->size;
	 ai->rwoffset   = a - sc->data;
	 ai->lastchange = get_sc_where(sc);
	 ai->lasttid    = get_sc_tid(sc);
	 return;
      } 
   }
 
   /* Clueless ... */
   ai->akind = Unknown;
   return;
}


/* Creates a copy of the err_extra, updates the copy with address info if
   necessary, sticks the copy into the SkinError. */
void SK_(dup_extra_and_update)(SkinError* err)
{
   HelgrindError* err_extra;

   err_extra  = VG_(malloc)(sizeof(HelgrindError));
   *err_extra = *((HelgrindError*)err->extra);

   if (err_extra->addrinfo.akind == Undescribed)
      describe_addr ( err->addr, &(err_extra->addrinfo) );

   err->extra = err_extra;
}

static void record_eraser_error ( ThreadState *tst, Addr a, Bool is_write,
				  shadow_word prevstate )
{
   HelgrindError err_extra;

   n_eraser_warnings++;

   clear_HelgrindError(&err_extra);
   err_extra.isWrite = is_write;
   err_extra.addrinfo.akind = Undescribed;
   err_extra.prevstate = prevstate;

   VG_(maybe_record_error)( tst, EraserErr, a, 
                            (is_write ? "writing" : "reading"),
                            &err_extra);

   set_sword(a, error_sword);
}

static void record_mutex_error(ThreadId tid, Mutex *mutex, 
			       Char *str, ExeContext *ec)
{
   HelgrindError err_extra;

   clear_HelgrindError(&err_extra);
   err_extra.addrinfo.akind = Undescribed;
   err_extra.mutex = mutex;
   err_extra.lasttouched = ec;
   err_extra.lasttid = tid;

   VG_(maybe_record_error)(VG_(get_ThreadState)(tid), MutexErr, 
			   (Addr)mutex->mutexp, str, &err_extra);
}

static void record_lockgraph_error(ThreadId tid, Mutex *mutex,
				   const LockSet *lockset_holding,
				   const LockSet *lockset_prev)
{
   HelgrindError err_extra;

   n_lockorder_warnings++;

   clear_HelgrindError(&err_extra);
   err_extra.addrinfo.akind = Undescribed;
   err_extra.mutex = mutex;
   
   err_extra.lasttouched = mutex->location;
   err_extra.held_lockset = lockset_holding;
   err_extra.prev_lockset = lockset_prev;
   
   VG_(maybe_record_error)(VG_(get_ThreadState)(tid), LockGraphErr, 
			   mutex->mutexp, "", &err_extra);
}

Bool SK_(eq_SkinError) ( VgRes not_used,
                          SkinError* e1, SkinError* e2 )
{
   sk_assert(e1->ekind == e2->ekind);

   switch(e1->ekind) {
   case EraserErr:
      return e1->addr == e2->addr;

   case MutexErr:
      return e1->addr == e2->addr;
   }

   if (e1->string != e2->string) return False;
   if (0 != VG_(strcmp)(e1->string, e2->string)) return False;
   return True;
}

static void pp_AddrInfo ( Addr a, AddrInfo* ai )
{
   switch (ai->akind) {
      case Stack: 
         VG_(message)(Vg_UserMsg, 
                      "  Address %p is on thread %d's stack", 
                      a, ai->stack_tid);
         break;
      case Unknown:
         if (ai->maybe_gcc) {
            VG_(message)(Vg_UserMsg, 
               "  Address %p is just below %%esp.  Possibly a bug in GCC/G++",
               a);
            VG_(message)(Vg_UserMsg, 
               "   v 2.96 or 3.0.X.  To suppress, use: --workaround-gcc296-bugs=yes");
	 } else {
            VG_(message)(Vg_UserMsg, 
               "  Address %p is not stack'd, malloc'd or free'd", a);
         }
         break;
      case Segment:
	VG_(message)(Vg_UserMsg,
		     "  Address %p is in %s section of %s", 
		     a, ai->section, ai->filename);
	break;
      case Mallocd:
      case Freed: {
         UInt delta;
         UChar* relative;
         if (ai->rwoffset < 0) {
            delta    = (UInt)(- ai->rwoffset);
            relative = "before";
         } else if (ai->rwoffset >= ai->blksize) {
            delta    = ai->rwoffset - ai->blksize;
            relative = "after";
         } else {
            delta    = ai->rwoffset;
            relative = "inside";
         }
	 VG_(message)(Vg_UserMsg, 
		      "  Address %p is %d bytes %s a block of size %d %s by thread %d at",
		      a, delta, relative, 
		      ai->blksize,
		      ai->akind == Mallocd ? "alloc'd" : "freed",
		      ai->lasttid);

         VG_(pp_ExeContext)(ai->lastchange);
         break;
      }   
      default:
         VG_(skin_panic)("pp_AddrInfo");
   }
}

static Char *lockset_str(const Char *prefix, const LockSet *lockset)
{
   Char *buf, *cp;
   Int i;

   buf = VG_(malloc)((prefix == NULL ? 0 : VG_(strlen)(prefix)) +
		     lockset->setsize * 120 +
		     1);

   cp = buf;
   if (prefix)
      cp += VG_(sprintf)(cp, "%s", prefix);

   for(i = 0; i < lockset->setsize; i++)
      cp += VG_(sprintf)(cp, "%p%(y, ", lockset->mutex[i]->mutexp, 
			 lockset->mutex[i]->mutexp);

   if (lockset->setsize)
      cp[-2] = '\0';
   else
      *cp = '\0';

   return buf;
}

void SK_(pp_SkinError) ( SkinError* err, void (*pp_ExeContext)(void) )
{
   HelgrindError *extra = (HelgrindError *)err->extra;
   Char buf[100];
   Char *msg = buf;
   const LockSet *ls;

   *msg = '\0';

   switch(err->ekind) {
   case EraserErr:
      VG_(message)(Vg_UserMsg, "Possible data race %s variable at %p %(y",
		   err->string, err->addr, err->addr );
      pp_ExeContext();

      switch(extra->prevstate.state) {
      case Vge_Virgin:
	 /* shouldn't be possible to go directly from virgin -> error */
	 VG_(sprintf)(buf, "virgin!?");
	 break;

      case Vge_Excl:
	 sk_assert(extra->prevstate.other != TID_INDICATING_ALL);
	 VG_(sprintf)(buf, "exclusively owned by thread %d", extra->prevstate.other);
	 break;

      case Vge_Shar:
      case Vge_SharMod:
	 ls = getLockSet(extra->prevstate.other);

	 if (isempty(ls)) {
	    VG_(sprintf)(buf, "shared %s, no locks", 
			 extra->prevstate.state == Vge_Shar ? "RO" : "RW");
	    break;
	 }

	 msg = lockset_str(extra->prevstate.state == Vge_Shar ?
			   "shared RO, locked by:" :
			   "shared RW, locked by:", ls);

	 break;
      }

      if (*msg) {
	 VG_(message)(Vg_UserMsg, "  Previous state: %s", msg);
      }
      pp_AddrInfo(err->addr, &extra->addrinfo);
      break;

   case MutexErr:
      VG_(message)(Vg_UserMsg, "Mutex problem at %p%(y trying to %s at",
		   err->addr, err->addr, err->string );
      pp_ExeContext();
      if (extra->lasttouched) {
	 VG_(message)(Vg_UserMsg, "  last touched by thread %d at", extra->lasttid);
	 VG_(pp_ExeContext)(extra->lasttouched);
      }
      pp_AddrInfo(err->addr, &extra->addrinfo);
      break;

   case LockGraphErr: {
      const LockSet *heldset = extra->held_lockset;
      Int i;

      msg = lockset_str(NULL, heldset);

      VG_(message)(Vg_UserMsg, "Mutex %p%(y locked in inconsistent order",
		   err->addr, err->addr);
      pp_ExeContext();
      VG_(message)(Vg_UserMsg, " while holding locks %s", msg);

      for(i = 0; i < heldset->setsize; i++) {
	 const Mutex *lsmx = heldset->mutex[i];

	 if (!ismember(lsmx->lockdep, extra->mutex))
	    continue;
      
	 VG_(message)(Vg_UserMsg, "  %p%(y last locked at", 
		      lsmx->mutexp, lsmx->mutexp);
	 VG_(pp_ExeContext)(lsmx->location);
	 VG_(free)(msg);
	 msg = lockset_str(NULL, lsmx->lockdep);
	 VG_(message)(Vg_UserMsg, "  while depending on locks %s", msg);
      }
      
      break;
   }
   }

   if (msg != buf)
      VG_(free)(msg);
}


Bool SK_(recognised_suppression) ( Char* name, SuppKind *skind )
{
   if (0 == VG_(strcmp)(name, "Eraser")) {
      *skind = EraserSupp;
      return True;
   } else {
      return False;
   }
}


Bool SK_(read_extra_suppression_info) ( Int fd, Char* buf, 
                                        Int nBuf, SkinSupp* s )
{
   /* do nothing -- no extra suppression info present.  Return True to
      indicate nothing bad happened. */
   return True;
}


Bool SK_(error_matches_suppression)(SkinError* err, SkinSupp* su)
{
   sk_assert( su->skind == EraserSupp);
   sk_assert(err->ekind == EraserErr);
   return True;
}


static void eraser_post_mutex_lock(ThreadId tid, void* void_mutex)
{
   static const Bool debug = False;
   Mutex *mutex = get_mutex((Addr)void_mutex);
   const LockSet*  ls;

   set_mutex_state(mutex, MxLocked, tid, VG_(get_ThreadState)(tid));

#  if DEBUG_LOCKS
   VG_(printf)("lock  (%u, %p)\n", tid, mutex->mutexp);
#  endif

   /* VG_(printf)("LOCK: held %d, new %p\n", thread_locks[tid], mutex); */
#  if LOCKSET_SANITY > 1
   sanity_check_locksets("eraser_post_mutex_lock-IN");
#  endif

   ls = lookup_LockSet_with(thread_locks[tid], mutex);

   if (ls == NULL) {
      LockSet *newset = add_LockSet(thread_locks[tid], mutex);
      insert_LockSet(newset);
      ls = newset;
   }
   thread_locks[tid] = ls;

   if (debug || DEBUG_LOCKS)
      VG_(printf)("tid %u now has lockset %p\n", tid, ls);

   if (debug || LOCKSET_SANITY > 1)
      sanity_check_locksets("eraser_post_mutex_lock-OUT");
}


static void eraser_post_mutex_unlock(ThreadId tid, void* void_mutex)
{
   static const Bool debug = False;
   Int i = 0;
   Mutex *mutex = get_mutex((Addr)void_mutex);
   const LockSet *ls;

   set_mutex_state(mutex, MxUnlocked, tid, VG_(get_ThreadState)(tid));

   if (!ismember(thread_locks[tid], mutex))
       return;

   if (debug || DEBUG_LOCKS)
      VG_(printf)("unlock(%u, %p%(y)\n", tid, mutex->mutexp, mutex->mutexp);

   if (debug || LOCKSET_SANITY > 1)
      sanity_check_locksets("eraser_post_mutex_unlock-IN");

   ls = lookup_LockSet_without(thread_locks[tid], mutex);

   if (ls == NULL) {
      LockSet *newset = remove_LockSet(thread_locks[tid], mutex);
      insert_LockSet(newset);
      ls = newset;
   }

   /* Update the thread's lock vector */
   if (debug || DEBUG_LOCKS)
      VG_(printf)("tid %u reverts from %p to lockset %p\n", 
		  tid, thread_locks[tid], i);

   thread_locks[tid] = ls;

   if (debug || LOCKSET_SANITY > 1)
      sanity_check_locksets("eraser_post_mutex_unlock-OUT");
}


/* ---------------------------------------------------------------------
   Checking memory reads and writes
   ------------------------------------------------------------------ */

/* Behaviour on reads and writes:
 *
 *                      VIR          EXCL        SHAR        SH_MOD
 * ----------------------------------------------------------------
 * rd/wr, 1st thread |  -            EXCL        -           -
 * rd, new thread    |  -            SHAR        -           -
 * wr, new thread    |  -            SH_MOD      -           -
 * rd                |  error!       -           SHAR        SH_MOD
 * wr                |  EXCL         -           SH_MOD      SH_MOD
 * ----------------------------------------------------------------
 */

#if 0
static 
void dump_around_a(Addr a)
{
   UInt i;
   shadow_word* sword;
   VG_(printf)("NEARBY:\n");
   for (i = a - 12; i <= a + 12; i += 4) {
      sword = get_sword_addr(i); 
      VG_(printf)("    %x -- tid: %u, state: %u\n", i, sword->other, sword->state);
   }
}
#endif

/* Find which word the first and last bytes are in (by shifting out bottom 2
 * bits) then find the difference. */
static __inline__ 
Int compute_num_words_accessed(Addr a, UInt size) 
{
   Int x, y, n_words;
   x =  a             >> 2;
   y = (a + size - 1) >> 2;
   n_words = y - x + 1;
   return n_words;
}


#if DEBUG_ACCESSES
   #define DEBUG_STATE(args...)   \
      VG_(printf)("(%u) ", size), \
      VG_(printf)(args)
#else
   #define DEBUG_STATE(args...)
#endif


static void eraser_mem_read(Addr a, UInt size, ThreadState *tst)
{
   ThreadId tid;
   shadow_word* sword;
   Addr     end = a + 4*compute_num_words_accessed(a, size);
   shadow_word  prevstate;
   const LockSet *ls;

   tid = (tst == NULL) ? VG_(get_current_tid)() : VG_(get_tid_from_ThreadState)(tst);

   for ( ; a < end; a += 4) {

      sword = get_sword_addr(a);
      if (sword == SEC_MAP_ACCESS) {
         VG_(printf)("read distinguished 2ndary map! 0x%x\n", a);
         continue;
      }

      prevstate = *sword;

      switch (sword->state) {

      /* This looks like reading of unitialised memory, may be legit.  Eg. 
       * calloc() zeroes its values, so untouched memory may actually be 
       * initialised.   Leave that stuff to Valgrind.  */
      case Vge_Virgin:
         if (TID_INDICATING_NONVIRGIN == sword->other) {
            DEBUG_STATE("Read  VIRGIN --> EXCL:   %8x, %u\n", a, tid);
#           if DEBUG_VIRGIN_READS
            dump_around_a(a);
#           endif
         } else {
            DEBUG_STATE("Read  SPECIAL --> EXCL:  %8x, %u\n", a, tid);
         }
         sword->state = Vge_Excl;
         sword->other = tid;       /* remember exclusive owner */
         break;

      case Vge_Excl:
         if (tid == sword->other) {
            DEBUG_STATE("Read  EXCL:              %8x, %u\n", a, tid);
         } else if (TID_INDICATING_ALL == sword->other) {
            DEBUG_STATE("Read  EXCL/ERR:          %8x, %u\n", a, tid);
	 } else {
            DEBUG_STATE("Read  EXCL(%u) --> SHAR:  %8x, %u\n", sword->other, a, tid);
            sword->state = Vge_Shar;
            sword->other = getLockSetId(thread_locks[tid]);
	    if (DEBUG_MEM_LOCKSET_CHANGES)
	       print_LockSet("excl read locks", getLockSet(sword->other));
         }
         break;

      case Vge_Shar:
         DEBUG_STATE("Read  SHAR:              %8x, %u\n", a, tid);
         sword->other = getLockSetId(intersect(getLockSet(sword->other), 
					       thread_locks[tid]));
         break;

      case Vge_SharMod:
         DEBUG_STATE("Read  SHAR_MOD:          %8x, %u\n", a, tid);
	 ls = intersect(getLockSet(sword->other), 
			thread_locks[tid]);
         sword->other = getLockSetId(ls);

         if (isempty(ls)) {
            record_eraser_error(tst, a, False /* !is_write */, prevstate);
         }
         break;

      default:
         VG_(skin_panic)("Unknown eraser state");
      }
   }
}

static void eraser_mem_write(Addr a, UInt size, ThreadState *tst)
{
   ThreadId tid;
   shadow_word* sword;
   Addr     end = a + 4*compute_num_words_accessed(a, size);
   shadow_word  prevstate;

   tid = (tst == NULL) ? VG_(get_current_tid)() : VG_(get_tid_from_ThreadState)(tst);

   for ( ; a < end; a += 4) {

      sword = get_sword_addr(a);
      if (sword == SEC_MAP_ACCESS) {
         VG_(printf)("read distinguished 2ndary map! 0x%x\n", a);
         continue;
      }

      prevstate = *sword;

      switch (sword->state) {
      case Vge_Virgin:
         if (TID_INDICATING_NONVIRGIN == sword->other)
            DEBUG_STATE("Write VIRGIN --> EXCL:   %8x, %u\n", a, tid);
         else
            DEBUG_STATE("Write SPECIAL --> EXCL:  %8x, %u\n", a, tid);
         sword->state = Vge_Excl;
         sword->other = tid;       /* remember exclusive owner */
         break;

      case Vge_Excl:
         if (tid == sword->other) {
            DEBUG_STATE("Write EXCL:              %8x, %u\n", a, tid);
            break;
         } else if (TID_INDICATING_ALL == sword->other) {
            DEBUG_STATE("Write EXCL/ERR:          %8x, %u\n", a, tid);
	    break;
         } else {
            DEBUG_STATE("Write EXCL(%u) --> SHAR_MOD: %8x, %u\n", sword->other, a, tid);
            sword->state = Vge_SharMod;
            sword->other = getLockSetId(thread_locks[tid]);
	    if(DEBUG_MEM_LOCKSET_CHANGES)
	       print_LockSet("excl write locks", getLockSet(sword->other));
            goto SHARED_MODIFIED;
         }

      case Vge_Shar:
         DEBUG_STATE("Write SHAR --> SHAR_MOD: %8x, %u\n", a, tid);
         sword->state = Vge_SharMod;
         sword->other = getLockSetId(intersect(getLockSet(sword->other),
					       thread_locks[tid]));
         goto SHARED_MODIFIED;

      case Vge_SharMod:
         DEBUG_STATE("Write SHAR_MOD:          %8x, %u\n", a, tid);
         sword->other = getLockSetId(intersect(getLockSet(sword->other), 
					       thread_locks[tid]));
         SHARED_MODIFIED:
	 if (isempty(getLockSet(sword->other))) {
            record_eraser_error(tst, a, True /* is_write */, prevstate);
         }
         break;

      default:
         VG_(skin_panic)("Unknown eraser state");
      }
   }
}

#undef DEBUG_STATE

static void eraser_mem_help_read_1(Addr a)
{
   eraser_mem_read(a, 1, NULL);
}

static void eraser_mem_help_read_2(Addr a)
{
   eraser_mem_read(a, 2, NULL);
}

static void eraser_mem_help_read_4(Addr a)
{
   eraser_mem_read(a, 4, NULL);
}

static void eraser_mem_help_read_N(Addr a, UInt size)
{
   eraser_mem_read(a, size, NULL);
}

static void eraser_mem_help_write_1(Addr a, UInt val)
{
   if (*(UChar *)a != val)
      eraser_mem_write(a, 1, NULL);
}
static void eraser_mem_help_write_2(Addr a, UInt val)
{
   if (*(UShort *)a != val)
      eraser_mem_write(a, 2, NULL);
}
static void eraser_mem_help_write_4(Addr a, UInt val)
{
   if (*(UInt *)a != val)
      eraser_mem_write(a, 4, NULL);
}
static void eraser_mem_help_write_N(Addr a, UInt size)
{
   eraser_mem_write(a, size, NULL);
}

/*--------------------------------------------------------------------*/
/*--- Client requests                                              ---*/
/*--------------------------------------------------------------------*/

Bool SK_(handle_client_request)(ThreadState *tst, UInt *args, UInt *ret)
{
   if (!VG_IS_SKIN_USERREQ('H','G',args[0]))
      return False;

   switch(args[0]) {
   case VG_USERREQ__HG_CLEAN_MEMORY:
      set_address_range_state(args[1], args[2], Vge_VirginInit);
      *ret = 0;			/* meaningless */
      break;

   case VG_USERREQ__HG_KNOWN_RACE:
      set_address_range_state(args[1], args[2], Vge_Error);
      *ret = 0;			/* meaningless */
      break;

   default:
      return False;
   }

   return True;
}


/*--------------------------------------------------------------------*/
/*--- Setup                                                        ---*/
/*--------------------------------------------------------------------*/

void SK_(pre_clo_init)(VgDetails* details, VgNeeds* needs, VgTrackEvents* track)
{
   Int i;
   LockSet *empty;

   details->name             = "Helgrind";
   details->version          = NULL;
   details->description      = "a data race detector";
   details->copyright_author =
      "Copyright (C) 2002, and GNU GPL'd, by Nicholas Nethercote.";
   details->bug_reports_to   = "njn25@cam.ac.uk";

   needs->core_errors           = True;
   needs->skin_errors           = True;
   needs->data_syms             = True;
   needs->sizeof_shadow_block	= SHADOW_EXTRA;
   needs->alternative_free      = True;
   needs->client_requests       = True;

   track->new_mem_startup       = & eraser_new_mem_startup;
   track->new_mem_heap          = & eraser_new_mem_heap;
   track->new_mem_stack         = & make_writable;
   track->new_mem_stack_aligned = & make_writable_aligned;
   track->new_mem_stack_signal  = & make_writable;
   track->new_mem_brk           = & make_writable;
   track->new_mem_mmap          = & eraser_new_mem_startup;

   track->copy_mem_heap         = & copy_address_range_state;
   track->change_mem_mprotect   = & eraser_set_perms;

   track->ban_mem_heap          = NULL;
   track->ban_mem_stack         = NULL;

   track->die_mem_heap          = NULL;
   track->die_mem_stack         = NULL;
   track->die_mem_stack_aligned = NULL;
   track->die_mem_stack_signal  = NULL;
   track->die_mem_brk           = NULL;
   track->die_mem_munmap        = NULL;

   track->pre_mem_read          = & eraser_pre_mem_read;
   track->pre_mem_read_asciiz   = & eraser_pre_mem_read_asciiz;
   track->pre_mem_write         = & eraser_pre_mem_write;
   track->post_mem_write        = NULL;

   track->post_mutex_lock       = & eraser_post_mutex_lock;
   track->post_mutex_unlock     = & eraser_post_mutex_unlock;

   VG_(register_compact_helper)((Addr) & eraser_mem_help_read_1);
   VG_(register_compact_helper)((Addr) & eraser_mem_help_read_2);
   VG_(register_compact_helper)((Addr) & eraser_mem_help_read_4);
   VG_(register_noncompact_helper)((Addr) & eraser_mem_help_read_N);

   VG_(register_compact_helper)((Addr) & eraser_mem_help_write_1);
   VG_(register_compact_helper)((Addr) & eraser_mem_help_write_2);
   VG_(register_compact_helper)((Addr) & eraser_mem_help_write_4);
   VG_(register_noncompact_helper)((Addr) & eraser_mem_help_write_N);

   for(i = 0; i < LOCKSET_HASH_SZ; i++)
      lockset_hash[i] = NULL;

   empty = alloc_LockSet(0);
   insert_LockSet(empty);
   emptyset = empty;

   /* Init lock table */
   for (i = 0; i < VG_N_THREADS; i++) 
      thread_locks[i] = empty;

   init_shadow_memory();
}


void SK_(post_clo_init)(void)
{
}


void SK_(fini)(void)
{
   if (DEBUG_LOCK_TABLE) {
      pp_all_LockSets();
      pp_all_mutexes();
   }

   if (LOCKSET_SANITY)
      sanity_check_locksets("SK_(fini)");

   VG_(message)(Vg_UserMsg, "%u possible data races found; %u lock order problems",
		n_eraser_warnings, n_lockorder_warnings);
}

/*--------------------------------------------------------------------*/
/*--- end                                                hg_main.c ---*/
/*--------------------------------------------------------------------*/
