
/*--------------------------------------------------------------------*/
/*--- Helgrind: checking for data races in threaded programs.      ---*/
/*---                                                    hg_main.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Helgrind, a Valgrind tool for detecting
   data races in threaded programs.

   Copyright (C) 2002-2005 Nicholas Nethercote
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

#include "tool.h"
#include "pub_tool_hashtable.h"
#include "pub_tool_mallocfree.h"
#include "pub_tool_replacemalloc.h"
#include "pub_tool_tooliface.h"

#include "helgrind.h"

static UInt n_hg_warnings = 0;
static UInt n_lockorder_warnings = 0;

/*------------------------------------------------------------*/
/*--- Debug guff                                           ---*/
/*------------------------------------------------------------*/

#define DEBUG_LOCK_TABLE    0   /* Print lock table at end */

#define DEBUG_MAKE_ACCESSES 0   /* Print make_access() calls */
#define DEBUG_LOCKS         0   /* Print lock()/unlock() calls and locksets */
#define DEBUG_NEW_LOCKSETS  0   /* Print new locksets when created */
#define DEBUG_ACCESSES      0   /* Print reads, writes */
#define DEBUG_MEM_LOCKSET_CHANGES 0
                                /* Print when an address's lockset
                                   changes; only useful with
                                   DEBUG_ACCESSES */
#define SLOW_ASSERTS        0	/* do expensive asserts */
#define DEBUG_VIRGIN_READS  0   /* Dump around address on VIRGIN reads */

#if SLOW_ASSERTS
#define TL_ASSERT(x)	tl_assert(x)
#else
#define TL_ASSERT(x)
#endif

/* heavyweight LockSet sanity checking:
   0 == never
   1 == after important ops
   2 == As 1 and also after pthread_mutex_* ops (excessively slow)
 */
#define LOCKSET_SANITY 0

/* Rotate an unsigned quantity left */
#define ROTL(x, n)	(((x) << (n)) | ((x) >> ((sizeof(x)*8)-(n))))

/* round a up to the next multiple of N.  N must be a power of 2 */
#define ROUNDUP(a, N)	((a + N - 1) & ~(N-1))

/* Round a down to the next multiple of N.  N must be a power of 2 */
#define ROUNDDN(a, N)	((a) & ~(N-1))

/*------------------------------------------------------------*/
/*--- Command line options                                 ---*/
/*------------------------------------------------------------*/

static enum {
   EC_None,
   EC_Some,
   EC_All
} clo_execontext = EC_None;

static Bool clo_priv_stacks = False;

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
   do { tl_assert((ev) >= 0 && (ev) < N_PROF_EVENTS);   \
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

typedef
   struct _HG_Chunk {
      struct _HG_Chunk* next;
      Addr          data;           /* ptr to actual block              */
      SizeT         size;           /* size requested                   */
      ExeContext*   where;          /* where it was allocated           */
      ThreadId      tid;            /* allocating thread                */
   }
   HG_Chunk;

typedef enum 
   { Vge_VirginInit, Vge_NonVirginInit, Vge_SegmentInit, Vge_Error } 
   VgeInitStatus;


// XXX: not 64-bit clean!
/* Should add up to 32 to fit in one word */
#define OTHER_BITS      30
#define STATE_BITS      2

#define ESEC_MAP_WORDS  16384   /* Words per secondary map */

/* This is for indicating that a memory block has been initialised but not
 * really directly by a particular thread... (eg. text/data initialised
 * automatically at startup).
 * Must be different to virgin_word.other */
#define TID_INDICATING_NONVIRGIN    1

/* Magic packed TLS used for error suppression; if word state is Excl
   and tid is this, then it means all access are OK without changing
   state and without raising any more errors  */
#define TLSP_INDICATING_ALL          ((1 << OTHER_BITS) - 1)

/* Number of entries must fit in STATE_BITS bits */
typedef enum { Vge_Virgin, Vge_Excl, Vge_Shar, Vge_SharMod } pth_state;

static inline const Char *pp_state(pth_state st)
{
   const Char *ret;

   switch(st) {
   case Vge_Virgin:	ret = "virgin"; break;
   case Vge_Excl:	ret = "exclusive"; break;
   case Vge_Shar:	ret = "shared RO"; break;
   case Vge_SharMod:	ret = "shared RW"; break;
   default:		ret = "???";
   }
   return ret;
}

typedef
   struct {
      /* gcc arranges this bitfield with state in the 2LSB and other
	 in the 30MSB, which is what we want */
      UInt state:STATE_BITS;
      UInt other:OTHER_BITS;
   } shadow_word;

#define SW(st, other)	((shadow_word) { st, other })

typedef
   struct {
      shadow_word swords[ESEC_MAP_WORDS];
   }
   ESecMap;

static ESecMap* primary_map[ 65536 ];
static ESecMap  distinguished_secondary_map;

static const shadow_word virgin_sword = SW(Vge_Virgin, 0);
static const shadow_word error_sword = SW(Vge_Excl, TLSP_INDICATING_ALL);

#define VGE_IS_DISTINGUISHED_SM(smap) \
   ((smap) == &distinguished_secondary_map)

#define ENSURE_MAPPABLE(addr,caller)                                  \
   do {                                                               \
      if (VGE_IS_DISTINGUISHED_SM(primary_map[(addr) >> 16])) {       \
         primary_map[(addr) >> 16] = alloc_secondary_map(caller);     \
         /*VG_(printf)("new 2map because of %p\n", addr);*/           \
      } \
   } while(0)


/* Parallel map which contains execution contexts when words last
  changed state (if required) */

typedef struct EC_IP {
   union u_ec_ip {
      Addr		ip;
      ExeContext	*ec;
   } uu_ec_ip;
   UInt			state:STATE_BITS;
   UInt			tls:OTHER_BITS;		/* packed TLS */
} EC_IP;

#define NULL_EC_IP	((EC_IP){ { 0 }, 0, 0})

#define IP(ip, prev, tls) ((EC_IP) { (union u_ec_ip)(ip), (prev).state, packTLS(tls) })
#define EC(ec, prev, tls)   ((EC_IP) { (union u_ec_ip)(ec), (prev).state, packTLS(tls) })

static inline UInt packEC(ExeContext *ec)
{
   TL_ASSERT(((UWord)ec & ((1 << STATE_BITS)-1)) == 0);
   return ((UWord)ec) >> STATE_BITS;
}

/* Lose 2 LSB of IP */
static inline UInt packIP(Addr ip)
{
   return ip >> STATE_BITS;
}

static inline Addr unpackIP(UInt i)
{
   return (Addr)(i << STATE_BITS);
}

typedef struct {
   EC_IP execontext[ESEC_MAP_WORDS];
} ExeContextMap;

static ExeContextMap** execontext_map;

static inline void setExeContext(Addr a, EC_IP ec)
{
   UInt idx = (a >> 16) & 0xffff;
   UInt off = (a >>  2) & 0x3fff;

   if (execontext_map[idx] == NULL) {
      execontext_map[idx] = VG_(malloc)(sizeof(ExeContextMap));
      VG_(memset)(execontext_map[idx], 0, sizeof(ExeContextMap));
   }

   execontext_map[idx]->execontext[off] = ec;
}

static inline EC_IP getExeContext(Addr a)
{
   UInt idx = (a >> 16) & 0xffff;
   UInt off = (a >>  2) & 0x3fff;
   EC_IP ec = NULL_EC_IP;

   if (execontext_map[idx] != NULL)
      ec = execontext_map[idx]->execontext[off];

   return ec;
}

/*------------------------------------------------------------*/
/*--- Thread lifetime segments                             ---*/
/*------------------------------------------------------------*/

/*
 * This mechanism deals with the common case of a parent thread
 * creating a structure for a child thread, and then passing ownership
 * of the structure to that thread.  It similarly copes with a child
 * thread passing information back to another thread waiting to join
 * on it.
 *
 * Each thread's lifetime can be partitioned into segments.  Those
 * segments are arranged to form an interference graph which indicates
 * whether two thread lifetime segments can possibly be concurrent.
 * If not, then memory with is exclusively accessed by one TLS can be
 * passed on to another TLS without an error occurring, and without
 * moving it from Excl state.
 *
 * At present this only considers thread creation and join as
 * synchronisation events for creating new lifetime segments, but
 * others may be possible (like mutex operations).
 */

typedef struct _ThreadLifeSeg ThreadLifeSeg;

struct _ThreadLifeSeg {
   ThreadId	         tid;
   ThreadLifeSeg	*prior[2];	/* Previous lifetime segments */
   UInt	                 refcount;	/* Number of memory locations pointing here */
   UInt	                 mark;		/* mark used for graph traversal */
   ThreadLifeSeg	*next;	        /* list of all TLS */
};

static ThreadLifeSeg *all_tls;
static UInt tls_since_gc;
#define TLS_SINCE_GC	10000

/* current mark used for TLS graph traversal */
static UInt tlsmark;

static ThreadLifeSeg *thread_seg[VG_N_THREADS];


static void tls_gc(void)
{
   /* XXX later.  Walk through all TLSs and look for ones with 0
      refcount and remove them from the structure and free them.
      Could probably get rid of ThreadLifeSeg.refcount and simply use
      mark-sweep from the shadow table. */
   VG_(printf)("WRITEME: TLS GC\n");
}

static void newTLS(ThreadId tid)
{
   static const Bool debug = False;
   ThreadLifeSeg *tls;

   /* Initial NULL */
   if (thread_seg[tid] == NULL) {
      tls = VG_(malloc)(sizeof(*tls));
      tls->tid = tid;
      tls->prior[0] = tls->prior[1] = NULL;
      tls->refcount = 0;
      tls->mark = tlsmark-1;

      tls->next = all_tls;
      all_tls = tls;
      tls_since_gc++;

      thread_seg[tid] = tls;
      return;
   }
   
   /* Previous TLS was unused, so just recycle */
   if (thread_seg[tid]->refcount == 0) {
      if (debug)
	 VG_(printf)("newTLS; recycling TLS %p for tid %u\n", 
		     thread_seg[tid], tid);
      return;
   }

   /* Use existing TLS for this tid as a prior for new TLS */
   tls = VG_(malloc)(sizeof(*tls));
   tls->tid = tid;
   tls->prior[0] = thread_seg[tid];
   tls->prior[1] = NULL;
   tls->refcount = 0;
   tls->mark = tlsmark-1;

   tls->next = all_tls;
   all_tls = tls;
   if (++tls_since_gc > TLS_SINCE_GC) {
      tls_gc();
      tls_since_gc = 0;
   }
   
   if (debug)
      VG_(printf)("newTLS: made new TLS %p for tid %u (prior %p(%u))\n",
		  tls, tid, tls->prior[0], tls->prior[0]->tid);

   thread_seg[tid] = tls;
}

/* clear out a TLS for a thread that's died */
static void clearTLS(ThreadId tid)
{
   newTLS(tid);

   thread_seg[tid]->prior[0] = NULL;
   thread_seg[tid]->prior[1] = NULL;
}

static void addPriorTLS(ThreadId tid, ThreadId prior)
{
   static const Bool debug = False;
   ThreadLifeSeg *tls = thread_seg[tid];

   if (debug)
      VG_(printf)("making TLS %p(%u) prior to TLS %p(%u)\n",
		  thread_seg[prior], prior, tls, tid);

   tl_assert(thread_seg[tid] != NULL);
   tl_assert(thread_seg[prior] != NULL);

   if (tls->prior[0] == NULL)
      tls->prior[0] = thread_seg[prior];
   else {
      tl_assert(tls->prior[1] == NULL);
      tls->prior[1] = thread_seg[prior];
   }
}

static Bool isPrior(const ThreadLifeSeg *t, const ThreadLifeSeg *prior)
{
   if (t == NULL || t->mark == tlsmark)
      return False;

   if (t == prior)
      return True;

   ((ThreadLifeSeg *)t)->mark = tlsmark;

   return isPrior(t->prior[0], prior) || isPrior(t->prior[1], prior);
}

/* Return True if prior is definitely not concurrent with tls */
static Bool tlsIsDisjoint(const ThreadLifeSeg *tls, 
			  const ThreadLifeSeg *prior)
{
   tlsmark++;			/* new traversal mark */

   return isPrior(tls, prior);
}

static inline UInt packTLS(ThreadLifeSeg *tls)
{
   TL_ASSERT(((UWord)tls & ((1 << STATE_BITS)-1)) == 0);
   return ((UWord)tls) >> STATE_BITS;
}

static inline ThreadLifeSeg *unpackTLS(UInt i)
{
   return (ThreadLifeSeg *)(i << STATE_BITS);
}

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

   // Mark all words as virgin.
   map = (ESecMap *)VG_(shadow_alloc)(sizeof(ESecMap));
   for (i = 0; i < ESEC_MAP_WORDS; i++)
      map->swords[i] = virgin_sword;

   return map;
}


/* Set a word.  The byte give by 'a' could be anywhere in the word -- the whole
 * word gets set. */
static /* __inline__ */
void set_sword ( Addr a, shadow_word sword )
{
   ESecMap* sm;
   shadow_word *oldsw;

   //PROF_EVENT(23); PPP
   ENSURE_MAPPABLE(a, "VGE_(set_sword)");

   /* Use bits 31..16 for primary, 15..2 for secondary lookup */
   sm     = primary_map[a >> 16];
   tl_assert(sm != &distinguished_secondary_map);
   oldsw = &sm->swords[(a & 0xFFFC) >> 2];
   if (oldsw->state == Vge_Excl && oldsw->other != TLSP_INDICATING_ALL) {
      ThreadLifeSeg *tls = unpackTLS(oldsw->other);
      tls->refcount--;
   }

   if (sword.state == Vge_Excl && sword.other != TLSP_INDICATING_ALL) {
      ThreadLifeSeg *tls = unpackTLS(sword.other);
      tls->refcount++;
   }
   
   sm->swords[(a & 0xFFFC) >> 2] = sword;

   if (VGE_IS_DISTINGUISHED_SM(sm)) {
      VG_(printf)("wrote to distinguished 2ndary map! 0x%x\n", a);
      // XXX: may be legit, but I want to know when it happens --njn
      VG_(tool_panic)("wrote to distinguished 2ndary map!");
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
      //VG_(tool_panic)("accessed distinguished 2ndary map!");
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
   if (clo_execontext != EC_None)
      setExeContext(a, NULL_EC_IP);
   set_sword(a, virgin_sword);
}

static __inline__
void init_error_sword(Addr a)
{
   set_sword(a, error_sword);
}

static __inline__ 
void init_nonvirgin_sword(Addr a)
{
   shadow_word sword;
   ThreadId tid;
   ThreadLifeSeg *tls;

   // The tid must be passed in here now;  this requires more events to be
   // given the tid in the first place.
   //
   //tid = VG_(get_current_or_recent_tid)();
   VG_(message)(Vg_DebugMsg, "tid needs to be passed in here");
   VG_(exit)(1);

   tl_assert(tid != VG_INVALID_THREADID);
   tls = thread_seg[tid];

   sword = SW(Vge_Excl, packTLS(tls));
   set_sword(a, sword);
}


/* In this case, we treat it for Helgrind's sake like virgin (it hasn't
 * been inited by a particular thread, it's just done automatically upon
 * startup), but we mark its .state specially so it doesn't look like an 
 * uninited read. */
static __inline__ 
void init_magically_inited_sword(Addr a)
{
   shadow_word sword;

   sword = SW(Vge_Virgin, TID_INDICATING_NONVIRGIN);

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
   Int		      setsize;	/* number of members */
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
static inline UInt packLockSet(const LockSet *p)
{
   UInt id;

   TL_ASSERT(((UWord)p & ((1 << STATE_BITS)-1)) == 0);
   id = ((UWord)p) >> STATE_BITS;

   return id;
}

static inline const LockSet *unpackLockSet(UInt id)
{
   return (LockSet *)(id << STATE_BITS);
}

static 
void pp_LockSet(const LockSet* p)
{
   Int i;
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
static UInt hash_LockSet_w_wo(const LockSet *ls, 
			      const Mutex *with,
			      const Mutex *without)
{
   Int  i;
   UInt hash = ls->setsize + (with != NULL) - (without != NULL);
   
   tl_assert(with == NULL || with != without);

   for(i = 0; with != NULL || i < ls->setsize; i++) {
      const Mutex *mx = i >= ls->setsize ? NULL : ls->mutex[i];

      if (without && mutex_cmp(without, mx) == 0)
	 continue;

      if (with && (mx == NULL || mutex_cmp(with, mx) < 0)) {
	 mx = with;
	 with = NULL;
	 i--;
      }

      hash = ROTL(hash, 17);
      hash ^= mx->mutexp;
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

   tl_assert(ia == a->setsize || mutex_cmp(a->mutex[ia], missing_mutex) >= 0);

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

   tl_assert(lookup_LockSet(set) == NULL);

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

static
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
	 if (ls->hash != (UInt)i) {
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
   VG_(tool_panic)("sanity_check_locksets");
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

   tl_assert(!ismember(ls, mx));

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

   tl_assert(j == ret->setsize);

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

   tl_assert(ismember(ls, mx));

   ret = alloc_LockSet(ls->setsize-1);

   for(i = j = 0; i < ls->setsize; i++) {
      if (mutex_cmp(ls->mutex[i], mx) == 0)
	      continue;
      ret->mutex[j++] = ls->mutex[i];
   }

   tl_assert(j == ret->setsize);

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
	 tl_assert(mutex_cmp(a->mutex[ia], b->mutex[ib]) > 0);
	 ib++;
      } 
   }

   /* Build the intersection of the two sets */
   ret = alloc_LockSet(size);
   for (iret = ia = ib = 0; ia < a->setsize && ib < b->setsize; ) {
      if (mutex_cmp(a->mutex[ia], b->mutex[ib]) == 0) {
	 tl_assert(iret < ret->setsize);
	 ret->mutex[iret++] = a->mutex[ia];
	 ia++;
	 ib++;
      } else if (mutex_cmp(a->mutex[ia], b->mutex[ib]) < 0) {
	 ia++;
      } else {
	 tl_assert(mutex_cmp(a->mutex[ia], b->mutex[ib]) > 0);
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
	 tl_assert(cmp > 0);
	 size++;
	 ib++;
      } 
   }

   /* Build the intersection of the two sets */
   ret = alloc_LockSet(size);
   for (iret = ia = ib = 0; (ia < a->setsize) || (ib < b->setsize); ) {
      Int cmp;
      tl_assert(iret < ret->setsize);

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
	 tl_assert(cmp > 0);
	 ret->mutex[iret++] = b->mutex[ib];
	 ib++;
      } 
   }

   tl_assert(iret == ret->setsize);

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
/*--- Implementation of mutex structure.                   ---*/
/*------------------------------------------------------------*/

static UInt graph_mark;		/* current mark we're using for graph traversal */

static void record_mutex_error(ThreadId tid, Mutex *mutex, 
			       Char *str, ExeContext *ec);
static void record_lockgraph_error(ThreadId tid, Mutex *mutex,
				   const LockSet *lockset_holding, 
				   const LockSet *lockset_prev);

static void set_mutex_state(Mutex *mutex, MutexState state, ThreadId tid);

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
   UInt bucket = mutexp % M_MUTEX_HASHSZ;
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

#define MARK_LOOP	(graph_mark+0)
#define MARK_DONE	(graph_mark+1)

static Bool check_cycle_inner(const Mutex *mutex, const LockSet *ls)
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

static Bool check_cycle(const Mutex *start, const LockSet* lockset)
{

   graph_mark += 2;		/* clear all marks */

   return check_cycle_inner(start, lockset);
}

/* test to see if a mutex state change would be problematic; this
   makes no changes to the mutex state.  This should be called before
   the locking thread has actually blocked. */
static void test_mutex_state(Mutex *mutex, MutexState state, ThreadId tid)
{
   static const Bool debug = False;

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
      tl_assert(!check_cycle(mutex, mutex->lockdep));

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
      break;

   case MxDead:
      break;

   default:
      break;
   }
}

/* Update a mutex state.  Expects most error testing and reporting to
   have happened in test_mutex_state().  The assumption is that no
   client code is run by thread tid between test and set, either
   because it is blocked or test and set are called together
   atomically.  

   Setting state to MxDead is the exception, since that can happen as
   a result of any thread freeing memory; in this case set_mutex_state
   does all the error reporting as well.
*/
static void set_mutex_state(Mutex *mutex, MutexState state, ThreadId tid)
{
   static const Bool debug = False;

   if (debug)
      VG_(printf)("\ntid %d changing mutex (%p)->%p%(y state %s -> %s\n",
		  tid, mutex, mutex->mutexp, mutex->mutexp,
		  pp_MutexState(mutex->state), pp_MutexState(state));

   if (mutex->state == MxDead) {
      /* can't do anything legal to a destroyed mutex */
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

	 VG_(tool_panic)("core should have checked this\n");
	 break;
      }

      tl_assert(!check_cycle(mutex, mutex->lockdep));

      mutex->tid = tid;
      break;

   case MxUnlocked:
      if (debug)
	 print_LockSet("thread holding", thread_locks[tid]);

      if (mutex->state != MxLocked || mutex->tid != tid)
	 break;

      mutex->tid = VG_INVALID_THREADID;
      break;

   case MxDead:
      if (mutex->state == MxLocked) {
	 /* forcably remove offending lock from thread's lockset  */
	 tl_assert(ismember(thread_locks[mutex->tid], mutex));
	 thread_locks[mutex->tid] = remove_LockSet(thread_locks[mutex->tid], mutex);
	 mutex->tid = VG_INVALID_THREADID;

	 record_mutex_error(tid, mutex,
			    "free locked mutex", mutex->location);
      }
      break;

   default:
      break;
   }

   mutex->location = VG_(record_ExeContext)(tid);
   mutex->state = state;
}

/*------------------------------------------------------------*/
/*--- Setting and checking permissions.                    ---*/
/*------------------------------------------------------------*/

/* only clean up dead mutexes */
static
Bool cleanmx(Mutex *mx) {
   return mx->state == MxDead;
}

static
void set_address_range_state ( Addr a, SizeT len /* in bytes */, 
                               VgeInitStatus status )
{
   Addr end;

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
   end = ROUNDUP(a + len, 4);
   a   = ROUNDDN(a, 4);

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
      VG_(tool_panic)("Unexpected Vge_InitStatus");
   }
      
   VGP_POPCC(VgpSARP);
}


static void make_segment_readable ( Addr a, SizeT len )
{
   //PROF_EVENT(??);    PPP
   set_address_range_state ( a, len, Vge_SegmentInit );
}

static void make_writable ( Addr a, SizeT len )
{
   //PROF_EVENT(36);  PPP
   set_address_range_state( a, len, Vge_VirginInit );
}

static void make_readable ( Addr a, SizeT len )
{
   //PROF_EVENT(37);  PPP
   set_address_range_state( a, len, Vge_VirginInit );
}


/* Block-copy states (needed for implementing realloc()). */
static void copy_address_range_state(Addr src, Addr dst, SizeT len)
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
static void hg_mem_read (Addr a, SizeT data_size, ThreadId tid);
static void hg_mem_write(Addr a, SizeT data_size, ThreadId tid);

static void hg_mem_help_read_1(Addr a) VGA_REGPARM(1);
static void hg_mem_help_read_2(Addr a) VGA_REGPARM(1);
static void hg_mem_help_read_4(Addr a) VGA_REGPARM(1);
static void hg_mem_help_read_N(Addr a, SizeT size) VGA_REGPARM(2);

static void hg_mem_help_write_1(Addr a, UInt val) VGA_REGPARM(2);
static void hg_mem_help_write_2(Addr a, UInt val) VGA_REGPARM(2);
static void hg_mem_help_write_4(Addr a, UInt val) VGA_REGPARM(2);
static void hg_mem_help_write_N(Addr a, SizeT size) VGA_REGPARM(2);

static void bus_lock(void);
static void bus_unlock(void);

static
void hg_pre_mem_read(CorePart part, ThreadId tid,
                     Char* s, Addr base, SizeT size )
{
   if (tid > 50) { VG_(printf)("pid = %d, s = `%s`, part = %d\n", tid, s, part); VG_(tool_panic)("a");}
   hg_mem_read(base, size, tid);
}

static
void hg_pre_mem_read_asciiz(CorePart part, ThreadId tid,
                            Char* s, Addr base )
{
   hg_mem_read(base, VG_(strlen)((Char*)base), tid);
}

static
void hg_pre_mem_write(CorePart part, ThreadId tid,
                      Char* s, Addr base, SizeT size )
{
   hg_mem_write(base, size, tid);
}



static
void hg_new_mem_startup( Addr a, SizeT len, Bool rr, Bool ww, Bool xx )
{
   /* Ignore the permissions, just make it readable.  Seems to work... */
   make_segment_readable(a, len);
}


static
void hg_new_mem_heap ( Addr a, SizeT len, Bool is_inited )
{
   if (is_inited) {
      make_readable(a, len);
   } else {
      make_writable(a, len);
   }
}

static
void hg_set_perms (Addr a, SizeT len,
                   Bool rr, Bool ww, Bool xx)
{
   if      (rr) make_readable(a, len);
   else if (ww) make_writable(a, len);
   /* else do nothing */
}

static
void hg_new_mem_stack_private(Addr a, SizeT len)
{
   set_address_range_state(a, len, Vge_NonVirginInit);
}

static
void hg_new_mem_stack(Addr a, SizeT len)
{
   set_address_range_state(a, len, Vge_VirginInit);
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


/*------------------------------------------------------------*/
/*--- malloc() et al replacements                          ---*/
/*------------------------------------------------------------*/

static VgHashTable hg_malloc_list = NULL;

#define N_FREED_CHUNKS	2
static Int freechunkptr = 0;
static HG_Chunk *freechunks[N_FREED_CHUNKS];


/* Allocate a user-chunk of size bytes.  Also allocate its shadow
   block, make the shadow block point at the user block.  Put the
   shadow chunk on the appropriate list, and set all memory
   protections correctly. */

static void add_HG_Chunk ( ThreadId tid, Addr p, SizeT size )
{
   HG_Chunk* hc;

   hc            = VG_(malloc)(sizeof(HG_Chunk));
   hc->data      = p;
   hc->size      = size;
   hc->where     = VG_(record_ExeContext)(tid);
   hc->tid       = tid;

   VG_(HT_add_node)( hg_malloc_list, (VgHashNode*)hc );
}

/* Allocate memory and note change in memory available */
static __inline__
void* alloc_and_new_mem ( ThreadId tid, SizeT size, SizeT alignment,
                          Bool is_zeroed )
{
   Addr p;

   if (size < 0) return NULL;

   p = (Addr)VG_(cli_malloc)(alignment, size);
   if (!p) {
      return NULL;
   }
   if (is_zeroed) VG_(memset)((void*)p, 0, size);
   add_HG_Chunk ( tid, p, size );
   hg_new_mem_heap( p, size, is_zeroed );

   return (void*)p;
}

static void* hg_malloc ( ThreadId tid, SizeT n )
{
   return alloc_and_new_mem ( tid, n, VG_(clo_alignment), /*is_zeroed*/False );
}

static void* hg___builtin_new ( ThreadId tid, SizeT n )
{
   return alloc_and_new_mem ( tid, n, VG_(clo_alignment), /*is_zeroed*/False );
}

static void* hg___builtin_vec_new ( ThreadId tid, SizeT n )
{
   return alloc_and_new_mem ( tid, n, VG_(clo_alignment), /*is_zeroed*/False );
}

static void* hg_memalign ( ThreadId tid, SizeT align, SizeT n )
{
   return alloc_and_new_mem ( tid, n, align,              /*is_zeroed*/False );
}

static void* hg_calloc ( ThreadId tid, SizeT nmemb, SizeT size )
{
   return alloc_and_new_mem ( tid, nmemb*size, VG_(clo_alignment),
                              /*is_zeroed*/True );
}

static ThreadId deadmx_tid;

static
Bool deadmx(Mutex *mx) {
   if (mx->state != MxDead)
      set_mutex_state(mx, MxDead, deadmx_tid);
   
   return False;
}

static
void die_and_free_mem ( ThreadId tid, HG_Chunk* hc,
                        HG_Chunk** prev_chunks_next_ptr )
{
   Addr start = hc->data;
   Addr end   = start + hc->size;

   /* Remove hc from the malloclist using prev_chunks_next_ptr to
      avoid repeating the hash table lookup.  Can't remove until at least
      after free and free_mismatch errors are done because they use
      describe_addr() which looks for it in malloclist. */
   *prev_chunks_next_ptr = hc->next;

   /* Record where freed */
   hc->where = VG_(record_ExeContext) ( tid );

   /* maintain a small window so that the error reporting machinery
      knows about this memory */
   if (freechunks[freechunkptr] != NULL) {
      /* free HG_Chunk */
      HG_Chunk* sc1 = freechunks[freechunkptr];
      VG_(cli_free) ( (void*)(sc1->data) );
      VG_(free) ( sc1 );
   }

   freechunks[freechunkptr] = hc;

   if (++freechunkptr == N_FREED_CHUNKS)
      freechunkptr = 0;

   /* mark all mutexes in range dead */
   deadmx_tid = tid;
   find_mutex_range(start, end, deadmx);
}


static __inline__
void handle_free ( ThreadId tid, void* p )
{
   HG_Chunk*  hc;
   HG_Chunk** prev_chunks_next_ptr;

   hc = (HG_Chunk*)VG_(HT_get_node) ( hg_malloc_list, (UWord)p,
                                      (VgHashNode***)&prev_chunks_next_ptr );
   if (hc == NULL) {
      return;
   }
   die_and_free_mem ( tid, hc, prev_chunks_next_ptr );
}

static void hg_free ( ThreadId tid, void* p )
{
   handle_free(tid, p);
}

static void hg___builtin_delete ( ThreadId tid, void* p )
{
   handle_free(tid, p);
}

static void hg___builtin_vec_delete ( ThreadId tid, void* p )
{
   handle_free(tid, p);
}

static void* hg_realloc ( ThreadId tid, void* p, SizeT new_size )
{
   HG_Chunk  *hc;
   HG_Chunk **prev_chunks_next_ptr;
   Int        i;

   /* First try and find the block. */
   hc = (HG_Chunk*)VG_(HT_get_node) ( hg_malloc_list, (UWord)p,
                                       (VgHashNode***)&prev_chunks_next_ptr );

   if (hc == NULL) {
      return NULL;
   }
  
   if (hc->size == new_size) {
      /* size unchanged */
      hc->where = VG_(record_ExeContext)(tid);
      return p;
      
   } else if (hc->size > new_size) {
      /* new size is smaller */
      hc->size = new_size;
      hc->where = VG_(record_ExeContext)(tid);
      return p;

   } else {
      /* new size is bigger */
      Addr p_new;

      /* Get new memory */
      p_new = (Addr)VG_(cli_malloc)(VG_(clo_alignment), new_size);

      /* First half kept and copied, second half new */
      copy_address_range_state( (Addr)p, p_new, hc->size );
      hg_new_mem_heap ( p_new+hc->size, new_size-hc->size,
                        /*inited*/False );

      /* Copy from old to new */
      for (i = 0; i < hc->size; i++)
         ((UChar*)p_new)[i] = ((UChar*)p)[i];

      /* Free old memory */
      die_and_free_mem ( tid, hc, prev_chunks_next_ptr );

      /* this has to be after die_and_free_mem, otherwise the
         former succeeds in shorting out the new block, not the
         old, in the case when both are on the same list.  */
      add_HG_Chunk ( tid, p_new, new_size );

      return (void*)p_new;
   }  
}

/*--------------------------------------------------------------*/
/*--- Machinery to support sanity checking                   ---*/
/*--------------------------------------------------------------*/

static Bool hg_cheap_sanity_check ( void )
{
   /* nothing useful we can rapidly check */
   return True;
}

static Bool hg_expensive_sanity_check(void)
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

static UInt stk_ld, nonstk_ld, stk_st, nonstk_st;

#if 0
/* Create and return an instrumented version of cb_in.  Free cb_in
   before returning. */
UCodeBlock* TL_(instrument) ( UCodeBlock* cb_in, Addr not_used )
{
   UCodeBlock* cb;
   Int         i;
   UInstr*     u_in;
   Int         t_size = INVALID_TEMPREG;
   Int	       ntemps;
   Bool	       *stackref = NULL;
   Bool        locked = False;	/* lock prefix */

   cb = VG_(setup_UCodeBlock)(cb_in);

   /* stackref[] is used for super-simple value tracking to keep note
      of which tempregs currently hold a value which is derived from
      the stack pointer or frame pointer, and is therefore likely
      stack-relative if used as the address for LOAD or STORE. */
   ntemps = VG_(get_num_temps)(cb);
   stackref = VG_(malloc)(sizeof(*stackref) * ntemps);
   VG_(memset)(stackref, 0, sizeof(*stackref) * ntemps);

   for (i = 0; i < VG_(get_num_instrs)(cb_in); i++) {
      u_in = VG_(get_instr)(cb_in, i);

      switch (u_in->opcode) {

         case NOP: case CALLM_S: case CALLM_E:
            break;
    
         case LOCK:
	    locked = True;
	    uInstr0(cb, CCALL, 0);
	    uCCall(cb, (Addr)bus_lock, 0, 0, False);
	    break;

         case JMP: case INCEIP:
	    if (locked) {
	       uInstr0(cb, CCALL, 0);
	       uCCall(cb, (Addr)bus_unlock, 0, 0, False);
	    }
	    locked = False;
	    VG_(copy_UInstr)(cb, u_in);
	    break;

         case GET:
	    tl_assert(u_in->tag1 == ArchReg);
	    tl_assert(u_in->tag2 == TempReg);
	    tl_assert(u_in->val2 < ntemps);

	    stackref[u_in->val2] = (u_in->size == 4 &&
				    (u_in->val1 == VGA_R_STACK_PTR ||
                                     u_in->val1 == VGA_R_FRAME_PTR));
	    VG_(copy_UInstr)(cb, u_in);
	    break;

         case MOV:
	    if (u_in->size == 4 && u_in->tag1 == TempReg) {
	       tl_assert(u_in->tag2 == TempReg);
	       stackref[u_in->val2] = stackref[u_in->val1];
	    }
	    VG_(copy_UInstr)(cb, u_in);
	    break;

         case LEA1:
         case ADD: case SUB:
	    if (u_in->size == 4 && u_in->tag1 == TempReg) {
	       tl_assert(u_in->tag2 == TempReg);
	       stackref[u_in->val2] |= stackref[u_in->val1];
	    }
	    VG_(copy_UInstr)(cb, u_in);
	    break;

         case LOAD: {
	    void (*help)(Addr);
	    tl_assert(1 == u_in->size || 2 == u_in->size || 4 == u_in->size);
	    tl_assert(u_in->tag1 == TempReg);

	    if (!clo_priv_stacks || !stackref[u_in->val1]) {
	       nonstk_ld++;

	       switch(u_in->size) {
	       case 1: help = hg_mem_help_read_1; break;
	       case 2: help = hg_mem_help_read_2; break;
	       case 4: help = hg_mem_help_read_4; break;
	       default:
		  VG_(tool_panic)("bad size");
	       }

	       /* XXX all registers should be flushed to baseblock
		  here */
	       uInstr1(cb, CCALL, 0, TempReg, u_in->val1);
	       uCCall(cb, (Addr)help, 1, 1, False);
	    } else
	       stk_ld++;

	    VG_(copy_UInstr)(cb, u_in);
	    t_size = INVALID_TEMPREG;
	    break;
	 }

         case MMX2_MemRd:
         case FPU_R: {
            tl_assert(1 == u_in->size || 2 == u_in->size || 4 == u_in->size || 
                      8 == u_in->size || 10 == u_in->size || 108 == u_in->size);
	    
	    t_size = newTemp(cb);
	    uInstr2(cb, MOV,   4, Literal, 0, TempReg, t_size);
	    uLiteral(cb, (UInt)u_in->size);

	    /* XXX all registers should be flushed to baseblock
	       here */
	    uInstr2(cb, CCALL, 0, TempReg, u_in->val2, TempReg, t_size);
	    uCCall(cb, (Addr) & hg_mem_help_read_N, 2, 2, False);
	    
	    VG_(copy_UInstr)(cb, u_in);
	    t_size = INVALID_TEMPREG;
	    break;
	 } 

         case MMX2a1_MemRd: {
            tl_assert(8 == u_in->size);
	    
	    t_size = newTemp(cb);
	    uInstr2(cb, MOV,   4, Literal, 0, TempReg, t_size);
	    uLiteral(cb, (UInt)u_in->size);

	    /* XXX all registers should be flushed to baseblock
	       here */
	    uInstr2(cb, CCALL, 0, TempReg, u_in->val3, TempReg, t_size);
	    uCCall(cb, (Addr) & hg_mem_help_read_N, 2, 2, False);
	    
	    VG_(copy_UInstr)(cb, u_in);
	    t_size = INVALID_TEMPREG;
	    break;
	 } 

         case SSE2a_MemRd:
         case SSE2a1_MemRd:
         case SSE3a_MemRd:
         case SSE3a1_MemRd:
         case SSE3ag_MemRd_RegWr: {
	    Int addr = (u_in->opcode == SSE3ag_MemRd_RegWr) ? u_in->val1 : u_in->val3;

            tl_assert(u_in->size == 4 || u_in->size == 8 || u_in->size == 16 || u_in->size == 512);
	    
	    t_size = newTemp(cb);
	    uInstr2(cb, MOV,   4, Literal, 0, TempReg, t_size);
	    uLiteral(cb, (UInt)u_in->size);

	    uInstr2(cb, CCALL, 0, TempReg, addr, TempReg, t_size);
	    uCCall(cb, (Addr) & hg_mem_help_read_N, 2, 2, False);
	    
	    VG_(copy_UInstr)(cb, u_in);
	    t_size = INVALID_TEMPREG;
	    break;
         }

         case STORE: {
	    void (*help)(Addr, UInt);
            tl_assert(1 == u_in->size || 2 == u_in->size || 4 == u_in->size);
	    tl_assert(u_in->tag2 == TempReg);

	    if (!clo_priv_stacks || !stackref[u_in->val2]) {
	       nonstk_st++;

	       switch(u_in->size) {
	       case 1: help = hg_mem_help_write_1; break;
	       case 2: help = hg_mem_help_write_2; break;
	       case 4: help = hg_mem_help_write_4; break;
	       default:
		  VG_(tool_panic)("bad size");
	       }

	       /* XXX all registers should be flushed to baseblock
		  here */
	       uInstr2(cb, CCALL, 0, TempReg, u_in->val2, TempReg, u_in->val1);
	       uCCall(cb, (Addr)help, 2, 2, False);
	    } else
	       stk_st++;

	    VG_(copy_UInstr)(cb, u_in);
	    t_size = INVALID_TEMPREG;
	    break;
	 }

         case MMX2_MemWr:
         case FPU_W: {
            tl_assert(1 == u_in->size || 2 == u_in->size || 4 == u_in->size || 
                      8 == u_in->size || 10 == u_in->size || 108 == u_in->size);

	    t_size = newTemp(cb);
	    uInstr2(cb, MOV,   4, Literal, 0, TempReg, t_size);
	    uLiteral(cb, (UInt)u_in->size);
	       /* XXX all registers should be flushed to baseblock
		  here */
	    uInstr2(cb, CCALL, 0, TempReg, u_in->val2, TempReg, t_size);
	    uCCall(cb, (Addr) & hg_mem_help_write_N, 2, 2, False);

	    VG_(copy_UInstr)(cb, u_in);
	    t_size = INVALID_TEMPREG;
	    break;
	 }

         case SSE2a_MemWr:
         case SSE3a_MemWr: {
            tl_assert(4 == u_in->size || 8 == u_in->size || 16 == u_in->size ||
		      512 == u_in->size);

	    t_size = newTemp(cb);
	    uInstr2(cb, MOV,   4, Literal, 0, TempReg, t_size);
	    uLiteral(cb, (UInt)u_in->size);
	       /* XXX all registers should be flushed to baseblock
		  here */
	    uInstr2(cb, CCALL, 0, TempReg, u_in->val3, TempReg, t_size);
	    uCCall(cb, (Addr) & hg_mem_help_write_N, 2, 2, False);

	    VG_(copy_UInstr)(cb, u_in);
	    t_size = INVALID_TEMPREG;
	    break;
	 }

         default:
	    /* conservative tromping */
	    if (0 && u_in->tag1 == TempReg) /* can val1 ever be dest? */
	       stackref[u_in->val1] = False;
	    if (u_in->tag2 == TempReg)
	       stackref[u_in->val2] = False;
	    if (u_in->tag3 == TempReg)
	       stackref[u_in->val3] = False;
            VG_(copy_UInstr)(cb, u_in);
            break;
      }
   }

   VG_(free)(stackref);
   VG_(free_UCodeBlock)(cb_in);
   return cb;
}
#endif
static IRBB* hg_instrument ( IRBB* bb_in, VexGuestLayout* layout, 
                             IRType gWordTy, IRType hWordTy )
{
   VG_(message)(Vg_DebugMsg, "Helgrind is not yet ready to handle Vex IR");
   VG_(exit)(1);
}

/*--------------------------------------------------------------------*/
/*--- Error and suppression handling                               ---*/
/*--------------------------------------------------------------------*/

typedef
   enum {
      /* Possible data race */
      RaceSupp
   }
   RaceSuppKind;

/* What kind of error it is. */
typedef
   enum { 
      RaceErr,	        	/* data-race */
      MutexErr,			/* mutex operations */
      LockGraphErr,		/* mutex order error */
   }
   RaceErrorKind;

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
      /* True if is just-below the stack pointer -- could be a gcc bug. */
      Bool maybe_gcc;
      /* symbolic address description */
      Char *expr;
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
      EC_IP      lasttouched;
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
   ai->expr       = NULL;
}

static __inline__
void clear_HelgrindError ( HelgrindError* err_extra )
{
   err_extra->axskind    = ReadAxs;
   err_extra->size       = 0;
   err_extra->mutex      = NULL;
   err_extra->lasttouched= NULL_EC_IP;
   err_extra->lasttid    = VG_INVALID_THREADID;
   err_extra->prev_lockset = 0;
   err_extra->held_lockset = 0;
   err_extra->prevstate  = SW(Vge_Virgin, 0);
   clear_AddrInfo ( &err_extra->addrinfo );
   err_extra->isWrite    = False;
}



/* Describe an address as best you can, for error messages,
   putting the result in ai. */

/* Callback for searching malloc'd and free'd lists */
static Bool addr_is_in_block(VgHashNode *node, void *ap)
{
   HG_Chunk* hc2 = (HG_Chunk*)node;
   Addr a = *(Addr *)ap;
   
   return (hc2->data <= a && a < hc2->data + hc2->size);
}

static void describe_addr ( Addr a, AddrInfo* ai )
{
   HG_Chunk* hc;
   Int i;

   /* Search for it in segments */
   {
      const SegInfo *si;

      for (si = VG_(next_seginfo)(NULL); 
	   si != NULL; 
	   si = VG_(next_seginfo)(si)) 
      {
	 Addr base = VG_(seg_start)(si);
	 SizeT size = VG_(seg_size)(si);
	 const UChar *filename = VG_(seg_filename)(si);

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
   hc = (HG_Chunk*)VG_(HT_first_match)(hg_malloc_list, addr_is_in_block, &a);
   if (NULL != hc) {
      ai->akind      = Mallocd;
      ai->blksize    = hc->size;
      ai->rwoffset   = (Int)a - (Int)(hc->data);
      ai->lastchange = hc->where;
      ai->lasttid    = hc->tid;
      return;
   } 

   /* Look in recently freed memory */
   for(i = 0; i < N_FREED_CHUNKS; i++) {
      hc = freechunks[i];
      if (hc == NULL)
	 continue;

      if (a >= hc->data && a < hc->data + hc->size) {
	 ai->akind      = Freed;
	 ai->blksize    = hc->size;
	 ai->rwoffset   = a - hc->data;
	 ai->lastchange = hc->where;
	 ai->lasttid    = hc->tid;
	 return;
      } 
   }
 
   /* Clueless ... */
   ai->akind = Unknown;
   return;
}


/* Updates the copy with address info if necessary. */
static UInt hg_update_extra(Error* err)
{
   HelgrindError* extra;

   extra = (HelgrindError*)VG_(get_error_extra)(err);
   if (extra != NULL && Undescribed == extra->addrinfo.akind) {
      describe_addr ( VG_(get_error_address)(err), &(extra->addrinfo) );
   }
   return sizeof(HelgrindError);
}

static void record_race_error ( ThreadId tid, Addr a, Bool is_write,
                                shadow_word prevstate )
{
   shadow_word *sw;
   HelgrindError err_extra;

   n_hg_warnings++;

   clear_HelgrindError(&err_extra);
   err_extra.isWrite = is_write;
   err_extra.addrinfo.akind = Undescribed;
   err_extra.prevstate = prevstate;
   if (clo_execontext)
      err_extra.lasttouched = getExeContext(a);
   err_extra.addrinfo.expr = VG_(describe_addr)(tid, a);

   VG_(maybe_record_error)( tid, RaceErr, a, 
                            (is_write ? "writing" : "reading"),
                            &err_extra);

   sw = get_sword_addr(a);
   if (sw->state == Vge_Excl && sw->other != TLSP_INDICATING_ALL) {
      ThreadLifeSeg *tls = unpackTLS(sw->other);
      tls->refcount--;
   }

   set_sword(a, error_sword);
}

static void record_mutex_error(ThreadId tid, Mutex *mutex, 
			       Char *str, ExeContext *ec)
{
   HelgrindError err_extra;

   clear_HelgrindError(&err_extra);
   err_extra.addrinfo.akind = Undescribed;
   err_extra.mutex = mutex;
   err_extra.lasttouched = EC(ec, virgin_sword, thread_seg[tid]);
   err_extra.lasttid = tid;

   VG_(maybe_record_error)(tid, MutexErr, 
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
   
   err_extra.lasttouched = EC(mutex->location, virgin_sword, 0);
   err_extra.held_lockset = lockset_holding;
   err_extra.prev_lockset = lockset_prev;
   
   VG_(maybe_record_error)(tid, LockGraphErr, mutex->mutexp, "", &err_extra);
}

static Bool hg_eq_Error ( VgRes not_used, Error* e1, Error* e2 )
{
   Char *e1s, *e2s;

   tl_assert(VG_(get_error_kind)(e1) == VG_(get_error_kind)(e2));

   switch (VG_(get_error_kind)(e1)) {
   case RaceErr:
      return VG_(get_error_address)(e1) == VG_(get_error_address)(e2);

   case MutexErr:
      return VG_(get_error_address)(e1) == VG_(get_error_address)(e2);
   }

   e1s = VG_(get_error_string)(e1);
   e2s = VG_(get_error_string)(e2);
   if (e1s != e2s) return False;
   if (0 != VG_(strcmp)(e1s, e2s)) return False;
   return True;
}

static void pp_AddrInfo ( Addr a, AddrInfo* ai )
{
   if (ai->expr != NULL)
      VG_(message)(Vg_UserMsg, 
		   " Address %p == %s", a, ai->expr);
   
   switch (ai->akind) {
      case Stack: 
         VG_(message)(Vg_UserMsg, 
                      " Address %p is on thread %d's stack", 
                      a, ai->stack_tid);
         break;
      case Unknown:
	 if (ai->expr != NULL)
	    break;

         /* maybe_gcc is never set to True!  This is a hangover from code
            in Memcheck */
         if (ai->maybe_gcc) {
            VG_(message)(Vg_UserMsg, 
               " Address %p is just below the stack pointer.  Possibly a bug in GCC/G++",
               a);
            VG_(message)(Vg_UserMsg, 
               "   v 2.96 or 3.0.X.  To suppress, use: --workaround-gcc296-bugs=yes");
	 } else {
            VG_(message)(Vg_UserMsg, 
               " Address %p is not stack'd, malloc'd or (recently) free'd", a);
         }
         break;
      case Segment:
	VG_(message)(Vg_UserMsg,
		     " Address %p is in %s section of %s", 
		     a, ai->section, ai->filename);
	break;
      case Mallocd:
      case Freed: {
         SizeT delta;
         UChar* relative;
         if (ai->rwoffset < 0) {
            delta    = (SizeT)(- ai->rwoffset);
            relative = "before";
         } else if (ai->rwoffset >= ai->blksize) {
            delta    = ai->rwoffset - ai->blksize;
            relative = "after";
         } else {
            delta    = ai->rwoffset;
            relative = "inside";
         }
	 VG_(message)(Vg_UserMsg, 
		      " Address %p is %llu bytes %s a block of size %d %s by thread %d",
		      a, (ULong)delta, relative, 
		      ai->blksize,
		      ai->akind == Mallocd ? "alloc'd" : "freed",
		      ai->lasttid);

         VG_(pp_ExeContext)(ai->lastchange);
         break;
      }   
      default:
         VG_(tool_panic)("pp_AddrInfo");
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

static void hg_pp_Error ( Error* err )
{
   HelgrindError *extra = (HelgrindError *)VG_(get_error_extra)(err);
   Char buf[100];
   Char *msg = buf;
   const LockSet *ls;

   *msg = '\0';

   switch(VG_(get_error_kind)(err)) {
   case RaceErr: {
      Addr err_addr = VG_(get_error_address)(err);
                      
      VG_(message)(Vg_UserMsg, "Possible data race %s variable at %p %(y",
		   VG_(get_error_string)(err), err_addr, err_addr);
      VG_(pp_ExeContext)( VG_(get_error_where)(err) );
      pp_AddrInfo(err_addr, &extra->addrinfo);

      switch(extra->prevstate.state) {
      case Vge_Virgin:
	 /* shouldn't be possible to go directly from virgin -> error */
	 VG_(sprintf)(buf, "virgin!?");
	 break;

      case Vge_Excl: {
	 ThreadLifeSeg *tls = unpackTLS(extra->prevstate.other);

	 tl_assert(tls != unpackTLS(TLSP_INDICATING_ALL));
	 VG_(sprintf)(buf, "exclusively owned by thread %u", tls->tid);
	 break;
      }

      case Vge_Shar:
      case Vge_SharMod:
	 ls = unpackLockSet(extra->prevstate.other);

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

      if (*msg)
	 VG_(message)(Vg_UserMsg, " Previous state: %s", msg);

      if (clo_execontext == EC_Some 
          && extra->lasttouched.uu_ec_ip.ip != 0) {
	 Char file[100];
	 UInt line;
	 Addr ip = extra->lasttouched.uu_ec_ip.ip;
	 
	 VG_(message)(Vg_UserMsg, " Word at %p last changed state from %s by thread %u",
		      err_addr,
		      pp_state(extra->lasttouched.state),
		      unpackTLS(extra->lasttouched.tls)->tid);
	 
	 if (VG_(get_filename_linenum)(ip, file, sizeof(file), &line)) {
	    VG_(message)(Vg_UserMsg, "   at %p: %y (%s:%u)",
			 ip, ip, file, line);
	 } else if (VG_(get_objname)(ip, file, sizeof(file))) {
	    VG_(message)(Vg_UserMsg, "   at %p: %y (in %s)",
			 ip, ip, file);
	 } else {
	    VG_(message)(Vg_UserMsg, "   at %p: %y", ip, ip);
	 }
      } else if (clo_execontext == EC_All 
                 && extra->lasttouched.uu_ec_ip.ec != NULL) {
	 VG_(message)(Vg_UserMsg, " Word at %p last changed state from %s in tid %u",
		      err_addr,
		      pp_state(extra->lasttouched.state),
		      unpackTLS(extra->lasttouched.tls)->tid);
	 VG_(pp_ExeContext)(extra->lasttouched.uu_ec_ip.ec);
      }
      break;
   }

   case MutexErr:
      VG_(message)(Vg_UserMsg, "Mutex problem at %p%(y trying to %s",
		   VG_(get_error_address)(err),
		   VG_(get_error_address)(err),
		   VG_(get_error_string)(err));
      VG_(pp_ExeContext)( VG_(get_error_where)(err) );
      if (extra->lasttouched.uu_ec_ip.ec != NULL) {
	 VG_(message)(Vg_UserMsg, " last touched by thread %d", extra->lasttid);
	 VG_(pp_ExeContext)(extra->lasttouched.uu_ec_ip.ec);
      }
      pp_AddrInfo(VG_(get_error_address)(err), &extra->addrinfo);
      break;

   case LockGraphErr: {
      const LockSet *heldset = extra->held_lockset;
      Addr err_addr = VG_(get_error_address)(err);
      Int i;

      msg = lockset_str(NULL, heldset);

      VG_(message)(Vg_UserMsg, "Mutex %p%(y locked in inconsistent order",
		   err_addr, err_addr);
      VG_(pp_ExeContext)( VG_(get_error_where)(err) );
      VG_(message)(Vg_UserMsg, " while holding locks %s", msg);

      for(i = 0; i < heldset->setsize; i++) {
	 const Mutex *lsmx = heldset->mutex[i];

	 /* needs to be a recursive search+display */
	 if (0 && !ismember(lsmx->lockdep, extra->mutex))
	    continue;
      
	 VG_(message)(Vg_UserMsg, " %p%(y last locked at", 
		      lsmx->mutexp, lsmx->mutexp);
	 VG_(pp_ExeContext)(lsmx->location);
	 VG_(free)(msg);
	 msg = lockset_str(NULL, lsmx->lockdep);
	 VG_(message)(Vg_UserMsg, " while depending on locks %s", msg);
      }
      
      break;
   }
   }

   if (msg != buf)
      VG_(free)(msg);
}


static Bool hg_recognised_suppression ( Char* name, Supp *su )
{
   if (0 == VG_(strcmp)(name, "Eraser")) {
      VG_(set_supp_kind)(su, RaceSupp);
      return True;
   } else {
      return False;
   }
}


static Bool hg_read_extra_suppression_info ( Int fd, Char* buf, Int nBuf, Supp* su )
{
   /* do nothing -- no extra suppression info present.  Return True to
      indicate nothing bad happened. */
   return True;
}


static Bool hg_error_matches_suppression(Error* err, Supp* su)
{
   tl_assert(VG_(get_supp_kind)(su) == RaceSupp);

   return (VG_(get_error_kind)(err) == RaceErr);
}

static Char* hg_get_error_name ( Error* err )
{
   if (RaceErr == VG_(get_error_kind)(err)) {
      return "Eraser";     // old name, required for backwards compatibility
   } else {
      return NULL;      /* Other errors types can't be suppressed */
   }
}

static void hg_print_extra_suppression_info ( Error* err )
{
   /* Do nothing */
}

static void hg_pre_mutex_lock(ThreadId tid, void* void_mutex)
{
   Mutex *mutex = get_mutex((Addr)void_mutex);

   test_mutex_state(mutex, MxLocked, tid);
}

static void hg_post_mutex_lock(ThreadId tid, void* void_mutex)
{
   static const Bool debug = False;
   Mutex *mutex = get_mutex((Addr)void_mutex);
   const LockSet*  ls;

   set_mutex_state(mutex, MxLocked, tid);

#  if DEBUG_LOCKS
   VG_(printf)("lock  (%u, %p)\n", tid, mutex->mutexp);
#  endif

   /* VG_(printf)("LOCK: held %d, new %p\n", thread_locks[tid], mutex); */
#  if LOCKSET_SANITY > 1
   sanity_check_locksets("hg_post_mutex_lock-IN");
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
      sanity_check_locksets("hg_post_mutex_lock-OUT");
}


static void hg_post_mutex_unlock(ThreadId tid, void* void_mutex)
{
   static const Bool debug = False;
   Int i = 0;
   Mutex *mutex = get_mutex((Addr)void_mutex);
   const LockSet *ls;

   test_mutex_state(mutex, MxUnlocked, tid);
   set_mutex_state(mutex, MxUnlocked, tid);

   if (!ismember(thread_locks[tid], mutex))
       return;

   if (debug || DEBUG_LOCKS)
      VG_(printf)("unlock(%u, %p%(y)\n", tid, mutex->mutexp, mutex->mutexp);

   if (debug || LOCKSET_SANITY > 1)
      sanity_check_locksets("hg_post_mutex_unlock-IN");

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
      sanity_check_locksets("hg_post_mutex_unlock-OUT");
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

static inline
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

#if DEBUG_ACCESSES
   #define DEBUG_STATE(args...)   \
      VG_(printf)("(%u) ", size), \
      VG_(printf)(args)
#else
   #define DEBUG_STATE(args...)
#endif

static void hg_mem_read_word(Addr a, ThreadId tid)
{
   shadow_word* sword /* egcs-2.91.66 complains uninit */ = NULL; 
   shadow_word  prevstate;
   ThreadLifeSeg *tls;
   const LockSet *ls;
   Bool statechange = False;

   static const void *const states[4] = {
      [Vge_Virgin]  &&st_virgin,
      [Vge_Excl]    &&st_excl,
      [Vge_Shar]    &&st_shar,
      [Vge_SharMod] &&st_sharmod,
   };

   tls = thread_seg[tid];
   tl_assert(tls != NULL && tls->tid == tid);

   sword = get_sword_addr(a);
   if (sword == SEC_MAP_ACCESS) {
      VG_(printf)("read distinguished 2ndary map! 0x%x\n", a);
      return;
   }

   prevstate = *sword;

   goto *states[sword->state];

   /* This looks like reading of unitialised memory, may be legit.  Eg. 
    * calloc() zeroes its values, so untouched memory may actually be 
    * initialised.   Leave that stuff to Valgrind.  */
  st_virgin:
   if (TID_INDICATING_NONVIRGIN == sword->other) {
      DEBUG_STATE("Read  VIRGIN --> EXCL:   %8x, %u\n", a, tid);
      if (DEBUG_VIRGIN_READS)
	 dump_around_a(a);
   } else {
      DEBUG_STATE("Read  SPECIAL --> EXCL:  %8x, %u\n", a, tid);
   }
   statechange = True;
   *sword = SW(Vge_Excl, packTLS(tls));       /* remember exclusive owner */
   tls->refcount++;
   goto done;

  st_excl: {
      ThreadLifeSeg *sw_tls = unpackTLS(sword->other);

      if (tls == sw_tls) {
	 DEBUG_STATE("Read  EXCL:              %8x, %u\n", a, tid);
      } else if (unpackTLS(TLSP_INDICATING_ALL) == sw_tls) {
	 DEBUG_STATE("Read  EXCL/ERR:          %8x, %u\n", a, tid);
      } else if (tlsIsDisjoint(tls, sw_tls)) {
	 DEBUG_STATE("Read  EXCL(%u) --> EXCL:  %8x, %u\n", sw_tls->tid, a, tid);
	 statechange = True;
	 sword->other = packTLS(tls);
	 sw_tls->refcount--;
	 tls->refcount++;
      } else {
	 DEBUG_STATE("Read  EXCL(%u) --> SHAR:  %8x, %u\n", sw_tls->tid, a, tid);
	 sw_tls->refcount--;
	 statechange = True;
	 *sword = SW(Vge_Shar, packLockSet(thread_locks[tid]));
	    
	 if (DEBUG_MEM_LOCKSET_CHANGES)
	    print_LockSet("excl read locks", unpackLockSet(sword->other));
      }
      goto done;
   }

  st_shar:
   DEBUG_STATE("Read  SHAR:              %8x, %u\n", a, tid);
   sword->other = packLockSet(intersect(unpackLockSet(sword->other), 
					thread_locks[tid]));
   statechange = sword->other != prevstate.other;
   goto done;

  st_sharmod:
   DEBUG_STATE("Read  SHAR_MOD:          %8x, %u\n", a, tid);
   ls = intersect(unpackLockSet(sword->other), 
		  thread_locks[tid]);
   sword->other = packLockSet(ls);

   statechange = sword->other != prevstate.other;

   if (isempty(ls)) {
      record_race_error(tid, a, False /* !is_write */, prevstate);
   }
   goto done;

  done:
   if (clo_execontext != EC_None && statechange) {
      EC_IP ecip;

      if (clo_execontext == EC_Some)
	 ecip = IP(VG_(get_IP)(tid), prevstate, tls);
      else
	 ecip = EC(VG_(record_ExeContext)(tid), prevstate, tls);
      setExeContext(a, ecip);
   }
}

static void hg_mem_read(Addr a, SizeT size, ThreadId tid)
{
   Addr end;

   end = ROUNDUP(a+size, 4);
   a = ROUNDDN(a, 4);

   for ( ; a < end; a += 4)
      hg_mem_read_word(a, tid);
}

static void hg_mem_write_word(Addr a, ThreadId tid)
{
   ThreadLifeSeg *tls;
   shadow_word* sword /* egcs-2.91.66 complains uninit */ = NULL;
   shadow_word  prevstate;
   Bool statechange = False;
   static const void *const states[4] = {
      [Vge_Virgin]  &&st_virgin,
      [Vge_Excl]    &&st_excl,
      [Vge_Shar]    &&st_shar,
      [Vge_SharMod] &&st_sharmod,
   };

   tls = thread_seg[tid];
   tl_assert(tls != NULL && tls->tid == tid);

   sword = get_sword_addr(a);
   if (sword == SEC_MAP_ACCESS) {
      VG_(printf)("read distinguished 2ndary map! 0x%x\n", a);
      return;
   }

   prevstate = *sword;

   goto *states[sword->state];

  st_virgin:
   if (TID_INDICATING_NONVIRGIN == sword->other)
      DEBUG_STATE("Write VIRGIN --> EXCL:   %8x, %u\n", a, tid);
   else
      DEBUG_STATE("Write SPECIAL --> EXCL:  %8x, %u\n", a, tid);
   statechange = True;
   *sword = SW(Vge_Excl, packTLS(tls));/* remember exclusive owner */
   tls->refcount++;
   goto done;

  st_excl: {
      ThreadLifeSeg *sw_tls = unpackTLS(sword->other);

      if (tls == sw_tls) {
	 DEBUG_STATE("Write EXCL:              %8x, %u\n", a, tid);
	 goto done;
      } else if (unpackTLS(TLSP_INDICATING_ALL) == sw_tls) {
	 DEBUG_STATE("Write EXCL/ERR:          %8x, %u\n", a, tid);
	 goto done;
      } else if (tlsIsDisjoint(tls, sw_tls)) {
	 DEBUG_STATE("Write EXCL(%u) --> EXCL: %8x, %u\n", sw_tls->tid, a, tid);
	 sword->other = packTLS(tls);
	 sw_tls->refcount--;
	 tls->refcount++;
	 goto done;
      } else {
	 DEBUG_STATE("Write EXCL(%u) --> SHAR_MOD: %8x, %u\n", sw_tls->tid, a, tid);
	 statechange = True;
	 sw_tls->refcount--;
	 *sword = SW(Vge_SharMod, packLockSet(thread_locks[tid]));
	 if(DEBUG_MEM_LOCKSET_CHANGES)
	    print_LockSet("excl write locks", unpackLockSet(sword->other));
	 goto SHARED_MODIFIED;
      }
   }

  st_shar:
   DEBUG_STATE("Write SHAR --> SHAR_MOD: %8x, %u\n", a, tid);
   sword->state = Vge_SharMod;
   sword->other = packLockSet(intersect(unpackLockSet(sword->other),
					thread_locks[tid]));
   statechange = True;
   goto SHARED_MODIFIED;

  st_sharmod:
   DEBUG_STATE("Write SHAR_MOD:          %8x, %u\n", a, tid);
   sword->other = packLockSet(intersect(unpackLockSet(sword->other), 
					thread_locks[tid]));
   statechange = sword->other != prevstate.other;

  SHARED_MODIFIED:
   if (isempty(unpackLockSet(sword->other))) {
      record_race_error(tid, a, True /* is_write */, prevstate);
   }
   goto done;

  done:
   if (clo_execontext != EC_None && statechange) {
      EC_IP ecip;

      if (clo_execontext == EC_Some)
	 ecip = IP(VG_(get_IP)(tid), prevstate, tls);
      else
	 ecip = EC(VG_(record_ExeContext)(tid), prevstate, tls);
      setExeContext(a, ecip);
   }
}

static void hg_mem_write(Addr a, SizeT size, ThreadId tid)
{
   Addr     end;

   end = ROUNDUP(a+size, 4);
   a = ROUNDDN(a, 4);

   for ( ; a < end; a += 4)
      hg_mem_write_word(a, tid);
}

#undef DEBUG_STATE

VGA_REGPARM(1) static void hg_mem_help_read_1(Addr a)
{
   hg_mem_read(a, 1, VG_(get_running_tid)());
}

VGA_REGPARM(1) static void hg_mem_help_read_2(Addr a)
{
   hg_mem_read(a, 2, VG_(get_running_tid)());
}

VGA_REGPARM(1) static void hg_mem_help_read_4(Addr a)
{
   hg_mem_read(a, 4, VG_(get_running_tid)());
}

VGA_REGPARM(2) static void hg_mem_help_read_N(Addr a, SizeT size)
{
   hg_mem_read(a, size, VG_(get_running_tid)());
}

VGA_REGPARM(2) static void hg_mem_help_write_1(Addr a, UInt val)
{
   if (*(UChar *)a != val)
      hg_mem_write(a, 1, VG_(get_running_tid)());
}
VGA_REGPARM(2) static void hg_mem_help_write_2(Addr a, UInt val)
{
   if (*(UShort *)a != val)
      hg_mem_write(a, 2, VG_(get_running_tid)());
}
VGA_REGPARM(2) static void hg_mem_help_write_4(Addr a, UInt val)
{
   if (*(UInt *)a != val)
      hg_mem_write(a, 4, VG_(get_running_tid)());
}
VGA_REGPARM(2) static void hg_mem_help_write_N(Addr a, SizeT size)
{
   hg_mem_write(a, size, VG_(get_running_tid)());
}

static void hg_thread_create(ThreadId parent, ThreadId child)
{
   if (0)
      VG_(printf)("CREATE: %u creating %u\n", parent, child);

   newTLS(child);
   addPriorTLS(child, parent);

   newTLS(parent);
}

static void hg_thread_join(ThreadId joiner, ThreadId joinee)
{
   if (0)
      VG_(printf)("JOIN: %u joining on %u\n", joiner, joinee);

   newTLS(joiner);
   addPriorTLS(joiner, joinee);

   clearTLS(joinee);
}

static Int __BUS_HARDWARE_LOCK__;

static void bus_lock(void)
{
   ThreadId tid = VG_(get_running_tid)();
   hg_pre_mutex_lock(tid, &__BUS_HARDWARE_LOCK__);
   hg_post_mutex_lock(tid, &__BUS_HARDWARE_LOCK__);
}

static void bus_unlock(void)
{
   ThreadId tid = VG_(get_running_tid)();
   hg_post_mutex_unlock(tid, &__BUS_HARDWARE_LOCK__);
}

/*--------------------------------------------------------------------*/
/*--- Client requests                                              ---*/
/*--------------------------------------------------------------------*/

static Bool hg_handle_client_request(ThreadId tid, UWord *args, UWord *ret)
{
   if (!VG_IS_TOOL_USERREQ('H','G',args[0]))
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
/*--- Setup and finalisation                                       ---*/
/*--------------------------------------------------------------------*/

static Bool hg_process_cmd_line_option(Char* arg)
{
   if      (VG_CLO_STREQ(arg, "--show-last-access=no"))
      clo_execontext = EC_None;
   else if (VG_CLO_STREQ(arg, "--show-last-access=some"))
      clo_execontext = EC_Some;
   else if (VG_CLO_STREQ(arg, "--show-last-access=all"))
      clo_execontext = EC_All;

   else VG_BOOL_CLO(arg, "--private-stacks", clo_priv_stacks)

   else 
      return VG_(replacement_malloc_process_cmd_line_option)(arg);

   return True;
}

static void hg_print_usage(void)
{
   VG_(printf)(
"    --private-stacks=yes|no   assume thread stacks are used privately [no]\n"
"    --show-last-access=no|some|all\n"
"                           show location of last word access on error [no]\n"
   );
   VG_(replacement_malloc_print_usage)();
}

static void hg_print_debug_usage(void)
{
   VG_(replacement_malloc_print_debug_usage)();
}

static void hg_post_clo_init(void)
{
   void (*stack_tracker)(Addr a, SizeT len);
   
   if (clo_execontext) {
      execontext_map = VG_(malloc)(sizeof(ExeContextMap *) * 65536);
      VG_(memset)(execontext_map, 0, sizeof(ExeContextMap *) * 65536);
   }

   if (clo_priv_stacks)
      stack_tracker = & hg_new_mem_stack_private;
   else
      stack_tracker = & hg_new_mem_stack;

   VG_(track_new_mem_stack)        (stack_tracker);
   VG_(track_new_mem_stack_signal) (stack_tracker);
}


static void hg_fini(Int exitcode)
{
   if (DEBUG_LOCK_TABLE) {
      pp_all_LockSets();
      pp_all_mutexes();
   }

   if (LOCKSET_SANITY)
      sanity_check_locksets("hg_fini");

   if (VG_(clo_verbosity) > 0)
      VG_(message)(Vg_UserMsg, "%u possible data races found; %u lock order problems",
		   n_hg_warnings, n_lockorder_warnings);

   if (0)
      VG_(printf)("stk_ld:%u+stk_st:%u = %u  nonstk_ld:%u+nonstk_st:%u = %u  %u%%\n",
		  stk_ld, stk_st, stk_ld + stk_st,
		  nonstk_ld, nonstk_st, nonstk_ld + nonstk_st,
		  ((stk_ld+stk_st)*100) / (stk_ld + stk_st + nonstk_ld + nonstk_st));
}

static void hg_pre_clo_init(void)
{
   Int i;
   LockSet *empty;

   VG_(details_name)            ("Helgrind");
   VG_(details_version)         (NULL);
   VG_(details_description)     ("a data race detector");
   VG_(details_copyright_author)(
      "Copyright (C) 2002-2005, and GNU GPL'd, by Nicholas Nethercote et al.");
   VG_(details_bug_reports_to)  (VG_BUGS_TO);
   VG_(details_avg_translation_sizeB) ( 115 );

   VG_(basic_tool_funcs)          (hg_post_clo_init,
                                   hg_instrument,
                                   hg_fini);

   VG_(needs_core_errors)         ();
   VG_(needs_tool_errors)         (hg_eq_Error,
                                   hg_pp_Error,
                                   hg_update_extra,
                                   hg_recognised_suppression,
                                   hg_read_extra_suppression_info,
                                   hg_error_matches_suppression,
                                   hg_get_error_name,
                                   hg_print_extra_suppression_info);
   VG_(needs_data_syms)           ();
   VG_(needs_client_requests)     (hg_handle_client_request);
   VG_(needs_sanity_checks)       (hg_cheap_sanity_check,
                                   hg_expensive_sanity_check);
   VG_(needs_command_line_options)(hg_process_cmd_line_option,
                                   hg_print_usage,
                                   hg_print_debug_usage);
   VG_(needs_shadow_memory)       ();

   VG_(malloc_funcs)              (hg_malloc,
                                   hg___builtin_new,
                                   hg___builtin_vec_new,
                                   hg_memalign,
                                   hg_calloc,
                                   hg_free,
                                   hg___builtin_delete,
                                   hg___builtin_vec_delete,
                                   hg_realloc,
                                   8 );

   VG_(track_new_mem_startup)      (& hg_new_mem_startup);

   /* stack ones not decided until hg_post_clo_init() */

   VG_(track_new_mem_brk)         (& make_writable);
   VG_(track_new_mem_mmap)        (& hg_new_mem_startup);

   VG_(track_change_mem_mprotect) (& hg_set_perms);

   VG_(track_ban_mem_stack)       (NULL);

   VG_(track_die_mem_stack)       (NULL);
   VG_(track_die_mem_stack_signal)(NULL);
   VG_(track_die_mem_brk)         (NULL);
   VG_(track_die_mem_munmap)      (NULL);

   VG_(track_pre_mem_read)        (& hg_pre_mem_read);
   VG_(track_pre_mem_read_asciiz) (& hg_pre_mem_read_asciiz);
   VG_(track_pre_mem_write)       (& hg_pre_mem_write);
   VG_(track_post_mem_write)      (NULL);

   VG_(track_post_thread_create)  (& hg_thread_create);
   VG_(track_post_thread_join)    (& hg_thread_join);

   VG_(track_pre_mutex_lock)      (& hg_pre_mutex_lock);
   VG_(track_post_mutex_lock)     (& hg_post_mutex_lock);
   VG_(track_post_mutex_unlock)   (& hg_post_mutex_unlock);

   for (i = 0; i < LOCKSET_HASH_SZ; i++)
      lockset_hash[i] = NULL;

   empty = alloc_LockSet(0);
   insert_LockSet(empty);
   emptyset = empty;

   /* Init lock table and thread segments */
   for (i = 0; i < VG_N_THREADS; i++) {
      thread_locks[i] = empty;

      newTLS(i);
   }

   init_shadow_memory();
   hg_malloc_list = VG_(HT_construct)();
}

/* Uses a 1:1 mapping */
VG_DETERMINE_INTERFACE_VERSION(hg_pre_clo_init, 1.0)

/*--------------------------------------------------------------------*/
/*--- end                                                hg_main.c ---*/
/*--------------------------------------------------------------------*/
