
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


static UInt n_eraser_warnings = 0;


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
   { Vge_VirginInit, Vge_NonVirginInit, Vge_SegmentInit } 
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

static shadow_word virgin_sword = { 0, Vge_Virgin };

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

   sword.other = VG_(get_current_tid_1_if_root)();
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

   sk_assert(1 == VG_(get_current_tid_1_if_root)());
   sword.other = TID_INDICATING_NONVIRGIN;
   sword.state = Vge_Virgin;
   set_sword(a, virgin_sword);
}


/*------------------------------------------------------------*/
/*--- Implementation of lock sets.                         ---*/
/*------------------------------------------------------------*/

#define M_LOCKSET_TABLE 1000

#include <pthread.h>

typedef 
   struct _LockSet {
       pthread_mutex_t* mutex;
       struct _LockSet* next;
   } LockSet;


/* Each one is an index into the lockset table. */
static UInt thread_locks[VG_N_THREADS];

/* # lockset table entries used. */
static Int n_lockset_table = 1; 

/* lockset_table[0] is always NULL, representing the empty lockset */
static LockSet* lockset_table[M_LOCKSET_TABLE];


static __inline__
Bool is_valid_lockset_id ( Int id )
{
   return id >= 0 && id < n_lockset_table;
}


static
Int allocate_LockSet(LockSet* set)
{
   if (n_lockset_table >= M_LOCKSET_TABLE) 
      VG_(skin_panic)("lockset table full -- increase M_LOCKSET_TABLE");
   lockset_table[n_lockset_table] = set;
   n_lockset_table++;
#  if DEBUG_MEM_LOCKSET_CHANGES || DEBUG_NEW_LOCKSETS
   VG_(printf)("allocate LOCKSET VECTOR %p to %d\n", set, n_lockset_table-1);
#  endif
   return n_lockset_table-1;
}


static 
void pp_LockSet(LockSet* p)
{
   VG_(printf)("{ ");
   while (p != NULL) {
      VG_(printf)("%x ", p->mutex);
      p = p->next;
   }
   VG_(printf)("}\n");
}


static __attribute__((unused))
void pp_all_LockSets ( void )
{
   Int i;
   for (i = 0; i < n_lockset_table; i++) {
      VG_(printf)("[%d] = ", i);
      pp_LockSet(lockset_table[i]);
   }
}


static 
void free_LockSet(LockSet *p)
{
   LockSet* q;
   while (NULL != p) {
      q = p;
      p = p->next;
      VG_(free)(q);
#     if DEBUG_MEM_LOCKSET_CHANGES
      VG_(printf)("free'd   %x\n", q);
#     endif
   }
}


static 
Bool structural_eq_LockSet(LockSet* a, LockSet* b)
{
   while (a && b) {
      if (a->mutex != b->mutex) {
         return False;
      }
      a = a->next;
      b = b->next;
   }
   return (NULL == a && NULL == b);
}


#if LOCKSET_SANITY 
/* Check invariants:
   - all locksets are unique
   - each set is a linked list in strictly increasing order of mutex addr 
*/
static
void sanity_check_locksets ( Char* caller )
{
   Int              i, j, badness;
   LockSet*         v;
   pthread_mutex_t* mx_prev;

   badness = 0;
   i = j = -1;

   //VG_(printf)("sanity %s\n", caller);
   /* Check really simple things first */

   if (n_lockset_table < 1 || n_lockset_table > M_LOCKSET_TABLE)
      { badness = 1; goto baaad; }

   if (lockset_table[0] != NULL)
      { badness = 2; goto baaad; }

   for (i = 1; i < n_lockset_table; i++)
      if (lockset_table[i] == NULL)
         { badness = 3; goto baaad; }

   for (i = n_lockset_table; i < M_LOCKSET_TABLE; i++)
      if (lockset_table[i] != NULL)
         { badness = 4; goto baaad; }

   /* Check the sanity of each individual set. */
   for (i = 1; i < n_lockset_table; i++) {
      v = lockset_table[i];
      mx_prev = (pthread_mutex_t*)0;
      while (True) {
         if (v == NULL) break;
         if (mx_prev >= v->mutex) 
            { badness = 5; goto baaad; }
         mx_prev = v->mutex;
         v = v->next;
      }
   }

   /* Ensure the sets are unique, both structurally and in respect of
      the address of their first nodes. */
   for (i = 1; i < n_lockset_table; i++) {
      for (j = i+1; j < n_lockset_table; j++) {
         if (lockset_table[i] == lockset_table[j]) 
            { badness = 6; goto baaad; }
         if (structural_eq_LockSet(lockset_table[i], lockset_table[j])) 
            { badness = 7; goto baaad; }
      }
   }
   return;

  baaad:
   VG_(printf)("sanity_check_locksets: "
               "i = %d, j = %d, badness = %d, caller = %s\n", 
               i, j, badness, caller);
   pp_all_LockSets();
   VG_(skin_panic)("sanity_check_locksets");
}
#endif /* LOCKSET_SANITY */


/* Builds ia with mx removed.  mx should actually be in ia! 
   (a checked assertion).  Resulting set should not already
   exist in the table (unchecked).
*/
static 
UInt remove ( UInt ia, pthread_mutex_t* mx )
{
   Int       found, res;
   LockSet*  new_vector = NULL;
   LockSet*  new_node;
   LockSet** prev_ptr = &new_vector;
   LockSet*  a = lockset_table[ia];
   sk_assert(is_valid_lockset_id(ia));

#  if DEBUG_MEM_LOCKSET_CHANGES
   VG_(printf)("Removing from %d mutex %p:\n", ia, mx);
#  endif

#  if DEBUG_MEM_LOCKSET_CHANGES
   print_LockSet(a);
#  endif

#  if LOCKSET_SANITY 
   sanity_check_locksets("remove-IN");
#  endif

   /* Build the intersection of the two lists */
   found = 0;
   while (a) {
      if (a->mutex != mx) {
         new_node = VG_(malloc)(sizeof(LockSet));
#        if DEBUG_MEM_LOCKSET_CHANGES
         VG_(printf)("malloc'd %x\n", new_node);
#        endif
         new_node->mutex = a->mutex;
         *prev_ptr = new_node;
         prev_ptr = &((*prev_ptr)->next);
         a = a->next;
      } else {
         found++;
      }
      *prev_ptr = NULL;
   }
   sk_assert(found == 1 /* sigh .. if the client is buggy */ || found == 0 );

   /* Preserve uniqueness invariants in face of client buggyness */
   if (found == 0) {
      free_LockSet(new_vector);
      return ia;
   }

   /* Add to the table. */
   res = allocate_LockSet(new_vector);

#  if LOCKSET_SANITY 
   sanity_check_locksets("remove-OUT");
#  endif

   return res;
}


/* Tricky: equivalent to (compare(insert(missing_elem, a), b)), but
 * doesn't do the insertion.  Returns True if they match.
 */
static Bool 
weird_LockSet_equals(LockSet* a, LockSet* b, 
                     pthread_mutex_t* missing_mutex)
{
   /* Idea is to try and match each element of b against either an
      element of a, or missing_mutex. */
   while (True) {
      if (b == NULL) 
         break;
      /* deal with missing already being in a */
      if (a && a->mutex == missing_mutex) 
         a = a->next;
      /* match current b element either against a or missing */
      if (b->mutex == missing_mutex) {
         b = b->next;
         continue;
      }
      /* wasn't == missing, so have to match from a, or fail */
      if (a && b->mutex == a->mutex) {
         a = a->next;
         b = b->next;
         continue;
      }
      break;
   }
   return (b==NULL ? True : False);
}


/* Builds the intersection, and then unbuilds it if it's already in the table.
 */
static UInt intersect(UInt ia, UInt ib)
{
   Int       i;
   LockSet*  a = lockset_table[ia];
   LockSet*  b = lockset_table[ib];
   LockSet*  new_vector = NULL;
   LockSet*  new_node;
   LockSet** prev_ptr = &new_vector;

#  if DEBUG_MEM_LOCKSET_CHANGES
   VG_(printf)("Intersecting %d %d:\n", ia, ib);
#  endif

#  if LOCKSET_SANITY 
   sanity_check_locksets("intersect-IN");
#  endif

   /* Fast case -- when the two are the same */
   if (ia == ib) {
#     if DEBUG_MEM_LOCKSET_CHANGES
      VG_(printf)("Fast case -- both the same: %u\n", ia);
      print_LockSet(a);
#     endif
      return ia;
   }

#  if DEBUG_MEM_LOCKSET_CHANGES
   print_LockSet(a);
   print_LockSet(b);
#  endif

   /* Build the intersection of the two lists */
   while (a && b) {
      if (a->mutex == b->mutex) {
         new_node = VG_(malloc)(sizeof(LockSet));
#        if DEBUG_MEM_LOCKSET_CHANGES
         VG_(printf)("malloc'd %x\n", new_node);
#        endif
         new_node->mutex = a->mutex;
         *prev_ptr = new_node;
         prev_ptr = &((*prev_ptr)->next);
         a = a->next;
         b = b->next;
      } else if (a->mutex < b->mutex) {
         a = a->next;
      } else if (a->mutex > b->mutex) {
         b = b->next;
      } else VG_(skin_panic)("STOP PRESS: Laws of arithmetic broken");

      *prev_ptr = NULL;
   }

   /* Now search for it in the table, adding it if not seen before */
   for (i = 0; i < n_lockset_table; i++) {
      if (structural_eq_LockSet(lockset_table[i], new_vector))
         break;
   }

   if (i == n_lockset_table) {
     i = allocate_LockSet(new_vector);
   } else {
     free_LockSet(new_vector);
   }

   /* Check we won't overflow the OTHER_BITS bits of sword->other */
   sk_assert(i < (1 << OTHER_BITS));

#  if LOCKSET_SANITY 
   sanity_check_locksets("intersect-OUT");
#  endif

   return i;
}


/*------------------------------------------------------------*/
/*--- Setting and checking permissions.                    ---*/
/*------------------------------------------------------------*/

static
void set_address_range_state ( Addr a, UInt len /* in bytes */, 
                               VgeInitStatus status )
{
   Addr aligned_a, end, aligned_end;

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

   /* Memory block may not be aligned or a whole word multiple.  In neat cases,
    * we have to init len/4 words (len is in bytes).  In nasty cases, it's
    * len/4+1 words.  This works out which it is by aligning the block and
    * seeing if the end byte is in the same word as it is for the unaligned
    * block; if not, it's the awkward case. */
   aligned_a   = a & 0xc;                       /* zero bottom two bits */
   end         = a + len;
   aligned_end = aligned_a + len;
   if ((end & 0xc) != (aligned_end & 0xc)) {
       end += 4;    /* len/4 + 1 case */
   }

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


// SSS: change name
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
static void eraser_mem_read (Addr a, UInt data_size);
static void eraser_mem_write(Addr a, UInt data_size);

static
void eraser_pre_mem_read(CorePart part, ThreadState* tst,
                         Char* s, UInt base, UInt size )
{
   eraser_mem_read(base, size);
}

static
void eraser_pre_mem_read_asciiz(CorePart part, ThreadState* tst,
                                Char* s, UInt base )
{
   eraser_mem_read(base, VG_(strlen)((Char*)base));
}

static
void eraser_pre_mem_write(CorePart part, ThreadState* tst,
                          Char* s, UInt base, UInt size )
{
   eraser_mem_write(base, size);
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
                       Bool nn, Bool rr, Bool ww, Bool xx)
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

#define uInstr1   VG_(new_UInstr1)
#define uInstr2   VG_(new_UInstr2)
#define uLiteral  VG_(set_lit_field)
#define uCCall    VG_(set_ccall_fields)
#define newTemp   VG_(get_new_temp)

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

         /* For LOAD, address is in val1 */
         case LOAD:
            t_size = newTemp(cb);
            uInstr2(cb, MOV,   4, Literal, 0, TempReg, t_size);
            uLiteral(cb, (UInt)u_in->size);

            sk_assert(1 == u_in->size || 2 == u_in->size || 4 == u_in->size || 
                      8 == u_in->size || 10 == u_in->size);
            uInstr2(cb, CCALL, 0, TempReg, u_in->val1, TempReg, t_size);
            // SSS: make regparms(2) eventually...
            uCCall(cb, (Addr) & eraser_mem_read, 2, 0, False);
            VG_(copy_UInstr)(cb, u_in);
            t_size = INVALID_TEMPREG;
            break;

         /* For others, address is in val2 */
         case STORE:  case FPU_R:  case FPU_W:
            t_size = newTemp(cb);
            uInstr2(cb, MOV,   4, Literal, 0, TempReg, t_size);
            uLiteral(cb, (UInt)u_in->size);

            sk_assert(1 == u_in->size || 2 == u_in->size || 4 == u_in->size || 
                      8 == u_in->size || 10 == u_in->size);
            uInstr2(cb, CCALL, 0, TempReg, u_in->val2, TempReg, t_size);
            uCCall(cb, (Addr) & eraser_mem_write, 2, 0, False);
            VG_(copy_UInstr)(cb, u_in);
            t_size = INVALID_TEMPREG;
            break;

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
      EraserErr 
   }
   EraserErrorKind;


static void record_eraser_error ( ThreadId tid, Addr a, Bool is_write )
{
   VG_(maybe_record_error)( VG_(get_ThreadState)(tid), EraserErr, a, 
                            (is_write ? "writing" : "reading"),
                            /*extra*/NULL);
}


Bool SK_(eq_SkinError) ( VgRes not_used,
                          SkinError* e1, SkinError* e2 )
{
   sk_assert(EraserErr == e1->ekind && EraserErr == e2->ekind);
   if (e1->string != e2->string) return False;
   if (0 != VG_(strcmp)(e1->string, e2->string)) return False;
   return True;
}


void SK_(pp_SkinError) ( SkinError* err, void (*pp_ExeContext)(void) )
{
   sk_assert(EraserErr == err->ekind);
   VG_(message)(Vg_UserMsg, "Possible data race %s variable at 0x%x",
                err->string, err->addr );
   pp_ExeContext();
}


void SK_(dup_extra_and_update)(SkinError* err)
{
   /* do nothing -- extra field not used, and no need to update */
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


// SSS: copying mutex's pointer... is that ok?  Could they get deallocated?
// (does that make sense, deallocating a mutex?)
static void eraser_post_mutex_lock(ThreadId tid, void* void_mutex)
{
   Int i = 1;
   LockSet*  new_node;
   LockSet*  p;
   LockSet** q;
   pthread_mutex_t* mutex = (pthread_mutex_t*)void_mutex;
   
#  if DEBUG_LOCKS
   VG_(printf)("lock  (%u, %x)\n", tid, mutex);
#  endif

   sk_assert(tid < VG_N_THREADS &&
             thread_locks[tid] < M_LOCKSET_TABLE);
   /* VG_(printf)("LOCK: held %d, new %p\n", thread_locks[tid], mutex); */
#  if LOCKSET_SANITY > 1
   sanity_check_locksets("eraser_post_mutex_lock-IN");
#  endif

   while (True) {
      if (i == M_LOCKSET_TABLE) 
         VG_(skin_panic)("lockset table full -- increase M_LOCKSET_TABLE");

      /* the lockset didn't already exist */
      if (i == n_lockset_table) {

         p = lockset_table[thread_locks[tid]];
         q = &lockset_table[i];

         /* copy the thread's lockset, creating a new list */
         while (p != NULL) {
            new_node = VG_(malloc)(sizeof(LockSet));
            new_node->mutex = p->mutex;
            *q = new_node;
            q = &((*q)->next);
            p = p->next;
         }
         (*q) = NULL;

         /* find spot for the new mutex in the new list */
         p = lockset_table[i];
         q = &lockset_table[i];
         while (NULL != p && mutex > p->mutex) {
            p = p->next;
            q = &((*q)->next);
         }

         /* insert new mutex in new list */
         new_node = VG_(malloc)(sizeof(LockSet));
         new_node->mutex = mutex;
         new_node->next = p;
         (*q) = new_node;

         p = lockset_table[i];
         sk_assert(i == n_lockset_table);
         n_lockset_table++;

#        if DEBUG_NEW_LOCKSETS
         VG_(printf)("new lockset vector (%d): ", i);
         print_LockSet(p);
#        endif
         
         goto done;

      } else {
         /* If this succeeds, the required vector (with the new mutex added)
          * already exists in the table at position i.  Otherwise, keep
          * looking. */
         if (weird_LockSet_equals(lockset_table[thread_locks[tid]],
                                  lockset_table[i], mutex)) {
            goto done;
         }
      }
      /* if we get to here, table lockset didn't match the new thread
       * lockset, so keep looking */
      i++;
   }

  done:
   /* Update the thread's lock vector */
   thread_locks[tid] = i;
#  if DEBUG_LOCKS
   VG_(printf)("tid %u now has lockset %d\n", tid, i);
#  endif

#  if LOCKSET_SANITY > 1
   sanity_check_locksets("eraser_post_mutex_lock-OUT");
#  endif

}


static void eraser_post_mutex_unlock(ThreadId tid, void* void_mutex)
{
   Int i = 0;
   pthread_mutex_t* mutex = (pthread_mutex_t*)void_mutex;
   
#  if DEBUG_LOCKS
   VG_(printf)("unlock(%u, %x)\n", tid, mutex);
#  endif

#  if LOCKSET_SANITY > 1
   sanity_check_locksets("eraser_post_mutex_unlock-IN");
#  endif

   // find the lockset that is the current one minus tid, change thread to use
   // that index.
   
   while (True) {

      if (i == n_lockset_table) {
         /* We can't find a suitable pre-made set, so we'll have to
            make one. */
         i = remove ( thread_locks[tid], mutex );
         break;
      }

      /* Args are in opposite order to call above, for reverse effect */
      if (weird_LockSet_equals( lockset_table[i],
                                lockset_table[thread_locks[tid]], mutex) ) {
         /* found existing diminished set -- the best outcome. */
         break;
      }

      i++;
   }

   /* Update the thread's lock vector */
#  if DEBUG_LOCKS
   VG_(printf)("tid %u reverts from %d to lockset %d\n", 
               tid, thread_locks[tid], i);
#  endif

   thread_locks[tid] = i;

#  if LOCKSET_SANITY > 1
   sanity_check_locksets("eraser_post_mutex_unlock-OUT");
#  endif
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


static void eraser_mem_read(Addr a, UInt size)
{
   shadow_word* sword;
   ThreadId tid = VG_(get_current_tid_1_if_root)();
   Addr     end = a + 4*compute_num_words_accessed(a, size);

   for ( ; a < end; a += 4) {

      sword = get_sword_addr(a);
      if (sword == SEC_MAP_ACCESS) {
         VG_(printf)("read distinguished 2ndary map! 0x%x\n", a);
         continue;
      }

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

         } else {
            DEBUG_STATE("Read  EXCL(%u) --> SHAR:  %8x, %u\n", sword->other, a, tid);
            sword->state = Vge_Shar;
            sword->other = thread_locks[tid];
#           if DEBUG_MEM_LOCKSET_CHANGES
            print_LockSet(lockset_table[sword->other]);
#           endif
         }
         break;

      case Vge_Shar:
         DEBUG_STATE("Read  SHAR:              %8x, %u\n", a, tid);
         sword->other = intersect(sword->other, thread_locks[tid]);
         break;

      case Vge_SharMod:
         DEBUG_STATE("Read  SHAR_MOD:          %8x, %u\n", a, tid);
         sword->other = intersect(sword->other, thread_locks[tid]);

         if (lockset_table[sword->other] == NULL) {
            record_eraser_error(tid, a, False /* !is_write */);
            n_eraser_warnings++;
         }
         break;

      default:
         VG_(skin_panic)("Unknown eraser state");
      }
   }
}


static void eraser_mem_write(Addr a, UInt size)
{
   shadow_word* sword;
   ThreadId tid = VG_(get_current_tid_1_if_root)();
   Addr     end = a + 4*compute_num_words_accessed(a, size);

   for ( ; a < end; a += 4) {

      sword = get_sword_addr(a);
      if (sword == SEC_MAP_ACCESS) {
         VG_(printf)("read distinguished 2ndary map! 0x%x\n", a);
         continue;
      }

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

         } else {
            DEBUG_STATE("Write EXCL(%u) --> SHAR_MOD: %8x, %u\n", sword->other, a, tid);
            sword->state = Vge_SharMod;
            sword->other = thread_locks[tid];
#           if DEBUG_MEM_LOCKSET_CHANGES
            print_LockSet(lockset_table[sword->other]);
#           endif
            goto SHARED_MODIFIED;
         }

      case Vge_Shar:
         DEBUG_STATE("Write SHAR --> SHAR_MOD: %8x, %u\n", a, tid);
         sword->state = Vge_SharMod;
         sword->other = intersect(sword->other, thread_locks[tid]);
         goto SHARED_MODIFIED;

      case Vge_SharMod:
         DEBUG_STATE("Write SHAR_MOD:          %8x, %u\n", a, tid);
         sword->other = intersect(sword->other, thread_locks[tid]);
         SHARED_MODIFIED:
         if (lockset_table[sword->other] == NULL) {
            record_eraser_error(tid, a, True /* is_write */);
            n_eraser_warnings++;
         }
         break;

      default:
         VG_(skin_panic)("Unknown eraser state");
      }
   }
}

#undef DEBUG_STATE


/*--------------------------------------------------------------------*/
/*--- Setup                                                        ---*/
/*--------------------------------------------------------------------*/

void SK_(pre_clo_init)(VgDetails* details, VgNeeds* needs, VgTrackEvents* track)
{
   Int i;

   details->name             = "helgrind";
   details->version          = NULL;
   details->description      = "a data race detector";
   details->copyright_author =
      "Copyright (C) 2002, and GNU GPL'd, by Nicholas Nethercote.";
   details->bug_reports_to   = "njn25@cam.ac.uk";

   needs->core_errors = True;
   needs->skin_errors = True;

   track->new_mem_startup       = & eraser_new_mem_startup;
   track->new_mem_heap          = & eraser_new_mem_heap;
   track->new_mem_stack         = & make_writable;
   track->new_mem_stack_aligned = & make_writable_aligned;
   track->new_mem_stack_signal  = & make_writable;
   track->new_mem_brk           = & make_writable;
   track->new_mem_mmap          = & eraser_set_perms;

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

   VG_(register_compact_helper)((Addr) & eraser_mem_read);
   VG_(register_compact_helper)((Addr) & eraser_mem_write);

   /* Init lock table */
   for (i = 0; i < VG_N_THREADS; i++) 
      thread_locks[i] = 0 /* the empty lock set */;

   lockset_table[0] = NULL;
   for (i = 1; i < M_LOCKSET_TABLE; i++) 
      lockset_table[i] = NULL;

   init_shadow_memory();
}


void SK_(post_clo_init)(void)
{
}


void SK_(fini)(void)
{
#  if DEBUG_LOCK_TABLE
   pp_all_LockSets();
#  endif
#  if LOCKSET_SANITY 
   sanity_check_locksets("SK_(fini)");
#  endif
   VG_(message)(Vg_UserMsg, "%u possible data races found", n_eraser_warnings);
}

/*--------------------------------------------------------------------*/
/*--- end                                                hg_main.c ---*/
/*--------------------------------------------------------------------*/
