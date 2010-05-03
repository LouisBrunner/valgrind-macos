
/*--------------------------------------------------------------------*/
/*--- Cache simulation.                                            ---*/
/*---                                                        sim.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Callgrind, a Valgrind tool for call graph
   profiling programs.

   Copyright (C) 2003-2005, Josef Weidendorfer (Josef.Weidendorfer@gmx.de)

   This tool is derived from and contains code from Cachegrind
   Copyright (C) 2002-2010 Nicholas Nethercote (njn@valgrind.org)

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

#include "global.h"


/* Notes:
  - simulates a write-allocate cache
  - (block --> set) hash function uses simple bit selection
  - handling of references straddling two cache blocks:
      - counts as only one cache access (not two)
      - both blocks hit                  --> one hit
      - one block hits, the other misses --> one miss
      - both blocks miss                 --> one miss (not two)
*/

/* Cache configuration */
#include "cg_arch.h"

/* additional structures for cache use info, separated
 * according usage frequency:
 * - line_loaded : pointer to cost center of instruction 
 *                 which loaded the line into cache.
 *                 Needed to increment counters when line is evicted.
 * - line_use    : updated on every access
 */
typedef struct {
  UInt count;
  UInt mask; /* e.g. for 64Byte line size 1bit/2Byte */
} line_use;

typedef struct {
  Addr memline, iaddr;
  line_use* dep_use; /* point to higher-level cacheblock for this memline */
  ULong* use_base;
} line_loaded;  

/* Cache state */
typedef struct {
   char*        name;
   int          size;                   /* bytes */
   int          assoc;
   int          line_size;              /* bytes */
   Bool         sectored;  /* prefetch nearside cacheline on read */
   int          sets;
   int          sets_min_1;
   int          line_size_bits;
   int          tag_shift;
   UWord        tag_mask;
   char         desc_line[128];
   UWord*       tags;

  /* for cache use */
   int          line_size_mask;
   int*         line_start_mask;
   int*         line_end_mask;
   line_loaded* loaded;
   line_use*    use;
} cache_t2;

/*
 * States of flat caches in our model.
 * We use a 2-level hierarchy, 
 */
static cache_t2 I1, D1, L2;

/* Lower bits of cache tags are used as flags for a cache line */
#define CACHELINE_FLAGMASK (MIN_LINE_SIZE-1)
#define CACHELINE_DIRTY    1


/* Cache simulator Options */
static Bool clo_simulate_writeback = False;
static Bool clo_simulate_hwpref = False;
static Bool clo_simulate_sectors = False;
static Bool clo_collect_cacheuse = False;

/* Following global vars are setup before by
 *  setup_bbcc()/cachesim_after_bbsetup():
 *
 * - Addr   bb_base     (instruction start address of original BB)
 * - ULong* cost_base   (start of cost array for BB)
 * - BBCC*  nonskipped  (only != 0 when in a function not skipped)
 */

/* Offset to events in event set, used in log_* functions
 * <off_EventSet_BasicEventSet>: offset where basic set is found
 */
static Int off_UIr_Ir;
static Int off_UIrDr_Ir,   off_UIrDr_Dr;
static Int off_UIrDrDw_Ir, off_UIrDrDw_Dr, off_UIrDrDw_Dw;
static Int off_UIrDw_Ir,   off_UIrDw_Dw;
static Int off_UIrDwDr_Ir, off_UIrDwDr_Dr, off_UIrDwDr_Dw;

static Addr   bb_base;
static ULong* cost_base;
static InstrInfo* current_ii;

/* Cache use offsets */
/* The offsets are only correct because all per-instruction event sets get
 * the "Use" set added first !
 */
static Int off_I1_AcCost  = 0;
static Int off_I1_SpLoss  = 1;
static Int off_D1_AcCost  = 0;
static Int off_D1_SpLoss  = 1;
static Int off_L2_AcCost  = 2;
static Int off_L2_SpLoss  = 3;

/* Cache access types */
typedef enum { Read = 0, Write = CACHELINE_DIRTY } RefType;

/* Result of a reference into a flat cache */
typedef enum { Hit  = 0, Miss, MissDirty } CacheResult;

/* Result of a reference into a hierarchical cache model */
typedef enum {
    L1_Hit, 
    L2_Hit,
    MemAccess,
    WriteBackMemAccess } CacheModelResult;

typedef CacheModelResult (*simcall_type)(Addr, UChar);

static struct {
    simcall_type I1_Read;
    simcall_type D1_Read;
    simcall_type D1_Write;
} simulator;

/*------------------------------------------------------------*/
/*--- Cache Simulator Initialization                       ---*/
/*------------------------------------------------------------*/

static void cachesim_clearcache(cache_t2* c)
{
  Int i;

  for (i = 0; i < c->sets * c->assoc; i++)
    c->tags[i] = 0;
  if (c->use) {
    for (i = 0; i < c->sets * c->assoc; i++) {
      c->loaded[i].memline  = 0;
      c->loaded[i].use_base = 0;
      c->loaded[i].dep_use = 0;
      c->loaded[i].iaddr = 0;
      c->use[i].mask    = 0;
      c->use[i].count   = 0;
      c->tags[i] = i % c->assoc; /* init lower bits as pointer */
    }
  }
}

static void cacheuse_initcache(cache_t2* c);

/* By this point, the size/assoc/line_size has been checked. */
static void cachesim_initcache(cache_t config, cache_t2* c)
{
   c->size      = config.size;
   c->assoc     = config.assoc;
   c->line_size = config.line_size;
   c->sectored  = False; // FIXME

   c->sets           = (c->size / c->line_size) / c->assoc;
   c->sets_min_1     = c->sets - 1;
   c->line_size_bits = VG_(log2)(c->line_size);
   c->tag_shift      = c->line_size_bits + VG_(log2)(c->sets);
   c->tag_mask       = ~((1<<c->tag_shift)-1);

   /* Can bits in tag entries be used for flags?
    * Should be always true as MIN_LINE_SIZE >= 16 */
   CLG_ASSERT( (c->tag_mask & CACHELINE_FLAGMASK) == 0);

   if (c->assoc == 1) {
      VG_(sprintf)(c->desc_line, "%d B, %d B, direct-mapped%s",
		   c->size, c->line_size,
		   c->sectored ? ", sectored":"");
   } else {
      VG_(sprintf)(c->desc_line, "%d B, %d B, %d-way associative%s",
		   c->size, c->line_size, c->assoc,
		   c->sectored ? ", sectored":"");
   }

   c->tags = (UWord*) CLG_MALLOC("cl.sim.cs_ic.1",
                                 sizeof(UWord) * c->sets * c->assoc);
   if (clo_collect_cacheuse)
       cacheuse_initcache(c);
   else
     c->use = 0;
   cachesim_clearcache(c);
}


#if 0
static void print_cache(cache_t2* c)
{
   UInt set, way, i;

   /* Note initialisation and update of 'i'. */
   for (i = 0, set = 0; set < c->sets; set++) {
      for (way = 0; way < c->assoc; way++, i++) {
         VG_(printf)("%8x ", c->tags[i]);
      }
      VG_(printf)("\n");
   }
}
#endif 


/*------------------------------------------------------------*/
/*--- Write Through Cache Simulation                       ---*/
/*------------------------------------------------------------*/

/*
 * Simple model: L1 & L2 Write Through
 * Does not distinguish among read and write references
 *
 * Simulator functions:
 *  CacheModelResult cachesim_I1_ref(Addr a, UChar size)
 *  CacheModelResult cachesim_D1_ref(Addr a, UChar size)
 */

static __inline__
CacheResult cachesim_setref(cache_t2* c, UInt set_no, UWord tag)
{
    int i, j;
    UWord *set;

    set = &(c->tags[set_no * c->assoc]);

    /* This loop is unrolled for just the first case, which is the most */
    /* common.  We can't unroll any further because it would screw up   */
    /* if we have a direct-mapped (1-way) cache.                        */
    if (tag == set[0])
        return Hit;

    /* If the tag is one other than the MRU, move it into the MRU spot  */
    /* and shuffle the rest down.                                       */
    for (i = 1; i < c->assoc; i++) {
        if (tag == set[i]) {
            for (j = i; j > 0; j--) {
                set[j] = set[j - 1];
            }
            set[0] = tag;
            return Hit;
        }
    }

    /* A miss;  install this tag as MRU, shuffle rest down. */
    for (j = c->assoc - 1; j > 0; j--) {
        set[j] = set[j - 1];
    }
    set[0] = tag;

    return Miss;
}

static CacheResult cachesim_ref(cache_t2* c, Addr a, UChar size)
{
    UInt  set1 = ( a         >> c->line_size_bits) & (c->sets_min_1);
    UInt  set2 = ((a+size-1) >> c->line_size_bits) & (c->sets_min_1);
    UWord tag  = a >> c->tag_shift;

    /* Access entirely within line. */
    if (set1 == set2) 
	return cachesim_setref(c, set1, tag);

    /* Access straddles two lines. */
    /* Nb: this is a fast way of doing ((set1+1) % c->sets) */
    else if (((set1 + 1) & (c->sets-1)) == set2) {
	UWord tag2  = (a+size-1) >> c->tag_shift;

	/* the call updates cache structures as side effect */
	CacheResult res1 =  cachesim_setref(c, set1, tag);
	CacheResult res2 =  cachesim_setref(c, set2, tag2);
	return ((res1 == Miss) || (res2 == Miss)) ? Miss : Hit;

   } else {
       VG_(printf)("addr: %lx  size: %u  sets: %d %d", a, size, set1, set2);
       VG_(tool_panic)("item straddles more than two cache sets");
   }
   return Hit;
}

static
CacheModelResult cachesim_I1_ref(Addr a, UChar size)
{
    if ( cachesim_ref( &I1, a, size) == Hit ) return L1_Hit;
    if ( cachesim_ref( &L2, a, size) == Hit ) return L2_Hit;
    return MemAccess;
}

static
CacheModelResult cachesim_D1_ref(Addr a, UChar size)
{
    if ( cachesim_ref( &D1, a, size) == Hit ) return L1_Hit;
    if ( cachesim_ref( &L2, a, size) == Hit ) return L2_Hit;
    return MemAccess;
}


/*------------------------------------------------------------*/
/*--- Write Back Cache Simulation                          ---*/
/*------------------------------------------------------------*/

/*
 * More complex model: L1 Write-through, L2 Write-back
 * This needs to distinguish among read and write references.
 *
 * Simulator functions:
 *  CacheModelResult cachesim_I1_Read(Addr a, UChar size)
 *  CacheModelResult cachesim_D1_Read(Addr a, UChar size)
 *  CacheModelResult cachesim_D1_Write(Addr a, UChar size)
 */

/*
 * With write-back, result can be a miss evicting a dirty line
 * The dirty state of a cache line is stored in Bit0 of the tag for
 * this cache line (CACHELINE_DIRTY = 1). By OR'ing the reference
 * type (Read/Write), the line gets dirty on a write.
 */
static __inline__
CacheResult cachesim_setref_wb(cache_t2* c, RefType ref, UInt set_no, UWord tag)
{
    int i, j;
    UWord *set, tmp_tag;

    set = &(c->tags[set_no * c->assoc]);

    /* This loop is unrolled for just the first case, which is the most */
    /* common.  We can't unroll any further because it would screw up   */
    /* if we have a direct-mapped (1-way) cache.                        */
    if (tag == (set[0] & ~CACHELINE_DIRTY)) {
	set[0] |= ref;
        return Hit;
    }
    /* If the tag is one other than the MRU, move it into the MRU spot  */
    /* and shuffle the rest down.                                       */
    for (i = 1; i < c->assoc; i++) {
	if (tag == (set[i] & ~CACHELINE_DIRTY)) {
	    tmp_tag = set[i] | ref; // update dirty flag
            for (j = i; j > 0; j--) {
                set[j] = set[j - 1];
            }
            set[0] = tmp_tag;
            return Hit;
        }
    }

    /* A miss;  install this tag as MRU, shuffle rest down. */
    tmp_tag = set[c->assoc - 1];
    for (j = c->assoc - 1; j > 0; j--) {
        set[j] = set[j - 1];
    }
    set[0] = tag | ref;

    return (tmp_tag & CACHELINE_DIRTY) ? MissDirty : Miss;
}


static __inline__
CacheResult cachesim_ref_wb(cache_t2* c, RefType ref, Addr a, UChar size)
{
    UInt set1 = ( a         >> c->line_size_bits) & (c->sets_min_1);
    UInt set2 = ((a+size-1) >> c->line_size_bits) & (c->sets_min_1);
    UWord tag = a & c->tag_mask;

    /* Access entirely within line. */
    if (set1 == set2)
	return cachesim_setref_wb(c, ref, set1, tag);

    /* Access straddles two lines. */
    /* Nb: this is a fast way of doing ((set1+1) % c->sets) */
    else if (((set1 + 1) & (c->sets-1)) == set2) {
	UWord tag2  = (a+size-1) & c->tag_mask;

	/* the call updates cache structures as side effect */
	CacheResult res1 =  cachesim_setref_wb(c, ref, set1, tag);
	CacheResult res2 =  cachesim_setref_wb(c, ref, set2, tag2);

	if ((res1 == MissDirty) || (res2 == MissDirty)) return MissDirty;
	return ((res1 == Miss) || (res2 == Miss)) ? Miss : Hit;

   } else {
       VG_(printf)("addr: %lx  size: %u  sets: %d %d", a, size, set1, set2);
       VG_(tool_panic)("item straddles more than two cache sets");
   }
   return Hit;
}


static
CacheModelResult cachesim_I1_Read(Addr a, UChar size)
{
    if ( cachesim_ref( &I1, a, size) == Hit ) return L1_Hit;
    switch( cachesim_ref_wb( &L2, Read, a, size) ) {
	case Hit: return L2_Hit;
	case Miss: return MemAccess;
	default: break;
    }
    return WriteBackMemAccess;
}

static
CacheModelResult cachesim_D1_Read(Addr a, UChar size)
{
    if ( cachesim_ref( &D1, a, size) == Hit ) return L1_Hit;
    switch( cachesim_ref_wb( &L2, Read, a, size) ) {
	case Hit: return L2_Hit;
	case Miss: return MemAccess;
	default: break;
    }
    return WriteBackMemAccess;
}

static
CacheModelResult cachesim_D1_Write(Addr a, UChar size)
{
    if ( cachesim_ref( &D1, a, size) == Hit ) {
	/* Even for a L1 hit, the write-trough L1 passes
	 * the write to the L2 to make the L2 line dirty.
	 * But this causes no latency, so return the hit.
	 */
	cachesim_ref_wb( &L2, Write, a, size);
	return L1_Hit;
    }
    switch( cachesim_ref_wb( &L2, Write, a, size) ) {
	case Hit: return L2_Hit;
	case Miss: return MemAccess;
	default: break;
    }
    return WriteBackMemAccess;
}


/*------------------------------------------------------------*/
/*--- Hardware Prefetch Simulation                         ---*/
/*------------------------------------------------------------*/

static ULong prefetch_up = 0;
static ULong prefetch_down = 0;

#define PF_STREAMS  8
#define PF_PAGEBITS 12

static UInt pf_lastblock[PF_STREAMS];
static Int  pf_seqblocks[PF_STREAMS];

static
void prefetch_clear(void)
{
  int i;
  for(i=0;i<PF_STREAMS;i++)
    pf_lastblock[i] = pf_seqblocks[i] = 0;
}

/*
 * HW Prefetch emulation
 * Start prefetching when detecting sequential access to 3 memory blocks.
 * One stream can be detected per 4k page.
 */
static __inline__
void prefetch_L2_doref(Addr a)
{
  UInt stream = (a >> PF_PAGEBITS) % PF_STREAMS;
  UInt block = ( a >> L2.line_size_bits);

  if (block != pf_lastblock[stream]) {
    if (pf_seqblocks[stream] == 0) {
      if (pf_lastblock[stream] +1 == block) pf_seqblocks[stream]++;
      else if (pf_lastblock[stream] -1 == block) pf_seqblocks[stream]--;
    }
    else if (pf_seqblocks[stream] >0) {
      if (pf_lastblock[stream] +1 == block) {
	pf_seqblocks[stream]++;
	if (pf_seqblocks[stream] >= 2) {
	  prefetch_up++;
	  cachesim_ref(&L2, a + 5 * L2.line_size,1);
	}
      }
      else pf_seqblocks[stream] = 0;
    }
    else if (pf_seqblocks[stream] <0) {
      if (pf_lastblock[stream] -1 == block) {
	pf_seqblocks[stream]--;
	if (pf_seqblocks[stream] <= -2) {
	  prefetch_down++;
	  cachesim_ref(&L2, a - 5 * L2.line_size,1);
	}
      }
      else pf_seqblocks[stream] = 0;
    }
    pf_lastblock[stream] = block;
  }
}  

/* simple model with hardware prefetch */

static
CacheModelResult prefetch_I1_ref(Addr a, UChar size)
{
    if ( cachesim_ref( &I1, a, size) == Hit ) return L1_Hit;
    prefetch_L2_doref(a);
    if ( cachesim_ref( &L2, a, size) == Hit ) return L2_Hit;
    return MemAccess;
}

static
CacheModelResult prefetch_D1_ref(Addr a, UChar size)
{
    if ( cachesim_ref( &D1, a, size) == Hit ) return L1_Hit;
    prefetch_L2_doref(a);
    if ( cachesim_ref( &L2, a, size) == Hit ) return L2_Hit;
    return MemAccess;
}


/* complex model with hardware prefetch */

static
CacheModelResult prefetch_I1_Read(Addr a, UChar size)
{
    if ( cachesim_ref( &I1, a, size) == Hit ) return L1_Hit;
    prefetch_L2_doref(a);
    switch( cachesim_ref_wb( &L2, Read, a, size) ) {
	case Hit: return L2_Hit;
	case Miss: return MemAccess;
	default: break;
    }
    return WriteBackMemAccess;
}

static
CacheModelResult prefetch_D1_Read(Addr a, UChar size)
{
    if ( cachesim_ref( &D1, a, size) == Hit ) return L1_Hit;
    prefetch_L2_doref(a);
    switch( cachesim_ref_wb( &L2, Read, a, size) ) {
	case Hit: return L2_Hit;
	case Miss: return MemAccess;
	default: break;
    }
    return WriteBackMemAccess;
}

static
CacheModelResult prefetch_D1_Write(Addr a, UChar size)
{
    prefetch_L2_doref(a);
    if ( cachesim_ref( &D1, a, size) == Hit ) {
	/* Even for a L1 hit, the write-trough L1 passes
	 * the write to the L2 to make the L2 line dirty.
	 * But this causes no latency, so return the hit.
	 */
	cachesim_ref_wb( &L2, Write, a, size);
	return L1_Hit;
    }
    switch( cachesim_ref_wb( &L2, Write, a, size) ) {
	case Hit: return L2_Hit;
	case Miss: return MemAccess;
	default: break;
    }
    return WriteBackMemAccess;
}


/*------------------------------------------------------------*/
/*--- Cache Simulation with use metric collection          ---*/
/*------------------------------------------------------------*/

/* can not be combined with write-back or prefetch */

static
void cacheuse_initcache(cache_t2* c)
{
    int i;
    unsigned int start_mask, start_val;
    unsigned int end_mask, end_val;

    c->use    = CLG_MALLOC("cl.sim.cu_ic.1",
                           sizeof(line_use) * c->sets * c->assoc);
    c->loaded = CLG_MALLOC("cl.sim.cu_ic.2",
                           sizeof(line_loaded) * c->sets * c->assoc);
    c->line_start_mask = CLG_MALLOC("cl.sim.cu_ic.3",
                                    sizeof(int) * c->line_size);
    c->line_end_mask = CLG_MALLOC("cl.sim.cu_ic.4",
                                  sizeof(int) * c->line_size);
    
    c->line_size_mask = c->line_size-1;

    /* Meaning of line_start_mask/line_end_mask
     * Example: for a given cache line, you get an access starting at
     * byte offset 5, length 4, byte 5 - 8 was touched. For a cache
     * line size of 32, you have 1 bit per byte in the mask:
     *
     *   bit31   bit8 bit5  bit 0
     *       |      |  |    |
     *       11..111111100000   line_start_mask[5]
     *       00..000111111111   line_end_mask[(5+4)-1]
     *
     *  use_mask |= line_start_mask[5] && line_end_mask[8]
     *
     */
    start_val = end_val = ~0;
    if (c->line_size < 32) {
	int bits_per_byte = 32/c->line_size;
	start_mask = (1<<bits_per_byte)-1;
	end_mask   = start_mask << (32-bits_per_byte);
	for(i=0;i<c->line_size;i++) {
	    c->line_start_mask[i] = start_val;
	    start_val  = start_val & ~start_mask;
	    start_mask = start_mask << bits_per_byte;
	    
	    c->line_end_mask[c->line_size-i-1] = end_val;
	    end_val  = end_val & ~end_mask;
	    end_mask = end_mask >> bits_per_byte;
	}
    }
    else {
	int bytes_per_bit = c->line_size/32;
	start_mask = 1;
	end_mask   = 1 << 31;
	for(i=0;i<c->line_size;i++) {
	    c->line_start_mask[i] = start_val;
	    c->line_end_mask[c->line_size-i-1] = end_val;
	    if ( ((i+1)%bytes_per_bit) == 0) {
		start_val   &= ~start_mask;
		end_val     &= ~end_mask;
		start_mask <<= 1;
		end_mask   >>= 1;
	    }
	}
    }
    
    CLG_DEBUG(6, "Config %s:\n", c->desc_line);
    for(i=0;i<c->line_size;i++) {
	CLG_DEBUG(6, " [%2d]: start mask %8x, end mask %8x\n",
		  i, c->line_start_mask[i], c->line_end_mask[i]);
    }
    
    /* We use lower tag bits as offset pointers to cache use info.
     * I.e. some cache parameters don't work.
     */
    if ( (1<<c->tag_shift) < c->assoc) {
	VG_(message)(Vg_DebugMsg,
		     "error: Use associativity < %d for cache use statistics!\n",
		     (1<<c->tag_shift) );
	VG_(tool_panic)("Unsupported cache configuration");
    }
}
    

/* for I1/D1 caches */
#define CACHEUSE(L)                                                         \
                                                                            \
static CacheModelResult cacheuse##_##L##_doRead(Addr a, UChar size)         \
{                                                                           \
   UInt set1 = ( a         >> L.line_size_bits) & (L.sets_min_1);           \
   UInt set2 = ((a+size-1) >> L.line_size_bits) & (L.sets_min_1);           \
   UWord tag  = a & L.tag_mask;                                             \
   UWord tag2;                                                              \
   int i, j, idx;                                                           \
   UWord *set, tmp_tag; 						    \
   UInt use_mask;							    \
                                                                            \
   CLG_DEBUG(6,"%s.Acc(Addr %#lx, size %d): Sets [%d/%d]\n",                  \
	    L.name, a, size, set1, set2);				    \
                                                                            \
   /* First case: word entirely within line. */                             \
   if (set1 == set2) {                                                      \
                                                                            \
      set = &(L.tags[set1 * L.assoc]);                                      \
      use_mask = L.line_start_mask[a & L.line_size_mask] &                  \
	         L.line_end_mask[(a+size-1) & L.line_size_mask];	    \
                                                                            \
      /* This loop is unrolled for just the first case, which is the most */\
      /* common.  We can't unroll any further because it would screw up   */\
      /* if we have a direct-mapped (1-way) cache.                        */\
      if (tag == (set[0] & L.tag_mask)) {                                   \
        idx = (set1 * L.assoc) + (set[0] & ~L.tag_mask);                    \
        L.use[idx].count ++;                                                \
        L.use[idx].mask |= use_mask;                                        \
	CLG_DEBUG(6," Hit0 [idx %d] (line %#lx from %#lx): %x => %08x, count %d\n",\
		 idx, L.loaded[idx].memline,  L.loaded[idx].iaddr,          \
		 use_mask, L.use[idx].mask, L.use[idx].count);              \
	return L1_Hit;							    \
      }                                                                     \
      /* If the tag is one other than the MRU, move it into the MRU spot  */\
      /* and shuffle the rest down.                                       */\
      for (i = 1; i < L.assoc; i++) {                                       \
	 if (tag == (set[i] & L.tag_mask)) {			            \
  	    tmp_tag = set[i];                                               \
            for (j = i; j > 0; j--) {                                       \
               set[j] = set[j - 1];                                         \
            }                                                               \
            set[0] = tmp_tag;			                            \
            idx = (set1 * L.assoc) + (tmp_tag & ~L.tag_mask);               \
            L.use[idx].count ++;                                            \
            L.use[idx].mask |= use_mask;                                    \
	CLG_DEBUG(6," Hit%d [idx %d] (line %#lx from %#lx): %x => %08x, count %d\n",\
		 i, idx, L.loaded[idx].memline,  L.loaded[idx].iaddr,       \
		 use_mask, L.use[idx].mask, L.use[idx].count);              \
            return L1_Hit;                                                  \
         }                                                                  \
      }                                                                     \
                                                                            \
      /* A miss;  install this tag as MRU, shuffle rest down. */            \
      tmp_tag = set[L.assoc - 1] & ~L.tag_mask;                             \
      for (j = L.assoc - 1; j > 0; j--) {                                   \
         set[j] = set[j - 1];                                               \
      }                                                                     \
      set[0] = tag | tmp_tag;                                               \
      idx = (set1 * L.assoc) + tmp_tag;                                     \
      return update_##L##_use(&L, idx,         			            \
		       use_mask, a &~ L.line_size_mask);		    \
                                                                            \
   /* Second case: word straddles two lines. */                             \
   /* Nb: this is a fast way of doing ((set1+1) % L.sets) */                \
   } else if (((set1 + 1) & (L.sets-1)) == set2) {                          \
      Int miss1=0, miss2=0; /* 0: L1 hit, 1:L1 miss, 2:L2 miss */           \
      set = &(L.tags[set1 * L.assoc]);                                      \
      use_mask = L.line_start_mask[a & L.line_size_mask];		    \
      if (tag == (set[0] & L.tag_mask)) {                                   \
         idx = (set1 * L.assoc) + (set[0] & ~L.tag_mask);                   \
         L.use[idx].count ++;                                               \
         L.use[idx].mask |= use_mask;                                       \
	CLG_DEBUG(6," Hit0 [idx %d] (line %#lx from %#lx): %x => %08x, count %d\n",\
		 idx, L.loaded[idx].memline,  L.loaded[idx].iaddr,          \
		 use_mask, L.use[idx].mask, L.use[idx].count);              \
         goto block2;                                                       \
      }                                                                     \
      for (i = 1; i < L.assoc; i++) {                                       \
	 if (tag == (set[i] & L.tag_mask)) {			            \
  	    tmp_tag = set[i];                                               \
            for (j = i; j > 0; j--) {                                       \
               set[j] = set[j - 1];                                         \
            }                                                               \
            set[0] = tmp_tag;                                               \
            idx = (set1 * L.assoc) + (tmp_tag & ~L.tag_mask);               \
            L.use[idx].count ++;                                            \
            L.use[idx].mask |= use_mask;                                    \
	CLG_DEBUG(6," Hit%d [idx %d] (line %#lx from %#lx): %x => %08x, count %d\n",\
		 i, idx, L.loaded[idx].memline,  L.loaded[idx].iaddr,       \
		 use_mask, L.use[idx].mask, L.use[idx].count);              \
            goto block2;                                                    \
         }                                                                  \
      }                                                                     \
      tmp_tag = set[L.assoc - 1] & ~L.tag_mask;                             \
      for (j = L.assoc - 1; j > 0; j--) {                                   \
         set[j] = set[j - 1];                                               \
      }                                                                     \
      set[0] = tag | tmp_tag;                                               \
      idx = (set1 * L.assoc) + tmp_tag;                                     \
      miss1 = update_##L##_use(&L, idx,        			            \
		       use_mask, a &~ L.line_size_mask);		    \
block2:                                                                     \
      set = &(L.tags[set2 * L.assoc]);                                      \
      use_mask = L.line_end_mask[(a+size-1) & L.line_size_mask];  	    \
      tag2  = (a+size-1) & L.tag_mask;                                      \
      if (tag2 == (set[0] & L.tag_mask)) {                                  \
         idx = (set2 * L.assoc) + (set[0] & ~L.tag_mask);                   \
         L.use[idx].count ++;                                               \
         L.use[idx].mask |= use_mask;                                       \
	CLG_DEBUG(6," Hit0 [idx %d] (line %#lx from %#lx): %x => %08x, count %d\n",\
		 idx, L.loaded[idx].memline,  L.loaded[idx].iaddr,          \
		 use_mask, L.use[idx].mask, L.use[idx].count);              \
         return miss1;                                                      \
      }                                                                     \
      for (i = 1; i < L.assoc; i++) {                                       \
	 if (tag2 == (set[i] & L.tag_mask)) {			            \
  	    tmp_tag = set[i];                                               \
            for (j = i; j > 0; j--) {                                       \
               set[j] = set[j - 1];                                         \
            }                                                               \
            set[0] = tmp_tag;                                               \
            idx = (set2 * L.assoc) + (tmp_tag & ~L.tag_mask);               \
            L.use[idx].count ++;                                            \
            L.use[idx].mask |= use_mask;                                    \
	CLG_DEBUG(6," Hit%d [idx %d] (line %#lx from %#lx): %x => %08x, count %d\n",\
		 i, idx, L.loaded[idx].memline,  L.loaded[idx].iaddr,       \
		 use_mask, L.use[idx].mask, L.use[idx].count);              \
            return miss1;                                                   \
         }                                                                  \
      }                                                                     \
      tmp_tag = set[L.assoc - 1] & ~L.tag_mask;                             \
      for (j = L.assoc - 1; j > 0; j--) {                                   \
         set[j] = set[j - 1];                                               \
      }                                                                     \
      set[0] = tag2 | tmp_tag;                                              \
      idx = (set2 * L.assoc) + tmp_tag;                                     \
      miss2 = update_##L##_use(&L, idx,			                    \
		       use_mask, (a+size-1) &~ L.line_size_mask);	    \
      return (miss1==MemAccess || miss2==MemAccess) ? MemAccess:L2_Hit;     \
                                                                            \
   } else {                                                                 \
       VG_(printf)("addr: %#lx  size: %u  sets: %d %d", a, size, set1, set2); \
       VG_(tool_panic)("item straddles more than two cache sets");          \
   }                                                                        \
   return 0;                                                                \
}


/* logarithmic bitcounting algorithm, see
 * http://graphics.stanford.edu/~seander/bithacks.html
 */
static __inline__ unsigned int countBits(unsigned int bits)
{
  unsigned int c; // store the total here
  const int S[] = {1, 2, 4, 8, 16}; // Magic Binary Numbers
  const int B[] = {0x55555555, 0x33333333, 0x0F0F0F0F, 0x00FF00FF, 0x0000FFFF};

  c = bits;
  c = ((c >> S[0]) & B[0]) + (c & B[0]);
  c = ((c >> S[1]) & B[1]) + (c & B[1]);
  c = ((c >> S[2]) & B[2]) + (c & B[2]);
  c = ((c >> S[3]) & B[3]) + (c & B[3]);
  c = ((c >> S[4]) & B[4]) + (c & B[4]);
  return c;
}

static void update_L2_use(int idx, Addr memline)
{
  line_loaded* loaded = &(L2.loaded[idx]);
  line_use* use = &(L2.use[idx]);
  int i = ((32 - countBits(use->mask)) * L2.line_size)>>5;
  
  CLG_DEBUG(2, " L2.miss [%d]: at %#lx accessing memline %#lx\n",
	   idx, bb_base + current_ii->instr_offset, memline);
  if (use->count>0) {
    CLG_DEBUG(2, "   old: used %d, loss bits %d (%08x) [line %#lx from %#lx]\n",
	     use->count, i, use->mask, loaded->memline, loaded->iaddr);
    CLG_DEBUG(2, "   collect: %d, use_base %p\n",
	     CLG_(current_state).collect, loaded->use_base);
    
    if (CLG_(current_state).collect && loaded->use_base) {
      (loaded->use_base)[off_L2_AcCost] += 1000 / use->count;
      (loaded->use_base)[off_L2_SpLoss] += i;
    }
   }

   use->count = 0;
   use->mask  = 0;

  loaded->memline = memline;
  loaded->iaddr   = bb_base + current_ii->instr_offset;
  loaded->use_base = (CLG_(current_state).nonskipped) ?
    CLG_(current_state).nonskipped->skipped :
    cost_base + current_ii->cost_offset;
}

static
CacheModelResult cacheuse_L2_access(Addr memline, line_loaded* l1_loaded)
{
   UInt setNo = (memline >> L2.line_size_bits) & (L2.sets_min_1);
   UWord* set = &(L2.tags[setNo * L2.assoc]);
   UWord tag  = memline & L2.tag_mask;

   int i, j, idx;
   UWord tmp_tag;
   
   CLG_DEBUG(6,"L2.Acc(Memline %#lx): Set %d\n", memline, setNo);

   if (tag == (set[0] & L2.tag_mask)) {
     idx = (setNo * L2.assoc) + (set[0] & ~L2.tag_mask);
     l1_loaded->dep_use = &(L2.use[idx]);

     CLG_DEBUG(6," Hit0 [idx %d] (line %#lx from %#lx): => %08x, count %d\n",
		 idx, L2.loaded[idx].memline,  L2.loaded[idx].iaddr,
		 L2.use[idx].mask, L2.use[idx].count);
     return L2_Hit;
   }
   for (i = 1; i < L2.assoc; i++) {
     if (tag == (set[i] & L2.tag_mask)) {
       tmp_tag = set[i];
       for (j = i; j > 0; j--) {
	 set[j] = set[j - 1];
       }
       set[0] = tmp_tag;
       idx = (setNo * L2.assoc) + (tmp_tag & ~L2.tag_mask);
       l1_loaded->dep_use = &(L2.use[idx]);

	CLG_DEBUG(6," Hit%d [idx %d] (line %#lx from %#lx): => %08x, count %d\n",
		 i, idx, L2.loaded[idx].memline,  L2.loaded[idx].iaddr,
		 L2.use[idx].mask, L2.use[idx].count);
	return L2_Hit;
     }
   }

   /* A miss;  install this tag as MRU, shuffle rest down. */
   tmp_tag = set[L2.assoc - 1] & ~L2.tag_mask;
   for (j = L2.assoc - 1; j > 0; j--) {
     set[j] = set[j - 1];
   }
   set[0] = tag | tmp_tag;
   idx = (setNo * L2.assoc) + tmp_tag;
   l1_loaded->dep_use = &(L2.use[idx]);

   update_L2_use(idx, memline);

   return MemAccess;
}




#define UPDATE_USE(L)					             \
                                                                     \
static CacheModelResult update##_##L##_use(cache_t2* cache, int idx, \
			       UInt mask, Addr memline)		     \
{                                                                    \
  line_loaded* loaded = &(cache->loaded[idx]);			     \
  line_use* use = &(cache->use[idx]);				     \
  int c = ((32 - countBits(use->mask)) * cache->line_size)>>5;       \
                                                                     \
  CLG_DEBUG(2, " %s.miss [%d]: at %#lx accessing memline %#lx (mask %08x)\n", \
	   cache->name, idx, bb_base + current_ii->instr_offset, memline, mask); \
  if (use->count>0) {                                                \
    CLG_DEBUG(2, "   old: used %d, loss bits %d (%08x) [line %#lx from %#lx]\n",\
	     use->count, c, use->mask, loaded->memline, loaded->iaddr);	\
    CLG_DEBUG(2, "   collect: %d, use_base %p\n", \
	     CLG_(current_state).collect, loaded->use_base);	     \
                                                                     \
    if (CLG_(current_state).collect && loaded->use_base) {            \
      (loaded->use_base)[off_##L##_AcCost] += 1000 / use->count;     \
      (loaded->use_base)[off_##L##_SpLoss] += c;                     \
                                                                     \
      /* FIXME (?): L1/L2 line sizes must be equal ! */              \
      loaded->dep_use->mask |= use->mask;                            \
      loaded->dep_use->count += use->count;                          \
    }                                                                \
  }                                                                  \
                                                                     \
  use->count = 1;                                                    \
  use->mask  = mask;                                                 \
  loaded->memline = memline;                                         \
  loaded->iaddr   = bb_base + current_ii->instr_offset;              \
  loaded->use_base = (CLG_(current_state).nonskipped) ?               \
    CLG_(current_state).nonskipped->skipped :                         \
    cost_base + current_ii->cost_offset;		             \
                                                                     \
  if (memline == 0) return L2_Hit;                                   \
  return cacheuse_L2_access(memline, loaded);                        \
}

UPDATE_USE(I1);
UPDATE_USE(D1);

CACHEUSE(I1);
CACHEUSE(D1);


static
void cacheuse_finish(void)
{
  int i;
  InstrInfo ii = { 0,0,0,0 };

  if (!CLG_(current_state).collect) return;

  bb_base = 0;
  current_ii = &ii;
  cost_base = 0;

  /* update usage counters */
  if (I1.use)
    for (i = 0; i < I1.sets * I1.assoc; i++)
      if (I1.loaded[i].use_base)
	update_I1_use( &I1, i, 0,0);

  if (D1.use)
    for (i = 0; i < D1.sets * D1.assoc; i++)
      if (D1.loaded[i].use_base)
	update_D1_use( &D1, i, 0,0);

  if (L2.use)
    for (i = 0; i < L2.sets * L2.assoc; i++)
      if (L2.loaded[i].use_base)
	update_L2_use(i, 0);
}
  


/*------------------------------------------------------------*/
/*--- Helper functions called by instrumented code         ---*/
/*------------------------------------------------------------*/


static __inline__
void inc_costs(CacheModelResult r, ULong* c1, ULong* c2)
{
    switch(r) {
	case WriteBackMemAccess:
	    if (clo_simulate_writeback) {
		c1[3]++;
		c2[3]++;
	    }
	    // fall through

	case MemAccess:
	    c1[2]++;
	    c2[2]++;
	    // fall through

	case L2_Hit:
	    c1[1]++;
	    c2[1]++;
	    // fall through

	default:
	    c1[0]++;
	    c2[0]++;
    }
}

static
Char* cacheRes(CacheModelResult r)
{
    switch(r) {
    case L1_Hit:    return "L1 Hit ";
    case L2_Hit:    return "L2 Hit ";
    case MemAccess: return "L2 Miss";
    case WriteBackMemAccess: return "L2 Miss (dirty)";
    default:
	tl_assert(0);
    }
    return "??";
}

VG_REGPARM(1)
static void log_1I0D(InstrInfo* ii)
{
    CacheModelResult IrRes;

    current_ii = ii;
    IrRes = (*simulator.I1_Read)(bb_base + ii->instr_offset, ii->instr_size);

    CLG_DEBUG(6, "log_1I0D:  Ir  %#lx/%u => %s\n",
	      bb_base + ii->instr_offset, ii->instr_size, cacheRes(IrRes));

    if (CLG_(current_state).collect) {
	ULong* cost_Ir;

	if (CLG_(current_state).nonskipped)
	    cost_Ir = CLG_(current_state).nonskipped->skipped + CLG_(sets).off_full_Ir;
	else
	    cost_Ir = cost_base + ii->cost_offset + off_UIr_Ir;

	inc_costs(IrRes, cost_Ir, 
		  CLG_(current_state).cost + CLG_(sets).off_full_Ir );
    }
}

VG_REGPARM(2)
static void log_2I0D(InstrInfo* ii1, InstrInfo* ii2)
{
    CacheModelResult Ir1Res, Ir2Res;
    ULong *global_cost_Ir;

    current_ii = ii1;
    Ir1Res = (*simulator.I1_Read)(bb_base + ii1->instr_offset, ii1->instr_size);
    current_ii = ii2;
    Ir2Res = (*simulator.I1_Read)(bb_base + ii2->instr_offset, ii2->instr_size);

    CLG_DEBUG(6, "log_2I0D:  Ir1 %#lx/%u => %s, Ir2 %#lx/%u => %s\n",
	      bb_base + ii1->instr_offset, ii1->instr_size, cacheRes(Ir1Res),
	      bb_base + ii2->instr_offset, ii2->instr_size, cacheRes(Ir2Res) );

    if (!CLG_(current_state).collect) return;

    global_cost_Ir = CLG_(current_state).cost + CLG_(sets).off_full_Ir;
    if (CLG_(current_state).nonskipped) {
	ULong* skipped_cost_Ir = CLG_(current_state).nonskipped->skipped +
				 CLG_(sets).off_full_Ir;
	inc_costs(Ir1Res, global_cost_Ir, skipped_cost_Ir);
	inc_costs(Ir2Res, global_cost_Ir, skipped_cost_Ir);
	return;
    }

    inc_costs(Ir1Res, global_cost_Ir, cost_base + ii1->cost_offset + off_UIr_Ir);
    inc_costs(Ir2Res, global_cost_Ir, cost_base + ii2->cost_offset + off_UIr_Ir);
}

VG_REGPARM(3)
static void log_3I0D(InstrInfo* ii1, InstrInfo* ii2, InstrInfo* ii3)
{
    CacheModelResult Ir1Res, Ir2Res, Ir3Res;
    ULong *global_cost_Ir;

    current_ii = ii1;
    Ir1Res = (*simulator.I1_Read)(bb_base + ii1->instr_offset, ii1->instr_size);
    current_ii = ii2;
    Ir2Res = (*simulator.I1_Read)(bb_base + ii2->instr_offset, ii2->instr_size);
    current_ii = ii3;
    Ir3Res = (*simulator.I1_Read)(bb_base + ii3->instr_offset, ii3->instr_size);

    CLG_DEBUG(6, "log_3I0D:  Ir1 %#lx/%u => %s, Ir2 %#lx/%u => %s, Ir3 %#lx/%u => %s\n",
	      bb_base + ii1->instr_offset, ii1->instr_size, cacheRes(Ir1Res),
	      bb_base + ii2->instr_offset, ii2->instr_size, cacheRes(Ir2Res),
	      bb_base + ii3->instr_offset, ii3->instr_size, cacheRes(Ir3Res) );

    if (!CLG_(current_state).collect) return;

    global_cost_Ir = CLG_(current_state).cost + CLG_(sets).off_full_Ir;
    if (CLG_(current_state).nonskipped) {
	ULong* skipped_cost_Ir = CLG_(current_state).nonskipped->skipped +
				 CLG_(sets).off_full_Ir;
	inc_costs(Ir1Res, global_cost_Ir, skipped_cost_Ir);
	inc_costs(Ir2Res, global_cost_Ir, skipped_cost_Ir);
	inc_costs(Ir3Res, global_cost_Ir, skipped_cost_Ir);
	return;
    }

    inc_costs(Ir1Res, global_cost_Ir, cost_base + ii1->cost_offset + off_UIr_Ir);
    inc_costs(Ir2Res, global_cost_Ir, cost_base + ii2->cost_offset + off_UIr_Ir);
    inc_costs(Ir3Res, global_cost_Ir, cost_base + ii3->cost_offset + off_UIr_Ir);
}

/* Instruction doing a read access */

VG_REGPARM(3)
static void log_1I1Dr(InstrInfo* ii, Addr data_addr, Word data_size)
{
    CacheModelResult IrRes, DrRes;

    current_ii = ii;
    IrRes = (*simulator.I1_Read)(bb_base + ii->instr_offset, ii->instr_size);
    DrRes = (*simulator.D1_Read)(data_addr, data_size);

    CLG_DEBUG(6, "log_1I1Dr: Ir  %#lx/%u => %s, Dr  %#lx/%lu => %s\n",
	      bb_base + ii->instr_offset, ii->instr_size, cacheRes(IrRes),
	      data_addr, data_size, cacheRes(DrRes));

    if (CLG_(current_state).collect) {
	ULong *cost_Ir, *cost_Dr;
	
	if (CLG_(current_state).nonskipped) {
	    cost_Ir = CLG_(current_state).nonskipped->skipped + CLG_(sets).off_full_Ir;
	    cost_Dr = CLG_(current_state).nonskipped->skipped + CLG_(sets).off_full_Dr;
	}
	else {
	    // event set must be UIrDr or extension
	    CLG_ASSERT((ii->eventset == CLG_(sets).UIrDr) ||
		       (ii->eventset == CLG_(sets).UIrDrDw));
	    cost_Ir = cost_base + ii->cost_offset + off_UIrDr_Ir;
	    cost_Dr = cost_base + ii->cost_offset + off_UIrDr_Dr;
	}
       
	inc_costs(IrRes, cost_Ir, 
		  CLG_(current_state).cost + CLG_(sets).off_full_Ir );
	inc_costs(DrRes, cost_Dr,
		  CLG_(current_state).cost + CLG_(sets).off_full_Dr );
    }
}


VG_REGPARM(3)
static void log_0I1Dr(InstrInfo* ii, Addr data_addr, Word data_size)
{
    CacheModelResult DrRes;

    current_ii = ii;
    DrRes = (*simulator.D1_Read)(data_addr, data_size);

    CLG_DEBUG(6, "log_0I1Dr: Dr  %#lx/%lu => %s\n",
	      data_addr, data_size, cacheRes(DrRes));

    if (CLG_(current_state).collect) {
	ULong *cost_Dr;
	
	if (CLG_(current_state).nonskipped) {
	    cost_Dr = CLG_(current_state).nonskipped->skipped + CLG_(sets).off_full_Dr;
	}
	else {
	    Int off_Dr;
	    if      (ii->eventset == CLG_(sets).UIrDr)   off_Dr = off_UIrDr_Dr;
	    else if (ii->eventset == CLG_(sets).UIrDrDw) off_Dr = off_UIrDrDw_Dr;
	    else if (ii->eventset == CLG_(sets).UIrDwDr) off_Dr = off_UIrDwDr_Dr;
	    else CLG_ASSERT(0);

	    cost_Dr = cost_base + ii->cost_offset + off_Dr;
	}

	inc_costs(DrRes, cost_Dr,
		  CLG_(current_state).cost + CLG_(sets).off_full_Dr );
    }
}


/* Instruction doing a write access */

VG_REGPARM(3)
static void log_1I1Dw(InstrInfo* ii, Addr data_addr, Word data_size)
{
    CacheModelResult IrRes, DwRes;

    current_ii = ii;
    IrRes = (*simulator.I1_Read)(bb_base + ii->instr_offset, ii->instr_size);
    DwRes = (*simulator.D1_Write)(data_addr, data_size);

    CLG_DEBUG(6, "log_1I1Dw: Ir  %#lx/%u => %s, Dw  %#lx/%lu => %s\n",
	      bb_base + ii->instr_offset, ii->instr_size, cacheRes(IrRes),
	      data_addr, data_size, cacheRes(DwRes));

    if (CLG_(current_state).collect) {
	ULong *cost_Ir, *cost_Dw;
	
	if (CLG_(current_state).nonskipped) {
	    cost_Ir = CLG_(current_state).nonskipped->skipped + CLG_(sets).off_full_Ir;
	    cost_Dw = CLG_(current_state).nonskipped->skipped + CLG_(sets).off_full_Dw;
	}
	else {
	    // This helper is called when a Dr event follows Ir;
	    // Event set must be UIrDw or extension
	    CLG_ASSERT((ii->eventset == CLG_(sets).UIrDw) ||
		       (ii->eventset == CLG_(sets).UIrDwDr));
	    cost_Ir = cost_base + ii->cost_offset + off_UIrDw_Ir;
	    cost_Dw = cost_base + ii->cost_offset + off_UIrDw_Dw;
	}
       
	inc_costs(IrRes, cost_Ir,
		  CLG_(current_state).cost + CLG_(sets).off_full_Ir );
	inc_costs(DwRes, cost_Dw,
		  CLG_(current_state).cost + CLG_(sets).off_full_Dw );
    }
}

VG_REGPARM(3)
static void log_0I1Dw(InstrInfo* ii, Addr data_addr, Word data_size)
{
    CacheModelResult DwRes;

    current_ii = ii;
    DwRes = (*simulator.D1_Write)(data_addr, data_size);

    CLG_DEBUG(6, "log_0I1Dw: Dw  %#lx/%lu => %s\n",
	      data_addr, data_size, cacheRes(DwRes));

    if (CLG_(current_state).collect) {
	ULong *cost_Dw;
	
	if (CLG_(current_state).nonskipped) {
	    cost_Dw = CLG_(current_state).nonskipped->skipped + CLG_(sets).off_full_Dw;
	}
	else {
	    Int off_Dw;
	    if      (ii->eventset == CLG_(sets).UIrDw)   off_Dw = off_UIrDw_Dw;
	    else if (ii->eventset == CLG_(sets).UIrDwDr) off_Dw = off_UIrDwDr_Dw;
	    else if (ii->eventset == CLG_(sets).UIrDrDw) off_Dw = off_UIrDrDw_Dw;
	    else CLG_ASSERT(0);

	    cost_Dw = cost_base + ii->cost_offset + off_Dw;
	}
       
	inc_costs(DwRes, cost_Dw,
		  CLG_(current_state).cost + CLG_(sets).off_full_Dw );
    }
}



/*------------------------------------------------------------*/
/*--- Cache configuration                                  ---*/
/*------------------------------------------------------------*/

#define UNDEFINED_CACHE     ((cache_t) { -1, -1, -1 }) 

static cache_t clo_I1_cache = UNDEFINED_CACHE;
static cache_t clo_D1_cache = UNDEFINED_CACHE;
static cache_t clo_L2_cache = UNDEFINED_CACHE;


/* Checks cache config is ok;  makes it so if not. */
static
void check_cache(cache_t* cache, Char *name)
{
   /* Simulator requires line size and set count to be powers of two */
   if (( cache->size % (cache->line_size * cache->assoc) != 0) ||
       (-1 == VG_(log2)(cache->size/cache->line_size/cache->assoc))) {
      VG_(message)(Vg_UserMsg,
         "error: %s set count not a power of two; aborting.\n",
         name);
   }

   if (-1 == VG_(log2)(cache->line_size)) {
      VG_(message)(Vg_UserMsg,
         "error: %s line size of %dB not a power of two; aborting.\n",
         name, cache->line_size);
      VG_(exit)(1);
   }

   // Then check line size >= 16 -- any smaller and a single instruction could
   // straddle three cache lines, which breaks a simulation assertion and is
   // stupid anyway.
   if (cache->line_size < MIN_LINE_SIZE) {
      VG_(message)(Vg_UserMsg,
         "error: %s line size of %dB too small; aborting.\n",
         name, cache->line_size);
      VG_(exit)(1);
   }

   /* Then check cache size > line size (causes seg faults if not). */
   if (cache->size <= cache->line_size) {
      VG_(message)(Vg_UserMsg,
         "error: %s cache size of %dB <= line size of %dB; aborting.\n",
         name, cache->size, cache->line_size);
      VG_(exit)(1);
   }

   /* Then check assoc <= (size / line size) (seg faults otherwise). */
   if (cache->assoc > (cache->size / cache->line_size)) {
      VG_(message)(Vg_UserMsg,
         "warning: %s associativity > (size / line size); aborting.\n", name);
      VG_(exit)(1);
   }
}

static
void configure_caches(cache_t* I1c, cache_t* D1c, cache_t* L2c)
{
#define DEFINED(L)   (-1 != L.size  || -1 != L.assoc || -1 != L.line_size)

   Int n_clos = 0;

   // Count how many were defined on the command line.
   if (DEFINED(clo_I1_cache)) { n_clos++; }
   if (DEFINED(clo_D1_cache)) { n_clos++; }
   if (DEFINED(clo_L2_cache)) { n_clos++; }

   // Set the cache config (using auto-detection, if supported by the
   // architecture)
   VG_(configure_caches)( I1c, D1c, L2c, (3 == n_clos) );

   // Then replace with any defined on the command line.
   if (DEFINED(clo_I1_cache)) { *I1c = clo_I1_cache; }
   if (DEFINED(clo_D1_cache)) { *D1c = clo_D1_cache; }
   if (DEFINED(clo_L2_cache)) { *L2c = clo_L2_cache; }

   // Then check values and fix if not acceptable.
   check_cache(I1c, "I1");
   check_cache(D1c, "D1");
   check_cache(L2c, "L2");

   if (VG_(clo_verbosity) > 1) {
      VG_(message)(Vg_UserMsg, "Cache configuration used:\n");
      VG_(message)(Vg_UserMsg, "  I1: %dB, %d-way, %dB lines\n",
                               I1c->size, I1c->assoc, I1c->line_size);
      VG_(message)(Vg_UserMsg, "  D1: %dB, %d-way, %dB lines\n",
                               D1c->size, D1c->assoc, D1c->line_size);
      VG_(message)(Vg_UserMsg, "  L2: %dB, %d-way, %dB lines\n",
                               L2c->size, L2c->assoc, L2c->line_size);
   }
#undef CMD_LINE_DEFINED
}


/* Initialize and clear simulator state */
static void cachesim_post_clo_init(void)
{
  /* Cache configurations. */
  cache_t  I1c, D1c, L2c;

  /* Initialize access handlers */
  if (!CLG_(clo).simulate_cache) {
    CLG_(cachesim).log_1I0D  = 0;
    CLG_(cachesim).log_1I0D_name = "(no function)";
    CLG_(cachesim).log_2I0D  = 0;
    CLG_(cachesim).log_2I0D_name = "(no function)";
    CLG_(cachesim).log_3I0D  = 0;
    CLG_(cachesim).log_3I0D_name = "(no function)";

    CLG_(cachesim).log_1I1Dr = 0;
    CLG_(cachesim).log_1I1Dr_name = "(no function)";
    CLG_(cachesim).log_1I1Dw = 0;
    CLG_(cachesim).log_1I1Dw_name = "(no function)";

    CLG_(cachesim).log_0I1Dr = 0;
    CLG_(cachesim).log_0I1Dr_name = "(no function)";
    CLG_(cachesim).log_0I1Dw = 0;
    CLG_(cachesim).log_0I1Dw_name = "(no function)";
    return;
  }

  /* Configuration of caches only needed with real cache simulation */
  configure_caches(&I1c, &D1c, &L2c);
  
  I1.name = "I1";
  D1.name = "D1";
  L2.name = "L2";

  cachesim_initcache(I1c, &I1);
  cachesim_initcache(D1c, &D1);
  cachesim_initcache(L2c, &L2);

  /* the other cache simulators use the standard helpers
   * with dispatching via simulator struct */

  CLG_(cachesim).log_1I0D  = log_1I0D;
  CLG_(cachesim).log_1I0D_name  = "log_1I0D";
  CLG_(cachesim).log_2I0D  = log_2I0D;
  CLG_(cachesim).log_2I0D_name  = "log_2I0D";
  CLG_(cachesim).log_3I0D  = log_3I0D;
  CLG_(cachesim).log_3I0D_name  = "log_3I0D";

  CLG_(cachesim).log_1I1Dr = log_1I1Dr;
  CLG_(cachesim).log_1I1Dw = log_1I1Dw;
  CLG_(cachesim).log_1I1Dr_name = "log_1I1Dr";
  CLG_(cachesim).log_1I1Dw_name = "log_1I1Dw";

  CLG_(cachesim).log_0I1Dr = log_0I1Dr;
  CLG_(cachesim).log_0I1Dw = log_0I1Dw;
  CLG_(cachesim).log_0I1Dr_name = "log_0I1Dr";
  CLG_(cachesim).log_0I1Dw_name = "log_0I1Dw";

  if (clo_collect_cacheuse) {

      /* Output warning for not supported option combinations */
      if (clo_simulate_hwpref) {
	  VG_(message)(Vg_DebugMsg,
		       "warning: prefetch simulation can not be "
                       "used with cache usage\n");
	  clo_simulate_hwpref = False;
      }

      if (clo_simulate_writeback) {
	  VG_(message)(Vg_DebugMsg,
		       "warning: write-back simulation can not be "
                       "used with cache usage\n");
	  clo_simulate_writeback = False;
      }

      simulator.I1_Read  = cacheuse_I1_doRead;
      simulator.D1_Read  = cacheuse_D1_doRead;
      simulator.D1_Write = cacheuse_D1_doRead;
      return;
  }

  if (clo_simulate_hwpref) {
    prefetch_clear();

    if (clo_simulate_writeback) {
      simulator.I1_Read  = prefetch_I1_Read;
      simulator.D1_Read  = prefetch_D1_Read;
      simulator.D1_Write = prefetch_D1_Write;
    }
    else {
      simulator.I1_Read  = prefetch_I1_ref;
      simulator.D1_Read  = prefetch_D1_ref;
      simulator.D1_Write = prefetch_D1_ref;
    }

    return;
  }

  if (clo_simulate_writeback) {
      simulator.I1_Read  = cachesim_I1_Read;
      simulator.D1_Read  = cachesim_D1_Read;
      simulator.D1_Write = cachesim_D1_Write;
  }
  else {
      simulator.I1_Read  = cachesim_I1_ref;
      simulator.D1_Read  = cachesim_D1_ref;
      simulator.D1_Write = cachesim_D1_ref;
  }
}


/* Clear simulator state. Has to be initialized before */
static
void cachesim_clear(void)
{
  cachesim_clearcache(&I1);
  cachesim_clearcache(&D1);
  cachesim_clearcache(&L2);

  prefetch_clear();
}


static void cachesim_getdesc(Char* buf)
{
  Int p;
  p = VG_(sprintf)(buf, "\ndesc: I1 cache: %s\n", I1.desc_line);
  p += VG_(sprintf)(buf+p, "desc: D1 cache: %s\n", D1.desc_line);
  VG_(sprintf)(buf+p, "desc: L2 cache: %s\n", L2.desc_line);
}

static
void cachesim_print_opts(void)
{
  VG_(printf)(
"\n   cache simulator options:\n"
"    --simulate-cache=no|yes   Do cache simulation [no]\n"
"    --simulate-wb=no|yes      Count write-back events [no]\n"
"    --simulate-hwpref=no|yes  Simulate hardware prefetch [no]\n"
#if CLG_EXPERIMENTAL
"    --simulate-sectors=no|yes Simulate sectored behaviour [no]\n"
#endif
"    --cacheuse=no|yes         Collect cache block use [no]\n"
"    --I1=<size>,<assoc>,<line_size>  set I1 cache manually\n"
"    --D1=<size>,<assoc>,<line_size>  set D1 cache manually\n"
"    --L2=<size>,<assoc>,<line_size>  set L2 cache manually\n"
	      );
}

static void parse_opt ( cache_t* cache, char* opt )
{
   Long i1, i2, i3;
   Char* endptr;

   // Option argument looks like "65536,2,64".  Extract them.
   i1 = VG_(strtoll10)(opt,      &endptr); if (*endptr != ',')  goto bad;
   i2 = VG_(strtoll10)(endptr+1, &endptr); if (*endptr != ',')  goto bad;
   i3 = VG_(strtoll10)(endptr+1, &endptr); if (*endptr != '\0') goto bad;

   // Check for overflow.
   cache->size      = (Int)i1;
   cache->assoc     = (Int)i2;
   cache->line_size = (Int)i3;
   if (cache->size      != i1) goto overflow;
   if (cache->assoc     != i2) goto overflow;
   if (cache->line_size != i3) goto overflow;

   return;

  overflow:
   VG_(message)(Vg_UserMsg,
                "one of the cache parameters was too large and overflowed\n");
  bad:
   // XXX: this omits the "--I1/D1/L2=" part from the message, but that's
   // not a big deal.
   VG_(err_bad_option)(opt);
}

/* Check for command line option for cache configuration.
 * Return False if unknown and not handled.
 *
 * Called from CLG_(process_cmd_line_option)() in clo.c
 */
static Bool cachesim_parse_opt(Char* arg)
{
   Char* tmp_str;

   if      VG_BOOL_CLO(arg, "--simulate-wb",      clo_simulate_writeback) {}
   else if VG_BOOL_CLO(arg, "--simulate-hwpref",  clo_simulate_hwpref)    {}
   else if VG_BOOL_CLO(arg, "--simulate-sectors", clo_simulate_sectors)   {}

   else if VG_BOOL_CLO(arg, "--cacheuse", clo_collect_cacheuse) {
      if (clo_collect_cacheuse) {
         /* Use counters only make sense with fine dumping */
         CLG_(clo).dump_instr = True;
      }
   }

   else if VG_STR_CLO(arg, "--I1", tmp_str)
      parse_opt(&clo_I1_cache, tmp_str);
   else if VG_STR_CLO(arg, "--D1", tmp_str)
      parse_opt(&clo_D1_cache, tmp_str);
   else if VG_STR_CLO(arg, "--L2", tmp_str)
      parse_opt(&clo_L2_cache, tmp_str);
  else
    return False;

  return True;
}

/* Adds commas to ULong, right justifying in a field field_width wide, returns
 * the string in buf. */
static
Int commify(ULong n, int field_width, char* buf)
{
   int len, n_commas, i, j, new_len, space;

   VG_(sprintf)(buf, "%llu", n);
   len = VG_(strlen)(buf);
   n_commas = (len - 1) / 3;
   new_len = len + n_commas;
   space = field_width - new_len;

   /* Allow for printing a number in a field_width smaller than it's size */
   if (space < 0) space = 0;    

   /* Make j = -1 because we copy the '\0' before doing the numbers in groups
    * of three. */
   for (j = -1, i = len ; i >= 0; i--) {
      buf[i + n_commas + space] = buf[i];

      if ((i>0) && (3 == ++j)) {
         j = 0;
         n_commas--;
         buf[i + n_commas + space] = ',';
      }
   }
   /* Right justify in field. */
   for (i = 0; i < space; i++)  buf[i] = ' ';
   return new_len;
}

static
void percentify(Int n, Int ex, Int field_width, char buf[]) 
{
   int i, len, space;
    
   VG_(sprintf)(buf, "%d.%d%%", n / ex, n % ex);
   len = VG_(strlen)(buf);
   space = field_width - len;
   if (space < 0) space = 0;     /* Allow for v. small field_width */
   i = len;

   /* Right justify in field */
   for (     ; i >= 0;    i--)  buf[i + space] = buf[i];
   for (i = 0; i < space; i++)  buf[i] = ' ';
}

static
void cachesim_printstat(void)
{
  FullCost total = CLG_(total_cost), D_total = 0;
  ULong L2_total_m, L2_total_mr, L2_total_mw,
    L2_total, L2_total_r, L2_total_w;
  char buf1[RESULTS_BUF_LEN], 
    buf2[RESULTS_BUF_LEN], 
    buf3[RESULTS_BUF_LEN];
  Int l1, l2, l3;
  Int p;

  if ((VG_(clo_verbosity) >1) && clo_simulate_hwpref) {
    VG_(message)(Vg_DebugMsg, "Prefetch Up:       %llu\n", 
		 prefetch_up);
    VG_(message)(Vg_DebugMsg, "Prefetch Down:     %llu\n", 
		 prefetch_down);
    VG_(message)(Vg_DebugMsg, "\n");
  }

  /* I cache results.  Use the I_refs value to determine the first column
   * width. */
  l1 = commify(total[CLG_(sets).off_full_Ir], 0, buf1);
  VG_(message)(Vg_UserMsg, "I   refs:      %s\n", buf1);

  if (!CLG_(clo).simulate_cache) return;

  commify(total[CLG_(sets).off_full_Ir +1], l1, buf1);
  VG_(message)(Vg_UserMsg, "I1  misses:    %s\n", buf1);

  commify(total[CLG_(sets).off_full_Ir +2], l1, buf1);
  VG_(message)(Vg_UserMsg, "L2i misses:    %s\n", buf1);

  p = 100;

  if (0 == total[CLG_(sets).off_full_Ir]) 
    total[CLG_(sets).off_full_Ir] = 1;

  percentify(total[CLG_(sets).off_full_Ir+1] * 100 * p /
	     total[CLG_(sets).off_full_Ir], p, l1+1, buf1);
  VG_(message)(Vg_UserMsg, "I1  miss rate: %s\n", buf1);
       
  percentify(total[CLG_(sets).off_full_Ir+2] * 100 * p /
	     total[CLG_(sets).off_full_Ir], p, l1+1, buf1);
  VG_(message)(Vg_UserMsg, "L2i miss rate: %s\n", buf1);
  VG_(message)(Vg_UserMsg, "\n");
   
  /* D cache results.
     Use the D_refs.rd and D_refs.wr values to determine the
   * width of columns 2 & 3. */

  D_total = CLG_(get_eventset_cost)( CLG_(sets).full );
  CLG_(init_cost)( CLG_(sets).full, D_total);
  CLG_(copy_cost)( CLG_(sets).Dr, D_total, total + CLG_(sets).off_full_Dr );
  CLG_(add_cost) ( CLG_(sets).Dw, D_total, total + CLG_(sets).off_full_Dw );

  commify( D_total[0], l1, buf1);
  l2 = commify(total[CLG_(sets).off_full_Dr], 0,  buf2);
  l3 = commify(total[CLG_(sets).off_full_Dw], 0,  buf3);
  VG_(message)(Vg_UserMsg, "D   refs:      %s  (%s rd + %s wr)\n",
	       buf1,  buf2,  buf3);

  commify( D_total[1], l1, buf1);
  commify(total[CLG_(sets).off_full_Dr+1], l2, buf2);
  commify(total[CLG_(sets).off_full_Dw+1], l3, buf3);
  VG_(message)(Vg_UserMsg, "D1  misses:    %s  (%s rd + %s wr)\n",
	       buf1, buf2, buf3);

  commify( D_total[2], l1, buf1);
  commify(total[CLG_(sets).off_full_Dr+2], l2, buf2);
  commify(total[CLG_(sets).off_full_Dw+2], l3, buf3);
  VG_(message)(Vg_UserMsg, "L2d misses:    %s  (%s rd + %s wr)\n",
	       buf1, buf2, buf3);

  p = 10;
  
  if (0 == D_total[0])   D_total[0] = 1;
  if (0 == total[CLG_(sets).off_full_Dr]) total[CLG_(sets).off_full_Dr] = 1;
  if (0 == total[CLG_(sets).off_full_Dw]) total[CLG_(sets).off_full_Dw] = 1;
  
  percentify( D_total[1] * 100 * p / D_total[0],  p, l1+1, buf1);
  percentify(total[CLG_(sets).off_full_Dr+1] * 100 * p /
	     total[CLG_(sets).off_full_Dr], p, l2+1, buf2);
  percentify(total[CLG_(sets).off_full_Dw+1] * 100 * p /
	     total[CLG_(sets).off_full_Dw], p, l3+1, buf3);
  VG_(message)(Vg_UserMsg, "D1  miss rate: %s (%s   + %s  )\n", 
               buf1, buf2,buf3);
  
  percentify( D_total[2] * 100 * p / D_total[0],  p, l1+1, buf1);
  percentify(total[CLG_(sets).off_full_Dr+2] * 100 * p /
	     total[CLG_(sets).off_full_Dr], p, l2+1, buf2);
  percentify(total[CLG_(sets).off_full_Dw+2] * 100 * p /
	     total[CLG_(sets).off_full_Dw], p, l3+1, buf3);
  VG_(message)(Vg_UserMsg, "L2d miss rate: %s (%s   + %s  )\n", 
               buf1, buf2,buf3);
  VG_(message)(Vg_UserMsg, "\n");


  
  /* L2 overall results */
  
  L2_total   =
    total[CLG_(sets).off_full_Dr +1] +
    total[CLG_(sets).off_full_Dw +1] +
    total[CLG_(sets).off_full_Ir +1];
  L2_total_r =
    total[CLG_(sets).off_full_Dr +1] +
    total[CLG_(sets).off_full_Ir +1];
  L2_total_w = total[CLG_(sets).off_full_Dw +1];
  commify(L2_total,   l1, buf1);
  commify(L2_total_r, l2, buf2);
  commify(L2_total_w, l3, buf3);
  VG_(message)(Vg_UserMsg, "L2 refs:       %s  (%s rd + %s wr)\n",
	       buf1, buf2, buf3);
  
  L2_total_m  =
    total[CLG_(sets).off_full_Dr +2] +
    total[CLG_(sets).off_full_Dw +2] +
    total[CLG_(sets).off_full_Ir +2];
  L2_total_mr =
    total[CLG_(sets).off_full_Dr +2] +
    total[CLG_(sets).off_full_Ir +2];
  L2_total_mw = total[CLG_(sets).off_full_Dw +2];
  commify(L2_total_m,  l1, buf1);
  commify(L2_total_mr, l2, buf2);
  commify(L2_total_mw, l3, buf3);
  VG_(message)(Vg_UserMsg, "L2 misses:     %s  (%s rd + %s wr)\n",
	       buf1, buf2, buf3);
  
  percentify(L2_total_m  * 100 * p /
	     (total[CLG_(sets).off_full_Ir] + D_total[0]),  p, l1+1, buf1);
  percentify(L2_total_mr * 100 * p /
	     (total[CLG_(sets).off_full_Ir] + total[CLG_(sets).off_full_Dr]),
	     p, l2+1, buf2);
  percentify(L2_total_mw * 100 * p /
	     total[CLG_(sets).off_full_Dw], p, l3+1, buf3);
  VG_(message)(Vg_UserMsg, "L2 miss rate:  %s (%s   + %s  )\n",
	       buf1, buf2,buf3);
}


/*------------------------------------------------------------*/
/*--- Setup for Event set.                                 ---*/
/*------------------------------------------------------------*/

struct event_sets CLG_(sets);

void CLG_(init_eventsets)(Int max_user)
{
  EventType * e1, *e2, *e3, *e4;
  // Basic event sets from which others are composed
  EventSet *Use, *Ir, *Dr, *Dw;
  // Compositions of basic sets used for per-instruction counters
  EventSet *UIr, *UIrDr, *UIrDrDw, *UIrDw, *UIrDwDr;
  // Composition used for global counters and aggregation
  EventSet *full;
  int sizeOfUseIr;

  // the "Use" events types only are used with "cacheuse" simulation
  Use = CLG_(get_eventset)("Use", 4);
  if (clo_collect_cacheuse) {
    /* if TUse is 0, there was never a load, and no loss, too */
    e1 = CLG_(register_eventtype)("AcCost1");
    CLG_(add_eventtype)(Use, e1);
    e1 = CLG_(register_eventtype)("SpLoss1");
    CLG_(add_eventtype)(Use, e1);
    e1 = CLG_(register_eventtype)("AcCost2");
    CLG_(add_eventtype)(Use, e1);
    e1 = CLG_(register_eventtype)("SpLoss2");
    CLG_(add_eventtype)(Use, e1);
  }

  Ir = CLG_(get_eventset)("Ir", 4);
  Dr = CLG_(get_eventset)("Dr", 4);
  Dw = CLG_(get_eventset)("Dw", 4);
  if (CLG_(clo).simulate_cache) {
    e1 = CLG_(register_eventtype)("Ir");
    e2 = CLG_(register_eventtype)("I1mr");
    e3 = CLG_(register_eventtype)("I2mr");
    if (clo_simulate_writeback) {
      e4 = CLG_(register_eventtype)("I2dmr");
      CLG_(add_dep_event4)(Ir, e1,e2,e3,e4);
    }
    else
      CLG_(add_dep_event3)(Ir, e1,e2,e3);

    e1 = CLG_(register_eventtype)("Dr");
    e2 = CLG_(register_eventtype)("D1mr");
    e3 = CLG_(register_eventtype)("D2mr");
    if (clo_simulate_writeback) {
      e4 = CLG_(register_eventtype)("D2dmr");
      CLG_(add_dep_event4)(Dr, e1,e2,e3,e4);
    }
    else
      CLG_(add_dep_event3)(Dr, e1,e2,e3);
    
    e1 = CLG_(register_eventtype)("Dw");
    e2 = CLG_(register_eventtype)("D1mw");
    e3 = CLG_(register_eventtype)("D2mw");
    if (clo_simulate_writeback) {
      e4 = CLG_(register_eventtype)("D2dmw");
      CLG_(add_dep_event4)(Dw, e1,e2,e3,e4);
    }
    else
      CLG_(add_dep_event3)(Dw, e1,e2,e3);

  }
  else {
    e1 = CLG_(register_eventtype)("Ir");
    CLG_(add_eventtype)(Ir, e1);
  }

  // Self cost event sets per guest instruction (U used only for cacheUse).
  // Each basic event set only appears once, as eg. multiple different Dr's
  // in one guest instruction are counted in the same counter.

  sizeOfUseIr =  Use->size + Ir->size;
  UIr = CLG_(get_eventset)("UIr", sizeOfUseIr);
  CLG_(add_eventset)(UIr, Use);
  off_UIr_Ir  = CLG_(add_eventset)(UIr, Ir);

  UIrDr = CLG_(get_eventset)("UIrDr", sizeOfUseIr + Dr->size);
  CLG_(add_eventset)(UIrDr, Use);
  off_UIrDr_Ir = CLG_(add_eventset)(UIrDr, Ir);
  off_UIrDr_Dr = CLG_(add_eventset)(UIrDr, Dr);

  UIrDrDw  = CLG_(get_eventset)("IrDrDw", sizeOfUseIr + Dr->size + Dw->size);
  CLG_(add_eventset)(UIrDrDw, Use);
  off_UIrDrDw_Ir    = CLG_(add_eventset)(UIrDrDw, Ir);
  off_UIrDrDw_Dr    = CLG_(add_eventset)(UIrDrDw, Dr);
  off_UIrDrDw_Dw    = CLG_(add_eventset)(UIrDrDw, Dw);

  UIrDw = CLG_(get_eventset)("UIrDw", sizeOfUseIr + Dw->size);
  CLG_(add_eventset)(UIrDw, Use);
  off_UIrDw_Ir   = CLG_(add_eventset)(UIrDw, Ir);
  off_UIrDw_Dw   = CLG_(add_eventset)(UIrDw, Dw);

  UIrDwDr  = CLG_(get_eventset)("IrDwDr", sizeOfUseIr + Dw->size + Dr->size);
  CLG_(add_eventset)(UIrDwDr, Use);
  off_UIrDwDr_Ir    = CLG_(add_eventset)(UIrDrDw, Ir);
  off_UIrDwDr_Dw    = CLG_(add_eventset)(UIrDrDw, Dw);
  off_UIrDwDr_Dr    = CLG_(add_eventset)(UIrDrDw, Dr);


  // the "full" event set is used as global counter and for aggregation
  if (CLG_(clo).collect_alloc)   max_user += 2;
  if (CLG_(clo).collect_systime) max_user += 2;
  full = CLG_(get_eventset)("full",
			    sizeOfUseIr + Dr->size + Dw->size + max_user);
  CLG_(add_eventset)(full, Use);
  CLG_(sets).off_full_Ir   = CLG_(add_eventset)(full, Ir);
  CLG_(sets).off_full_Dr   = CLG_(add_eventset)(full, Dr);
  CLG_(sets).off_full_Dw   = CLG_(add_eventset)(full, Dw);
  if (CLG_(clo).collect_alloc) {
      e1 = CLG_(register_eventtype)("allocCount");
      e2 = CLG_(register_eventtype)("allocSize");
      CLG_(sets).off_full_alloc =  CLG_(add_dep_event2)(full, e1,e2);
  }
  if (CLG_(clo).collect_systime) {
      e1 = CLG_(register_eventtype)("sysCount");
      e2 = CLG_(register_eventtype)("sysTime");
      CLG_(sets).off_full_systime =  CLG_(add_dep_event2)(full, e1,e2);
  }

  CLG_(sets).Use = Use;
  CLG_(sets).Ir  = Ir;
  CLG_(sets).Dr  = Dr;
  CLG_(sets).Dw  = Dw;
  CLG_(sets).UIr  = UIr;
  CLG_(sets).UIrDr = UIrDr;
  CLG_(sets).UIrDrDw  = UIrDrDw;
  CLG_(sets).UIrDw = UIrDw;
  CLG_(sets).UIrDwDr  = UIrDwDr;
  CLG_(sets).full = full;


  CLG_DEBUGIF(1) {
    CLG_DEBUG(1, "EventSets:\n");
    CLG_(print_eventset)(-2, Use);
    CLG_(print_eventset)(-2, Ir);
    CLG_(print_eventset)(-2, Dr);
    CLG_(print_eventset)(-2, Dw);
    CLG_(print_eventset)(-2, full);
  }

  /* Not-existing events are silently ignored */
  CLG_(dumpmap) = CLG_(get_eventmapping)(full);
  CLG_(append_event)(CLG_(dumpmap), "Ir");
  CLG_(append_event)(CLG_(dumpmap), "Dr");
  CLG_(append_event)(CLG_(dumpmap), "Dw");
  CLG_(append_event)(CLG_(dumpmap), "I1mr");
  CLG_(append_event)(CLG_(dumpmap), "D1mr");
  CLG_(append_event)(CLG_(dumpmap), "D1mw");
  CLG_(append_event)(CLG_(dumpmap), "I2mr");
  CLG_(append_event)(CLG_(dumpmap), "D2mr");
  CLG_(append_event)(CLG_(dumpmap), "D2mw");
  CLG_(append_event)(CLG_(dumpmap), "I2dmr");
  CLG_(append_event)(CLG_(dumpmap), "D2dmr");
  CLG_(append_event)(CLG_(dumpmap), "D2dmw");
  CLG_(append_event)(CLG_(dumpmap), "AcCost1");
  CLG_(append_event)(CLG_(dumpmap), "SpLoss1");
  CLG_(append_event)(CLG_(dumpmap), "AcCost2");
  CLG_(append_event)(CLG_(dumpmap), "SpLoss2");
  CLG_(append_event)(CLG_(dumpmap), "allocCount");
  CLG_(append_event)(CLG_(dumpmap), "allocSize");
  CLG_(append_event)(CLG_(dumpmap), "sysCount");
  CLG_(append_event)(CLG_(dumpmap), "sysTime");

}



static
void add_and_zero_Dx(EventSet* es, SimCost dst, ULong* cost)
{
  /* if eventset use is defined, it is always first (hardcoded!) */
  CLG_(add_and_zero_cost)( CLG_(sets).Use, dst, cost);

  if (es == CLG_(sets).UIr) {
    CLG_(add_and_zero_cost)( CLG_(sets).Ir, dst + CLG_(sets).off_full_Ir,
			    cost + off_UIr_Ir);
  }
  else if (es == CLG_(sets).UIrDr) {
    CLG_(add_and_zero_cost)( CLG_(sets).Ir, dst + CLG_(sets).off_full_Ir,
			    cost + off_UIrDr_Ir);
    CLG_(add_and_zero_cost)( CLG_(sets).Dr, dst + CLG_(sets).off_full_Dr,
			    cost + off_UIrDr_Dr);
  }
  else if (es == CLG_(sets).UIrDrDw) {
    CLG_(add_and_zero_cost)( CLG_(sets).Ir, dst + CLG_(sets).off_full_Ir,
			    cost + off_UIrDrDw_Ir);
    CLG_(add_and_zero_cost)( CLG_(sets).Dr, dst + CLG_(sets).off_full_Dr,
			    cost + off_UIrDrDw_Dr);
    CLG_(add_and_zero_cost)( CLG_(sets).Dw, dst + CLG_(sets).off_full_Dw,
			    cost + off_UIrDrDw_Dw);
  }
  else if (es == CLG_(sets).UIrDw) {
      CLG_(add_and_zero_cost)( CLG_(sets).Ir, dst + CLG_(sets).off_full_Ir,
			       cost + off_UIrDw_Ir);
      CLG_(add_and_zero_cost)( CLG_(sets).Dw, dst + CLG_(sets).off_full_Dw,
			       cost + off_UIrDw_Dw);
  }
  else if (es == CLG_(sets).UIrDwDr) {
    CLG_(add_and_zero_cost)( CLG_(sets).Ir, dst + CLG_(sets).off_full_Ir,
			    cost + off_UIrDwDr_Ir);
    CLG_(add_and_zero_cost)( CLG_(sets).Dw, dst + CLG_(sets).off_full_Dw,
			    cost + off_UIrDwDr_Dw);
    CLG_(add_and_zero_cost)( CLG_(sets).Dr, dst + CLG_(sets).off_full_Dr,
			     cost + off_UIrDwDr_Dr);
  }
  else CLG_ASSERT(0);
}

/* this is called at dump time for every instruction executed */
static void cachesim_add_icost(SimCost cost, BBCC* bbcc,
			       InstrInfo* ii, ULong exe_count)
{
  if (!CLG_(clo).simulate_cache)
      cost[CLG_(sets).off_full_Ir] += exe_count;
  else {

#if 0
/* There is always a trivial case where exe_count and Ir can be
 * slightly different because ecounter is updated when executing
 * the next BB. E.g. for last BB executed, or when toggling collection
 */
      /* FIXME: Hardcoded that each eventset has Ir as first */
      if ((bbcc->cost + ii->cost_offset)[0] != exe_count) {
	  VG_(printf)("==> Ir %llu, exe %llu\n",
		      (bbcc->cost + ii->cost_offset)[0], exe_count);
	  CLG_(print_bbcc_cost)(-2, bbcc);
	  //CLG_ASSERT((bbcc->cost + ii->cost_offset)[0] == exe_count);
      }
#endif

      add_and_zero_Dx(ii->eventset, cost,
		      bbcc->cost + ii->cost_offset);
  }
}

static
void cachesim_after_bbsetup(void)
{
  BBCC* bbcc = CLG_(current_state).bbcc;

  if (CLG_(clo).simulate_cache) {
    BB* bb = bbcc->bb;

    /* only needed if log_* functions are called */
    bb_base   = bb->obj->offset + bb->offset;
    cost_base = bbcc->cost;
  }
}

static
void cachesim_finish(void)
{
  if (clo_collect_cacheuse)
    cacheuse_finish();
}

/*------------------------------------------------------------*/
/*--- The simulator defined in this file                   ---*/
/*------------------------------------------------------------*/

struct cachesim_if CLG_(cachesim) = {
  .print_opts    = cachesim_print_opts,
  .parse_opt     = cachesim_parse_opt,
  .post_clo_init = cachesim_post_clo_init,
  .clear         = cachesim_clear,
  .getdesc       = cachesim_getdesc,
  .printstat     = cachesim_printstat,
  .add_icost     = cachesim_add_icost,
  .after_bbsetup = cachesim_after_bbsetup,
  .finish        = cachesim_finish,

  /* these will be set by cachesim_post_clo_init */
  .log_1I0D        = 0,
  .log_2I0D        = 0,
  .log_3I0D        = 0,

  .log_1I1Dr       = 0,
  .log_1I1Dw       = 0,

  .log_0I1Dr       = 0,
  .log_0I1Dw       = 0,

  .log_1I0D_name = "(no function)",
  .log_2I0D_name = "(no function)",
  .log_3I0D_name = "(no function)",

  .log_1I1Dr_name = "(no function)",
  .log_1I1Dw_name = "(no function)",

  .log_0I1Dr_name = "(no function)",
  .log_0I1Dw_name = "(no function)",
};


/*--------------------------------------------------------------------*/
/*--- end                                                 ct_sim.c ---*/
/*--------------------------------------------------------------------*/

