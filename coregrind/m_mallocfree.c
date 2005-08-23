
/*--------------------------------------------------------------------*/
/*--- An implementation of malloc/free which doesn't use sbrk.     ---*/
/*---                                               m_mallocfree.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2005 Julian Seward 
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
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcmman.h"
#include "pub_core_libcprint.h"
#include "pub_core_mallocfree.h"
#include "pub_core_options.h"
#include "pub_core_profile.h"
#include "pub_core_tooliface.h"
#include "valgrind.h"

//zz#include "memcheck/memcheck.h"

//#define DEBUG_MALLOC      // turn on heavyweight debugging machinery
//#define VERBOSE_MALLOC    // make verbose, esp. in debugging machinery

/*------------------------------------------------------------*/
/*--- Main types                                           ---*/
/*------------------------------------------------------------*/

#define N_MALLOC_LISTS     18    // do not change this

// The amount you can ask for is limited only by sizeof(SizeT)...
#define MAX_PSZB              (~((SizeT)0x0))

typedef UChar UByte;

/* Layout of an in-use block:

      this block total szB     (sizeof(SizeT) bytes)
      red zone bytes           (depends on Arena.rz_szB, but > sizeof(void*))
      (payload bytes)
      red zone bytes           (depends on Arena.rz_szB, but > sizeof(void*))
      this block total szB     (sizeof(SizeT) bytes)

   Layout of a block on the free list:

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

// A superblock.  'padding' is never used, it just ensures that if the
// entire Superblock is aligned to VG_MIN_MALLOC_SZB, then payload_bytes[]
// will be too.  It can add small amounts of padding unnecessarily -- eg.
// 8-bytes on 32-bit machines with an 8-byte VG_MIN_MALLOC_SZB -- because
// it's too hard to make a constant expression that works perfectly in all
// cases.
// payload_bytes[] is made a single big Block when the Superblock is
// created, and then can be split and the splittings remerged, but Blocks
// always cover its entire length -- there's never any unused bytes at the
// end, for example.
typedef 
   struct _Superblock {
      struct _Superblock* next;
      SizeT n_payload_bytes;
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
      Char*       name;
      Bool        clientmem;        // Allocates in the client address space?
      SizeT       rz_szB;           // Red zone size in bytes
      SizeT       min_sblock_szB;   // Minimum superblock size in bytes
      Block*      freelist[N_MALLOC_LISTS];
      Superblock* sblocks;
      // Stats only.
      SizeT bytes_on_loan;
      SizeT bytes_mmaped;
      SizeT bytes_on_loan_max;
   } 
   Arena;


/*------------------------------------------------------------*/
/*--- Low-level functions for working with Blocks.         ---*/
/*------------------------------------------------------------*/

#define SIZE_T_0x1      ((SizeT)0x1)

// Mark a bszB as in-use, and not in-use, and remove the in-use attribute.
static __inline__
SizeT mk_inuse_bszB ( SizeT bszB )
{
   vg_assert(bszB != 0);
   return bszB & (~SIZE_T_0x1);
}
static __inline__
SizeT mk_free_bszB ( SizeT bszB )
{
   vg_assert(bszB != 0);
   return bszB | SIZE_T_0x1;
}
static __inline__
SizeT mk_plain_bszB ( SizeT bszB )
{
   vg_assert(bszB != 0);
   return bszB & (~SIZE_T_0x1);
}

//---------------------------------------------------------------------------

// Get a block's size as stored, ie with the in-use/free attribute.
static __inline__
SizeT get_bszB_as_is ( Block* b )
{
   UByte* b2     = (UByte*)b;
   SizeT bszB_lo = *(SizeT*)&b2[0];
   SizeT bszB_hi = *(SizeT*)&b2[mk_plain_bszB(bszB_lo) - sizeof(SizeT)];
   vg_assert2(bszB_lo == bszB_hi, 
      "Heap block lo/hi size mismatch: lo = %llu, hi = %llu.\n"
      "Probably caused by overrunning/underrunning a heap block's bounds\n");
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
   *(SizeT*)&b2[0]                                   = bszB;
   *(SizeT*)&b2[mk_plain_bszB(bszB) - sizeof(SizeT)] = bszB;
}

//---------------------------------------------------------------------------

// Does this block have the in-use attribute?
static __inline__
Bool is_inuse_block ( Block* b )
{
   SizeT bszB = get_bszB_as_is(b);
   vg_assert(bszB != 0);
   return (0 != (bszB & SIZE_T_0x1)) ? False : True;
}

//---------------------------------------------------------------------------

// Return the lower, upper and total overhead in bytes for a block.
// These are determined purely by which arena the block lives in.
static __inline__
SizeT overhead_szB_lo ( Arena* a )
{
   return sizeof(SizeT) + a->rz_szB;
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
   vg_assert(bszB >= overhead_szB(a));
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
   *(Block**)&b2[sizeof(SizeT)] = prev_p;
}
static __inline__
void set_next_b ( Block* b, Block* next_p )
{
   UByte* b2 = (UByte*)b;
   *(Block**)&b2[get_bszB(b) - sizeof(SizeT) - sizeof(void*)] = next_p;
}
static __inline__
Block* get_prev_b ( Block* b )
{ 
   UByte* b2 = (UByte*)b;
   return *(Block**)&b2[sizeof(SizeT)];
}
static __inline__
Block* get_next_b ( Block* b )
{ 
   UByte* b2 = (UByte*)b;
   return *(Block**)&b2[get_bszB(b) - sizeof(SizeT) - sizeof(void*)];
}

//---------------------------------------------------------------------------

// Get the block immediately preceding this one in the Superblock.
static __inline__
Block* get_predecessor_block ( Block* b )
{
   UByte* b2 = (UByte*)b;
   SizeT  bszB = mk_plain_bszB( (*(SizeT*)&b2[-sizeof(SizeT)]) );
   return (Block*)&b2[-bszB];
}

//---------------------------------------------------------------------------

// Read and write the lower and upper red-zone bytes of a block.
static __inline__
void set_rz_lo_byte ( Arena* a, Block* b, UInt rz_byteno, UByte v )
{
   UByte* b2 = (UByte*)b;
   b2[sizeof(SizeT) + rz_byteno] = v;
}
static __inline__
void set_rz_hi_byte ( Arena* a, Block* b, UInt rz_byteno, UByte v )
{
   UByte* b2 = (UByte*)b;
   b2[get_bszB(b) - sizeof(SizeT) - rz_byteno - 1] = v;
}
static __inline__
UByte get_rz_lo_byte ( Arena* a, Block* b, UInt rz_byteno )
{
   UByte* b2 = (UByte*)b;
   return b2[sizeof(SizeT) + rz_byteno];
}
static __inline__
UByte get_rz_hi_byte ( Arena* a, Block* b, UInt rz_byteno )
{
   UByte* b2 = (UByte*)b;
   return b2[get_bszB(b) - sizeof(SizeT) - rz_byteno - 1];
}


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

// Initialise an arena.  rz_szB is the minimum redzone size;  it might be
// made bigger to ensure that VG_MIN_MALLOC_SZB is observed.
static
void arena_init ( ArenaId aid, Char* name, SizeT rz_szB, SizeT min_sblock_szB )
{
   SizeT i;
   Arena* a = arenaId_to_ArenaP(aid);
   
   vg_assert(rz_szB < 128);      // ensure reasonable size
   vg_assert((min_sblock_szB % VKI_PAGE_SIZE) == 0);
   a->name      = name;
   a->clientmem = ( VG_AR_CLIENT == aid ? True : False );

   // The size of the low and high admin sections in a block must be a
   // multiple of VG_MIN_MALLOC_SZB.  So we round up the asked-for
   // redzone size if necessary to achieve this.
   a->rz_szB = rz_szB;
   while (0 != overhead_szB_lo(a) % VG_MIN_MALLOC_SZB) a->rz_szB++;
   vg_assert(overhead_szB_lo(a) == overhead_szB_hi(a));

   a->min_sblock_szB = min_sblock_szB;
   for (i = 0; i < N_MALLOC_LISTS; i++) a->freelist[i] = NULL;
   a->sblocks           = NULL;
   a->bytes_on_loan     = 0;
   a->bytes_mmaped      = 0;
   a->bytes_on_loan_max = 0;
}

/* Print vital stats for an arena. */
void VG_(print_all_arena_stats) ( void )
{
   UInt i;
   for (i = 0; i < VG_N_ARENAS; i++) {
      Arena* a = arenaId_to_ArenaP(i);
      VG_(message)(Vg_DebugMsg,
         "%8s: %8d mmap'd, %8d/%8d max/curr",
         a->name, a->bytes_mmaped, a->bytes_on_loan_max, a->bytes_on_loan 
      );
   }
}

/* This library is self-initialising, as it makes this more self-contained,
   less coupled with the outside world.  Hence VG_(arena_malloc)() and
   VG_(arena_free)() below always call ensure_mm_init() to ensure things are
   correctly initialised.  */
static
void ensure_mm_init ( void )
{
   static Bool  init_done = False;
   static SizeT client_redzone_szB = 8;   // default: be paranoid

   if (init_done) {
      // This assertion ensures that a tool cannot try to change the client
      // redzone size with VG_(needs_malloc_replacement)() after this module
      // has done its first allocation.
      if (VG_(needs).malloc_replacement)
         vg_assert(client_redzone_szB == VG_(tdict).tool_client_redzone_szB);
      return;
   }

   if (VG_(needs).malloc_replacement) {
      client_redzone_szB = VG_(tdict).tool_client_redzone_szB;
      // 128 is no special figure, just something not too big
      if (client_redzone_szB > 128) {
         VG_(printf)( "\nTool error:\n"
                      "  specified redzone size is too big (%llu)\n", 
                      (ULong)client_redzone_szB);
         VG_(exit)(1);
      }
   }

   /* Use checked red zones (of various sizes) for our internal stuff,
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
   arena_init ( VG_AR_CORE,      "core",     4,       CORE_ARENA_MIN_SZB );
   arena_init ( VG_AR_TOOL,      "tool",     4,                  1048576 );
   arena_init ( VG_AR_SYMTAB,    "symtab",   4,                  1048576 );
   arena_init ( VG_AR_CLIENT,    "client",   client_redzone_szB, 1048576 );
   arena_init ( VG_AR_DEMANGLE,  "demangle", 4,                    65536 );
   arena_init ( VG_AR_EXECTXT,   "exectxt",  4,                    65536 );
   arena_init ( VG_AR_ERRORS,    "errors",   4,                    65536 );

   init_done = True;
#  ifdef DEBUG_MALLOC
   VG_(sanity_check_malloc_all)();
#  endif
}


/*------------------------------------------------------------*/
/*--- Superblock management                                ---*/
/*------------------------------------------------------------*/

// Align ptr p upwards to an align-sized boundary.
static
void* align_upwards ( void* p, SizeT align )
{
   Addr a = (Addr)p;
   if ((a % align) == 0) return (void*)a;
   return (void*)(a - (a % align) + align);
}

// If not enough memory available, either aborts (for non-client memory)
// or returns 0 (for client memory).
static
Superblock* newSuperblock ( Arena* a, SizeT cszB )
{
   // The extra VG_MIN_MALLOC_SZB bytes are for possible alignment up.
   static UByte bootstrap_superblock[CORE_ARENA_MIN_SZB+VG_MIN_MALLOC_SZB];
   static Bool  called_before = True; //False;
   Superblock* sb;

   // Take into account admin bytes in the Superblock.
   cszB += sizeof(Superblock);

   if (cszB < a->min_sblock_szB) cszB = a->min_sblock_szB;
   while ((cszB % VKI_PAGE_SIZE) > 0) cszB++;

   if (!called_before) {
      // First time we're called -- use the special static bootstrap
      // superblock (see comment at top of main() for details).
      called_before = True;
      vg_assert(a == arenaId_to_ArenaP(VG_AR_CORE));
      vg_assert(CORE_ARENA_MIN_SZB >= cszB);
      // Ensure sb is suitably aligned.
      sb = (Superblock*)align_upwards( bootstrap_superblock, 
                                       VG_MIN_MALLOC_SZB );
   } else if (a->clientmem) {
      // client allocation -- return 0 to client if it fails
      sb = (Superblock*)VG_(get_memory_from_mmap_for_client)(cszB);
      if (NULL == sb)
         return 0;
   } else {
      // non-client allocation -- aborts if it fails
      sb = VG_(get_memory_from_mmap) ( cszB, "newSuperblock" );
   }
   vg_assert(NULL != sb);
   //zzVALGRIND_MAKE_WRITABLE(sb, cszB);
   vg_assert(0 == (Addr)sb % VG_MIN_MALLOC_SZB);
   sb->n_payload_bytes = cszB - sizeof(Superblock);
   a->bytes_mmaped += cszB;
   if (0)
      VG_(message)(Vg_DebugMsg, "newSuperblock, %d payload bytes", 
                                sb->n_payload_bytes);
   return sb;
}

// Find the superblock containing the given chunk.
static
Superblock* findSb ( Arena* a, Block* b )
{
   Superblock* sb;
   for (sb = a->sblocks; sb; sb = sb->next)
      if ((Block*)&sb->payload_bytes[0] <= b
          && b < (Block*)&sb->payload_bytes[sb->n_payload_bytes])
         return sb;
   VG_(printf)("findSb: can't find pointer %p in arena '%s'\n", b, a->name );
   VG_(core_panic)("findSb: VG_(arena_free)() in wrong arena?");
   return NULL; /*NOTREACHED*/
}


/*------------------------------------------------------------*/
/*--- Functions for working with freelists.                ---*/
/*------------------------------------------------------------*/

// Nb: Determination of which freelist a block lives on is based on the
// payload size, not block size.

// Convert a payload size in bytes to a freelist number.
static
UInt pszB_to_listNo ( SizeT pszB )
{
   vg_assert(0 == pszB % VG_MIN_MALLOC_SZB);
   SizeT n = pszB / VG_MIN_MALLOC_SZB;

   // The first 13 lists hold blocks of size VG_MIN_MALLOC_SZB * list_num.
   // The final 5 hold bigger blocks.
   if (n <= 12)  return (UInt)n;
   if (n <= 16)  return 13;
   if (n <= 32)  return 14;
   if (n <= 64)  return 15;
   if (n <= 128) return 16;
   return 17;
}

// What is the minimum payload size for a given list?
static
SizeT listNo_to_pszB_min ( UInt listNo )
{
   SizeT pszB = 0;
   vg_assert(listNo <= N_MALLOC_LISTS);
   while (pszB_to_listNo(pszB) < listNo) pszB += VG_MIN_MALLOC_SZB;
   return pszB;
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
   for (i = 0; i < 20; i++) {
      pn = get_next_b(pn);
      pp = get_prev_b(pp);
      if (pn < p_best) p_best = pn;
      if (pp < p_best) p_best = pp;
   }
   if (p_best < a->freelist[lno]) {
#     ifdef VERBOSE_MALLOC
      VG_(printf)("retreat by %d\n", a->freelist[lno] - p_best);
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
      for (i = 0; i < a->rz_szB; i++) {
         if (get_rz_lo_byte(a, b, i) != 
            (UByte)(((Addr)b&0xff) ^ REDZONE_LO_MASK))
               {BLEAT("redzone-lo");return False;}
         if (get_rz_hi_byte(a, b, i) != 
            (UByte)(((Addr)b&0xff) ^ REDZONE_HI_MASK))
               {BLEAT("redzone-hi");return False;}
      }      
   }
   return True;
#  undef BLEAT
}

// Print superblocks (only for debugging).
static 
void ppSuperblocks ( Arena* a )
{
   UInt i, blockno = 1;
   Superblock* sb = a->sblocks;
   SizeT b_bszB;

   while (sb) {
      VG_(printf)( "\n" );
      VG_(printf)( "superblock %d at %p, sb->n_pl_bs = %d, next = %p\n", 
                   blockno++, sb, sb->n_payload_bytes, sb->next );
      for (i = 0; i < sb->n_payload_bytes; i += b_bszB) {
         Block* b = (Block*)&sb->payload_bytes[i];
         b_bszB   = get_bszB(b);
         VG_(printf)( "   block at %d, bszB %d: ", i, b_bszB );
         VG_(printf)( "%s, ", is_inuse_block(b) ? "inuse" : "free");
         VG_(printf)( "%s\n", blockSane(a, b) ? "ok" : "BAD" );
      }
      vg_assert(i == sb->n_payload_bytes);   // no overshoot at end of Sb
      sb = sb->next;
   }
   VG_(printf)( "end of superblocks\n\n" );
}

// Sanity check both the superblocks and the chains.
static void sanity_check_malloc_arena ( ArenaId aid )
{
   UInt        i, superblockctr, blockctr_sb, blockctr_li;
   UInt        blockctr_sb_free, listno;
   SizeT       b_bszB, b_pszB, list_min_pszB, list_max_pszB;
   Superblock* sb;
   Bool        thisFree, lastWasFree;
   Block*      b;
   Block*      b_prev;
   SizeT       arena_bytes_on_loan;
   Arena*      a;

#  define BOMB VG_(core_panic)("sanity_check_malloc_arena")

   a = arenaId_to_ArenaP(aid);
   
   // First, traverse all the superblocks, inspecting the Blocks in each.
   superblockctr = blockctr_sb = blockctr_sb_free = 0;
   arena_bytes_on_loan = 0;
   sb = a->sblocks;
   while (sb) {
      lastWasFree = False;
      superblockctr++;
      for (i = 0; i < sb->n_payload_bytes; i += mk_plain_bszB(b_bszB)) {
         blockctr_sb++;
         b     = (Block*)&sb->payload_bytes[i];
         b_bszB = get_bszB_as_is(b);
         if (!blockSane(a, b)) {
            VG_(printf)("sanity_check_malloc_arena: sb %p, block %d (bszB %d): "
                        " BAD\n", sb, i, b_bszB );
            BOMB;
         }
         thisFree = !is_inuse_block(b);
         if (thisFree && lastWasFree) {
            VG_(printf)("sanity_check_malloc_arena: sb %p, block %d (bszB %d): "
                        "UNMERGED FREES\n",
                         sb, i, b_bszB );
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
      sb = sb->next;
   }

   if (arena_bytes_on_loan != a->bytes_on_loan) {
#     ifdef VERBOSE_MALLOC
      VG_(printf)( "sanity_check_malloc_arena: a->bytes_on_loan %d, "
                   "arena_bytes_on_loan %d: "
                   "MISMATCH\n", a->bytes_on_loan, arena_bytes_on_loan);
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
            VG_(printf)( "sanity_check_malloc_arena: list %d at %p: "
                         "BAD LINKAGE\n", 
                         listno, b );
            BOMB;
         }
         b_pszB = get_pszB(a, b);
         if (b_pszB < list_min_pszB || b_pszB > list_max_pszB) {
            VG_(printf)( 
               "sanity_check_malloc_arena: list %d at %p: "
               "WRONG CHAIN SIZE %dB (%dB, %dB)\n", 
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
                   "%8s: %2d sbs, %5d bs, %2d/%-2d free bs, "
                   "%7d mmap, %7d loan", 
                   a->name,
                   superblockctr,
                   blockctr_sb, blockctr_sb_free, blockctr_li, 
                   a->bytes_mmaped, a->bytes_on_loan);   
#  undef BOMB
}


void VG_(sanity_check_malloc_all) ( void )
{
   UInt i;
   for (i = 0; i < VG_N_ARENAS; i++)
      sanity_check_malloc_arena ( i );
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
   //zzVALGRIND_MAKE_WRITABLE(b, bszB);
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
   //zzVALGRIND_MAKE_WRITABLE(b, bszB);
   set_bszB(b, mk_inuse_bszB(bszB));
   set_prev_b(b, NULL);    // Take off freelist
   set_next_b(b, NULL);    // ditto
   if (!a->clientmem) {
      for (i = 0; i < a->rz_szB; i++) {
         set_rz_lo_byte(a, b, i, (UByte)(((Addr)b&0xff) ^ REDZONE_LO_MASK));
         set_rz_hi_byte(a, b, i, (UByte)(((Addr)b&0xff) ^ REDZONE_HI_MASK));
      }
   }
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

void* VG_(arena_malloc) ( ArenaId aid, SizeT req_pszB )
{
   SizeT       req_bszB, frag_bszB, b_bszB;
   UInt        lno;
   Superblock* new_sb;
   Block*      b = NULL;
   Arena*      a;
   void*       v;

   VGP_PUSHCC(VgpMalloc);

   ensure_mm_init();
   a = arenaId_to_ArenaP(aid);

   vg_assert(req_pszB < MAX_PSZB);
   req_pszB = align_req_pszB(req_pszB);
   req_bszB = pszB_to_bszB(a, req_pszB);

   // Scan through all the big-enough freelists for a block.
   for (lno = pszB_to_listNo(req_pszB); lno < N_MALLOC_LISTS; lno++) {
      b = a->freelist[lno];
      if (NULL == b) continue;   // If this list is empty, try the next one.
      while (True) {
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
   new_sb->next = a->sblocks;
   a->sblocks = new_sb;
   b = (Block*)&new_sb->payload_bytes[0];
   lno = pszB_to_listNo(bszB_to_pszB(a, new_sb->n_payload_bytes));
   mkFreeBlock ( a, b, new_sb->n_payload_bytes, lno);
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
   frag_bszB = b_bszB - req_bszB;
   if (frag_bszB >= min_useful_bszB(a)) {
      // Yes, split block in two, put the fragment on the appropriate free
      // list, and update b_bszB accordingly.
      // printf( "split %dB into %dB and %dB\n", b_bszB, req_bszB, frag_bszB );
      unlinkBlock(a, b, lno);
      mkInuseBlock(a, b, req_bszB);
      mkFreeBlock(a, &b[req_bszB], frag_bszB, 
                     pszB_to_listNo(bszB_to_pszB(a, frag_bszB)));
      b_bszB = get_bszB(b);
   } else {
      // No, mark as in use and use as-is.
      unlinkBlock(a, b, lno);
      mkInuseBlock(a, b, b_bszB);
   }

   // Update stats
   a->bytes_on_loan += bszB_to_pszB(a, b_bszB);
   if (a->bytes_on_loan > a->bytes_on_loan_max)
      a->bytes_on_loan_max = a->bytes_on_loan;

#  ifdef DEBUG_MALLOC
   sanity_check_malloc_arena(aid);
#  endif

   VGP_POPCC(VgpMalloc);
   v = get_block_payload(a, b);
   vg_assert( (((Addr)v) & (VG_MIN_MALLOC_SZB-1)) == 0 );

   VALGRIND_MALLOCLIKE_BLOCK(v, req_pszB, 0, False);
   return v;
}

 
void VG_(arena_free) ( ArenaId aid, void* ptr )
{
   Superblock* sb;
   UByte*      sb_start;
   UByte*      sb_end;
   Block*      other_b;
   Block*      b;
   SizeT       b_bszB, b_pszB, other_bszB;
   UInt        b_listno;
   Arena*      a;

   VGP_PUSHCC(VgpMalloc);

   ensure_mm_init();
   a = arenaId_to_ArenaP(aid);

   if (ptr == NULL) {
      VGP_POPCC(VgpMalloc);
      return;
   }
      
   b = get_payload_block(a, ptr);

#  ifdef DEBUG_MALLOC
   vg_assert(blockSane(a, b));
#  endif

   b_bszB   = get_bszB(b);
   b_pszB   = bszB_to_pszB(a, b_bszB);
   sb       = findSb( a, b );
   sb_start = &sb->payload_bytes[0];
   sb_end   = &sb->payload_bytes[sb->n_payload_bytes - 1];

   a->bytes_on_loan -= b_pszB;

   // Put this chunk back on a list somewhere.
   b_listno = pszB_to_listNo(b_pszB);
   mkFreeBlock( a, b, b_bszB, b_listno );

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
         unlinkBlock( a, other_b, pszB_to_listNo(bszB_to_pszB(a,other_bszB)) );
         b_bszB += other_bszB;
         b_listno = pszB_to_listNo(bszB_to_pszB(a, b_bszB));
         mkFreeBlock( a, b, b_bszB, b_listno );
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
         unlinkBlock( a, other_b, pszB_to_listNo(bszB_to_pszB(a, other_bszB)) );
         b = other_b;
         b_bszB += other_bszB;
         b_listno = pszB_to_listNo(bszB_to_pszB(a, b_bszB));
         mkFreeBlock( a, b, b_bszB, b_listno );
      }
   } else {
      // Not enough space for predecessor: check that b is the first block,
      // ie. there are no unused bytes at the start of the Superblock.
      vg_assert((Block*)sb_start == b);
   }

#  ifdef DEBUG_MALLOC
   sanity_check_malloc_arena(aid);
#  endif

   VALGRIND_FREELIKE_BLOCK(ptr, 0);

   VGP_POPCC(VgpMalloc);
}


/*
   The idea for malloc_aligned() is to allocate a big block, base, and
   then split it into two parts: frag, which is returned to the the
   free pool, and align, which is the bit we're really after.  Here's
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
void* VG_(arena_memalign) ( ArenaId aid, SizeT req_alignB, SizeT req_pszB )
{
   SizeT  base_pszB_req, base_pszB_act, frag_bszB;
   Block  *base_b, *align_b;
   UByte  *base_p, *align_p;
   SizeT  saved_bytes_on_loan;
   Arena* a;

   VGP_PUSHCC(VgpMalloc);

   ensure_mm_init();
   a = arenaId_to_ArenaP(aid);

   vg_assert(req_pszB < MAX_PSZB);

   // Check that the requested alignment seems reasonable; that is, is
   // a power of 2.
   if (req_alignB < VG_MIN_MALLOC_SZB
       || req_alignB > 1048576
       || VG_(log2)( req_alignB ) == -1 /* not a power of 2 */) {
      VG_(printf)("VG_(arena_memalign)(%p, %d, %d)\nbad alignment", 
                  a, req_alignB, req_pszB );
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
   saved_bytes_on_loan = a->bytes_on_loan;
   base_p = VG_(arena_malloc) ( aid, base_pszB_req );
   a->bytes_on_loan = saved_bytes_on_loan;

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

   /* Create the aligned block. */
   mkInuseBlock ( a, align_b,
                  base_p + base_pszB_act 
                         + overhead_szB_hi(a) - (UByte*)align_b );

   /* Final sanity checks. */
   vg_assert( is_inuse_block(get_payload_block(a, align_p)) );

   vg_assert(req_pszB <= get_pszB(a, get_payload_block(a, align_p)));

   a->bytes_on_loan += get_pszB(a, get_payload_block(a, align_p));
   if (a->bytes_on_loan > a->bytes_on_loan_max)
      a->bytes_on_loan_max = a->bytes_on_loan;

#  ifdef DEBUG_MALLOC
   sanity_check_malloc_arena(aid);
#  endif

   VGP_POPCC(VgpMalloc);

   vg_assert( (((Addr)align_p) % req_alignB) == 0 );

   VALGRIND_MALLOCLIKE_BLOCK(align_p, req_pszB, 0, False);

   return align_p;
}


SizeT VG_(arena_payload_szB) ( ThreadId tid, ArenaId aid, void* ptr )
{
   Arena* a = arenaId_to_ArenaP(aid);
   Block* b = get_payload_block(a, ptr);
   return get_pszB(a, b);
}

// We cannot return the whole struct as the library function does,
// because this is called by a client request.  So instead we use
// a pointer to do call by reference.
void VG_(mallinfo) ( ThreadId tid, struct vg_mallinfo* mi )
{
   // Should do better than this...
   VG_(memset)(mi, 0x0, sizeof(struct vg_mallinfo));
}

/*------------------------------------------------------------*/
/*--- Services layered on top of malloc/free.              ---*/
/*------------------------------------------------------------*/

void* VG_(arena_calloc) ( ArenaId aid, SizeT nmemb, SizeT bytes_per_memb )
{
   SizeT  size;
   UChar* p;

   VGP_PUSHCC(VgpMalloc);

   size = nmemb * bytes_per_memb;
   vg_assert(size >= nmemb && size >= bytes_per_memb);// check against overflow

   p = VG_(arena_malloc) ( aid, size );

   VG_(memset)(p, 0, size);

   VALGRIND_MALLOCLIKE_BLOCK(p, size, 0, True);

   VGP_POPCC(VgpMalloc);
   
   return p;
}


void* VG_(arena_realloc) ( ArenaId aid, void* ptr, SizeT req_pszB )
{
   Arena* a;
   SizeT  old_pszB;
   UChar  *p_new;
   Block* b;

   VGP_PUSHCC(VgpMalloc);

   ensure_mm_init();
   a = arenaId_to_ArenaP(aid);

   vg_assert(req_pszB < MAX_PSZB);

   b = get_payload_block(a, ptr);
   vg_assert(blockSane(a, b));

   vg_assert(is_inuse_block(b));
   old_pszB = get_pszB(a, b);

   if (req_pszB <= old_pszB) {
      VGP_POPCC(VgpMalloc);
      return ptr;
   }

   p_new = VG_(arena_malloc) ( aid, req_pszB );
      
   VG_(memcpy)(p_new, ptr, old_pszB);

   VG_(arena_free)(aid, ptr);

   VGP_POPCC(VgpMalloc);
   return p_new;
}


/* Inline just for the wrapper VG_(strdup) below */
__inline__ Char* VG_(arena_strdup) ( ArenaId aid, const Char* s )
{
   Int   i;
   Int   len;
   Char* res;

   if (s == NULL)
      return NULL;

   len = VG_(strlen)(s) + 1;
   res = VG_(arena_malloc) (aid, len);

   for (i = 0; i < len; i++)
      res[i] = s[i];
   return res;
}


/*------------------------------------------------------------*/
/*--- Tool-visible functions.                              ---*/
/*------------------------------------------------------------*/

// All just wrappers to avoid exposing arenas to tools.

void* VG_(malloc) ( SizeT nbytes )
{
   return VG_(arena_malloc) ( VG_AR_TOOL, nbytes );
}

void  VG_(free) ( void* ptr )
{
   VG_(arena_free) ( VG_AR_TOOL, ptr );
}

void* VG_(calloc) ( SizeT nmemb, SizeT bytes_per_memb )
{
   return VG_(arena_calloc) ( VG_AR_TOOL, nmemb, bytes_per_memb );
}

void* VG_(realloc) ( void* ptr, SizeT size )
{
   return VG_(arena_realloc) ( VG_AR_TOOL, ptr, size );
}

Char* VG_(strdup) ( const Char* s )
{
   return VG_(arena_strdup) ( VG_AR_TOOL, s ); 
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
