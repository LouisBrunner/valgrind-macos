
/*--------------------------------------------------------------------*/
/*--- An implementation of malloc/free which doesn't use sbrk.     ---*/
/*---                                                 vg_malloc2.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an extensible x86 protected-mode
   emulator for monitoring program execution on x86-Unixes.

   Copyright (C) 2000-2003 Julian Seward 
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


#include "vg_include.h"

/* Define to turn on (heavyweight) debugging machinery. */
/* #define DEBUG_MALLOC */


/*------------------------------------------------------------*/
/*--- Structs n stuff                                      ---*/
/*------------------------------------------------------------*/

#define VG_REDZONE_LO_MASK 0x31415927
#define VG_REDZONE_HI_MASK 0x14141356

#define VG_N_MALLOC_LISTS 16 /* do not change this */


typedef UInt Word;
typedef Word WordF;
typedef Word WordL;


/* A superblock. */
typedef 
   struct _Superblock {
      struct _Superblock* next;
      /* number of payload words in this superblock. */
      Int  n_payload_words;
      Word payload_words[0];
   }
   Superblock;


/* An arena. */
typedef 
   struct {
      Char*       name;
      Int         rz_szW; /* Red zone size in words */
      Bool        rz_check; /* Check red-zone on free? */
      Int         min_sblockW; /* Minimum superblock size */
      WordF*      freelist[VG_N_MALLOC_LISTS];
      Superblock* sblocks;
      /* Stats only. */
      UInt bytes_on_loan;
      UInt bytes_mmaped;
      UInt bytes_on_loan_max;
   } 
   Arena;


/* Block layout:

     this block total sizeW   (1 word)
     freelist previous ptr    (1 word)
     freelist next  ptr       (1 word)
     red zone words (depends on .rz_szW field of Arena)
     (payload words)
     red zone words (depends on .rz_szW field of Arena)
     this block total sizeW  (1 word)

     Total size in words (bszW) and payload size in words (pszW)
     are related by
        bszW == pszW + 4 + 2 * a->rz_szW

     Furthermore, both size fields in the block are negative if it is
     not in use, and positive if it is in use.  A block size of zero
     is not possible, because a block always has at least four words
     of overhead.  
*/
typedef
   struct {
      Int   bszW_lo;
      Word* prev;
      Word* next;
      Word  redzone[0];
   } 
   BlockHeader;


/*------------------------------------------------------------*/
/*--- Forwardses ... and misc ...                          ---*/
/*------------------------------------------------------------*/

static Bool blockSane ( Arena* a, Word* b );

/* Align ptr p upwards to an align-sized boundary. */
static
void* align_upwards ( void* p, Int align )
{
   Addr a = (Addr)p;
   if ((a % align) == 0) return (void*)a;
   return (void*)(a - (a % align) + align);
}


/*------------------------------------------------------------*/
/*--- Arena management stuff                               ---*/
/*------------------------------------------------------------*/

/* The arena structures themselves. */
static Arena vg_arena[VG_N_ARENAS];

/* Functions external to this module identify arenas using ArenaIds,
   not Arena*s.  This fn converts the former to the latter. */
static Arena* arenaId_to_ArenaP ( ArenaId arena )
{
   vg_assert(arena >= 0 && arena < VG_N_ARENAS);
   return & vg_arena[arena];
}


/* Initialise an arena. */
static
void arena_init ( Arena* a, Char* name, 
                  Int rz_szW, Bool rz_check, Int min_sblockW )
{
   Int i;
   vg_assert((min_sblockW % VKI_WORDS_PER_PAGE) == 0);
   a->name = name;
   a->rz_szW = rz_szW;
   a->rz_check = rz_check;
   a->min_sblockW = min_sblockW;
   for (i = 0; i < VG_N_MALLOC_LISTS; i++) a->freelist[i] = NULL;
   a->sblocks = NULL;
   a->bytes_on_loan     = 0;
   a->bytes_mmaped      = 0;
   a->bytes_on_loan_max = 0;
}


/* Print vital stats for an arena. */
void VG_(show_all_arena_stats) ( void )
{
   Int i;
   for (i = 0; i < VG_N_ARENAS; i++) {
      VG_(message)(Vg_DebugMsg,
         "Arena `%s': %7d max useful, %7d mmap'd, %7d current useful",
         vg_arena[i].name, 
         vg_arena[i].bytes_on_loan_max, 
         vg_arena[i].bytes_mmaped, 
         vg_arena[i].bytes_on_loan 
      );
   }
}


/* It is important that this library is self-initialising, because it
   may get called very early on -- as a result of C++ static
   constructor initialisations -- before Valgrind itself is
   initialised.  Hence VG_(arena_malloc)() and VG_(arena_free)() below always
   call ensure_mm_init() to ensure things are correctly initialised.  */

static
void ensure_mm_init ( void )
{
   static Bool init_done = False;

   if (init_done) return;

   /* Use a checked red zone size of 1 word for our internal stuff,
      and an unchecked zone of arbitrary size for the client.  Of
      course the client's red zone can be checked by the skin, eg. 
      by using addressibility maps, but not by the mechanism implemented
      here, which merely checks at the time of freeing that the red 
      zone words are unchanged. */

   arena_init ( &vg_arena[VG_AR_CORE],      "core",     1, True, 262144 );

   arena_init ( &vg_arena[VG_AR_SKIN],      "skin",     1, True, 262144 );

   arena_init ( &vg_arena[VG_AR_SYMTAB],    "symtab",   1, True, 262144 );

   arena_init ( &vg_arena[VG_AR_JITTER],    "JITter",   1, True, 8192 );

   /* No particular reason for this figure, it's just smallish */
   sk_assert(VG_(vg_malloc_redzone_szB) < 128);
   arena_init ( &vg_arena[VG_AR_CLIENT],    "client",  
                VG_(vg_malloc_redzone_szB)/4, False, 262144 );

   arena_init ( &vg_arena[VG_AR_DEMANGLE],  "demangle", 4 /*paranoid*/,
                                                           True, 16384 );

   arena_init ( &vg_arena[VG_AR_EXECTXT],   "exectxt",  1, True, 16384 );

   arena_init ( &vg_arena[VG_AR_ERRORS],    "errors",   1, True, 16384 );

   arena_init ( &vg_arena[VG_AR_TRANSIENT], "transien", 2, True, 16384 );

   init_done = True;
#  ifdef DEBUG_MALLOC
   VG_(mallocSanityCheckAll)();
#  endif
}


/*------------------------------------------------------------*/
/*--- Arena management stuff                               ---*/
/*------------------------------------------------------------*/

static
Superblock* newSuperblock ( Arena* a, Int cszW )
{
   Superblock* sb;
   cszW += 2; /* Take into account sb->next and sb->n_words fields */
   if (cszW < a->min_sblockW) cszW = a->min_sblockW;
   while ((cszW % VKI_WORDS_PER_PAGE) > 0) cszW++;
   sb = VG_(get_memory_from_mmap) ( cszW * sizeof(Word), 
                                    "newSuperblock" );
   sb->n_payload_words = cszW - 2;
   a->bytes_mmaped += cszW * sizeof(Word);
   if (0)
      VG_(message)(Vg_DebugMsg, "newSuperblock, %d payload words", 
                                sb->n_payload_words);
   return sb;
}


/* Find the superblock containing the given chunk. */
static
Superblock* findSb ( Arena* a, UInt* ch )
{
   Superblock* sb;
   for (sb = a->sblocks; sb; sb = sb->next)
      if (&sb->payload_words[0] <= ch
          && ch < &sb->payload_words[sb->n_payload_words]) 
         return sb;
   VG_(printf)("findSb: can't find pointer %p in arena `%s'\n",
               ch, a->name );
   VG_(core_panic)("findSb: vg_free() in wrong arena?");
   return NULL; /*NOTREACHED*/
}


/*------------------------------------------------------------*/
/*--- Low-level functions for working with blocks.         ---*/
/*------------------------------------------------------------*/

/* Add the not-in-use attribute to a bszW. */
static __inline__
Int mk_free_bszW ( Int bszW )
{
   vg_assert(bszW != 0);
   return (bszW < 0) ? bszW : -bszW;
}

/* Add the in-use attribute to a bszW. */
static __inline__
Int mk_inuse_bszW ( Int bszW )
{
   vg_assert(bszW != 0);
   return (bszW < 0) ? -bszW : bszW;
}

/* Remove the in-use/not-in-use attribute from a bszW, leaving just
   the size. */
static __inline__
Int mk_plain_bszW ( Int bszW )
{
   vg_assert(bszW != 0);
   return (bszW < 0) ? -bszW : bszW;
}

/* Does this bszW have the in-use attribute ? */
static __inline__
Bool is_inuse_bszW ( Int bszW )
{
   vg_assert(bszW != 0);
   return (bszW < 0) ? False : True;
}


/* Given the addr of the first word of a block, return the addr of the
   last word. */
static __inline__
WordL* first_to_last ( WordF* fw )
{
   return fw + mk_plain_bszW(fw[0]) - 1;
}

/* Given the addr of the last word of a block, return the addr of the
   first word. */
static __inline__
WordF* last_to_first ( WordL* lw )
{
   return lw - mk_plain_bszW(lw[0]) + 1;
}


/* Given the addr of the first word of a block, return the addr of the
   first word of its payload. */
static __inline__
Word* first_to_payload ( Arena* a, WordF* fw )
{
   return & fw[3 + a->rz_szW];
}

/* Given the addr of the first word of a the payload of a block,
   return the addr of the first word of the block. */
static __inline__
Word* payload_to_first ( Arena* a, WordF* payload )
{
   return & payload[- 3 - a->rz_szW];
}

/* Set and get the lower size field of a block. */
static __inline__
void set_bszW_lo ( WordF* fw, Int bszW ) { 
   fw[0] = bszW; 
}
static __inline__
Int get_bszW_lo ( WordF* fw )
{
   return fw[0];
}


/* Set and get the next and previous link fields of a block. */
static __inline__
void set_prev_p  ( WordF* fw, Word* prev_p ) { 
   fw[1] = (Word)prev_p; 
}
static __inline__
void set_next_p  ( WordF* fw, Word* next_p ) { 
   fw[2] = (Word)next_p; 
}
static __inline__
Word* get_prev_p  ( WordF* fw ) { 
   return (Word*)(fw[1]);
}
static __inline__
Word* get_next_p  ( WordF* fw ) { 
   return (Word*)(fw[2]);
}


/* Set and get the upper size field of a block. */
static __inline__
void set_bszW_hi ( WordF* fw, Int bszW ) {
   WordL* lw = first_to_last(fw);
   vg_assert(lw == fw + mk_plain_bszW(bszW) - 1);
   lw[0] = bszW;
}
static __inline__
Int get_bszW_hi ( WordF* fw ) {
   WordL* lw = first_to_last(fw);
   return lw[0];
}

/* Get the upper size field of a block, given a pointer to the last
   word of it. */
static __inline__
Int get_bszW_hi_from_last_word ( WordL* lw ) {
   WordF* fw = last_to_first(lw);
   return get_bszW_lo(fw);
}


/* Read and write the lower and upper red-zone words of a block. */
static __inline__
void set_rz_lo_word ( Arena* a, WordF* fw, Int rz_wordno, Word w )
{
   fw[3 + rz_wordno] = w;
}
static __inline__
void set_rz_hi_word ( Arena* a, WordF* fw, Int rz_wordno, Word w )
{
   WordL* lw = first_to_last(fw);
   lw[-1-rz_wordno] = w;
}
static __inline__
Word get_rz_lo_word ( Arena* a, WordF* fw, Int rz_wordno )
{
   return fw[3 + rz_wordno];
}
static __inline__
Word get_rz_hi_word ( Arena* a, WordF* fw, Int rz_wordno )
{
   WordL* lw = first_to_last(fw);
   return lw[-1-rz_wordno];
}


/* Return the lower, upper and total overhead in words for a block.
   These are determined purely by which arena the block lives in. */
static __inline__
Int overhead_szW_lo ( Arena* a )
{
   return 3 + a->rz_szW;
}
static __inline__
Int overhead_szW_hi ( Arena* a )
{
   return 1 + a->rz_szW;
}
static __inline__
Int overhead_szW ( Arena* a )
{
   return overhead_szW_lo(a) + overhead_szW_hi(a);
}


/* Convert pointer size in words to block size in words, and back. */
static __inline__
Int pszW_to_bszW ( Arena* a, Int pszW )
{
   vg_assert(pszW >= 0);
   return pszW + overhead_szW(a);
}
static __inline__
Int bszW_to_pszW ( Arena* a, Int bszW )
{
   Int pszW = bszW - overhead_szW(a);
   vg_assert(pszW >= 0);
   return pszW;
}

/*------------------------------------------------------------*/
/*--- Functions for working with freelists.                ---*/
/*------------------------------------------------------------*/

/* Determination of which freelist a block lives on is based on the
   payload size, not block size, in words. */

/* Convert a payload size in words to a freelist number. */

static
Int pszW_to_listNo ( Int pszW )
{
   vg_assert(pszW >= 0);
   if (pszW <= 3)   return 0;
   if (pszW <= 4)   return 1;
   if (pszW <= 5)   return 2;
   if (pszW <= 6)   return 3;
   if (pszW <= 7)   return 4;
   if (pszW <= 8)   return 5;
   if (pszW <= 9)   return 6;
   if (pszW <= 10)  return 7;
   if (pszW <= 11)  return 8;
   if (pszW <= 12)  return 9;
   if (pszW <= 16)  return 10;
   if (pszW <= 32)  return 11;
   if (pszW <= 64)  return 12;
   if (pszW <= 128) return 13;
   if (pszW <= 256) return 14;
   return 15;
}


/* What are the minimum and maximum payload sizes for a given list? */

static
Int listNo_to_pszW_min ( Int listNo )
{
   Int pszW = 0;
   vg_assert(listNo >= 0 && listNo <= VG_N_MALLOC_LISTS);
   while (pszW_to_listNo(pszW) < listNo) pszW++;
   return pszW;
}

static
Int listNo_to_pszW_max ( Int listNo )
{
   vg_assert(listNo >= 0 && listNo <= VG_N_MALLOC_LISTS);
   if (listNo == VG_N_MALLOC_LISTS-1) {
      return 999999999;
   } else {
      return listNo_to_pszW_min(listNo+1) - 1;
   }
}


/* A nasty hack to try and reduce fragmentation.  Try and replace
   a->freelist[lno] with another block on the same list but with a
   lower address, with the idea of attempting to recycle the same
   blocks rather than cruise through the address space. */

static 
void swizzle ( Arena* a, Int lno )
{
   UInt* p_best;
   UInt* pp;
   UInt* pn;
   Int   i;

   p_best = a->freelist[lno];
   if (p_best == NULL) return;

   pn = pp = p_best;
   for (i = 0; i < 20; i++) {
      pn = get_next_p(pn);
      pp = get_prev_p(pp);
      if (pn < p_best) p_best = pn;
      if (pp < p_best) p_best = pp;
   }
   if (p_best < a->freelist[lno]) {
#     ifdef DEBUG_MALLOC
      VG_(printf)("retreat by %d\n", 
           ((Char*)(a->freelist[lno])) - ((Char*)p_best));
#     endif
      a->freelist[lno] = p_best;
   }
}


/*------------------------------------------------------------*/
/*--- Creating and deleting blocks.                        ---*/
/*------------------------------------------------------------*/

/* Mark the words at b .. b+bszW-1 as not in use, and add them to the
   relevant free list. */

static
void mkFreeBlock ( Arena* a, Word* b, Int bszW, Int b_lno )
{
   Int pszW = bszW_to_pszW(a, bszW);
   vg_assert(pszW >= 0);
   vg_assert(b_lno == pszW_to_listNo(pszW));
   /* Set the size fields and indicate not-in-use. */
   set_bszW_lo(b, mk_free_bszW(bszW));
   set_bszW_hi(b, mk_free_bszW(bszW));

   /* Add to the relevant list. */
   if (a->freelist[b_lno] == NULL) {
      set_prev_p(b, b);
      set_next_p(b, b);
      a->freelist[b_lno] = b;
   } else {
      Word* b_prev = get_prev_p(a->freelist[b_lno]);
      Word* b_next = a->freelist[b_lno];
      set_next_p(b_prev, b);
      set_prev_p(b_next, b);
      set_next_p(b, b_next);
      set_prev_p(b, b_prev);
   }
#  ifdef DEBUG_MALLOC
   (void)blockSane(a,b);
#  endif
}


/* Mark the words at b .. b+bszW-1 as in use, and set up the block
   appropriately. */
static
void mkInuseBlock ( Arena* a, UInt* b, UInt bszW )
{
   Int i;
   set_bszW_lo(b, mk_inuse_bszW(bszW));
   set_bszW_hi(b, mk_inuse_bszW(bszW));
   set_prev_p(b, NULL);
   set_next_p(b, NULL);
   if (a->rz_check) {
      for (i = 0; i < a->rz_szW; i++) {
         set_rz_lo_word(a, b, i, (UInt)b ^ VG_REDZONE_LO_MASK);
         set_rz_hi_word(a, b, i, (UInt)b ^ VG_REDZONE_HI_MASK);
      }
   }
#  ifdef DEBUG_MALLOC
   (void)blockSane(a,b);
#  endif
}


/* Remove a block from a given list.  Does no sanity checking. */
static
void unlinkBlock ( Arena* a, UInt* b, Int listno )
{
   vg_assert(listno >= 0 && listno < VG_N_MALLOC_LISTS);
   if (get_prev_p(b) == b) {
      /* Only one element in the list; treat it specially. */
      vg_assert(get_next_p(b) == b);
      a->freelist[listno] = NULL;
   } else {
      UInt* b_prev = get_prev_p(b);
      UInt* b_next = get_next_p(b);
      a->freelist[listno] = b_prev;
      set_next_p(b_prev, b_next);
      set_prev_p(b_next, b_prev);
      swizzle ( a, listno );
   }
   set_prev_p(b, NULL);
   set_next_p(b, NULL);
}


/* Split an existing free block into two pieces, and put the fragment
   (the second one along in memory) onto the relevant free list.
   req_bszW is the required size of the block which isn't the
   fragment. */
static
void splitChunk ( Arena* a, UInt* b, Int b_listno, UInt req_bszW )
{
   Int b_bszW, frag_bszW;
   b_bszW = mk_plain_bszW(get_bszW_lo(b));
   vg_assert(req_bszW < b_bszW);
   frag_bszW = b_bszW - req_bszW;
   vg_assert(frag_bszW >= overhead_szW(a));
   /*
   printf( "split %d into %d and %d\n", 
                   b_bszW,req_bszW,frag_bszW  );
   */
   vg_assert(bszW_to_pszW(a, frag_bszW) > 0);
   unlinkBlock(a, b, b_listno);
   mkInuseBlock(a, b, req_bszW);
   mkFreeBlock(a, &b[req_bszW], frag_bszW, 
                  pszW_to_listNo(bszW_to_pszW(a, frag_bszW)));
}


/*------------------------------------------------------------*/
/*--- Sanity-check/debugging machinery.                    ---*/
/*------------------------------------------------------------*/

/* Do some crude sanity checks on a chunk. */
static 
Bool blockSane ( Arena* a, Word* b )
{
#  define BLEAT(str) VG_(printf)("blockSane: fail -- %s\n",str)
   Int i;
   if (get_bszW_lo(b) != get_bszW_hi(b)) 
      {BLEAT("sizes");return False;}
   if (a->rz_check && is_inuse_bszW(get_bszW_lo(b))) {
      for (i = 0; i < a->rz_szW; i++) {
         if (get_rz_lo_word(a, b, i) != ((Word)b ^ VG_REDZONE_LO_MASK))
            {BLEAT("redzone-lo");return False;}
         if (get_rz_hi_word(a, b, i) != ((Word)b ^ VG_REDZONE_HI_MASK))
            {BLEAT("redzone-hi");return False;}
      }      
   }
   return True;
#  undef BLEAT
}


/* Print superblocks (only for debugging). */
static 
void ppSuperblocks ( Arena* a )
{
   Int i, ch_bszW, blockno;
   UInt* ch;
   Superblock* sb = a->sblocks;
   blockno = 1;

   while (sb) {
      VG_(printf)( "\n" );
      VG_(printf)( "superblock %d at %p, sb->n_pl_ws = %d, next = %p\n", 
                   blockno++, sb, sb->n_payload_words, sb->next );
      i = 0;
      while (True) {
         if (i >= sb->n_payload_words) break;
         ch     = &sb->payload_words[i];
         ch_bszW = get_bszW_lo(ch);
         VG_(printf)( "   block at %d, bszW %d: ", i, mk_plain_bszW(ch_bszW) );
         VG_(printf)( "%s, ", is_inuse_bszW(ch_bszW) ? "inuse" : "free" );
         VG_(printf)( "%s\n", blockSane(a,ch) ? "ok" : "BAD" );
         i += mk_plain_bszW(ch_bszW);
      }
      if (i > sb->n_payload_words) 
         VG_(printf)( "   last block overshoots end of SB\n");
      sb = sb->next;
   }
   VG_(printf)( "end of superblocks\n\n" );
}


/* Sanity check both the superblocks and the chains. */
static void mallocSanityCheckArena ( ArenaId aid )
{
   Int         i, superblockctr, b_bszW, b_pszW, blockctr_sb, blockctr_li;
   Int         blockctr_sb_free, listno, list_min_pszW, list_max_pszW;
   Superblock* sb;
   Bool        thisFree, lastWasFree;
   Word*       b;
   Word*       b_prev;
   UInt        arena_bytes_on_loan;
   Arena*      a;

#  define BOMB VG_(core_panic)("mallocSanityCheckArena")

   a = arenaId_to_ArenaP(aid);
   
   /* First, traverse all the superblocks, inspecting the chunks in
      each. */
   superblockctr = blockctr_sb = blockctr_sb_free = 0;
   arena_bytes_on_loan = 0;
   sb = a->sblocks;
   while (sb) {
      lastWasFree = False;
      superblockctr++;
      i = 0;
      while (True) {
         if (i >= sb->n_payload_words) break;
         blockctr_sb++;
         b     = &sb->payload_words[i];
         b_bszW = get_bszW_lo(b);
         if (!blockSane(a, b)) {
            VG_(printf)("mallocSanityCheckArena: sb %p, block %d (bszW %d): "
                        " BAD\n",
                         sb, i, b_bszW );
            BOMB;
         }
         thisFree = !is_inuse_bszW(b_bszW);
         if (thisFree && lastWasFree) {
            VG_(printf)("mallocSanityCheckArena: sb %p, block %d (bszW %d): "
                        "UNMERGED FREES\n",
                         sb, i, b_bszW );
            BOMB;
         }
         lastWasFree = thisFree;
         if (thisFree) blockctr_sb_free++;
         if (!thisFree) 
            arena_bytes_on_loan += sizeof(Word) * bszW_to_pszW(a, b_bszW);
         i += mk_plain_bszW(b_bszW);
      }
      if (i > sb->n_payload_words) {
         VG_(printf)( "mallocSanityCheckArena: sb %p: last block "
                      "overshoots end\n", sb);
         BOMB;
      }
      sb = sb->next;
   }

   if (arena_bytes_on_loan != a->bytes_on_loan) {
            VG_(printf)( 
                    "mallocSanityCheckArena: a->bytes_on_loan %d, "
                    "arena_bytes_on_loan %d: "
                    "MISMATCH\n", a->bytes_on_loan, arena_bytes_on_loan);
      ppSuperblocks(a);
      BOMB;
   }

   /* Second, traverse each list, checking that the back pointers make
      sense, counting blocks encountered, and checking that each block
      is an appropriate size for this list. */
   blockctr_li = 0;
   for (listno = 0; listno < VG_N_MALLOC_LISTS; listno++) {
      list_min_pszW = listNo_to_pszW_min(listno);
      list_max_pszW = listNo_to_pszW_max(listno);
      b = a->freelist[listno];
      if (b == NULL) continue;
      while (True) {
         b_prev = b;
         b = get_next_p(b);
         if (get_prev_p(b) != b_prev) {
            VG_(printf)( "mallocSanityCheckArena: list %d at %p: "
                         "BAD LINKAGE\n", 
                         listno, b );
            BOMB;
         }
         b_pszW = bszW_to_pszW(a, mk_plain_bszW(get_bszW_lo(b)));
         if (b_pszW < list_min_pszW || b_pszW > list_max_pszW) {
            VG_(printf)( 
               "mallocSanityCheckArena: list %d at %p: "
               "WRONG CHAIN SIZE %d (%d, %d)\n", 
               listno, b, b_pszW, list_min_pszW, list_max_pszW );
            BOMB;
         }
         blockctr_li++;
         if (b == a->freelist[listno]) break;
      }
   }

   if (blockctr_sb_free != blockctr_li) {
      VG_(printf)( 
         "mallocSanityCheckArena: BLOCK COUNT MISMATCH "
         "(via sbs %d, via lists %d)\n",
         blockctr_sb_free, blockctr_li );
      ppSuperblocks(a);
      BOMB;
   }

   VG_(message)(Vg_DebugMsg,
                "mSC [%8s]: %2d sbs, %5d tot bs, %4d/%-4d free bs, "
                "%2d lists, %7d mmap, %7d loan", 
                a->name,
                superblockctr,
                blockctr_sb, blockctr_sb_free, blockctr_li, 
                VG_N_MALLOC_LISTS, 
                a->bytes_mmaped, a->bytes_on_loan);   
#  undef BOMB
}


void VG_(mallocSanityCheckAll) ( void )
{
   Int i;
   for (i = 0; i < VG_N_ARENAS; i++)
      mallocSanityCheckArena ( i );
}


/* Really, this isn't the right place for this.  Nevertheless: find
   out if an arena is empty -- currently has no bytes on loan.  This
   is useful for checking for memory leaks (of valgrind, not the
   client.) 
*/
Bool VG_(is_empty_arena) ( ArenaId aid )
{
   Arena*      a;
   Superblock* sb;
   WordF*      b;
   Int         b_bszW;

   ensure_mm_init();
   a = arenaId_to_ArenaP(aid);
   for (sb = a->sblocks; sb != NULL; sb = sb->next) {
      /* If the superblock is empty, it should contain a single free
         block, of the right size. */
      b = &(sb->payload_words[0]);
      b_bszW = get_bszW_lo(b);
      if (is_inuse_bszW(b_bszW)) return False;
      if (mk_plain_bszW(b_bszW) != sb->n_payload_words) return False;
      /* So this block is not in use and is of the right size.  Keep
         going. */
   }
   return True;
}


/*------------------------------------------------------------*/
/*--- Core-visible functions.                              ---*/
/*------------------------------------------------------------*/

void* VG_(arena_malloc) ( ArenaId aid, Int req_pszB )
{
   Int         req_pszW, req_bszW, frag_bszW, b_bszW, lno;
   Superblock* new_sb;
   Word*       b;
   Arena*      a;

   VGP_PUSHCC(VgpMalloc);

   ensure_mm_init();
   a = arenaId_to_ArenaP(aid);

   vg_assert(req_pszB >= 0);
   vg_assert(req_pszB < 0x7FFFFFF0);

   req_pszW = (req_pszB + VKI_BYTES_PER_WORD - 1) / VKI_BYTES_PER_WORD;

   /* Keep gcc -O happy: */
   b = NULL;

   /* Start searching at this list. */
   lno = pszW_to_listNo(req_pszW);

   /* This loop finds a list which has a block big enough, or sets
      req_listno to N_LISTS if no such block exists. */
   while (True) {
      if (lno == VG_N_MALLOC_LISTS) break;
      /* If this list is empty, try the next one. */
      if (a->freelist[lno] == NULL) {
         lno++;
         continue;
      }
      /* Scan a->list[lno] to find a big-enough chunk. */
      b = a->freelist[lno];
      b_bszW = mk_plain_bszW(get_bszW_lo(b));
      while (True) {
         if (bszW_to_pszW(a, b_bszW) >= req_pszW) break;
         b = get_next_p(b);
         b_bszW = mk_plain_bszW(get_bszW_lo(b));
         if (b == a->freelist[lno]) break;
      }
      if (bszW_to_pszW(a, b_bszW) >= req_pszW) break;
      /* No luck?  Try a larger list. */
      lno++;
   }

   /* Either lno < VG_N_MALLOC_LISTS and b points to the selected
      block, or lno == VG_N_MALLOC_LISTS, and we have to allocate a
      new superblock. */

   if (lno == VG_N_MALLOC_LISTS) {
      req_bszW = pszW_to_bszW(a, req_pszW);      
      new_sb = newSuperblock(a, req_bszW);
      vg_assert(new_sb != NULL);
      new_sb->next = a->sblocks;
      a->sblocks = new_sb;
      b = &(new_sb->payload_words[0]);
      lno = pszW_to_listNo(bszW_to_pszW(a, new_sb->n_payload_words));
      mkFreeBlock ( a, b, new_sb->n_payload_words, lno);
   }

   /* Ok, we can allocate from b, which lives in list req_listno. */
   vg_assert(b != NULL);
   vg_assert(lno >= 0 && lno < VG_N_MALLOC_LISTS);
   vg_assert(a->freelist[lno] != NULL);
   b_bszW = mk_plain_bszW(get_bszW_lo(b));
   req_bszW = pszW_to_bszW(a, req_pszW);
   /* req_bszW is the size of the block we are after.  b_bszW is the
      size of what we've actually got. */
   vg_assert(b_bszW >= req_bszW);

   /* Could we split this block and still get a useful fragment?
      Where "useful" means that the payload size of the frag is at
      least one word.  */
   frag_bszW = b_bszW - req_bszW;
   if (frag_bszW > overhead_szW(a)) {
      splitChunk(a, b, lno, req_bszW);
   } else {
      /* No, mark as in use and use as-is. */
      unlinkBlock(a, b, lno);
      /*
      set_bszW_lo(b, mk_inuse_bszW(b_bszW));
      set_bszW_hi(b, mk_inuse_bszW(b_bszW));
      */
      mkInuseBlock(a, b, b_bszW);
   }
   vg_assert(req_bszW <= mk_plain_bszW(get_bszW_lo(b)));

   a->bytes_on_loan 
      += sizeof(Word) 
         * bszW_to_pszW(a, mk_plain_bszW(get_bszW_lo(b)));
   if (a->bytes_on_loan > a->bytes_on_loan_max)
      a->bytes_on_loan_max = a->bytes_on_loan;

#  ifdef DEBUG_MALLOC
   mallocSanityCheckArena(aid);
#  endif

   VGP_POPCC(VgpMalloc);
   return first_to_payload(a, b);
}

 
void VG_(arena_free) ( ArenaId aid, void* ptr )
{
   Superblock* sb;
   UInt*       sb_payl_firstw;
   UInt*       sb_payl_lastw;
   UInt*       other;
   UInt*       ch;
   Int         ch_bszW, ch_pszW, other_bszW, ch_listno;
   Arena*      a;

   VGP_PUSHCC(VgpMalloc);

   ensure_mm_init();
   a = arenaId_to_ArenaP(aid);

   if (ptr == NULL) {
      VGP_POPCC(VgpMalloc);
      return;
   }
      
   ch = payload_to_first(a, ptr);

#  ifdef DEBUG_MALLOC
   vg_assert(blockSane(a,ch));
#  endif

   a->bytes_on_loan 
      -= sizeof(Word) 
         * bszW_to_pszW(a, mk_plain_bszW(get_bszW_lo(ch)));

   sb             = findSb( a, ch );
   sb_payl_firstw = &(sb->payload_words[0]);
   sb_payl_lastw  = &(sb->payload_words[sb->n_payload_words-1]);

   /* Put this chunk back on a list somewhere. */
   ch_bszW    = get_bszW_lo(ch);
   ch_pszW    = bszW_to_pszW(a, ch_bszW);
   ch_listno  = pszW_to_listNo(ch_pszW);
   mkFreeBlock( a, ch, ch_bszW, ch_listno );

   /* See if this block can be merged with the following one. */
   other = ch + ch_bszW;
   /* overhead_szW(a) is the smallest possible bszW for this arena.
      So the nearest possible end to the block beginning at other is
      other+overhead_szW(a)-1.  Hence the test below. */
   if (other+overhead_szW(a)-1 <= sb_payl_lastw) {
      other_bszW = get_bszW_lo(other);
      if (!is_inuse_bszW(other_bszW)) {
         /* VG_(printf)( "merge-successor\n"); */
         other_bszW = mk_plain_bszW(other_bszW);
#        ifdef DEBUG_MALLOC
         vg_assert(blockSane(a, other));
#        endif
         unlinkBlock( a, ch, ch_listno );
         unlinkBlock( a, other, pszW_to_listNo(bszW_to_pszW(a,other_bszW)) );
         ch_bszW += other_bszW; 
         ch_listno = pszW_to_listNo(bszW_to_pszW(a, ch_bszW));
         mkFreeBlock( a, ch, ch_bszW, ch_listno );
      }
   }

   /* See if this block can be merged with its predecessor. */
   if (ch-overhead_szW(a) >= sb_payl_firstw) {
      other_bszW = get_bszW_hi_from_last_word( ch-1 );
      if (!is_inuse_bszW(other_bszW)) {
         /* VG_(printf)( "merge-predecessor\n"); */
         other = last_to_first( ch-1 );
         other_bszW = mk_plain_bszW(other_bszW);         
         unlinkBlock( a, ch, ch_listno );
         unlinkBlock( a, other, pszW_to_listNo(bszW_to_pszW(a, other_bszW)) );
         ch = other;
         ch_bszW += other_bszW;
         ch_listno = pszW_to_listNo(bszW_to_pszW(a, ch_bszW));
         mkFreeBlock( a, ch, ch_bszW, ch_listno );
      }
   }

#  ifdef DEBUG_MALLOC
   mallocSanityCheckArena(aid);
#  endif

   VGP_POPCC(VgpMalloc);
}


/*
   The idea for malloc_aligned() is to allocate a big block, base, and
   then split it into two parts: frag, which is returned to the the
   free pool, and align, which is the bit we're really after.  Here's
   a picture.  L and H denote the block lower and upper overheads, in
   words.  The details are gruesome.  Note it is slightly complicated
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
   <------ frag_bszW ------->   .               .   .
   .    <------------- base_pszW_act ----------->   .
   .    .               .   .   .               .   .

*/
void* VG_(arena_malloc_aligned) ( ArenaId aid, Int req_alignB, Int req_pszB )
{
   Int    req_alignW, req_pszW, base_pszW_req, base_pszW_act, frag_bszW;
   Word   *base_b, *base_p, *align_p;
   UInt   saved_bytes_on_loan;
   Arena* a;

   VGP_PUSHCC(VgpMalloc);

   ensure_mm_init();
   a = arenaId_to_ArenaP(aid);

   vg_assert(req_pszB >= 0);
   vg_assert(req_pszB < 0x7FFFFFF0);

   /* Check that the requested alignment seems reasonable; that is, is
      a power of 2.  */
   switch (req_alignB) {
      case 4:
      case 8: case 16: case 32: case 64: case 128: case 256: 
      case 512: case 1024: case 2048: case 4096: case 8192: 
      case 16384: case 32768: case 65536: case 131072: 
      case 262144:
      case 1048576: 
         /* can't be bothered to calculate larger ones */
         break;
      default:
         VG_(printf)("vg_malloc_aligned(%p, %d, %d)\nbad alignment request", 
                     a, req_alignB, req_pszB );
         VG_(core_panic)("vg_malloc_aligned");
         /*NOTREACHED*/
   }

   /* Required alignment, in words.  Since it's constrained to be a
      power of 2 >= word size, no need to align the alignment.  Still,
      we check. */
   req_alignW = req_alignB / VKI_BYTES_PER_WORD;
   vg_assert(req_alignB == req_alignW * VKI_BYTES_PER_WORD);

   /* Required payload size for the aligned chunk. */
   req_pszW = (req_pszB + VKI_BYTES_PER_WORD - 1) / VKI_BYTES_PER_WORD;
   
   /* Payload size to request for the big block that we will split
      up. */
   base_pszW_req = req_pszW + overhead_szW(a) + req_alignW;

   /* Payload ptr for the block we are going to split.  Note this
      changes a->bytes_on_loan; we save and restore it ourselves. */
   saved_bytes_on_loan = a->bytes_on_loan;
   base_p = VG_(arena_malloc) ( aid, base_pszW_req * VKI_BYTES_PER_WORD );
   a->bytes_on_loan = saved_bytes_on_loan;

   /* Block ptr for the block we are going to split. */
   base_b = payload_to_first ( a, base_p );

   /* Pointer to the payload of the aligned block we are going to
      return.  This has to be suitably aligned. */
   align_p = align_upwards ( base_b + 2 * overhead_szW_lo(a) 
                                    + overhead_szW_hi(a),
                             req_alignB );

   /* The block size of the fragment we will create.  This must be big
      enough to actually create a fragment. */
   frag_bszW = align_p - overhead_szW_lo(a) - base_b;
   vg_assert(frag_bszW >= overhead_szW(a));

   /* The actual payload size of the block we are going to split. */
   base_pszW_act = bszW_to_pszW(a, mk_plain_bszW(get_bszW_lo(base_b)));

   /* Create the fragment block, and put it back on the relevant free
      list. */
   mkFreeBlock ( a, base_b, frag_bszW, 
                 pszW_to_listNo(bszW_to_pszW(a, frag_bszW)) );

   /* Create the aligned block. */
   mkInuseBlock ( a,
                  align_p - overhead_szW_lo(a), 
                  base_p + base_pszW_act 
                         + overhead_szW_hi(a) 
                         - (align_p - overhead_szW_lo(a)) );

   /* Final sanity checks. */
   vg_assert(( (UInt)align_p % req_alignB) == 0);

   vg_assert(is_inuse_bszW(get_bszW_lo(payload_to_first(a, align_p))));

   vg_assert(req_pszW 
             <= 
             bszW_to_pszW(a, mk_plain_bszW(get_bszW_lo(
                payload_to_first(a, align_p))))
            );

   a->bytes_on_loan 
      += sizeof(Word)
         * bszW_to_pszW(a, mk_plain_bszW(get_bszW_lo(
              payload_to_first(a, align_p))));
   if (a->bytes_on_loan > a->bytes_on_loan_max)
      a->bytes_on_loan_max = a->bytes_on_loan;

#  ifdef DEBUG_MALLOC
   mallocSanityCheckArena(aid);
#  endif

   VGP_POPCC(VgpMalloc);

   return align_p;
}


/*------------------------------------------------------------*/
/*--- Services layered on top of malloc/free.              ---*/
/*------------------------------------------------------------*/

void* VG_(arena_calloc) ( ArenaId aid, Int alignB, Int nmemb, Int nbytes )
{
   Int    i, size;
   UChar* p;

   VGP_PUSHCC(VgpMalloc);

   size = nmemb * nbytes;
   vg_assert(size >= 0);

   if (alignB == 4)
      p = VG_(arena_malloc) ( aid, size );
   else
      p = VG_(arena_malloc_aligned) ( aid, alignB, size );

   for (i = 0; i < size; i++) p[i] = 0;

   VGP_POPCC(VgpMalloc);
   
   return p;
}


void* VG_(arena_realloc) ( ArenaId aid, void* ptr, 
                          Int req_alignB, Int req_pszB )
{
   Arena* a;
   Int    old_bszW, old_pszW, old_pszB, i;
   UChar  *p_old, *p_new;
   UInt*  ch;

   VGP_PUSHCC(VgpMalloc);

   ensure_mm_init();
   a = arenaId_to_ArenaP(aid);

   vg_assert(req_pszB >= 0);
   vg_assert(req_pszB < 0x7FFFFFF0);

   ch = payload_to_first(a, ptr);
   vg_assert(blockSane(a, ch));

   old_bszW = get_bszW_lo(ch);
   vg_assert(is_inuse_bszW(old_bszW));
   old_bszW = mk_plain_bszW(old_bszW);
   old_pszW = bszW_to_pszW(a, old_bszW);
   old_pszB = old_pszW * VKI_BYTES_PER_WORD;

   if (req_pszB <= old_pszB) {
      VGP_POPCC(VgpMalloc);
      return ptr;
   }

   if (req_alignB == 4)
      p_new = VG_(arena_malloc) ( aid, req_pszB );
   else
      p_new = VG_(arena_malloc_aligned) ( aid, req_alignB, req_pszB );

   p_old = (UChar*)ptr;
   for (i = 0; i < old_pszB; i++)
      p_new[i] = p_old[i];

   VG_(arena_free)(aid, p_old);

   VGP_POPCC(VgpMalloc);
   return p_new;
}


/*------------------------------------------------------------*/
/*--- Skin-visible functions.                              ---*/
/*------------------------------------------------------------*/

/* All just wrappers to avoid exposing arenas to skins */

void* VG_(malloc) ( Int nbytes )
{
   return VG_(arena_malloc) ( VG_AR_SKIN, nbytes );
}

void  VG_(free) ( void* ptr )
{
   VG_(arena_free) ( VG_AR_SKIN, ptr );
}

void* VG_(calloc) ( Int nmemb, Int nbytes )
{
   return VG_(arena_calloc) ( VG_AR_SKIN, /*alignment*/4, nmemb, nbytes );
}

void* VG_(realloc) ( void* ptr, Int size )
{
   return VG_(arena_realloc) ( VG_AR_SKIN, ptr, /*alignment*/4, size );
}

void* VG_(malloc_aligned) ( Int req_alignB, Int req_pszB )
{
   return VG_(arena_malloc_aligned) ( VG_AR_SKIN, req_alignB, req_pszB );
}


void* VG_(cli_malloc) ( UInt align, Int nbytes )                 
{                                                                             
   vg_assert(align >= 4);                                                     
   if (4 == align)                                                            
      return VG_(arena_malloc)         ( VG_AR_CLIENT, nbytes ); 
   else                                                                       
      return VG_(arena_malloc_aligned) ( VG_AR_CLIENT, align, nbytes );                            
}                                                                             

void VG_(cli_free) ( void* p )                                   
{                                                                             
   VG_(arena_free) ( VG_AR_CLIENT, p );                          
}


Bool VG_(addr_is_in_block)( Addr a, Addr start, UInt size )
{  
   return (start - VG_(vg_malloc_redzone_szB) <= a
           && a < start + size + VG_(vg_malloc_redzone_szB));
}


/*------------------------------------------------------------*/
/*--- The original test driver machinery.                  ---*/
/*------------------------------------------------------------*/

#if 0

#if 1
#define N_TEST_TRANSACTIONS 100000000
#define N_TEST_ARR 200000
#define M_TEST_MALLOC 1000
#else
#define N_TEST_TRANSACTIONS 500000
#define N_TEST_ARR 30000
#define M_TEST_MALLOC 500
#endif


void* test_arr[N_TEST_ARR];

int main ( int argc, char** argv )
{
   Int i, j, k, nbytes, qq;
   unsigned char* chp;
   Arena* a = &arena[VG_AR_CORE];
   srandom(1);
   for (i = 0; i < N_TEST_ARR; i++)
      test_arr[i] = NULL;

   for (i = 0; i < N_TEST_TRANSACTIONS; i++) {
      if (i % 50000 == 0) mallocSanityCheck(a);
      j = random() % N_TEST_ARR;
      if (test_arr[j]) {
         vg_free(a, test_arr[j]);
         test_arr[j] = NULL;
      } else {
         nbytes = 1 + random() % M_TEST_MALLOC;
         qq = random()%64;
         if (qq == 32) 
            nbytes *= 17;
         else if (qq == 33)
            nbytes = 0;
         test_arr[j] 
           = (i % 17) == 0
                ? vg_memalign(a, nbytes, 1<< (3+(random()%10)))
                : vg_malloc( a, nbytes );
         chp = test_arr[j];
         for (k = 0; k < nbytes; k++) 
            chp[k] = (unsigned char)(k + 99);
      }
   }


   for (i = 0; i < N_TEST_ARR; i++) {
      if (test_arr[i]) {
         vg_free(a, test_arr[i]);
         test_arr[i] = NULL;
      }
   }
   mallocSanityCheck(a);

   fprintf(stderr, "ALL DONE\n");

   show_arena_stats(a);
   fprintf(stderr, "%d max useful, %d bytes mmap'd (%4.1f%%), %d useful\n",
           a->bytes_on_loan_max, 
           a->bytes_mmaped, 
	   100.0 * (double)a->bytes_on_loan_max / (double)a->bytes_mmaped,
           a->bytes_on_loan );

   return 0;
}
#endif /* 0 */


/*--------------------------------------------------------------------*/
/*--- end                                             vg_malloc2.c ---*/
/*--------------------------------------------------------------------*/
