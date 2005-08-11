
/*--------------------------------------------------------------------*/
/*--- malloc/free wrappers for detecting errors and updating bits. ---*/
/*---                                        mac_malloc_wrappers.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of MemCheck, a heavyweight Valgrind tool for
   detecting memory errors, and AddrCheck, a lightweight Valgrind tool 
   for detecting memory errors.

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

#include "pub_tool_basics.h"
#include "pub_tool_errormgr.h"      // For mac_shared.h
#include "pub_tool_execontext.h"    // For mac_shared.h
#include "pub_tool_hashtable.h"     // For mac_shared.h
#include "pub_tool_libcbase.h"
#include "pub_tool_libcassert.h"
#include "pub_tool_libcprint.h"
#include "pub_tool_mallocfree.h"
#include "pub_tool_options.h"
#include "pub_tool_profile.h"       // For mac_shared.h
#include "pub_tool_replacemalloc.h"
#include "pub_tool_threadstate.h"
#include "mac_shared.h"

/*------------------------------------------------------------*/
/*--- Defns                                                ---*/
/*------------------------------------------------------------*/

/* Stats ... */
static SizeT cmalloc_n_mallocs  = 0;
static SizeT cmalloc_n_frees    = 0;
static SizeT cmalloc_bs_mallocd = 0;

/* Function pointers for the two tools to track interesting events. */
void (*MAC_(new_mem_heap)) ( Addr a, SizeT len, Bool is_inited )  = NULL;
void (*MAC_(ban_mem_heap)) ( Addr a, SizeT len )                  = NULL;
void (*MAC_(die_mem_heap)) ( Addr a, SizeT len )                  = NULL;
void (*MAC_(copy_mem_heap))( Addr from, Addr to, SizeT len )      = NULL;

/* Function pointers for internal sanity checking. */
Bool (*MAC_(check_noaccess))( Addr a, SizeT len, Addr* bad_addr ) = NULL;


/*------------------------------------------------------------*/
/*--- Tracking malloc'd and free'd blocks                  ---*/
/*------------------------------------------------------------*/

/* Record malloc'd blocks.  Nb: Addrcheck and Memcheck construct this
   separately in their respective initialisation functions. */
VgHashTable MAC_(malloc_list) = NULL;

/* Memory pools.  Nb: Addrcheck and Memcheck construct this separately
   in their respective initialisation functions. */
VgHashTable MAC_(mempool_list) = NULL;
   
/* Records blocks after freeing. */
static MAC_Chunk* freed_list_start  = NULL;
static MAC_Chunk* freed_list_end    = NULL;
static Int        freed_list_volume = 0;

/* Put a shadow chunk on the freed blocks queue, possibly freeing up
   some of the oldest blocks in the queue at the same time. */
static void add_to_freed_queue ( MAC_Chunk* mc )
{
   MAC_Chunk* sc1;

   /* Put it at the end of the freed list */
   if (freed_list_end == NULL) {
      tl_assert(freed_list_start == NULL);
      freed_list_end    = freed_list_start = mc;
      freed_list_volume = mc->size;
   } else {
      tl_assert(freed_list_end->next == NULL);
      freed_list_end->next = mc;
      freed_list_end       = mc;
      freed_list_volume += mc->size;
   }
   mc->next = NULL;

   /* Release enough of the oldest blocks to bring the free queue
      volume below vg_clo_freelist_vol. */

   while (freed_list_volume > MAC_(clo_freelist_vol)) {
      tl_assert(freed_list_start != NULL);
      tl_assert(freed_list_end != NULL);

      sc1 = freed_list_start;
      freed_list_volume -= sc1->size;
      /* VG_(printf)("volume now %d\n", freed_list_volume); */
      tl_assert(freed_list_volume >= 0);

      if (freed_list_start == freed_list_end) {
         freed_list_start = freed_list_end = NULL;
      } else {
         freed_list_start = sc1->next;
      }
      sc1->next = NULL; /* just paranoia */

      /* free MAC_Chunk */
      VG_(cli_free) ( (void*)(sc1->data) );
      VG_(free) ( sc1 );
   }
}

/* Return the first shadow chunk satisfying the predicate p. */
MAC_Chunk* MAC_(first_matching_freed_MAC_Chunk) ( Bool (*p)(MAC_Chunk*, void*),
                                                  void* d )
{
   MAC_Chunk* mc;

   /* No point looking through freed blocks if we're not keeping
      them around for a while... */
   for (mc = freed_list_start; mc != NULL; mc = mc->next)
      if (p(mc, d))
         return mc;

   return NULL;
}

/* Allocate its shadow chunk, put it on the appropriate list. */
static
void add_MAC_Chunk ( ThreadId tid,
                     Addr p, SizeT size, MAC_AllocKind kind, VgHashTable table)
{
   MAC_Chunk* mc;

   mc            = VG_(malloc)(sizeof(MAC_Chunk));
   mc->data      = p;
   mc->size      = size;
   mc->allockind = kind;
   mc->where     = VG_(record_ExeContext)(tid);

   /* Paranoia ... ensure this area is off-limits to the client, so
      the mc->data field isn't visible to the leak checker.  If memory
      management is working correctly, anything pointer returned by
      VG_(malloc) should be noaccess as far as the client is
      concerned. */
   if (!MAC_(check_noaccess)( (Addr)mc, sizeof(MAC_Chunk), NULL )) {
      VG_(tool_panic)("add_MAC_Chunk: shadow area is accessible");
   } 

   VG_(HT_add_node)( table, (VgHashNode*)mc );
}

/*------------------------------------------------------------*/
/*--- client_malloc(), etc                                 ---*/
/*------------------------------------------------------------*/

static Bool complain_about_silly_args(SizeT sizeB, Char* fn)
{
   // Cast to a signed type to catch any unexpectedly negative args.  We're
   // assuming here that the size asked for is not greater than 2^31 bytes
   // (for 32-bit platforms) or 2^63 bytes (for 64-bit platforms).
   if ((SSizeT)sizeB < 0) {
      VG_(message)(Vg_UserMsg, "Warning: silly arg (%d) to %s()", sizeB, fn );
      return True;
   }
   return False;
}

static Bool complain_about_silly_args2(SizeT n, SizeT sizeB)
{
   if ((SSizeT)n < 0 || (SSizeT)sizeB < 0) {
      VG_(message)(Vg_UserMsg, "Warning: silly args (%d,%d) to calloc()",
                   n, sizeB);
      return True;
   }
   return False;
}

/* Allocate memory and note change in memory available */
__inline__
void* MAC_(new_block) ( ThreadId tid,
                        Addr p, SizeT size, SizeT align, UInt rzB,
                        Bool is_zeroed, MAC_AllocKind kind, VgHashTable table)
{
   VGP_PUSHCC(VgpCliMalloc);
   cmalloc_n_mallocs ++;

   // Allocate and zero if necessary
   if (p) {
      tl_assert(MAC_AllocCustom == kind);
   } else {
      tl_assert(MAC_AllocCustom != kind);
      p = (Addr)VG_(cli_malloc)( align, size );
      if (!p) {
         VGP_POPCC(VgpCliMalloc);
         return NULL;
      }
      if (is_zeroed) VG_(memset)((void*)p, 0, size);
   }

   // Only update this stat if allocation succeeded.
   cmalloc_bs_mallocd += size;

   add_MAC_Chunk( tid, p, size, kind, table );

   MAC_(ban_mem_heap)( p-rzB, rzB );
   MAC_(new_mem_heap)( p, size, is_zeroed );
   MAC_(ban_mem_heap)( p+size, rzB );

   VGP_POPCC(VgpCliMalloc);

   return (void*)p;
}

void* MAC_(malloc) ( ThreadId tid, SizeT n )
{
   if (complain_about_silly_args(n, "malloc")) {
      return NULL;
   } else {
      return MAC_(new_block) ( tid, 0, n, VG_(clo_alignment), 
         MAC_MALLOC_REDZONE_SZB, /*is_zeroed*/False, MAC_AllocMalloc,
         MAC_(malloc_list));
   }
}

void* MAC_(__builtin_new) ( ThreadId tid, SizeT n )
{
   if (complain_about_silly_args(n, "__builtin_new")) {
      return NULL;
   } else {
      return MAC_(new_block) ( tid, 0, n, VG_(clo_alignment), 
         MAC_MALLOC_REDZONE_SZB, /*is_zeroed*/False, MAC_AllocNew,
         MAC_(malloc_list));
   }
}

void* MAC_(__builtin_vec_new) ( ThreadId tid, SizeT n )
{
   if (complain_about_silly_args(n, "__builtin_vec_new")) {
      return NULL;
   } else {
      return MAC_(new_block) ( tid, 0, n, VG_(clo_alignment), 
         MAC_MALLOC_REDZONE_SZB, /*is_zeroed*/False, MAC_AllocNewVec,
         MAC_(malloc_list));
   }
}

void* MAC_(memalign) ( ThreadId tid, SizeT align, SizeT n )
{
   if (complain_about_silly_args(n, "memalign")) {
      return NULL;
   } else {
      return MAC_(new_block) ( tid, 0, n, align, 
         MAC_MALLOC_REDZONE_SZB, /*is_zeroed*/False, MAC_AllocMalloc,
         MAC_(malloc_list));
   }
}

void* MAC_(calloc) ( ThreadId tid, SizeT nmemb, SizeT size1 )
{
   if (complain_about_silly_args2(nmemb, size1)) {
      return NULL;
   } else {
      return MAC_(new_block) ( tid, 0, nmemb*size1, VG_(clo_alignment),
         MAC_MALLOC_REDZONE_SZB, /*is_zeroed*/True, MAC_AllocMalloc,
         MAC_(malloc_list));
   }
}

static
void die_and_free_mem ( ThreadId tid, MAC_Chunk* mc, SizeT rzB )
{
   /* Note: ban redzones again -- just in case user de-banned them
      with a client request... */
   MAC_(ban_mem_heap)( mc->data-rzB, rzB );
   MAC_(die_mem_heap)( mc->data, mc->size );
   MAC_(ban_mem_heap)( mc->data+mc->size, rzB );

   /* Put it out of harm's way for a while, if not from a client request */
   if (MAC_AllocCustom != mc->allockind) {
      /* Record where freed */
      mc->where = VG_(record_ExeContext) ( tid );
      add_to_freed_queue ( mc );
   } else {
      VG_(free) ( mc );
   }
}

__inline__
void MAC_(handle_free) ( ThreadId tid, Addr p, UInt rzB, MAC_AllocKind kind )
{
   MAC_Chunk*  mc;

   VGP_PUSHCC(VgpCliMalloc);

   cmalloc_n_frees++;

   mc = (MAC_Chunk*)VG_(HT_remove) ( MAC_(malloc_list), (UWord)p );
   if (mc == NULL) {
      MAC_(record_free_error) ( tid, p );
      VGP_POPCC(VgpCliMalloc);
      return;
   }

   /* check if its a matching free() / delete / delete [] */
   if (kind != mc->allockind) {
      MAC_(record_freemismatch_error) ( tid, p, mc );
   }

   die_and_free_mem ( tid, mc, rzB );

   VGP_POPCC(VgpCliMalloc);
}

void MAC_(free) ( ThreadId tid, void* p )
{
   MAC_(handle_free)( 
      tid, (Addr)p, MAC_MALLOC_REDZONE_SZB, MAC_AllocMalloc );
}

void MAC_(__builtin_delete) ( ThreadId tid, void* p )
{
   MAC_(handle_free)(
      tid, (Addr)p, MAC_MALLOC_REDZONE_SZB, MAC_AllocNew);
}

void MAC_(__builtin_vec_delete) ( ThreadId tid, void* p )
{
   MAC_(handle_free)(
      tid, (Addr)p, MAC_MALLOC_REDZONE_SZB, MAC_AllocNewVec);
}

void* MAC_(realloc) ( ThreadId tid, void* p, SizeT new_size )
{
   MAC_Chunk*  mc;
   MAC_Chunk** prev_chunks_next_ptr;

   VGP_PUSHCC(VgpCliMalloc);

   cmalloc_n_frees ++;
   cmalloc_n_mallocs ++;
   cmalloc_bs_mallocd += new_size;

   if (complain_about_silly_args(new_size, "realloc")) 
      return NULL;

   /* First try and find the block. */
   mc = (MAC_Chunk*)VG_(HT_get_node) ( MAC_(malloc_list), (UWord)p,
                                       (void*)&prev_chunks_next_ptr );

   if (mc == NULL) {
      MAC_(record_free_error) ( tid, (Addr)p );
      /* Perhaps we should return to the program regardless. */
      VGP_POPCC(VgpCliMalloc);
      return NULL;
   }
  
   /* check if its a matching free() / delete / delete [] */
   if (MAC_AllocMalloc != mc->allockind) {
      /* can not realloc a range that was allocated with new or new [] */
      MAC_(record_freemismatch_error) ( tid, (Addr)p, mc );
      /* but keep going anyway */
   }

   if (mc->size == new_size) {
      /* size unchanged */
      mc->where = VG_(record_ExeContext)(tid);
      VGP_POPCC(VgpCliMalloc);
      return p;
      
   } else if (mc->size > new_size) {
      /* new size is smaller */
      MAC_(die_mem_heap)( mc->data+new_size, mc->size-new_size );
      mc->size = new_size;
      mc->where = VG_(record_ExeContext)(tid);
      VGP_POPCC(VgpCliMalloc);
      return p;

   } else {
      /* new size is bigger */
      Addr p_new;

      /* Get new memory */
      p_new = (Addr)VG_(cli_malloc)(VG_(clo_alignment), new_size);

      /* First half kept and copied, second half new, 
         red zones as normal */
      MAC_(ban_mem_heap) ( p_new-MAC_MALLOC_REDZONE_SZB, MAC_MALLOC_REDZONE_SZB );
      MAC_(copy_mem_heap)( (Addr)p, p_new, mc->size );
      MAC_(new_mem_heap) ( p_new+mc->size, new_size-mc->size, /*inited*/False );
      MAC_(ban_mem_heap) ( p_new+new_size, MAC_MALLOC_REDZONE_SZB );

      /* Copy from old to new */
      VG_(memcpy)((void*)p_new, p, mc->size);

      /* Free old memory */
      /* Remove mc from the malloclist using prev_chunks_next_ptr to
         avoid repeating the hash table lookup.  Can't remove until at least
         after free_mismatch errors are done because they use
         describe_addr() which looks for it in malloclist. */
      *prev_chunks_next_ptr = mc->next;
      die_and_free_mem ( tid, mc, MAC_MALLOC_REDZONE_SZB );

      /* this has to be after die_and_free_mem, otherwise the
         former succeeds in shorting out the new block, not the
         old, in the case when both are on the same list.  */
      add_MAC_Chunk ( tid, p_new, new_size, 
                           MAC_AllocMalloc, MAC_(malloc_list) );

      VGP_POPCC(VgpCliMalloc);
      return (void*)p_new;
   }  
}

/* Memory pool stuff. */

void MAC_(create_mempool)(Addr pool, UInt rzB, Bool is_zeroed)
{
   MAC_Mempool* mp;

   mp            = VG_(malloc)(sizeof(MAC_Mempool));
   mp->pool      = pool;
   mp->rzB       = rzB;
   mp->is_zeroed = is_zeroed;
   mp->chunks    = VG_(HT_construct)( 3001 );  // prime, not so big

   /* Paranoia ... ensure this area is off-limits to the client, so
      the mp->data field isn't visible to the leak checker.  If memory
      management is working correctly, anything pointer returned by
      VG_(malloc) should be noaccess as far as the client is
      concerned. */
   if (!MAC_(check_noaccess)( (Addr)mp, sizeof(MAC_Mempool), NULL )) {
      VG_(tool_panic)("MAC_(create_mempool): shadow area is accessible");
   } 

   VG_(HT_add_node)( MAC_(mempool_list), (VgHashNode*)mp );
   
}

static void destroy_mempool_nuke_chunk(VgHashNode *node, void *d)
{
   MAC_Chunk *mc = (MAC_Chunk *)node;
   MAC_Mempool *mp = (MAC_Mempool *)d;
  
   /* Note: ban redzones again -- just in case user de-banned them
      with a client request... */
   MAC_(ban_mem_heap)(mc->data-mp->rzB, mp->rzB );
   MAC_(die_mem_heap)(mc->data, mc->size );
   MAC_(ban_mem_heap)(mc->data+mc->size, mp->rzB );
}

void MAC_(destroy_mempool)(Addr pool)
{
   MAC_Mempool* mp;

   mp = (MAC_Mempool*)VG_(HT_remove) ( MAC_(mempool_list), (UWord)pool );

   if (mp == NULL) {
      ThreadId tid = VG_(get_running_tid)();
      MAC_(record_illegal_mempool_error) ( tid, pool );
      return;
   }

   VG_(HT_apply_to_all_nodes)(mp->chunks, destroy_mempool_nuke_chunk, mp);
   VG_(HT_destruct)(mp->chunks);

   VG_(free)(mp);
}

void MAC_(mempool_alloc)(ThreadId tid, Addr pool, Addr addr, SizeT size)
{
   MAC_Mempool*  mp;

   mp = (MAC_Mempool*)VG_(HT_lookup) ( MAC_(mempool_list), (UWord)pool );

   if (mp == NULL) {
      MAC_(record_illegal_mempool_error) ( tid, pool );
      return;
   }

   MAC_(new_block)(tid, addr, size, /*ignored*/0, mp->rzB, mp->is_zeroed,
                   MAC_AllocCustom, mp->chunks);
}

void MAC_(mempool_free)(Addr pool, Addr addr)
{
   MAC_Mempool*  mp;
   MAC_Chunk*    mc;
   ThreadId      tid = VG_(get_running_tid)();

   mp = (MAC_Mempool*)VG_(HT_lookup)(MAC_(mempool_list), (UWord)pool);
   if (mp == NULL) {
      MAC_(record_illegal_mempool_error)(tid, pool);
      return;
   }

   mc = (MAC_Chunk*)VG_(HT_remove)(mp->chunks, (UWord)addr);
   if (mc == NULL) {
      MAC_(record_free_error)(tid, (Addr)addr);
      return;
   }

   die_and_free_mem ( tid, mc, mp->rzB );
}

/*------------------------------------------------------------*/
/*--- Statistics printing                                  ---*/
/*------------------------------------------------------------*/

typedef
   struct {
      UInt  nblocks;
      SizeT nbytes;
   }
   MallocStats;

static void malloc_stats_count_chunk(VgHashNode* node, void* d) {
   MAC_Chunk* mc = (MAC_Chunk*)node;
   MallocStats *ms = (MallocStats *)d;

   ms->nblocks ++;
   ms->nbytes  += mc->size;
}

void MAC_(print_malloc_stats) ( void )
{
   MallocStats ms;
  
   ms.nblocks = 0;
   ms.nbytes = 0;
   
   if (VG_(clo_verbosity) == 0)
      return;
   if (VG_(clo_xml))
      return;

   /* Count memory still in use. */
   VG_(HT_apply_to_all_nodes)(MAC_(malloc_list), malloc_stats_count_chunk, &ms);

   VG_(message)(Vg_UserMsg, 
                "malloc/free: in use at exit: %d bytes in %d blocks.",
                ms.nbytes, ms.nblocks);
   VG_(message)(Vg_UserMsg, 
                "malloc/free: %d allocs, %d frees, %u bytes allocated.",
                cmalloc_n_mallocs,
                cmalloc_n_frees, cmalloc_bs_mallocd);
   if (VG_(clo_verbosity) > 1)
      VG_(message)(Vg_UserMsg, "");
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
