
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

/* Record malloc'd blocks. */
VgHashTable MAC_(malloc_list) = NULL;

/* Memory pools. */
VgHashTable MAC_(mempool_list) = NULL;
   
/* Records blocks after freeing. */
static MAC_Chunk* freed_list_start  = NULL;
static MAC_Chunk* freed_list_end    = NULL;
static Int        freed_list_volume = 0;

/* Put a shadow chunk on the freed blocks queue, possibly freeing up
   some of the oldest blocks in the queue at the same time. */
static void add_to_freed_queue ( MAC_Chunk* mc )
{
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
      MAC_Chunk* mc1;

      tl_assert(freed_list_start != NULL);
      tl_assert(freed_list_end != NULL);

      mc1 = freed_list_start;
      freed_list_volume -= mc1->size;
      /* VG_(printf)("volume now %d\n", freed_list_volume); */
      tl_assert(freed_list_volume >= 0);

      if (freed_list_start == freed_list_end) {
         freed_list_start = freed_list_end = NULL;
      } else {
         freed_list_start = mc1->next;
      }
      mc1->next = NULL; /* just paranoia */

      /* free MAC_Chunk */
      VG_(cli_free) ( (void*)(mc1->data) );
      VG_(free) ( mc1 );
   }
}

MAC_Chunk* MAC_(get_freed_list_head)(void)
{
   return freed_list_start;
}

/* Allocate its shadow chunk, put it on the appropriate list. */
static
MAC_Chunk* create_MAC_Chunk ( ThreadId tid, Addr p, SizeT size,
                              MAC_AllocKind kind)
{
   MAC_Chunk* mc = VG_(malloc)(sizeof(MAC_Chunk));
   mc->data      = p;
   mc->size      = size;
   mc->allockind = kind;
   mc->where     = VG_(record_ExeContext)(tid);

   /* Paranoia ... ensure the MAC_Chunk is off-limits to the client, so
      the mc->data field isn't visible to the leak checker.  If memory
      management is working correctly, any pointer returned by VG_(malloc)
      should be noaccess as far as the client is concerned. */
   if (!MAC_(check_noaccess)( (Addr)mc, sizeof(MAC_Chunk), NULL )) {
      VG_(tool_panic)("create_MAC_Chunk: shadow area is accessible");
   } 
   return mc;
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
#if VG_WORDSIZE == 4
      VG_(message)(Vg_UserMsg, "Warning: silly arg (%d) to %s()",
                   (Int)sizeB, fn );
#elif VG_WORDSIZE == 8
      VG_(message)(Vg_UserMsg, "Warning: silly arg (%lld) to %s()",
                   (Long)sizeB, fn );
#else
#  error Unexpected word size
#endif
      return True;
   }
   return False;
}

static Bool complain_about_silly_args2(SizeT n, SizeT sizeB)
{
   if ((SSizeT)n < 0 || (SSizeT)sizeB < 0) {
#if VG_WORDSIZE == 4
      VG_(message)(Vg_UserMsg, "Warning: silly args (%d,%d) to calloc()",
                   (Int)n, (Int)sizeB);
#elif VG_WORDSIZE == 8
      VG_(message)(Vg_UserMsg, "Warning: silly args (%lld,%lld) to calloc()",
                   (Long)n, (Long)sizeB);
#else
#  error Unexpected word size
#endif
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

   VG_(HT_add_node)( table, create_MAC_Chunk(tid, p, size, kind) );

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
   MAC_Chunk* mc;

   VGP_PUSHCC(VgpCliMalloc);

   cmalloc_n_frees++;

   mc = VG_(HT_remove) ( MAC_(malloc_list), (UWord)p );
   if (mc == NULL) {
      MAC_(record_free_error) ( tid, p );
   } else {
      /* check if it is a matching free() / delete / delete [] */
      if (kind != mc->allockind) {
         MAC_(record_freemismatch_error) ( tid, p, mc );
      }
      die_and_free_mem ( tid, mc, rzB );
   }

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

void* MAC_(realloc) ( ThreadId tid, void* p_old, SizeT new_size )
{
   MAC_Chunk* mc;
   void*      p_new;
   SizeT      old_size;

   VGP_PUSHCC(VgpCliMalloc);

   cmalloc_n_frees ++;
   cmalloc_n_mallocs ++;
   cmalloc_bs_mallocd += new_size;

   if (complain_about_silly_args(new_size, "realloc")) 
      return NULL;

   /* Remove the old block */
   mc = VG_(HT_remove) ( MAC_(malloc_list), (UWord)p_old );
   if (mc == NULL) {
      MAC_(record_free_error) ( tid, (Addr)p_old );
      /* We return to the program regardless. */
      VGP_POPCC(VgpCliMalloc);
      return NULL;
   }

   /* check if its a matching free() / delete / delete [] */
   if (MAC_AllocMalloc != mc->allockind) {
      /* can not realloc a range that was allocated with new or new [] */
      MAC_(record_freemismatch_error) ( tid, (Addr)p_old, mc );
      /* but keep going anyway */
   }

   old_size = mc->size;

   if (old_size == new_size) {
      /* size unchanged */
      mc->where = VG_(record_ExeContext)(tid);
      p_new = p_old;
      
   } else if (old_size > new_size) {
      /* new size is smaller */
      MAC_(die_mem_heap)( mc->data+new_size, mc->size-new_size );
      mc->size = new_size;
      mc->where = VG_(record_ExeContext)(tid);
      p_new = p_old;

   } else {
      /* new size is bigger */
      /* Get new memory */
      Addr a_new = (Addr)VG_(cli_malloc)(VG_(clo_alignment), new_size);

      /* First half kept and copied, second half new, red zones as normal */
      MAC_(ban_mem_heap) ( a_new-MAC_MALLOC_REDZONE_SZB, MAC_MALLOC_REDZONE_SZB );
      MAC_(copy_mem_heap)( (Addr)p_old, a_new, mc->size );
      MAC_(new_mem_heap) ( a_new+mc->size, new_size-mc->size, /*init'd*/False );
      MAC_(ban_mem_heap) ( a_new+new_size, MAC_MALLOC_REDZONE_SZB );

      /* Copy from old to new */
      VG_(memcpy)((void*)a_new, p_old, mc->size);

      /* Free old memory */
      /* Nb: we have to allocate a new MAC_Chunk for the new memory rather
         than recycling the old one, so that any erroneous accesses to the
         old memory are reported. */
      die_and_free_mem ( tid, mc, MAC_MALLOC_REDZONE_SZB );

      // Allocate a new chunk.
      mc = create_MAC_Chunk( tid, a_new, new_size, MAC_AllocMalloc );
      p_new = (void*)a_new;
   }  

   // Now insert the new mc (with a possibly new 'data' field) into
   // malloc_list.  If this realloc() did not increase the memory size, we
   // will have removed and then re-added mc unnecessarily.  But that's ok
   // because shrinking a block with realloc() is (presumably) much rarer
   // than growing it, and this way simplifies the growing case.
   VG_(HT_add_node)( MAC_(malloc_list), mc );

   VGP_POPCC(VgpCliMalloc);
   return p_new;
}

/* Memory pool stuff. */

void MAC_(create_mempool)(Addr pool, UInt rzB, Bool is_zeroed)
{
   MAC_Mempool* mp = VG_(malloc)(sizeof(MAC_Mempool));
   mp->pool        = pool;
   mp->rzB         = rzB;
   mp->is_zeroed   = is_zeroed;
   mp->chunks      = VG_(HT_construct)( 3001 );  // prime, not so big

   /* Paranoia ... ensure this area is off-limits to the client, so
      the mp->data field isn't visible to the leak checker.  If memory
      management is working correctly, anything pointer returned by
      VG_(malloc) should be noaccess as far as the client is
      concerned. */
   if (!MAC_(check_noaccess)( (Addr)mp, sizeof(MAC_Mempool), NULL )) {
      VG_(tool_panic)("MAC_(create_mempool): shadow area is accessible");
   } 

   VG_(HT_add_node)( MAC_(mempool_list), mp );
}

void MAC_(destroy_mempool)(Addr pool)
{
   MAC_Chunk*   mc;
   MAC_Mempool* mp;

   mp = VG_(HT_remove) ( MAC_(mempool_list), (UWord)pool );

   if (mp == NULL) {
      ThreadId tid = VG_(get_running_tid)();
      MAC_(record_illegal_mempool_error) ( tid, pool );
      return;
   }

   // Clean up the chunks, one by one
   VG_(HT_ResetIter)(mp->chunks);
   while ( (mc = VG_(HT_Next)(mp->chunks)) ) {
      /* Note: ban redzones again -- just in case user de-banned them
         with a client request... */
      MAC_(ban_mem_heap)(mc->data-mp->rzB, mp->rzB );
      MAC_(die_mem_heap)(mc->data, mc->size );
      MAC_(ban_mem_heap)(mc->data+mc->size, mp->rzB );
   }
   // Destroy the chunk table
   VG_(HT_destruct)(mp->chunks);

   VG_(free)(mp);
}

void MAC_(mempool_alloc)(ThreadId tid, Addr pool, Addr addr, SizeT size)
{
   MAC_Mempool* mp = VG_(HT_lookup) ( MAC_(mempool_list), (UWord)pool );

   if (mp == NULL) {
      MAC_(record_illegal_mempool_error) ( tid, pool );
   } else {
      MAC_(new_block)(tid, addr, size, /*ignored*/0, mp->rzB, mp->is_zeroed,
                      MAC_AllocCustom, mp->chunks);
   }
}

void MAC_(mempool_free)(Addr pool, Addr addr)
{
   MAC_Mempool*  mp;
   MAC_Chunk*    mc;
   ThreadId      tid = VG_(get_running_tid)();

   mp = VG_(HT_lookup)(MAC_(mempool_list), (UWord)pool);
   if (mp == NULL) {
      MAC_(record_illegal_mempool_error)(tid, pool);
      return;
   }

   mc = VG_(HT_remove)(mp->chunks, (UWord)addr);
   if (mc == NULL) {
      MAC_(record_free_error)(tid, (Addr)addr);
      return;
   }

   die_and_free_mem ( tid, mc, mp->rzB );
}

/*------------------------------------------------------------*/
/*--- Statistics printing                                  ---*/
/*------------------------------------------------------------*/

void MAC_(print_malloc_stats) ( void )
{
   MAC_Chunk* mc;
   UInt       nblocks = 0;
   SizeT      nbytes  = 0;
   
   if (VG_(clo_verbosity) == 0)
      return;
   if (VG_(clo_xml))
      return;

   /* Count memory still in use. */
   VG_(HT_ResetIter)(MAC_(malloc_list));
   while ( (mc = VG_(HT_Next)(MAC_(malloc_list))) ) {
      nblocks++;
      nbytes += mc->size;
   }

   VG_(message)(Vg_UserMsg, 
                "malloc/free: in use at exit: %d bytes in %d blocks.",
                nbytes, nblocks);
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
