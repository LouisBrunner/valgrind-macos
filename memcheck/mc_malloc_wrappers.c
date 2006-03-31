
/*--------------------------------------------------------------------*/
/*--- malloc/free wrappers for detecting errors and updating bits. ---*/
/*---                                         mc_malloc_wrappers.c ---*/
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
#include "pub_tool_execontext.h"
#include "pub_tool_hashtable.h"
#include "pub_tool_libcbase.h"
#include "pub_tool_libcassert.h"
#include "pub_tool_libcprint.h"
#include "pub_tool_mallocfree.h"
#include "pub_tool_options.h"
#include "pub_tool_replacemalloc.h"
#include "pub_tool_threadstate.h"
#include "pub_tool_tooliface.h"     // Needed for mc_include.h

#include "mc_include.h"

/*------------------------------------------------------------*/
/*--- Defns                                                ---*/
/*------------------------------------------------------------*/

/* Stats ... */
static SizeT cmalloc_n_mallocs  = 0;
static SizeT cmalloc_n_frees    = 0;
static SizeT cmalloc_bs_mallocd = 0;


/*------------------------------------------------------------*/
/*--- Tracking malloc'd and free'd blocks                  ---*/
/*------------------------------------------------------------*/

/* Record malloc'd blocks. */
VgHashTable MC_(malloc_list) = NULL;

/* Memory pools. */
VgHashTable MC_(mempool_list) = NULL;
   
/* Records blocks after freeing. */
static MC_Chunk* freed_list_start  = NULL;
static MC_Chunk* freed_list_end    = NULL;
static Int       freed_list_volume = 0;

/* Put a shadow chunk on the freed blocks queue, possibly freeing up
   some of the oldest blocks in the queue at the same time. */
static void add_to_freed_queue ( MC_Chunk* mc )
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

   while (freed_list_volume > MC_(clo_freelist_vol)) {
      MC_Chunk* mc1;

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

      /* free MC_Chunk */
      VG_(cli_free) ( (void*)(mc1->data) );
      VG_(free) ( mc1 );
   }
}

MC_Chunk* MC_(get_freed_list_head)(void)
{
   return freed_list_start;
}

/* Allocate its shadow chunk, put it on the appropriate list. */
static
MC_Chunk* create_MC_Chunk ( ThreadId tid, Addr p, SizeT size,
                            MC_AllocKind kind)
{
   MC_Chunk* mc  = VG_(malloc)(sizeof(MC_Chunk));
   mc->data      = p;
   mc->size      = size;
   mc->allockind = kind;
   mc->where     = VG_(record_ExeContext)(tid);

   /* Paranoia ... ensure the MC_Chunk is off-limits to the client, so
      the mc->data field isn't visible to the leak checker.  If memory
      management is working correctly, any pointer returned by VG_(malloc)
      should be noaccess as far as the client is concerned. */
   if (!MC_(check_mem_is_noaccess)( (Addr)mc, sizeof(MC_Chunk), NULL )) {
      VG_(tool_panic)("create_MC_Chunk: shadow area is accessible");
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
      VG_(message)(Vg_UserMsg, "Warning: silly arg (%ld) to %s()",
                   (SSizeT)sizeB, fn );
      return True;
   }
   return False;
}

static Bool complain_about_silly_args2(SizeT n, SizeT sizeB)
{
   if ((SSizeT)n < 0 || (SSizeT)sizeB < 0) {
      VG_(message)(Vg_UserMsg, "Warning: silly args (%ld,%ld) to calloc()",
                   (SSizeT)n, (SSizeT)sizeB);
      return True;
   }
   return False;
}

/* Allocate memory and note change in memory available */
__inline__
void* MC_(new_block) ( ThreadId tid,
                        Addr p, SizeT size, SizeT align, UInt rzB,
                        Bool is_zeroed, MC_AllocKind kind, VgHashTable table)
{
   cmalloc_n_mallocs ++;

   // Allocate and zero if necessary
   if (p) {
      tl_assert(MC_AllocCustom == kind);
   } else {
      tl_assert(MC_AllocCustom != kind);
      p = (Addr)VG_(cli_malloc)( align, size );
      if (!p) {
         return NULL;
      }
      if (is_zeroed) VG_(memset)((void*)p, 0, size);
   }

   // Only update this stat if allocation succeeded.
   cmalloc_bs_mallocd += size;

   VG_(HT_add_node)( table, create_MC_Chunk(tid, p, size, kind) );

   if (is_zeroed)
      MC_(make_mem_defined)( p, size );
   else
      MC_(make_mem_undefined)( p, size );

   return (void*)p;
}

void* MC_(malloc) ( ThreadId tid, SizeT n )
{
   if (complain_about_silly_args(n, "malloc")) {
      return NULL;
   } else {
      return MC_(new_block) ( tid, 0, n, VG_(clo_alignment), 
         MC_MALLOC_REDZONE_SZB, /*is_zeroed*/False, MC_AllocMalloc,
         MC_(malloc_list));
   }
}

void* MC_(__builtin_new) ( ThreadId tid, SizeT n )
{
   if (complain_about_silly_args(n, "__builtin_new")) {
      return NULL;
   } else {
      return MC_(new_block) ( tid, 0, n, VG_(clo_alignment), 
         MC_MALLOC_REDZONE_SZB, /*is_zeroed*/False, MC_AllocNew,
         MC_(malloc_list));
   }
}

void* MC_(__builtin_vec_new) ( ThreadId tid, SizeT n )
{
   if (complain_about_silly_args(n, "__builtin_vec_new")) {
      return NULL;
   } else {
      return MC_(new_block) ( tid, 0, n, VG_(clo_alignment), 
         MC_MALLOC_REDZONE_SZB, /*is_zeroed*/False, MC_AllocNewVec,
         MC_(malloc_list));
   }
}

void* MC_(memalign) ( ThreadId tid, SizeT align, SizeT n )
{
   if (complain_about_silly_args(n, "memalign")) {
      return NULL;
   } else {
      return MC_(new_block) ( tid, 0, n, align, 
         MC_MALLOC_REDZONE_SZB, /*is_zeroed*/False, MC_AllocMalloc,
         MC_(malloc_list));
   }
}

void* MC_(calloc) ( ThreadId tid, SizeT nmemb, SizeT size1 )
{
   if (complain_about_silly_args2(nmemb, size1)) {
      return NULL;
   } else {
      return MC_(new_block) ( tid, 0, nmemb*size1, VG_(clo_alignment),
         MC_MALLOC_REDZONE_SZB, /*is_zeroed*/True, MC_AllocMalloc,
         MC_(malloc_list));
   }
}

static
void die_and_free_mem ( ThreadId tid, MC_Chunk* mc, SizeT rzB )
{
   /* Note: make redzones noaccess again -- just in case user made them
      accessible with a client request... */
   MC_(make_mem_noaccess)( mc->data-rzB, mc->size + 2*rzB );

   /* Put it out of harm's way for a while, if not from a client request */
   if (MC_AllocCustom != mc->allockind) {
      /* Record where freed */
      mc->where = VG_(record_ExeContext) ( tid );
      add_to_freed_queue ( mc );
   } else {
      VG_(free) ( mc );
   }
}

__inline__
void MC_(handle_free) ( ThreadId tid, Addr p, UInt rzB, MC_AllocKind kind )
{
   MC_Chunk* mc;

   cmalloc_n_frees++;

   mc = VG_(HT_remove) ( MC_(malloc_list), (UWord)p );
   if (mc == NULL) {
      MC_(record_free_error) ( tid, p );
   } else {
      /* check if it is a matching free() / delete / delete [] */
      if (kind != mc->allockind) {
         MC_(record_freemismatch_error) ( tid, p, mc );
      }
      die_and_free_mem ( tid, mc, rzB );
   }
}

void MC_(free) ( ThreadId tid, void* p )
{
   MC_(handle_free)( 
      tid, (Addr)p, MC_MALLOC_REDZONE_SZB, MC_AllocMalloc );
}

void MC_(__builtin_delete) ( ThreadId tid, void* p )
{
   MC_(handle_free)(
      tid, (Addr)p, MC_MALLOC_REDZONE_SZB, MC_AllocNew);
}

void MC_(__builtin_vec_delete) ( ThreadId tid, void* p )
{
   MC_(handle_free)(
      tid, (Addr)p, MC_MALLOC_REDZONE_SZB, MC_AllocNewVec);
}

void* MC_(realloc) ( ThreadId tid, void* p_old, SizeT new_size )
{
   MC_Chunk* mc;
   void*     p_new;
   SizeT     old_size;

   cmalloc_n_frees ++;
   cmalloc_n_mallocs ++;
   cmalloc_bs_mallocd += new_size;

   if (complain_about_silly_args(new_size, "realloc")) 
      return NULL;

   /* Remove the old block */
   mc = VG_(HT_remove) ( MC_(malloc_list), (UWord)p_old );
   if (mc == NULL) {
      MC_(record_free_error) ( tid, (Addr)p_old );
      /* We return to the program regardless. */
      return NULL;
   }

   /* check if its a matching free() / delete / delete [] */
   if (MC_AllocMalloc != mc->allockind) {
      /* can not realloc a range that was allocated with new or new [] */
      MC_(record_freemismatch_error) ( tid, (Addr)p_old, mc );
      /* but keep going anyway */
   }

   old_size = mc->size;

   if (old_size == new_size) {
      /* size unchanged */
      mc->where = VG_(record_ExeContext)(tid);
      p_new = p_old;
      
   } else if (old_size > new_size) {
      /* new size is smaller */
      MC_(make_mem_noaccess)( mc->data+new_size, mc->size-new_size );
      mc->size = new_size;
      mc->where = VG_(record_ExeContext)(tid);
      p_new = p_old;

   } else {
      /* new size is bigger */
      /* Get new memory */
      Addr a_new = (Addr)VG_(cli_malloc)(VG_(clo_alignment), new_size);

      if (a_new) {
         /* First half kept and copied, second half new, red zones as normal */
         MC_(make_mem_noaccess)( a_new-MC_MALLOC_REDZONE_SZB, MC_MALLOC_REDZONE_SZB );
         MC_(copy_address_range_state)( (Addr)p_old, a_new, mc->size );
         MC_(make_mem_undefined)( a_new+mc->size, new_size-mc->size );
         MC_(make_mem_noaccess) ( a_new+new_size, MC_MALLOC_REDZONE_SZB );

         /* Copy from old to new */
         VG_(memcpy)((void*)a_new, p_old, mc->size);

         /* Free old memory */
         /* Nb: we have to allocate a new MC_Chunk for the new memory rather
            than recycling the old one, so that any erroneous accesses to the
            old memory are reported. */
         die_and_free_mem ( tid, mc, MC_MALLOC_REDZONE_SZB );

         // Allocate a new chunk.
         mc = create_MC_Chunk( tid, a_new, new_size, MC_AllocMalloc );
      }

      p_new = (void*)a_new;
   }  

   // Now insert the new mc (with a possibly new 'data' field) into
   // malloc_list.  If this realloc() did not increase the memory size, we
   // will have removed and then re-added mc unnecessarily.  But that's ok
   // because shrinking a block with realloc() is (presumably) much rarer
   // than growing it, and this way simplifies the growing case.
   VG_(HT_add_node)( MC_(malloc_list), mc );

   return p_new;
}

/* Memory pool stuff. */

void MC_(create_mempool)(Addr pool, UInt rzB, Bool is_zeroed)
{
   MC_Mempool* mp = VG_(malloc)(sizeof(MC_Mempool));
   mp->pool       = pool;
   mp->rzB        = rzB;
   mp->is_zeroed  = is_zeroed;
   mp->chunks     = VG_(HT_construct)( 3001 );  // prime, not so big

   /* Paranoia ... ensure this area is off-limits to the client, so
      the mp->data field isn't visible to the leak checker.  If memory
      management is working correctly, anything pointer returned by
      VG_(malloc) should be noaccess as far as the client is
      concerned. */
   if (!MC_(check_mem_is_noaccess)( (Addr)mp, sizeof(MC_Mempool), NULL )) {
      VG_(tool_panic)("MC_(create_mempool): shadow area is accessible");
   } 

   VG_(HT_add_node)( MC_(mempool_list), mp );
}

void MC_(destroy_mempool)(Addr pool)
{
   MC_Chunk*   mc;
   MC_Mempool* mp;

   mp = VG_(HT_remove) ( MC_(mempool_list), (UWord)pool );

   if (mp == NULL) {
      ThreadId tid = VG_(get_running_tid)();
      MC_(record_illegal_mempool_error) ( tid, pool );
      return;
   }

   // Clean up the chunks, one by one
   VG_(HT_ResetIter)(mp->chunks);
   while ( (mc = VG_(HT_Next)(mp->chunks)) ) {
      /* Note: make redzones noaccess again -- just in case user made them
         accessible with a client request... */
      MC_(make_mem_noaccess)(mc->data-mp->rzB, mc->size + 2*mp->rzB );
   }
   // Destroy the chunk table
   VG_(HT_destruct)(mp->chunks);

   VG_(free)(mp);
}

void MC_(mempool_alloc)(ThreadId tid, Addr pool, Addr addr, SizeT size)
{
   MC_Mempool* mp = VG_(HT_lookup) ( MC_(mempool_list), (UWord)pool );

   if (mp == NULL) {
      MC_(record_illegal_mempool_error) ( tid, pool );
   } else {
      MC_(new_block)(tid, addr, size, /*ignored*/0, mp->rzB, mp->is_zeroed,
                     MC_AllocCustom, mp->chunks);
   }
}

void MC_(mempool_free)(Addr pool, Addr addr)
{
   MC_Mempool*  mp;
   MC_Chunk*    mc;
   ThreadId     tid = VG_(get_running_tid)();

   mp = VG_(HT_lookup)(MC_(mempool_list), (UWord)pool);
   if (mp == NULL) {
      MC_(record_illegal_mempool_error)(tid, pool);
      return;
   }

   mc = VG_(HT_remove)(mp->chunks, (UWord)addr);
   if (mc == NULL) {
      MC_(record_free_error)(tid, (Addr)addr);
      return;
   }

   die_and_free_mem ( tid, mc, mp->rzB );
}

/*------------------------------------------------------------*/
/*--- Statistics printing                                  ---*/
/*------------------------------------------------------------*/

void MC_(print_malloc_stats) ( void )
{
   MC_Chunk* mc;
   SizeT     nblocks = 0;
   SizeT     nbytes  = 0;
   
   if (VG_(clo_verbosity) == 0)
      return;
   if (VG_(clo_xml))
      return;

   /* Count memory still in use. */
   VG_(HT_ResetIter)(MC_(malloc_list));
   while ( (mc = VG_(HT_Next)(MC_(malloc_list))) ) {
      nblocks++;
      nbytes += mc->size;
   }

   VG_(message)(Vg_UserMsg, 
                "malloc/free: in use at exit: %,lu bytes in %,lu blocks.",
                nbytes, nblocks);
   VG_(message)(Vg_UserMsg, 
                "malloc/free: %,lu allocs, %,lu frees, %,lu bytes allocated.",
                cmalloc_n_mallocs,
                cmalloc_n_frees, cmalloc_bs_mallocd);
   if (VG_(clo_verbosity) > 1)
      VG_(message)(Vg_UserMsg, "");
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
