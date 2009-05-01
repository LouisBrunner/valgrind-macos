/* -*- mode: C; c-basic-offset: 3; -*- */
/*
  This file is part of drd, a thread error detector.

  Copyright (C) 2006-2009 Bart Van Assche <bart.vanassche@gmail.com>.

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


#include "drd_malloc_wrappers.h"
#include "drd_thread.h"
#include "pub_tool_basics.h"
#include "pub_tool_execontext.h"
#include "pub_tool_hashtable.h"
#include "pub_tool_libcassert.h"
#include "pub_tool_libcbase.h"
#include "pub_tool_libcprint.h"
#include "pub_tool_mallocfree.h"
#include "pub_tool_options.h"
#include "pub_tool_replacemalloc.h"
#include "pub_tool_threadstate.h"
#include "pub_tool_tooliface.h"


/* Local type definitions. */

typedef struct _DRD_Chunk {
   struct _DRD_Chunk* next;
   Addr          data;            // ptr to actual block
   SizeT         size : (sizeof(UWord)*8)-2; //size requested; 30 or 62 bits
   ExeContext*   where;           // where it was allocated
} DRD_Chunk;


/* Local variables. */

static StartUsingMem s_start_using_mem_callback;
static StopUsingMem  s_stop_using_mem_callback;
/* Stats ... */
static SizeT s_cmalloc_n_mallocs  = 0;
static SizeT s_cmalloc_n_frees    = 0;
static SizeT s_cmalloc_bs_mallocd = 0;
/* Record malloc'd blocks. */
static VgHashTable s_malloc_list = NULL;


/*------------------------------------------------------------*/
/*--- Tracking malloc'd and free'd blocks                  ---*/
/*------------------------------------------------------------*/

/** Allocate its shadow chunk, put it on the appropriate list. */
static DRD_Chunk* create_chunk(ThreadId tid, Addr p, SizeT size)
{
   DRD_Chunk* mc = VG_(malloc)("drd.malloc_wrappers.cDC.1",
                               sizeof(DRD_Chunk));
   mc->data      = p;
   mc->size      = size;
   mc->where     = VG_(record_ExeContext)(tid, 0);

   return mc;
}

/*------------------------------------------------------------*/
/*--- client_malloc(), etc                                 ---*/
/*------------------------------------------------------------*/

/* Allocate memory and note change in memory available */
static void* new_block(ThreadId tid, SizeT size, SizeT align, Bool is_zeroed)
{
   Addr p;

   s_cmalloc_n_mallocs ++;

   // Allocate and zero
   p = (Addr)VG_(cli_malloc)(align, size);
   if (!p) {
      return NULL;
   }
   if (is_zeroed) VG_(memset)((void*)p, 0, size);

   DRD_(malloclike_block)(tid, p, size);

   return (void*)p;
}

/**
 * Store information about a memory block that has been allocated by
 * malloc() or a malloc() replacement.
 */
void DRD_(malloclike_block)(const ThreadId tid, const Addr p, const SizeT size)
{
   tl_assert(p);

   s_start_using_mem_callback(p, p + size, 0/*ec_uniq*/);

   // Only update this stat if allocation succeeded.
   s_cmalloc_bs_mallocd += size;

   VG_(HT_add_node)(s_malloc_list, create_chunk(tid, p, size));
}

static void* DRD_(malloc)(ThreadId tid, SizeT n)
{
   return new_block(tid, n, VG_(clo_alignment), /*is_zeroed*/False);
}

static void* DRD_(memalign)(ThreadId tid, SizeT align, SizeT n)
{
   return new_block(tid, n, align, /*is_zeroed*/False);
}

static void* DRD_(calloc)(ThreadId tid, SizeT nmemb, SizeT size1)
{
   return new_block(tid, nmemb*size1, VG_(clo_alignment),
                          /*is_zeroed*/True);
}

/**
 * Remove the information that was stored by DRD_(malloclike_block)() about
 * a memory block.
 */
Bool DRD_(freelike_block)(const ThreadId tid, const Addr p)
{
   DRD_Chunk* mc;

   tl_assert(p);

   s_cmalloc_n_frees++;

   mc = VG_(HT_remove)(s_malloc_list, (UWord)p);
   if (mc)
   {
      tl_assert(p == mc->data);
      if (mc->size > 0)
         s_stop_using_mem_callback(mc->data, mc->size);
      VG_(free)(mc);
      return True;
   }
   return False;
}

static void handle_free(ThreadId tid, Addr p)
{
   if (DRD_(freelike_block)(tid, p))
      VG_(cli_free)((void*)p);
   else
      tl_assert(False);
}

static void DRD_(free)(ThreadId tid, void* p)
{
   handle_free(tid, (Addr)p);
}

static void* DRD_(realloc)(ThreadId tid, void* p_old, SizeT new_size)
{
   DRD_Chunk* mc;
   void*     p_new;
   SizeT     old_size;

   s_cmalloc_n_frees ++;
   s_cmalloc_n_mallocs ++;
   s_cmalloc_bs_mallocd += new_size;

   /* Remove the old block */
   mc = VG_(HT_remove)(s_malloc_list, (UWord)p_old);
   if (mc == NULL) {
      tl_assert(0);
      return NULL;
   }

   old_size = mc->size;

   if (old_size == new_size)
   {
      /* size unchanged */
      mc->where = VG_(record_ExeContext)(tid, 0);
      p_new = p_old;
      
   }
   else if (old_size > new_size)
   {
      /* new size is smaller */
      s_stop_using_mem_callback(mc->data + new_size, old_size);
      mc->size = new_size;
      mc->where = VG_(record_ExeContext)(tid, 0);
      p_new = p_old;

   }
   else
   {
      /* new size is bigger */
      /* Get new memory */
      const Addr a_new = (Addr)VG_(cli_malloc)(VG_(clo_alignment), new_size);

      if (a_new)
      {
         /* Copy from old to new */
         VG_(memcpy)((void*)a_new, p_old, mc->size);

         /* Free old memory */
         s_stop_using_mem_callback(mc->data, mc->size);
         VG_(free)(mc);

         // Allocate a new chunk.
         mc = create_chunk(tid, a_new, new_size);
         s_start_using_mem_callback(a_new, a_new + new_size,
                                          0/*ec_uniq*/);
      }
      else
      {
         /* Allocation failed -- leave original block untouched. */
      }

      p_new = (void*)a_new;
   }  

   // Now insert the new mc (with a possibly new 'data' field) into
   // malloc_list.  If this realloc() did not increase the memory size, we
   // will have removed and then re-added mc unnecessarily.  But that's ok
   // because shrinking a block with realloc() is (presumably) much rarer
   // than growing it, and this way simplifies the growing case.
   VG_(HT_add_node)(s_malloc_list, mc);

   return p_new;
}

static void* DRD_(__builtin_new)(ThreadId tid, SizeT n)
{
   void* const result = new_block(tid, n, VG_(clo_alignment),
                                        /*is_zeroed*/False);
   //VG_(message)(Vg_DebugMsg, "__builtin_new(%d, %d) = %p", tid, n, result);
   return result;
}

static void DRD_(__builtin_delete)(ThreadId tid, void* p)
{
   //VG_(message)(Vg_DebugMsg, "__builtin_delete(%d, %p)", tid, p);
   handle_free(tid, (Addr)p);
}

static void* DRD_(__builtin_vec_new)(ThreadId tid, SizeT n)
{
   return new_block(tid, n, VG_(clo_alignment), /*is_zeroed*/False);
}

static void DRD_(__builtin_vec_delete)(ThreadId tid, void* p)
{
   handle_free(tid, (Addr)p);
}

static SizeT DRD_(malloc_usable_size) ( ThreadId tid, void* p )
{
   DRD_Chunk *mc = VG_(HT_lookup)( s_malloc_list, (UWord)p );

   // There may be slop, but pretend there isn't because only the asked-for
   // area will have been shadowed properly.
   return ( mc ? mc->size : 0 );
}

void DRD_(register_malloc_wrappers)(const StartUsingMem start_callback,
                                    const StopUsingMem stop_callback)
{
   tl_assert(s_malloc_list == 0);
   s_malloc_list = VG_(HT_construct)("drd_malloc_list");   // a big prime
   tl_assert(s_malloc_list != 0);
   tl_assert(start_callback);
   tl_assert(stop_callback);

   s_start_using_mem_callback = start_callback;
   s_stop_using_mem_callback  = stop_callback;

   VG_(needs_malloc_replacement)(DRD_(malloc),
                                 DRD_(__builtin_new),
                                 DRD_(__builtin_vec_new),
                                 DRD_(memalign),
                                 DRD_(calloc),
                                 DRD_(free),
                                 DRD_(__builtin_delete),
                                 DRD_(__builtin_vec_delete),
                                 DRD_(realloc),
                                 DRD_(malloc_usable_size),
                                 0);
}

Bool DRD_(heap_addrinfo)(Addr const a,
                         Addr* const data,
                         SizeT* const size,
                         ExeContext** const where)
{
   DRD_Chunk* mc;

   tl_assert(data);
   tl_assert(size);
   tl_assert(where);

   VG_(HT_ResetIter)(s_malloc_list);
   while ((mc = VG_(HT_Next)(s_malloc_list)))
   {
      if (mc->data <= a && a < mc->data + mc->size)
      {
         *data  = mc->data;
         *size  = mc->size;
         *where = mc->where;
         return True;
      }
   }
   return False;
}

/*------------------------------------------------------------*/
/*--- Statistics printing                                  ---*/
/*------------------------------------------------------------*/

void DRD_(print_malloc_stats)(void)
{
   DRD_Chunk* mc;
   SizeT     nblocks = 0;
   SizeT     nbytes  = 0;
   
   if (VG_(clo_verbosity) == 0)
      return;
   if (VG_(clo_xml))
      return;

   /* Count memory still in use. */
   VG_(HT_ResetIter)(s_malloc_list);
   while ((mc = VG_(HT_Next)(s_malloc_list)))
   {
      nblocks++;
      nbytes += mc->size;
   }

   VG_(message)(Vg_DebugMsg, 
                "malloc/free: in use at exit: %lu bytes in %lu blocks.",
                nbytes, nblocks);
   VG_(message)(Vg_DebugMsg, 
                "malloc/free: %lu allocs, %lu frees, %lu bytes allocated.",
                s_cmalloc_n_mallocs,
                s_cmalloc_n_frees, s_cmalloc_bs_mallocd);
   if (VG_(clo_verbosity) > 1)
      VG_(message)(Vg_DebugMsg, " ");
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
