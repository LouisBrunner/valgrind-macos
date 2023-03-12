/*
  This file is part of drd, a thread error detector.

  Copyright (C) 2006-2020 Bart Van Assche <bvanassche@acm.org>.

  This program is free software; you can redistribute it and/or
  modify it under the terms of the GNU General Public License as
  published by the Free Software Foundation; either version 2 of the
  License, or (at your option) any later version.

  This program is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, see <http://www.gnu.org/licenses/>.

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

/**
 * Node with per-allocation information that will be stored in a hash map.
 * As specified in <pub_tool_hashtable.h>, the first member must be a pointer
 * and the second member must be an UWord.
 */
typedef struct _DRD_Chunk {
   struct _DRD_Chunk* next;
   UWord              data;   // pointer to actual block
   SizeT              size;   // size requested
   ExeContext*        where;  // where it was allocated
} DRD_Chunk;


/* Local variables. */

static StartUsingMem s_start_using_mem_callback;
static StopUsingMem  s_stop_using_mem_callback;
/* Statistics. */
static SizeT s_cmalloc_n_mallocs  = 0;
static SizeT s_cmalloc_n_frees    = 0;
static SizeT s_cmalloc_bs_mallocd = 0;
/* Record malloc'd blocks. */
static VgHashTable *s_malloc_list = NULL;


/* Function definitions. */

/** Allocate client memory memory and update the hash map. */
static void* new_block(ThreadId tid, SizeT size, SizeT align, Bool is_zeroed)
{
   void* p;

   p = VG_(cli_malloc)(align, size);
   if (!p)
      return NULL;
   if (is_zeroed)
      VG_(memset)(p, 0, size);

   DRD_(malloclike_block)(tid, (Addr)p, size);

   return p;
}

/**
 * Store information about a memory block that has been allocated by
 * malloc() or a malloc() replacement in the hash map.
 */
void DRD_(malloclike_block)(const ThreadId tid, const Addr p, const SizeT size)
{
   DRD_Chunk* mc;

   tl_assert(p);

   if (size > 0)
      s_start_using_mem_callback(p, size, 0/*ec_uniq*/);

   s_cmalloc_n_mallocs++;
   // Only update this stat if allocation succeeded.
   s_cmalloc_bs_mallocd += size;

   mc = VG_(malloc)("drd.malloc_wrappers.cDC.1", sizeof(DRD_Chunk));
   mc->data  = p;
   mc->size  = size;
   mc->where = VG_(record_ExeContext)(tid, 0);
   VG_(HT_add_node)(s_malloc_list, mc);
}

static void handle_free(ThreadId tid, void* p)
{
   Bool success;

   tl_assert(p);
   success = DRD_(freelike_block)(tid, (Addr)p, True);
   tl_assert(success);
}

/**
 * Remove the information that was stored by DRD_(malloclike_block)() about
 * a memory block.
 */
Bool DRD_(freelike_block)(const ThreadId tid, const Addr p, const Bool dealloc)
{
   DRD_Chunk* mc;

   tl_assert(p);

   s_cmalloc_n_frees++;

   mc = VG_(HT_lookup)(s_malloc_list, (UWord)p);
   if (mc)
   {
      tl_assert(p == mc->data);
      if (mc->size > 0)
         s_stop_using_mem_callback(mc->data, mc->size);
      if (dealloc)
	 VG_(cli_free)((void*)p);
      VG_(HT_remove)(s_malloc_list, (UWord)p);
      VG_(free)(mc);
      return True;
   }
   return False;
}

/** Wrapper for malloc(). */
static void* drd_malloc(ThreadId tid, SizeT n)
{
   return new_block(tid, n, VG_(clo_alignment), /*is_zeroed*/False);
}

/** Wrapper for memalign(). */
static void* drd_memalign(ThreadId tid, SizeT align, SizeT orig_alignT, SizeT n)
{
   return new_block(tid, n, align, /*is_zeroed*/False);
}

/** Wrapper for calloc(). */
static void* drd_calloc(ThreadId tid, SizeT nmemb, SizeT size1)
{
   return new_block(tid, nmemb*size1, VG_(clo_alignment),
                          /*is_zeroed*/True);
}

/** Wrapper for free(). */
static void drd_free(ThreadId tid, void* p)
{
   handle_free(tid, p);
}

/**
 * Wrapper for realloc(). Returns a pointer to the new block of memory, or
 * NULL if no new block could not be allocated. Notes:
 * - realloc(NULL, size) has the same effect as malloc(size).
 * - realloc(p, 0) has the same effect as free(p).
 * - success is not guaranteed even if the requested size is smaller than the
 *   allocated size.
*/
static void* drd_realloc(ThreadId tid, void* p_old, SizeT new_size)
{
   DRD_Chunk* mc;
   void*      p_new;
   SizeT      old_size;

   if (! p_old)
      return drd_malloc(tid, new_size);

   if (new_size == 0)
   {
      if (VG_(clo_realloc_zero_bytes_frees) == True)
      {
         drd_free(tid, p_old);
         return NULL;
      }
      new_size = 1;
   }

   s_cmalloc_n_mallocs++;
   s_cmalloc_n_frees++;
   s_cmalloc_bs_mallocd += new_size;

   mc = VG_(HT_lookup)(s_malloc_list, (UWord)p_old);
   if (mc == NULL)
   {
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
   else if (new_size < old_size)
   {
      /* new size is smaller but nonzero */
      s_stop_using_mem_callback(mc->data + new_size, old_size - new_size);
      mc->size = new_size;
      mc->where = VG_(record_ExeContext)(tid, 0);
      p_new = p_old;
   }
   else
   {
      /* new size is bigger */
      p_new = VG_(cli_malloc)(VG_(clo_alignment), new_size);

      if (p_new)
      {
         /* Copy from old to new. */
         VG_(memcpy)(p_new, p_old, mc->size);

         /* Free old memory. */
         if (mc->size > 0)
            s_stop_using_mem_callback(mc->data, mc->size);
         VG_(cli_free)(p_old);
         VG_(HT_remove)(s_malloc_list, (UWord)p_old);

         /* Update state information. */
         mc->data  = (Addr)p_new;
         mc->size  = new_size;
         mc->where = VG_(record_ExeContext)(tid, 0);
         VG_(HT_add_node)(s_malloc_list, mc);
         s_start_using_mem_callback((Addr)p_new, new_size, 0/*ec_uniq*/);
      }
      else
      {
         /* Allocation failed -- leave original block untouched. */
      }
   }

   return p_new;
}

/** Wrapper for __builtin_new(). */
static void* drd___builtin_new(ThreadId tid, SizeT n)
{
   return new_block(tid, n, VG_(clo_alignment), /*is_zeroed*/False);
}

/** Wrapper for __builtin_new_aligned(). */
static void* drd___builtin_new_aligned(ThreadId tid, SizeT n, SizeT align, SizeT orig_align)
{
   return new_block(tid, n, align, /*is_zeroed*/False);
}

/** Wrapper for __builtin_delete(). */
static void drd___builtin_delete(ThreadId tid, void* p)
{
   handle_free(tid, p);
}

/** Wrapper for __builtin_delete_aligned(). */
static void drd___builtin_delete_aligned(ThreadId tid, void* p, SizeT align)
{
   handle_free(tid, p);
}

/** Wrapper for __builtin_vec_new(). */
static void* drd___builtin_vec_new(ThreadId tid, SizeT n)
{
   return new_block(tid, n, VG_(clo_alignment), /*is_zeroed*/False);
}

/** Wrapper for __builtin_vec_new_aligned(). */
static void* drd___builtin_vec_new_aligned(ThreadId tid, SizeT n, SizeT align, SizeT orig_align)
{
   return new_block(tid, n, align, /*is_zeroed*/False);
}

/** Wrapper for __builtin_vec_delete(). */
static void drd___builtin_vec_delete(ThreadId tid, void* p)
{
   handle_free(tid, p);
}

/** Wrapper for __builtin_vec_delete_aligned(). */
static void drd___builtin_vec_delete_aligned(ThreadId tid, void* p, SizeT align)
{
   handle_free(tid, p);
}

/**
 * Wrapper for malloc_usable_size() / malloc_size(). This function takes
 * a pointer to a block allocated by `malloc' and returns the amount of space
 * that is available in the block. This may or may not be more than the size
 * requested from `malloc', due to alignment or minimum size constraints.
 */
static SizeT drd_malloc_usable_size(ThreadId tid, void* p)
{
   DRD_Chunk* mc;

   mc = VG_(HT_lookup)(s_malloc_list, (UWord)p);

   return mc ? mc->size : 0;
}

void DRD_(register_malloc_wrappers)(const StartUsingMem start_callback,
                                    const StopUsingMem stop_callback)
{
   tl_assert(s_malloc_list == 0);
   s_malloc_list = VG_(HT_construct)("drd_malloc_list");
   tl_assert(start_callback);
   tl_assert(stop_callback);

   s_start_using_mem_callback = start_callback;
   s_stop_using_mem_callback  = stop_callback;

   VG_(needs_malloc_replacement)(drd_malloc,
                                 drd___builtin_new,
                                 drd___builtin_new_aligned,
                                 drd___builtin_vec_new,
                                 drd___builtin_vec_new_aligned,
                                 drd_memalign,
                                 drd_calloc,
                                 drd_free,
                                 drd___builtin_delete,
                                 drd___builtin_delete_aligned,
                                 drd___builtin_vec_delete,
                                 drd___builtin_vec_delete_aligned,
                                 drd_realloc,
                                 drd_malloc_usable_size,
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
                "malloc/free: in use at exit: %lu bytes in %lu blocks.\n",
                nbytes, nblocks);
   VG_(message)(Vg_DebugMsg,
                "malloc/free: %lu allocs, %lu frees, %lu bytes allocated.\n",
                s_cmalloc_n_mallocs,
                s_cmalloc_n_frees, s_cmalloc_bs_mallocd);
   if (VG_(clo_verbosity) > 1)
      VG_(message)(Vg_DebugMsg, " \n");
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
