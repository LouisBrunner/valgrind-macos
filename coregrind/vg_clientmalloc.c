
/*--------------------------------------------------------------------*/
/*--- An implementation of malloc/free for the client.             ---*/
/*---                                            vg_clientmalloc.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an extensible x86 protected-mode
   emulator for monitoring program execution on x86-Unixes.

   Copyright (C) 2000-2002 Julian Seward 
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


/*------------------------------------------------------------*/
/*--- Defns                                                ---*/
/*------------------------------------------------------------*/

/* #define DEBUG_CLIENTMALLOC */

/* Holds malloc'd but not freed blocks.  Static, so zero-inited by default. */
#define VG_MALLOCLIST_NO(aa) (((UInt)(aa)) % VG_N_MALLOCLISTS)
static ShadowChunk* vg_malloclist[VG_N_MALLOCLISTS];

/* Stats ... */
static UInt         vg_cmalloc_n_mallocs  = 0;
static UInt         vg_cmalloc_n_frees    = 0;
static UInt         vg_cmalloc_bs_mallocd = 0;

static UInt         vg_mlist_frees = 0;
static UInt         vg_mlist_tries = 0;


/*------------------------------------------------------------*/
/*--- Fns                                                  ---*/
/*------------------------------------------------------------*/

static __inline__
Bool needs_shadow_chunks ( void )
{
   return VG_(needs).core_errors             ||
          VG_(needs).alternative_free        ||
          VG_(needs).sizeof_shadow_block > 0 ||
          VG_(track_events).bad_free         ||
          VG_(track_events).mismatched_free  ||
          VG_(track_events).copy_mem_heap    ||
          VG_(track_events).die_mem_heap;
}

#ifdef DEBUG_CLIENTMALLOC
static 
Int count_malloclists ( void )
{
   ShadowChunk* sc;
   UInt ml_no;
   Int  n = 0;

   for (ml_no = 0; ml_no < VG_N_MALLOCLISTS; ml_no++) 
      for (sc = vg_malloclist[ml_no]; sc != NULL; sc = sc->next)
         n++;
   return n;
}
#endif

/*------------------------------------------------------------*/
/*--- Shadow chunks, etc                                   ---*/
/*------------------------------------------------------------*/

/* Allocate a user-chunk of size bytes.  Also allocate its shadow
   block, make the shadow block point at the user block.  Put the
   shadow chunk on the appropriate list, and set all memory
   protections correctly. */
static void addShadowChunk ( ThreadState* tst,
                             Addr p, UInt size, VgAllocKind kind )
{
   ShadowChunk* sc;
   UInt         ml_no = VG_MALLOCLIST_NO(p);

#  ifdef DEBUG_CLIENTMALLOC
   VG_(printf)("[m %d, f %d (%d)] addShadowChunk "
               "( sz %d, addr %p, list %d )\n", 
               count_malloclists(), 
               0/*count_freelist()*/, 0/*vg_freed_list_volume*/,
               size, p, ml_no );
#  endif

   sc = VG_(arena_malloc)(VG_AR_CORE, 
                          sizeof(ShadowChunk)
                           + VG_(needs).sizeof_shadow_block);
   sc->size      = size;
   sc->allockind = kind;
   sc->data      = p;
   /* Fill in any skin-specific shadow chunk stuff */
   if (VG_(needs).sizeof_shadow_block > 0)
      SK_(complete_shadow_chunk) ( sc, tst );

   sc->next  = vg_malloclist[ml_no];
   vg_malloclist[ml_no] = sc;
}

/* Get the sc, and return the address of the previous node's next pointer
   which allows sc to be removed from the list later without having to look
   it up again.  */
static ShadowChunk* getShadowChunk ( Addr a, /*OUT*/ShadowChunk*** next_ptr )
{
   ShadowChunk *prev, *curr;
   Int ml_no;
   
   ml_no = VG_MALLOCLIST_NO(a);

   prev = NULL;
   curr = vg_malloclist[ml_no];
   while (True) {
      if (curr == NULL) 
         break;
      if (a == curr->data)
         break;
      prev = curr;
      curr = curr->next;
   }

   if (NULL == prev)
      *next_ptr = &vg_malloclist[ml_no];
   else
      *next_ptr = &prev->next;

   return curr;
}

void VG_(free_ShadowChunk) ( ShadowChunk* sc )
{
   VG_(arena_free) ( VG_AR_CLIENT, (void*)sc->data );
   VG_(arena_free) ( VG_AR_CORE,   sc );
}


/* Allocate a suitably-sized array, copy all the malloc-d block
   shadows into it, and return both the array and the size of it.
   This is used by the memory-leak detector.
*/
ShadowChunk** VG_(get_malloc_shadows) ( /*OUT*/ UInt* n_shadows )
{
   UInt          i, scn;
   ShadowChunk** arr;
   ShadowChunk*  sc;
   *n_shadows = 0;
   for (scn = 0; scn < VG_N_MALLOCLISTS; scn++) {
      for (sc = vg_malloclist[scn]; sc != NULL; sc = sc->next) {
         (*n_shadows)++;
      }
   }
   if (*n_shadows == 0) return NULL;

   arr = VG_(malloc)( *n_shadows * sizeof(ShadowChunk*) );

   i = 0;
   for (scn = 0; scn < VG_N_MALLOCLISTS; scn++) {
      for (sc = vg_malloclist[scn]; sc != NULL; sc = sc->next) {
         arr[i++] = sc;
      }
   }
   vg_assert(i == *n_shadows);
   return arr;
}

Bool VG_(addr_is_in_block)( Addr a, Addr start, UInt size )
{
   return (start - VG_AR_CLIENT_REDZONE_SZB <= a
           && a < start + size + VG_AR_CLIENT_REDZONE_SZB);
}

/* Return the first shadow chunk satisfying the predicate p. */
ShadowChunk* VG_(any_matching_mallocd_ShadowChunks)
                        ( Bool (*p) ( ShadowChunk* ))
{
   UInt ml_no;
   ShadowChunk* sc;

   for (ml_no = 0; ml_no < VG_N_MALLOCLISTS; ml_no++)
      for (sc = vg_malloclist[ml_no]; sc != NULL; sc = sc->next)
         if (p(sc))
            return sc;

   return NULL;
}


/*------------------------------------------------------------*/
/*--- client_malloc(), etc                                 ---*/
/*------------------------------------------------------------*/

/* Allocate memory, noticing whether or not we are doing the full
   instrumentation thing. */
static __inline__
void* alloc_and_new_mem ( ThreadState* tst, UInt size, UInt alignment,
                          Bool is_zeroed, VgAllocKind kind )
{
   Addr p;

   VGP_PUSHCC(VgpCliMalloc);

   vg_cmalloc_n_mallocs ++;
   vg_cmalloc_bs_mallocd += size;

   vg_assert(alignment >= 4);
   if (alignment == 4)
      p = (Addr)VG_(arena_malloc)(VG_AR_CLIENT, size);
   else
      p = (Addr)VG_(arena_malloc_aligned)(VG_AR_CLIENT, alignment, size);

   if (needs_shadow_chunks())
      addShadowChunk ( tst, p, size, kind );

   VG_TRACK( ban_mem_heap, p-VG_AR_CLIENT_REDZONE_SZB, 
                           VG_AR_CLIENT_REDZONE_SZB );
   VG_TRACK( new_mem_heap, p, size, is_zeroed );
   VG_TRACK( ban_mem_heap, p+size, VG_AR_CLIENT_REDZONE_SZB );

   VGP_POPCC(VgpCliMalloc);
   return (void*)p;
}

void* VG_(client_malloc) ( ThreadState* tst, UInt size, VgAllocKind kind )
{
   void* p = alloc_and_new_mem ( tst, size, VG_(clo_alignment), 
                                 /*is_zeroed*/False, kind );
#  ifdef DEBUG_CLIENTMALLOC
   VG_(printf)("[m %d, f %d (%d)] client_malloc ( %d, %x ) = %p\n", 
               count_malloclists(), 
               0/*count_freelist()*/, 0/*vg_freed_list_volume*/,
               size, kind, p );
#  endif
   return p;
}


void* VG_(client_memalign) ( ThreadState* tst, UInt align, UInt size )
{
   void* p = alloc_and_new_mem ( tst, size, align, 
                                 /*is_zeroed*/False, Vg_AllocMalloc );
#  ifdef DEBUG_CLIENTMALLOC
   VG_(printf)("[m %d, f %d (%d)] client_memalign ( al %d, sz %d ) = %p\n", 
               count_malloclists(), 
               0/*count_freelist()*/, 0/*vg_freed_list_volume*/,
               align, size, p );
#  endif
   return p;
}


void* VG_(client_calloc) ( ThreadState* tst, UInt nmemb, UInt size1 )
{
   void*        p;
   UInt         size, i;

   size = nmemb * size1;

   p = alloc_and_new_mem ( tst, size, VG_(clo_alignment), 
                              /*is_zeroed*/True, Vg_AllocMalloc );
   /* Must zero block for calloc! */
   for (i = 0; i < size; i++) ((UChar*)p)[i] = 0;

#  ifdef DEBUG_CLIENTMALLOC
   VG_(printf)("[m %d, f %d (%d)] client_calloc ( %d, %d ) = %p\n", 
               count_malloclists(), 
               0/*count_freelist()*/, 0/*vg_freed_list_volume*/,
               nmemb, size1, p );
#  endif

   return p;
}

static
void die_and_free_mem ( ThreadState* tst, ShadowChunk* sc,
                        ShadowChunk** prev_chunks_next_ptr )
{
   /* Note: ban redzones again -- just in case user de-banned them
      with a client request... */
   VG_TRACK( ban_mem_heap, sc->data-VG_AR_CLIENT_REDZONE_SZB, 
                           VG_AR_CLIENT_REDZONE_SZB );
   VG_TRACK( die_mem_heap, sc->data, sc->size );
   VG_TRACK( ban_mem_heap, sc->data+sc->size, VG_AR_CLIENT_REDZONE_SZB );

   /* Remove sc from the malloclist using prev_chunks_next_ptr to
      avoid repeating the hash table lookup.  Can't remove until at least
      after free and free_mismatch errors are done because they use
      describe_addr() which looks for it in malloclist. */
   *prev_chunks_next_ptr = sc->next;

   if (VG_(needs).alternative_free)
      SK_(alt_free) ( sc, tst );
   else
      VG_(free_ShadowChunk) ( sc );
}


void VG_(client_free) ( ThreadState* tst, void* p, VgAllocKind kind )
{
   ShadowChunk*  sc;
   ShadowChunk** prev_chunks_next_ptr;

   VGP_PUSHCC(VgpCliMalloc);

#  ifdef DEBUG_CLIENTMALLOC
   VG_(printf)("[m %d, f %d (%d)] client_free ( %p, %x )\n", 
               count_malloclists(), 
               0/*count_freelist()*/, 0/*vg_freed_list_volume*/,
               p, kind );
#  endif

   vg_cmalloc_n_frees ++;

   if (! needs_shadow_chunks()) {
      VG_(arena_free) ( VG_AR_CLIENT, p );

   } else {
      sc = getShadowChunk ( (Addr)p, &prev_chunks_next_ptr );

      if (sc == NULL) {
         VG_TRACK( bad_free, tst, (Addr)p );
         VGP_POPCC(VgpCliMalloc);
         return;
      }

      /* check if its a matching free() / delete / delete [] */
      if (kind != sc->allockind)
         VG_TRACK( mismatched_free, tst, (Addr)p );

      die_and_free_mem ( tst, sc, prev_chunks_next_ptr );
   } 
   VGP_POPCC(VgpCliMalloc);
}


void* VG_(client_realloc) ( ThreadState* tst, void* p, UInt new_size )
{
   ShadowChunk  *sc;
   ShadowChunk **prev_chunks_next_ptr;
   UInt          i;

   VGP_PUSHCC(VgpCliMalloc);

   vg_cmalloc_n_frees ++;
   vg_cmalloc_n_mallocs ++;
   vg_cmalloc_bs_mallocd += new_size;

   if (! needs_shadow_chunks()) {
      vg_assert(p != NULL && new_size != 0);
      p = VG_(arena_realloc) ( VG_AR_CLIENT, p, VG_(clo_alignment), 
                               new_size );
      VGP_POPCC(VgpCliMalloc);
      return p;

   } else {
      /* First try and find the block. */
      sc = getShadowChunk ( (Addr)p, &prev_chunks_next_ptr );

      if (sc == NULL) {
         VG_TRACK( bad_free, tst, (Addr)p );
         /* Perhaps we should return to the program regardless. */
         VGP_POPCC(VgpCliMalloc);
         return NULL;
      }
     
      /* check if its a matching free() / delete / delete [] */
      if (Vg_AllocMalloc != sc->allockind) {
         /* can not realloc a range that was allocated with new or new [] */
         VG_TRACK( mismatched_free, tst, (Addr)p );
         /* but keep going anyway */
      }

      if (sc->size == new_size) {
         /* size unchanged */
         VGP_POPCC(VgpCliMalloc);
         return p;
         
      } else if (sc->size > new_size) {
         /* new size is smaller */
         VG_TRACK( die_mem_heap, sc->data+new_size, sc->size-new_size );
         sc->size = new_size;
         VGP_POPCC(VgpCliMalloc);
#        ifdef DEBUG_CLIENTMALLOC
         VG_(printf)("[m %d, f %d (%d)] client_realloc_smaller ( %p, %d ) = %p\n", 
                     count_malloclists(), 
                     0/*count_freelist()*/, 0/*vg_freed_list_volume*/,
                     p, new_size, p );
#        endif
         return p;

      } else {
         /* new size is bigger */
         Addr p_new;
         
         /* Get new memory */
         vg_assert(VG_(clo_alignment) >= 4);
         if (VG_(clo_alignment) == 4)
            p_new = (Addr)VG_(arena_malloc)(VG_AR_CLIENT, new_size);
         else
            p_new = (Addr)VG_(arena_malloc_aligned)(VG_AR_CLIENT, 
                                            VG_(clo_alignment), new_size);

         /* First half kept and copied, second half new, 
            red zones as normal */
         VG_TRACK( ban_mem_heap, p_new-VG_AR_CLIENT_REDZONE_SZB, 
                                 VG_AR_CLIENT_REDZONE_SZB );
         VG_TRACK( copy_mem_heap, (Addr)p, p_new, sc->size );
         VG_TRACK( new_mem_heap, p_new+sc->size, new_size-sc->size, 
                   /*inited=*/False );
         VG_TRACK( ban_mem_heap, p_new+new_size, VG_AR_CLIENT_REDZONE_SZB );

         /* Copy from old to new */
         for (i = 0; i < sc->size; i++)
            ((UChar*)p_new)[i] = ((UChar*)p)[i];

         /* Free old memory */
         die_and_free_mem ( tst, sc, prev_chunks_next_ptr );

         /* this has to be after die_and_free_mem, otherwise the
            former succeeds in shorting out the new block, not the
            old, in the case when both are on the same list.  */
         addShadowChunk ( tst, p_new, new_size, Vg_AllocMalloc );

         VGP_POPCC(VgpCliMalloc);
#        ifdef DEBUG_CLIENTMALLOC
         VG_(printf)("[m %d, f %d (%d)] client_realloc_bigger ( %p, %d ) = %p\n", 
                     count_malloclists(), 
                     0/*count_freelist()*/, 0/*vg_freed_list_volume*/,
                     p, new_size, (void*)p_new );
#        endif
         return (void*)p_new;
      }  
   }
}

void VG_(print_malloc_stats) ( void )
{
   UInt         nblocks, nbytes, ml_no;
   ShadowChunk* sc;

   if (VG_(clo_verbosity) == 0)
      return;

   vg_assert(needs_shadow_chunks());

   nblocks = nbytes = 0;

   for (ml_no = 0; ml_no < VG_N_MALLOCLISTS; ml_no++) {
      for (sc = vg_malloclist[ml_no]; sc != NULL; sc = sc->next) {
         nblocks ++;
         nbytes  += sc->size;
      }
   }

   VG_(message)(Vg_UserMsg, 
                "malloc/free: in use at exit: %d bytes in %d blocks.",
                nbytes, nblocks);
   VG_(message)(Vg_UserMsg, 
                "malloc/free: %d allocs, %d frees, %u bytes allocated.",
                vg_cmalloc_n_mallocs,
                vg_cmalloc_n_frees, vg_cmalloc_bs_mallocd);
   if (0)
      VG_(message)(Vg_DebugMsg,
                   "free search: %d tries, %d frees", 
                   vg_mlist_tries, 
                   vg_mlist_frees );
   if (VG_(clo_verbosity) > 1)
      VG_(message)(Vg_UserMsg, "");
}

/*--------------------------------------------------------------------*/
/*--- end                                        vg_clientmalloc.c ---*/
/*--------------------------------------------------------------------*/
