
/*--------------------------------------------------------------------*/
/*--- An implementation of malloc/free for the client.             ---*/
/*---                                            vg_clientmalloc.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an x86 protected-mode emulator 
   designed for debugging and profiling binaries on x86-Unixes.

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

   The GNU General Public License is contained in the file LICENSE.
*/

#include "vg_include.h"


/*------------------------------------------------------------*/
/*--- Defns                                                ---*/
/*------------------------------------------------------------*/

/* #define DEBUG_CLIENTMALLOC */

/* Holds malloc'd but not freed blocks. */
#define VG_MALLOCLIST_NO(aa) (((UInt)(aa)) % VG_N_MALLOCLISTS)
static ShadowChunk* vg_malloclist[VG_N_MALLOCLISTS];
static Bool         vg_client_malloc_init_done = False;

/* Holds blocks after freeing. */
static ShadowChunk* vg_freed_list_start   = NULL;
static ShadowChunk* vg_freed_list_end     = NULL;
static Int          vg_freed_list_volume  = 0;

/* Stats ... */
static UInt         vg_cmalloc_n_mallocs  = 0;
static UInt         vg_cmalloc_n_frees    = 0;
static UInt         vg_cmalloc_bs_mallocd = 0;

static UInt         vg_mlist_frees = 0;
static UInt         vg_mlist_tries = 0;


/*------------------------------------------------------------*/
/*--- Fns                                                  ---*/
/*------------------------------------------------------------*/

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

   arr = VG_(malloc)( VG_AR_PRIVATE, 
                      *n_shadows * sizeof(ShadowChunk*) );

   i = 0;
   for (scn = 0; scn < VG_N_MALLOCLISTS; scn++) {
      for (sc = vg_malloclist[scn]; sc != NULL; sc = sc->next) {
         arr[i++] = sc;
      }
   }
   vg_assert(i == *n_shadows);
   return arr;
}

static void client_malloc_init ( void )
{
   UInt ml_no;
   if (vg_client_malloc_init_done) return;
   for (ml_no = 0; ml_no < VG_N_MALLOCLISTS; ml_no++)
      vg_malloclist[ml_no] = NULL;
   vg_client_malloc_init_done = True;
}


static __attribute__ ((unused))
       Int count_freelist ( void )
{
   ShadowChunk* sc;
   Int n = 0;
   for (sc = vg_freed_list_start; sc != NULL; sc = sc->next)
      n++;
   return n;
}

static __attribute__ ((unused))
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

static __attribute__ ((unused))
       void freelist_sanity ( void )
{
   ShadowChunk* sc;
   Int n = 0;
   /* VG_(printf)("freelist sanity\n"); */
   for (sc = vg_freed_list_start; sc != NULL; sc = sc->next)
      n += sc->size;
   vg_assert(n == vg_freed_list_volume);
}

/* Remove sc from malloc list # sc.  It is an unchecked error for
   sc not to be present in the list. 
*/
static void remove_from_malloclist ( UInt ml_no, ShadowChunk* sc )
{
   ShadowChunk *sc1, *sc2;
   if (sc == vg_malloclist[ml_no]) {
      vg_malloclist[ml_no] = vg_malloclist[ml_no]->next;
   } else {
      sc1 = vg_malloclist[ml_no];
      vg_assert(sc1 != NULL);
      sc2 = sc1->next;
      while (sc2 != sc) {
         vg_assert(sc2 != NULL);
         sc1 = sc2;
         sc2 = sc2->next;
      }
      vg_assert(sc1->next == sc);
      vg_assert(sc2 == sc);
      sc1->next = sc2->next;
   }
}


/* Put a shadow chunk on the freed blocks queue, possibly freeing up
   some of the oldest blocks in the queue at the same time. */

static void add_to_freed_queue ( ShadowChunk* sc )
{
   ShadowChunk* sc1;

   /* Put it at the end of the freed list */
   if (vg_freed_list_end == NULL) {
      vg_assert(vg_freed_list_start == NULL);
      vg_freed_list_end = vg_freed_list_start = sc;
      vg_freed_list_volume = sc->size;
   } else {
      vg_assert(vg_freed_list_end->next == NULL);
      vg_freed_list_end->next = sc;
      vg_freed_list_end = sc;
      vg_freed_list_volume += sc->size;
   }
   sc->next = NULL;

   /* Release enough of the oldest blocks to bring the free queue
      volume below vg_clo_freelist_vol. */

   while (vg_freed_list_volume > VG_(clo_freelist_vol)) {
      /* freelist_sanity(); */
      vg_assert(vg_freed_list_start != NULL);
      vg_assert(vg_freed_list_end != NULL);

      sc1 = vg_freed_list_start;
      vg_freed_list_volume -= sc1->size;
      /* VG_(printf)("volume now %d\n", vg_freed_list_volume); */
      vg_assert(vg_freed_list_volume >= 0);

      if (vg_freed_list_start == vg_freed_list_end) {
         vg_freed_list_start = vg_freed_list_end = NULL;
      } else {
         vg_freed_list_start = sc1->next;
      }
      sc1->next = NULL; /* just paranoia */
      VG_(free)(VG_AR_CLIENT,  (void*)(sc1->data));
      VG_(free)(VG_AR_PRIVATE, sc1);
   }
}


/* Allocate a user-chunk of size bytes.  Also allocate its shadow
   block, make the shadow block point at the user block.  Put the
   shadow chunk on the appropriate list, and set all memory
   protections correctly. */

static ShadowChunk* client_malloc_shadow ( ThreadState* tst,
                                           UInt align, UInt size, 
                                           VgAllocKind kind )
{
   ShadowChunk* sc;
   Addr         p;
   UInt         ml_no;

#  ifdef DEBUG_CLIENTMALLOC
   VG_(printf)("[m %d, f %d (%d)] client_malloc_shadow ( al %d, sz %d )\n", 
               count_malloclists(), 
               count_freelist(), vg_freed_list_volume,
               align, size );
#  endif

   if (align == 0)
      p = (Addr)VG_(malloc)(VG_AR_CLIENT, size);
   else
      p = (Addr)VG_(malloc_aligned)(VG_AR_CLIENT, align, size);

   sc        = VG_(malloc)(VG_AR_PRIVATE, sizeof(ShadowChunk));
   sc->where = VG_(get_ExeContext)(False, tst->m_eip, tst->m_ebp);
   sc->size  = size;
   sc->allockind = kind;
   sc->data  = p;
   ml_no     = VG_MALLOCLIST_NO(p);
   sc->next  = vg_malloclist[ml_no];
   vg_malloclist[ml_no] = sc;

   VGM_(make_writable)(p, size);
   VGM_(make_noaccess)(p + size, 
                       VG_AR_CLIENT_REDZONE_SZB);
   VGM_(make_noaccess)(p - VG_AR_CLIENT_REDZONE_SZB, 
                       VG_AR_CLIENT_REDZONE_SZB);

   return sc;
}


/* Allocate memory, noticing whether or not we are doing the full
   instrumentation thing. */

void* VG_(client_malloc) ( ThreadState* tst, UInt size, VgAllocKind kind )
{
   ShadowChunk* sc;

   VGP_PUSHCC(VgpCliMalloc);
   client_malloc_init();
#  ifdef DEBUG_CLIENTMALLOC
   VG_(printf)("[m %d, f %d (%d)] client_malloc ( %d, %x )\n", 
               count_malloclists(), 
               count_freelist(), vg_freed_list_volume,
               size, raw_alloc_kind );
#  endif

   vg_cmalloc_n_mallocs ++;
   vg_cmalloc_bs_mallocd += size;

   if (!VG_(clo_instrument)) {
      VGP_POPCC;
      return VG_(malloc) ( VG_AR_CLIENT, size );
   }

   sc = client_malloc_shadow ( tst, 0, size, kind );
   VGP_POPCC;
   return (void*)(sc->data);
}


void* VG_(client_memalign) ( ThreadState* tst, UInt align, UInt size )
{
   ShadowChunk* sc;
   VGP_PUSHCC(VgpCliMalloc);
   client_malloc_init();
#  ifdef DEBUG_CLIENTMALLOC
   VG_(printf)("[m %d, f %d (%d)] client_memalign ( al %d, sz %d )\n", 
               count_malloclists(), 
               count_freelist(), vg_freed_list_volume,
               align, size );
#  endif

   vg_cmalloc_n_mallocs ++;
   vg_cmalloc_bs_mallocd += size;

   if (!VG_(clo_instrument)) {
      VGP_POPCC;
      return VG_(malloc_aligned) ( VG_AR_CLIENT, align, size );
   }
   sc = client_malloc_shadow ( tst, align, size, Vg_AllocMalloc );
   VGP_POPCC;
   return (void*)(sc->data);
}


void VG_(client_free) ( ThreadState* tst, void* ptrV, VgAllocKind kind )
{
   ShadowChunk* sc;
   UInt         ml_no;

   VGP_PUSHCC(VgpCliMalloc);
   client_malloc_init();
#  ifdef DEBUG_CLIENTMALLOC
   VG_(printf)("[m %d, f %d (%d)] client_free ( %p, %x )\n", 
               count_malloclists(), 
               count_freelist(), vg_freed_list_volume,
               ptrV, raw_alloc_kind );
#  endif

   vg_cmalloc_n_frees ++;

   if (!VG_(clo_instrument)) {
      VGP_POPCC;
      VG_(free) ( VG_AR_CLIENT, ptrV );
      return;
   }

   /* first, see if ptrV is one vg_client_malloc gave out. */
   ml_no = VG_MALLOCLIST_NO(ptrV);
   vg_mlist_frees++;
   for (sc = vg_malloclist[ml_no]; sc != NULL; sc = sc->next) {
      vg_mlist_tries++;
      if ((Addr)ptrV == sc->data)
         break;
   }

   if (sc == NULL) {
      VG_(record_free_error) ( tst, (Addr)ptrV );
      VGP_POPCC;
      return;
   }

   /* check if its a matching free() / delete / delete [] */
   if (kind != sc->allockind)
      VG_(record_freemismatch_error) ( tst, (Addr) ptrV );

   /* Remove the shadow chunk from the mallocd list. */
   remove_from_malloclist ( ml_no, sc );

   /* Declare it inaccessible. */
   VGM_(make_noaccess) ( sc->data - VG_AR_CLIENT_REDZONE_SZB, 
                         sc->size + 2*VG_AR_CLIENT_REDZONE_SZB );
   VGM_(make_noaccess) ( (Addr)sc, sizeof(ShadowChunk) );
   sc->where = VG_(get_ExeContext)(False, tst->m_eip, tst->m_ebp);

   /* Put it out of harm's way for a while. */
   add_to_freed_queue ( sc );
   VGP_POPCC;
}



void* VG_(client_calloc) ( ThreadState* tst, UInt nmemb, UInt size1 )
{
   ShadowChunk* sc;
   Addr         p;
   UInt         size, i, ml_no;

   VGP_PUSHCC(VgpCliMalloc);
   client_malloc_init();

#  ifdef DEBUG_CLIENTMALLOC
   VG_(printf)("[m %d, f %d (%d)] client_calloc ( %d, %d )\n", 
               count_malloclists(), 
               count_freelist(), vg_freed_list_volume,
               nmemb, size1 );
#  endif

   vg_cmalloc_n_mallocs ++;
   vg_cmalloc_bs_mallocd += nmemb * size1;

   if (!VG_(clo_instrument)) {
      VGP_POPCC;
      return VG_(calloc) ( VG_AR_CLIENT, nmemb, size1 );
   }

   size      = nmemb * size1;
   p         = (Addr)VG_(malloc)(VG_AR_CLIENT, size);
   sc        = VG_(malloc)(VG_AR_PRIVATE, sizeof(ShadowChunk));
   sc->where = VG_(get_ExeContext)(False, tst->m_eip, tst->m_ebp);
   sc->size  = size;
   sc->allockind = Vg_AllocMalloc; /* its a lie - but true. eat this :) */
   sc->data  = p;
   ml_no     = VG_MALLOCLIST_NO(p);
   sc->next  = vg_malloclist[ml_no];
   vg_malloclist[ml_no] = sc;

   VGM_(make_readable)(p, size);
   VGM_(make_noaccess)(p + size, 
                       VG_AR_CLIENT_REDZONE_SZB);
   VGM_(make_noaccess)(p - VG_AR_CLIENT_REDZONE_SZB, 
                       VG_AR_CLIENT_REDZONE_SZB);

   for (i = 0; i < size; i++) ((UChar*)p)[i] = 0;

   VGP_POPCC;
   return (void*)p;
}


void* VG_(client_realloc) ( ThreadState* tst, void* ptrV, UInt size_new )
{
   ShadowChunk *sc, *sc_new;
   UInt         i, ml_no;

   VGP_PUSHCC(VgpCliMalloc);
   client_malloc_init();

#  ifdef DEBUG_CLIENTMALLOC
   VG_(printf)("[m %d, f %d (%d)] client_realloc ( %p, %d )\n", 
               count_malloclists(), 
               count_freelist(), vg_freed_list_volume,
               ptrV, size_new );
#  endif

   vg_cmalloc_n_frees ++;
   vg_cmalloc_n_mallocs ++;
   vg_cmalloc_bs_mallocd += size_new;

   if (!VG_(clo_instrument)) {
      vg_assert(ptrV != NULL && size_new != 0);
      VGP_POPCC;
      return VG_(realloc) ( VG_AR_CLIENT, ptrV, size_new );
   }

   /* First try and find the block. */
   ml_no = VG_MALLOCLIST_NO(ptrV);
   for (sc = vg_malloclist[ml_no]; sc != NULL; sc = sc->next) {
      if ((Addr)ptrV == sc->data)
         break;
   }
  
   if (sc == NULL) {
      VG_(record_free_error) ( tst, (Addr)ptrV );
      /* Perhaps we should keep going regardless. */
      VGP_POPCC;
      return NULL;
   }

   if (sc->allockind != Vg_AllocMalloc) {
      /* can not realloc a range that was allocated with new or new [] */
      VG_(record_freemismatch_error) ( tst, (Addr)ptrV );
      /* but keep going anyway */
   }

   if (sc->size == size_new) {
      /* size unchanged */
      VGP_POPCC;
      return ptrV;
   }
   if (sc->size > size_new) {
      /* new size is smaller */
      VGM_(make_noaccess)( sc->data + size_new, 
                           sc->size - size_new );
      sc->size = size_new;
      VGP_POPCC;
      return ptrV;
   } else {
      /* new size is bigger */
      sc_new = client_malloc_shadow ( tst, 0, size_new, Vg_AllocMalloc );
      for (i = 0; i < sc->size; i++)
         ((UChar*)(sc_new->data))[i] = ((UChar*)(sc->data))[i];
      VGM_(copy_address_range_perms) ( 
         sc->data, sc_new->data, sc->size );
      remove_from_malloclist ( VG_MALLOCLIST_NO(sc->data), sc );
      VGM_(make_noaccess) ( sc->data - VG_AR_CLIENT_REDZONE_SZB, 
                            sc->size + 2*VG_AR_CLIENT_REDZONE_SZB );
      VGM_(make_noaccess) ( (Addr)sc, sizeof(ShadowChunk) );
      add_to_freed_queue ( sc );
      VGP_POPCC;
      return (void*)sc_new->data;
   }  
}


void VG_(clientmalloc_done) ( void )
{
   UInt         nblocks, nbytes, ml_no;
   ShadowChunk* sc;

   client_malloc_init();

   nblocks = nbytes = 0;

   for (ml_no = 0; ml_no < VG_N_MALLOCLISTS; ml_no++) {
      for (sc = vg_malloclist[ml_no]; sc != NULL; sc = sc->next) {
         nblocks ++;
         nbytes  += sc->size;
      }
   }

   if (VG_(clo_verbosity) == 0)
     return;

   VG_(message)(Vg_UserMsg, 
                "malloc/free: in use at exit: %d bytes in %d blocks.",
                nbytes, nblocks);
   VG_(message)(Vg_UserMsg, 
                "malloc/free: %d allocs, %d frees, %d bytes allocated.",
                vg_cmalloc_n_mallocs,
                vg_cmalloc_n_frees, vg_cmalloc_bs_mallocd);
   if (!VG_(clo_leak_check))
      VG_(message)(Vg_UserMsg, 
                   "For a detailed leak analysis,  rerun with: --leak-check=yes");
   if (0)
      VG_(message)(Vg_DebugMsg,
                   "free search: %d tries, %d frees", 
                   vg_mlist_tries, 
                   vg_mlist_frees );
   if (VG_(clo_verbosity) > 1)
      VG_(message)(Vg_UserMsg, "");
}


/* Describe an address as best you can, for error messages,
   putting the result in ai. */

void VG_(describe_addr) ( Addr a, AddrInfo* ai )
{
   ShadowChunk* sc;
   UInt         ml_no;
   Bool         ok;
   ThreadId     tid;

   /* Perhaps it's a user-def'd block ? */
   ok = VG_(client_perm_maybe_describe)( a, ai );
   if (ok)
      return;
   /* Perhaps it's on a thread's stack? */
   tid = VG_(identify_stack_addr)(a);
   if (tid != VG_INVALID_THREADID) {
      ai->akind     = Stack;
      ai->stack_tid = tid;
      return;
   }
   /* Search for a freed block which might bracket it. */
   for (sc = vg_freed_list_start; sc != NULL; sc = sc->next) {
      if (sc->data - VG_AR_CLIENT_REDZONE_SZB <= a
          && a < sc->data + sc->size + VG_AR_CLIENT_REDZONE_SZB) {
         ai->akind      = Freed;
         ai->blksize    = sc->size;
         ai->rwoffset   = (Int)(a) - (Int)(sc->data);
         ai->lastchange = sc->where;
         return;
      }
   }
   /* Search for a mallocd block which might bracket it. */
   for (ml_no = 0; ml_no < VG_N_MALLOCLISTS; ml_no++) {
      for (sc = vg_malloclist[ml_no]; sc != NULL; sc = sc->next) {
         if (sc->data - VG_AR_CLIENT_REDZONE_SZB <= a
             && a < sc->data + sc->size + VG_AR_CLIENT_REDZONE_SZB) {
            ai->akind      = Mallocd;
            ai->blksize    = sc->size;
            ai->rwoffset   = (Int)(a) - (Int)(sc->data);
            ai->lastchange = sc->where;
            return;
         }
      }
   }
   /* Clueless ... */
   ai->akind = Unknown;
   return;
}


/*--------------------------------------------------------------------*/
/*--- end                                        vg_clientmalloc.c ---*/
/*--------------------------------------------------------------------*/
