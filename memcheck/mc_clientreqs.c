
/*--------------------------------------------------------------------*/
/*--- For when the client advises Valgrind about memory            ---*/
/*--- permissions.                                                 ---*/
/*---                                              mc_clientreqs.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of MemCheck, a heavyweight Valgrind tool for
   detecting memory errors.

   Copyright (C) 2000-2004 Julian Seward 
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

#include "mc_include.h"

#include "memcheck.h"  /* for VG_USERREQ__* */


/*------------------------------------------------------------*/
/*--- General client block management.                     ---*/
/*------------------------------------------------------------*/

/* This is managed as an expanding array of client block descriptors.
   Indices of live descriptors are issued to the client, so it can ask
   to free them later.  Therefore we cannot slide live entries down
   over dead ones.  Instead we must use free/inuse flags and scan for
   an empty slot at allocation time.  This in turn means allocation is
   relatively expensive, so we hope this does not happen too often. 
*/

typedef
   enum { CG_NotInUse, CG_NoAccess, CG_Writable, CG_Readable }
   CGenBlockKind;

typedef
   struct {
      Addr          start;
      UInt          size;
      ExeContext*   where;
      CGenBlockKind kind;
   } 
   CGenBlock;

/* This subsystem is self-initialising. */
static UInt       vg_cgb_size = 0;
static UInt       vg_cgb_used = 0;
static CGenBlock* vg_cgbs     = NULL;

/* Stats for this subsystem. */
static UInt vg_cgb_used_MAX = 0;   /* Max in use. */
static UInt vg_cgb_allocs   = 0;   /* Number of allocs. */
static UInt vg_cgb_discards = 0;   /* Number of discards. */
static UInt vg_cgb_search   = 0;   /* Number of searches. */


static
Int vg_alloc_client_block ( void )
{
   UInt       i, sz_new;
   CGenBlock* cgbs_new;

   vg_cgb_allocs++;

   for (i = 0; i < vg_cgb_used; i++) {
      vg_cgb_search++;
      if (vg_cgbs[i].kind == CG_NotInUse)
         return i;
   }

   /* Not found.  Try to allocate one at the end. */
   if (vg_cgb_used < vg_cgb_size) {
      vg_cgb_used++;
      return vg_cgb_used-1;
   }

   /* Ok, we have to allocate a new one. */
   sk_assert(vg_cgb_used == vg_cgb_size);
   sz_new = (vg_cgbs == NULL) ? 10 : (2 * vg_cgb_size);

   cgbs_new = VG_(malloc)( sz_new * sizeof(CGenBlock) );
   for (i = 0; i < vg_cgb_used; i++) 
      cgbs_new[i] = vg_cgbs[i];

   if (vg_cgbs != NULL)
      VG_(free)( vg_cgbs );
   vg_cgbs = cgbs_new;

   vg_cgb_size = sz_new;
   vg_cgb_used++;
   if (vg_cgb_used > vg_cgb_used_MAX)
      vg_cgb_used_MAX = vg_cgb_used;
   return vg_cgb_used-1;
}


/*------------------------------------------------------------*/
/*--- Externally visible functions.                        ---*/
/*------------------------------------------------------------*/

void MC_(show_client_block_stats) ( void )
{
   VG_(message)(Vg_DebugMsg, 
      "general CBs: %d allocs, %d discards, %d maxinuse, %d search",
      vg_cgb_allocs, vg_cgb_discards, vg_cgb_used_MAX, vg_cgb_search 
   );
}

static Bool find_addr(VgHashNode* sh_ch, void* ap)
{
  MAC_Chunk *m = (MAC_Chunk*)sh_ch;
  Addr a = *(Addr*)ap;

  return VG_(addr_is_in_block)(a, m->data, m->size);
}

Bool MC_(client_perm_maybe_describe)( Addr a, AddrInfo* ai )
{
   UInt i;
   /* VG_(printf)("try to identify %d\n", a); */

   /* Perhaps it's a general block ? */
   for (i = 0; i < vg_cgb_used; i++) {
      if (vg_cgbs[i].kind == CG_NotInUse) 
         continue;
      if (VG_(addr_is_in_block)(a, vg_cgbs[i].start, vg_cgbs[i].size)) {
         MAC_Mempool **d, *mp;

         /* OK - maybe it's a mempool, too? */
         mp = (MAC_Mempool*)VG_(HT_get_node)(MAC_(mempool_list),
                                             (UInt)vg_cgbs[i].start,
                                             (void*)&d);
         if(mp != NULL) {
            if(mp->chunks != NULL) {
               MAC_Chunk *mc;

               mc = (MAC_Chunk*)VG_(HT_first_match)(mp->chunks, find_addr, &a);
               if(mc != NULL) {
                  ai->akind = UserG;
                  ai->blksize = mc->size;
                  ai->rwoffset = (Int)(a) - (Int)mc->data;
                  ai->lastchange = mc->where;
                  return True;
               }
            }
            ai->akind = Mempool;
            ai->blksize = vg_cgbs[i].size;
            ai->rwoffset  = (Int)(a) - (Int)(vg_cgbs[i].start);
            ai->lastchange = vg_cgbs[i].where;
            return True;
         }
         ai->akind = UserG;
         ai->blksize = vg_cgbs[i].size;
         ai->rwoffset  = (Int)(a) - (Int)(vg_cgbs[i].start);
         ai->lastchange = vg_cgbs[i].where;
         return True;
      }
   }
   return False;
}

Bool SK_(handle_client_request) ( ThreadId tid, UInt* arg, UInt* ret )
{
   Int   i;
   Bool  ok;
   Addr  bad_addr;

   if (!VG_IS_SKIN_USERREQ('M','C',arg[0])
    && VG_USERREQ__MALLOCLIKE_BLOCK != arg[0]
    && VG_USERREQ__FREELIKE_BLOCK   != arg[0]
    && VG_USERREQ__CREATE_MEMPOOL   != arg[0]
    && VG_USERREQ__DESTROY_MEMPOOL  != arg[0]
    && VG_USERREQ__MEMPOOL_ALLOC    != arg[0]
    && VG_USERREQ__MEMPOOL_FREE     != arg[0])
      return False;

   switch (arg[0]) {
      case VG_USERREQ__CHECK_WRITABLE: /* check writable */
         ok = MC_(check_writable) ( arg[1], arg[2], &bad_addr );
         if (!ok)
            MC_(record_user_error) ( tid, bad_addr, True );
         *ret = ok ? (UInt)NULL : bad_addr;
	 break;

      case VG_USERREQ__CHECK_READABLE: /* check readable */
         ok = MC_(check_readable) ( arg[1], arg[2], &bad_addr );
         if (!ok)
            MC_(record_user_error) ( tid, bad_addr, False );
         *ret = ok ? (UInt)NULL : bad_addr;
	 break;

      case VG_USERREQ__DO_LEAK_CHECK:
         MC_(detect_memory_leaks)();
	 *ret = 0; /* return value is meaningless */
	 break;

      case VG_USERREQ__MAKE_NOACCESS: /* make no access */
         i = vg_alloc_client_block();
         /* VG_(printf)("allocated %d %p\n", i, vg_cgbs); */
         vg_cgbs[i].kind  = CG_NoAccess;
         vg_cgbs[i].start = arg[1];
         vg_cgbs[i].size  = arg[2];
         vg_cgbs[i].where = VG_(get_ExeContext) ( tid );
         MC_(make_noaccess) ( arg[1], arg[2] );
	 *ret = i;
	 break;

      case VG_USERREQ__MAKE_WRITABLE: /* make writable */
         i = vg_alloc_client_block();
         vg_cgbs[i].kind  = CG_Writable;
         vg_cgbs[i].start = arg[1];
         vg_cgbs[i].size  = arg[2];
         vg_cgbs[i].where = VG_(get_ExeContext) ( tid );
         MC_(make_writable) ( arg[1], arg[2] );
         *ret = i;
	 break;

      case VG_USERREQ__MAKE_READABLE: /* make readable */
         i = vg_alloc_client_block();
         vg_cgbs[i].kind  = CG_Readable;
         vg_cgbs[i].start = arg[1];
         vg_cgbs[i].size  = arg[2];
         vg_cgbs[i].where = VG_(get_ExeContext) ( tid );
         MC_(make_readable) ( arg[1], arg[2] );
	 *ret = i;
         break;

      case VG_USERREQ__DISCARD: /* discard */
         if (vg_cgbs == NULL 
             || arg[2] >= vg_cgb_used || vg_cgbs[arg[2]].kind == CG_NotInUse)
            return 1;
         sk_assert(arg[2] >= 0 && arg[2] < vg_cgb_used);
         vg_cgbs[arg[2]].kind = CG_NotInUse;
         vg_cgb_discards++;
	 *ret = 0;
	 break;

      case VG_USERREQ__GET_VBITS:
         /* Returns: 1 == OK, 2 == alignment error, 3 == addressing
            error. */
         /* VG_(printf)("get_vbits %p %p %d\n", arg[1], arg[2], arg[3] ); */
         *ret = MC_(get_or_set_vbits_for_client)
                   ( tid, arg[1], arg[2], arg[3], False /* get them */ );
         break;

      case VG_USERREQ__SET_VBITS:
         /* Returns: 1 == OK, 2 == alignment error, 3 == addressing
            error. */
         /* VG_(printf)("set_vbits %p %p %d\n", arg[1], arg[2], arg[3] ); */
         *ret = MC_(get_or_set_vbits_for_client)
                   ( tid, arg[1], arg[2], arg[3], True /* set them */ );
         break;

      default:
         if (MAC_(handle_common_client_requests)(tid, arg, ret )) {
            return True;
         } else {
            VG_(message)(Vg_UserMsg, 
                         "Warning: unknown memcheck client request code %d",
                         arg[0]);
            return False;
         }
   }
   return True;
}


/*--------------------------------------------------------------------*/
/*--- end                                          mc_clientreqs.c ---*/
/*--------------------------------------------------------------------*/
