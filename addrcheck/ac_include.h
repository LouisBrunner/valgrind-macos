
/*--------------------------------------------------------------------*/
/*--- A header file for the AddrCheck skin.                        ---*/
/*---                                                 ac_include.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of AddrCheck, a lightweight Valgrind skin for
   detecting memory errors.

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

#ifndef __AC_INCLUDE_H
#define __AC_INCLUDE_H

#include "vg_skin.h"


/* The classification of a faulting address. */
typedef 
   enum { Undescribed, /* as-yet unclassified */
          Stack, 
          Unknown, /* classification yielded nothing useful */
          Freed, Mallocd
   }
   AcAddrKind;

/* Records info about a faulting address. */
typedef
   struct {
      /* ALL */
      AcAddrKind akind;
      /* Freed, Mallocd */
      Int blksize;
      /* Freed, Mallocd */
      Int rwoffset;
      /* Freed, Mallocd */
      ExeContext* lastchange;
      /* Stack */
      ThreadId stack_tid;
      /* True if is just-below %esp -- could be a gcc bug. */
      Bool maybe_gcc;
   }
   AcAddrInfo;


/*------------------------------------------------------------*/
/*--- Skin-specific command line options + defaults        ---*/
/*------------------------------------------------------------*/

/* Allow loads from partially-valid addresses?  default: YES */
extern Bool SK_(clo_partial_loads_ok);

/* Max volume of the freed blocks queue. */
extern Int SK_(clo_freelist_vol);

/* Do leak check at exit?  default: NO */
extern Bool SK_(clo_leak_check);

/* How closely should we compare ExeContexts in leak records? default: 2 */
extern VgRes SK_(clo_leak_resolution);

/* In leak check, show reachable-but-not-freed blocks?  default: NO */
extern Bool SK_(clo_show_reachable);

/* Assume accesses immediately below %esp are due to gcc-2.96 bugs.
 * default: NO*/
extern Bool SK_(clo_workaround_gcc296_bugs);


/*------------------------------------------------------------*/
/*--- Functions                                            ---*/
/*------------------------------------------------------------*/

/* Functions defined in vg_addrcheck.c */
extern void SK_(helperc_ACCESS4) ( Addr );
extern void SK_(helperc_ACCESS2) ( Addr );
extern void SK_(helperc_ACCESS1) ( Addr );
   
extern void SK_(fpu_ACCESS_check) ( Addr addr, Int size );

extern ShadowChunk* SK_(any_matching_freed_ShadowChunks) 
                        ( Bool (*p) ( ShadowChunk* ) );

/* For client requests */
extern void SK_(make_noaccess) ( Addr a, UInt len );
extern void SK_(make_accessible) ( Addr a, UInt len );

extern Bool SK_(check_accessible) ( Addr a, UInt len, Addr* bad_addr );

extern void SK_(detect_memory_leaks) ( void );


#endif

/*--------------------------------------------------------------------*/
/*--- end                                             ac_include.h ---*/
/*--------------------------------------------------------------------*/

