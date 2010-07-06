
/*--------------------------------------------------------------------*/
/*--- Malloc replacement.                     replacemalloc_core.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2000-2010 Julian Seward 
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

#include "pub_core_basics.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcprint.h"
#include "pub_core_mallocfree.h"
#include "pub_core_options.h"
#include "pub_core_replacemalloc.h"

/*------------------------------------------------------------*/
/*--- Command line options                                 ---*/
/*------------------------------------------------------------*/

/* Nb: the allocator always rounds blocks up to a multiple of
   VG_MIN_MALLOC_SZB.
*/

/* DEBUG: print malloc details?  default: NO */
Bool VG_(clo_trace_malloc)  = False;

/* Minimum alignment in functions that don't specify alignment explicitly.
   default: 0, i.e. use VG_MIN_MALLOC_SZB. */
UInt VG_(clo_alignment)     = VG_MIN_MALLOC_SZB;


Bool VG_(replacement_malloc_process_cmd_line_option)(Char* arg)
{
   if VG_INT_CLO(arg, "--alignment", VG_(clo_alignment)) {
      if (VG_(clo_alignment) < VG_MIN_MALLOC_SZB ||
          VG_(clo_alignment) > 4096 ||
          VG_(log2)( VG_(clo_alignment) ) == -1 /* not a power of 2 */)
      {
         VG_(fmsg_bad_option)(arg,
            "Alignment must be a power of 2 in the range %d..4096.\n",
            VG_MIN_MALLOC_SZB);
      }
   }

   else if VG_BOOL_CLO(arg, "--trace-malloc",  VG_(clo_trace_malloc)) {}
   else 
      return False;

   return True;
}

/*------------------------------------------------------------*/
/*--- Useful functions                                     ---*/
/*------------------------------------------------------------*/

void* VG_(cli_malloc) ( SizeT align, SizeT nbytes )                 
{                                                                             
   // 'align' should be valid (ie. big enough and a power of two) by now.
   // VG_(arena_memalign)() will abort if it's not.
   if (VG_MIN_MALLOC_SZB == align)
      return VG_(arena_malloc)   ( VG_AR_CLIENT, "replacemalloc.cm.1", 
                                   nbytes ); 
   else                                                                       
      return VG_(arena_memalign) ( VG_AR_CLIENT, "replacemalloc.cm.2", 
                                   align, nbytes );
}                                                                             

void VG_(cli_free) ( void* p )                                   
{                                                                             
   VG_(arena_free) ( VG_AR_CLIENT, p );                          
}

Bool VG_(addr_is_in_block)( Addr a, Addr start, SizeT size, SizeT rz_szB )
{
   return ( start - rz_szB <= a  &&  a < start + size + rz_szB );
}

/*--------------------------------------------------------------------*/
/*--- end                                                          ---*/
/*--------------------------------------------------------------------*/
