
/*--------------------------------------------------------------------*/
/*--- Ptrcheck: a pointer-use checker.                             ---*/
/*--- Exports for heap access checking.                            ---*/
/*---                                                     h_main.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Ptrcheck, a Valgrind tool for checking pointer
   use in programs.

   Copyright (C) 2003-2010 Nicholas Nethercote
      njn@valgrind.org
   Copyright (C) 2008-2010 OpenWorks Ltd
      info@open-works.co.uk

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

#ifndef __H_MAIN_H

#define __H_MAIN_H

// Choose values that couldn't possibly be pointers
#define NONPTR          ((Seg*)0xA1)
#define UNKNOWN         ((Seg*)0xB2)
#define BOTTOM          ((Seg*)0xC3)

static inline Bool is_known_segment(Seg* teg) {
   return (UNKNOWN != teg && BOTTOM != teg && NONPTR != teg);
   // better?  teg <= BOTTOM
}

void        Seg__cmp(Seg* seg, Addr a, Int* cmp, UWord* n);
Bool        Seg__is_freed(Seg* seg);
ExeContext* Seg__where(Seg* seg);
SizeT       Seg__size(Seg* seg);
Addr        Seg__addr(Seg* seg);

void h_pre_clo_init ( void );
void h_post_clo_init ( void );
void h_fini ( Int exitcode );

void* h_replace_malloc ( ThreadId tid, SizeT n );
void* h_replace___builtin_new ( ThreadId tid, SizeT n );
void* h_replace___builtin_vec_new ( ThreadId tid, SizeT n );
void* h_replace_memalign ( ThreadId tid, SizeT align, SizeT n );
void* h_replace_calloc ( ThreadId tid, SizeT nmemb, SizeT size1 );
void  h_replace_free ( ThreadId tid, void* p );
void  h_replace___builtin_delete ( ThreadId tid, void* p );
void  h_replace___builtin_vec_delete ( ThreadId tid, void* p );
void* h_replace_realloc ( ThreadId tid, void* p_old, SizeT new_size );
SizeT h_replace_malloc_usable_size ( ThreadId tid, void* p );

void h_new_mem_startup( Addr a, SizeT len,
                        Bool rr, Bool ww, Bool xx, ULong di_handle );
void h_new_mem_mmap( Addr a, SizeT len,
                     Bool rr, Bool ww, Bool xx, ULong di_handle );
void h_die_mem_munmap( Addr a, SizeT len );
void h_pre_mem_access ( CorePart part, ThreadId tid, Char* s,
                        Addr base, SizeT size );
void h_pre_mem_read_asciiz ( CorePart part, ThreadId tid, 
                             Char* s, Addr lo );

void h_post_reg_write_demux ( CorePart part, ThreadId tid,
                              PtrdiffT guest_state_offset, SizeT size);
void h_post_reg_write_clientcall(ThreadId tid, PtrdiffT guest_state_offset,
                                 SizeT size, Addr f );

void h_pre_syscall ( ThreadId tid, UInt syscallno,
                     UWord* args, UInt nArgs );
void h_post_syscall ( ThreadId tid, UInt syscallno,
                      UWord* args, UInt nArgs, SysRes res );

/* Note that this also does the sg_ instrumentation. */
IRSB* h_instrument ( VgCallbackClosure* closure,
                     IRSB* sbIn,
                     VexGuestLayout* layout,
                     VexGuestExtents* vge,
                     IRType gWordTy, IRType hWordTy );

#endif

/*--------------------------------------------------------------------*/
/*--- end                                                 h_main.h ---*/
/*--------------------------------------------------------------------*/
