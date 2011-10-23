
/*--------------------------------------------------------------------*/
/*--- Ptrcheck: a pointer-use checker.                             ---*/
/*--- Exports for stack and global access checking.                ---*/
/*---                                                    sg_main.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Ptrcheck, a Valgrind tool for checking pointer
   use in programs.

   Copyright (C) 2008-2011 OpenWorks Ltd
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

#ifndef __SG_MAIN_H

#define __SG_MAIN_H

void sg_pre_clo_init ( void );
void sg_post_clo_init ( void );
void sg_fini(Int exitcode);

void sg_die_mem_stack ( Addr old_SP, SizeT len );
void sg_pre_thread_ll_create ( ThreadId parent, ThreadId child );
void sg_pre_thread_first_insn ( ThreadId tid );

void sg_new_mem_mmap( Addr a, SizeT len,
                      Bool rr, Bool ww, Bool xx, ULong di_handle );
void sg_new_mem_startup( Addr a, SizeT len,
                         Bool rr, Bool ww, Bool xx, ULong di_handle );
void sg_die_mem_munmap ( Addr a, SizeT len );

/* These really ought to be moved elsewhere, so that we don't have to
   include this file in h_main.c.  See comments in sg_main.c and
   h_main.c for what this is about. */

struct _SGEnv;  /* abstract export */

struct _SGEnv* sg_instrument_init ( IRTemp (*newIRTemp_cb)(IRType,void*),
                                    void* newIRTemp_opaque );

void sg_instrument_fini ( struct _SGEnv * env );

void sg_instrument_IRStmt ( /*MOD*/struct _SGEnv * env, 
                            /*MOD*/IRSB* sbOut,
                            IRStmt* st,
                            VexGuestLayout* layout,
                            IRType gWordTy, IRType hWordTy );

void sg_instrument_final_jump ( /*MOD*/struct _SGEnv * env, 
                                /*MOD*/IRSB* sbOut,
                                IRExpr* next,
                                IRJumpKind jumpkind,
                                VexGuestLayout* layout,
                                IRType gWordTy, IRType hWordTy );
#endif

/*--------------------------------------------------------------------*/
/*--- end                                                sg_main.h ---*/
/*--------------------------------------------------------------------*/
