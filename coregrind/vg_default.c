
/*--------------------------------------------------------------------*/
/*--- Default panicky definitions of template functions that skins ---*/
/*--- should override.                                             ---*/
/*---                                                vg_defaults.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an extensible x86 protected-mode
   emulator for monitoring program execution on x86-Unixes.

   Copyright (C) 2000-2003 Nicholas Nethercote
      njn25@cam.ac.uk

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


/* These functions aren't intended to be run.  Replacement functions used by
 * the chosen skin are substituted by compiling the skin into a .so and
 * LD_PRELOADing it.  Nasty :) */

#include "vg_include.h"

/* ---------------------------------------------------------------------
   Error messages (for malformed skins)
   ------------------------------------------------------------------ */

/* If the skin fails to define one or more of the required functions,
 * make it very clear what went wrong! */

static __attribute__ ((noreturn))
void fund_panic ( Char* fn )
{
   VG_(printf)(
      "\nSkin error:\n"
      "  The skin you have selected is missing the function `%s',\n"
      "  which is required.\n\n",
      fn);
   VG_(skin_panic)("Missing skin function");
}

static __attribute__ ((noreturn))
void non_fund_panic ( Char* fn )
{
   VG_(printf)(
      "\nSkin error:\n"
      "  The skin you have selected is missing the function `%s'\n"
      "  required by one of its needs.\n\n",
      fn);
   VG_(skin_panic)("Missing skin function");
}

static __attribute__ ((noreturn))
void malloc_panic ( Char* fn )
{
   VG_(printf)(
      "\nSkin error:\n"
      "  The skin you have selected is missing the function `%s'\n"
      "  required because it is replacing malloc() et al.\n\n",
      fn);
   VG_(skin_panic)("Missing skin function");
}

#define FUND(proto)                       \
__attribute__((weak))                     \
proto                                     \
{                                         \
   fund_panic(__PRETTY_FUNCTION__);       \
}

#define NON_FUND(proto)                   \
__attribute__((weak))                     \
proto                                     \
{                                         \
   non_fund_panic(__PRETTY_FUNCTION__);   \
}

#define MALLOC(proto)                     \
__attribute__((weak))                     \
proto                                     \
{                                         \
   malloc_panic(__PRETTY_FUNCTION__);     \
}

/* ---------------------------------------------------------------------
   Default functions
   ------------------------------------------------------------------ */

/* Fundamental template functions */
FUND( void        SK_(pre_clo_init) (void) );
FUND( void        SK_(post_clo_init)(void) );
FUND( UCodeBlock* SK_(instrument)   (UCodeBlock* cb, Addr not_used) );
FUND( void        SK_(fini)         (Int exitcode) );

/* For error reporting and suppression handling */
NON_FUND( Bool  SK_(eq_SkinError)(VgRes res, Error* e1, Error* e2) );
NON_FUND( void  SK_(pp_SkinError)(Error* err) );
NON_FUND( UInt  SK_(update_extra)(Error* err) );
NON_FUND( Bool  SK_(recognised_suppression)(Char* name, Supp* su) );
NON_FUND( Bool  SK_(read_extra_suppression_info)(Int fd, Char* buf, Int nBuf,
                                                 Supp* su) );
NON_FUND( Bool  SK_(error_matches_suppression)(Error* err, Supp* su) );
NON_FUND( Char* SK_(get_error_name)(Error* err) );
NON_FUND( void  SK_(print_extra_suppression_info)(Error* err) );

/* For throwing out basic block level info when code is invalidated */
NON_FUND( void SK_(discard_basic_block_info)(Addr a, UInt size) );

/* For throwing out basic block level info when code is invalidated */
NON_FUND( void SK_(written_shadow_regs_values)(UInt* gen_reg, UInt* eflags) );

/* Command line arg handling functions */
NON_FUND( Bool SK_(process_cmd_line_option)(Char* argv) );
NON_FUND( void SK_(print_usage)(void) );
NON_FUND( void SK_(print_debug_usage)(void) );

/* Client request template function */
NON_FUND( Bool SK_(handle_client_request)(ThreadState* tst, UInt* arg_block,
                                          UInt *ret) );

/* UCode extension */
NON_FUND( void  SK_(emit_XUInstr)  (UInstr* u, RRegSet regs_live_before) );
NON_FUND( Bool  SK_(sane_XUInstr)  (Bool beforeRA, Bool beforeLiveness, 
                                    UInstr* u) );
NON_FUND( Char* SK_(name_XUOpcode) (Opcode opc) );
NON_FUND( void  SK_(pp_XUInstr)    (UInstr* u) );
NON_FUND( Int   SK_(get_Xreg_usage)(UInstr* u, Tag tag, Int* regs,
                                    Bool* isWrites) );               

/* Syscall wrapping */
NON_FUND( void* SK_(pre_syscall) (ThreadId tid, UInt syscallno,
                                  Bool is_blocking) );
NON_FUND( void  SK_(post_syscall)(ThreadId tid, UInt syscallno,
                                 void* pre_result, Int res, Bool is_blocking) );

/* Sanity checks */
NON_FUND( Bool SK_(cheap_sanity_check)(void) );
NON_FUND( Bool SK_(expensive_sanity_check)(void) );

/*------------------------------------------------------------*/
/*--- Replacing malloc et al                               ---*/
/*------------------------------------------------------------*/

/* Default redzone for CLIENT arena of Valgrind's malloc() is 4 bytes */
__attribute__ ((weak))
UInt VG_(vg_malloc_redzone_szB) = 4;

Bool VG_(sk_malloc_called_by_scheduler) = False;

/* If the skin hasn't replaced malloc(), this one can be called from the
   scheduler, for the USERREQ__MALLOC user request used by vg_libpthread.c. 
   (Nb: it cannot call glibc's malloc().)  The lock variable ensures that the
   scheduler is the only place this can be called from;  this ensures that a
   malloc()-replacing skin cannot forget to implement SK_(malloc)() or
   SK_(free)().  */
__attribute__ ((weak))
void* SK_(malloc)( ThreadState* tst, Int size )
{
   if (VG_(sk_malloc_called_by_scheduler))
      return VG_(cli_malloc)(4, size);
   else 
      malloc_panic(__PRETTY_FUNCTION__);
}

__attribute__ ((weak))
void  SK_(free)( ThreadState* tst, void* p )
{
   /* see comment for SK_(malloc)() above */
   if (VG_(sk_malloc_called_by_scheduler))
      VG_(cli_free)(p);
   else 
      malloc_panic(__PRETTY_FUNCTION__);
}

MALLOC( void* SK_(__builtin_new)    ( ThreadState* tst, Int size ) );
MALLOC( void* SK_(__builtin_vec_new)( ThreadState* tst, Int size ) );
MALLOC( void* SK_(memalign)         ( ThreadState* tst, Int align, Int size ) );
MALLOC( void* SK_(calloc)           ( ThreadState* tst, Int nmemb, Int size ) );

MALLOC( void  SK_(__builtin_delete)     ( ThreadState* tst, void* p ) );
MALLOC( void  SK_(__builtin_vec_delete) ( ThreadState* tst, void* p ) );
MALLOC( void* SK_(realloc)              ( ThreadState* tst, void* p,
                                          Int new_size ) );

/*--------------------------------------------------------------------*/
/*--- end                                            vg_defaults.c ---*/
/*--------------------------------------------------------------------*/
