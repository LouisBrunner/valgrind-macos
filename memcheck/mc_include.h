
/*--------------------------------------------------------------------*/
/*--- A header file for all parts of the MemCheck skin.            ---*/
/*---                                                 mc_include.h ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of MemCheck, a heavyweight Valgrind skin for
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

#ifndef __MC_INCLUDE_H
#define __MC_INCLUDE_H

#include "vg_skin.h"

/* UCode extension for efficient memory checking operations */
typedef
   enum {
      /* uinstrs which are not needed for mere translation of x86 code,
         only for instrumentation of it. */
      LOADV = DUMMY_FINAL_UOPCODE + 1,
      STOREV,
      GETV,
      PUTV,
      TESTV,
      SETV, 
      /* Get/set the v-bit (and it is only one bit) for the simulated
         %eflags register. */
      GETVF,
      PUTVF,

      /* Do a unary or binary tag op.  Only for post-instrumented
         code.  For TAG1, first and only arg is a TempReg, and is both
         arg and result reg.  For TAG2, first arg is src, second is
         dst, in the normal way; both are TempRegs.  In both cases,
         3rd arg is a RiCHelper with a Lit16 tag.  This indicates
         which tag op to do. */
      TAG1,
      TAG2
   }
   MemCheckOpcode;


/* Lists the names of value-tag operations used in instrumented
   code.  These are the third argument to TAG1 and TAG2 uinsns. */
typedef
   enum { 
     /* Unary. */
     Tag_PCast40, Tag_PCast20, Tag_PCast10,
     Tag_PCast01, Tag_PCast02, Tag_PCast04,

     Tag_PCast14, Tag_PCast12, Tag_PCast11,

     Tag_Left4, Tag_Left2, Tag_Left1,

     Tag_SWiden14, Tag_SWiden24, Tag_SWiden12,
     Tag_ZWiden14, Tag_ZWiden24, Tag_ZWiden12,

     /* Binary; 1st is rd; 2nd is rd+wr */
     Tag_UifU4, Tag_UifU2, Tag_UifU1, Tag_UifU0,
     Tag_DifD4, Tag_DifD2, Tag_DifD1,

     Tag_ImproveAND4_TQ, Tag_ImproveAND2_TQ, Tag_ImproveAND1_TQ,
     Tag_ImproveOR4_TQ, Tag_ImproveOR2_TQ, Tag_ImproveOR1_TQ,
     Tag_DebugFn
   }
   TagOp;

/* The classification of a faulting address. */
typedef 
   enum { Undescribed, /* as-yet unclassified */
          Stack, 
          Unknown, /* classification yielded nothing useful */
          Freed, Mallocd, 
          UserG, UserS 
   }
   AddrKind;

/* Records info about a faulting address. */
typedef
   struct {
      /* ALL */
      AddrKind akind;
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
   AddrInfo;


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

/* Shall we V-check addrs? (they are always A checked too)   default: YES */
extern Bool SK_(clo_check_addrVs);

/* DEBUG: clean up instrumented code?  default: YES */
extern Bool SK_(clo_cleanup);

/* When instrumenting, omit some checks if tell-tale literals for
   inlined strlen() are visible in the basic block.  default: YES */
extern Bool SK_(clo_avoid_strlen_errors);


/*------------------------------------------------------------*/
/*--- Functions                                            ---*/
/*------------------------------------------------------------*/

/* Functions defined in vg_memcheck_helpers.S */
extern void SK_(helper_value_check4_fail) ( void );
extern void SK_(helper_value_check2_fail) ( void );
extern void SK_(helper_value_check1_fail) ( void );
extern void SK_(helper_value_check0_fail) ( void );

/* Functions defined in vg_memcheck.c */
extern void SK_(helperc_STOREV4) ( UInt, Addr );
extern void SK_(helperc_STOREV2) ( UInt, Addr );
extern void SK_(helperc_STOREV1) ( UInt, Addr );
   
extern UInt SK_(helperc_LOADV1) ( Addr );
extern UInt SK_(helperc_LOADV2) ( Addr );
extern UInt SK_(helperc_LOADV4) ( Addr );

extern void SK_(fpu_write_check) ( Addr addr, Int size );
extern void SK_(fpu_read_check)  ( Addr addr, Int size );

extern ShadowChunk* SK_(any_matching_freed_ShadowChunks) 
                        ( Bool (*p) ( ShadowChunk* ) );

/* For client requests */
extern void SK_(make_noaccess) ( Addr a, UInt len );
extern void SK_(make_readable) ( Addr a, UInt len );
extern void SK_(make_writable) ( Addr a, UInt len );

extern Bool SK_(check_writable) ( Addr a, UInt len, Addr* bad_addr );
extern Bool SK_(check_readable) ( Addr a, UInt len, Addr* bad_addr );

extern void SK_(detect_memory_leaks) ( void );


/* Functions defined in vg_memcheck_clientreqs.c */
extern Bool SK_(client_perm_maybe_describe)( Addr a, AddrInfo* ai );
extern void SK_(delete_client_stack_blocks_following_ESP_change) ( void );
extern void SK_(show_client_block_stats) ( void );

/* Functions defined in vg_memcheck_errcontext.c */
extern void SK_(record_value_error)       ( Int size );
extern void SK_(record_address_error)     ( Addr a, Int size, Bool isWrite );
extern void SK_(record_core_mem_error)    ( ThreadState* tst, Bool isWrite,
                                            Char* s );
extern void SK_(record_param_error)       ( ThreadState* tst, Addr a,   
                                            Bool isWriteLack, Char* msg );
extern void SK_(record_jump_error)        ( ThreadState* tst, Addr a );
extern void SK_(record_free_error)        ( ThreadState* tst, Addr a );
extern void SK_(record_freemismatch_error)( ThreadState* tst, Addr a );
extern void SK_(record_user_error)        ( ThreadState* tst, Addr a, 
                                            Bool isWrite );

#endif

/*--------------------------------------------------------------------*/
/*--- end                                             mc_include.h ---*/
/*--------------------------------------------------------------------*/

