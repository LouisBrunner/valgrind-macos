
/*--------------------------------------------------------------------*/
/*--- A header file for all parts of the MemCheck tool.            ---*/
/*---                                                 mc_include.h ---*/
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

/* Note: this header should contain declarations that are for use by
   Memcheck only -- declarations shared with Addrcheck go in mac_shared.h.
*/

#ifndef __MC_INCLUDE_H
#define __MC_INCLUDE_H

#include "mac_shared.h"
#include "mc_asm.h"

/*------------------------------------------------------------*/
/*--- Types                                                ---*/
/*------------------------------------------------------------*/

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


/*------------------------------------------------------------*/
/*--- Command line options                                 ---*/
/*------------------------------------------------------------*/

/* DEBUG: clean up instrumented code?  default: YES */
extern Bool MC_(clo_cleanup);

/* When instrumenting, omit some checks if tell-tale literals for
   inlined strlen() are visible in the basic block.  default: YES */
extern Bool MC_(clo_avoid_strlen_errors);


/*------------------------------------------------------------*/
/*--- Functions                                            ---*/
/*------------------------------------------------------------*/

/* Functions defined in mc_helpers.S */
extern void MC_(helper_value_check4_fail) ( void );
extern void MC_(helper_value_check2_fail) ( void );
extern void MC_(helper_value_check1_fail) ( void );
extern void MC_(helper_value_check0_fail) ( void );


/* Functions defined in mc_main.c */
extern REGPARM(2) void MC_(helperc_STOREV4) ( Addr, UInt );
extern REGPARM(2) void MC_(helperc_STOREV2) ( Addr, UInt );
extern REGPARM(2) void MC_(helperc_STOREV1) ( Addr, UInt );
   
extern REGPARM(1) UInt MC_(helperc_LOADV1)  ( Addr );
extern REGPARM(1) UInt MC_(helperc_LOADV2)  ( Addr );
extern REGPARM(1) UInt MC_(helperc_LOADV4)  ( Addr );

extern REGPARM(2) void MC_(fpu_write_check) ( Addr addr, SizeT size );
extern REGPARM(2) void MC_(fpu_read_check)  ( Addr addr, SizeT size );


/* For client requests */
extern void MC_(make_noaccess) ( Addr a, SizeT len );
extern void MC_(make_readable) ( Addr a, SizeT len );
extern void MC_(make_writable) ( Addr a, SizeT len );

extern Bool MC_(check_writable) ( Addr a, SizeT len, Addr* bad_addr );
extern Bool MC_(check_readable) ( Addr a, SizeT len, Addr* bad_addr );

extern void MC_(detect_memory_leaks) ( void );

extern Int  MC_(get_or_set_vbits_for_client) ( 
               ThreadId tid,
               Addr dataV, 
               Addr vbitsV, 
               SizeT size, 
               Bool setting /* True <=> set vbits,  False <=> get vbits */ 
            );

/* Functions defined in mc_clientreqs.c */
extern Bool MC_(client_perm_maybe_describe)( Addr a, AddrInfo* ai );
extern void MC_(show_client_block_stats) ( void );


/* Functions defined in mc_errcontext.c */
extern void MC_(record_value_error)  ( ThreadId tid, Int size );
extern void MC_(record_user_error)   ( ThreadId tid, Addr a, Bool isWrite );


#endif

/*--------------------------------------------------------------------*/
/*--- end                                             mc_include.h ---*/
/*--------------------------------------------------------------------*/

