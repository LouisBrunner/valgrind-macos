
/*--------------------------------------------------------------------*/
/*--- C startup stuff, reached from vg_startup.S.                  ---*/
/*---                                                    vg_main.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, an x86 protected-mode emulator 
   designed for debugging and profiling binaries on x86-Unixes.

   Copyright (C) 2000-2002 Julian Seward 
      jseward@acm.org
      Julian_Seward@muraroa.demon.co.uk

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
#include "vg_constants.h"
#include "vg_version.h"


/* ---------------------------------------------------------------------
   Compute offsets into baseBlock.  See comments in vg_include.h.
   ------------------------------------------------------------------ */

/* The variables storing offsets. */

#define INVALID_OFFSET (-1)

Int VGOFF_(m_eax) = INVALID_OFFSET;
Int VGOFF_(m_ecx) = INVALID_OFFSET;
Int VGOFF_(m_edx) = INVALID_OFFSET;
Int VGOFF_(m_ebx) = INVALID_OFFSET;
Int VGOFF_(m_esp) = INVALID_OFFSET;
Int VGOFF_(m_ebp) = INVALID_OFFSET;
Int VGOFF_(m_esi) = INVALID_OFFSET;
Int VGOFF_(m_edi) = INVALID_OFFSET;
Int VGOFF_(m_eflags) = INVALID_OFFSET;
Int VGOFF_(m_fpustate) = INVALID_OFFSET;
Int VGOFF_(m_eip) = INVALID_OFFSET;
Int VGOFF_(spillslots) = INVALID_OFFSET;
Int VGOFF_(sh_eax) = INVALID_OFFSET;
Int VGOFF_(sh_ecx) = INVALID_OFFSET;
Int VGOFF_(sh_edx) = INVALID_OFFSET;
Int VGOFF_(sh_ebx) = INVALID_OFFSET;
Int VGOFF_(sh_esp) = INVALID_OFFSET;
Int VGOFF_(sh_ebp) = INVALID_OFFSET;
Int VGOFF_(sh_esi) = INVALID_OFFSET;
Int VGOFF_(sh_edi) = INVALID_OFFSET;
Int VGOFF_(sh_eflags) = INVALID_OFFSET;
Int VGOFF_(helper_idiv_64_32) = INVALID_OFFSET;
Int VGOFF_(helper_div_64_32) = INVALID_OFFSET;
Int VGOFF_(helper_idiv_32_16) = INVALID_OFFSET;
Int VGOFF_(helper_div_32_16) = INVALID_OFFSET;
Int VGOFF_(helper_idiv_16_8) = INVALID_OFFSET;
Int VGOFF_(helper_div_16_8) = INVALID_OFFSET;
Int VGOFF_(helper_imul_32_64) = INVALID_OFFSET;
Int VGOFF_(helper_mul_32_64) = INVALID_OFFSET;
Int VGOFF_(helper_imul_16_32) = INVALID_OFFSET;
Int VGOFF_(helper_mul_16_32) = INVALID_OFFSET;
Int VGOFF_(helper_imul_8_16) = INVALID_OFFSET;
Int VGOFF_(helper_mul_8_16) = INVALID_OFFSET;
Int VGOFF_(helper_CLD) = INVALID_OFFSET;
Int VGOFF_(helper_STD) = INVALID_OFFSET;
Int VGOFF_(helper_get_dirflag) = INVALID_OFFSET;
Int VGOFF_(helper_shldl) = INVALID_OFFSET;
Int VGOFF_(helper_shldw) = INVALID_OFFSET;
Int VGOFF_(helper_shrdl) = INVALID_OFFSET;
Int VGOFF_(helper_shrdw) = INVALID_OFFSET;
Int VGOFF_(helper_RDTSC) = INVALID_OFFSET;
Int VGOFF_(helper_CPUID) = INVALID_OFFSET;
Int VGOFF_(helper_BSWAP) = INVALID_OFFSET;
Int VGOFF_(helper_bt) = INVALID_OFFSET;
Int VGOFF_(helper_bts) = INVALID_OFFSET;
Int VGOFF_(helper_btr) = INVALID_OFFSET;
Int VGOFF_(helper_btc) = INVALID_OFFSET;
Int VGOFF_(helper_bsf) = INVALID_OFFSET;
Int VGOFF_(helper_bsr) = INVALID_OFFSET;
Int VGOFF_(helper_fstsw_AX) = INVALID_OFFSET;
Int VGOFF_(helper_SAHF) = INVALID_OFFSET;
Int VGOFF_(helper_value_check4_fail) = INVALID_OFFSET;
Int VGOFF_(helper_value_check2_fail) = INVALID_OFFSET;
Int VGOFF_(helper_value_check1_fail) = INVALID_OFFSET;
Int VGOFF_(helper_value_check0_fail) = INVALID_OFFSET;
Int VGOFF_(helper_do_syscall) = INVALID_OFFSET;
Int VGOFF_(helper_do_client_request) = INVALID_OFFSET;
Int VGOFF_(helperc_LOADV4) = INVALID_OFFSET;
Int VGOFF_(helperc_LOADV2) = INVALID_OFFSET;
Int VGOFF_(helperc_LOADV1) = INVALID_OFFSET;
Int VGOFF_(helperc_STOREV4) = INVALID_OFFSET;
Int VGOFF_(helperc_STOREV2) = INVALID_OFFSET;
Int VGOFF_(helperc_STOREV1) = INVALID_OFFSET;
Int VGOFF_(handle_esp_assignment) = INVALID_OFFSET;
Int VGOFF_(fpu_write_check) = INVALID_OFFSET;
Int VGOFF_(fpu_read_check) = INVALID_OFFSET;
Int VGOFF_(helper_request_normal_exit) = INVALID_OFFSET;


/* This is the actual defn of baseblock. */
UInt VG_(baseBlock)[VG_BASEBLOCK_WORDS];

/* Words. */
static Int baB_off = 0;

/* Returns the offset, in words. */
static Int alloc_BaB ( Int words )
{
   Int off = baB_off;
   baB_off += words;
   if (baB_off >= VG_BASEBLOCK_WORDS)
      VG_(panic)( "alloc_BaB: baseBlock is too small");

   return off;   
}

/* Allocate 1 word in baseBlock and set it to the given value. */
static Int alloc_BaB_1_set ( Addr a )
{
   Int off = alloc_BaB(1);
   VG_(baseBlock)[off] = (UInt)a;
   return off;
}


/* Here we assign actual offsets.  It's important to get the most
   popular referents within 128 bytes of the start, so we can take
   advantage of short addressing modes relative to %ebp.  Popularity
   of offsets was measured on 22 Feb 02 running a KDE application, and
   the slots rearranged accordingly, with a 1.5% reduction in total
   size of translations. */

static void vg_init_baseBlock ( void )
{
   baB_off = 0;

   /* Those with offsets under 128 are carefully chosen. */

   /* WORD offsets in this column */
   /* 0   */ VGOFF_(m_eax)     = alloc_BaB(1);
   /* 1   */ VGOFF_(m_ecx)     = alloc_BaB(1);
   /* 2   */ VGOFF_(m_edx)     = alloc_BaB(1);
   /* 3   */ VGOFF_(m_ebx)     = alloc_BaB(1);
   /* 4   */ VGOFF_(m_esp)     = alloc_BaB(1);
   /* 5   */ VGOFF_(m_ebp)     = alloc_BaB(1);
   /* 6   */ VGOFF_(m_esi)     = alloc_BaB(1);
   /* 7   */ VGOFF_(m_edi)     = alloc_BaB(1);
   /* 8   */ VGOFF_(m_eflags)  = alloc_BaB(1);

   /* 9   */ VGOFF_(sh_eax)    = alloc_BaB(1);
   /* 10  */ VGOFF_(sh_ecx)    = alloc_BaB(1);
   /* 11  */ VGOFF_(sh_edx)    = alloc_BaB(1);
   /* 12  */ VGOFF_(sh_ebx)    = alloc_BaB(1);
   /* 13  */ VGOFF_(sh_esp)    = alloc_BaB(1);
   /* 14  */ VGOFF_(sh_ebp)    = alloc_BaB(1);
   /* 15  */ VGOFF_(sh_esi)    = alloc_BaB(1);
   /* 16  */ VGOFF_(sh_edi)    = alloc_BaB(1);
   /* 17  */ VGOFF_(sh_eflags) = alloc_BaB(1);

   /* 18  */ 
   VGOFF_(helper_value_check4_fail) 
      = alloc_BaB_1_set( (Addr) & VG_(helper_value_check4_fail) );
   /* 19 */
   VGOFF_(helper_value_check0_fail)
      = alloc_BaB_1_set( (Addr) & VG_(helper_value_check0_fail) );

   /* 20  */
   VGOFF_(helperc_STOREV4)
      = alloc_BaB_1_set( (Addr) & VG_(helperc_STOREV4) );
   /* 21  */
   VGOFF_(helperc_STOREV1)
      = alloc_BaB_1_set( (Addr) & VG_(helperc_STOREV1) );

   /* 22  */
   VGOFF_(helperc_LOADV4)
      = alloc_BaB_1_set( (Addr) & VG_(helperc_LOADV4) );
   /* 23  */
   VGOFF_(helperc_LOADV1)
      = alloc_BaB_1_set( (Addr) & VG_(helperc_LOADV1) );

   /* 24  */
   VGOFF_(handle_esp_assignment)
      = alloc_BaB_1_set( (Addr) & VGM_(handle_esp_assignment) );

   /* 25 */
   VGOFF_(m_eip) = alloc_BaB(1);

   /* There are currently 24 spill slots */
   /* 26 .. 49  This overlaps the magic boundary at >= 32 words, but
      most spills are to low numbered spill slots, so the ones above
      the boundary don't see much action. */
   VGOFF_(spillslots) = alloc_BaB(VG_MAX_SPILLSLOTS);

   /* These two pushed beyond the boundary because 2-byte transactions
      are rare. */
   /* 50  */
   VGOFF_(helperc_STOREV2)
      = alloc_BaB_1_set( (Addr) & VG_(helperc_STOREV2) );
   /* 51  */
   VGOFF_(helperc_LOADV2)
      = alloc_BaB_1_set( (Addr) & VG_(helperc_LOADV2) );

   /* 52  */
   VGOFF_(fpu_write_check)
      = alloc_BaB_1_set( (Addr) & VGM_(fpu_write_check) );
   /* 53  */
   VGOFF_(fpu_read_check)
      = alloc_BaB_1_set( (Addr) & VGM_(fpu_read_check) );

   /* Actually I don't think these two are ever used. */
   /* 54  */ 
   VGOFF_(helper_value_check2_fail)
      = alloc_BaB_1_set( (Addr) & VG_(helper_value_check2_fail) );
   /* 55  */ 
   VGOFF_(helper_value_check1_fail)
      = alloc_BaB_1_set( (Addr) & VG_(helper_value_check1_fail) );

   /* I gave up counting at this point.  Since they're way above the
      short-amode-boundary, there's no point. */

   VGOFF_(m_fpustate) = alloc_BaB(VG_SIZE_OF_FPUSTATE_W);

   VGOFF_(helper_idiv_64_32)
      = alloc_BaB_1_set( (Addr) & VG_(helper_idiv_64_32) );
   VGOFF_(helper_div_64_32)
      = alloc_BaB_1_set( (Addr) & VG_(helper_div_64_32) );
   VGOFF_(helper_idiv_32_16)
      = alloc_BaB_1_set( (Addr) & VG_(helper_idiv_32_16) );
   VGOFF_(helper_div_32_16)
      = alloc_BaB_1_set( (Addr) & VG_(helper_div_32_16) );
   VGOFF_(helper_idiv_16_8)
      = alloc_BaB_1_set( (Addr) & VG_(helper_idiv_16_8) );
   VGOFF_(helper_div_16_8)
      = alloc_BaB_1_set( (Addr) & VG_(helper_div_16_8) );

   VGOFF_(helper_imul_32_64)
      = alloc_BaB_1_set( (Addr) & VG_(helper_imul_32_64) );
   VGOFF_(helper_mul_32_64)
      = alloc_BaB_1_set( (Addr) & VG_(helper_mul_32_64) );
   VGOFF_(helper_imul_16_32)
      = alloc_BaB_1_set( (Addr) & VG_(helper_imul_16_32) );
   VGOFF_(helper_mul_16_32)
      = alloc_BaB_1_set( (Addr) & VG_(helper_mul_16_32) );
   VGOFF_(helper_imul_8_16)
      = alloc_BaB_1_set( (Addr) & VG_(helper_imul_8_16) );
   VGOFF_(helper_mul_8_16)
      = alloc_BaB_1_set( (Addr) & VG_(helper_mul_8_16) );

   VGOFF_(helper_CLD)
      = alloc_BaB_1_set( (Addr) & VG_(helper_CLD) );
   VGOFF_(helper_STD)
      = alloc_BaB_1_set( (Addr) & VG_(helper_STD) );
   VGOFF_(helper_get_dirflag)
      = alloc_BaB_1_set( (Addr) & VG_(helper_get_dirflag) );

   VGOFF_(helper_shldl)
      = alloc_BaB_1_set( (Addr) & VG_(helper_shldl) );
   VGOFF_(helper_shldw)
      = alloc_BaB_1_set( (Addr) & VG_(helper_shldw) );
   VGOFF_(helper_shrdl)
      = alloc_BaB_1_set( (Addr) & VG_(helper_shrdl) );
   VGOFF_(helper_shrdw)
      = alloc_BaB_1_set( (Addr) & VG_(helper_shrdw) );

   VGOFF_(helper_RDTSC)
      = alloc_BaB_1_set( (Addr) & VG_(helper_RDTSC) );
   VGOFF_(helper_CPUID)
      = alloc_BaB_1_set( (Addr) & VG_(helper_CPUID) );

   VGOFF_(helper_bt)
      = alloc_BaB_1_set( (Addr) & VG_(helper_bt) );
   VGOFF_(helper_bts)
      = alloc_BaB_1_set( (Addr) & VG_(helper_bts) );
   VGOFF_(helper_btr)
      = alloc_BaB_1_set( (Addr) & VG_(helper_btr) );
   VGOFF_(helper_btc)
      = alloc_BaB_1_set( (Addr) & VG_(helper_btc) );

   VGOFF_(helper_bsf)
      = alloc_BaB_1_set( (Addr) & VG_(helper_bsf) );
   VGOFF_(helper_bsr)
      = alloc_BaB_1_set( (Addr) & VG_(helper_bsr) );

   VGOFF_(helper_fstsw_AX)
      = alloc_BaB_1_set( (Addr) & VG_(helper_fstsw_AX) );
   VGOFF_(helper_SAHF)
      = alloc_BaB_1_set( (Addr) & VG_(helper_SAHF) );

   VGOFF_(helper_request_normal_exit)
      = alloc_BaB_1_set( (Addr) & VG_(helper_request_normal_exit) );

   VGOFF_(helper_do_syscall)
      = alloc_BaB_1_set( (Addr) & VG_(helper_do_syscall) );
   VGOFF_(helper_do_client_request)
      = alloc_BaB_1_set( (Addr) & VG_(helper_do_client_request) );
}


/* ---------------------------------------------------------------------
   Global entities which are not referenced from generated code.
   ------------------------------------------------------------------ */

/* The stack on which Valgrind runs.  We can't use the same stack as
   the simulatee -- that's an important design decision.  */
UInt VG_(stack)[10000];

/* Ditto our signal delivery stack. */
UInt VG_(sigstack)[10000];

/* Saving stuff across system calls. */
UInt VG_(real_fpu_state_saved_over_syscall_d1)[VG_SIZE_OF_FPUSTATE_W];
UInt VG_(real_fpu_state_saved_over_syscall_d2)[VG_SIZE_OF_FPUSTATE_W];
Addr VG_(esp_saved_over_syscall_d1);
Addr VG_(esp_saved_over_syscall_d2);

/* Counts downwards in vg_run_innerloop. */
UInt VG_(dispatch_ctr);

/* If vg_dispatch_ctr is set to 1 to force a stop, its
   previous value is saved here. */
UInt VG_(dispatch_ctr_SAVED);

/* This is why vg_run_innerloop() exited. */
UInt VG_(interrupt_reason);

/* vg_oursignalhandler() might longjmp().  Here's the jmp_buf. */
jmp_buf VG_(toploop_jmpbuf);
/* ... and if so, here's the signal which caused it to do so. */
Int     VG_(longjmpd_on_signal);

/* 64-bit counter for the number of basic blocks done. */
ULong VG_(bbs_done);
/* 64-bit counter for the number of bbs to go before a debug exit. */
ULong VG_(bbs_to_go);

/* Produce debugging output? */
Bool VG_(disassemble) = False;

/* The current LRU epoch. */
UInt VG_(current_epoch) = 0;


/* ---------------------------------------------------------------------
   Counters, for informational purposes only.
   ------------------------------------------------------------------ */

/* Number of lookups which miss the fast tt helper. */
UInt VG_(tt_fast_misses) = 0;


/* Counts for LRU informational messages. */

/* Number and total o/t size of new translations this epoch. */
UInt VG_(this_epoch_in_count) = 0;
UInt VG_(this_epoch_in_osize) = 0;
UInt VG_(this_epoch_in_tsize) = 0;
/* Number and total o/t size of discarded translations this epoch. */
UInt VG_(this_epoch_out_count) = 0;
UInt VG_(this_epoch_out_osize) = 0;
UInt VG_(this_epoch_out_tsize) = 0;
/* Number and total o/t size of translations overall. */
UInt VG_(overall_in_count) = 0;
UInt VG_(overall_in_osize) = 0;
UInt VG_(overall_in_tsize) = 0;
/* Number and total o/t size of discards overall. */
UInt VG_(overall_out_count) = 0;
UInt VG_(overall_out_osize) = 0;
UInt VG_(overall_out_tsize) = 0;

/* The number of LRU-clearings of TT/TC. */
UInt VG_(number_of_lrus) = 0;


/* Counts pertaining to the register allocator. */

/* total number of uinstrs input to reg-alloc */
UInt VG_(uinstrs_prealloc) = 0;

/* total number of uinstrs added due to spill code */
UInt VG_(uinstrs_spill) = 0;

/* number of bbs requiring spill code */
UInt VG_(translations_needing_spill) = 0;

/* total of register ranks over all translations */
UInt VG_(total_reg_rank) = 0;


/* Counts pertaining to the self-modifying-code detection machinery. */

/* Total number of writes checked. */
UInt VG_(smc_total_check4s) = 0;

/* Number of writes which the fast smc check couldn't show were
   harmless. */
UInt VG_(smc_cache_passed) = 0;

/* Numnber of writes which really did write on original code. */
UInt VG_(smc_fancy_passed) = 0;

/* Number of translations discarded as a result. */
UInt VG_(smc_discard_count) = 0;


/* Counts pertaining to internal sanity checking. */

UInt VG_(sanity_fast_count) = 0;
UInt VG_(sanity_slow_count) = 0;



/* ---------------------------------------------------------------------
   Values derived from command-line options.
   ------------------------------------------------------------------ */

Bool   VG_(clo_GDB_attach);
Int    VG_(sanity_level);
Int    VG_(clo_verbosity);
Bool   VG_(clo_demangle);
Bool   VG_(clo_leak_check);
Bool   VG_(clo_show_reachable);
Int    VG_(clo_leak_resolution);
Bool   VG_(clo_sloppy_malloc);
Bool   VG_(clo_partial_loads_ok);
Bool   VG_(clo_trace_children);
Int    VG_(clo_logfile_fd);
Int    VG_(clo_freelist_vol);
Bool   VG_(clo_workaround_gcc296_bugs);
Int    VG_(clo_n_suppressions);
Char*  VG_(clo_suppressions)[VG_CLO_MAX_SFILES];
Bool   VG_(clo_single_step);
Bool   VG_(clo_optimise);
Bool   VG_(clo_instrument);
Bool   VG_(clo_cleanup);
Bool   VG_(clo_client_perms);
Int    VG_(clo_smc_check);
Bool   VG_(clo_trace_syscalls);
Bool   VG_(clo_trace_signals);
Bool   VG_(clo_trace_symtab);
Bool   VG_(clo_trace_malloc);
ULong  VG_(clo_stop_after);
Int    VG_(clo_dump_error);
Int    VG_(clo_backtrace_size);

/* This Bool is needed by wrappers in vg_clientmalloc.c to decide how
   to behave.  Initially we say False. */
Bool VG_(running_on_simd_CPU) = False;

/* Holds client's %esp at the point we gained control. */
Addr VG_(esp_at_startup);

/* As deduced from VG_(esp_at_startup), the client's argc, argv[] and
   envp[] as extracted from the client's stack at startup-time. */
Int    VG_(client_argc);
Char** VG_(client_argv);
Char** VG_(client_envp);

/* A place into which to copy the value of env var VG_ARGS, so we
   don't have to modify the original. */
static Char vg_cmdline_copy[M_VG_CMDLINE_STRLEN];


/* ---------------------------------------------------------------------
   Top level simulation loop.
   ------------------------------------------------------------------ */

/* Create a translation of the client basic block beginning at
   orig_addr, and add it to the translation cache & translation table.
   This probably doesn't really belong here, but, hey ... */
void VG_(create_translation_for) ( Addr orig_addr )
{
   Addr    trans_addr;
   TTEntry tte;
   Int orig_size, trans_size;
   /* Ensure there is space to hold a translation. */
   VG_(maybe_do_lru_pass)();
   VG_(translate)( orig_addr, &orig_size, &trans_addr, &trans_size );
   /* Copy data at trans_addr into the translation cache.
      Returned pointer is to the code, not to the 4-byte
      header. */
   /* Since the .orig_size and .trans_size fields are
      UShort, be paranoid. */
   vg_assert(orig_size > 0 && orig_size < 65536);
   vg_assert(trans_size > 0 && trans_size < 65536);
   tte.orig_size  = orig_size;
   tte.orig_addr  = orig_addr;
   tte.trans_size = trans_size;
   tte.trans_addr = VG_(copy_to_transcache)
                       ( trans_addr, trans_size );
   tte.mru_epoch  = VG_(current_epoch);
   /* Free the intermediary -- was allocated by VG_(emit_code). */
   VG_(jitfree)( (void*)trans_addr );
   /* Add to trans tab and set back pointer. */
   VG_(add_to_trans_tab) ( &tte );
   /* Update stats. */
   VG_(this_epoch_in_count) ++;
   VG_(this_epoch_in_osize) += orig_size;
   VG_(this_epoch_in_tsize) += trans_size;
   VG_(overall_in_count) ++;
   VG_(overall_in_osize) += orig_size;
   VG_(overall_in_tsize) += trans_size;
   /* Record translated area for SMC detection. */
   VG_(smc_mark_original) ( 
      VG_(baseBlock)[VGOFF_(m_eip)], orig_size );
}


/* Runs the client program from %EIP (baseBlock[off_eip]) until it
   asks to exit, or until vg_bbs_to_go jumps have happened (the latter
   case is for debugging).  */

void VG_(toploop) ( void )
{
   volatile UInt dispatch_ctr_SAVED;
   volatile Int  done_this_time;

   /* For the LRU structures, records when the epoch began. */
   volatile ULong epoch_started_at = 0;

   while (True) {
     next_outer_loop:

      /* Age the LRU structures if an epoch has been completed. */
      if (VG_(bbs_done) - epoch_started_at >= VG_BBS_PER_EPOCH) {
         VG_(current_epoch)++;
         epoch_started_at = VG_(bbs_done);
         if (VG_(clo_verbosity) > 2) {
            UInt tt_used, tc_used;
            VG_(get_tt_tc_used) ( &tt_used, &tc_used );
            VG_(message)(Vg_UserMsg,
               "%lu bbs, in: %d (%d -> %d), out %d (%d -> %d), TT %d, TC %d",
               VG_(bbs_done), 
               VG_(this_epoch_in_count),
               VG_(this_epoch_in_osize),
               VG_(this_epoch_in_tsize),
               VG_(this_epoch_out_count),
               VG_(this_epoch_out_osize),
               VG_(this_epoch_out_tsize),
               tt_used, tc_used
            );
	 }
         VG_(this_epoch_in_count) = 0;
         VG_(this_epoch_in_osize) = 0;
         VG_(this_epoch_in_tsize) = 0;
         VG_(this_epoch_out_count) = 0;
         VG_(this_epoch_out_osize) = 0;
         VG_(this_epoch_out_tsize) = 0;
      }

      /* Figure out how many bbs to ask vg_run_innerloop to do. */
      if (VG_(bbs_to_go) >= VG_SIGCHECK_INTERVAL)
         VG_(dispatch_ctr) = 1 + VG_SIGCHECK_INTERVAL;
      else
         VG_(dispatch_ctr) = 1 + (UInt)VG_(bbs_to_go);

      /* ... and remember what we asked for. */
      dispatch_ctr_SAVED = VG_(dispatch_ctr);

      /* Now have a go at doing them. */
      VG_(interrupt_reason) = VG_Y_SIGCHECK;
      if (__builtin_setjmp(VG_(toploop_jmpbuf)) == 0) {
         /* try this ... */
         VG_(run_innerloop)();
         /* We get here if the client didn't take a fault. */
         switch (VG_(interrupt_reason)) {
            case VG_Y_SIGCHECK:
               /* The counter fell to zero and no other situation has
                  been detected. */
               vg_assert(VG_(dispatch_ctr) == 0);
               done_this_time  = dispatch_ctr_SAVED - 1;
               VG_(bbs_to_go)  -= (ULong)done_this_time;
               VG_(bbs_done)   += (ULong)done_this_time;
               /* Exit if the debug run has ended. */
               if (VG_(bbs_to_go) == 0) goto debug_stop;
               VG_(deliver_signals)();
               VG_(do_sanity_checks)(False);
               goto next_outer_loop;
            case VG_Y_EXIT:
               /* The target program tried to exit. */
               done_this_time = dispatch_ctr_SAVED - VG_(dispatch_ctr_SAVED);
               done_this_time --;
               VG_(bbs_to_go)   -= (ULong)done_this_time;
               VG_(bbs_done)    += (ULong)done_this_time;
               return;
            case VG_Y_SMC:
               /* A write to original code was detected. */
               done_this_time = dispatch_ctr_SAVED - VG_(dispatch_ctr_SAVED);
               VG_(bbs_to_go)   -= (ULong)done_this_time;
               VG_(bbs_done)    += (ULong)done_this_time;
               VG_(flush_transtab)();
               goto next_outer_loop;
            case VG_Y_TRANSLATE: {
               /* Need to provide a translation of code at vg_m_eip. */
               done_this_time = dispatch_ctr_SAVED - VG_(dispatch_ctr);
               vg_assert(done_this_time > 0);
               done_this_time --;
               VG_(bbs_to_go) -= (ULong)done_this_time;
               VG_(bbs_done)  += (ULong)done_this_time;
               VG_(create_translation_for)(VG_(baseBlock)[VGOFF_(m_eip)]);
               goto next_outer_loop;
            }
            default:
               VG_(panic)("vg_toploop: invalid interrupt reason");
         }
      } else {
        /* We get here if the client took a fault, which caused our
           signal handler to longjmp. */
         done_this_time = dispatch_ctr_SAVED - VG_(dispatch_ctr);
         VG_(bbs_to_go)   -= (ULong)done_this_time;
         VG_(bbs_done)    += (ULong)done_this_time;
         if (VG_(interrupt_reason) == VG_Y_EXIT) return;
         VG_(deliver_signals)();
         VG_(do_sanity_checks)(False);
         VG_(unblock_host_signal)(VG_(longjmpd_on_signal));
      }
   }

   /* NOTREACHED */

  debug_stop:
   /* If we exited because of a debug stop, print the translation 
      of the last block executed -- by translating it again, and 
      throwing away the result. */
   VG_(printf)(
      "======vvvvvvvv====== LAST TRANSLATION ======vvvvvvvv======\n");
   VG_(translate)( VG_(baseBlock)[VGOFF_(m_eip)], NULL, NULL, NULL );
   VG_(printf)("\n");
   VG_(printf)(
      "======^^^^^^^^====== LAST TRANSLATION ======^^^^^^^^======\n");
}


/* ---------------------------------------------------------------------
   Processing of command-line options.
   ------------------------------------------------------------------ */

static void bad_option ( Char* opt )
{
   VG_(shutdown_logging)();
   VG_(clo_logfile_fd) = 2; /* stderr */
   VG_(printf)("valgrind.so: Bad option `%s'; aborting.\n", opt);
   VG_(exit)(1);
}

static void config_error ( Char* msg )
{
   VG_(shutdown_logging)();
   VG_(clo_logfile_fd) = 2; /* stderr */
   VG_(printf)("valgrind.so: Startup or configuration error:\n\t%s\n", msg);
   VG_(printf)("valgrind.so: Unable to start up properly.  Giving up.\n");
   VG_(exit)(1);
}


static void process_cmd_line_options ( void )
{
   UChar* argv[M_VG_CMDLINE_OPTS];
   UInt   argc;
   UChar* p;
   UChar* str;
   Int    i, eventually_logfile_fd;

#  define ISSPACE(cc)      ((cc) == ' ' || (cc) == '\t' || (cc) == '\n')
#  define STREQ(s1,s2)     (0==VG_(strcmp_ws)((s1),(s2)))
#  define STREQN(nn,s1,s2) (0==VG_(strncmp_ws)((s1),(s2),(nn)))

   /* Set defaults. */
   VG_(clo_GDB_attach)       = False;
   VG_(sanity_level)         = 1;
   VG_(clo_verbosity)        = 1;
   VG_(clo_demangle)         = True;
   VG_(clo_leak_check)       = False;
   VG_(clo_show_reachable)   = False;
   VG_(clo_leak_resolution)  = 2;
   VG_(clo_sloppy_malloc)    = False;
   VG_(clo_partial_loads_ok) = True;
   VG_(clo_trace_children)   = False;
   VG_(clo_logfile_fd)       = 2; /* stderr */
   VG_(clo_freelist_vol)     = 1000000;
   VG_(clo_workaround_gcc296_bugs) = False;
   VG_(clo_n_suppressions)   = 0;
   VG_(clo_single_step)      = False;
   VG_(clo_optimise)         = True;
   VG_(clo_instrument)       = True;
   VG_(clo_cleanup)          = True;
   VG_(clo_client_perms)     = False;
   VG_(clo_smc_check)        = /* VG_CLO_SMC_SOME */ VG_CLO_SMC_NONE;
   VG_(clo_trace_syscalls)   = False;
   VG_(clo_trace_signals)    = False;
   VG_(clo_trace_symtab)     = False;
   VG_(clo_trace_malloc)     = False;
   VG_(clo_stop_after)       = 1000000000000LL;
   VG_(clo_dump_error)       = 0;
   VG_(clo_backtrace_size)   = 4;

   eventually_logfile_fd = VG_(clo_logfile_fd);

   /* Once logging is started, we can safely send messages pertaining
      to failures in initialisation. */
   VG_(startup_logging)();

   /* Magically find the client's argc/argv/envp.  This kludge is
      entirely dependent on the stack layout imposed by libc at
      startup.  Hence the magic offsets.  Then check (heuristically)
      that the results are plausible.  There must be a better way to
      do this ... */

#  if 0
   /* Use this to search for the correct offsets if the tests below
      barf. */
   { Int i;
     VG_(printf)("startup %%esp is %p\n", VG_(esp_at_startup) );
     for (i = 0; i < 10; i++) {
        Char* p = ((Char**)VG_(esp_at_startup))[i];
        VG_(printf)("%d:  %p\n", i, p);
     }
   }
#  endif

   /* These offsets (5,6,7) are right for my RedHat 7.2 (glibc-2.2.4)
      box. */

   VG_(client_argc) = (Int)   ( ((void**)VG_(esp_at_startup)) [5] );
   VG_(client_argv) = (Char**)( ((void**)VG_(esp_at_startup)) [6] );
   VG_(client_envp) = (Char**)( ((void**)VG_(esp_at_startup)) [7] );

   if ( ((UInt)VG_(client_argc)) > 0 &&
        ((UInt)VG_(client_argc)) < 10000 &&
        (Addr)VG_(client_argv) >= 0x8000000 &&
        (Addr)VG_(client_envp) >= 0x8000000)
      goto argc_argv_envp_OK;

   /* If that's no good, try some other offsets discovered by KDE
      folks on 8 Feb 02:
      For glibc > 2.2.4 the offset 9/10/11 did the trick. Coolo found
      out those, on I think a Caldera 3.1 with glibc 2.2.4 -- the same
      offsets worked for on a debian sid with glibc 2.2.5.  */

   VG_(client_argc) = (Int)   ( ((void**)VG_(esp_at_startup)) [9] );
   VG_(client_argv) = (Char**)( ((void**)VG_(esp_at_startup)) [10] );
   VG_(client_envp) = (Char**)( ((void**)VG_(esp_at_startup)) [11] );

   if ( ((UInt)VG_(client_argc)) > 0 &&
        ((UInt)VG_(client_argc)) < 10000 &&
        (Addr)VG_(client_argv) >= 0x8000000 &&
        (Addr)VG_(client_envp) >= 0x8000000)
      goto argc_argv_envp_OK;

   /* Doesn't look promising.  Try offsets for RedHat 6.2
      (glibc-2.1.3) instead.  In this case, the argv and envp vectors
      are actually on the stack (bizarrely). */

   VG_(client_argc) = (Int)      ( ((void**)VG_(esp_at_startup)) [4] );
   VG_(client_argv) = (Char**) & ( ((void**)VG_(esp_at_startup)) [5] );
   VG_(client_envp) 
      = (Char**) & ( ((void**)VG_(esp_at_startup)) [6 + VG_(client_argc)] );

   if ( ((UInt)VG_(client_argc)) > 0 &&
        ((UInt)VG_(client_argc)) < 10000 &&
        (Addr)VG_(client_argv) >= 0x8000000 &&
        (Addr)VG_(client_envp) >= 0x8000000)
      goto argc_argv_envp_OK;

   /* Here's yet another variant, from <hansen> (irc.kde.org). */

   VG_(client_argc) = (Int)      ( ((void**)VG_(esp_at_startup)) [9] );
   VG_(client_argv) = (Char**) & ( ((void**)VG_(esp_at_startup)) [10] );
   VG_(client_envp) 
      = (Char**) & ( ((void**)VG_(esp_at_startup)) [11 + VG_(client_argc)] );

   if ( ((UInt)VG_(client_argc)) > 0 &&
        ((UInt)VG_(client_argc)) < 10000 &&
        (Addr)VG_(client_argv) >= 0x8000000 &&
        (Addr)VG_(client_envp) >= 0x8000000)
      goto argc_argv_envp_OK;

   /* VG_(printf)("%d %p %p\n", VG_(client_argc), VG_(client_argv), 
                                                  VG_(client_envp));
   */
   /* We're hosed.  Give up :-( */
   config_error(
      "Can't get plausible values for client's argc/argv/envp.\n\t"
      "You may be able to fix this; see process_cmd_line_options()\n\t"
      "in vg_main.c"
   );
   /* NOTREACHED */

  argc_argv_envp_OK:

   /* Now that VG_(client_envp) has been set, we can extract the args
      for Valgrind itself.  Copy into global var so that we don't have to
      write zeroes to the getenv'd value itself. */
   str = VG_(getenv)("VG_ARGS");
   argc = 0;

   if (!str) {
      config_error("Can't read options from env var VG_ARGS.");
   }

   if (VG_(strlen)(str) >= M_VG_CMDLINE_STRLEN-1) {
      config_error("Command line length exceeds M_CMDLINE_STRLEN.");
   }
   VG_(strcpy)(vg_cmdline_copy, str);
   str = NULL;

   p = &vg_cmdline_copy[0];
   while (True) {
      while (ISSPACE(*p)) { *p = 0; p++; }
      if (*p == 0) break;
      if (argc < M_VG_CMDLINE_OPTS-1) { 
         argv[argc] = p; argc++; 
      } else {
         config_error(
            "Found more than M_CMDLINE_OPTS command-line opts.");
      }
      while (*p != 0 && !ISSPACE(*p)) p++;
   }

   for (i = 0; i < argc; i++) {

      if (STREQ(argv[i], "-v") || STREQ(argv[i], "--verbose"))
         VG_(clo_verbosity)++;
      else if (STREQ(argv[i], "-q") || STREQ(argv[i], "--quiet"))
         VG_(clo_verbosity)--;

      else if (STREQ(argv[i], "--gdb-attach=yes"))
         VG_(clo_GDB_attach) = True;
      else if (STREQ(argv[i], "--gdb-attach=no"))
         VG_(clo_GDB_attach) = False;

      else if (STREQ(argv[i], "--demangle=yes"))
         VG_(clo_demangle) = True;
      else if (STREQ(argv[i], "--demangle=no"))
         VG_(clo_demangle) = False;

      else if (STREQ(argv[i], "--partial-loads-ok=yes"))
         VG_(clo_partial_loads_ok) = True;
      else if (STREQ(argv[i], "--partial-loads-ok=no"))
         VG_(clo_partial_loads_ok) = False;

      else if (STREQ(argv[i], "--leak-check=yes"))
         VG_(clo_leak_check) = True;
      else if (STREQ(argv[i], "--leak-check=no"))
         VG_(clo_leak_check) = False;

      else if (STREQ(argv[i], "--show-reachable=yes"))
         VG_(clo_show_reachable) = True;
      else if (STREQ(argv[i], "--show-reachable=no"))
         VG_(clo_show_reachable) = False;

      else if (STREQ(argv[i], "--leak-resolution=low"))
         VG_(clo_leak_resolution) = 2;
      else if (STREQ(argv[i], "--leak-resolution=med"))
         VG_(clo_leak_resolution) = 4;
      else if (STREQ(argv[i], "--leak-resolution=high"))
         VG_(clo_leak_resolution) = VG_DEEPEST_BACKTRACE;

      else if (STREQ(argv[i], "--sloppy-malloc=yes"))
         VG_(clo_sloppy_malloc) = True;
      else if (STREQ(argv[i], "--sloppy-malloc=no"))
         VG_(clo_sloppy_malloc) = False;

      else if (STREQ(argv[i], "--trace-children=yes"))
         VG_(clo_trace_children) = True;
      else if (STREQ(argv[i], "--trace-children=no"))
         VG_(clo_trace_children) = False;

      else if (STREQ(argv[i], "--workaround-gcc296-bugs=yes"))
         VG_(clo_workaround_gcc296_bugs) = True;
      else if (STREQ(argv[i], "--workaround-gcc296-bugs=no"))
         VG_(clo_workaround_gcc296_bugs) = False;

      else if (STREQN(15, argv[i], "--sanity-level="))
         VG_(sanity_level) = (Int)VG_(atoll)(&argv[i][15]);

      else if (STREQN(13, argv[i], "--logfile-fd="))
         eventually_logfile_fd = (Int)VG_(atoll)(&argv[i][13]);

      else if (STREQN(15, argv[i], "--freelist-vol=")) {
         VG_(clo_freelist_vol) = (Int)VG_(atoll)(&argv[i][15]);
         if (VG_(clo_freelist_vol) < 0) VG_(clo_freelist_vol) = 2;
      }

      else if (STREQN(15, argv[i], "--suppressions=")) {
         if (VG_(clo_n_suppressions) >= VG_CLO_MAX_SFILES) {
            VG_(message)(Vg_UserMsg, "Too many logfiles specified.");
            VG_(message)(Vg_UserMsg, 
                         "Increase VG_CLO_MAX_SFILES and recompile.");
            bad_option(argv[i]);
         }
         VG_(clo_suppressions)[VG_(clo_n_suppressions)] = &argv[i][15];
         VG_(clo_n_suppressions)++;
      }
      else if (STREQ(argv[i], "--single-step=yes"))
         VG_(clo_single_step) = True;
      else if (STREQ(argv[i], "--single-step=no"))
         VG_(clo_single_step) = False;

      else if (STREQ(argv[i], "--optimise=yes"))
         VG_(clo_optimise) = True;
      else if (STREQ(argv[i], "--optimise=no"))
         VG_(clo_optimise) = False;

      else if (STREQ(argv[i], "--instrument=yes"))
         VG_(clo_instrument) = True;
      else if (STREQ(argv[i], "--instrument=no"))
         VG_(clo_instrument) = False;

      else if (STREQ(argv[i], "--cleanup=yes"))
         VG_(clo_cleanup) = True;
      else if (STREQ(argv[i], "--cleanup=no"))
         VG_(clo_cleanup) = False;

      else if (STREQ(argv[i], "--client-perms=yes"))
         VG_(clo_client_perms) = True;
      else if (STREQ(argv[i], "--client-perms=no"))
         VG_(clo_client_perms) = False;

      else if (STREQ(argv[i], "--smc-check=none"))
         VG_(clo_smc_check) = VG_CLO_SMC_NONE;
      else if (STREQ(argv[i], "--smc-check=some"))
         VG_(clo_smc_check) = VG_CLO_SMC_SOME;
      else if (STREQ(argv[i], "--smc-check=all"))
         VG_(clo_smc_check) = VG_CLO_SMC_ALL;

      else if (STREQ(argv[i], "--trace-syscalls=yes"))
         VG_(clo_trace_syscalls) = True;
      else if (STREQ(argv[i], "--trace-syscalls=no"))
         VG_(clo_trace_syscalls) = False;

      else if (STREQ(argv[i], "--trace-signals=yes"))
         VG_(clo_trace_signals) = True;
      else if (STREQ(argv[i], "--trace-signals=no"))
         VG_(clo_trace_signals) = False;

      else if (STREQ(argv[i], "--trace-symtab=yes"))
         VG_(clo_trace_symtab) = True;
      else if (STREQ(argv[i], "--trace-symtab=no"))
         VG_(clo_trace_symtab) = False;

      else if (STREQ(argv[i], "--trace-malloc=yes"))
         VG_(clo_trace_malloc) = True;
      else if (STREQ(argv[i], "--trace-malloc=no"))
         VG_(clo_trace_malloc) = False;

      else if (STREQN(13, argv[i], "--stop-after="))
         VG_(clo_stop_after) = VG_(atoll)(&argv[i][13]);

      else if (STREQN(13, argv[i], "--dump-error="))
         VG_(clo_dump_error) = (Int)VG_(atoll)(&argv[i][13]);

      else if (STREQN(14, argv[i], "--num-callers=")) {
         /* Make sure it's sane. */
	 VG_(clo_backtrace_size) = (Int)VG_(atoll)(&argv[i][14]);
         if (VG_(clo_backtrace_size) < 2)
            VG_(clo_backtrace_size) = 2;
         if (VG_(clo_backtrace_size) >= VG_DEEPEST_BACKTRACE)
            VG_(clo_backtrace_size) = VG_DEEPEST_BACKTRACE;
      }

      else
         bad_option(argv[i]);
   }

#  undef ISSPACE
#  undef STREQ
#  undef STREQN

   if (VG_(clo_verbosity < 0))
      VG_(clo_verbosity) = 0;

   if (VG_(clo_GDB_attach) && VG_(clo_trace_children)) {
      VG_(message)(Vg_UserMsg, "");
      VG_(message)(Vg_UserMsg, 
         "--gdb-attach=yes conflicts with --trace-children=yes");
      VG_(message)(Vg_UserMsg, 
         "Please choose one or the other, but not both.");
      bad_option("--gdb-attach=yes and --trace-children=yes");
   }

   if (VG_(clo_client_perms) && !VG_(clo_instrument)) {
      VG_(message)(Vg_UserMsg, "");
      VG_(message)(Vg_UserMsg, 
         "--client-perms=yes requires --instrument=yes");
      bad_option("--client-perms=yes without --instrument=yes");
   }

   if (VG_(clo_client_perms))
      vg_assert(VG_(clo_instrument));

   VG_(clo_logfile_fd) = eventually_logfile_fd;

#  define STRINGIFY(xx)  __STRING(xx)
   if (VG_(clo_verbosity > 0))
      VG_(message)(Vg_UserMsg, 
                   "valgrind-%s, a memory error detector for x86 GNU/Linux.",
                   STRINGIFY(VG_VERSION));
#  undef STRINGIFY
   if (VG_(clo_verbosity > 0))
      VG_(message)(Vg_UserMsg, 
                   "Copyright (C) 2000-2002, and GNU GPL'd, by Julian Seward.");
   if (VG_(clo_verbosity) > 1) {
      VG_(message)(Vg_UserMsg, "Startup, with flags:");
      for (i = 0; i < argc; i++) {
         VG_(message)(Vg_UserMsg, "   %s", argv[i]);
      }
   }

   if (VG_(clo_n_suppressions) == 0) {
      config_error("No error-suppression files were specified.");
   }
}


/* ---------------------------------------------------------------------
   Copying to/from m_state_static.
   ------------------------------------------------------------------ */

UInt VG_(m_state_static) [8 /* int regs, in Intel order */ 
                          + 1 /* %eflags */ 
                          + 1 /* %eip */
                          + VG_SIZE_OF_FPUSTATE_W /* FPU state */
                         ];

void VG_(copy_baseBlock_to_m_state_static) ( void )
{
   Int i;
   VG_(m_state_static)[ 0/4] = VG_(baseBlock)[VGOFF_(m_eax)];
   VG_(m_state_static)[ 4/4] = VG_(baseBlock)[VGOFF_(m_ecx)];
   VG_(m_state_static)[ 8/4] = VG_(baseBlock)[VGOFF_(m_edx)];
   VG_(m_state_static)[12/4] = VG_(baseBlock)[VGOFF_(m_ebx)];
   VG_(m_state_static)[16/4] = VG_(baseBlock)[VGOFF_(m_esp)];
   VG_(m_state_static)[20/4] = VG_(baseBlock)[VGOFF_(m_ebp)];
   VG_(m_state_static)[24/4] = VG_(baseBlock)[VGOFF_(m_esi)];
   VG_(m_state_static)[28/4] = VG_(baseBlock)[VGOFF_(m_edi)];

   VG_(m_state_static)[32/4] = VG_(baseBlock)[VGOFF_(m_eflags)];
   VG_(m_state_static)[36/4] = VG_(baseBlock)[VGOFF_(m_eip)];

   for (i = 0; i < VG_SIZE_OF_FPUSTATE_W; i++)
      VG_(m_state_static)[40/4 + i] 
         = VG_(baseBlock)[VGOFF_(m_fpustate) + i];
}


void VG_(copy_m_state_static_to_baseBlock) ( void )
{
   Int i;
   VG_(baseBlock)[VGOFF_(m_eax)] = VG_(m_state_static)[ 0/4];
   VG_(baseBlock)[VGOFF_(m_ecx)] = VG_(m_state_static)[ 4/4];
   VG_(baseBlock)[VGOFF_(m_edx)] = VG_(m_state_static)[ 8/4];
   VG_(baseBlock)[VGOFF_(m_ebx)] = VG_(m_state_static)[12/4];
   VG_(baseBlock)[VGOFF_(m_esp)] = VG_(m_state_static)[16/4];
   VG_(baseBlock)[VGOFF_(m_ebp)] = VG_(m_state_static)[20/4];
   VG_(baseBlock)[VGOFF_(m_esi)] = VG_(m_state_static)[24/4];
   VG_(baseBlock)[VGOFF_(m_edi)] = VG_(m_state_static)[28/4];

   VG_(baseBlock)[VGOFF_(m_eflags)] = VG_(m_state_static)[32/4];
   VG_(baseBlock)[VGOFF_(m_eip)] = VG_(m_state_static)[36/4];

   for (i = 0; i < VG_SIZE_OF_FPUSTATE_W; i++)
      VG_(baseBlock)[VGOFF_(m_fpustate) + i]
         = VG_(m_state_static)[40/4 + i];
}


/* ---------------------------------------------------------------------
   Show accumulated counts.
   ------------------------------------------------------------------ */

static void vg_show_counts ( void )
{
   VG_(message)(Vg_DebugMsg,
                " dispatch: %lu basic blocks, %d tt_fast misses.", 
                VG_(bbs_done),  VG_(tt_fast_misses));
   VG_(message)(Vg_DebugMsg,
                "translate: new %d (%d -> %d), discard %d (%d -> %d).",
                VG_(overall_in_count),
                VG_(overall_in_osize),
                VG_(overall_in_tsize),
                VG_(overall_out_count),
                VG_(overall_out_osize),
                VG_(overall_out_tsize) );
   VG_(message)(Vg_DebugMsg,
		"      lru: %d epochs, %d clearings.",
		VG_(current_epoch),
                VG_(number_of_lrus) );
   VG_(message)(Vg_DebugMsg, 
                "reg-alloc: %d t-req-spill, "
                "%d+%d orig+spill uis, %d total-reg-r.",
                VG_(translations_needing_spill),
                VG_(uinstrs_prealloc),
                VG_(uinstrs_spill),
                VG_(total_reg_rank) );
   VG_(message)(Vg_DebugMsg, 
                "smc-check: %d checks, %d fast pass, "
                "%d slow pass, %d discards.",
		VG_(smc_total_check4s),
		VG_(smc_cache_passed),
		VG_(smc_fancy_passed),
		VG_(smc_discard_count) );
   VG_(message)(Vg_DebugMsg, 
                "   sanity: %d cheap, %d expensive checks.",
                VG_(sanity_fast_count), 
                VG_(sanity_slow_count) );
}


/* ---------------------------------------------------------------------
   Main!
   ------------------------------------------------------------------ */

/* Where we jump to once Valgrind has got control, and the real
   machine's state has been copied to the m_state_static. */

void VG_(main) ( void )
{
   Int i;

   /* Set up our stack sanity-check words. */
   for (i = 0; i < 10; i++) {
      VG_(stack)[i]         = (UInt)(&VG_(stack)[i])         ^ 0xA4B3C2D1;
      VG_(stack)[10000-1-i] = (UInt)(&VG_(stack)[10000-i-1]) ^ 0xABCD4321;
   }

   /* Set up baseBlock offsets and copy the saved machine's state into
      it. */
   vg_init_baseBlock();
   VG_(copy_m_state_static_to_baseBlock)();

   /* Process Valgrind's command-line opts (from env var VG_OPTS). */
   process_cmd_line_options();

   /* Initialise the signal handling subsystem. */
   VG_(sigstartup_actions)();

#  ifdef VG_PROFILE
   VGP_(init_profiling)();
#  endif

   if (VG_(clo_instrument)) {
      VGP_PUSHCC(VgpInitAudit);
      VGM_(init_memory_audit)();
      VGP_POPCC;
      VGP_PUSHCC(VgpReadSyms);
      VG_(read_symbols)();
      VGP_POPCC;
   }

   /* This should come after init_memory_audit; otherwise the latter
      carefully sets up the permissions maps to cover the anonymous
      mmaps for the translation table and translation cache, which
      wastes > 20M of virtual address space. */
   VG_(init_transtab_and_SMC)();

   if (VG_(clo_verbosity) == 1) {
      VG_(message)(Vg_UserMsg, 
                   "For more details, rerun with: -v");
   }

   /* Now it is safe for malloc et al in vg_clientmalloc.c to act
      instrumented-ly. */
   VG_(running_on_simd_CPU) = True;
   if (VG_(clo_instrument)) {
      VGM_(make_readable) ( (Addr)&VG_(running_on_simd_CPU), 1 );
      VGM_(make_readable) ( (Addr)&VG_(clo_instrument), 1 );
      VGM_(make_readable) ( (Addr)&VG_(clo_trace_malloc), 1 );
      VGM_(make_readable) ( (Addr)&VG_(clo_sloppy_malloc), 1 );
   }

   if (VG_(clo_verbosity) > 0)
      VG_(message)(Vg_UserMsg, "");

   VG_(bbs_to_go) = VG_(clo_stop_after);
   VG_(toploop)();

   if (VG_(clo_verbosity) > 0)
      VG_(message)(Vg_UserMsg, "");

   if (VG_(clo_instrument)) {
      VG_(show_all_errors)();
      VG_(clientmalloc_done)();
      if (VG_(clo_verbosity) == 1) {
         VG_(message)(Vg_UserMsg, 
                      "For counts of detected errors, rerun with: -v");
      }
      if (VG_(clo_leak_check)) VG_(detect_memory_leaks)();
   }
   VG_(running_on_simd_CPU) = False;
   
   VG_(do_sanity_checks)(True /*include expensive checks*/ );

   if (VG_(clo_verbosity) > 1)
      vg_show_counts();

   if (0) {
      VG_(message)(Vg_DebugMsg, "");
      VG_(message)(Vg_DebugMsg, 
         "------ Valgrind's internal memory use stats follow ------" );
      VG_(mallocSanityCheckAll)();
      VG_(show_all_arena_stats)();
      VG_(message)(Vg_DebugMsg, 
         "------ Valgrind's ExeContext management stats follow ------" );
      VG_(show_ExeContext_stats)();
      VG_(message)(Vg_DebugMsg, 
         "------ Valgrind's client block stats follow ---------------" );
      VG_(show_client_block_stats)();
   }
 
#  ifdef VG_PROFILE
   VGP_(done_profiling)();
#  endif

   VG_(done_prof_mem)();

   VG_(shutdown_logging)();

   /* In LD_PRELOAD, convert "valgrind.so" into "valgrinq.so", so that
      child processes don't get traced into.  Also done on simulated
      execve system call. */
   if (!VG_(clo_trace_children)) { 
      VG_(mash_LD_PRELOAD_string)(VG_(getenv)("LD_PRELOAD"));
   }

   /* Prepare to restore state to the real CPU. */
   VG_(copy_baseBlock_to_m_state_static)();

   /* This pushes a return address on the simulator's stack, which
      is abandoned.  We call vg_sigshutdown_actions() at the end
      of vg_switch_to_real_CPU(), so as to ensure that the original
      stack and machine state is restored before the real signal
      mechanism is restored.
   */
   VG_(switch_to_real_CPU)();
}


/* Debugging thing .. can be called from assembly with OYNK macro. */
void VG_(oynk) ( Int n )
{
   OINK(n);
}


/* Find "valgrind.so" in a LD_PRELOAD=... string, and convert it to
   "valgrinq.so", which doesn't do anything.  This is used to avoid
   tracing into child processes.  To make this work the build system
   also supplies a dummy file, "valgrinq.so". 
*/
void VG_(mash_LD_PRELOAD_string)( Char* ld_preload_str )
{
   Char* p;
   if (ld_preload_str == NULL)
      return;
   p = VG_(strstr)(ld_preload_str, "valgrind.so");
   if (p == NULL)
      return;
   p[7] = 'q';
}

/* RUNS ON THE CLIENT'S STACK, but on the real CPU.  Start GDB and get
   it to attach to this process.  Called if the user requests this
   service after an error has been shown, so she can poke around and
   look at parameters, memory, etc.  You can't meaningfully get GDB to
   continue the program, though; to continue, quit GDB.  */
extern void VG_(start_GDB_whilst_on_client_stack) ( void )
{
   UChar buf[100];
   VG_(sprintf)(buf,
                "/usr/bin/gdb -nw /proc/%d/exe %d", 
                VG_(getpid)(), VG_(getpid)());
   VG_(printf)("starting GDB with cmd: %s\n", buf);
   VG_(mash_LD_PRELOAD_string)(VG_(getenv)("LD_PRELOAD"));
   { /* HACK ALERT */
     extern int system ( const char * );
     system(buf);
     /* end of HACK ALERT */
   }
   VG_(message)(Vg_UserMsg, "");
   VG_(message)(Vg_UserMsg, 
      "GDB has detached.  Valgrind regains control.  We continue.");
}


/* Print some helpful-ish text about unimplemented things, and give
   up. */
extern void VG_(unimplemented) ( Char* msg )
{
   VG_(message)(Vg_UserMsg, "");
   VG_(message)(Vg_UserMsg, 
      "Valgrind detected that your program requires");
   VG_(message)(Vg_UserMsg, 
      "the following unimplemented functionality:");
   VG_(message)(Vg_UserMsg, "   %s", msg);
   VG_(message)(Vg_UserMsg,
      "This may be because the functionality is hard to implement,");
   VG_(message)(Vg_UserMsg,
      "or because no reasonable program would behave this way,");
   VG_(message)(Vg_UserMsg,
      "or because nobody has yet needed it.  In any case, let me know");
   VG_(message)(Vg_UserMsg,
      "(jseward@acm.org) and/or try to work around the problem, if you can.");
   VG_(message)(Vg_UserMsg,
      "");
   VG_(message)(Vg_UserMsg,
      "Valgrind has to exit now.  Sorry.  Bye!");
   VG_(message)(Vg_UserMsg,
      "");
   VG_(exit)(1);
}


/*-------------------------------------------------------------*/
/*--- Replace some C lib things with equivs which don't get ---*/
/*--- spurious value warnings.  THEY RUN ON SIMD CPU!       ---*/
/*-------------------------------------------------------------*/

char* strrchr ( const char* s, int c )
{
   UChar  ch   = (UChar)((UInt)c);
   UChar* p    = (UChar*)s;
   UChar* last = NULL;
   while (True) {
      if (*p == ch) last = p;
      if (*p == 0) return last;
      p++;
   }
}

char* strchr ( const char* s, int c )
{
   UChar  ch = (UChar)((UInt)c);
   UChar* p  = (UChar*)s;
   while (True) {
      if (*p == ch) return p;
      if (*p == 0) return NULL;
      p++;
   }
}

char* strcat ( char* dest, const char* src )
{
   Char* dest_orig = dest;
   while (*dest) dest++;
   while (*src) *dest++ = *src++;
   *dest = 0;
   return dest_orig;
}

unsigned int strlen ( const char* str )
{
   UInt i = 0;
   while (str[i] != 0) i++;
   return i;
}

char* strcpy ( char* dest, const char* src )
{
   Char* dest_orig = dest;
   while (*src) *dest++ = *src++;
   *dest = 0;
   return dest_orig;
}

int strncmp ( const char* s1, const char* s2, unsigned int nmax )
{
   unsigned int n = 0;
   while (True) {
      if (n >= nmax) return 0;
      if (*s1 == 0 && *s2 == 0) return 0;
      if (*s1 == 0) return -1;
      if (*s2 == 0) return 1;

      if (*(UChar*)s1 < *(UChar*)s2) return -1;
      if (*(UChar*)s1 > *(UChar*)s2) return 1;

      s1++; s2++; n++;
   }
}

int strcmp ( const char* s1, const char* s2 )
{
   while (True) {
      if (*s1 == 0 && *s2 == 0) return 0;
      if (*s1 == 0) return -1;
      if (*s2 == 0) return 1;

      if (*(char*)s1 < *(char*)s2) return -1;
      if (*(char*)s1 > *(char*)s2) return 1;

      s1++; s2++;
   }
}

void* memchr(const void *s, int c, unsigned int n)
{
   unsigned int i;
   UChar c0 = (UChar)c;
   UChar* p = (UChar*)s;
   for (i = 0; i < n; i++)
      if (p[i] == c0) return (void*)(&p[i]);
   return NULL;
}

void* memcpy( void *dst, const void *src, unsigned int len )
{
    register char *d;
    register char *s;
    if ( dst > src ) {
        d = (char *)dst + len - 1;
        s = (char *)src + len - 1;
        while ( len-- )
            *d-- = *s--;
    } else if ( dst < src ) {
        d = (char *)dst;
        s = (char *)src;
        while ( len-- )
            *d++ = *s++;
    }
    return dst;
}

/*--------------------------------------------------------------------*/
/*--- end                                                vg_main.c ---*/
/*--------------------------------------------------------------------*/
