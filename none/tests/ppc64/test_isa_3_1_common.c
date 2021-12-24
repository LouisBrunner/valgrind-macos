/* test_isa_3_1_common.c */

/*  Copyright (C) 2020, IBM

 This program is free software; you can redistribute it and/or
 modify it under the terms of the GNU General Public License as
 published by the Free Software Foundation; either version 2 of the
 License, or (at your option) any later version.

 This program is distributed in the hope that it will be useful, but
 WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program; if not, see <http://www.gnu.org/licenses/>.

 The GNU General Public License is contained in the file COPYING.
 */

#ifdef HAS_ISA_3_1

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#include "isa_3_1_register_defines.h"
#include "isa_3_1_helpers.h"
#include "tests/malloc.h" // memalign

/* post_test indicates to the printf helpers if we are pre- or post-
   instruction execution, subsequently used to suppress register
   output when those register contents are not useful.  */
unsigned long post_test;
/* increase verbosity for increasing amounts of debug output.  */
int verbose = 0;
#define DEADBEEF 0x1111111111111111ULL

vector unsigned long long vec_xs;
vector unsigned long long vec_xt;
unsigned long long dcmx;

/* Iterator controls.  These are adjusted as appropriate for the tests
 being exercised.  See set_up_iterators () below.
*/
unsigned long a_iters, b_iters, c_iters, m_iters;
unsigned long a_inc, b_inc, c_inc, m_inc;
unsigned long a_start, b_start, c_start, m_start;
unsigned long vrai, vrbi, vrci, vrmi;
unsigned long a_limit = 0xffff, b_limit = 0xffff, c_limit = 0xffff;

vector unsigned long long vrt, vra, vrb, vrc;
vector unsigned long long vrm;

/* Debug:  Set these to allow skipping of test subsets that
   have nonzero vrm or mc values.
*/
unsigned long prefix_override = 0;
unsigned long vrm_override = 0;
unsigned long mc_override = 0;
unsigned long enable_setjmp = 0;
unsigned long dump_tables = 0;

/* condition register misc.  */
extern unsigned long current_cr;
extern unsigned long current_fpscr;

/* Helpers to manage when our output fields require special handling.
   This includes scenarios including:
   - some parts of the output fields are Undefined.
   - some parts of the output field contain *estimated* data that needs to be
     truncated when printed.
   - Some parts of the output need to be reported as INF or NAN.
   - The contents need to be interpreted as single or double precision.
*/
// Double precision indicators.
#define DP0               0b00100000
#define DP1               0b00010000
#define DOUBLE_MASK       0b00110000
// Single precision indicators.
#define SP0               0b00001000
#define SP1               0b00000100
#define SP2               0b00000010
#define SP3               0b00000001
#define SINGLE_MASK       0b00001111
// Estimated output indicators.
#define SINGLE_EST_MASK   0b01000000
#define DOUBLE_EST_MASK   0b10000000
// bfloat16 indicators.
#define B16_MASK  0b1111111100000000
#define B16_0     0b1000000000000000
#define B16_1     0b0100000000000000
#define B16_2     0b0010000000000000
#define B16_3     0b0001000000000000
#define B16_4     0b0000100000000000
#define B16_5     0b0000010000000000
#define B16_6     0b0000001000000000
#define B16_7     0b0000000100000000

/* Instruction Form indicators.
   These are set based on the instruction name and the associated
   instruction form.  These are subsequently used to help initialize
   the incoming register contents when testing the specific instruction.
*/
bool has_ra, has_rb, has_rc, has_rs, has_rt;
bool has_rtp, has_rsp;
bool has_vra, has_vrb, has_vrc, has_vrm, has_vrt;
bool has_xa, has_xb, has_xc, has_xs, has_xt;
bool has_xap;
bool uses_xc_as_blend_mask;
bool has_xsp, has_xtp;
bool has_frb, has_frbp; // frb* uses same regs as frsp.
bool has_frs, has_frsp;
bool has_frt, has_frtp;
bool uses_CRBIT, uses_RC, uses_MC;
bool uses_cr;
bool is_divide_or_modulo;
bool is_insert_double;
bool is_testlsb;
bool has_rs_as_value_source;
bool has_dcmx;
unsigned long is_clear_or_insert_insns;
unsigned long is_mtvsr_insn;
unsigned long is_cmp_insn;
bool has_ra_target;
bool uses_dfp128_input;
bool uses_dfp128_output;
bool uses_acc;	// Accumulator related.
bool uses_acc_src;
bool uses_acc_dest;
bool uses_acc_vsrs;
bool uses_pmsk;
bool uses_buffer;  // Buffer related.
bool uses_load_buffer, uses_store_buffer, uses_any_buffer;
bool updates_byte, updates_halfword, updates_word; // output helpers.
bool uses_quad;
unsigned long output_mask;  // Output field special handling.
bool instruction_is_sp, instruction_is_sp_estimate;
bool instruction_is_dp, instruction_is_dp_estimate;
bool instruction_is_b16;
bool instruction_is_relative;

unsigned long long min (unsigned long long a, unsigned long long b) {
   if ( a < b )
      return a;
   return b;
}

/* Parse the 'form' field to mark and identify arguments to the instruction. */
void identify_form_components (const char *instruction_name,
			       const char *cur_form)
{
   has_ra = ((strstr (cur_form, ",RA") != NULL) ||
	     (strstr (cur_form, "(RA)") != NULL));
   has_ra_target = (strncmp (cur_form, "RA,", 3) == 0);
   has_rb = strstr (cur_form, ",RB") != NULL;
   has_rc = strstr (cur_form, ",RC") != NULL;
   has_rs = ((strstr (cur_form, ",RS") != NULL) ||
	     (strncmp (cur_form, "RS", 2) == 0));
   has_rsp = (strncmp (cur_form, "RSp", 3) == 0);
   has_rt = (strncmp (cur_form, "RT", 2) == 0);
   has_rtp = (strncmp (cur_form, "RTp", 3) == 0);

   has_vra = strstr (cur_form, "VRA") != NULL;
   has_vrb = strstr (cur_form, "VRB") != NULL;
   has_vrc = strstr (cur_form, "VRC") != NULL;
   has_vrm = strstr (cur_form, "VRM") != NULL;
   has_vrt = (strncmp (cur_form, "VRT", 3) == 0);

   has_frb = strstr (cur_form, "FRB") != NULL;
   has_frbp = strstr (cur_form, "FRBp") != NULL;
   has_frs = strstr (cur_form, "FRS") != NULL;
   has_frsp = strstr (cur_form, "FRSp") != NULL;
   has_frt = strstr (cur_form, "FRT") != NULL;
   has_frtp = strstr (cur_form, "FRTp") != NULL;

   has_xa = strstr (cur_form, ",XA") != NULL;
   has_xap = strstr (cur_form, "XAp") != NULL;
   has_xb = strstr (cur_form, ",XB") != NULL;
   has_xc = strstr (cur_form, ",XC") != NULL;
   has_xs = (strncmp (cur_form, "XS", 2) == 0);
   has_xsp = (strncmp (cur_form, "XSp", 3) == 0);
   has_xt = (strncmp (cur_form, "XT", 2) == 0);
   has_xtp = (strncmp (cur_form, "XTp", 3) == 0);

   uses_acc_src = (strstr (cur_form, "AS") != NULL);
   uses_acc_dest = (strstr (cur_form, "AT") != NULL);
/* These (xxm*acc) are special cases where the acc_src is used, but we
   need to read the associated _vsrs on the way out.
*/
   uses_acc_vsrs = (
	 (strstr (instruction_name, "xxmfacc") != NULL) ||
	 (strstr (instruction_name, "xxmtacc") != NULL) );
   uses_acc = uses_acc_src || uses_acc_dest || uses_acc_vsrs;
   uses_pmsk = strstr (cur_form, "PMSK") !=NULL;

   uses_dfp128_input = (
	 (strncmp (instruction_name, "dctf", 4) == 0));
   uses_dfp128_output = (
	 (strncmp (instruction_name, "dcff", 4) == 0));
   is_divide_or_modulo = (
	 (strncmp (instruction_name, "vdiv", 4) == 0) ||
	 (strncmp (instruction_name, "pmvdiv", 6) == 0) ||
	 (strncmp (instruction_name, "vmod", 4) == 0) ||
	 (strncmp (instruction_name, "pmvmod", 6) == 0) );
   is_insert_double = (
	 (strncmp (instruction_name, "vinsd", 5) == 0) );
   is_testlsb = (
	 (strncmp (instruction_name, "xvtlsbb", 7) == 0) );
   uses_xc_as_blend_mask = (
	 (strncmp (instruction_name, "xxblend", 7) == 0) );
   has_dcmx = strstr (cur_form, "DCMX") != NULL;
   uses_CRBIT = (
      (strncmp (cur_form, "BF", 2) == 0) ||
      (strstr (cur_form, ",BI") != 0));
   uses_RC = (
      (strstr (instruction_name, ".") != NULL ));
   uses_MC = (
      (strstr (instruction_name, ",MC") != NULL ));
   uses_cr = (
      (strstr (instruction_name, "setbcr") != 0) ||
      (strstr (instruction_name, "setnbcr") != 0));
/* The lxvkq instruction loads special values into a VSX vector, so although
   this looks like a load, it is excluded from the uses_load_buffer set
   because it does not load a value from a buffer.  */
   uses_load_buffer = (
      (strncmp (instruction_name, "ld", 2) == 0)  ||
      (strncmp (instruction_name, "lq", 2) == 0)  ||
      (strncmp (instruction_name, "plq", 3) == 0)  ||
      (strncmp (instruction_name, "plx", 3) == 0)  ||
      (strncmp (instruction_name, "pmlx", 4) == 0) ||
      (strncmp (instruction_name, "lxv", 3) == 0) ||
      ( (strncmp (instruction_name, "lxva", 4) == 0) &&
        (strncmp (instruction_name, "lxvkq", 5) != 0)) );
   uses_store_buffer = (
      (strncmp (instruction_name, "pmst", 4) == 0) ||
      (strncmp (instruction_name, "pst", 3) == 0) ||
      (strncmp (instruction_name, "st", 2) == 0));
   updates_byte = (
      (strncmp (instruction_name, "pstb", 4) == 0) );
   updates_halfword = (
      (strncmp (instruction_name, "psth", 4) == 0) ||
       (strncmp (instruction_name, "pstfs", 4) == 0) ||
       (strncmp (instruction_name, "pstxsd", 4) == 0) ||
       (strncmp (instruction_name, "pstxssp", 4) == 0) ||
       (strncmp (instruction_name, "pstxv", 4) == 0) ||
       (strncmp (instruction_name, "psfs", 4) == 0) );
   updates_word = (
      (strncmp (instruction_name, "pstw", 4) == 0) );

   uses_any_buffer = (strstr (cur_form, "(RA)") != NULL);
   uses_buffer = uses_any_buffer||uses_load_buffer||uses_store_buffer;

   uses_quad = (uses_buffer && (strstr (instruction_name, "q") != NULL));

   has_rs_as_value_source = (
      (strcmp (cur_form, "RA,RS,RB") == 0) ||
      (strcmp (cur_form, "RA,RS") == 0) );

   is_clear_or_insert_insns = (
      (strncmp (instruction_name, "vclr", 4) == 0) ||
      (strncmp (instruction_name, "vins", 4) == 0) );

   /* This is used by a helper function to control the CR field output when
   the instruction is a compare, otherwise it is likely a bitfield check. */
   is_cmp_insn = ( (strstr (cur_form, "cmp") != NULL));

   is_mtvsr_insn = ( (strncmp (instruction_name, "mtvsr", 5) == 0));

  /* If the instruction output needs to be something other than a hex dump,
     a mask will have been defined as part of the test_list_t structure.
     This includes instructions that return estimated values, as well as
     those that return NAN results which contain sign bits that need to be
     filtered out.  */
   output_mask = ( current_test.mask );
   instruction_is_dp =          ( current_test.mask & DOUBLE_MASK     );
   instruction_is_dp_estimate = ( current_test.mask & DOUBLE_EST_MASK );
   instruction_is_sp =          ( current_test.mask & SINGLE_MASK     );
   instruction_is_sp_estimate = ( current_test.mask & SINGLE_EST_MASK );
   instruction_is_b16 =         ( current_test.mask & B16_MASK        );
}

/* Parse the provided function name to set assorted values.
   In particular, set an indicator when the instruction test has
   indicated it will run with R==1 that indicates it is a PC-relative
   instruction.  Those tests should all have "_R1" as part of
   the function name.  */
void identify_instruction_by_func_name(const char * function_name) {
   instruction_is_relative = ( (strstr (function_name, "R1") != NULL));
}

void display_form_components (char * cur_form) {
   printf (" %s\n", cur_form);
   printf ("Instruction form elements: ");
   if (has_ra)   printf ("ra ");
   if (has_rb)   printf ("rb ");
   if (has_rc)   printf ("rc ");
   if (has_rs)   printf ("rs ");
   if (has_rsp)  printf ("rsp ");
   if (has_rt)   printf ("rt ");
   if (has_rtp)  printf ("rtp ");
   if (has_vra)  printf ("vra ");
   if (has_vrb)  printf ("vrb ");
   if (has_vrc)  printf ("vrc ");
   if (has_vrm)  printf ("vrm ");
   if (has_vrt)  printf ("vrt ");

   if (has_frb)  printf ("frb ");
   if (has_frbp) printf ("frbp ");
   if (has_frs)  printf ("frs ");
   if (has_frsp) printf ("frsp ");
   if (has_frt)  printf ("frt%s ",(instruction_is_relative)?"-raw":"");
   if (has_frtp) printf ("frtp ");
   if (has_xa)   printf ("xa ");
   if (has_xap)  printf ("xap ");
   if (has_xb)   printf ("xb ");
   if (has_xc)   printf ("xc ");
   if (has_xs)   printf ("xs ");
   if (has_xsp)  printf ("xsp ");
   if (has_xt)   printf ("xt ");
   if (has_xtp)  printf ("xtp ");
   if (instruction_is_relative)  printf ("R==1 ");
   if (uses_acc_src) printf ("AS ");
   if (uses_acc_dest) printf ("AT ");
   printf ("\n");
   if (uses_dfp128_input)
	printf ("uses dfp128 input.\n");
   if (uses_dfp128_output)
	printf ("uses dfp128 output.\n");
   if (has_ra_target)
	printf ("ra is a target register.\n");
   if (has_rs_as_value_source)
	printf ("rs is a value source.\n");
   if (uses_xc_as_blend_mask)
	printf ("uses xc as a blend mask.\n");
   if (is_clear_or_insert_insns)
	printf ("is a clear or insert insn.\n");
   if (is_insert_double)
	printf ("is an insert doubleword.\n");
   if (is_testlsb)
	printf ("tests lsb.\n");
   if (uses_buffer)
	printf ("uses_buffer: (l:%d s:%d ?:%d)\n",
		uses_load_buffer, uses_store_buffer, uses_any_buffer);
   if (uses_quad)
	printf ("is a quad load or store.\n");
   if (is_cmp_insn)
	printf ("is a compare instruction.\n");
   if (uses_CRBIT)
	printf ("instruction references a CR.\n");
   if (uses_cr)
	printf ("instruction reads CR bits.\n");
   if (uses_MC)
	printf ("Instruction uses MC.\n");
   if (uses_RC)
	printf ("Instruction uses Record Bit (cr6).\n");
   if (uses_acc)
	printf ("Instruction uses ACC: (src:%d, dst:%d, vsrs:%d).\n",
		uses_acc_src, uses_acc_dest, uses_acc_vsrs);
   if (uses_pmsk)
	printf ("Instruction uses PMSK \n");
   if (output_mask) {
	printf ("Instruction results are masked: ");
	printf (" (%lx) ", output_mask);
	printf ("%s ", instruction_is_sp?"SP ":"");
	printf ("%s ", instruction_is_sp_estimate?"SP Estimate ":"");
	printf ("%s ", instruction_is_dp?"DP ":"");
	printf ("%s ", instruction_is_dp_estimate?"DP Estimate ":"");
	printf ("%s ", instruction_is_b16?"bfloat16 ":"");
   }
   printf ("\n");
}

long long mask64[] = { 0x0, 0x00000000ffffffff, 0xffffffff55555555,
		       0x5555aaaaaaaa5555, 0xaaaa00000000aaaa  };
#define MASK64SIZE 5
unsigned long long vrm_mask[] = { 0x0, 0x8000000000000000,
				  0x8000000000000000, 0x0 };
#define VRMMASK_SIZE 4

// Helpers to print double/float values.
// Union to help handle referencing hex/float/double values.
union rosetta_t {
	unsigned long long ull;
	unsigned long long ullp[2];
	float flt;
	float fltp[2];
	uint16_t uint16s[4];
	double dbl;
};

void generic_print_float_as_hex (float f) {
	union rosetta_t stone;
	stone.ullp[0] = stone.ullp[1] = 0; //init
	stone.flt = f;
	printf (" %016llx", stone.ull);
}

void generic_print_ull_as_float (unsigned long long ull) {
	union rosetta_t stone;
	stone.ullp[0] = stone.ullp[1] = 0; //init
	stone.ull = ull;
	printf (" %f", stone.flt);
}

void generic_print_ull_as_double (unsigned long long ull) {
	union rosetta_t stone;
	stone.ullp[0] = stone.ullp[1] = 0; //init
	stone.ull = ull;
	printf (" %e", stone.dbl);
}

void generic_print_double_as_hex (double d) {
	union rosetta_t stone;
	stone.ullp[0] = stone.ullp[1] = 0; //init
	stone.dbl = d;
	printf (" %016llx", stone.ull);
}

// SP in a 32-bit field.
#define SP_SIGNBIT_MASK     0x80000000
#define SP_EXPONENT_MASK    0x7f800000
#define SP_FRACTION_MASK    0x007fffff

// DP (64-bit).
#define DP_SIGNBIT_MASK     0x8000000000000000UL
#define DP_EXPONENT_MASK    0x7ff0000000000000UL
#define DP_FRACTION_MASK    0x000fffffffffffffUL

// B16 bfloat16.
#define BF16_SIGNBIT_MASK   0x8000
#define BF16_EXPONENT_MASK  0x7f80
#define BF16_FRACTION_MASK  0x007f

/*
 - NAN and Zero values need the sign bit display suppressed. (See comments
   in jm-insns.c, approx line 7203).
 - Some instructions return estimated values, which are calculated
   to a different level of precision within valgrind.  Those
   instructions need their outputs limited to a specific number of
   digits as seen below.  */

// NAN - Maximum biased exponent and a nonzero mantissa (fraction).
#define SPFMT "%16s"
#define PRINT_SP_NAN		    printf (SPFMT,"NaN");
// DEN - Exp == 0 and Frac != 0
#define PRINT_SP_PLUS_DEN	    printf (SPFMT,"+Den");
#define PRINT_SP_MINUS_DEN	    printf (SPFMT,"-Den");
// INF - Maximum biased exponent and a zero mantissa.
#define PRINT_SP_INF		    printf (SPFMT,"Inf");
#define PRINT_SP_PLUS_INF	    printf (SPFMT,"+Inf");
#define PRINT_SP_MINUS_INF	    printf (SPFMT,"-Inf");
#define PRINT_SP_FLOAT(x)	    printf ("%16.05e",  x);
#define PRINT_SP_FLOAT_EST(x)	    printf ("%16.03e",  x);
#define PRINT_SP_FLOAT_PLUS_ZERO    printf (SPFMT,"+Zero");
#define PRINT_SP_FLOAT_MINUS_ZERO   printf (SPFMT,"-Zero");
#define PRINT_SP_SPLAT		    printf (SPFMT,"*");

/* Print a SINGLE (16 bit) SP value out of the left part of a 32-bit field. */
void special_print_sp_value (uint32_t value) {
   int signbit;
   int exponent;
   unsigned long long fraction;
   union rosetta_t stone;

   stone.ull = value;
   signbit = value & SP_SIGNBIT_MASK;
   exponent = (value & SP_EXPONENT_MASK);
   fraction = value & SP_FRACTION_MASK;

   if (debug_show_raw_values) {
      printf ("\nsp_debug: v:%08x s: %d %3x %8llx %f , ",
	       value, signbit?1:0, exponent, fraction, stone.flt);
   }
   if (exponent == SP_EXPONENT_MASK && fraction == 0 ) {
      if (signbit)
	 PRINT_SP_MINUS_INF
      else
	 PRINT_SP_PLUS_INF
   } else if (exponent == SP_EXPONENT_MASK && fraction != 0 ) {
	 PRINT_SP_NAN
   } else if (exponent == 0 && fraction == 0 ) {
      if (signbit)
	 PRINT_SP_FLOAT_MINUS_ZERO
      else
	 PRINT_SP_FLOAT_PLUS_ZERO
   } else if (exponent == 0 && fraction != 0 ) {
      if (signbit)
	 PRINT_SP_MINUS_DEN
      else
	 PRINT_SP_PLUS_DEN
   } else if (instruction_is_sp_estimate) {
      PRINT_SP_FLOAT_EST (stone.flt);
   } else {
      PRINT_SP_FLOAT (stone.flt);
   }
}

void dissect_sp_value (unsigned long long foo) {
   if (debug_show_raw_values) {
      printf ("RAW sp::%4llx ", foo);
      printf (" [s:");
      printf ("%x", (foo & SP_SIGNBIT_MASK)>0);
      printf (" e:");
      printf ("%4llx", foo & SP_EXPONENT_MASK);
      printf (" f:");
      printf ("%4llx", foo & SP_FRACTION_MASK);
      printf ("] ");
   }
   special_print_sp_value (foo);
   printf (" ");
}

/* Print one DP values out of our vec_ field. */
#define DPFMT "%17s"
#define PRINT_DP_NAN		   printf (DPFMT,"NaN");
#define PRINT_DP_MINUS_DEN	   printf (DPFMT,"-Den");
#define PRINT_DP_PLUS_DEN	   printf (DPFMT,"+Den");
#define PRINT_DP_MINUS_INF	   printf (DPFMT,"-Inf");
#define PRINT_DP_PLUS_INF	   printf (DPFMT,"+InF");
#define PRINT_DP_FLOAT(x)	   printf ("%17.08e",  x);
#define PRINT_DP_FLOAT_EST(x)	   printf ("%17.02e",  x);
#define PRINT_DP_FLOAT_PLUS_ZERO   printf (DPFMT,"+Zero");
#define PRINT_DP_FLOAT_MINUS_ZERO  printf (DPFMT,"-Zero");
#define PRINT_DP_FLOAT_ZERO	   printf (DPFMT,"0.000000e+000");
#define PRINT_DP_SPLAT		   printf (DPFMT,"*");
void special_print_dp_value (unsigned long long value) {
   unsigned long long signbit;
   unsigned long long exponent;
   unsigned long long fraction;
   union rosetta_t stone;

   stone.ull = value;
   signbit = (value & DP_SIGNBIT_MASK) > 0;
   exponent = value & DP_EXPONENT_MASK; // >> double_exponent_shift;
   fraction = value & DP_FRACTION_MASK;
   if (debug_show_raw_values)
      printf ("\ndb_debug: %16llx s:%d %3llx %8llx %llx , ",
	       value, signbit?1:0, exponent, fraction, stone.ull);
   if (exponent == DP_EXPONENT_MASK /* MAX */ && fraction == 0 ) {
      if (signbit)
	 PRINT_DP_MINUS_INF
      else
	 PRINT_DP_PLUS_INF
   } else if (exponent == DP_EXPONENT_MASK && fraction != 0 ) {
	 PRINT_DP_NAN
   } else if (exponent == 0 && fraction == 0 ) {
      if (signbit)
	 PRINT_DP_FLOAT_MINUS_ZERO
      else
	 PRINT_DP_FLOAT_PLUS_ZERO
   } else if (exponent == 0 && fraction != 0 ) {
      if (signbit)
	 PRINT_DP_MINUS_DEN
      else
	 PRINT_DP_PLUS_DEN
   } else if (instruction_is_dp_estimate) {
	 PRINT_DP_FLOAT_EST (stone.dbl);
   } else {
	 PRINT_DP_FLOAT (stone.dbl);
   }
}

void dissect_dp_value (unsigned long long foo) {
   if (debug_show_raw_values) {
      printf ("RAW dp::%llx", (foo));
      printf (" [sign:");
      printf ("%x ", (foo & DP_SIGNBIT_MASK) > 0);
      printf (" expbits:");
      printf ("%3llx",  foo & DP_EXPONENT_MASK );
      printf (" frac:");
      printf ("%16llx", foo & DP_FRACTION_MASK);
      printf ("] ");
   }
   special_print_dp_value (foo);
   printf (" ");
}

// NAN - Maximum biased exponent and a nonzero mantissa (fraction).
#define BFFMT "%6s"
#define PRINT_BF16_NAN		      printf (BFFMT,"NaN");
// DEN - Exp == 0 and Frac != 0
#define PRINT_BF16_PLUS_DEN	      printf (BFFMT,"+Den");
#define PRINT_BF16_MINUS_DEN	      printf (BFFMT,"-Den");
// INF - Maximum biased exponent and a zero mantissa.
#define PRINT_BF16_INF		      printf (BFFMT,"Inf");
#define PRINT_BF16_PLUS_INF	      printf (BFFMT,"+Inf");
#define PRINT_BF16_MINUS_INF	      printf (BFFMT,"-Inf");
#define PRINT_BF16_FLOAT(x)	      printf ("0x%04x", x);
#define PRINT_BF16_FLOAT_PLUS_ZERO    printf (BFFMT,"+Zero");
#define PRINT_BF16_FLOAT_MINUS_ZERO   printf (BFFMT,"-Zero");
/* print a single bfloat16 value.  */
void special_print_bf16_value (uint16_t value) {
  int signbit;
  int exponent;
  unsigned long long fraction;
  union rosetta_t stone;
  signbit = value & BF16_SIGNBIT_MASK;
  exponent = (value & BF16_EXPONENT_MASK);
  fraction = (value & BF16_FRACTION_MASK);
  stone.ull = value;
   if (debug_show_raw_values) {
      printf ("\nbf16_debug: v:%08x s: %d %3x %8llx %f , ",
	       value, signbit?1:0, exponent, fraction, stone.flt);
   } else if (debug_show_values) {
      printf (" v:%08x", value);
   }
   if (exponent == BF16_EXPONENT_MASK && fraction == 0 ) {
      if (signbit)
	 PRINT_BF16_MINUS_INF
      else
	 PRINT_BF16_PLUS_INF
   } else if (exponent == BF16_EXPONENT_MASK && fraction != 0 ) {
	 PRINT_BF16_NAN
   } else if (exponent == 0 && fraction == 0 ) {
      if (signbit)
	 PRINT_BF16_FLOAT_MINUS_ZERO
      else
	 PRINT_BF16_FLOAT_PLUS_ZERO
   } else if (exponent == 0 && fraction != 0 ) {
      if (signbit)
	 PRINT_BF16_MINUS_DEN
      else
	 PRINT_BF16_PLUS_DEN
   } else
	 PRINT_BF16_FLOAT (value);
}

/* ******************** */
/* Accumulator related. */
/* Note that our tests to set and clear the acc both read and write
   from and to the associated VSRs, so some tests may be
   self-fulfilling.  */
void push_vsrs_to_acc () {
   if (!setup_only)
      __asm__ __volatile__ ("xxmtacc 4 "); // $ACCNUM
}

void push_acc_to_vsrs () {
   if (!setup_only)
      __asm__ __volatile__ ("xxmfacc 4 "); // $ACCNUM
}


void __print_splat_or_sp(long long vv) {
   if (vv == DEADBEEF) {
      PRINT_SP_SPLAT
      PRINT_SP_SPLAT
  } else {
      special_print_sp_value (0xffffffff & (vv>>32));
      special_print_sp_value (0xffffffff & (vv));
   }
}

void __print_splat_or_dp(long long vv) {
   if (vv == DEADBEEF)
      PRINT_DP_SPLAT
   else {
      special_print_dp_value (vv);
   }
}

void __print_splat_or_raw(long long vv) {
   if (vv == DEADBEEF)
      printf (" %16s", "*");
   else
      printf (" %016llx", vv);
}

void print_accumulator () {
   if (uses_acc || debug_show_all_regs) {
      push_acc_to_vsrs ();
      if (debug_show_labels) printf (" Acc[]:");
      if (instruction_is_sp) {
	 printf (" (");
	 __print_splat_or_sp(TEST_ACC0[0]);
	 __print_splat_or_sp(TEST_ACC0[1]);
	 __print_splat_or_sp(TEST_ACC1[0]);
	 __print_splat_or_sp(TEST_ACC1[1]);
	 __print_splat_or_sp(TEST_ACC2[0]);
	 __print_splat_or_sp(TEST_ACC2[1]);
	 __print_splat_or_sp(TEST_ACC3[0]);
	 __print_splat_or_sp(TEST_ACC3[1]);
	 printf (")");
      } else if (instruction_is_dp) {
	 printf (" {");
	 __print_splat_or_dp(TEST_ACC0[0]);
	 __print_splat_or_sp(TEST_ACC0[1]);
	 __print_splat_or_dp(TEST_ACC1[0]);
	 __print_splat_or_sp(TEST_ACC1[1]);
	 __print_splat_or_dp(TEST_ACC2[0]);
	 __print_splat_or_sp(TEST_ACC2[1]);
	 __print_splat_or_dp(TEST_ACC3[0]);
	 __print_splat_or_sp(TEST_ACC3[1]);
	  printf ("}");
      } else {
	 printf (" [");
	 __print_splat_or_raw(TEST_ACC0[0]);
	 __print_splat_or_raw(TEST_ACC0[1]);
	 __print_splat_or_raw(TEST_ACC1[0]);
	 __print_splat_or_raw(TEST_ACC1[1]);
	 __print_splat_or_raw(TEST_ACC2[0]);
	 __print_splat_or_raw(TEST_ACC2[1]);
	 __print_splat_or_raw(TEST_ACC3[0]);
	 __print_splat_or_raw(TEST_ACC3[1]);
	 printf ("]");
      }
   }
}


/* ************** */
/*  The bit definitions for the FPSCR are as follows.
Bit (s) Description
0:31 Reserved
32 Floating-Point Exception Summary (FX)
33 Floating-Point Enabled Exception Summary (FEX)
34 Floating-Point Invalid Operation Exception Summary (VX)
35 Floating-Point Overflow Exception (OX)
36 Floating-Point Underflow Exception (UX)
37 Floating-Point Zero Divide Exception (ZX)
38 Floating-Point Inexact Exception (XX)
39 Floating-Point Invalid Operation Exception (SNaN) (VXSNAN)
40 Floating-Point Invalid Operation Exception (∞ - ∞) (VXISI)
41 Floating-Point Invalid Operation Exception (∞ ÷ ∞) (VXIDI)
42 Floating-Point Invalid Operation Exception (0 ÷ 0) (VXZDZ)
43 Floating-Point Invalid Operation Exception (∞ × 0) (VXIMZ)
44 Floating-Point Invalid Operation Exception (Invalid Compare) (VXVC)
45 Floating-Point Fraction Rounded (FR)
46 Floating-Point Fraction Inexact (FI)
47:51 Floating-Point Result Flags (FPRF)
47 Floating-Point Result Class Descriptor (C)
48:51 Floating-Point Condition Code (FPCC)
      48 Floating-Point Less Than or Negative (FL or <)
      49 Floating-Point Greater Than or Positive (FG or >)
      50 Floating-Point Equal or Zero (FE or = )
      51 Floating-Point Unordered or NaN (FU or ?)
52 Reserved
53 Floating-Point Invalid Operation Exception (Software-Defined Condition) (VXSOFT)
54 Floating-Point Invalid Operation Exception (Invalid Square Root) (VXSQRT)
55 Floating-Point Invalid Operation Exception (Invalid Integer Convert) (VXCVI)
56 Floating-Point Invalid Operation Exception Enable (VE)
57 Floating-Point Overflow Exception Enable (OE)
58 Floating-Point Underflow Exception Enable (UE)
59 Floating-Point Zero Divide Exception Enable (ZE)
60 Floating-Point Inexact Exception Enable (XE)
61 Floating-Point Non-IEEE Mode (NI)
62:63 Floating-Point Rounding Control (RN)
   00 Round to Nearest
   01 Round toward Zero
   10 Round toward +Infinity
   11 Round toward -Infinity
*/
/* Valgrind currently tracks the rounding mode, C and FPCC fields
   of the FPSCR.  Additional checking in the testcase is not
   necessary or beneficial.  */

#define FPCC_C_BIT    (0x1 << (63-47))
#define FPCC_FL_BIT   (0x1 << (63-48))
#define FPCC_FG_BIT   (0x1 << (63-49))
#define FPCC_FE_BIT   (0x1 << (63-50))
#define FPCC_FU_BIT   (0x1 << (63-51))
#define FPCC_FPRF_MASK  \
	FPCC_C_BIT | FPCC_FL_BIT | FPCC_FG_BIT | FPCC_FE_BIT | FPCC_FU_BIT

#define FPSCR_RN_BIT62   (0x1 << (63-62))
#define FPSCR_RN_BIT63   (0x1 << (63-63))

#define CRFIELD_BIT0 0x8
#define CRFIELD_BIT1 0x4
#define CRFIELD_BIT2 0x2
#define CRFIELD_BIT3 0x1

/* Display the condition register bits.  */
int cr_overflow_set (unsigned this_cr) {
   return (this_cr & CRFIELD_BIT3);
}

int cr_zero_set (unsigned this_cr) {
   return (this_cr & CRFIELD_BIT2);
}

int cr_positive_set (unsigned this_cr) {
   return (this_cr & CRFIELD_BIT1);
}

int cr_negative_set (unsigned this_cr) {
   return (this_cr & CRFIELD_BIT0);
}

/* This function (__dissect_cr) takes a bitfield directly.  */
static void __dissect_cr (unsigned this_cr) {
   extern unsigned long is_cmp_insn;
   printf ("[");
   if (cr_negative_set (this_cr))
      printf ("%s", is_cmp_insn ? " (LT) 0x1 = Negative 0b1 " : "1");
   else
      printf ("%s", verbose ? "0" : "0");

   if (cr_positive_set (this_cr))
      printf ("%s", is_cmp_insn ? " (GT) 0x2 = Positive fg_flag (zero/inf/denorm) " : "1");
   else
      printf ("%s", verbose ? "0" : "0");

   if (cr_zero_set (this_cr))
      printf ("%s", is_cmp_insn ? " (EQ) 0x4 = Zero fe_flag (zero/nan/inf/neg/e_b<-970" : "1");
   else
      printf ("%s", verbose ? "0" : "0");

   if (cr_overflow_set (this_cr))
      printf ("%s", is_cmp_insn ? " (SO) 0x8 = Overflow 0b0" : "1");
   else
      printf ("%s", verbose ? "0" : "0");
   printf ("]");
}

/* Extract one CR field */
int extract_cr_rn (unsigned long chosen_cr, unsigned long rn) {
   unsigned int masked_cr;
   unsigned long shifted_value;
   shifted_value = chosen_cr >> ( ( (7 - rn) * 4 ) );
   masked_cr = shifted_value & 0xf;
   return masked_cr;
}

/* Display one CR field */
void dissect_cr_rn (unsigned long chosen_cr, unsigned long rn) {
   unsigned int masked_cr;
   if (debug_show_labels) printf (" RC/CR (%ld):", rn );
   masked_cr = extract_cr_rn (chosen_cr, rn);
   printf ("%ld:", rn);
   __dissect_cr (masked_cr);
}

char * fpscr_strings[] = {
" 0-RSVD", " 1-RSVD", " 2-RSVD", " 3-RSVD", " 4-RSVD", " 5-RSVD", " 6-RSVD",
" 7-RSVD", " 8-RSVD", " 9-RSVD", "10-RSVD", "11-RSVD", "12-RSVD", "13-RSVD",
"14-RSVD", "15-RSVD", "16-RSVD", "17-RSVD", "18-RSVD", "19-RSVD", "20-RSVD",
"21-RSVD", "22-RSVD", "23-RSVD", "24-RSVD", "25-RSVD", "26-RSVD", "27-RSVD",
"28-RSVD", "29-DRN0", "30-DRN1", "31-DRN2",
/* 32 */ "FX", "FEX", "VX",
/* 35 */ "OX", "UX", "ZX", "XX", "VXSNAN",
/* 40 */ "VXISI (inf-inf)", "VXIDI (inf/inf)", "VXZDZ (0/0)",
/* 43 */ "VXIMZ (inf*0)", "VXVC",
/* 45 */ "FR", "FI",
/* 47 */ "FPRF-C", "FPCC-FL", "FPCC-FG",
/* 50 */ "FPCC-FE", "FPCC-FU",
/* 52 */ "52-RSVD", "FXSOFT", "VXSQRT",
/* 55 */ "VXCVI", "VE", "OE", "UE", "ZE",
/* 60 */ "XE", "NI", "RN-bit62", "RN-bit63"
};
/* Display only the fpscr bits that are valid under valgrind.
 * Valgrind tracks the C (FPSCR[47]), FPCC (FPSCR[48:51)
 * DRN (FPSCR[29:31]) and RN (FPSCR[62:63]).  */
void dissect_fpscr_valgrind (unsigned long local_fpscr) {
   int i;
   long mybit;

   /* Print DRN fields */
   for (i = 29; i < 32; i++) {
      mybit = 1LL << (63 - i);
      if (mybit & local_fpscr) {
         printf (" %s", fpscr_strings[i]);
      }
   }

   /* Print C and FPCC fields */
   for (i = 47; i < 52; i++) {
      mybit = 1LL << (63 - i);
      if (mybit & local_fpscr) {
         printf (" %s", fpscr_strings[i]);
      }
   }

   /* Print RN field */
   for (i = 62; i < 64; i++) {
      mybit = 1LL << (63 - i);
      if (mybit & local_fpscr) {
         printf (" %s", fpscr_strings[i]);
      }
   }
}

/*
 * This prints the entire FPSCR field.  This is only called under higher
 * verbosities, as valgrind does not track most of these bits.
 */
void dissect_fpscr_raw (unsigned long local_fpscr) {
/* Due to the additional involved logic, the rounding mode (RN) bits 61-62
 * are handled within dissect_fpscr_rounding_mode (). */
   int i;
   long mybit;
   for (i = 0; i < 61; i++) {
      /* also note that the bit numbering is backwards. */
      mybit = 1LL << (63 - i);
      if (mybit & local_fpscr) {
         printf (" %s", fpscr_strings[i]);
      }
   }
}

void dissect_fpscr (unsigned long local_fpscr) {
   if (debug_show_values) {
      printf (" [[ fpscr:%lx ]] ", local_fpscr);
      dissect_fpscr_raw (local_fpscr);
   } else {
      dissect_fpscr_valgrind (local_fpscr);
   }
}


/* *************** */
/* Buffer Helpers.
Define both a base and a reference buffer.  When printing results, only print
the values when there is a difference between the two. */
#define BUFFER_SIZE 12
/* Note: Watch the alignment of the buffer, some loads/stores may require
stronger alignments. */
__attribute__ ( (aligned (16))) unsigned long long buffer[2*BUFFER_SIZE];
__attribute__ ( (aligned (16))) unsigned long long reference_buffer[2*BUFFER_SIZE];
unsigned long changed_index[2*BUFFER_SIZE];
void initialize_buffer (int t)
{
   int x;
   for (x = 0; x < BUFFER_SIZE; x++)
      /* We don't want each of the 32-bit chunks to be identical since loads
       * of a byte from the wrong 32-bit chuck may be difficult to spot.
       * Load these up with values that are also interesting if SP/DP, etc.
      */
      switch ( (t+x)%BUFFER_SIZE) {
      case  0: buffer[x] = 0x3fe00094e0007359; break; // sp
      case  1: buffer[x] = 0x7ff7020304057607; break; // nan
      case  2: buffer[x] = 0x7ff0000000007000; break; // inf
      case  3: buffer[x] = 0x7f0000007f007000; break; // sp pair.
      case  4: buffer[x] = 0x5a05a05a05a07a05; break;
      case  5: buffer[x] = 0x0102030405067708; break;
      case  6: buffer[x] = 0xfedcba9876547210; break;
      case  7: buffer[x] = 0x0123456789ab7def; break;
      case  8: buffer[x] = 0xffeeddccbbaa7988; break;
      default: buffer[x] = 0x1112111211127112* (x-8); break;
   }
   for (x = 0; x < BUFFER_SIZE; x++)
	reference_buffer[x] = buffer[x];
}

/* Buffer printing helper.   This only displays the contents if they have
   changed with respect to the reference buffer, or if running under
   high verbosity. */
void dump_changed_buffer (unsigned long range) {
   int x;
   int buffer_changed = 0;

   for (x = 0; (x < BUFFER_SIZE) && (x<range) ; x++) {
      changed_index[x] = 0;
      if (buffer[x] !=reference_buffer[x]) {
	 buffer_changed = 1;
	 changed_index[x] = 1;
	 if (debug_show_values)
	    printf (" {idx %d %016llx %016llx}",
		    x, reference_buffer[x] , buffer[x] );
      }
   }
   if (debug_show_values || buffer_changed) {
      printf (" [");
	 for (x = 0; x < BUFFER_SIZE && (x<range); x++) {
	    if (x) printf (" ");
	    if (verbose)
	       printf ("%s%016llx", changed_index[x] == 1?"*":" ", buffer[x] );
	    if (changed_index[x]) {
	       if (instruction_is_sp) {
		  printf (" (");
		  special_print_sp_value (0xffffffff & buffer[x] >> 32 );
		  printf (" ");
		  special_print_sp_value (0xffffffff & buffer[x]);
		  printf (") ");
	       } else if (instruction_is_dp) {
		  printf (" {");
		  special_print_dp_value (buffer[x]);
		  printf ("} ");
	       }
	        printf ("%016llx", buffer[x]);
	    } else
		printf (" - ");
	 }
      printf ("]");
   }
}

void dump_raw_buffer () {
   int x;
   printf ("buffer:[");
   for (x = 0; x < BUFFER_SIZE ; x++) {
      if (x%4 == 0) printf (" (%d)", x);
         printf ("%016llx ", buffer[x]);
   }
   printf ("]");
}

void dump_small_buffer (void) {
  dump_changed_buffer (8);
}

void dump_large_buffer (void) {
  dump_changed_buffer (8);
}

void dump_buffer () {
if (debug_show_values) printf (" buffer:");
  if (uses_quad) {
    dump_large_buffer ();
  } else {
    dump_small_buffer ();
  }
}

/* **** Reloc Buffer **************************************** */
/* Create a large buffer to be the destination for pc-relative
 * writes.  This test is built with linker hints in order
 * to ensure our buffer, stored in the .bss section, is at a
 * mostly known offset from the instructions being exercised,
 * so a hardcoded offset from the PC (pc-relative) will be
 * on-target.
 * If there are significant reworks to the code, the bss or
 * text sections, or the offsets used may need to change.
 *
 * The linker hints are specifically -Tbss and -Ttext.
 * gcc foo.c test_isa_3_1_common.c -I../../../   -Wl,-Tbss 0x20000 -Wl,-Ttext 0x40000
 */
 /* RELOC_BUFFER_SIZE is defined to 0x1000 in isa_3_1_helpers.h  */
#define RELOC_BUFFER_PATTERN 0x0001000100010001
volatile unsigned long long pcrelative_write_target[RELOC_BUFFER_SIZE];

/* Initialize the buffer to known values. */
void init_pcrelative_write_target() {
       int i;
       for (i=0;i<RELOC_BUFFER_SIZE;i++)
               pcrelative_write_target[i]=i*RELOC_BUFFER_PATTERN;
}

/* Review the pcrelative_write_target buffer; and print any
   elements that vary from the initialized value.
   Exclude portions of the output as appropriate if the current test
   is marked for byte,halfword,word.  */
void print_pcrelative_write_target() {
  int i,z,rshift;
  unsigned long long curr_value;
  unsigned long long ref_value;
  unsigned long long curr_token,init_token;
  for (i=0;i<RELOC_BUFFER_SIZE;i++) {
    ref_value=i*RELOC_BUFFER_PATTERN;
    curr_value = pcrelative_write_target[i];
    if (ref_value != curr_value) {
      printf(" ");
      if (verbose)
	printf("delta found: %d %llx -> %llx\n",i,ref_value,curr_value);
      if (updates_byte) {
	for (z=0;z<8;z++) {
	  rshift=z*8;
	  if (verbose) printf("z:%d ",z);
	  init_token = (ref_value>>rshift) & 0xff;
	  curr_token = (curr_value>>rshift) & 0xff;
	  if (verbose)
	    printf("wms byte:: %llx -> %llx \n",init_token,curr_token);
	  if (init_token == curr_token && (updates_byte||updates_halfword||updates_word) ) {
	     printf("%2s","  ");
	  } else {
	    printf("%02llx",curr_token);
	  }
        }
      }
      else if (updates_halfword) {
	for (z=0;z<4;z++) {
	  rshift=z*16;
	  if (verbose) printf("z:%d ",z);
	  init_token = (ref_value>>rshift) & 0xffff;
	  curr_token = (curr_value>>rshift) & 0xffff;
	  if (verbose)
	    printf("wms half:: %llx -> %llx \n",init_token,curr_token);
	  if (init_token == curr_token) {
	     printf("%2s","  ");
	  } else {
	    printf("%04llx",curr_token);
	  }
        }
      }
      else if (updates_word) {
	for (z=0;z<2;z++) {
	  rshift=z*32;
	  if (verbose) printf("z:%d ",z);
	  init_token = (ref_value>>rshift) & 0xffffffff;
	  curr_token = (curr_value>>rshift) & 0xffffffff;
	  if (verbose)
	    printf("wms word:: %llx -> %llx \n",init_token,curr_token);
	  if (init_token == curr_token ) {
	     printf("%2s","  ");
	  } else {
	    printf("%08llx",curr_token);
	  }
        }
      }
      else {
	printf("%016llx ",curr_value);
      }
    }
  }
}

/* Helper that returns the address of the pcrelative_write_target buffer.
   Due to variances in where the sections land in memory, this value is
   used to normalize the results.  (see paddi tests for usage).   */
unsigned long long pcrelative_buff_addr(int x) {
   /* Return the base address of the array.  The base address will be
      a function of the code load address.  */
   return (unsigned long long) &pcrelative_write_target[x];
}

void print_undefined () {
   if (debug_show_values)
      printf (" [Undef]");
   else
      printf ("        ");
}

/* print the input 64-bit vector as 32-bit SP lumps. */
void print_vec_as_sp (unsigned long long ull64) {
   printf (" %08llx", ull64 >> 32 );
   printf (" %08llx", ull64 & 0xffff );
}

/*------------------------------------------------------------------*/
/* Decimal Floating Point (DFP) helper functions */
/*------------------------------------------------------------------*/
#define NOT(x)   ( ( ( x ) == 0) ? 1 : 0)
#define GET(x,y) ( ( ( x ) & ( 0x1UL << ( y ) ) ) >> ( y ) )
#define PUT(x,y) ( ( x )<< ( y ) )

unsigned long dpb_to_bcd ( unsigned long chunk )
{
   int a, b, c, d, e, f, g, h, i, j, k, m;
   int p, q, r, s, t, u, v, w, x, y;
   unsigned long value;

   /* convert 10 bit densely packed BCD to BCD */
   p = GET ( chunk, 9 );
   q = GET ( chunk, 8 );
   r = GET ( chunk, 7 );
   s = GET ( chunk, 6 );
   t = GET ( chunk, 5 );
   u = GET ( chunk, 4 );
   v = GET ( chunk, 3 );
   w = GET ( chunk, 2 );
   x = GET ( chunk, 1 );
   y = GET ( chunk, 0 );

   /* The BCD bit values are given by the following boolean equations.*/
   a = ( NOT (s) & v & w ) | ( t & v & w & s ) | ( v & w & NOT (x) );
   b = ( p & s & x & NOT (t) ) | ( p & NOT (w) ) | ( p & NOT (v) );
   c = ( q & s & x & NOT (t) ) | ( q & NOT (w) ) | ( q & NOT (v) );
   d =r;
   e = ( v & NOT (w) & x ) | ( s & v & w & x ) | ( NOT (t) & v & x & w );
   f = ( p & t & v & w & x & NOT (s) ) | ( s & NOT (x) & v ) | ( s & NOT (v) );
   g = ( q & t & w & v & x & NOT (s) ) | ( t & NOT (x) & v ) | ( t & NOT (v) );
   h = u;
   i = ( t & v & w & x ) | ( s & v & w & x ) | ( v & NOT (w) & NOT (x) );
   j = ( p & NOT (s) & NOT (t) & w & v ) | ( s & v & NOT (w) & x )
      | ( p & w & NOT (x) & v ) | ( w & NOT (v) );
   k = ( q & NOT (s) & NOT (t) & v & w ) | ( t & v & NOT (w) & x )
      | ( q & v & w & NOT (x) ) | ( x & NOT (v) );
   m = y;

   value = PUT (a, 11) | PUT (b, 10) | PUT (c, 9) | PUT (d, 8) | PUT (e, 7)
               | PUT (f, 6) | PUT (g, 5) | PUT (h, 4) | PUT (i, 3) | PUT (j, 2)
               | PUT (k, 1) | PUT (m, 0);
    return value;
}
#undef NOT
#undef GET
#undef PUT

/* get_declet ().  Return a 10-bit declet, beginning at the 'start'
 * offset.
 *
 * | dword1 | dword0 |
 * | 0    63|64   127|
 */
#define TEN_BITS 0x03ffULL

int get_declet (int start, uint64_t dword1, uint64_t dword0) {
   unsigned long local_declet;
   unsigned int dword0_shift;
   unsigned int dword1_shift;

   dword1_shift = 63 - (start + 9);
   dword0_shift = 127 - (start + 9);

   if (debug_show_all_regs) printf ("\n%s (%d) %016lx %016lx",
                         __FUNCTION__, start, dword1, dword0);

   if ( (start + 9) < 63) { /* fully within dword1 */
      local_declet = (dword1 >> dword1_shift) & TEN_BITS;

   } else if (start >= 65) {/* fully within dword0 */
      local_declet = (dword0 >> dword0_shift) & TEN_BITS;

   } else { /* straddling the two dwords*/
      unsigned long mask_dword0;
      unsigned long mask_dword1;

      mask_dword1 = TEN_BITS >> (64 - dword0_shift);
      mask_dword0 = TEN_BITS << (dword0_shift);
      local_declet =
         ( (dword1 & mask_dword1) << (64-dword0_shift)) +
         ( (dword0 & mask_dword0) >> dword0_shift);
   }
   return local_declet;
}

int get_bcd_digit_from_dpd (int start, uint64_t dword1,
                                  uint64_t dword0) {
   long bcd_digit;
   long declet;

   declet = get_declet (start, dword1, dword0);
   bcd_digit = dpb_to_bcd (declet);
   return bcd_digit;
}

/* For DFP finite numbers, the combination field (G field) is a
 * combination of the exponent and the LMD (Left Most Digit) of the
 * significand.  The fields are encoded/decoded as described in the
 * table here.
 *       00       01      10   -< Exponent bits.
 * 0:   00000   01000   10000
 * ...
 * 7:   00111   01111   10111
 * 8:   11000   11010   11100
 * 9:   11001   11011   11101  (encoded special field).
 * |
 * ^ LMD value.
*/
#define DFP_GFIELD_MASK  0x7c00000000000000UL
#define DFP_GFIELD_SHIFT 58
//The exponent bias value is 101 for DFP Short, 398
//for DFP Long, and 6176 for DFP Extended.
#define DFP128_EXPONENT_BIAS 6176
#define DFP64_EXPONENT_BIAS   398

unsigned int special_field_LMD (uint64_t dword1) {
   unsigned long g_field_specials;
   int left_two_bits;
   int right_three_bits;

   g_field_specials = (dword1 & DFP_GFIELD_MASK) >> DFP_GFIELD_SHIFT;
   left_two_bits = (g_field_specials & 0x18) >> 3;
   right_three_bits = g_field_specials & 0x07;

   /* The LMD result maps directly to the right_three_bits value as
    * long as the left two bits are 0b00, 0b01, 0b10.  So a compare
    * against 3 is sufficient to determine if we can return the right
    * three bits directly.  (LMD values 0..7).
    */
   if (left_two_bits < 3) {
      return (right_three_bits);
   }

   /* LMD values of 8 or 9 require a bit of swizzle, but a check of
    * the right-most bit is sufficient to determine whether LMD value
    * is 8 or 9.
    */
   if (right_three_bits & 0x1)
      return 9;
   else
      return 8;
}

/* Returns the exponent bits, as decoded from the G field. */
int special_field_exponent_bits (unsigned long dword1) {
   unsigned long g_field_specials;
   int left_two_bits;
   int right_three_bits;

   g_field_specials = (dword1 & DFP_GFIELD_MASK) >> DFP_GFIELD_SHIFT;
   left_two_bits = (g_field_specials & 0x18) >> 3;
   right_three_bits = g_field_specials & 0x07;

   /* The special field exponent bits maps directly to the left_two_bits
    * value as long as the left two bits are 0b00, 0b01, 0b10.  So a compare
    * against 3 is sufficient for those values.
    */
   if (left_two_bits < 3) {
      return (left_two_bits);
   }

   switch (right_three_bits) {
      case 0:
      case 1: return 0x0;
      case 2:
      case 3: return 0x1;
      case 4:
      case 5: return 0x2;
      case 6: /* Infinity */ return 0x0;
      case 7: /* NaN */  return 0x0;
   }
   return -1;  /* should never hit this */
}

/* The 'exponent left' shift is for moving the leftmost two bits
 * of the exponent down to where they can be easily merged with the
 * rest of the exponent.
 */
#define DFP128_EXPONENT_RIGHT_MASK       0x03ffc00000000000
#define DFP64_EXPONENT_RIGHT_MASK        0x03fc000000000000
#define DFP128_EXPONENT_RIGHT_MASK_SHIFT 46
#define DFP64_EXPONENT_RIGHT_MASK_SHIFT  50
#define DFP128_EXPONENT_LEFT_SHIFT       12
#define DFP64_EXPONENT_LEFT_SHIFT         8

#define DFP_NAN                          0x1f
#define DFP_INF                          0x1e
#define DFP_SIGNALING_NAN_BIT            0x0200000000000000

/* return the dfp exponent from the leading dword. */
signed long dfp128_exponent (unsigned long dword1) {
   unsigned long exponent_left;
   unsigned long exponent_right;
   unsigned long biased_exponent;
   signed long exponent;

   exponent_left = special_field_exponent_bits (dword1);
   exponent_right = (dword1 & DFP128_EXPONENT_RIGHT_MASK);
   biased_exponent = (exponent_left << DFP128_EXPONENT_LEFT_SHIFT) +
                     (exponent_right >> DFP128_EXPONENT_RIGHT_MASK_SHIFT);

   /* Unbias the exponent. */
   exponent = biased_exponent - DFP128_EXPONENT_BIAS;
   return exponent;
}

/* Interpret the paired 64-bit values as a extended (quad) 128 bit DFP.
 *
 * | Significand | Combination Field/  |                          |
 * | sign bit    | Encoded Exponent    | remainder of significand |
 * |0            |1                  17|18                     127|
 *  ^ (bit0) Significand sign bit.
 *                ^ (bit 1:17) Combination field. Contains high bits of
 *                  exponent (encoded), LMD of significand (encoded), 
 *                  and the remainder of the exponent.  First five bits
 *                  will indicate special cases NAN or INF.
 *                                     ^ (bit 18:127) Remainder of the
 *                                       significand.
 */

#define DFP128_COMBINATION_MASK     0x7fffc
#define DFP64_COMBINATION_MASK      0x7ffc
#define DFP128_COMBINATION_SHIFT    46
#define DFP64_COMBINATION_SHIFT     50
#define DFP_SPECIAL_SYMBOLS_MASK    0x1f
#define DFP_SPECIAL_SYMBOLS_SHIFT   58

#define DFP_NAN			    0x1f
#define DFP_INF			    0x1e
#define DFP_SIGNALING_NAN_BIT	    0x0200000000000000

#define DFP128_T_START		    18

void dissect_dfp128_float (uint64_t dword1, uint64_t dword0) {
   long signbit;
   signed long exponent;
   unsigned long gfield_special_symbols;
   unsigned long lmd_digit;
   unsigned long bcd_digits[13];
   int i;
   int silent = 0; // suppress leading zeros from the output.

   if (debug_show_raw_values)
      printf ("DFP128R:%016lx, %016lx", dword1, dword0);

   signbit = (dword1 >> 63);

   if (signbit) printf (" -");
   else         printf ("  ");

   gfield_special_symbols =
      ((dword1 >> DFP_SPECIAL_SYMBOLS_SHIFT) & DFP_SPECIAL_SYMBOLS_MASK);

   switch (gfield_special_symbols) {
      case DFP_INF:
         printf (   "inf    ");
         break;

      case DFP_NAN:
         if (dword1 & DFP_SIGNALING_NAN_BIT)
            printf ("SNaN   ");
         else
            printf ("QNaN   ");
         break;

      default:
         // printf (   "Finite ");
         exponent  = dfp128_exponent (dword1);
         // printf ("Exponent: %d Bias: %d ", exponent, DFP128_EXPONENT_BIAS );

         lmd_digit = special_field_LMD (dword1);
         for (i = 0; i < 11; i++) {
            bcd_digits[i] = get_bcd_digit_from_dpd ( (DFP128_T_START
                                                    + 10 * i), dword1, dword0);
         }
         if (lmd_digit) {
            silent++;
            printf ("%01lx", lmd_digit);
         } else {
            printf (" ");
         }
         for (i = 0; i < 11; i++) {
            if (bcd_digits[i] || silent ) {
               silent++;
               printf ("%01lx", bcd_digits[i]);
            } else {
               /* always print at least the last zero */
               if (i == 10)
                  printf ("0");
               else
                  printf (" ");
            }
         }
         printf (" * 10^");
         printf ("%ld", exponent);
   }
}

void print_vsr (int vsr_to_print) {
unsigned long long blob1 = 0, blob2 = 0;
   switch (vsr_to_print) {
	case 26:
	__asm__ __volatile__ ("mfvsrd %0, 26":"=r" (blob1));
	__asm__ __volatile__ ("mfvsrld %0, 26":"=r" (blob2));
	break;
	case 27:
	__asm__ __volatile__ ("mfvsrd %0, 27":"=r" (blob1));
	__asm__ __volatile__ ("mfvsrld %0, 27":"=r" (blob2));
	break;
	case 28:
	__asm__ __volatile__ ("mfvsrd %0, 28":"=r" (blob1));
	__asm__ __volatile__ ("mfvsrld %0, 28":"=r" (blob2));
	break;
	case 29:
	__asm__ __volatile__ ("mfvsrd %0, 29":"=r" (blob1));
	__asm__ __volatile__ ("mfvsrld %0, 29":"=r" (blob2));
	break;
	default:
	  printf ("Add entry for VSR %d to %s in %s.\n", vsr_to_print, __FUNCTION__, __FILE__);
	}
if (debug_show_labels)
	printf (" VSR (%d):", vsr_to_print);
printf (" %llx, %llx ", blob1, blob2);
}

void print_frt () {
   unsigned long long value1, value3;
   if (has_frt || debug_show_all_regs ) {
      if (debug_show_labels) printf (" frt%s:", has_frtp?"p":"" );
      /* If the result is a dfp128 value, the dfp128 value is
         contained in the frt, frtp values which are split across
         a pair of VSRs.  */
      if (!instruction_is_relative && uses_dfp128_output) {
	 if (verbose) print_vsr (28);
	 if (verbose) print_vsr (29);
	 value1 = get_vsrhd_vs28 ();
	 value3 = get_vsrhd_vs29 ();
	 dissect_dfp128_float (value1, value3);
      } else {
	 if (debug_show_raw_values) generic_print_float_as_hex (frt);
	 if (instruction_is_relative) {
	    printf ("_ %e _ ", frt);
	    print_vsr (28);
	 } else {
		printf (" %e", frt);
	  }
	 if (has_frtp) {
	    if (debug_show_raw_values) generic_print_float_as_hex (frtp);
	    printf (" %e", frtp);
	 }
      }
   }
}

/* implementation detail.. FRS and FRB use the same set of regs. */
void print_frs_or_frb () {
   unsigned long long vsrvalue1, vsrvalue3;
   if (debug_show_labels) {
      printf (" fr" );
      if (has_frs) printf ("s%s:", has_frsp?"p":"" );
      else if (has_frb) printf ("b%s:", has_frbp?"p":"" );
      else printf("?");
   }
   if (uses_dfp128_input) {
      if (verbose) print_vsr (26);
      if (verbose) print_vsr (27);
      vsrvalue1 = get_vsrhd_vs26 (); vsrvalue3 = get_vsrhd_vs27 ();
      dissect_dfp128_float (vsrvalue1, vsrvalue3);
   } else if (instruction_is_dp) {
      generic_print_double_as_hex (frsb);
      generic_print_double_as_hex (frsbp);
   } else if (instruction_is_sp) {
      generic_print_float_as_hex (frsb);
      generic_print_float_as_hex (frsbp);
   } else {
      printf (" %18.8e", frsb);
      printf (" %18.8e", frsbp);
   }
}

void print_ra () {
   if (debug_show_labels) printf (" ra:");
   /* special case for when ra == &buffer. */
   if ( (void *)ra == &buffer )
      printf (" (&buffer)");
   else if ( (void *)ra != &buffer || debug_show_raw_values) {
      printf (" %lx", ra);
   }
}

void print_rb () {
   if (debug_show_labels) printf (" rb:");
   if ( (void *)rb == &buffer)
       printf (" (&buffer)");
   else
      printf (" %lx", rb);
}

void print_rc () {
   if (debug_show_labels) printf (" rc:");
      printf (" %lx", rc);
}

void print_rs () {
  if (debug_show_labels) printf (" rs:");
    printf (" %lx", rs);
}

// Second half of a rs pair.
void print_rsp () {
   if (debug_show_labels) printf (" rsp:");
     printf (" %lx", rsp);
}

void print_rs_or_rsp () {
   if (debug_show_labels) printf (" rs:");
   printf (" %lx", rs);
   if (has_rsp) {
       if (debug_show_labels) printf (" rsp:");
	  printf (" %lx", rsp);
    }
}

void print_rt () {
   if (debug_show_labels) printf (" rt%s:", has_rtp?"p":"");
   printf (" %16lx", rt);
   if (has_rtp) {
      printf (" %16lx", rtp);
   }
}

void print_vra () {
   if (debug_show_labels) printf (" vra:");
   printf (" %016llx,%016llx", vra[0], vra[1]);
}

void print_vrb () {
   if (debug_show_labels) printf (" vrb:");
   printf (" %016llx,%016llx", vrb[0], vrb[1]);
}

void print_vrc () {
   if (debug_show_labels) printf (" vrc:");
   printf (" %016llx,%016llx", vrc[0], vrc[1]);
}

/* for VRM, don't print leading zeros for better visibility of diffs */
void print_vrm () {
   if (debug_show_labels) printf (" vrm:");
   printf (" %16llx,%16llx", vrm[0], vrm[1]);
}

void print_vrt () {
   if (debug_show_labels) printf (" vrt:");
   if (debug_show_raw_values || (output_mask && uses_load_buffer )) {
      printf (" %16llx,", vrt[1]);
      printf ( "%016llx", vrt[0]);
   }
   if (!post_test) return;
   if (!output_mask) {
      printf (" %16llx,", vrt[1]);
      printf ("%016llx", vrt[0]);
   } else {
      /* there is a mask requiring special handling. */
      if (instruction_is_dp) {
	 if (output_mask&DP0)
	    special_print_dp_value (vrt[1]);
	 if (output_mask&DP1)
	    special_print_dp_value (vrt[0]);
      }
      if (instruction_is_sp) {
	 if (output_mask&SP0)
	    special_print_sp_value (0xffffffff&vrt[1]>>32);
	 if (output_mask&SP1)
	   special_print_sp_value (0xffffffff&vrt[1]);
	 if (output_mask&SP2)
	   special_print_sp_value (0xffffffff&vrt[0]>>32);
	 if (output_mask&SP3)
	   special_print_sp_value (0xffffffff&vrt[0]);
      }
   }
}

void print_xtp () {
if (debug_show_labels) printf (" vec_x[st]p:" );
    printf (" %016llx", XTp0[0]);
    printf (" %016llx", XTp0[1]);
    printf (" %016llx", XTp1[0]);
    printf (" %016llx", XTp1[1]);
}

void print_xsp () {
    print_xtp();
}

void print_xa() {
	if (debug_show_labels) printf (" xa:");
	printf (" %016llx,", vec_xa[0] );
	printf ("%016llx", vec_xa[1] );
}

/* xc may also hold the second half of an xa pair */
void print_xc() {
	if (debug_show_labels) printf (" xc:");
	 printf (" %016llx,", vec_xc[0] );
	 printf ("%016llx", vec_xc[1] );
}

/* xap is the pair at rs22 (xa) and rs23 (xc). */
void print_xap() {
	if (debug_show_labels) printf (" xap:");
	print_xa();
	print_xc();
}

void print_xb () {
   if (debug_show_labels) printf (" xb:");
   if (instruction_is_sp_estimate) {
      print_vec_as_sp (vec_xb[0]);
      printf (",");
      print_vec_as_sp (vec_xb[1]);
   } else {
      printf (" %016llx,", vec_xb[0] );
      printf ("%016llx", vec_xb[1] );
   }
}

void print_xs () {
   if (debug_show_labels) printf (" vec_xs:");
      printf (" %016llx,", vec_xs[0] );
      printf ("%016llx", vec_xs[1] );
}

void print_xt () {
if (debug_show_labels) printf (" vec_xt:" );
    if (debug_show_raw_values) {
	printf (" %16llx", vec_xt[0]);
	printf (" %16llx", vec_xt[1]);
    }
    // Don't print the xt value unless we are post-instruction test.
    if (!post_test) return;
    if (!output_mask ) {
	if (vec_xt[0] == (unsigned long)&buffer) printf (" (&buffer) ");
	else printf (" %16llx", vec_xt[0]);
	if (vec_xt[1] == (unsigned long)&buffer) printf (" (&buffer) ");
	else printf (" %16llx", vec_xt[1]);
    } else {
	/* there is a mask requiring special handling. */
	if (instruction_is_dp) {
	    if (output_mask&0b100000)
		special_print_dp_value (vec_xt[0]);
	    if (output_mask&0b010000)
		special_print_dp_value (vec_xt[1]);
	} else if (instruction_is_sp) {
	    if (output_mask&0b1000)
		special_print_sp_value (0xffffffff&vec_xt[0]>>32);
	    else print_undefined ();
	    if (output_mask&0b0100)
		special_print_sp_value (0xffffffff&vec_xt[0]);
	    else print_undefined ();
	    if (output_mask&0b0010)
		special_print_sp_value (0xffffffff&vec_xt[1]>>32);
	    else print_undefined ();
	    if (output_mask&0b0001)
		special_print_sp_value (0xffffffff&vec_xt[1]);
	    else print_undefined ();
	} else if (instruction_is_b16) {
	    if (output_mask&B16_0)
		special_print_bf16_value (0xffffff& (vec_xt[0]>>48));
	    else
		print_undefined ();
	    if (output_mask&B16_1)
		special_print_bf16_value (0xffffff& (vec_xt[0]>>32));
	    else
		print_undefined ();
	    if (output_mask&B16_2)
		special_print_bf16_value (0xffffff& (vec_xt[0]>>16));
	    else
		print_undefined ();
	    if (output_mask&B16_3)
		special_print_bf16_value (0xffffff& (vec_xt[0]    ));
	    else
		print_undefined ();
	    if (output_mask&B16_4)
		special_print_bf16_value (0xffffff& (vec_xt[1]>> 48));
	    else
		print_undefined ();
	    if (output_mask&B16_5)
		special_print_bf16_value (0xffffff& (vec_xt[1]>> 32));
	    else
		print_undefined ();
	    if (output_mask&B16_6)
		special_print_bf16_value (0xffffff& (vec_xt[1]>> 16));
	    else
		print_undefined ();
	    if (output_mask&B16_7)
		special_print_bf16_value (0xffffff& (vec_xt[1]     ));
	    else
		print_undefined ();
	}
	else 
		printf("lost special handling on instruction (sp,dp,bf) type. \n");
    }
}

// print_register_header* ; print our testcase input values.
// if verbosity is set, print all defined values, including
// the output register contents, regardless
// of whether they are used for this test.
void print_all() {
	printf("\nALL:\n");
	print_ra();
	printf("\n");
	print_rb();
	printf("\n");
	print_rs();
	printf("\n");
	print_rsp();
	printf("\n");
	print_xap(); // includes print_xa, print_xc
	printf("\n");
	print_xb();
	printf("\n");
	print_xsp(); // includes print_xs, print_xt  ???
	printf("\n");
	print_xtp();
	printf("\n");
	print_vra();
	printf("\n");
	print_vrb();
	printf("\n");
	print_vrc();
	printf("\n");
	print_vrm();
	printf("\n");
	print_frs_or_frb();
	printf("\n");
	print_accumulator();
	printf("\n");
	dump_buffer();
	printf("\nEND_ALL\n");
}

// Call print_register_header_all if we have verbosity set and 
// want to print ALL input fields.
// Otherwise, print the input values that are used by the
// instructions under test.
void print_register_header () {
  post_test = 0;
  if (debug_show_all_regs) print_all();

  if (has_ra) {
	  /* Suppress the print of RA if the instruction has
	     R==1, since the ra value must be zero for the
	     instruction to be valid.  */
	  if (!instruction_is_relative)
		 print_ra();
  }

  if (has_rb) print_rb ();
  if (has_rc) print_rc ();
  if (has_rs) print_rs();
  if (has_rsp) print_rsp();
  if (has_xap) {
	  print_xap();
  } else {
	  if (has_xa) print_xa();
	  if (has_xc) print_xc();
  }
  if (has_xb) print_xb ();
  if (has_xsp) {
	  print_xsp();
  } else {
     if (has_xs) print_xs();
  }

  if (has_vra) print_vra ();
  if (has_vrb) print_vrb ();
  if (has_vrc) print_vrc ();
  if (has_vrm) print_vrm ();
  
  if (has_frs || has_frb) print_frs_or_frb ();
  if (uses_acc_src) print_accumulator ();
  if (uses_load_buffer) dump_buffer ();
}

void print_register_footer () {
   post_test = 1;
   if ( (uses_CRBIT || debug_show_all_regs || (uses_RC ) )) {
      if (debug_show_labels) printf (" CR:");
	 printf (" [%08lx]", current_cr);
   }
   if (current_fpscr) dissect_fpscr (current_fpscr);
   if (uses_RC) dissect_cr_rn (current_cr, 6);
   if (uses_acc_dest || uses_acc_vsrs)  print_accumulator ();
   if (has_vrt || debug_show_all_regs) print_vrt ();
   if (has_xt || debug_show_all_regs) {
      if (has_xtp) {
	 print_xtp ();
      } else {
	  print_xt ();
      }
   }
   if (has_ra_target || debug_show_all_regs) print_ra ();
   if (has_rt || debug_show_all_regs) print_rt ();
   if (has_frt || debug_show_all_regs) print_frt ();
}

void generic_prologue () {
   if (verbose)
      printf (" %s %s \n", __DATE__, __TIME__);
}

/*
  Helpers to build the VSX input table.
*/
#define MAX_VSX_ARRAY_SIZE 42
unsigned long long nb_divmod_num_vsxargs;
unsigned long long nb_divmod_den_vsxargs;
unsigned long long nb_vsxargs;
unsigned long long * vsxargs = NULL;
void build_vsx_table (void)
{
   long i = 0;
   vsxargs = memalign (16, MAX_VSX_ARRAY_SIZE * sizeof (unsigned long long));
/*
  The following hex values map to assorted Fp values including zero, inf, nan.
  +/-INF   EXP:MAX  FRAC:0
  +/-NOR   EXP:!0   FRAC:!0
  +/-DEN   EXP:0    FRAC:!0
  +/-zero  EXP:0    FRAC:0
*/
//   		    |	    | 		//  SP                     || DP
   vsxargs[i++] = 0x7F800000ff800000UL; // +inf, -inf		   || NOR (big)
   vsxargs[i++] = 0xff8000007f800000UL; // -inf, +inf		   || -NOR (big)
   vsxargs[i++] = 0xff7ffffe7f7ffffeUL; // -NOR (big), +NOR (big)  || +NOR (big)
   vsxargs[i++] = 0x0080000e8080000eUL; // +NOR (tiny), -NOR (tiny)|| +NOR (tiny)
   vsxargs[i++] = 0x0180055e0180077eUL; // +NOR (rnd), -NOR (rnd)  || random #
   nb_divmod_den_vsxargs = i;  // Values that are safe to divide by are above.
   vsxargs[i++] = 0x0000111e8000222eUL; // +den, -den		   || den
   vsxargs[i++] = 0x7ff0000000000000UL; // NAN, +zero		   || +inf
   vsxargs[i++] = 0xfff0000000000000UL; // NAN, +zero		   || -inf
   vsxargs[i++] = 0x2208400000000000UL; // dfp128 value
   vsxargs[i++] = 0x0000000000000009UL; // dfp128 value
   vsxargs[i++] = 0xffff000180000001UL; // NAN, NOR		   || NAN
   vsxargs[i++] = 0x0000000000000000UL; // +zero, +zero		   || +zero
   vsxargs[i++] = 0x8000000000000000UL; // -zero, +zero		   || -zero
   nb_divmod_num_vsxargs = i;  // Values that are safe to be divided are above.
   nb_vsxargs = i;
/* Eyecatcher.  If there are any 999_999 patterns in the generated output,
   it is likely a nb_vsx* bounds check has been missed.
   This may also be seen if we are dealing with a quadword instruction
   and have an odd number of pairs.  */
   vsxargs[i++] = 0x9999999999999999UL;
   vsxargs[i++] = 0x9999999999999999UL;
}

#define CHECK_LINES \
if ( (i) == nb_divmod_num_vsxargs) printf ("--numerator line--\n");	\
if ( (i) == nb_divmod_den_vsxargs) printf ("--denominator line--\n");

void dump_vsxargs () {
   int i;
   printf ("\ndump_vsxargs:\n");
   printf ("SP: \n");
   for (i = 0;i<nb_vsxargs;i++) {
      CHECK_LINES
      printf ("%2d:", i);
      printf ("raw:%08llx ", (0xffffffff & (vsxargs[i]>>32)));
      printf ("%08llx ", 0xffffffff & vsxargs[i]);
      dissect_sp_value (0xffffffff & (vsxargs[i]>>32));
      dissect_sp_value (0xffffffff & (vsxargs[i]>>0));
      printf ("\n");
   }
   printf ("\n DP: \n");
   for (i = 0;i<nb_vsxargs;i++) {
      CHECK_LINES
      printf ("%2d:", i);
      printf ("%016llx ", vsxargs[i]);
      dissect_dp_value ( (vsxargs[i]));
      printf ("\n");
   }
}

unsigned long nb_args;
unsigned long long * args = NULL;
void build_args_table (void)
{
   long i = 0;
   args = memalign (16, MAX_VSX_ARRAY_SIZE * sizeof (unsigned long));

   args[i++] = 0x0102030405060708UL;
   args[i++] = 0xa5b4c3d2e1f00918UL;
   args[i++] = 0xfff7fffafff3fff1UL;
   args[i++] = 0x7ff7000100030005UL;
   args[i++] = 0xffe7111022203330UL;
   args[i++] = 0x0000000000000000UL;
   nb_args = i;
   if (verbose>1)
	printf ("Registered %ld args values\n", nb_args);
}

/* hardcoded dfp128 table.  */
unsigned long long dfp128_vals[] = {
   // Some finite numbers
   0x2208000000000000ULL, 0x0000000000000001ULL, //  1 *10^0
   0xa208800000000000ULL, 0x0000000000000001ULL, // -1 *10^1
   0x2208000000000000ULL, 0x0000000000000000ULL, // 0*10^256
   0x0000000000000000ULL, 0x0000000000000001ULL, //  1 *10^-6176. (smallest exp)
   0x77ffc00000000000ULL, 0x0000000000000001ULL, //  1 *10^6111 (largest exp)
   0x77ffffffffffffffULL, 0xffffffffffffffffULL, // max possible value *10^6111 (largest exp)
   0x0000000000000000ULL, 0x0000000000000001ULL, // min possible value 1 *10^-6176. (smallest exp)
   0x8000000000000000ULL, 0x0000000000000001ULL, // -1 *10^-6176. (smallest exp)
   0xa208800000000000ULL, 0x0000000000000777ULL, // other neg value.
   0x2208400000000000ULL, 0x0000000000000009ULL, // other value.
   0x2208800000000011ULL, 0x1110000678900009ULL, // other value.
   // flavors of zero
   0x2208000000000000ULL, 0x0000000000000000ULL, // 0*10^256
   0xa208000000000000ULL, 0x0000000000000000ULL, // -0*10^0
   0xa248000000000000ULL, 0x0000000000000000ULL, // 0*10^256
   // flavors of NAN
   0x7c00000000000000ULL, 0x0000000000000000ULL, // quiet
   0xfc00000000000000ULL, 0xc00100035b007700ULL, // NAN
   0x7e00000000000000ULL, 0xfe000000d0e0a0d0ULL, // signaling NAN
   // flavors of Infinity
   0x7800000000000000ULL, 0x0000000000000000ULL, // +inf
   0xf800000000000000ULL, 0x0000000000000000ULL, // -inf
   0xf900000000000000ULL, 0x0000000000000000ULL,  // -inf

   0x9999999999999999ULL, 0x9999999999999999ULL  // Eyecatcher.
};
unsigned long nb_dfp128args = 32;


/* ********************************* */
/* helpers to set up loop iterators. */

void debug_show_iter_ranges () {
/* Show the iteration maxes and the increments. */
   if (debug_show_iters)
	printf ("{ a:/%2ld (%ld,+%ld) b:/%ld (%ld,+%ld) c:/%ld (%ld,+%ld) m:/%ld (%ld,+%ld) } \n",
		 a_iters, a_start, a_inc, b_iters, b_start, b_inc,
		 c_iters, c_start, c_inc, m_iters, m_start, m_inc);
}

void set_up_iterators () {
   /* Set the baselines.
      Increments for a, b, m default to 1, c defaults to 2.
      Total number of iterations default to 1. */
   a_inc = 1; b_inc = 1;
   c_inc = 2; m_inc = 1;
   a_iters = 1;  b_iters = 1;
   c_iters = 1;  m_iters = 1;
   /* Now, set the iterator limits as appropriate for the arguments
      that will be used to test the instructions.  */
   if (has_vra || has_xa)
      a_iters = nb_vsxargs;
   if (has_ra)
      a_iters = nb_args;
   if (has_frb || has_frs || has_vrb || has_xb) {
      b_iters = nb_vsxargs;
      if (uses_dfp128_input)
	 b_inc = 2;
   }
   if (has_rb)
      b_iters = nb_args;
   if (has_vrc || has_xc) {
      if (uses_xc_as_blend_mask)
	 c_iters = MASK64SIZE;
      else
         c_iters = nb_vsxargs;
   } else if (has_rc) {
      c_iters = nb_args;
   } else if (has_dcmx)
      // Note: dcmx is hardcoded in tests, otherwise would set to dcmx_iters.
      c_iters = 1;
   else if (has_rs_as_value_source) {
      c_iters = nb_args;
      c_inc = 1;
   }
   if (vrm_override)
      m_iters = 1;
   if (has_vrm )
      m_iters = 4;
   if (is_divide_or_modulo) {
      a_iters = nb_divmod_num_vsxargs+1;
      b_iters = nb_divmod_den_vsxargs;
   }
   if (is_clear_or_insert_insns) {
      a_iters = 4;
      b_iters = 6;
   }
   if (uses_acc_dest) {
	   a_inc+=3; b_inc+=3; c_inc+=3;
   }
   if (uses_pmsk) {
	   a_start=1; b_start=3; c_start=0; m_start=0;
   } else {
	   a_start=0; b_start=0; c_start=0; m_start=0;
   }
   /* Special casing for R==1 tests. */
   if (instruction_is_relative) {
	  a_iters = 1;
	  m_start=3; m_iters=4;
   }
   if ((has_vra+has_vrb+has_vrc+has_vrm+has_xa+has_xb+uses_MC > 2) &&
       (!debug_enable_all_iters)) {
      /* Instruction tests using multiple fields will generate a lot of
	 output. In those cases, arbitrarily increase the increment values
	 to cut the number of iterations.  */
      a_inc+= 5;
      b_inc+= 5;
      c_inc+= 5;
   }
  /* Drop the iterator count if we've specified a limit. */
   a_iters = min (a_iters, a_limit);
   b_iters = min (b_iters, b_limit);
   c_iters = min (c_iters, c_limit);
}

/* This is printed inline, so do not add carriage return, etc. */
void debug_show_current_iteration () {
   if (debug_show_iters)
      printf ("{ %2lx %lx %lx %lx } ", vrai, vrbi, vrci, vrmi);
}

void debug_dump_buffer () {
   if ( (debug_show_raw_values) || (verbose && uses_buffer)) {
      dump_raw_buffer ();
      printf ("\n");
   }
}

void print_result_buffer () {
   if (uses_store_buffer)
      dump_buffer ();
}

/* display the instruction form.  */
void debug_show_form (const char * instruction_name, char * cur_form) {
   if (verbose) {
      printf ("Instruction Name and form: %s ", instruction_name);
      display_form_components (cur_form);
   }
}

/* ***************************** */
/* Build Floating point helpers. */

/* Data Formats for floating point.
 * Floating point values include the following:
 *  -INF -NOR -DEN -0 +0 +DEN +NOR +INF
 *  INFinite: When the biased exponent is the MAX possible value, and
 *   the fraction field is 0.
 *  ZERo.    biased exponent is zero, fraction is 0.
 *  DENormalized.   biased exponent is 0, and fraction is non-zero.
 *  NORmalized. All other values that are neither Zero, Denormalized,
 *   or Infinite.  Biased exponent = 1..MAX-1.
 */

/* Quad (128bit):
 * | Sign | EXPonent+Bias  | FRACTION/Mantissa |
 *  0      1             15 16              127
 *  exponent is 15 bits. ranging from:  0x0000 .. 0x7fff
 *     0 = (zero if fraction == 0, DeNormal if fraction != 0 )
 *     1...0x7ffe = normalized
 *     7fff  =  (infinite if fraction == 0, NaN if fraction != 0)
 */
#define QUAD_EXP_MASK 0x7fff

/* This assumes we are working on the top half of a quad stored in a 64-bit
 *  register.
 */
#define QUAD_EXP_SHIFT 48
#define QUAD_MANTISSA_MASK 0x0000ffffffffffff
unsigned long long build_binary128_float (unsigned long long signbit,
					  unsigned long long exponent,
					  unsigned long long mantissa) {
   unsigned long long thevalue;

   thevalue = (unsigned long long) (signbit << 63) |
      ( (exponent & QUAD_EXP_MASK) << QUAD_EXP_SHIFT) |
      (mantissa & QUAD_MANTISSA_MASK);

   if (debug_show_tables) {
      printf ("%s %llx\n", __FUNCTION__, (unsigned long long)thevalue);
      printf ("SP: ");
      special_print_sp_value (0xffffffff & (thevalue>>48));
      special_print_sp_value (0xffffffff & (thevalue>>32));
      special_print_sp_value (0xffffffff & (thevalue>>16));
      special_print_sp_value (0xffffffff & thevalue);
      // Printing zeros here is unnecessary, but visually helpfull
      // for symmetry and visualization of the quadword.
      special_print_sp_value (0);
      special_print_sp_value (0);
      special_print_sp_value (0);
      special_print_sp_value (0);
      printf ("\n");
      printf ("F: ");
      generic_print_ull_as_float ( 0xffffffff & thevalue>>32);
      generic_print_ull_as_float ( 0xffffffff & thevalue);
      generic_print_ull_as_float ( 0);
      generic_print_ull_as_float ( 0);
      printf ("\n");
      printf ("D: ");
      generic_print_ull_as_double (thevalue);
      generic_print_ull_as_double (0);
      printf ("\n");
   }
   return thevalue;
}

/* A table of exponent values for use in the float precision tests. */
unsigned long exponent_table[] = {
#ifdef EXHAUSTIVE_TESTS
  0x0000,   /* +/-0 or +/-DENormalized, depending on associated mantissa. */
  0x1a,     /* within NORmalized for 16, 32, 64, 128-bit.                 */
  0x1f,     /* +/-INF or +/-NaN for 16bit, NORmalized for 32, 64, 128     */
  0xff,     /* +/-INF or +/-NaN for 32bit, NORmalized for 64, 128         */
  0x7ff,    /* +/-INF or +/-NaN for 32 and 64bit, NORmalized for 128      */
  0x7fff,   /* +/-INF or +/-NaN for 128bit.                               */
#else
  0x0000,   /* +/-0 or +/-DENormalized, depending on associated mantissa. */
  0xff,     /* +/-INF or +/-NaN for 32bit, NORmalized for 64, 128         */
  0x7ff,    /* +/-INF or +/-NaN for 32 and 64bit, NORmalized for 128      */
  0x7fff,   /* +/-INF or +/-NaN for 128bit.                               */
#endif
};
#define MAX_EXPONENTS  (sizeof (exponent_table) / sizeof (unsigned long))

unsigned long mantissa_table[] = {
#ifdef EXHAUSTIVE_TESTS
  0xbeefbeefbeef, /* NOR or DEN or NaN */
  0x000000000000, /* ZERO or INF */
  0x7fffffffffff, /* NOR or DEN or NaN */
#else
  0x000000000000, /* ZERO or INF */
  0x7fffffffffff, /* NOR or DEN or NaN */
#endif
};
#define MAX_MANTISSAS (sizeof (mantissa_table) / sizeof (unsigned long))

/* build in 64-bit chunks, low doubleword is zero. */
unsigned long * binary128_float_vsxargs = NULL;
unsigned long nb_float_vsxargs;
#define MAX_FLOAT_VSX_ARRAY_SIZE ( ( (MAX_EXPONENTS * MAX_MANTISSAS) * 2 + 1) * 2)

void dump_float_vsx_tables (void) {
   /* quad */
   printf ("Quad (binary128_float_vsxargs):\n");
   for (int i = 0 ; i < nb_float_vsxargs; i+= 2 ) {
      printf ("%2d:", i);
      printf ("%016lx%016lx \n", binary128_float_vsxargs[i], binary128_float_vsxargs[i+1]);
   }
   printf ("\n");
}

void build_float_vsx_tables () {
   long i = 0;
   unsigned long signbit;
   unsigned long exponent;
   unsigned long mantissa;/* also referred to as FRACTION in the ISA.*/
   unsigned long exponent_index;
   unsigned long mantissa_index;

   if (debug_show_tables) printf ("%s\n", __FUNCTION__);
   binary128_float_vsxargs = malloc (MAX_FLOAT_VSX_ARRAY_SIZE
                                    * sizeof (unsigned long));
   for (signbit = 0; signbit < 2; signbit++) {
      for (exponent_index = 0; exponent_index < MAX_EXPONENTS;
           exponent_index++) {
         for (mantissa_index = 0; mantissa_index < MAX_MANTISSAS;
              mantissa_index++) {
            exponent = exponent_table[exponent_index];
            mantissa = mantissa_table[mantissa_index];
         if (debug_show_tables) {
            printf ("signbit:%lx ", signbit);
            printf ("exponent:%4lx ", exponent);
            printf ("mantissa:%lx ", mantissa);
            printf ("\n");
         }

	 binary128_float_vsxargs[i] = build_binary128_float (signbit, exponent,
                                                            mantissa);
	 // for simplicity, leave the lower half of the 128-bit value as zero.
         binary128_float_vsxargs[i+1] = 0;
         i += 2;
         }
      }
   }
   nb_float_vsxargs = i;
   if (verbose>1)
	   printf ("Registered %ld float_vsxargs\n", nb_float_vsxargs);
}

/* **************************************** */
/* Source/destination register initializers */

void init_xtp() {
     XTp0[0] = DEADBEEF; //vsxargs[vrai+4];
     XTp0[1] = DEADBEEF; //vsxargs[vrai+3];
     XTp1[0] = DEADBEEF; //vsxargs[vrai+2];
     XTp1[1] = DEADBEEF; //vsxargs[vrai+1];
}

void init_xsp() {
     XTp0[0] = vsxargs[vrai+4];
     XTp0[1] = vsxargs[vrai+3];
     XTp1[0] = vsxargs[vrai+2];
     XTp1[1] = vsxargs[vrai+1];
}

void init_source_acc() {
      /* initialize the ACC with data */
      TEST_ACC0[0] = vsxargs[ (vrai  ) % nb_vsxargs];
      TEST_ACC0[1] = vsxargs[ (vrai+1) % nb_vsxargs];
      TEST_ACC1[0] = vsxargs[ (vrai+2) % nb_vsxargs];
      TEST_ACC1[1] = vsxargs[ (vrai+3) % nb_vsxargs];
      TEST_ACC2[0] = vsxargs[ (vrai+4) % nb_vsxargs];
      TEST_ACC2[1] = vsxargs[ (vrai+5) % nb_vsxargs];
      TEST_ACC3[0] = vsxargs[ (vrai+6) % nb_vsxargs];
      TEST_ACC3[1] = vsxargs[ (vrai+7) % nb_vsxargs];
      push_vsrs_to_acc ();
}

void init_acc_deadbeef() {
      // Initialize the associated VSRs to 'DEADBEEF', then call
      // xxmtacc to do the actual set.
      TEST_ACC0[0] = DEADBEEF;  TEST_ACC0[1] = DEADBEEF;
      TEST_ACC1[0] = DEADBEEF;  TEST_ACC1[1] = DEADBEEF;
      TEST_ACC2[0] = DEADBEEF;  TEST_ACC2[1] = DEADBEEF;
      TEST_ACC3[0] = DEADBEEF;  TEST_ACC3[1] = DEADBEEF;
      push_vsrs_to_acc ();
}

   /* initialize the VSRs that will be used by the accumulator related tests. */
void init_acc_vsrs() {
      TEST_ACC0[0] = vsxargs[vrai]  ;
      TEST_ACC0[1] = vsxargs[vrai+1];
      TEST_ACC1[0] = vsxargs[vrai+2];
      TEST_ACC1[1] = vsxargs[vrai+3];
      TEST_ACC2[0] = vsxargs[vrai+4];
      TEST_ACC2[1] = vsxargs[vrai+5];
      TEST_ACC3[0] = vsxargs[vrai+6];
      TEST_ACC3[1] = vsxargs[vrai+7];
}

void initialize_target_registers () {
   vrt[0] = DEADBEEF;
   vrt[1] = DEADBEEF;
   vec_xt[0] = vec_xt[1] = DEADBEEF;
   rt = DEADBEEF;
   frt = 0.0;
   frtp = 0.0;
   // xs/xt register pairs.
   if (has_xtp) {
	   if (has_xsp) printf("Warning.  uses xsp and xtp\n");
	   init_xtp();
   }
   if (uses_acc_dest) {
	   init_acc_deadbeef();
   }
}

float float_as_hex (unsigned long long hexval) {
   union rosetta_t stone;
   stone.ull = hexval;
   return stone.flt;
}

double double_as_hex (unsigned long long hexval) {
   union rosetta_t stone;
   stone.ull = hexval;
   return stone.dbl;
}

void initialize_source_registers () {
   SET_CR_ZERO;
   current_cr = 0;
   current_fpscr = 0;
   SET_FPSCR_ZERO;
   current_fpscr = 0;
   int isr_modulo;
  /* Special handing for input values.. ensure if we are 
     dividing or doing modulo operations that we do not
     attempt dividing by zero.  */
   if (is_divide_or_modulo)
      isr_modulo = nb_divmod_num_vsxargs;
   else
      isr_modulo = nb_vsxargs;

   if (has_xa) {
	   vec_xa[0] = vsxargs[ (vrai  ) % isr_modulo];
	   vec_xa[1] = vsxargs[ (vrai+1) % isr_modulo];
   }
   if (has_xb) {
	   vec_xb[0] = vsxargs[ (vrbi  ) % isr_modulo];
	   vec_xb[1] = vsxargs[ (vrbi+1) % isr_modulo];
   }
   if (has_vra) {
	  vra[0] = vsxargs[ (vrai  ) % isr_modulo];
	  vra[1] = vsxargs[ (vrai+1) % isr_modulo];
   }
   if (has_vrb) {
	  vrb[0] = vsxargs[ (vrbi  ) % isr_modulo];
	  vrb[1] = vsxargs[ (vrbi+1) % isr_modulo];
   }

   if (instruction_is_relative) {
     /* for pstxsd and friends using R=1 */
     vec_xa[0] = vsxargs[ (vrai+2  ) % isr_modulo];
     vec_xa[1] = vsxargs[ (vrai+3  ) % isr_modulo];
   }

   // xap 'shares' with the second half of an xa-pair.
  if (has_xap ) {
    vec_xc[0] = vsxargs[ (vrci+2) % isr_modulo];
    vec_xc[1] = vsxargs[ (vrci+3) % isr_modulo];
  }
  // Combine with the above has_xap clause ? May need addiitonal
  // logic later if these ever overlap.
  if (has_xc) {
    vec_xc[0] = vsxargs[ (vrai  ) % isr_modulo];
    vec_xc[1] = vsxargs[ (vrai+1) % isr_modulo];
  }
   if (has_vrc) {
      vrc[0] = vsxargs[ (vrci  ) % nb_vsxargs];
      vrc[1] = vsxargs[ (vrci+1) % nb_vsxargs];
   }
   if (is_testlsb) {
     /* Special casing for this test to force the vec_xb low bits
	 to zero or one. */
	 if (vrbi%3 == 0) {
	    // force bits to zero.
	    vec_xb[0] = vec_xb[0]&0xfefefefefefefefeUL;
	    vec_xb[1] = vec_xb[1]&0xfefefefefefefefeUL;
	 }
	 if (vrbi%3 == 1) {
	    // force bits to one.
	    vec_xb[0] = vec_xb[0]|0x0101010101010101UL;
	    vec_xb[1] = vec_xb[1]|0x0101010101010101UL;
      }
    }

   if (uses_xc_as_blend_mask) {
      vec_xc[0] = mask64[ (vrci  )%MASK64SIZE];
      vec_xc[1] = mask64[ (vrci+1)%MASK64SIZE];
   }

   if (uses_dfp128_input) {
      frsb = double_as_hex (dfp128_vals[ (vrbi  ) % nb_dfp128args]);
      frsbp = double_as_hex (dfp128_vals[ (vrbi+1) % nb_dfp128args]);
   } else {
      frsb = vsxargs[ (vrbi  )%nb_vsxargs];
      frsbp = vsxargs[ (vrbi+1)%nb_vsxargs];
   }

   /* default initializations.. */
   ra = args[vrai];
   rb = args[vrbi % nb_args ];
   rc = 2 * vrci;
   rs = args[vrai % nb_args ];
   rsp = args[ (vrai+1) % nb_args ];

   /* more special cases.. */
   if (is_clear_or_insert_insns) {
      if (has_rb)  rb = 2*vrbi;
      /* note special case for is_insert_double, see set_up_iterators () */
      if (has_ra)  ra = 4*vrai;
      if (is_insert_double) {
	 /* For an insert_double, the results are undefined
	    for ra > 8, so modulo those into a valid range.
	    Since ra is defined as a hard register, and due to gcc
	    issue (PR101882) where a modulo operation fails with
	    both input and output regs set to a hard register, this
	    assignment references the args[] array again, versus
	    ra = ra % 9;.  */
	 ra = args[vrai] % 9;
      }
   }

   if (uses_buffer) {
      if (has_rb) {
	 ra = 8*vrai;
	 rb = (unsigned long) &buffer;
	 b_iters = 1;
      } else if (has_ra) {
	 ra = (unsigned long ) &buffer;
	 a_iters = 1;
	 if (has_frs || has_rsp) {
	    b_iters = 2;
	 }
      }
      initialize_buffer (0);
   }
   if (is_mtvsr_insn && has_rb) {
      rb = mask64[vrbi%MASK64SIZE];
      b_iters = MASK64SIZE;
   }
   if (has_rs_as_value_source) {
      rs = args[vrci];
   }

   vrm[0] = vrm_mask[ vrmi % VRMMASK_SIZE ];
   vrm[1] = vrm_mask[ (vrmi+1) % VRMMASK_SIZE ];

   dcmx = 1 << vrci;

   if (uses_acc_src) {
	   init_source_acc();
   }
   if (uses_acc_vsrs) {
	   init_acc_vsrs();
   }
   if (has_xs) {
	   init_xsp();
// vec_xs is not directly shared with the register defined XSp/XTp, so
// explicitly assign the values when needed.
	   vec_xs[0] = XTp0[0];
	   vec_xs[1] = XTp0[1];
	   vec_xs[0] = XTp1[0];
	   vec_xs[1] = XTp1[1];
   }
}

unsigned long long vsrd;
unsigned long get_vsrhd_vs26 () {
    __asm__ __volatile__ ("mfvsrd %0, 26":"=r" (vsrd)); return vsrd; }
unsigned long get_vsrhd_vs27 () {
    __asm__ __volatile__ ("mfvsrd %0, 27":"=r" (vsrd)); return vsrd; }
unsigned long get_vsrhd_vs28 () {
    __asm__ __volatile__ ("mfvsrd %0, 28":"=r" (vsrd)); return vsrd; }
unsigned long get_vsrhd_vs29 () {
    __asm__ __volatile__ ("mfvsrd %0, 29":"=r" (vsrd)); return vsrd; }

#endif
