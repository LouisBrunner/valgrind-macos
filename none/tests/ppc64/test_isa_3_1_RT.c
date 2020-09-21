/*
 * Valgrind testcase for PowerPC ISA 3.1
 *
 * Copyright (C) 2019-2020 Will Schmidt <will_schmidt@vnet.ibm.com>
 *
 * 64bit build:
 *    gcc -Winline -Wall -g -O -mregnames -maltivec -m64
 */

/*
 *   This program is free software; you can redistribute it and/or
 *   modify it under the terms of the GNU General Public License as
 *   published by the Free Software Foundation; either version 2 of the
 *   License, or (at your option) any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program; if not, write to the Free Software
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#include <stdio.h>
#include <stdint.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <altivec.h>
#include <malloc.h>

#include <string.h>
#include <signal.h>
#include <setjmp.h>

/* Condition Register fields.
   These are used to capture the condition register values immediately after
   the instruction under test is executed. This is done to help prevent other
   test overhead (switch statements, result compares, etc) from disturbing
   the test case results.  */
unsigned long current_cr;
unsigned long current_fpscr;


struct test_list_t current_test;

#include "isa_3_1_helpers.h"

#ifdef HAS_ISA_3_1

static void test_plbz_off0 (void) {
  __asm__ __volatile__ ("plbz %0, 0(%1), 0" : "=r" (rt) : "r" (ra) );
}
static void test_plbz_off8 (void) {
  __asm__ __volatile__ ("plbz %0, 8(%1), 0" : "=r" (rt) : "r" (ra) );
}
static void test_plbz_off16 (void) {
  __asm__ __volatile__ ("plbz %0, 16(%1), 0" : "=r" (rt) : "r" (ra) );
}
static void test_plbz_off32 (void) {
  __asm__ __volatile__ ("plbz %0, 32(%1), 0" : "=r" (rt) : "r" (ra) );
}
static void test_plbz_off64 (void) {
  __asm__ __volatile__ ("plbz %0, 64(%1), 0" : "=r" (rt) : "r" (ra) );
}
static void test_plhz_off0 (void) {
  __asm__ __volatile__ ("plhz %0, 0(%1), 0" : "=r" (rt) : "r" (ra) );
}
static void test_plhz_off8 (void) {
  __asm__ __volatile__ ("plhz %0, 8(%1), 0" : "=r" (rt) : "r" (ra) );
}
static void test_plhz_off16 (void) {
  __asm__ __volatile__ ("plhz %0, 16(%1), 0" : "=r" (rt) : "r" (ra) );
}
static void test_plhz_off32 (void) {
  __asm__ __volatile__ ("plhz %0, 32(%1), 0" : "=r" (rt) : "r" (ra) );
}
static void test_plhz_off64 (void) {
  __asm__ __volatile__ ("plhz %0, 64(%1), 0" : "=r" (rt) : "r" (ra) );
}
static void test_plha_off0 (void) {
  __asm__ __volatile__ ("plha %0, 0(%1), 0" : "=r" (rt) : "r" (ra) );
}
static void test_plha_off8 (void) {
  __asm__ __volatile__ ("plha %0, 8(%1), 0" : "=r" (rt) : "r" (ra) );
}
static void test_plha_off16 (void) {
  __asm__ __volatile__ ("plha %0, 16(%1), 0" : "=r" (rt) : "r" (ra) );
}
static void test_plha_off32 (void) {
  __asm__ __volatile__ ("plha %0, 32(%1), 0" : "=r" (rt) : "r" (ra) );
}
static void test_plha_off64 (void) {
  __asm__ __volatile__ ("plha %0, 64(%1), 0" : "=r" (rt) : "r" (ra) );
}
static void test_plwz_off0 (void) {
  __asm__ __volatile__ ("plwz %0, 0(%1), 0" : "=r" (rt) : "r" (ra) );
}
static void test_plwz_off8 (void) {
  __asm__ __volatile__ ("plwz %0, 8(%1), 0" : "=r" (rt) : "r" (ra) );
}
static void test_plwz_off16 (void) {
  __asm__ __volatile__ ("plwz %0, 16(%1), 0" : "=r" (rt) : "r" (ra) );
}
static void test_plwz_off32 (void) {
  __asm__ __volatile__ ("plwz %0, 32(%1), 0" : "=r" (rt) : "r" (ra) );
}
static void test_plwz_off64 (void) {
  __asm__ __volatile__ ("plwz %0, 64(%1), 0" : "=r" (rt) : "r" (ra) );
}
static void test_plwa_off0 (void) {
  __asm__ __volatile__ ("plwa %0, 0(%1), 0" : "=r" (rt) : "r" (ra) );
}
static void test_plwa_off8 (void) {
  __asm__ __volatile__ ("plwa %0, 8(%1), 0" : "=r" (rt) : "r" (ra) );
}
static void test_plwa_off16 (void) {
  __asm__ __volatile__ ("plwa %0, 16(%1), 0" : "=r" (rt) : "r" (ra) );
}
static void test_plwa_off32 (void) {
  __asm__ __volatile__ ("plwa %0, 32(%1), 0" : "=r" (rt) : "r" (ra) );
}
static void test_plwa_off64 (void) {
  __asm__ __volatile__ ("plwa %0, 64(%1), 0" : "=r" (rt) : "r" (ra) );
}
static void test_pld_off0 (void) {
  __asm__ __volatile__ ("pld %0, 0(%1), 0" : "=r" (rt) : "r" (ra) );
}
static void test_pld_off8 (void) {
  __asm__ __volatile__ ("pld %0, 8(%1), 0" : "=r" (rt) : "r" (ra) );
}
static void test_pld_off16 (void) {
  __asm__ __volatile__ ("pld %0, 16(%1), 0" : "=r" (rt) : "r" (ra) );
}
static void test_pld_off32 (void) {
  __asm__ __volatile__ ("pld %0, 32(%1), 0" : "=r" (rt) : "r" (ra) );
}
static void test_pld_off64 (void) {
  __asm__ __volatile__ ("pld %0, 64(%1), 0" : "=r" (rt) : "r" (ra) );
}
static void test_pstb_off0 (void) {
  __asm__ __volatile__ ("pstb %0, 0(%1), 0" :: "r" (rs), "r" (ra) );
}
static void test_pstb_off8 (void) {
  __asm__ __volatile__ ("pstb %0, 8(%1), 0" :: "r" (rs), "r" (ra) );
}
static void test_pstb_off16 (void) {
  __asm__ __volatile__ ("pstb %0, 16(%1), 0" :: "r" (rs), "r" (ra) );
}
static void test_pstb_off32 (void) {
  __asm__ __volatile__ ("pstb %0, 32(%1), 0" :: "r" (rs), "r" (ra) );
}
static void test_psth_off0 (void) {
  __asm__ __volatile__ ("psth %0, 0(%1), 0" :: "r" (rs), "r" (ra) );
}
static void test_psth_off8 (void) {
  __asm__ __volatile__ ("psth %0, 8(%1), 0" :: "r" (rs), "r" (ra) );
}
static void test_psth_off16 (void) {
  __asm__ __volatile__ ("psth %0, 16(%1), 0" :: "r" (rs), "r" (ra) );
}
static void test_psth_off32 (void) {
  __asm__ __volatile__ ("psth %0, 32(%1), 0" :: "r" (rs), "r" (ra) );
}
static void test_pstw_off0 (void) {
  __asm__ __volatile__ ("pstw %0, 0(%1), 0" :: "r" (rs), "r" (ra) );
}
static void test_pstw_off8 (void) {
  __asm__ __volatile__ ("pstw %0, 8(%1), 0" :: "r" (rs), "r" (ra) );
}
static void test_pstw_off16 (void) {
  __asm__ __volatile__ ("pstw %0, 16(%1), 0" :: "r" (rs), "r" (ra) );
}
static void test_pstw_off32 (void) {
  __asm__ __volatile__ ("pstw %0, 32(%1), 0" :: "r" (rs), "r" (ra) );
}
static void test_pstd_off0 (void) {
  __asm__ __volatile__ ("pstd %0, 0(%1), 0" :: "r" (rs), "r" (ra) );
}
static void test_pstd_off8 (void) {
  __asm__ __volatile__ ("pstd %0, 8(%1), 0" :: "r" (rs), "r" (ra) );
}
static void test_pstd_off16 (void) {
  __asm__ __volatile__ ("pstd %0, 16(%1), 0" :: "r" (rs), "r" (ra) );
}
static void test_pstd_off32 (void) {
  __asm__ __volatile__ ("pstd %0, 32(%1), 0" :: "r" (rs), "r" (ra) );
}
static void test_paddi_0 (void) {
  __asm__ __volatile__ ("paddi %0, %1, 0, 0" : "=r" (rt) : "r" (ra) );
}
static void test_paddi_12 (void) {
  __asm__ __volatile__ ("paddi %0, %1, 12, 0" : "=r" (rt) : "r" (ra) );
}
static void test_paddi_48 (void) {
  __asm__ __volatile__ ("paddi %0, %1, 48, 0" : "=r" (rt) : "r" (ra) );
}
static void test_paddi_98 (void) {
  __asm__ __volatile__ ("paddi %0, %1, 98, 0" : "=r" (rt) : "r" (ra) );
}
static void test_plq_off0 (void) {
  __asm__ __volatile__ ("plq 26, 0(%0), 0" :: "r" (ra) );
}
static void test_plq_off8 (void) {
  __asm__ __volatile__ ("plq 26, 8(%0), 0" :: "r" (ra) );
}
static void test_plq_off16 (void) {
  __asm__ __volatile__ ("plq 26, 16(%0), 0" :: "r" (ra) );
}
static void test_plq_off32 (void) {
  __asm__ __volatile__ ("plq 26, 32(%0), 0" :: "r" (ra) );
}
static void test_plq_off48 (void) {
  __asm__ __volatile__ ("plq 26, 48(%0), 0" :: "r" (ra) );
}
static void test_plq_off64 (void) {
  __asm__ __volatile__ ("plq 26, 64(%0), 0" :: "r" (ra) );
}
static void test_pstq_off0 (void) {
  __asm__ __volatile__ ("pstq 24, 0(%0), 0" :: "r" (ra) );
}
static void test_pstq_off8 (void) {
  __asm__ __volatile__ ("pstq 24, 8(%0), 0" :: "r" (ra) );
}
static void test_pstq_off16 (void) {
  __asm__ __volatile__ ("pstq 24, 16(%0), 0" :: "r" (ra) );
}
static void test_pstq_off32 (void) {
  __asm__ __volatile__ ("pstq 24, 32(%0), 0" :: "r" (ra) );
}
static void test_pstq_off64 (void) {
  __asm__ __volatile__ ("pstq 24, 64(%0), 0" :: "r" (ra) );
}

static test_list_t testgroup_generic[] = {
  { &test_paddi_0, "paddi 0", "RT,RA,SI,R"}, /* bcwp */
  { &test_paddi_12, "paddi 12", "RT,RA,SI,R"}, /* bcwp */
  { &test_paddi_48, "paddi 48", "RT,RA,SI,R"}, /* bcwp */
  { &test_paddi_98, "paddi 98", "RT,RA,SI,R"}, /* bcwp */
  { &test_plbz_off0, "plbz off0", "RT,D(RA),R"}, /* bcwp */
  { &test_plbz_off8, "plbz off8", "RT,D(RA),R"}, /* bcwp */
  { &test_plbz_off16, "plbz off16", "RT,D(RA),R"}, /* bcwp */
  { &test_plbz_off32, "plbz off32", "RT,D(RA),R"}, /* bcwp */
  { &test_plbz_off64, "plbz off64", "RT,D(RA),R"}, /* bcwp */
  { &test_pld_off0, "pld off0", "RT,D(RA),R"}, /* bcwp */
  { &test_pld_off8, "pld off8", "RT,D(RA),R"}, /* bcwp */
  { &test_pld_off16, "pld off16", "RT,D(RA),R"}, /* bcwp */
  { &test_pld_off32, "pld off32", "RT,D(RA),R"}, /* bcwp */
  { &test_pld_off64, "pld off64", "RT,D(RA),R"}, /* bcwp */
  { &test_plha_off0, "plha off0", "RT,D(RA),R"}, /* bcwp */
  { &test_plha_off8, "plha off8", "RT,D(RA),R"}, /* bcwp */
  { &test_plha_off16, "plha off16", "RT,D(RA),R"}, /* bcwp */
  { &test_plha_off32, "plha off32", "RT,D(RA),R"}, /* bcwp */
  { &test_plha_off64, "plha off64", "RT,D(RA),R"}, /* bcwp */
  { &test_plhz_off0, "plhz off0", "RT,D(RA),R"}, /* bcwp */
  { &test_plhz_off8, "plhz off8", "RT,D(RA),R"}, /* bcwp */
  { &test_plhz_off16, "plhz off16", "RT,D(RA),R"}, /* bcwp */
  { &test_plhz_off32, "plhz off32", "RT,D(RA),R"}, /* bcwp */
  { &test_plhz_off64, "plhz off64", "RT,D(RA),R"}, /* bcwp */
  { &test_plq_off0, "plq off0", "RTp,D(RA),R"}, /* bcwp */
  { &test_plq_off8, "plq off8", "RTp,D(RA),R"}, /* bcwp */
  { &test_plq_off16, "plq off16", "RTp,D(RA),R"}, /* bcwp */
  { &test_plq_off32, "plq off32", "RTp,D(RA),R"}, /* bcwp */
  { &test_plq_off48, "plq off48", "RTp,D(RA),R"}, /* bcwp */
  { &test_plq_off64, "plq off64", "RTp,D(RA),R"}, /* bcwp */
  { &test_plwa_off0, "plwa off0", "RT,D(RA),R"}, /* bcwp */
  { &test_plwa_off8, "plwa off8", "RT,D(RA),R"}, /* bcwp */
  { &test_plwa_off16, "plwa off16", "RT,D(RA),R"}, /* bcwp */
  { &test_plwa_off32, "plwa off32", "RT,D(RA),R"}, /* bcwp */
  { &test_plwa_off64, "plwa off64", "RT,D(RA),R"}, /* bcwp */
  { &test_plwz_off0, "plwz off0", "RT,D(RA),R"}, /* bcwp */
  { &test_plwz_off8, "plwz off8", "RT,D(RA),R"}, /* bcwp */
  { &test_plwz_off16, "plwz off16", "RT,D(RA),R"}, /* bcwp */
  { &test_plwz_off32, "plwz off32", "RT,D(RA),R"}, /* bcwp */
  { &test_plwz_off64, "plwz off64", "RT,D(RA),R"}, /* bcwp */
  { &test_pstb_off0, "pstb off0", "RS,D(RA),R"}, /* bcwp */
  { &test_pstb_off8, "pstb off8", "RS,D(RA),R"}, /* bcwp */
  { &test_pstb_off16, "pstb off16", "RS,D(RA),R"}, /* bcwp */
  { &test_pstb_off32, "pstb off32", "RS,D(RA),R"}, /* bcwp */
  { &test_pstd_off0, "pstd off0", "RS,D(RA),R"}, /* bcwp */
  { &test_pstd_off8, "pstd off8", "RS,D(RA),R"}, /* bcwp */
  { &test_pstd_off16, "pstd off16", "RS,D(RA),R"}, /* bcwp */
  { &test_pstd_off32, "pstd off32", "RS,D(RA),R"}, /* bcwp */
  { &test_psth_off0, "psth off0", "RS,D(RA),R"}, /* bcwp */
  { &test_psth_off8, "psth off8", "RS,D(RA),R"}, /* bcwp */
  { &test_psth_off16, "psth off16", "RS,D(RA),R"}, /* bcwp */
  { &test_psth_off32, "psth off32", "RS,D(RA),R"}, /* bcwp */
  { &test_pstq_off0, "pstq off0", "RSp,D(RA),R"}, /* bcwp */
  { &test_pstq_off8, "pstq off8", "RSp,D(RA),R"}, /* bcwp */
  { &test_pstq_off16, "pstq off16", "RSp,D(RA),R"}, /* bcwp */
  { &test_pstq_off32, "pstq off32", "RSp,D(RA),R"}, /* bcwp */
  { &test_pstq_off64, "pstq off64", "RSp,D(RA),R"}, /* bcwp */
  { &test_pstw_off0, "pstw off0", "RS,D(RA),R"}, /* bcwp */
  { &test_pstw_off8, "pstw off8", "RS,D(RA),R"}, /* bcwp */
  { &test_pstw_off16, "pstw off16", "RS,D(RA),R"}, /* bcwp */
  { &test_pstw_off32, "pstw off32", "RS,D(RA),R"}, /* bcwp */
	{ NULL, 	    NULL },
};

/*  Allow skipping of tests. */
unsigned long test_count=0xffff;
unsigned long skip_count=0;
unsigned long setup_only=0;

/*  Set up a setjmp/longjmp to gently handle our SIGILLs and SIGSEGVs.  */
static jmp_buf mybuf;

/* This (testfunction_generic) is meant to handle all of the instruction
   variations.  The helpers set up the register and iterator values
   as is appropriate for the instruction being tested.  */
static void testfunction_generic (const char* instruction_name,
				  test_func_t test_function,
				  unsigned int ignore_flags,
				  char * cur_form) {

   identify_form_components (instruction_name , cur_form);
   debug_show_form (instruction_name, cur_form);
   set_up_iterators ();
   debug_show_iter_ranges ();
   initialize_buffer (0);
   debug_dump_buffer ();

   for (vrai = 0; vrai < a_iters ; vrai+=a_inc) {
      for (vrbi = 0; vrbi < b_iters ; vrbi+=b_inc) {
	 for (vrci = 0; vrci < c_iters ; vrci+=c_inc) {
	    for (vrmi = 0; (vrmi < m_iters) ; vrmi+=m_inc) {
		CHECK_OVERRIDES
		debug_show_current_iteration ();
		// Be sure to initialize the target registers first.
		initialize_target_registers ();
		initialize_source_registers ();
		printf ("%s", instruction_name);
		print_register_header ();
		printf( " =>"); fflush (stdout);
		if (!setup_only) {
		  if (enable_setjmp) {
		   if ( setjmp ( mybuf ) ) {
		     printf("signal tripped. (FIXME)\n");
		     continue;
		   }
		  }
		  (*test_function) ();
		}
		print_register_footer ();
		print_result_buffer ();
		printf ("\n");
	    }
	 }
      }
   }
}

void mykillhandler ( int x ) { longjmp (mybuf, 1); }
void mysegvhandler ( int x ) { longjmp (mybuf, 1); }

static void do_tests ( void )
{
   int groupcount;
   char * cur_form;
   test_group_t group_function = &testfunction_generic;
   test_list_t *tests = testgroup_generic;

   struct sigaction kill_action, segv_action;
   struct sigaction old_kill_action, old_segv_action;
   if (enable_setjmp) {
      kill_action.sa_handler = mykillhandler;
      segv_action.sa_handler = mysegvhandler;
      sigemptyset ( &kill_action.sa_mask );
      sigemptyset ( &segv_action.sa_mask );
      kill_action.sa_flags = SA_NODEFER;
      segv_action.sa_flags = SA_NODEFER;
      sigaction ( SIGILL, &kill_action, &old_kill_action);
      sigaction ( SIGSEGV, &segv_action, &old_segv_action);
   }

   for (groupcount = 0; tests[groupcount].name != NULL; groupcount++) {
	cur_form = strdup(tests[groupcount].form);
	current_test = tests[groupcount];
	if (groupcount < skip_count) continue;
	if (verbose) printf("Test #%d ,", groupcount);
	if (verbose > 1) printf(" instruction %s (v=%d)", current_test.name, verbose);
	(*group_function) (current_test.name, current_test.func, 0, cur_form );
	printf ("\n");
	if (groupcount >= (skip_count+test_count)) break;
   }
   if (debug_show_labels) printf("\n");
   printf ("All done. Tested %d different instruction groups\n", groupcount);
}

static void usage (void)
{
   fprintf(stderr,
      "Usage: test_isa_XXX [OPTIONS]\n"
      "\t-h: display this help and exit\n"
      "\t-v: increase verbosity\n"
      "\t-a <foo> : limit number of a-iterations to <foo>\n"
      "\t-b <foo> : limit number of b-iterations to <foo>\n"
      "\t-c <foo> : limit number of c-iterations to <foo>\n"
      "\t-n <foo> : limit to this number of tests.\n"
      "\t-r <foo>: run only test # <foo> \n"
      "\t\n"
      "\t-j :enable setjmp to recover from illegal insns. \n"
      "\t-m :(dev only?) lock VRM value to zero.\n"
      "\t-z :(dev only?) lock MC value to zero.\n"
      "\t-p :(dev only?) disable prefix instructions\n"
      "\t-s <foo>: skip <foo> tests \n"
      "\t-c <foo>: stop after running <foo> # of tests \n"
      "\t-f : Do the test setup but do not actually execute the test instruction. \n"
   );
}

int main (int argc, char **argv)
{
   int c;
   while ((c = getopt(argc, argv, "dhjvmpfzs:a:b:c:n:r:")) != -1) {
      switch (c) {
	 case 'h':
	    usage();
	    return 0;

	 case 'v':
	    verbose++;
	    break;

	 /* Options related to limiting the test iterations.  */
	 case 'a':
	    a_limit=atoi (optarg);
	    printf ("limiting a-iters to %ld.\n", a_limit);
	    break;
	 case 'b':
	    b_limit=atoi (optarg);
	    printf ("limiting b-iters to %ld.\n", b_limit);
	    break;
	 case 'c':
	    c_limit=atoi (optarg);
	    printf ("limiting c-iters to %ld.\n", c_limit);
	    break;
	 case 'n': // run this number of tests.
	    test_count=atoi (optarg);
	    printf ("limiting to %ld tests\n", test_count);
	    break;
	 case 'r': // run just test #<foo>.
	    skip_count=atoi (optarg);
	    test_count=0;
	    if (verbose) printf("Running only test number %ld\n", skip_count);
	    break;
	 case 's': // skip this number of tests.
	    skip_count=atoi (optarg);
	    printf ("skipping %ld tests\n", skip_count);
	    break;

	 /* debug options.  */
	 case 'd':
	    dump_tables=1;
	    printf("DEBUG:dump_tables.\n");
	    break;
	 case 'f':
	    setup_only=1;
	    printf("DEBUG:setup_only.\n");
	    break;
	 case 'j':
	    enable_setjmp=1;
	    printf ("DEBUG:setjmp enabled.\n");
	    break;
	 case 'm':
	    vrm_override=1;
	    printf ("DEBUG:vrm override enabled.\n");
	    break;
	 case 'p':
	    prefix_override=1;
	    printf ("DEBUG:prefix override enabled.\n");
	    break;
	 case 'z':
	    mc_override=1;
	    printf ("DEBUG:MC override enabled.\n");
	    break;
	 default:
	    usage();
	    fprintf(stderr, "Unknown argument: '%c'\n", c);
	   }
	}

	generic_prologue ();
	build_vsx_table ();
	build_args_table ();
	build_float_vsx_tables ();

	if (dump_tables) {
	   dump_float_vsx_tables ();
	   dump_vsxargs ();
	}

	do_tests ();

	return 0;
}

#else	   // HAS_ISA_3_1
int main (int argc, char **argv)
{
   printf("NO ISA 3.1 SUPPORT\n");
   return 0;
}
#endif
