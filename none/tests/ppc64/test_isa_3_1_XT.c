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
#ifdef HAS_ISA_3_1
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

static void test_plfd_64 (void) {
  __asm__ __volatile__ ("plfd 28, 64(%0), 0" :: "r" (ra) );
}
static void test_plfd_32 (void) {
  __asm__ __volatile__ ("plfd 28, 32(%0), 0" :: "r" (ra) );
}
static void test_plfd_16 (void) {
  __asm__ __volatile__ ("plfd 28, 16(%0), 0" :: "r" (ra) );
}
static void test_plfd_8 (void) {
  __asm__ __volatile__ ("plfd 28, 8(%0), 0" :: "r" (ra) );
}
static void test_plfd_4 (void) {
  __asm__ __volatile__ ("plfd 28, 4(%0), 0" :: "r" (ra) );
}
static void test_plfd_0 (void) {
  __asm__ __volatile__ ("plfd 28, 0(%0), 0" :: "r" (ra) );
}
static void test_plfs_64 (void) {
  __asm__ __volatile__ ("plfs 28, 64(%0), 0" :: "r" (ra) );
}
static void test_plfs_32 (void) {
  __asm__ __volatile__ ("plfs 28, 32(%0), 0" :: "r" (ra) );
}
static void test_plfs_16 (void) {
  __asm__ __volatile__ ("plfs 28, 16(%0), 0" :: "r" (ra) );
}
static void test_plfs_8 (void) {
  __asm__ __volatile__ ("plfs 28, 8(%0), 0" :: "r" (ra) );
}
static void test_plfs_4 (void) {
  __asm__ __volatile__ ("plfs 28, 4(%0), 0" :: "r" (ra) );
}
static void test_plfs_0 (void) {
  __asm__ __volatile__ ("plfs 28, 0(%0), 0" :: "r" (ra) );
}
static void test_pstfd_32 (void) {
  __asm__ __volatile__ ("pstfd 26, 32(%0), 0" :: "r" (ra) );
}
static void test_pstfd_16 (void) {
  __asm__ __volatile__ ("pstfd 26, 16(%0), 0" :: "r" (ra) );
}
static void test_pstfd_8 (void) {
  __asm__ __volatile__ ("pstfd 26, 8(%0), 0" :: "r" (ra) );
}
static void test_pstfd_4 (void) {
  __asm__ __volatile__ ("pstfd 26, 4(%0), 0" :: "r" (ra) );
}
static void test_pstfd_0 (void) {
  __asm__ __volatile__ ("pstfd 26, 0(%0), 0" :: "r" (ra) );
}
static void test_pstfs_32 (void) {
  __asm__ __volatile__ ("pstfs 26, 32(%0), 0" :: "r" (ra) );
}
static void test_pstfs_16 (void) {
  __asm__ __volatile__ ("pstfs 26, 16(%0), 0" :: "r" (ra) );
}
static void test_pstfs_8 (void) {
  __asm__ __volatile__ ("pstfs 26, 8(%0), 0" :: "r" (ra) );
}
static void test_pstfs_4 (void) {
  __asm__ __volatile__ ("pstfs 26, 4(%0), 0" :: "r" (ra) );
}
static void test_pstfs_0 (void) {
  __asm__ __volatile__ ("pstfs 26, 0(%0), 0" :: "r" (ra) );
}
static void test_plxsd_64 (void) {
  __asm__ __volatile__ ("plxsd %0, 64(%1), 0" : "=v" (vrt) : "r" (ra) );
}
static void test_plxsd_32 (void) {
  __asm__ __volatile__ ("plxsd %0, 32(%1), 0" : "=v" (vrt) : "r" (ra) );
}
static void test_plxsd_16 (void) {
  __asm__ __volatile__ ("plxsd %0, 16(%1), 0" : "=v" (vrt) : "r" (ra) );
}
static void test_plxsd_8 (void) {
  __asm__ __volatile__ ("plxsd %0, 8(%1), 0" : "=v" (vrt) : "r" (ra) );
}
static void test_plxsd_4 (void) {
  __asm__ __volatile__ ("plxsd %0, 4(%1), 0" : "=v" (vrt) : "r" (ra) );
}
static void test_plxsd_0 (void) {
  __asm__ __volatile__ ("plxsd %0, 0(%1), 0" : "=v" (vrt) : "r" (ra) );
}
static void test_plxssp_64 (void) {
  __asm__ __volatile__ ("plxssp %0, 64(%1), 0" : "=v" (vrt) : "r" (ra) );
}
static void test_plxssp_32 (void) {
  __asm__ __volatile__ ("plxssp %0, 32(%1), 0" : "=v" (vrt) : "r" (ra) );
}
static void test_plxssp_16 (void) {
  __asm__ __volatile__ ("plxssp %0, 16(%1), 0" : "=v" (vrt) : "r" (ra) );
}
static void test_plxssp_8 (void) {
  __asm__ __volatile__ ("plxssp %0, 8(%1), 0" : "=v" (vrt) : "r" (ra) );
}
static void test_plxssp_4 (void) {
  __asm__ __volatile__ ("plxssp %0, 4(%1), 0" : "=v" (vrt) : "r" (ra) );
}
static void test_plxssp_0 (void) {
  __asm__ __volatile__ ("plxssp %0, 0(%1), 0" : "=v" (vrt) : "r" (ra) );
}
static void test_plxv_16_0 (void) {
  __asm__ __volatile__ ("plxv %x0, 16(%1), 0" : "=wa" (vec_xt) : "r" (ra) );
}
static void test_plxv_8_0 (void) {
  __asm__ __volatile__ ("plxv %x0, 8(%1), 0" : "=wa" (vec_xt) : "r" (ra) );
}
static void test_plxv_4_0 (void) {
  __asm__ __volatile__ ("plxv %x0, 4(%1), 0" : "=wa" (vec_xt) : "r" (ra) );
}
static void test_plxv_0_0 (void) {
  __asm__ __volatile__ ("plxv %x0, 0(%1), 0" : "=wa" (vec_xt) : "r" (ra) );
}
static void test_pstxsd_64 (void) {
  __asm__ __volatile__ ("pstxsd 22, 64(%0), 0" :: "r" (ra) );
}
static void test_pstxsd_32 (void) {
  __asm__ __volatile__ ("pstxsd 22, 32(%0), 0" :: "r" (ra) );
}
static void test_pstxsd_16 (void) {
  __asm__ __volatile__ ("pstxsd 22, 16(%0), 0" :: "r" (ra) );
}
static void test_pstxsd_8 (void) {
  __asm__ __volatile__ ("pstxsd 22, 8(%0), 0" :: "r" (ra) );
}
static void test_pstxsd_4 (void) {
  __asm__ __volatile__ ("pstxsd 22, 4(%0), 0" :: "r" (ra) );
}
static void test_pstxsd_0 (void) {
  __asm__ __volatile__ ("pstxsd 22, 0(%0), 0" :: "r" (ra) );
}
static void test_pstxssp_64 (void) {
  __asm__ __volatile__ ("pstxssp 22, 64(%0), 0" :: "r" (ra) );
}
static void test_pstxssp_32 (void) {
  __asm__ __volatile__ ("pstxssp 22, 32(%0), 0" :: "r" (ra) );
}
static void test_pstxssp_16 (void) {
  __asm__ __volatile__ ("pstxssp 22, 16(%0), 0" :: "r" (ra) );
}
static void test_pstxssp_8 (void) {
  __asm__ __volatile__ ("pstxssp 22, 8(%0), 0" :: "r" (ra) );
}
static void test_pstxssp_4 (void) {
  __asm__ __volatile__ ("pstxssp 22, 4(%0), 0" :: "r" (ra) );
}
static void test_pstxssp_0 (void) {
  __asm__ __volatile__ ("pstxssp 22, 0(%0), 0" :: "r" (ra) );
}
static void test_pstxv_16 (void) {
  __asm__ __volatile__ ("pstxv %x0, 16(%1), 0" :: "wa" (vec_xs), "r" (ra) );
}
static void test_pstxv_8 (void) {
  __asm__ __volatile__ ("pstxv %x0, 8(%1), 0" :: "wa" (vec_xs), "r" (ra) );
}
static void test_pstxv_4 (void) {
  __asm__ __volatile__ ("pstxv %x0, 4(%1), 0" :: "wa" (vec_xs), "r" (ra) );
}
static void test_pstxv_0 (void) {
  __asm__ __volatile__ ("pstxv %x0, 0(%1), 0" :: "wa" (vec_xs), "r" (ra) );
}

static test_list_t testgroup_generic[] = {
  { &test_plfd_0, "plfd 0", "FRT,D(RA),R"}, /* bcwp */
  { &test_plfd_4, "plfd 4", "FRT,D(RA),R"}, /* bcwp */
  { &test_plfd_8, "plfd 8", "FRT,D(RA),R"}, /* bcwp */
  { &test_plfd_16, "plfd 16", "FRT,D(RA),R"}, /* bcwp */
  { &test_plfd_32, "plfd 32", "FRT,D(RA),R"}, /* bcwp */
  { &test_plfd_64, "plfd 64", "FRT,D(RA),R"}, /* bcwp */
  { &test_plfs_0, "plfs 0", "FRT,D(RA),R"}, /* bcwp */
  { &test_plfs_4, "plfs 4", "FRT,D(RA),R"}, /* bcwp */
  { &test_plfs_8, "plfs 8", "FRT,D(RA),R"}, /* bcwp */
  { &test_plfs_16, "plfs 16", "FRT,D(RA),R"}, /* bcwp */
  { &test_plfs_32, "plfs 32", "FRT,D(RA),R"}, /* bcwp */
  { &test_plfs_64, "plfs 64", "FRT,D(RA),R"}, /* bcwp */
  { &test_plxsd_0, "plxsd 0", "VRT,D(RA),R", 0b00110000}, /* bcwp */
  { &test_plxsd_4, "plxsd 4", "VRT,D(RA),R", 0b00110000}, /* bcwp */
  { &test_plxsd_8, "plxsd 8", "VRT,D(RA),R", 0b00110000}, /* bcwp */
  { &test_plxsd_16, "plxsd 16", "VRT,D(RA),R", 0b00110000}, /* bcwp */
  { &test_plxsd_32, "plxsd 32", "VRT,D(RA),R", 0b00110000}, /* bcwp */
  { &test_plxsd_64, "plxsd 64", "VRT,D(RA),R", 0b00110000}, /* bcwp */
  { &test_plxssp_0, "plxssp 0", "VRT,D(RA),R", 0b00001111}, /* bcwp */
  { &test_plxssp_4, "plxssp 4", "VRT,D(RA),R", 0b00001111}, /* bcwp */
  { &test_plxssp_8, "plxssp 8", "VRT,D(RA),R", 0b00001111}, /* bcwp */
  { &test_plxssp_16, "plxssp 16", "VRT,D(RA),R", 0b00001111}, /* bcwp */
  { &test_plxssp_32, "plxssp 32", "VRT,D(RA),R", 0b00001111}, /* bcwp */
  { &test_plxssp_64, "plxssp 64", "VRT,D(RA),R", 0b00001111}, /* bcwp */
  { &test_plxv_0_0, "plxv 0_0", "XT,D(RA),R"}, /* bcwp */
  { &test_plxv_4_0, "plxv 4_0", "XT,D(RA),R"}, /* bcwp */
  { &test_plxv_8_0, "plxv 8_0", "XT,D(RA),R"}, /* bcwp */
  { &test_plxv_16_0, "plxv 16_0", "XT,D(RA),R"}, /* bcwp */
  { &test_pstfd_0, "pstfd 0", "FRS,D(RA),R", 0b00110000}, /* bcwp */
  { &test_pstfd_4, "pstfd 4", "FRS,D(RA),R", 0b00110000}, /* bcwp */
  { &test_pstfd_8, "pstfd 8", "FRS,D(RA),R", 0b00110000}, /* bcwp */
  { &test_pstfd_16, "pstfd 16", "FRS,D(RA),R", 0b00110000}, /* bcwp */
  { &test_pstfd_32, "pstfd 32", "FRS,D(RA),R", 0b00110000}, /* bcwp */
  { &test_pstfs_0, "pstfs 0", "FRS,D(RA),R", 0b00001111}, /* bcwp */
  { &test_pstfs_4, "pstfs 4", "FRS,D(RA),R", 0b00001111}, /* bcwp */
  { &test_pstfs_8, "pstfs 8", "FRS,D(RA),R", 0b00001111}, /* bcwp */
  { &test_pstfs_16, "pstfs 16", "FRS,D(RA),R", 0b00001111}, /* bcwp */
  { &test_pstfs_32, "pstfs 32", "FRS,D(RA),R", 0b00001111}, /* bcwp */
  { &test_pstxsd_0, "pstxsd 0", "VRS,D(RA),R"}, /* bcwp */
  { &test_pstxsd_4, "pstxsd 4", "VRS,D(RA),R"}, /* bcwp */
  { &test_pstxsd_8, "pstxsd 8", "VRS,D(RA),R"}, /* bcwp */
  { &test_pstxsd_16, "pstxsd 16", "VRS,D(RA),R"}, /* bcwp */
  { &test_pstxsd_32, "pstxsd 32", "VRS,D(RA),R"}, /* bcwp */
  { &test_pstxsd_64, "pstxsd 64", "VRS,D(RA),R"}, /* bcwp */
  { &test_pstxssp_0, "pstxssp 0", "VRS,D(RA),R"}, /* bcwp */
  { &test_pstxssp_4, "pstxssp 4", "VRS,D(RA),R"}, /* bcwp */
  { &test_pstxssp_8, "pstxssp 8", "VRS,D(RA),R"}, /* bcwp */
  { &test_pstxssp_16, "pstxssp 16", "VRS,D(RA),R"}, /* bcwp */
  { &test_pstxssp_32, "pstxssp 32", "VRS,D(RA),R"}, /* bcwp */
  { &test_pstxssp_64, "pstxssp 64", "VRS,D(RA),R"}, /* bcwp */
  { &test_pstxv_0, "pstxv 0", "XS,D(RA),R"}, /* bcwp */
  { &test_pstxv_4, "pstxv 4", "XS,D(RA),R"}, /* bcwp */
  { &test_pstxv_8, "pstxv 8", "XS,D(RA),R"}, /* bcwp */
  { &test_pstxv_16, "pstxv 16", "XS,D(RA),R"}, /* bcwp */
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

   for (vrai = a_start; vrai < a_iters ; vrai+=a_inc) {
      for (vrbi = b_start; vrbi < b_iters ; vrbi+=b_inc) {
	 for (vrci = c_start; vrci < c_iters ; vrci+=c_inc) {
	    for (vrmi = m_start; (vrmi < m_iters) ; vrmi+=m_inc) {
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
