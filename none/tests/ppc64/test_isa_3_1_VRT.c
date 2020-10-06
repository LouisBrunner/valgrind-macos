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

static void test_vmulhsw (void) {
  __asm__ __volatile__ ("vmulhsw %0, %1, %2"
				: "=v" (vrt) : "v" (vra), "v" (vrb) );
}
static void test_vmulhuw (void) {
  __asm__ __volatile__ ("vmulhuw %0, %1, %2"
				: "=v" (vrt) : "v" (vra), "v" (vrb) );
}
static void test_vmulhsd (void) {
  __asm__ __volatile__ ("vmulhsd %0, %1, %2"
				: "=v" (vrt) : "v" (vra), "v" (vrb) );
}
static void test_vmulhud (void) {
  __asm__ __volatile__ ("vmulhud %0, %1, %2"
				: "=v" (vrt) : "v" (vra), "v" (vrb) );
}
static void test_vmulld (void) {
  __asm__ __volatile__ ("vmulld %0, %1, %2"
				: "=v" (vrt) : "v" (vra), "v" (vrb) );
}
static void test_vdivsw (void) {
  __asm__ __volatile__ ("vdivsw %0, %1, %2"
				: "=v" (vrt) : "v" (vra), "v" (vrb) );
}
static void test_vdivuw (void) {
  __asm__ __volatile__ ("vdivuw %0, %1, %2"
				: "=v" (vrt) : "v" (vra), "v" (vrb) );
}
static void test_vdivesw (void) {
  __asm__ __volatile__ ("vdivesw %0, %1, %2"
				: "=v" (vrt) : "v" (vra), "v" (vrb) );
}
static void test_vdiveuw (void) {
  __asm__ __volatile__ ("vdiveuw %0, %1, %2"
				: "=v" (vrt) : "v" (vra), "v" (vrb) );
}
static void test_vdivsd (void) {
  __asm__ __volatile__ ("vdivsd %0, %1, %2"
				: "=v" (vrt) : "v" (vra), "v" (vrb) );
}
static void test_vdivud (void) {
  __asm__ __volatile__ ("vdivud %0, %1, %2"
				: "=v" (vrt) : "v" (vra), "v" (vrb) );
}
static void test_vdivesd (void) {
  __asm__ __volatile__ ("vdivesd %0, %1, %2"
				: "=v" (vrt) : "v" (vra), "v" (vrb) );
}
static void test_vdiveud (void) {
  __asm__ __volatile__ ("vdiveud %0, %1, %2"
				: "=v" (vrt) : "v" (vra), "v" (vrb) );
}
static void test_vmodsw (void) {
  __asm__ __volatile__ ("vmodsw %0, %1, %2"
				: "=v" (vrt) : "v" (vra), "v" (vrb) );
}
static void test_vmoduw (void) {
  __asm__ __volatile__ ("vmoduw %0, %1, %2"
				: "=v" (vrt) : "v" (vra), "v" (vrb) );
}
static void test_vmodsd (void) {
  __asm__ __volatile__ ("vmodsd %0, %1, %2"
				: "=v" (vrt) : "v" (vra), "v" (vrb) );
}
static void test_vmodud (void) {
  __asm__ __volatile__ ("vmodud %0, %1, %2"
				: "=v" (vrt) : "v" (vra), "v" (vrb) );
}
static void test_vextdubvlx (void) {
  __asm__ __volatile__ ("vextdubvlx %0, %1, %2, %3"
			: "=v" (vrt) : "v" (vra), "v" (vrb), "r" (rc) );
}
static void test_vextdubvrx (void) {
  __asm__ __volatile__ ("vextdubvrx %0, %1, %2, %3"
			: "=v" (vrt) : "v" (vra), "v" (vrb), "r" (rc) );
}
static void test_vextduhvlx (void) {
  __asm__ __volatile__ ("vextduhvlx %0, %1, %2, %3"
			: "=v" (vrt) : "v" (vra), "v" (vrb), "r" (rc) );
}
static void test_vextduhvrx (void) {
  __asm__ __volatile__ ("vextduhvrx %0, %1, %2, %3"
			: "=v" (vrt) : "v" (vra), "v" (vrb), "r" (rc) );
}
static void test_vextduwvlx (void) {
  __asm__ __volatile__ ("vextduwvlx %0, %1, %2, %3"
			: "=v" (vrt) : "v" (vra), "v" (vrb), "r" (rc) );
}
static void test_vextduwvrx (void) {
  __asm__ __volatile__ ("vextduwvrx %0, %1, %2, %3"
			: "=v" (vrt) : "v" (vra), "v" (vrb), "r" (rc) );
}
static void test_vextddvlx (void) {
  __asm__ __volatile__ ("vextddvlx %0, %1, %2, %3"
			: "=v" (vrt) : "v" (vra), "v" (vrb), "r" (rc) );
}
static void test_vextddvrx (void) {
  __asm__ __volatile__ ("vextddvrx %0, %1, %2, %3"
			: "=v" (vrt) : "v" (vra), "v" (vrb), "r" (rc) );
}
static void test_vinsblx (void) {
  __asm__ __volatile__ ("vinsblx %0, %1, %2"
				: "+v" (vrt) : "r" (ra), "r" (rb) );
}
static void test_vinsbrx (void) {
  __asm__ __volatile__ ("vinsbrx %0, %1, %2"
				: "+v" (vrt) : "r" (ra), "r" (rb) );
}
static void test_vinshlx (void) {
  __asm__ __volatile__ ("vinshlx %0, %1, %2"
				: "+v" (vrt) : "r" (ra), "r" (rb) );
}
static void test_vinshrx (void) {
  __asm__ __volatile__ ("vinshrx %0, %1, %2"
				: "+v" (vrt) : "r" (ra), "r" (rb) );
}
static void test_vinswlx (void) {
  __asm__ __volatile__ ("vinswlx %0, %1, %2"
				: "+v" (vrt) : "r" (ra), "r" (rb) );
}
static void test_vinswrx (void) {
  __asm__ __volatile__ ("vinswrx %0, %1, %2"
				: "+v" (vrt) : "r" (ra), "r" (rb) );
}
static void test_vinsdlx (void) {
  __asm__ __volatile__ ("vinsdlx %0, %1, %2"
				: "+v" (vrt) : "r" (ra), "r" (rb) );
}
static void test_vinsdrx (void) {
  __asm__ __volatile__ ("vinsdrx %0, %1, %2"
				: "+v" (vrt) : "r" (ra), "r" (rb) );
}
static void test_vinsbvlx (void) {
  __asm__ __volatile__ ("vinsbvlx %0, %1, %2"
				: "+v" (vrt) : "r" (ra), "v" (vrb) );
}
static void test_vinshvlx (void) {
  __asm__ __volatile__ ("vinshvlx %0, %1, %2"
				: "+v" (vrt) : "r" (ra), "v" (vrb) );
}
static void test_vinsbvrx (void) {
  __asm__ __volatile__ ("vinsbvrx %0, %1, %2"
				: "+v" (vrt) : "r" (ra), "v" (vrb) );
}
static void test_vinshvrx (void) {
  __asm__ __volatile__ ("vinshvrx %0, %1, %2"
				: "+v" (vrt) : "r" (ra), "v" (vrb) );
}
static void test_vinswvlx (void) {
  __asm__ __volatile__ ("vinswvlx %0, %1, %2"
				: "+v" (vrt) : "r" (ra), "v" (vrb) );
}
static void test_vinswvrx (void) {
  __asm__ __volatile__ ("vinswvrx %0, %1, %2"
				: "+v" (vrt) : "r" (ra), "v" (vrb) );
}
static void test_vinsw_3 (void) {
  __asm__ __volatile__ ("vinsw %0, %1, 3" : "+v" (vrt) : "r" (rb) );
}
static void test_vinsw_7 (void) {
  __asm__ __volatile__ ("vinsw %0, %1, 7" : "+v" (vrt) : "r" (rb) );
}
static void test_vinsd_3 (void) {
  __asm__ __volatile__ ("vinsd %0, %1, 3" : "+v" (vrt) : "r" (rb) );
}
static void test_vinsd_7 (void) {
  __asm__ __volatile__ ("vinsd %0, %1, 7" : "+v" (vrt) : "r" (rb) );
}
static void test_vsldbi_0 (void) {
  __asm__ __volatile__ ("vsldbi %0, %1, %2, 0"
				: "=v" (vrt) : "v" (vra), "v" (vrb) );
}
static void test_vsldbi_4 (void) {
  __asm__ __volatile__ ("vsldbi %0, %1, %2, 4"
				: "=v" (vrt) : "v" (vra), "v" (vrb) );
}
static void test_vsrdbi_0 (void) {
  __asm__ __volatile__ ("vsrdbi %0, %1, %2, 0"
				: "=v" (vrt) : "v" (vra), "v" (vrb) );
}
static void test_vsrdbi_4 (void) {
  __asm__ __volatile__ ("vsrdbi %0, %1, %2, 4"
				: "=v" (vrt) : "v" (vra), "v" (vrb) );
}
static void test_xscmpeqqp (void) {
  __asm__ __volatile__ ("xscmpeqqp %0, %1, %2"
				: "=v" (vrt) : "v" (vra), "v" (vrb) );
}
static void test_xscmpgeqp (void) {
  __asm__ __volatile__ ("xscmpgeqp %0, %1, %2"
				: "=v" (vrt) : "v" (vra), "v" (vrb) );
}
static void test_xscmpgtqp (void) {
  __asm__ __volatile__ ("xscmpgtqp %0, %1, %2"
				: "=v" (vrt) : "v" (vra), "v" (vrb) );
}
static void test_xsmaxcqp (void) {
  __asm__ __volatile__ ("xsmaxcqp %0, %1, %2"
				: "=v" (vrt) : "v" (vra), "v" (vrb) );
}
static void test_xsmincqp (void) {
  __asm__ __volatile__ ("xsmincqp %0, %1, %2"
				: "=v" (vrt) : "v" (vra), "v" (vrb) );
}

static test_list_t testgroup_generic[] = {
  { &test_vdivesd, "vdivesd", "VRT,VRA,VRB"}, /* bcs */
  { &test_vdivesw, "vdivesw", "VRT,VRA,VRB"}, /* bcs */
  { &test_vdiveud, "vdiveud", "VRT,VRA,VRB"}, /* bcs */
  { &test_vdiveuw, "vdiveuw", "VRT,VRA,VRB"}, /* bcs */
  { &test_vdivsd, "vdivsd", "VRT,VRA,VRB"}, /* bcs */
  { &test_vdivsw, "vdivsw", "VRT,VRA,VRB"}, /* bcs */
  { &test_vdivud, "vdivud", "VRT,VRA,VRB"}, /* bcs */
  { &test_vdivuw, "vdivuw", "VRT,VRA,VRB"}, /* bcs */
  { &test_vextddvlx, "vextddvlx", "VRT,VRA,VRB,RC"}, /* bcs */
  { &test_vextddvrx, "vextddvrx", "VRT,VRA,VRB,RC"}, /* bcs */
  { &test_vextdubvlx, "vextdubvlx", "VRT,VRA,VRB,RC"}, /* bcs */
  { &test_vextdubvrx, "vextdubvrx", "VRT,VRA,VRB,RC"}, /* bcs */
  { &test_vextduhvlx, "vextduhvlx", "VRT,VRA,VRB,RC"}, /* bcs */
  { &test_vextduhvrx, "vextduhvrx", "VRT,VRA,VRB,RC"}, /* bcs */
  { &test_vextduwvlx, "vextduwvlx", "VRT,VRA,VRB,RC"}, /* bcs */
  { &test_vextduwvrx, "vextduwvrx", "VRT,VRA,VRB,RC"}, /* bcs */
  { &test_vinsblx, "vinsblx", "VRT,RA,RB"}, /* bcs */
  { &test_vinsbrx, "vinsbrx", "VRT,RA,RB"}, /* bcs */
  { &test_vinsbvlx, "vinsbvlx", "VRT,RA,VRB"}, /* bcs */
  { &test_vinsbvrx, "vinsbvrx", "VRT,RA,VRB"}, /* bcs */
  { &test_vinsdlx, "vinsdlx", "VRT,RA,RB"}, /* bcs */
  { &test_vinsdrx, "vinsdrx", "VRT,RA,RB"}, /* bcs */
  { &test_vinsd_3, "vinsd 3", "VRT,RB,UIM"}, /* bcwp */
  { &test_vinsd_7, "vinsd 7", "VRT,RB,UIM"}, /* bcwp */
  { &test_vinshlx, "vinshlx", "VRT,RA,RB"}, /* bcs */
  { &test_vinshrx, "vinshrx", "VRT,RA,RB"}, /* bcs */
  { &test_vinshvlx, "vinshvlx", "VRT,RA,VRB"}, /* bcs */
  { &test_vinshvrx, "vinshvrx", "VRT,RA,VRB"}, /* bcs */
  { &test_vinswlx, "vinswlx", "VRT,RA,RB"}, /* bcs */
  { &test_vinswrx, "vinswrx", "VRT,RA,RB"}, /* bcs */
  { &test_vinswvlx, "vinswvlx", "VRT,RA,VRB"}, /* bcs */
  { &test_vinswvrx, "vinswvrx", "VRT,RA,VRB"}, /* bcs */
  { &test_vinsw_3, "vinsw 3", "VRT,RB,UIM"}, /* bcwp */
  { &test_vinsw_7, "vinsw 7", "VRT,RB,UIM"}, /* bcwp */
  { &test_vmodsd, "vmodsd", "VRT,VRA,VRB"}, /* bcs */
  { &test_vmodsw, "vmodsw", "VRT,VRA,VRB"}, /* bcs */
  { &test_vmodud, "vmodud", "VRT,VRA,VRB"}, /* bcs */
  { &test_vmoduw, "vmoduw", "VRT,VRA,VRB"}, /* bcs */
  { &test_vmulhsd, "vmulhsd", "VRT,VRA,VRB"}, /* bcs */
  { &test_vmulhsw, "vmulhsw", "VRT,VRA,VRB"}, /* bcs */
  { &test_vmulhud, "vmulhud", "VRT,VRA,VRB"}, /* bcs */
  { &test_vmulhuw, "vmulhuw", "VRT,VRA,VRB"}, /* bcs */
  { &test_vmulld, "vmulld", "VRT,VRA,VRB"}, /* bcs */
  { &test_vsldbi_0, "vsldbi 0", "VRT,VRA,VRB,SH"}, /* bcwp */
  { &test_vsldbi_4, "vsldbi 4", "VRT,VRA,VRB,SH"}, /* bcwp */
  { &test_vsrdbi_0, "vsrdbi 0", "VRT,VRA,VRB,SH"}, /* bcwp */
  { &test_vsrdbi_4, "vsrdbi 4", "VRT,VRA,VRB,SH"}, /* bcwp */
  { &test_xscmpeqqp, "xscmpeqqp", "VRT,VRA,VRB"}, /* bcs */
  { &test_xscmpgeqp, "xscmpgeqp", "VRT,VRA,VRB"}, /* bcs */
  { &test_xscmpgtqp, "xscmpgtqp", "VRT,VRA,VRB"}, /* bcs */
  { &test_xsmaxcqp, "xsmaxcqp", "VRT,VRA,VRB"}, /* bcs */
  { &test_xsmincqp, "xsmincqp", "VRT,VRA,VRB"}, /* bcs */
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
