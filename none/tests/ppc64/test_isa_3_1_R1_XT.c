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

static void test_pstxvp_off0_R1 (void) {
  __asm__ __volatile__ ("pstxvp 20, -0x1f400+0(0),1");
}
static void test_pstxvp_off16_R1 (void) {
  __asm__ __volatile__ ("pstxvp 20, -0x1f400+16(0),1");
}
static void test_pstxvp_off32_R1 (void) {
  __asm__ __volatile__ ("pstxvp 20, -0x1f400+32(0),1");
}
static void test_pstxvp_off48_R1 (void) {
  __asm__ __volatile__ ("pstxvp 20, -0x1f400+48(0),1");
}
static void test_plfd_64_R1 (void) {
  __asm__ __volatile__ ("plfd 28, +64(0), 1");
	PAD_ORI
	PAD_ORI
}
static void test_plfd_32_R1 (void) {
  __asm__ __volatile__ ("plfd 28, +32(0), 1");
	PAD_ORI
}
static void test_plfd_16_R1 (void) {
  __asm__ __volatile__ ("plfd 28, +16(0), 1");
	PAD_ORI
}
static void test_plfd_8_R1 (void) {
  __asm__ __volatile__ ("plfd 28, +8(0), 1");
	PAD_ORI
}
static void test_plfd_4_R1 (void) {
  __asm__ __volatile__ ("plfd 28, +4(0), 1");
	PAD_ORI
}
static void test_plfd_0_R1 (void) {
  __asm__ __volatile__ ("plfd 28, +0(0), 1");
	PAD_ORI
}
static void test_plfs_64_R1 (void) {
  __asm__ __volatile__ ("plfs 28, +64(0), 1");
	PAD_ORI
	PAD_ORI
}
static void test_plfs_32_R1 (void) {
  __asm__ __volatile__ ("plfs 28, +32(0), 1");
	PAD_ORI
}
static void test_plfs_16_R1 (void) {
  __asm__ __volatile__ ("plfs 28, +16(0), 1");
	PAD_ORI
}
static void test_plfs_8_R1 (void) {
  __asm__ __volatile__ ("plfs 28, +8(0), 1");
	PAD_ORI
}
static void test_plfs_4_R1 (void) {
  __asm__ __volatile__ ("plfs 28, +4(0), 1");
	PAD_ORI
}
static void test_plfs_0_R1 (void) {
  __asm__ __volatile__ ("plfs 28, +0(0), 1");
	PAD_ORI
}
static void test_pstfd_32_R1 (void) {
  __asm__ __volatile__ ("pstfd 26, -0x1f400+32(0), 1");
}
static void test_pstfd_16_R1 (void) {
  __asm__ __volatile__ ("pstfd 26, -0x1f400+16(0), 1");
}
static void test_pstfd_8_R1 (void) {
  __asm__ __volatile__ ("pstfd 26, -0x1f400+8(0), 1");
}
static void test_pstfd_4_R1 (void) {
  __asm__ __volatile__ ("pstfd 26, -0x1f400+4(0), 1");
}
static void test_pstfd_0_R1 (void) {
  __asm__ __volatile__ ("pstfd 26, -0x1f400+0(0), 1");
}
static void test_pstfs_32_R1 (void) {
  __asm__ __volatile__ ("pstfs 26, -0x1f400+32(0), 1");
}
static void test_pstfs_16_R1 (void) {
  __asm__ __volatile__ ("pstfs 26, -0x1f400+16(0), 1");
}
static void test_pstfs_8_R1 (void) {
  __asm__ __volatile__ ("pstfs 26, -0x1f400+8(0), 1");
}
static void test_pstfs_4_R1 (void) {
  __asm__ __volatile__ ("pstfs 26, -0x1f400+4(0), 1");
}
static void test_pstfs_0_R1 (void) {
  __asm__ __volatile__ ("pstfs 26, -0x1f400+0(0), 1");
}
static void test_plxsd_64_R1 (void) {
  __asm__ __volatile__ ("plxsd %0, +64(0), 1" : "=v" (vrt) );
	PAD_ORI
	PAD_ORI
}
static void test_plxsd_32_R1 (void) {
  __asm__ __volatile__ (".align 2 ; plxsd %0, +32(0), 1" : "=v" (vrt) );
	PAD_ORI
}
static void test_plxsd_16_R1 (void) {
  __asm__ __volatile__ ("plxsd %0, +16(0), 1; pnop;pnop;pnop; " : "=v" (vrt) );
	PAD_ORI
}
static void test_plxsd_8_R1 (void) {
  __asm__ __volatile__ ("plxsd %0, +8(0), 1; pnop;pnop;pnop; " : "=v" (vrt) );
	PAD_ORI
}
static void test_plxsd_4_R1 (void) {
  __asm__ __volatile__ ("plxsd %0, +4(0), 1; pnop;pnop;pnop; "  : "=v" (vrt) );
	PAD_ORI
}
static void test_plxsd_0_R1 (void) {
  __asm__ __volatile__ ("plxsd %0, +0(0), 1; pnop;pnop;pnop; " : "=v" (vrt) );
	PAD_ORI
}
static void test_plxssp_64_R1 (void) {
  __asm__ __volatile__ ("plxssp %0, +64(0), 1; pnop;pnop;pnop; " : "=v" (vrt) );
	PAD_ORI
	PAD_ORI
}
static void test_plxssp_32_R1 (void) {
  __asm__ __volatile__ ("plxssp %0, +32(0), 1; pnop; " : "=v" (vrt) );
	PAD_ORI
}
static void test_plxssp_16_R1 (void) {
  __asm__ __volatile__ ("plxssp %0, +16(0), 1; pnop;pnop;pnop; " : "=v" (vrt) );
	PAD_ORI
}
static void test_plxssp_8_R1 (void) {
  __asm__ __volatile__ ("plxssp %0, +8(0), 1; pnop;pnop;pnop; " : "=v" (vrt) );
	PAD_ORI
}
static void test_plxssp_4_R1 (void) {
  __asm__ __volatile__ ("plxssp %0, +4(0), 1; pnop;pnop;pnop; " : "=v" (vrt) );
	PAD_ORI
}
static void test_plxssp_0_R1 (void) {
  __asm__ __volatile__ ("plxssp %0, +0(0), 1; pnop;pnop;pnop; " : "=v" (vrt) );
	PAD_ORI
}
/* Follow the short-range plxv instructions with nop in order to
   pad out subsequent instructions.  When written there are found
   to be fluctuations in the instructions to store the result back
   into the target variable.  (pla,pstxv...).
   */
static void test_plxv_16_R1 (void) {
  __asm__ __volatile__ ("plxv %x0, +16(0), 1; pnop;pnop;pnop;" : "=wa" (vec_xt) );
	PAD_ORI
}
static void test_plxv_8_R1 (void) {
  __asm__ __volatile__ ("plxv %x0, +8(0), 1; pnop;pnop;pnop;" : "=wa" (vec_xt) );
	PAD_ORI
}
static void test_plxv_4_R1 (void) {
  __asm__ __volatile__ ("plxv %x0, +4(0), 1; pnop;pnop;pnop;" : "=wa" (vec_xt) );
	PAD_ORI
}
static void test_plxv_0_R1 (void) {
  __asm__ __volatile__ ("plxv %x0, +0(0), 1; pnop;pnop;pnop; " : "=wa" (vec_xt) );
	PAD_ORI
}
static void test_pstxsd_64_R1 (void) {
  __asm__ __volatile__ (".align 2 ; pstxsd 22, -0x1f400+64(0), 1" );
}
static void test_pstxsd_32_R1 (void) {
  __asm__ __volatile__ (".align 2 ; pstxsd 22, -0x1f400+32(0), 1" );
}
static void test_pstxsd_16_R1 (void) {
  __asm__ __volatile__ (".align 2 ; pstxsd 22, -0x1f400+16(0), 1" );
}
static void test_pstxsd_8_R1 (void) {
  __asm__ __volatile__ (".align 2 ; pstxsd 22, -0x1f400+8(0), 1" );
}
static void test_pstxsd_4_R1 (void) {
  __asm__ __volatile__ (".align 2 ; pstxsd 22, -0x1f400+4(0), 1"  );
}
static void test_pstxsd_0_R1 (void) {
  __asm__ __volatile__ (".align 2 ; pstxsd 22, -0x1f400+0(0), 1" );
}
static void test_pstxssp_64_R1 (void) {
  __asm__ __volatile__ ("pstxssp 22, -0x1f400+64(0), 1" );
}
static void test_pstxssp_32_R1 (void) {
  __asm__ __volatile__ ("pstxssp 22, -0x1f400+32(0), 1");
}
static void test_pstxssp_16_R1 (void) {
  __asm__ __volatile__ ("pstxssp 22, -0x1f400+16(0), 1");
}
static void test_pstxssp_8_R1 (void) {
  __asm__ __volatile__ ("pstxssp 22, -0x1f400+8(0), 1");
}
static void test_pstxssp_4_R1 (void) {
  __asm__ __volatile__ ("pstxssp 22, -0x1f400+4(0), 1");
}
static void test_pstxssp_0_R1 (void) {
  __asm__ __volatile__ ("pstxssp 22, -0x1f400+0(0), 1");
}
static void test_pstxv_16_R1 (void) {
  __asm__ __volatile__ ("pstxv %x0, -0x1f400+16(0), 1" :: "wa" (vec_xs));
}
static void test_pstxv_8_R1 (void) {
  __asm__ __volatile__ ("pstxv %x0, -0x1f400+8(0), 1" :: "wa" (vec_xs));
}
static void test_pstxv_4_R1 (void) {
  __asm__ __volatile__ ("pstxv %x0, -0x1f400+4(0), 1" :: "wa" (vec_xs));
}
static void test_pstxv_0_R1 (void) {
  __asm__ __volatile__ ("pstxv %x0, -0x1f400+0(0), 1" :: "wa" (vec_xs));
}

static test_list_t testgroup_generic[] = {
  { &test_plfd_0_R1, "plfd 0_R1", "FRT,D(RA),R"}, /* bcwp */
  { &test_plfd_4_R1, "plfd 4_R1", "FRT,D(RA),R"}, /* bcwp */
  { &test_plfd_8_R1, "plfd 8_R1", "FRT,D(RA),R"}, /* bcwp */
  { &test_plfd_16_R1, "plfd 16_R1", "FRT,D(RA),R"}, /* bcwp */
  { &test_plfd_32_R1, "plfd 32_R1", "FRT,D(RA),R"}, /* bcwp */
  { &test_plfd_64_R1, "plfd 64_R1", "FRT,D(RA),R"}, /* bcwp */
  { &test_plfs_0_R1, "plfs 0_R1", "FRT,D(RA),R"}, /* bcwp */
  { &test_plfs_4_R1, "plfs 4_R1", "FRT,D(RA),R"}, /* bcwp */
  { &test_plfs_8_R1, "plfs 8_R1", "FRT,D(RA),R"}, /* bcwp */
  { &test_plfs_16_R1, "plfs 16_R1", "FRT,D(RA),R"}, /* bcwp */
  { &test_plfs_32_R1, "plfs 32_R1", "FRT,D(RA),R"}, /* bcwp */
  { &test_plfs_64_R1, "plfs 64_R1", "FRT,D(RA),R"}, /* bcwp */
  { &test_plxsd_0_R1, "plxsd 0_R1", "VRT,D(RA),R", 0b00110000}, /* bcwp */
  { &test_plxsd_4_R1, "plxsd 4_R1", "VRT,D(RA),R", 0b00110000}, /* bcwp */
  { &test_plxsd_8_R1, "plxsd 8_R1", "VRT,D(RA),R", 0b00110000}, /* bcwp */
  { &test_plxsd_16_R1, "plxsd 16_R1", "VRT,D(RA),R", 0b00110000}, /* bcwp */
  { &test_plxsd_32_R1, "plxsd 32_R1", "VRT,D(RA),R", 0b00110000}, /* bcwp */
  { &test_plxsd_64_R1, "plxsd 64_R1", "VRT,D(RA),R", 0b00110000}, /* bcwp */
  { &test_plxssp_0_R1, "plxssp 0_R1", "VRT,D(RA),R", 0b00001111}, /* bcwp */
  { &test_plxssp_4_R1, "plxssp 4_R1", "VRT,D(RA),R", 0b00001111}, /* bcwp */
  { &test_plxssp_8_R1, "plxssp 8_R1", "VRT,D(RA),R", 0b00001111}, /* bcwp */
  { &test_plxssp_16_R1, "plxssp 16_R1", "VRT,D(RA),R", 0b00001111}, /* bcwp */
  { &test_plxssp_32_R1, "plxssp 32_R1", "VRT,D(RA),R", 0b00001111}, /* bcwp */
  { &test_plxssp_64_R1, "plxssp 64_R1", "VRT,D(RA),R", 0b00001111}, /* bcwp */
  { &test_plxv_0_R1, "plxv 0_R1", "XT,D(RA),R"}, /* bcwp */
  { &test_plxv_4_R1, "plxv 4_R1", "XT,D(RA),R"}, /* bcwp */
  { &test_plxv_8_R1, "plxv 8_R1", "XT,D(RA),R"}, /* bcwp */
  { &test_plxv_16_R1, "plxv 16_R1", "XT,D(RA),R"}, /* bcwp */
  { &test_pstfd_0_R1, "pstfd 0_R1", "FRS,D(RA),R", 0b00110000}, /* bcwp */
  { &test_pstfd_4_R1, "pstfd 4_R1", "FRS,D(RA),R", 0b00110000}, /* bcwp */
  { &test_pstfd_8_R1, "pstfd 8_R1", "FRS,D(RA),R", 0b00110000}, /* bcwp */
  { &test_pstfd_16_R1, "pstfd 16_R1", "FRS,D(RA),R", 0b00110000}, /* bcwp */
  { &test_pstfd_32_R1, "pstfd 32_R1", "FRS,D(RA),R", 0b00110000}, /* bcwp */
  { &test_pstfs_0_R1, "pstfs 0_R1", "FRS,D(RA),R", 0b00001111}, /* bcwp */
  { &test_pstfs_4_R1, "pstfs 4_R1", "FRS,D(RA),R", 0b00001111}, /* bcwp */
  { &test_pstfs_8_R1, "pstfs 8_R1", "FRS,D(RA),R", 0b00001111}, /* bcwp */
  { &test_pstfs_16_R1, "pstfs 16_R1", "FRS,D(RA),R", 0b00001111}, /* bcwp */
  { &test_pstfs_32_R1, "pstfs 32_R1", "FRS,D(RA),R", 0b00001111}, /* bcwp */
  { &test_pstxsd_0_R1, "pstxsd 0_R1", "VRS,D(RA),R"}, /* bcwp */
  { &test_pstxsd_4_R1, "pstxsd 4_R1", "VRS,D(RA),R"}, /* bcwp */
  { &test_pstxsd_8_R1, "pstxsd 8_R1", "VRS,D(RA),R"}, /* bcwp */
  { &test_pstxsd_16_R1, "pstxsd 16_R1", "VRS,D(RA),R"}, /* bcwp */
  { &test_pstxsd_32_R1, "pstxsd 32_R1", "VRS,D(RA),R"}, /* bcwp */
  { &test_pstxsd_64_R1, "pstxsd 64_R1", "VRS,D(RA),R"}, /* bcwp */
  { &test_pstxssp_0_R1, "pstxssp 0_R1", "VRS,D(RA),R"}, /* bcwp */
  { &test_pstxssp_4_R1, "pstxssp 4_R1", "VRS,D(RA),R"}, /* bcwp */
  { &test_pstxssp_8_R1, "pstxssp 8_R1", "VRS,D(RA),R"}, /* bcwp */
  { &test_pstxssp_16_R1, "pstxssp 16_R1", "VRS,D(RA),R"}, /* bcwp */
  { &test_pstxssp_32_R1, "pstxssp 32_R1", "VRS,D(RA),R"}, /* bcwp */
  { &test_pstxssp_64_R1, "pstxssp 64_R1", "VRS,D(RA),R"}, /* bcwp */
  { &test_pstxvp_off0_R1, "pstxvp off0_R1", "XSp,D(RA),R"}, /* bcwp */
  { &test_pstxvp_off16_R1, "pstxvp off16_R1", "XSp,D(RA),R"}, /* bcwp */
  { &test_pstxvp_off32_R1, "pstxvp off32_R1", "XSp,D(RA),R"}, /* bcwp */
  { &test_pstxvp_off48_R1, "pstxvp off48_R1", "XSp,D(RA),R"}, /* bcwp */
  { &test_pstxv_0_R1, "pstxv 0_R1", "XS,D(RA),R"}, /* bcwp */
  { &test_pstxv_4_R1, "pstxv 4_R1", "XS,D(RA),R"}, /* bcwp */
  { &test_pstxv_8_R1, "pstxv 8_R1", "XS,D(RA),R"}, /* bcwp */
  { &test_pstxv_16_R1, "pstxv 16_R1", "XS,D(RA),R"}, /* bcwp */
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
   init_pcrelative_write_target ();
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
		vec_xa[0]=0x1234;
		vec_xa[1]=0x4567;
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
		print_pcrelative_write_target ();
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
	identify_instruction_by_func_name (current_test.name);
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
	    if (verbose) printf("Running test number %ld\n", skip_count);
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
