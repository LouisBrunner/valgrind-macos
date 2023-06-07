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

static void test_plxvp_off0_R1 (void) {
	PAD_ORI
  __asm__ __volatile__ ("plxvp 20, +0(0),1"  );
	PAD_ORI
}
static void test_plxvp_off8_R1 (void) {
	PAD_ORI
  __asm__ __volatile__ ("plxvp 20, +8(0),1" );
	PAD_ORI
}
static void test_plxvp_off16_R1 (void) {
	PAD_ORI
  __asm__ __volatile__ ("plxvp 20, +16(0),1" );
	PAD_ORI
}
static void test_plxvp_off24_R1 (void) {
	PAD_ORI
  __asm__ __volatile__ ("plxvp 20, +24(0),1" );
	PAD_ORI
}
static void test_plxvp_off32_R1 (void) {
	PAD_ORI
  __asm__ __volatile__ ("plxvp 20, +32(0),1" );
	PAD_ORI
}
static void test_plbz_off0_R1 (void) {
        PAD_ORI
  __asm__ __volatile__ ("plbz %0, +0(0), 1" : "=r" (rt) );
	PAD_ORI
}
static void test_plbz_off8_R1 (void) {
	PAD_ORI
  __asm__ __volatile__ ("plbz %0, +8(0), 1" : "=r" (rt) );
	PAD_ORI
}
static void test_plbz_off16_R1 (void) {
	PAD_ORI
  __asm__ __volatile__ ("plbz %0, +16(0), 1" : "=r" (rt) );
	PAD_ORI
}
static void test_plbz_off32_R1 (void) {
	PAD_ORI
  __asm__ __volatile__ ("plbz %0, +32(0), 1" : "=r" (rt) );
	PAD_ORI
}
static void test_plbz_off64_R1 (void) {
	PAD_ORI
  __asm__ __volatile__ ("plbz %0, +64(0), 1" : "=r" (rt) );
	PAD_ORI
	PAD_ORI
}
static void test_plhz_off0_R1 (void) {
	PAD_ORI
  __asm__ __volatile__ ("plhz %0, +0(0), 1" : "=r" (rt) );
	PAD_ORI
}
static void test_plhz_off8_R1 (void) {
	PAD_ORI
  __asm__ __volatile__ ("plhz %0, +8(0), 1" : "=r" (rt) );
	PAD_ORI
}
static void test_plhz_off16_R1 (void) {
	PAD_ORI
  __asm__ __volatile__ ("plhz %0, +16(0), 1" : "=r" (rt) );
	PAD_ORI
}
static void test_plhz_off32_R1 (void) {
	PAD_ORI
  __asm__ __volatile__ ("plhz %0, +32(0), 1" : "=r" (rt) );
	PAD_ORI
}
static void test_plhz_off64_R1 (void) {
	PAD_ORI
  __asm__ __volatile__ ("plhz %0, +64(0), 1" : "=r" (rt) );
	PAD_ORI
	PAD_ORI
}
static void test_plha_off0_R1 (void) {
	PAD_ORI
  __asm__ __volatile__ ("plha %0, +0(0), 1" : "=r" (rt) );
	PAD_ORI
}
static void test_plha_off8_R1 (void) {
	PAD_ORI
  __asm__ __volatile__ ("plha %0, +8(0), 1" : "=r" (rt)  );
	PAD_ORI
}
static void test_plha_off16_R1 (void) {
	PAD_ORI
  __asm__ __volatile__ ("plha %0, +16(0), 1" : "=r" (rt) );
	PAD_ORI
}
static void test_plha_off32_R1 (void) {
	PAD_ORI
  __asm__ __volatile__ ("plha %0, +32(0), 1" : "=r" (rt) );
	PAD_ORI
}
static void test_plha_off64_R1 (void) {
	PAD_ORI
  __asm__ __volatile__ ("plha %0, +64(0), 1" : "=r" (rt) );
	PAD_ORI
	PAD_ORI
}
static void test_plwz_off0_R1 (void) {
  __asm__ __volatile__ ("plwz %0, +0(0), 1" : "=r" (rt)  );
}
static void test_plwz_off8_R1 (void) {
  __asm__ __volatile__ ("plwz %0, +8(0), 1" : "=r" (rt) );
}
static void test_plwz_off16_R1 (void) {
  __asm__ __volatile__ ("plwz %0, +16(0), 1" : "=r" (rt) );
}
static void test_plwz_off32_R1 (void) {
	PAD_ORI
  __asm__ __volatile__ ("plwz %0, +32(0), 1" : "=r" (rt) );
	PAD_ORI
}
static void test_plwz_off64_R1 (void) {
	PAD_ORI
  __asm__ __volatile__ ("plwz %0, +64(0), 1" : "=r" (rt) );
	PAD_ORI
	PAD_ORI
}
static void test_plwa_off0_R1 (void) {
  __asm__ __volatile__ ("plwa %0, +0(0), 1" : "=r" (rt)  );
}
static void test_plwa_off8_R1 (void) {
  __asm__ __volatile__ ("plwa %0, +8(0), 1" : "=r" (rt)  );
}
static void test_plwa_off16_R1 (void) {
  __asm__ __volatile__ ("plwa %0, +16(0), 1" : "=r" (rt) );
}
static void test_plwa_off32_R1 (void) {
	PAD_ORI
  __asm__ __volatile__ ("plwa %0, +32(0), 1" : "=r" (rt) );
	PAD_ORI
}
static void test_plwa_off64_R1 (void) {
	PAD_ORI
  __asm__ __volatile__ ("plwa %0, +64(0), 1" : "=r" (rt) );
	PAD_ORI
	PAD_ORI
}
static void test_pld_off0_R1 (void) {
	PAD_ORI
  __asm__ __volatile__ ("pld %0, +0(0), 1" : "=r" (rt)  );
	PAD_ORI
}
static void test_pld_off8_R1 (void) {
	PAD_ORI
  __asm__ __volatile__ ("pld %0, +8(0), 1" : "=r" (rt)  );
	PAD_ORI
}
static void test_pld_off16_R1 (void) {
	PAD_ORI
  __asm__ __volatile__ ("pld %0, +16(0), 1" : "=r" (rt) );
	PAD_ORI
}
static void test_pld_off32_R1 (void) {
	PAD_ORI
  __asm__ __volatile__ ("pld %0, +32(0), 1" : "=r" (rt) );
	PAD_ORI
}
static void test_pld_off64_R1 (void) {
	PAD_ORI
  __asm__ __volatile__ ("pld %0, +64(0), 1" : "=r" (rt) );
	PAD_ORI
	PAD_ORI
}
static void test_pstb_off0_R1 (void) {
  __asm__ __volatile__ ("pstb %0, -0x1f400+0(0), 1" :: "r" (rs) );
}
static void test_pstb_off8_R1 (void) {
  __asm__ __volatile__ ("pstb %0, -0x1f400+8(0), 1" :: "r" (rs) );
}
static void test_pstb_off16_R1 (void) {
  __asm__ __volatile__ ("pstb %0, -0x1f400+16(0), 1" :: "r" (rs) );
}
static void test_pstb_off32_R1 (void) {
  __asm__ __volatile__ ("pstb %0, -0x1f400+32(0), 1" :: "r" (rs) );
}
static void test_psth_off0_R1 (void) {
  __asm__ __volatile__ ("psth %0, -0x1f400+0(0), 1" :: "r" (rs) );
}
static void test_psth_off8_R1 (void) {
  __asm__ __volatile__ ("psth %0, -0x1f400+8(0), 1" :: "r" (rs) );
}
static void test_psth_off16_R1 (void) {
  __asm__ __volatile__ ("psth %0, -0x1f400+16(0), 1" :: "r" (rs) );
}
static void test_psth_off32_R1 (void) {
  __asm__ __volatile__ ("psth %0, -0x1f400+32(0), 1" :: "r" (rs) );
}
static void test_pstw_off0_R1 (void) {
  __asm__ __volatile__ ("pstw %0, -0x1f400+0(0), 1" :: "r" (rs) );
}
static void test_pstw_off8_R1 (void) {
  __asm__ __volatile__ ("pstw %0, -0x1f400+8(0), 1" :: "r" (rs) );
}
static void test_pstw_off16_R1 (void) {
  __asm__ __volatile__ ("pstw %0, -0x1f400+16(0), 1" :: "r" (rs) );
}
static void test_pstw_off32_R1 (void) {
  __asm__ __volatile__ ("pstw %0, -0x1f400+32(0), 1" :: "r" (rs) );
}
static void test_pstd_off0_R1 (void) {
  __asm__ __volatile__ ("pstd %0, -0x1f400+0(0), 1" :: "r" (rs) );
}
static void test_pstd_off8_R1 (void) {
  __asm__ __volatile__ ("pstd %0, -0x1f400+8(0), 1" :: "r" (rs) );
}
static void test_pstd_off16_R1 (void) {
  __asm__ __volatile__ ("pstd %0, -0x1f400+16(0), 1" :: "r" (rs) );
}
static void test_pstd_off32_R1 (void) {
  __asm__ __volatile__ ("pstd %0, -0x1f400+32(0), 1" :: "r" (rs) );
}
  /* For the paddi tests; although we can get close to a read/write target
     due to forcing where the .text and .bss sections are placed, there is
     still enough codegen variability that having a raw value in the exp
     file will not be determinative for these instructions.
     Thus, compromise and just ensure that the generated value is an
     address that lands within the reloc buffer, and use quasi magic
     eyecatcher values in the return to indicate success.  */
static void test_paddi_0_R1 (void) {
  __asm__ __volatile__ ("paddi %0, 0, 0+0, 1" : "=r" (rt)  );
  rt = rt - TEXT_BSS_DELTA;
  if (rt > pcrelative_buff_addr(0) &&
		  rt < pcrelative_buff_addr(RELOC_BUFFER_SIZE))
	  rt = 0xffff0000;
}
static void test_paddi_12_R1 (void) {
  __asm__ __volatile__ ("paddi %0, 0, 0+12, 1" : "=r" (rt)  );
  rt = rt - TEXT_BSS_DELTA;
  if (rt > pcrelative_buff_addr(0) &&
		  rt < pcrelative_buff_addr(RELOC_BUFFER_SIZE))
	  rt = 0xffff0012;
}
static void test_paddi_48_R1 (void) {
  __asm__ __volatile__ ("paddi %0, 0, 0+48, 1" : "=r" (rt)  );
  rt = rt - TEXT_BSS_DELTA;
  if (rt > pcrelative_buff_addr(0) &&
		  rt < pcrelative_buff_addr(RELOC_BUFFER_SIZE))
	  rt = 0xffff0048;
}
static void test_paddi_98_R1 (void) {
  __asm__ __volatile__ ("paddi %0, 0, 0+98, 1" : "=r" (rt) );
  rt = rt - TEXT_BSS_DELTA;
  if (rt > pcrelative_buff_addr(0) &&
		  rt < pcrelative_buff_addr(RELOC_BUFFER_SIZE))
	  rt = 0xffff0098;
}
static void test_plq_off0_R1 (void) {
	PAD_ORI
  __asm__ __volatile__ ("plq %0, +0(0), 1" : "=r" (rt) );
	PAD_ORI
}
static void test_plq_off8_R1 (void) {
	PAD_ORI
  __asm__ __volatile__ ("plq %0, +8(0), 1" : "=r" (rt) );
	PAD_ORI
}
static void test_plq_off16_R1 (void) {
	PAD_ORI
  __asm__ __volatile__ ("plq %0, +16(0), 1" : "=r" (rt) );
	PAD_ORI
}
static void test_plq_off32_R1 (void) {
	PAD_ORI
  __asm__ __volatile__ ("plq %0, +32(0), 1" : "=r" (rt) );
	PAD_ORI
}
static void test_plq_off48_R1 (void) {
	PAD_ORI
  __asm__ __volatile__ ("plq %0, +48(0), 1" : "=r" (rt) );
	PAD_ORI
}
static void test_plq_off64_R1 (void) {
	PAD_ORI
  __asm__ __volatile__ ("plq %0, +64(0), 1" : "=r" (rt) );
	PAD_ORI
	PAD_ORI
}
static void test_pstq_off0_R1 (void) {
  __asm__ __volatile__ ("pstq 24, -0x1f400+0(0), 1"  );
}
static void test_pstq_off8_R1 (void) {
  __asm__ __volatile__ ("pstq 24, -0x1f400+8(0), 1"  );
}
static void test_pstq_off16_R1 (void) {
  __asm__ __volatile__ ("pstq 24, -0x1f400+16(0), 1"  );
}
static void test_pstq_off32_R1 (void) {
  __asm__ __volatile__ ("pstq 24, -0x1f400+32(0), 1"  );
}
static void test_pstq_off64_R1 (void) {
  __asm__ __volatile__ ("pstq 24, -0x1f400+64(0), 1"  );
}

static test_list_t testgroup_generic[] = {
  { &test_paddi_0_R1, "paddi 0_R1", "RT,RA,SI,R"}, /* bcwp */
  { &test_paddi_12_R1, "paddi 12_R1", "RT,RA,SI,R"}, /* bcwp */
  { &test_paddi_48_R1, "paddi 48_R1", "RT,RA,SI,R"}, /* bcwp */
  { &test_paddi_98_R1, "paddi 98_R1", "RT,RA,SI,R"}, /* bcwp */
  { &test_plbz_off0_R1, "plbz off0_R1", "RT,D(RA),R"}, /* bcwp */
  { &test_plbz_off8_R1, "plbz off8_R1", "RT,D(RA),R"}, /* bcwp */
  { &test_plbz_off16_R1, "plbz off16_R1", "RT,D(RA),R"}, /* bcwp */
  { &test_plbz_off32_R1, "plbz off32_R1", "RT,D(RA),R"}, /* bcwp */
  { &test_plbz_off64_R1, "plbz off64_R1", "RT,D(RA),R"}, /* bcwp */
  { &test_pld_off0_R1, "pld off0_R1", "RT,D(RA),R"}, /* bcwp */
  { &test_pld_off8_R1, "pld off8_R1", "RT,D(RA),R"}, /* bcwp */
  { &test_pld_off16_R1, "pld off16_R1", "RT,D(RA),R"}, /* bcwp */
  { &test_pld_off32_R1, "pld off32_R1", "RT,D(RA),R"}, /* bcwp */
  { &test_pld_off64_R1, "pld off64_R1", "RT,D(RA),R"}, /* bcwp */
  { &test_plha_off0_R1, "plha off0_R1", "RT,D(RA),R"}, /* bcwp */
  { &test_plha_off8_R1, "plha off8_R1", "RT,D(RA),R"}, /* bcwp */
  { &test_plha_off16_R1, "plha off16_R1", "RT,D(RA),R"}, /* bcwp */
  { &test_plha_off32_R1, "plha off32_R1", "RT,D(RA),R"}, /* bcwp */
  { &test_plha_off64_R1, "plha off64_R1", "RT,D(RA),R"}, /* bcwp */
  { &test_plhz_off0_R1, "plhz off0_R1", "RT,D(RA),R"}, /* bcwp */
  { &test_plhz_off8_R1, "plhz off8_R1", "RT,D(RA),R"}, /* bcwp */
  { &test_plhz_off16_R1, "plhz off16_R1", "RT,D(RA),R"}, /* bcwp */
  { &test_plhz_off32_R1, "plhz off32_R1", "RT,D(RA),R"}, /* bcwp */
  { &test_plhz_off64_R1, "plhz off64_R1", "RT,D(RA),R"}, /* bcwp */
  { &test_plq_off0_R1, "plq off0_R1", "RTp,D(RA),R"}, /* bcwp */
  { &test_plq_off8_R1, "plq off8_R1", "RTp,D(RA),R"}, /* bcwp */
  { &test_plq_off16_R1, "plq off16_R1", "RTp,D(RA),R"}, /* bcwp */
  { &test_plq_off32_R1, "plq off32_R1", "RTp,D(RA),R"}, /* bcwp */
  { &test_plq_off48_R1, "plq off48_R1", "RTp,D(RA),R"}, /* bcwp */
  { &test_plq_off64_R1, "plq off64_R1", "RTp,D(RA),R"}, /* bcwp */
  { &test_plwa_off0_R1, "plwa off0_R1", "RT,D(RA),R"}, /* bcwp */
  { &test_plwa_off8_R1, "plwa off8_R1", "RT,D(RA),R"}, /* bcwp */
  { &test_plwa_off16_R1, "plwa off16_R1", "RT,D(RA),R"}, /* bcwp */
  { &test_plwa_off32_R1, "plwa off32_R1", "RT,D(RA),R"}, /* bcwp */
  { &test_plwa_off64_R1, "plwa off64_R1", "RT,D(RA),R"}, /* bcwp */
  { &test_plwz_off0_R1, "plwz off0_R1", "RT,D(RA),R"}, /* bcwp */
  { &test_plwz_off8_R1, "plwz off8_R1", "RT,D(RA),R"}, /* bcwp */
  { &test_plwz_off16_R1, "plwz off16_R1", "RT,D(RA),R"}, /* bcwp */
  { &test_plwz_off32_R1, "plwz off32_R1", "RT,D(RA),R"}, /* bcwp */
  { &test_plwz_off64_R1, "plwz off64_R1", "RT,D(RA),R"}, /* bcwp */
  { &test_plxvp_off0_R1, "plxvp off0_R1", "XTp,D(RA),R"}, /* bcwp */
  { &test_plxvp_off8_R1, "plxvp off8_R1", "XTp,D(RA),R"}, /* bcwp */
  { &test_plxvp_off16_R1, "plxvp off16_R1", "XTp,D(RA),R"}, /* bcwp */
  { &test_plxvp_off24_R1, "plxvp off24_R1", "XTp,D(RA),R"}, /* bcwp */
  { &test_plxvp_off32_R1, "plxvp off32_R1", "XTp,D(RA),R"}, /* bcwp */
  { &test_pstb_off0_R1, "pstb off0_R1", "RS,D(RA),R"}, /* bcwp */
  { &test_pstb_off8_R1, "pstb off8_R1", "RS,D(RA),R"}, /* bcwp */
  { &test_pstb_off16_R1, "pstb off16_R1", "RS,D(RA),R"}, /* bcwp */
  { &test_pstb_off32_R1, "pstb off32_R1", "RS,D(RA),R"}, /* bcwp */
  { &test_pstd_off0_R1, "pstd off0_R1", "RS,D(RA),R"}, /* bcwp */
  { &test_pstd_off8_R1, "pstd off8_R1", "RS,D(RA),R"}, /* bcwp */
  { &test_pstd_off16_R1, "pstd off16_R1", "RS,D(RA),R"}, /* bcwp */
  { &test_pstd_off32_R1, "pstd off32_R1", "RS,D(RA),R"}, /* bcwp */
  { &test_psth_off0_R1, "psth off0_R1", "RS,D(RA),R"}, /* bcwp */
  { &test_psth_off8_R1, "psth off8_R1", "RS,D(RA),R"}, /* bcwp */
  { &test_psth_off16_R1, "psth off16_R1", "RS,D(RA),R"}, /* bcwp */
  { &test_psth_off32_R1, "psth off32_R1", "RS,D(RA),R"}, /* bcwp */
  { &test_pstq_off0_R1, "pstq off0_R1", "RSp,D(RA),R"}, /* bcwp */
  { &test_pstq_off8_R1, "pstq off8_R1", "RSp,D(RA),R"}, /* bcwp */
  { &test_pstq_off16_R1, "pstq off16_R1", "RSp,D(RA),R"}, /* bcwp */
  { &test_pstq_off32_R1, "pstq off32_R1", "RSp,D(RA),R"}, /* bcwp */
  { &test_pstq_off64_R1, "pstq off64_R1", "RSp,D(RA),R"}, /* bcwp */
  { &test_pstw_off0_R1, "pstw off0_R1", "RS,D(RA),R"}, /* bcwp */
  { &test_pstw_off8_R1, "pstw off8_R1", "RS,D(RA),R"}, /* bcwp */
  { &test_pstw_off16_R1, "pstw off16_R1", "RS,D(RA),R"}, /* bcwp */
  { &test_pstw_off32_R1, "pstw off32_R1", "RS,D(RA),R"}, /* bcwp */
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
