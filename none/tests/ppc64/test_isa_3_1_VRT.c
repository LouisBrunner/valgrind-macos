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
static void test_vmulesd (void) {
  __asm__ __volatile__ ("vmulesd %0, %1, %2"
				: "=v" (vrt) : "v" (vra), "v" (vrb) );
}
static void test_vmuleud (void) {
  __asm__ __volatile__ ("vmuleud %0, %1, %2"
				: "=v" (vrt) : "v" (vra), "v" (vrb) );
}
static void test_vmulosd (void) {
  __asm__ __volatile__ ("vmulosd %0, %1, %2"
				: "=v" (vrt) : "v" (vra), "v" (vrb) );
}
static void test_vmuloud (void) {
  __asm__ __volatile__ ("vmuloud %0, %1, %2"
				: "=v" (vrt) : "v" (vra), "v" (vrb) );
}
static void test_vextsd2q (void) {
  __asm__ __volatile__ ("vextsd2q %0, %1 " : "=v" (vrt) : "v" (vrb) );
}
static void test_vcmpequq (void) {
  __asm__ __volatile__ ("vcmpequq %0, %1, %2"
				: "=v" (vrt) : "v" (vra), "v" (vrb) );
}
static void test_dotted_vcmpequq (void) {
  SET_CR_ZERO;
  __asm__ __volatile__ ("vcmpequq. %0, %1, %2"
				: "=v" (vrt) : "v" (vra), "v" (vrb) );
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_vcmpgtsq (void) {
  __asm__ __volatile__ ("vcmpgtsq %0, %1, %2"
				: "=v" (vrt) : "v" (vra), "v" (vrb) );
}
static void test_dotted_vcmpgtsq (void) {
  SET_CR_ZERO;
  __asm__ __volatile__ ("vcmpgtsq. %0, %1, %2"
				: "=v" (vrt) : "v" (vra), "v" (vrb) );
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_vcmpgtuq (void) {
  __asm__ __volatile__ ("vcmpgtuq %0, %1, %2"
				: "=v" (vrt) : "v" (vra), "v" (vrb) );
}
static void test_dotted_vcmpgtuq (void) {
  SET_CR_ZERO;
  __asm__ __volatile__ ("vcmpgtuq. %0, %1, %2"
				: "=v" (vrt) : "v" (vra), "v" (vrb) );
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_vrlq (void) {
  __asm__ __volatile__ ("vrlq %0, %1, %2" : "=v" (vrt) : "v" (vra), "v" (vrb) );
}
static void test_vrlqmi (void) {
  __asm__ __volatile__ ("vrlqmi %0, %1, %2"
				: "=v" (vrt) : "v" (vra), "v" (vrb) );
}
static void test_vrlqnm (void) {
  __asm__ __volatile__ ("vrlqnm %0, %1, %2"
				: "=v" (vrt) : "v" (vra), "v" (vrb) );
}
static void test_vslq (void) {
  __asm__ __volatile__ ("vslq %0, %1, %2" : "=v" (vrt) : "v" (vra), "v" (vrb) );
}
static void test_vsraq (void) {
  __asm__ __volatile__ ("vsraq %0, %1, %2"
				: "=v" (vrt) : "v" (vra), "v" (vrb) );
}
static void test_vsrq (void) {
  __asm__ __volatile__ ("vsrq %0, %1, %2" : "=v" (vrt) : "v" (vra), "v" (vrb) );
}
static void test_dcffixqq (void) {
SET_FPSCR_ZERO; 
  __asm__ __volatile__ ("dcffixqq 28, %0" :: "v" (vrb) );
GET_FPSCR(current_fpscr); 
}
static void test_dctfixqq (void) {
  __asm__ __volatile__ ("dctfixqq %0, 26" : "=v" (vrt) );
}
static void test_vdivesq (void) {
  __asm__ __volatile__ ("vdivesq %0, %1, %2"
				: "=v" (vrt) : "v" (vra), "v" (vrb) );
}
static void test_vdiveuq (void) {
  __asm__ __volatile__ ("vdiveuq %0, %1, %2"
				: "=v" (vrt) : "v" (vra), "v" (vrb) );
}
static void test_vdivsq (void) {
  __asm__ __volatile__ ("vdivsq %0, %1, %2"
				: "=v" (vrt) : "v" (vra), "v" (vrb) );
}
static void test_vdivuq (void) {
  __asm__ __volatile__ ("vdivuq %0, %1, %2"
				: "=v" (vrt) : "v" (vra), "v" (vrb) );
}
static void test_vmodsq (void) {
  __asm__ __volatile__ ("vmodsq %0, %1, %2"
				: "=v" (vrt) : "v" (vra), "v" (vrb) );
}
static void test_vmoduq (void) {
  __asm__ __volatile__ ("vmoduq %0, %1, %2"
				: "=v" (vrt) : "v" (vra), "v" (vrb) );
}
static void test_vmsumcud (void) {
  __asm__ __volatile__ ("vmsumcud %0, %1, %2, %3"
			: "=v" (vrt) : "v" (vra), "v" (vrb), "v" (vrc) );
}
static void test_xscvqpsqz (void) {
  __asm__ __volatile__ ("xscvqpsqz %0, %1 " : "=v" (vrt) : "v" (vrb) );
}
static void test_xscvqpuqz (void) {
  __asm__ __volatile__ ("xscvqpuqz %0, %1 " : "=v" (vrt) : "v" (vrb) );
}
static void test_xscvsqqp (void) {
SET_FPSCR_ZERO; 
  __asm__ __volatile__ ("xscvsqqp %0, %1 " : "=v" (vrt) : "v" (vrb) );
GET_FPSCR(current_fpscr); 
}
static void test_xscvuqqp (void) {
SET_FPSCR_ZERO; 
  __asm__ __volatile__ ("xscvuqqp %0, %1 " : "=v" (vrt) : "v" (vrb) );
GET_FPSCR(current_fpscr); 
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
static void test_vcfuged (void) {
  __asm__ __volatile__ ("vcfuged %0, %1, %2"
				: "=v" (vrt) : "v" (vra), "v" (vrb) );
}
static void test_vclzdm (void) {
  __asm__ __volatile__ ("vclzdm %0, %1, %2"
				: "=v" (vrt) : "v" (vra), "v" (vrb) );
}
static void test_vctzdm (void) {
  __asm__ __volatile__ ("vctzdm %0, %1, %2"
				: "=v" (vrt) : "v" (vra), "v" (vrb) );
}
static void test_vpdepd (void) {
  __asm__ __volatile__ ("vpdepd %0, %1, %2"
				: "=v" (vrt) : "v" (vra), "v" (vrb) );
}
static void test_vpextd (void) {
  __asm__ __volatile__ ("vpextd %0, %1, %2"
				: "=v" (vrt) : "v" (vra), "v" (vrb) );
}
static void test_vclrlb (void) {
  __asm__ __volatile__ ("vclrlb %0, %1, %2"
				: "=v" (vrt) : "v" (vra), "r" (rb) );
}
static void test_vclrrb (void) {
  __asm__ __volatile__ ("vclrrb %0, %1, %2"
				: "=v" (vrt) : "v" (vra), "r" (rb) );
}
static void test_vstribl (void) {
  __asm__ __volatile__ ("vstribl %0, %1 " : "=v" (vrt) : "v" (vrb) );
}
static void test_dotted_vstribl (void) {
  SET_CR_ZERO;
  __asm__ __volatile__ ("vstribl. %0, %1 " : "=v" (vrt) : "v" (vrb) );
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_vstribr (void) {
  __asm__ __volatile__ ("vstribr %0, %1 " : "=v" (vrt) : "v" (vrb) );
}
static void test_dotted_vstribr (void) {
  SET_CR_ZERO;
  __asm__ __volatile__ ("vstribr. %0, %1 " : "=v" (vrt) : "v" (vrb) );
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_vstrihl (void) {
  __asm__ __volatile__ ("vstrihl %0, %1 " : "=v" (vrt) : "v" (vrb) );
}
static void test_dotted_vstrihl (void) {
  SET_CR_ZERO;
  __asm__ __volatile__ ("vstrihl. %0, %1 " : "=v" (vrt) : "v" (vrb) );
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_vstrihr (void) {
  __asm__ __volatile__ ("vstrihr %0, %1 " : "=v" (vrt) : "v" (vrb) );
}
static void test_dotted_vstrihr (void) {
  SET_CR_ZERO;
  __asm__ __volatile__ ("vstrihr. %0, %1 " : "=v" (vrt) : "v" (vrb) );
  GET_CR(current_cr); SET_CR_ZERO;
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
static void test_mtvsrbm (void) {
  __asm__ __volatile__ ("mtvsrbm %0, %1" : "=v" (vrt) : "r" (rb) );
}
static void test_mtvsrhm (void) {
  __asm__ __volatile__ ("mtvsrhm %0, %1" : "=v" (vrt) : "r" (rb) );
}
static void test_mtvsrwm (void) {
  __asm__ __volatile__ ("mtvsrwm %0, %1" : "=v" (vrt) : "r" (rb) );
}
static void test_mtvsrdm (void) {
  __asm__ __volatile__ ("mtvsrdm %0, %1" : "=v" (vrt) : "r" (rb) );
}
static void test_mtvsrqm (void) {
  __asm__ __volatile__ ("mtvsrqm %0, %1" : "=v" (vrt) : "r" (rb) );
}
static void test_mtvsrbmi_0 (void) {
  __asm__ __volatile__ ("mtvsrbmi %0, 0" : "=v" (vrt) );
}
static void test_mtvsrbmi_3 (void) {
  __asm__ __volatile__ ("mtvsrbmi %0, 3" : "=v" (vrt) );
}
static void test_mtvsrbmi_7 (void) {
  __asm__ __volatile__ ("mtvsrbmi %0, 7" : "=v" (vrt) );
}
static void test_vexpandbm (void) {
  __asm__ __volatile__ ("vexpandbm %0, %1 " : "=v" (vrt) : "v" (vrb) );
}
static void test_vexpandhm (void) {
  __asm__ __volatile__ ("vexpandhm %0, %1 " : "=v" (vrt) : "v" (vrb) );
}
static void test_vexpandwm (void) {
  __asm__ __volatile__ ("vexpandwm %0, %1 " : "=v" (vrt) : "v" (vrb) );
}
static void test_vexpanddm (void) {
  __asm__ __volatile__ ("vexpanddm %0, %1 " : "=v" (vrt) : "v" (vrb) );
}
static void test_vexpandqm (void) {
  __asm__ __volatile__ ("vexpandqm %0, %1 " : "=v" (vrt) : "v" (vrb) );
}

static test_list_t testgroup_generic[] = {
  { &test_dcffixqq, "dcffixqq", "FRTp,VRB"}, /* bcs */
  { &test_dctfixqq, "dctfixqq", "VRT,FRBp"}, /* bcs */
  { &test_dotted_vcmpequq, "vcmpequq.", "VRT,VRA,VRB"}, /* bcs */
  { &test_dotted_vcmpgtsq, "vcmpgtsq.", "VRT,VRA,VRB"}, /* bcs */
  { &test_dotted_vcmpgtuq, "vcmpgtuq.", "VRT,VRA,VRB"}, /* bcs */
  { &test_dotted_vstribl, "vstribl.", "VRT,VRB"}, /* bcs */
  { &test_dotted_vstribr, "vstribr.", "VRT,VRB"}, /* bcs */
  { &test_dotted_vstrihl, "vstrihl.", "VRT,VRB"}, /* bcs */
  { &test_dotted_vstrihr, "vstrihr.", "VRT,VRB"}, /* bcs */
  { &test_mtvsrbmi_0, "mtvsrbmi 0", "VRT,bm"}, /* bcwp */
  { &test_mtvsrbmi_3, "mtvsrbmi 3", "VRT,bm"}, /* bcwp */
  { &test_mtvsrbmi_7, "mtvsrbmi 7", "VRT,bm"}, /* bcwp */
  { &test_mtvsrbm, "mtvsrbm", "VRT,RB"}, /* bcs */
  { &test_mtvsrdm, "mtvsrdm", "VRT,RB"}, /* bcs */
  { &test_mtvsrhm, "mtvsrhm", "VRT,RB"}, /* bcs */
  { &test_mtvsrqm, "mtvsrqm", "VRT,RB"}, /* bcs */
  { &test_mtvsrwm, "mtvsrwm", "VRT,RB"}, /* bcs */
  { &test_vcfuged, "vcfuged", "VRT,VRA,VRB"}, /* bcs */
  { &test_vclrlb, "vclrlb", "VRT,VRA,RB"}, /* bcs */
  { &test_vclrrb, "vclrrb", "VRT,VRA,RB"}, /* bcs */
  { &test_vclzdm, "vclzdm", "VRT,VRA,VRB"}, /* bcs */
  { &test_vcmpequq, "vcmpequq", "VRT,VRA,VRB"}, /* bcs */
  { &test_vcmpgtsq, "vcmpgtsq", "VRT,VRA,VRB"}, /* bcs */
  { &test_vcmpgtuq, "vcmpgtuq", "VRT,VRA,VRB"}, /* bcs */
  { &test_vctzdm, "vctzdm", "VRT,VRA,VRB"}, /* bcs */
  { &test_vdivesd, "vdivesd", "VRT,VRA,VRB"}, /* bcs */
  { &test_vdivesq, "vdivesq", "VRT,VRA,VRB"}, /* bcs */
  { &test_vdivesw, "vdivesw", "VRT,VRA,VRB"}, /* bcs */
  { &test_vdiveud, "vdiveud", "VRT,VRA,VRB"}, /* bcs */
  { &test_vdiveuq, "vdiveuq", "VRT,VRA,VRB"}, /* bcs */
  { &test_vdiveuw, "vdiveuw", "VRT,VRA,VRB"}, /* bcs */
  { &test_vdivsd, "vdivsd", "VRT,VRA,VRB"}, /* bcs */
  { &test_vdivsq, "vdivsq", "VRT,VRA,VRB"}, /* bcs */
  { &test_vdivsw, "vdivsw", "VRT,VRA,VRB"}, /* bcs */
  { &test_vdivud, "vdivud", "VRT,VRA,VRB"}, /* bcs */
  { &test_vdivuq, "vdivuq", "VRT,VRA,VRB"}, /* bcs */
  { &test_vdivuw, "vdivuw", "VRT,VRA,VRB"}, /* bcs */
  { &test_vexpandbm, "vexpandbm", "VRT,VRB"}, /* bcs */
  { &test_vexpanddm, "vexpanddm", "VRT,VRB"}, /* bcs */
  { &test_vexpandhm, "vexpandhm", "VRT,VRB"}, /* bcs */
  { &test_vexpandqm, "vexpandqm", "VRT,VRB"}, /* bcs */
  { &test_vexpandwm, "vexpandwm", "VRT,VRB"}, /* bcs */
  { &test_vextddvlx, "vextddvlx", "VRT,VRA,VRB,RC"}, /* bcs */
  { &test_vextddvrx, "vextddvrx", "VRT,VRA,VRB,RC"}, /* bcs */
  { &test_vextdubvlx, "vextdubvlx", "VRT,VRA,VRB,RC"}, /* bcs */
  { &test_vextdubvrx, "vextdubvrx", "VRT,VRA,VRB,RC"}, /* bcs */
  { &test_vextduhvlx, "vextduhvlx", "VRT,VRA,VRB,RC"}, /* bcs */
  { &test_vextduhvrx, "vextduhvrx", "VRT,VRA,VRB,RC"}, /* bcs */
  { &test_vextduwvlx, "vextduwvlx", "VRT,VRA,VRB,RC"}, /* bcs */
  { &test_vextduwvrx, "vextduwvrx", "VRT,VRA,VRB,RC"}, /* bcs */
  { &test_vextsd2q, "vextsd2q", "VRT,VRB"}, /* bcs */
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
  { &test_vmodsq, "vmodsq", "VRT,VRA,VRB"}, /* bcs */
  { &test_vmodsw, "vmodsw", "VRT,VRA,VRB"}, /* bcs */
  { &test_vmodud, "vmodud", "VRT,VRA,VRB"}, /* bcs */
  { &test_vmoduq, "vmoduq", "VRT,VRA,VRB"}, /* bcs */
  { &test_vmoduw, "vmoduw", "VRT,VRA,VRB"}, /* bcs */
  { &test_vmsumcud, "vmsumcud", "VRT,VRA,VRB,VRC"}, /* bcs */
  { &test_vmulesd, "vmulesd", "VRT,VRA,VRB"}, /* bcs */
  { &test_vmuleud, "vmuleud", "VRT,VRA,VRB"}, /* bcs */
  { &test_vmulhsd, "vmulhsd", "VRT,VRA,VRB"}, /* bcs */
  { &test_vmulhsw, "vmulhsw", "VRT,VRA,VRB"}, /* bcs */
  { &test_vmulhud, "vmulhud", "VRT,VRA,VRB"}, /* bcs */
  { &test_vmulhuw, "vmulhuw", "VRT,VRA,VRB"}, /* bcs */
  { &test_vmulld, "vmulld", "VRT,VRA,VRB"}, /* bcs */
  { &test_vmulosd, "vmulosd", "VRT,VRA,VRB"}, /* bcs */
  { &test_vmuloud, "vmuloud", "VRT,VRA,VRB"}, /* bcs */
  { &test_vpdepd, "vpdepd", "VRT,VRA,VRB"}, /* bcs */
  { &test_vpextd, "vpextd", "VRT,VRA,VRB"}, /* bcs */
  { &test_vrlqmi, "vrlqmi", "VRT,VRA,VRB"}, /* bcs */
  { &test_vrlqnm, "vrlqnm", "VRT,VRA,VRB"}, /* bcs */
  { &test_vrlq, "vrlq", "VRT,VRA,VRB"}, /* bcs */
  { &test_vsldbi_0, "vsldbi 0", "VRT,VRA,VRB,SH"}, /* bcwp */
  { &test_vsldbi_4, "vsldbi 4", "VRT,VRA,VRB,SH"}, /* bcwp */
  { &test_vslq, "vslq", "VRT,VRA,VRB"}, /* bcs */
  { &test_vsraq, "vsraq", "VRT,VRA,VRB"}, /* bcs */
  { &test_vsrdbi_0, "vsrdbi 0", "VRT,VRA,VRB,SH"}, /* bcwp */
  { &test_vsrdbi_4, "vsrdbi 4", "VRT,VRA,VRB,SH"}, /* bcwp */
  { &test_vsrq, "vsrq", "VRT,VRA,VRB"}, /* bcs */
  { &test_vstribl, "vstribl", "VRT,VRB"}, /* bcs */
  { &test_vstribr, "vstribr", "VRT,VRB"}, /* bcs */
  { &test_vstrihl, "vstrihl", "VRT,VRB"}, /* bcs */
  { &test_vstrihr, "vstrihr", "VRT,VRB"}, /* bcs */
  { &test_xscmpeqqp, "xscmpeqqp", "VRT,VRA,VRB"}, /* bcs */
  { &test_xscmpgeqp, "xscmpgeqp", "VRT,VRA,VRB"}, /* bcs */
  { &test_xscmpgtqp, "xscmpgtqp", "VRT,VRA,VRB"}, /* bcs */
  { &test_xscvqpsqz, "xscvqpsqz", "VRT,VRB"}, /* bcs */
  { &test_xscvqpuqz, "xscvqpuqz", "VRT,VRB"}, /* bcs */
  { &test_xscvsqqp, "xscvsqqp", "VRT,VRB"}, /* bcs */
  { &test_xscvuqqp, "xscvuqqp", "VRT,VRB"}, /* bcs */
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
