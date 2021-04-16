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

static void test_xxmfacc (void) {
  __asm__ __volatile__ ("xxmfacc 4");
}
static void test_xxmtacc (void) {
  __asm__ __volatile__ ("xxmtacc 4");
}
static void test_xxsetaccz (void) {
  __asm__ __volatile__ ("xxsetaccz 4");
}
static void test_xvi4ger8 (void) {
  __asm__ __volatile__ ("xvi4ger8 4, %x0, %x1" :: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_xvi4ger8pp (void) {
  __asm__ __volatile__ ("xvi4ger8pp 4, %x0, %x1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi4ger8_XM0_YM0_PM0 (void) {
  __asm__ __volatile__ ("pmxvi4ger8  4, %x0, %x1, 0, 0, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi4ger8_XM0_YM0_PM45 (void) {
  __asm__ __volatile__ ("pmxvi4ger8  4, %x0, %x1, 0, 0, 45"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi4ger8_XM0_YM1_PM0 (void) {
  __asm__ __volatile__ ("pmxvi4ger8  4, %x0, %x1, 0, 1, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi4ger8_XM0_YM1_PM45 (void) {
  __asm__ __volatile__ ("pmxvi4ger8  4, %x0, %x1, 0, 1, 45"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi4ger8_XM11_YM0_PM0 (void) {
  __asm__ __volatile__ ("pmxvi4ger8  4, %x0, %x1, 11, 0, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi4ger8_XM11_YM0_PM45 (void) {
  __asm__ __volatile__ ("pmxvi4ger8  4, %x0, %x1, 11, 0, 45"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi4ger8_XM11_YM1_PM0 (void) {
  __asm__ __volatile__ ("pmxvi4ger8  4, %x0, %x1, 11, 1, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi4ger8_XM11_YM1_PM45 (void) {
  __asm__ __volatile__ ("pmxvi4ger8  4, %x0, %x1, 11, 1, 45"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi4ger8pp_XM0_YM0_PM0 (void) {
  __asm__ __volatile__ ("pmxvi4ger8pp  4, %x0, %x1, 0, 0, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi4ger8pp_XM0_YM0_PM45 (void) {
  __asm__ __volatile__ ("pmxvi4ger8pp  4, %x0, %x1, 0, 0, 45"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi4ger8pp_XM0_YM1_PM0 (void) {
  __asm__ __volatile__ ("pmxvi4ger8pp  4, %x0, %x1, 0, 1, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi4ger8pp_XM0_YM1_PM45 (void) {
  __asm__ __volatile__ ("pmxvi4ger8pp  4, %x0, %x1, 0, 1, 45"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi4ger8pp_XM11_YM0_PM0 (void) {
  __asm__ __volatile__ ("pmxvi4ger8pp  4, %x0, %x1, 11, 0, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi4ger8pp_XM11_YM0_PM45 (void) {
  __asm__ __volatile__ ("pmxvi4ger8pp  4, %x0, %x1, 11, 0, 45"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi4ger8pp_XM11_YM1_PM0 (void) {
  __asm__ __volatile__ ("pmxvi4ger8pp  4, %x0, %x1, 11, 1, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi4ger8pp_XM11_YM1_PM45 (void) {
  __asm__ __volatile__ ("pmxvi4ger8pp  4, %x0, %x1, 11, 1, 45"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_xvi8ger4 (void) {
  __asm__ __volatile__ ("xvi8ger4 4, %x0, %x1" :: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_xvi8ger4pp (void) {
  __asm__ __volatile__ ("xvi8ger4pp 4, %x0, %x1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi8ger4_XM0_YM0_PM0 (void) {
  __asm__ __volatile__ ("pmxvi8ger4  4, %x0, %x1, 0, 0, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi8ger4_XM0_YM0_PM5 (void) {
  __asm__ __volatile__ ("pmxvi8ger4  4, %x0, %x1, 0, 0, 5"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi8ger4_XM0_YM13_PM0 (void) {
  __asm__ __volatile__ ("pmxvi8ger4  4, %x0, %x1, 0, 13, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi8ger4_XM0_YM13_PM5 (void) {
  __asm__ __volatile__ ("pmxvi8ger4  4, %x0, %x1, 0, 13, 5"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi8ger4_XM11_YM0_PM0 (void) {
  __asm__ __volatile__ ("pmxvi8ger4  4, %x0, %x1, 11, 0, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi8ger4_XM11_YM0_PM5 (void) {
  __asm__ __volatile__ ("pmxvi8ger4  4, %x0, %x1, 11, 0, 5"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi8ger4_XM11_YM13_PM0 (void) {
  __asm__ __volatile__ ("pmxvi8ger4  4, %x0, %x1, 11, 13, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi8ger4_XM11_YM13_PM5 (void) {
  __asm__ __volatile__ ("pmxvi8ger4  4, %x0, %x1, 11, 13, 5"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi8ger4pp_XM0_YM0_PM0 (void) {
  __asm__ __volatile__ ("pmxvi8ger4pp  4, %x0, %x1, 0, 0, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi8ger4pp_XM0_YM0_PM5 (void) {
  __asm__ __volatile__ ("pmxvi8ger4pp  4, %x0, %x1, 0, 0, 5"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi8ger4pp_XM0_YM13_PM0 (void) {
  __asm__ __volatile__ ("pmxvi8ger4pp  4, %x0, %x1, 0, 13, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi8ger4pp_XM0_YM13_PM5 (void) {
  __asm__ __volatile__ ("pmxvi8ger4pp  4, %x0, %x1, 0, 13, 5"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi8ger4pp_XM11_YM0_PM0 (void) {
  __asm__ __volatile__ ("pmxvi8ger4pp  4, %x0, %x1, 11, 0, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi8ger4pp_XM11_YM0_PM5 (void) {
  __asm__ __volatile__ ("pmxvi8ger4pp  4, %x0, %x1, 11, 0, 5"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi8ger4pp_XM11_YM13_PM0 (void) {
  __asm__ __volatile__ ("pmxvi8ger4pp  4, %x0, %x1, 11, 13, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi8ger4pp_XM11_YM13_PM5 (void) {
  __asm__ __volatile__ ("pmxvi8ger4pp  4, %x0, %x1, 11, 13, 5"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_xvi16ger2s (void) {
  __asm__ __volatile__ ("xvi16ger2s 4, %x0, %x1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_xvi16ger2spp (void) {
  __asm__ __volatile__ ("xvi16ger2spp 4, %x0, %x1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi16ger2s_XM0_YM0_PM0 (void) {
  __asm__ __volatile__ ("pmxvi16ger2s  4, %x0, %x1, 0, 0, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi16ger2s_XM0_YM0_PM1 (void) {
  __asm__ __volatile__ ("pmxvi16ger2s  4, %x0, %x1, 0, 0, 1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi16ger2s_XM0_YM13_PM0 (void) {
  __asm__ __volatile__ ("pmxvi16ger2s  4, %x0, %x1, 0, 13, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi16ger2s_XM0_YM13_PM1 (void) {
  __asm__ __volatile__ ("pmxvi16ger2s  4, %x0, %x1, 0, 13, 1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi16ger2s_XM11_YM0_PM0 (void) {
  __asm__ __volatile__ ("pmxvi16ger2s  4, %x0, %x1, 11, 0, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi16ger2s_XM11_YM0_PM1 (void) {
  __asm__ __volatile__ ("pmxvi16ger2s  4, %x0, %x1, 11, 0, 1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi16ger2s_XM11_YM13_PM0 (void) {
  __asm__ __volatile__ ("pmxvi16ger2s  4, %x0, %x1, 11, 13, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi16ger2s_XM11_YM13_PM1 (void) {
  __asm__ __volatile__ ("pmxvi16ger2s  4, %x0, %x1, 11, 13, 1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi16ger2spp_XM0_YM0_PM0 (void) {
  __asm__ __volatile__ ("pmxvi16ger2spp  4, %x0, %x1, 0, 0, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi16ger2spp_XM0_YM0_PM1 (void) {
  __asm__ __volatile__ ("pmxvi16ger2spp  4, %x0, %x1, 0, 0, 1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi16ger2spp_XM0_YM13_PM0 (void) {
  __asm__ __volatile__ ("pmxvi16ger2spp  4, %x0, %x1, 0, 13, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi16ger2spp_XM0_YM13_PM1 (void) {
  __asm__ __volatile__ ("pmxvi16ger2spp  4, %x0, %x1, 0, 13, 1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi16ger2spp_XM11_YM0_PM0 (void) {
  __asm__ __volatile__ ("pmxvi16ger2spp  4, %x0, %x1, 11, 0, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi16ger2spp_XM11_YM0_PM1 (void) {
  __asm__ __volatile__ ("pmxvi16ger2spp  4, %x0, %x1, 11, 0, 1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi16ger2spp_XM11_YM13_PM0 (void) {
  __asm__ __volatile__ ("pmxvi16ger2spp  4, %x0, %x1, 11, 13, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi16ger2spp_XM11_YM13_PM1 (void) {
  __asm__ __volatile__ ("pmxvi16ger2spp  4, %x0, %x1, 11, 13, 1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_xvf16ger2 (void) {
  __asm__ __volatile__ ("xvf16ger2 4, %x0, %x1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_xvf16ger2pp (void) {
  __asm__ __volatile__ ("xvf16ger2pp 4, %x0, %x1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_xvf16ger2pn (void) {
  __asm__ __volatile__ ("xvf16ger2pn 4, %x0, %x1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_xvf16ger2np (void) {
  __asm__ __volatile__ ("xvf16ger2np 4, %x0, %x1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_xvf16ger2nn (void) {
  __asm__ __volatile__ ("xvf16ger2nn 4, %x0, %x1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvf16ger2_XM0_YM0_PM0 (void) {
  __asm__ __volatile__ ("pmxvf16ger2  4, %x0, %x1, 0, 0, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvf16ger2_XM0_YM0_PM1 (void) {
  __asm__ __volatile__ ("pmxvf16ger2  4, %x0, %x1, 0, 0, 1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvf16ger2_XM0_YM13_PM0 (void) {
  __asm__ __volatile__ ("pmxvf16ger2  4, %x0, %x1, 0, 13, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvf16ger2_XM0_YM13_PM1 (void) {
  __asm__ __volatile__ ("pmxvf16ger2  4, %x0, %x1, 0, 13, 1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvf16ger2_XM11_YM0_PM0 (void) {
  __asm__ __volatile__ ("pmxvf16ger2  4, %x0, %x1, 11, 0, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvf16ger2_XM11_YM0_PM1 (void) {
  __asm__ __volatile__ ("pmxvf16ger2  4, %x0, %x1, 11, 0, 1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvf16ger2_XM11_YM13_PM0 (void) {
  __asm__ __volatile__ ("pmxvf16ger2  4, %x0, %x1, 11, 13, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvf16ger2_XM11_YM13_PM1 (void) {
  __asm__ __volatile__ ("pmxvf16ger2  4, %x0, %x1, 11, 13, 1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvf16ger2pp_XM0_YM0_PM0 (void) {
  __asm__ __volatile__ ("pmxvf16ger2pp  4, %x0, %x1, 0, 0, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvf16ger2pp_XM0_YM0_PM1 (void) {
  __asm__ __volatile__ ("pmxvf16ger2pp  4, %x0, %x1, 0, 0, 1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvf16ger2pp_XM0_YM13_PM0 (void) {
  __asm__ __volatile__ ("pmxvf16ger2pp  4, %x0, %x1, 0, 13, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvf16ger2pp_XM0_YM13_PM1 (void) {
  __asm__ __volatile__ ("pmxvf16ger2pp  4, %x0, %x1, 0, 13, 1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvf16ger2pp_XM11_YM0_PM0 (void) {
  __asm__ __volatile__ ("pmxvf16ger2pp  4, %x0, %x1, 11, 0, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvf16ger2pp_XM11_YM0_PM1 (void) {
  __asm__ __volatile__ ("pmxvf16ger2pp  4, %x0, %x1, 11, 0, 1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvf16ger2pp_XM11_YM13_PM0 (void) {
  __asm__ __volatile__ ("pmxvf16ger2pp  4, %x0, %x1, 11, 13, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvf16ger2pp_XM11_YM13_PM1 (void) {
  __asm__ __volatile__ ("pmxvf16ger2pp  4, %x0, %x1, 11, 13, 1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvf16ger2pn_XM0_YM0_PM0 (void) {
  __asm__ __volatile__ ("pmxvf16ger2pn  4, %x0, %x1, 0, 0, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvf16ger2pn_XM0_YM0_PM1 (void) {
  __asm__ __volatile__ ("pmxvf16ger2pn  4, %x0, %x1, 0, 0, 1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvf16ger2pn_XM0_YM13_PM0 (void) {
  __asm__ __volatile__ ("pmxvf16ger2pn  4, %x0, %x1, 0, 13, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvf16ger2pn_XM0_YM13_PM1 (void) {
  __asm__ __volatile__ ("pmxvf16ger2pn  4, %x0, %x1, 0, 13, 1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvf16ger2pn_XM11_YM0_PM0 (void) {
  __asm__ __volatile__ ("pmxvf16ger2pn  4, %x0, %x1, 11, 0, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvf16ger2pn_XM11_YM0_PM1 (void) {
  __asm__ __volatile__ ("pmxvf16ger2pn  4, %x0, %x1, 11, 0, 1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvf16ger2pn_XM11_YM13_PM0 (void) {
  __asm__ __volatile__ ("pmxvf16ger2pn  4, %x0, %x1, 11, 13, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvf16ger2pn_XM11_YM13_PM1 (void) {
  __asm__ __volatile__ ("pmxvf16ger2pn  4, %x0, %x1, 11, 13, 1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvf16ger2np_XM0_YM0_PM0 (void) {
  __asm__ __volatile__ ("pmxvf16ger2np  4, %x0, %x1, 0, 0, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvf16ger2np_XM0_YM0_PM1 (void) {
  __asm__ __volatile__ ("pmxvf16ger2np  4, %x0, %x1, 0, 0, 1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvf16ger2np_XM0_YM13_PM0 (void) {
  __asm__ __volatile__ ("pmxvf16ger2np  4, %x0, %x1, 0, 13, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvf16ger2np_XM0_YM13_PM1 (void) {
  __asm__ __volatile__ ("pmxvf16ger2np  4, %x0, %x1, 0, 13, 1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvf16ger2np_XM11_YM0_PM0 (void) {
  __asm__ __volatile__ ("pmxvf16ger2np  4, %x0, %x1, 11, 0, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvf16ger2np_XM11_YM0_PM1 (void) {
  __asm__ __volatile__ ("pmxvf16ger2np  4, %x0, %x1, 11, 0, 1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvf16ger2np_XM11_YM13_PM0 (void) {
  __asm__ __volatile__ ("pmxvf16ger2np  4, %x0, %x1, 11, 13, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvf16ger2np_XM11_YM13_PM1 (void) {
  __asm__ __volatile__ ("pmxvf16ger2np  4, %x0, %x1, 11, 13, 1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvf16ger2nn_XM0_YM0_PM0 (void) {
  __asm__ __volatile__ ("pmxvf16ger2nn  4, %x0, %x1, 0, 0, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvf16ger2nn_XM0_YM0_PM1 (void) {
  __asm__ __volatile__ ("pmxvf16ger2nn  4, %x0, %x1, 0, 0, 1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvf16ger2nn_XM0_YM13_PM0 (void) {
  __asm__ __volatile__ ("pmxvf16ger2nn  4, %x0, %x1, 0, 13, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvf16ger2nn_XM0_YM13_PM1 (void) {
  __asm__ __volatile__ ("pmxvf16ger2nn  4, %x0, %x1, 0, 13, 1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvf16ger2nn_XM11_YM0_PM0 (void) {
  __asm__ __volatile__ ("pmxvf16ger2nn  4, %x0, %x1, 11, 0, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvf16ger2nn_XM11_YM0_PM1 (void) {
  __asm__ __volatile__ ("pmxvf16ger2nn  4, %x0, %x1, 11, 0, 1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvf16ger2nn_XM11_YM13_PM0 (void) {
  __asm__ __volatile__ ("pmxvf16ger2nn  4, %x0, %x1, 11, 13, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvf16ger2nn_XM11_YM13_PM1 (void) {
  __asm__ __volatile__ ("pmxvf16ger2nn  4, %x0, %x1, 11, 13, 1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_xvf32ger (void) {
  __asm__ __volatile__ ("xvf32ger 4, %x0, %x1" :: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_xvf32gerpp (void) {
  __asm__ __volatile__ ("xvf32gerpp 4, %x0, %x1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_xvf32gerpn (void) {
  __asm__ __volatile__ ("xvf32gerpn 4, %x0, %x1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_xvf32gernp (void) {
  __asm__ __volatile__ ("xvf32gernp 4, %x0, %x1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_xvf32gernn (void) {
  __asm__ __volatile__ ("xvf32gernn 4, %x0, %x1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvf32ger_XM0_YM0 (void) {
  __asm__ __volatile__ ("pmxvf32ger  4, %x0, %x1, 0, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvf32ger_XM0_YM13 (void) {
  __asm__ __volatile__ ("pmxvf32ger  4, %x0, %x1, 0, 13"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvf32ger_XM11_YM0 (void) {
  __asm__ __volatile__ ("pmxvf32ger  4, %x0, %x1, 11, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvf32ger_XM11_YM13 (void) {
  __asm__ __volatile__ ("pmxvf32ger  4, %x0, %x1, 11, 13"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvf32gerpp_XM0_YM0 (void) {
  __asm__ __volatile__ ("pmxvf32gerpp  4, %x0, %x1, 0, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvf32gerpp_XM0_YM13 (void) {
  __asm__ __volatile__ ("pmxvf32gerpp  4, %x0, %x1, 0, 13"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvf32gerpp_XM11_YM0 (void) {
  __asm__ __volatile__ ("pmxvf32gerpp  4, %x0, %x1, 11, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvf32gerpp_XM11_YM13 (void) {
  __asm__ __volatile__ ("pmxvf32gerpp  4, %x0, %x1, 11, 13"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvf32gerpn_XM0_YM0 (void) {
  __asm__ __volatile__ ("pmxvf32gerpn  4, %x0, %x1, 0, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvf32gerpn_XM0_YM13 (void) {
  __asm__ __volatile__ ("pmxvf32gerpn  4, %x0, %x1, 0, 13"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvf32gerpn_XM11_YM0 (void) {
  __asm__ __volatile__ ("pmxvf32gerpn  4, %x0, %x1, 11, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvf32gerpn_XM11_YM13 (void) {
  __asm__ __volatile__ ("pmxvf32gerpn  4, %x0, %x1, 11, 13"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvf32gernp_XM0_YM0 (void) {
  __asm__ __volatile__ ("pmxvf32gernp  4, %x0, %x1, 0, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvf32gernp_XM0_YM13 (void) {
  __asm__ __volatile__ ("pmxvf32gernp  4, %x0, %x1, 0, 13"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvf32gernp_XM11_YM0 (void) {
  __asm__ __volatile__ ("pmxvf32gernp  4, %x0, %x1, 11, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvf32gernp_XM11_YM13 (void) {
  __asm__ __volatile__ ("pmxvf32gernp  4, %x0, %x1, 11, 13"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvf32gernn_XM0_YM0 (void) {
  __asm__ __volatile__ ("pmxvf32gernn  4, %x0, %x1, 0, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvf32gernn_XM0_YM13 (void) {
  __asm__ __volatile__ ("pmxvf32gernn  4, %x0, %x1, 0, 13"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvf32gernn_XM11_YM0 (void) {
  __asm__ __volatile__ ("pmxvf32gernn  4, %x0, %x1, 11, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvf32gernn_XM11_YM13 (void) {
  __asm__ __volatile__ ("pmxvf32gernn  4, %x0, %x1, 11, 13"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_xvf64ger (void) {
  __asm__ __volatile__ ("xvf64ger 4, 22, %x0" :: "wa" (vec_xa) );
}
static void test_xvf64gerpp (void) {
  __asm__ __volatile__ ("xvf64gerpp 4, 22, %x0" :: "wa" (vec_xa) );
}
static void test_xvf64gerpn (void) {
  __asm__ __volatile__ ("xvf64gerpn 4, 22, %x0" :: "wa" (vec_xa) );
}
static void test_xvf64gernp (void) {
  __asm__ __volatile__ ("xvf64gernp 4, 22, %x0" :: "wa" (vec_xa) );
}
static void test_xvf64gernn (void) {
  __asm__ __volatile__ ("xvf64gernn 4, 22, %x0" :: "wa" (vec_xa) );
}
static void test_pmxvf64ger_XM0_YM0 (void) {
  __asm__ __volatile__ ("pmxvf64ger  4, 22, %x0, 0, 0" :: "wa" (vec_xa) );
}
static void test_pmxvf64ger_XM0_YM1 (void) {
  __asm__ __volatile__ ("pmxvf64ger  4, 22, %x0, 0, 1" :: "wa" (vec_xa) );
}
static void test_pmxvf64ger_XM11_YM0 (void) {
  __asm__ __volatile__ ("pmxvf64ger  4, 22, %x0, 11, 0" :: "wa" (vec_xa) );
}
static void test_pmxvf64ger_XM11_YM1 (void) {
  __asm__ __volatile__ ("pmxvf64ger  4, 22, %x0, 11, 1" :: "wa" (vec_xa) );
}
static void test_pmxvf64gerpp_XM0_YM0 (void) {
  __asm__ __volatile__ ("pmxvf64gerpp  4, 22, %x0, 0, 0" :: "wa" (vec_xa) );
}
static void test_pmxvf64gerpp_XM0_YM1 (void) {
  __asm__ __volatile__ ("pmxvf64gerpp  4, 22, %x0, 0, 1" :: "wa" (vec_xa) );
}
static void test_pmxvf64gerpp_XM11_YM0 (void) {
  __asm__ __volatile__ ("pmxvf64gerpp  4, 22, %x0, 11, 0" :: "wa" (vec_xa) );
}
static void test_pmxvf64gerpp_XM11_YM1 (void) {
  __asm__ __volatile__ ("pmxvf64gerpp  4, 22, %x0, 11, 1" :: "wa" (vec_xa) );
}
static void test_pmxvf64gerpn_XM0_YM0 (void) {
  __asm__ __volatile__ ("pmxvf64gerpn  4, 22, %x0, 0, 0" :: "wa" (vec_xa) );
}
static void test_pmxvf64gerpn_XM0_YM1 (void) {
  __asm__ __volatile__ ("pmxvf64gerpn  4, 22, %x0, 0, 1" :: "wa" (vec_xa) );
}
static void test_pmxvf64gerpn_XM11_YM0 (void) {
  __asm__ __volatile__ ("pmxvf64gerpn  4, 22, %x0, 11, 0" :: "wa" (vec_xa) );
}
static void test_pmxvf64gerpn_XM11_YM1 (void) {
  __asm__ __volatile__ ("pmxvf64gerpn  4, 22, %x0, 11, 1" :: "wa" (vec_xa) );
}
static void test_pmxvf64gernp_XM0_YM0 (void) {
  __asm__ __volatile__ ("pmxvf64gernp  4, 22, %x0, 0, 0" :: "wa" (vec_xa) );
}
static void test_pmxvf64gernp_XM0_YM1 (void) {
  __asm__ __volatile__ ("pmxvf64gernp  4, 22, %x0, 0, 1" :: "wa" (vec_xa) );
}
static void test_pmxvf64gernp_XM11_YM0 (void) {
  __asm__ __volatile__ ("pmxvf64gernp  4, 22, %x0, 11, 0" :: "wa" (vec_xa) );
}
static void test_pmxvf64gernp_XM11_YM1 (void) {
  __asm__ __volatile__ ("pmxvf64gernp  4, 22, %x0, 11, 1" :: "wa" (vec_xa) );
}
static void test_pmxvf64gernn_XM0_YM0 (void) {
  __asm__ __volatile__ ("pmxvf64gernn  4, 22, %x0, 0, 0" :: "wa" (vec_xa) );
}
static void test_pmxvf64gernn_XM0_YM1 (void) {
  __asm__ __volatile__ ("pmxvf64gernn  4, 22, %x0, 0, 1" :: "wa" (vec_xa) );
}
static void test_pmxvf64gernn_XM11_YM0 (void) {
  __asm__ __volatile__ ("pmxvf64gernn  4, 22, %x0, 11, 0" :: "wa" (vec_xa) );
}
static void test_pmxvf64gernn_XM11_YM1 (void) {
  __asm__ __volatile__ ("pmxvf64gernn  4, 22, %x0, 11, 1" :: "wa" (vec_xa) );
}
static void test_xvbf16ger2 (void) {
  __asm__ __volatile__ ("xvbf16ger2 4, %x0, %x1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_xvbf16ger2nn (void) {
  __asm__ __volatile__ ("xvbf16ger2nn 4, %x0, %x1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_xvbf16ger2np (void) {
  __asm__ __volatile__ ("xvbf16ger2np 4, %x0, %x1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_xvbf16ger2pn (void) {
  __asm__ __volatile__ ("xvbf16ger2pn 4, %x0, %x1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_xvbf16ger2pp (void) {
  __asm__ __volatile__ ("xvbf16ger2pp 4, %x0, %x1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvbf16ger2_XM0_YM0_PM0 (void) {
  __asm__ __volatile__ ("pmxvbf16ger2  4, %x0, %x1, 0, 0, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvbf16ger2_XM0_YM0_PM1 (void) {
  __asm__ __volatile__ ("pmxvbf16ger2  4, %x0, %x1, 0, 0, 1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvbf16ger2_XM0_YM13_PM0 (void) {
  __asm__ __volatile__ ("pmxvbf16ger2  4, %x0, %x1, 0, 13, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvbf16ger2_XM0_YM13_PM1 (void) {
  __asm__ __volatile__ ("pmxvbf16ger2  4, %x0, %x1, 0, 13, 1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvbf16ger2_XM11_YM0_PM0 (void) {
  __asm__ __volatile__ ("pmxvbf16ger2  4, %x0, %x1, 11, 0, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvbf16ger2_XM11_YM0_PM1 (void) {
  __asm__ __volatile__ ("pmxvbf16ger2  4, %x0, %x1, 11, 0, 1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvbf16ger2_XM11_YM13_PM0 (void) {
  __asm__ __volatile__ ("pmxvbf16ger2  4, %x0, %x1, 11, 13, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvbf16ger2_XM11_YM13_PM1 (void) {
  __asm__ __volatile__ ("pmxvbf16ger2  4, %x0, %x1, 11, 13, 1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvbf16ger2nn_XM0_YM0_PM0 (void) {
  __asm__ __volatile__ ("pmxvbf16ger2nn  4, %x0, %x1, 0, 0, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvbf16ger2nn_XM0_YM0_PM1 (void) {
  __asm__ __volatile__ ("pmxvbf16ger2nn  4, %x0, %x1, 0, 0, 1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvbf16ger2nn_XM0_YM13_PM0 (void) {
  __asm__ __volatile__ ("pmxvbf16ger2nn  4, %x0, %x1, 0, 13, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvbf16ger2nn_XM0_YM13_PM1 (void) {
  __asm__ __volatile__ ("pmxvbf16ger2nn  4, %x0, %x1, 0, 13, 1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvbf16ger2nn_XM11_YM0_PM0 (void) {
  __asm__ __volatile__ ("pmxvbf16ger2nn  4, %x0, %x1, 11, 0, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvbf16ger2nn_XM11_YM0_PM1 (void) {
  __asm__ __volatile__ ("pmxvbf16ger2nn  4, %x0, %x1, 11, 0, 1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvbf16ger2nn_XM11_YM13_PM0 (void) {
  __asm__ __volatile__ ("pmxvbf16ger2nn  4, %x0, %x1, 11, 13, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvbf16ger2nn_XM11_YM13_PM1 (void) {
  __asm__ __volatile__ ("pmxvbf16ger2nn  4, %x0, %x1, 11, 13, 1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvbf16ger2np_XM0_YM0_PM0 (void) {
  __asm__ __volatile__ ("pmxvbf16ger2np  4, %x0, %x1, 0, 0, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvbf16ger2np_XM0_YM0_PM1 (void) {
  __asm__ __volatile__ ("pmxvbf16ger2np  4, %x0, %x1, 0, 0, 1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvbf16ger2np_XM0_YM13_PM0 (void) {
  __asm__ __volatile__ ("pmxvbf16ger2np  4, %x0, %x1, 0, 13, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvbf16ger2np_XM0_YM13_PM1 (void) {
  __asm__ __volatile__ ("pmxvbf16ger2np  4, %x0, %x1, 0, 13, 1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvbf16ger2np_XM11_YM0_PM0 (void) {
  __asm__ __volatile__ ("pmxvbf16ger2np  4, %x0, %x1, 11, 0, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvbf16ger2np_XM11_YM0_PM1 (void) {
  __asm__ __volatile__ ("pmxvbf16ger2np  4, %x0, %x1, 11, 0, 1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvbf16ger2np_XM11_YM13_PM0 (void) {
  __asm__ __volatile__ ("pmxvbf16ger2np  4, %x0, %x1, 11, 13, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvbf16ger2np_XM11_YM13_PM1 (void) {
  __asm__ __volatile__ ("pmxvbf16ger2np  4, %x0, %x1, 11, 13, 1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvbf16ger2pn_XM0_YM0_PM0 (void) {
  __asm__ __volatile__ ("pmxvbf16ger2pn  4, %x0, %x1, 0, 0, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvbf16ger2pn_XM0_YM0_PM1 (void) {
  __asm__ __volatile__ ("pmxvbf16ger2pn  4, %x0, %x1, 0, 0, 1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvbf16ger2pn_XM0_YM13_PM0 (void) {
  __asm__ __volatile__ ("pmxvbf16ger2pn  4, %x0, %x1, 0, 13, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvbf16ger2pn_XM0_YM13_PM1 (void) {
  __asm__ __volatile__ ("pmxvbf16ger2pn  4, %x0, %x1, 0, 13, 1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvbf16ger2pn_XM11_YM0_PM0 (void) {
  __asm__ __volatile__ ("pmxvbf16ger2pn  4, %x0, %x1, 11, 0, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvbf16ger2pn_XM11_YM0_PM1 (void) {
  __asm__ __volatile__ ("pmxvbf16ger2pn  4, %x0, %x1, 11, 0, 1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvbf16ger2pn_XM11_YM13_PM0 (void) {
  __asm__ __volatile__ ("pmxvbf16ger2pn  4, %x0, %x1, 11, 13, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvbf16ger2pn_XM11_YM13_PM1 (void) {
  __asm__ __volatile__ ("pmxvbf16ger2pn  4, %x0, %x1, 11, 13, 1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvbf16ger2pp_XM0_YM0_PM0 (void) {
  __asm__ __volatile__ ("pmxvbf16ger2pp  4, %x0, %x1, 0, 0, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvbf16ger2pp_XM0_YM0_PM1 (void) {
  __asm__ __volatile__ ("pmxvbf16ger2pp  4, %x0, %x1, 0, 0, 1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvbf16ger2pp_XM0_YM13_PM0 (void) {
  __asm__ __volatile__ ("pmxvbf16ger2pp  4, %x0, %x1, 0, 13, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvbf16ger2pp_XM0_YM13_PM1 (void) {
  __asm__ __volatile__ ("pmxvbf16ger2pp  4, %x0, %x1, 0, 13, 1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvbf16ger2pp_XM11_YM0_PM0 (void) {
  __asm__ __volatile__ ("pmxvbf16ger2pp  4, %x0, %x1, 11, 0, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvbf16ger2pp_XM11_YM0_PM1 (void) {
  __asm__ __volatile__ ("pmxvbf16ger2pp  4, %x0, %x1, 11, 0, 1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvbf16ger2pp_XM11_YM13_PM0 (void) {
  __asm__ __volatile__ ("pmxvbf16ger2pp  4, %x0, %x1, 11, 13, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvbf16ger2pp_XM11_YM13_PM1 (void) {
  __asm__ __volatile__ ("pmxvbf16ger2pp  4, %x0, %x1, 11, 13, 1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_xvi8ger4spp (void) {
  __asm__ __volatile__ ("xvi8ger4spp 4, %x0, %x1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi8ger4spp_XM0_YM0_PM0 (void) {
  __asm__ __volatile__ ("pmxvi8ger4spp  4, %x0, %x1, 0, 0, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi8ger4spp_XM0_YM0_PM5 (void) {
  __asm__ __volatile__ ("pmxvi8ger4spp  4, %x0, %x1, 0, 0, 5"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi8ger4spp_XM0_YM13_PM0 (void) {
  __asm__ __volatile__ ("pmxvi8ger4spp  4, %x0, %x1, 0, 13, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi8ger4spp_XM0_YM13_PM5 (void) {
  __asm__ __volatile__ ("pmxvi8ger4spp  4, %x0, %x1, 0, 13, 5"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi8ger4spp_XM11_YM0_PM0 (void) {
  __asm__ __volatile__ ("pmxvi8ger4spp  4, %x0, %x1, 11, 0, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi8ger4spp_XM11_YM0_PM5 (void) {
  __asm__ __volatile__ ("pmxvi8ger4spp  4, %x0, %x1, 11, 0, 5"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi8ger4spp_XM11_YM13_PM0 (void) {
  __asm__ __volatile__ ("pmxvi8ger4spp  4, %x0, %x1, 11, 13, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi8ger4spp_XM11_YM13_PM5 (void) {
  __asm__ __volatile__ ("pmxvi8ger4spp  4, %x0, %x1, 11, 13, 5"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_xvi16ger2 (void) {
  __asm__ __volatile__ ("xvi16ger2 4, %x0, %x1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_xvi16ger2pp (void) {
  __asm__ __volatile__ ("xvi16ger2pp 4, %x0, %x1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi16ger2_XM0_YM0_PM0 (void) {
  __asm__ __volatile__ ("pmxvi16ger2  4, %x0, %x1, 0, 0, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi16ger2_XM0_YM0_PM1 (void) {
  __asm__ __volatile__ ("pmxvi16ger2  4, %x0, %x1, 0, 0, 1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi16ger2_XM0_YM13_PM0 (void) {
  __asm__ __volatile__ ("pmxvi16ger2  4, %x0, %x1, 0, 13, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi16ger2_XM0_YM13_PM1 (void) {
  __asm__ __volatile__ ("pmxvi16ger2  4, %x0, %x1, 0, 13, 1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi16ger2_XM11_YM0_PM0 (void) {
  __asm__ __volatile__ ("pmxvi16ger2  4, %x0, %x1, 11, 0, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi16ger2_XM11_YM0_PM1 (void) {
  __asm__ __volatile__ ("pmxvi16ger2  4, %x0, %x1, 11, 0, 1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi16ger2_XM11_YM13_PM0 (void) {
  __asm__ __volatile__ ("pmxvi16ger2  4, %x0, %x1, 11, 13, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi16ger2_XM11_YM13_PM1 (void) {
  __asm__ __volatile__ ("pmxvi16ger2  4, %x0, %x1, 11, 13, 1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi16ger2pp_XM0_YM0_PM0 (void) {
  __asm__ __volatile__ ("pmxvi16ger2pp  4, %x0, %x1, 0, 0, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi16ger2pp_XM0_YM0_PM1 (void) {
  __asm__ __volatile__ ("pmxvi16ger2pp  4, %x0, %x1, 0, 0, 1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi16ger2pp_XM0_YM13_PM0 (void) {
  __asm__ __volatile__ ("pmxvi16ger2pp  4, %x0, %x1, 0, 13, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi16ger2pp_XM0_YM13_PM1 (void) {
  __asm__ __volatile__ ("pmxvi16ger2pp  4, %x0, %x1, 0, 13, 1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi16ger2pp_XM11_YM0_PM0 (void) {
  __asm__ __volatile__ ("pmxvi16ger2pp  4, %x0, %x1, 11, 0, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi16ger2pp_XM11_YM0_PM1 (void) {
  __asm__ __volatile__ ("pmxvi16ger2pp  4, %x0, %x1, 11, 0, 1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi16ger2pp_XM11_YM13_PM0 (void) {
  __asm__ __volatile__ ("pmxvi16ger2pp  4, %x0, %x1, 11, 13, 0"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}
static void test_pmxvi16ger2pp_XM11_YM13_PM1 (void) {
  __asm__ __volatile__ ("pmxvi16ger2pp  4, %x0, %x1, 11, 13, 1"
				:: "wa" (vec_xa), "wa" (vec_xb) );
}

static test_list_t testgroup_generic[] = {
  { &test_pmxvbf16ger2nn_XM0_YM0_PM0, "pmxvbf16ger2nn XM0_YM0_PM0", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvbf16ger2nn_XM0_YM0_PM1, "pmxvbf16ger2nn XM0_YM0_PM1", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvbf16ger2nn_XM0_YM13_PM0, "pmxvbf16ger2nn XM0_YM13_PM0", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvbf16ger2nn_XM0_YM13_PM1, "pmxvbf16ger2nn XM0_YM13_PM1", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvbf16ger2nn_XM11_YM0_PM0, "pmxvbf16ger2nn XM11_YM0_PM0", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvbf16ger2nn_XM11_YM0_PM1, "pmxvbf16ger2nn XM11_YM0_PM1", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvbf16ger2nn_XM11_YM13_PM0, "pmxvbf16ger2nn XM11_YM13_PM0", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvbf16ger2nn_XM11_YM13_PM1, "pmxvbf16ger2nn XM11_YM13_PM1", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvbf16ger2np_XM0_YM0_PM0, "pmxvbf16ger2np XM0_YM0_PM0", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvbf16ger2np_XM0_YM0_PM1, "pmxvbf16ger2np XM0_YM0_PM1", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvbf16ger2np_XM0_YM13_PM0, "pmxvbf16ger2np XM0_YM13_PM0", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvbf16ger2np_XM0_YM13_PM1, "pmxvbf16ger2np XM0_YM13_PM1", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvbf16ger2np_XM11_YM0_PM0, "pmxvbf16ger2np XM11_YM0_PM0", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvbf16ger2np_XM11_YM0_PM1, "pmxvbf16ger2np XM11_YM0_PM1", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvbf16ger2np_XM11_YM13_PM0, "pmxvbf16ger2np XM11_YM13_PM0", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvbf16ger2np_XM11_YM13_PM1, "pmxvbf16ger2np XM11_YM13_PM1", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvbf16ger2pn_XM0_YM0_PM0, "pmxvbf16ger2pn XM0_YM0_PM0", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvbf16ger2pn_XM0_YM0_PM1, "pmxvbf16ger2pn XM0_YM0_PM1", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvbf16ger2pn_XM0_YM13_PM0, "pmxvbf16ger2pn XM0_YM13_PM0", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvbf16ger2pn_XM0_YM13_PM1, "pmxvbf16ger2pn XM0_YM13_PM1", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvbf16ger2pn_XM11_YM0_PM0, "pmxvbf16ger2pn XM11_YM0_PM0", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvbf16ger2pn_XM11_YM0_PM1, "pmxvbf16ger2pn XM11_YM0_PM1", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvbf16ger2pn_XM11_YM13_PM0, "pmxvbf16ger2pn XM11_YM13_PM0", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvbf16ger2pn_XM11_YM13_PM1, "pmxvbf16ger2pn XM11_YM13_PM1", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvbf16ger2pp_XM0_YM0_PM0, "pmxvbf16ger2pp XM0_YM0_PM0", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvbf16ger2pp_XM0_YM0_PM1, "pmxvbf16ger2pp XM0_YM0_PM1", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvbf16ger2pp_XM0_YM13_PM0, "pmxvbf16ger2pp XM0_YM13_PM0", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvbf16ger2pp_XM0_YM13_PM1, "pmxvbf16ger2pp XM0_YM13_PM1", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvbf16ger2pp_XM11_YM0_PM0, "pmxvbf16ger2pp XM11_YM0_PM0", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvbf16ger2pp_XM11_YM0_PM1, "pmxvbf16ger2pp XM11_YM0_PM1", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvbf16ger2pp_XM11_YM13_PM0, "pmxvbf16ger2pp XM11_YM13_PM0", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvbf16ger2pp_XM11_YM13_PM1, "pmxvbf16ger2pp XM11_YM13_PM1", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvbf16ger2_XM0_YM0_PM0, "pmxvbf16ger2 XM0_YM0_PM0", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvbf16ger2_XM0_YM0_PM1, "pmxvbf16ger2 XM0_YM0_PM1", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvbf16ger2_XM0_YM13_PM0, "pmxvbf16ger2 XM0_YM13_PM0", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvbf16ger2_XM0_YM13_PM1, "pmxvbf16ger2 XM0_YM13_PM1", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvbf16ger2_XM11_YM0_PM0, "pmxvbf16ger2 XM11_YM0_PM0", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvbf16ger2_XM11_YM0_PM1, "pmxvbf16ger2 XM11_YM0_PM1", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvbf16ger2_XM11_YM13_PM0, "pmxvbf16ger2 XM11_YM13_PM0", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvbf16ger2_XM11_YM13_PM1, "pmxvbf16ger2 XM11_YM13_PM1", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvf16ger2nn_XM0_YM0_PM0, "pmxvf16ger2nn XM0_YM0_PM0", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvf16ger2nn_XM0_YM0_PM1, "pmxvf16ger2nn XM0_YM0_PM1", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvf16ger2nn_XM0_YM13_PM0, "pmxvf16ger2nn XM0_YM13_PM0", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvf16ger2nn_XM0_YM13_PM1, "pmxvf16ger2nn XM0_YM13_PM1", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvf16ger2nn_XM11_YM0_PM0, "pmxvf16ger2nn XM11_YM0_PM0", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvf16ger2nn_XM11_YM0_PM1, "pmxvf16ger2nn XM11_YM0_PM1", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvf16ger2nn_XM11_YM13_PM0, "pmxvf16ger2nn XM11_YM13_PM0", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvf16ger2nn_XM11_YM13_PM1, "pmxvf16ger2nn XM11_YM13_PM1", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvf16ger2np_XM0_YM0_PM0, "pmxvf16ger2np XM0_YM0_PM0", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvf16ger2np_XM0_YM0_PM1, "pmxvf16ger2np XM0_YM0_PM1", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvf16ger2np_XM0_YM13_PM0, "pmxvf16ger2np XM0_YM13_PM0", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvf16ger2np_XM0_YM13_PM1, "pmxvf16ger2np XM0_YM13_PM1", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvf16ger2np_XM11_YM0_PM0, "pmxvf16ger2np XM11_YM0_PM0", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvf16ger2np_XM11_YM0_PM1, "pmxvf16ger2np XM11_YM0_PM1", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvf16ger2np_XM11_YM13_PM0, "pmxvf16ger2np XM11_YM13_PM0", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvf16ger2np_XM11_YM13_PM1, "pmxvf16ger2np XM11_YM13_PM1", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvf16ger2pn_XM0_YM0_PM0, "pmxvf16ger2pn XM0_YM0_PM0", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvf16ger2pn_XM0_YM0_PM1, "pmxvf16ger2pn XM0_YM0_PM1", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvf16ger2pn_XM0_YM13_PM0, "pmxvf16ger2pn XM0_YM13_PM0", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvf16ger2pn_XM0_YM13_PM1, "pmxvf16ger2pn XM0_YM13_PM1", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvf16ger2pn_XM11_YM0_PM0, "pmxvf16ger2pn XM11_YM0_PM0", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvf16ger2pn_XM11_YM0_PM1, "pmxvf16ger2pn XM11_YM0_PM1", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvf16ger2pn_XM11_YM13_PM0, "pmxvf16ger2pn XM11_YM13_PM0", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvf16ger2pn_XM11_YM13_PM1, "pmxvf16ger2pn XM11_YM13_PM1", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvf16ger2pp_XM0_YM0_PM0, "pmxvf16ger2pp XM0_YM0_PM0", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvf16ger2pp_XM0_YM0_PM1, "pmxvf16ger2pp XM0_YM0_PM1", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvf16ger2pp_XM0_YM13_PM0, "pmxvf16ger2pp XM0_YM13_PM0", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvf16ger2pp_XM0_YM13_PM1, "pmxvf16ger2pp XM0_YM13_PM1", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvf16ger2pp_XM11_YM0_PM0, "pmxvf16ger2pp XM11_YM0_PM0", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvf16ger2pp_XM11_YM0_PM1, "pmxvf16ger2pp XM11_YM0_PM1", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvf16ger2pp_XM11_YM13_PM0, "pmxvf16ger2pp XM11_YM13_PM0", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvf16ger2pp_XM11_YM13_PM1, "pmxvf16ger2pp XM11_YM13_PM1", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvf16ger2_XM0_YM0_PM0, "pmxvf16ger2 XM0_YM0_PM0", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvf16ger2_XM0_YM0_PM1, "pmxvf16ger2 XM0_YM0_PM1", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvf16ger2_XM0_YM13_PM0, "pmxvf16ger2 XM0_YM13_PM0", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvf16ger2_XM0_YM13_PM1, "pmxvf16ger2 XM0_YM13_PM1", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvf16ger2_XM11_YM0_PM0, "pmxvf16ger2 XM11_YM0_PM0", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvf16ger2_XM11_YM0_PM1, "pmxvf16ger2 XM11_YM0_PM1", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvf16ger2_XM11_YM13_PM0, "pmxvf16ger2 XM11_YM13_PM0", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvf16ger2_XM11_YM13_PM1, "pmxvf16ger2 XM11_YM13_PM1", "AT,XA,XB,XMSK,YMSK,PMSK", 0b00001111}, /* bcwp */
  { &test_pmxvf32gernn_XM0_YM0, "pmxvf32gernn XM0_YM0", "AT,XA,XB,XMSK,YMSK", 0b00001111}, /* bcwp */
  { &test_pmxvf32gernn_XM0_YM13, "pmxvf32gernn XM0_YM13", "AT,XA,XB,XMSK,YMSK", 0b00001111}, /* bcwp */
  { &test_pmxvf32gernn_XM11_YM0, "pmxvf32gernn XM11_YM0", "AT,XA,XB,XMSK,YMSK", 0b00001111}, /* bcwp */
  { &test_pmxvf32gernn_XM11_YM13, "pmxvf32gernn XM11_YM13", "AT,XA,XB,XMSK,YMSK", 0b00001111}, /* bcwp */
  { &test_pmxvf32gernp_XM0_YM0, "pmxvf32gernp XM0_YM0", "AT,XA,XB,XMSK,YMSK", 0b00001111}, /* bcwp */
  { &test_pmxvf32gernp_XM0_YM13, "pmxvf32gernp XM0_YM13", "AT,XA,XB,XMSK,YMSK", 0b00001111}, /* bcwp */
  { &test_pmxvf32gernp_XM11_YM0, "pmxvf32gernp XM11_YM0", "AT,XA,XB,XMSK,YMSK", 0b00001111}, /* bcwp */
  { &test_pmxvf32gernp_XM11_YM13, "pmxvf32gernp XM11_YM13", "AT,XA,XB,XMSK,YMSK", 0b00001111}, /* bcwp */
  { &test_pmxvf32gerpn_XM0_YM0, "pmxvf32gerpn XM0_YM0", "AT,XA,XB,XMSK,YMSK", 0b00001111}, /* bcwp */
  { &test_pmxvf32gerpn_XM0_YM13, "pmxvf32gerpn XM0_YM13", "AT,XA,XB,XMSK,YMSK", 0b00001111}, /* bcwp */
  { &test_pmxvf32gerpn_XM11_YM0, "pmxvf32gerpn XM11_YM0", "AT,XA,XB,XMSK,YMSK", 0b00001111}, /* bcwp */
  { &test_pmxvf32gerpn_XM11_YM13, "pmxvf32gerpn XM11_YM13", "AT,XA,XB,XMSK,YMSK", 0b00001111}, /* bcwp */
  { &test_pmxvf32gerpp_XM0_YM0, "pmxvf32gerpp XM0_YM0", "AT,XA,XB,XMSK,YMSK", 0b00001111}, /* bcwp */
  { &test_pmxvf32gerpp_XM0_YM13, "pmxvf32gerpp XM0_YM13", "AT,XA,XB,XMSK,YMSK", 0b00001111}, /* bcwp */
  { &test_pmxvf32gerpp_XM11_YM0, "pmxvf32gerpp XM11_YM0", "AT,XA,XB,XMSK,YMSK", 0b00001111}, /* bcwp */
  { &test_pmxvf32gerpp_XM11_YM13, "pmxvf32gerpp XM11_YM13", "AT,XA,XB,XMSK,YMSK", 0b00001111}, /* bcwp */
  { &test_pmxvf32ger_XM0_YM0, "pmxvf32ger XM0_YM0", "AT,XA,XB,XMSK,YMSK", 0b00001111}, /* bcwp */
  { &test_pmxvf32ger_XM0_YM13, "pmxvf32ger XM0_YM13", "AT,XA,XB,XMSK,YMSK", 0b00001111}, /* bcwp */
  { &test_pmxvf32ger_XM11_YM0, "pmxvf32ger XM11_YM0", "AT,XA,XB,XMSK,YMSK", 0b00001111}, /* bcwp */
  { &test_pmxvf32ger_XM11_YM13, "pmxvf32ger XM11_YM13", "AT,XA,XB,XMSK,YMSK", 0b00001111}, /* bcwp */
  { &test_pmxvf64gernn_XM0_YM0, "pmxvf64gernn XM0_YM0", "AT,XAp,XB,XMSK,YMSK", 0b00110000}, /* bcwp */
  { &test_pmxvf64gernn_XM0_YM1, "pmxvf64gernn XM0_YM1", "AT,XAp,XB,XMSK,YMSK", 0b00110000}, /* bcwp */
  { &test_pmxvf64gernn_XM11_YM0, "pmxvf64gernn XM11_YM0", "AT,XAp,XB,XMSK,YMSK", 0b00110000}, /* bcwp */
  { &test_pmxvf64gernn_XM11_YM1, "pmxvf64gernn XM11_YM1", "AT,XAp,XB,XMSK,YMSK", 0b00110000}, /* bcwp */
  { &test_pmxvf64gernp_XM0_YM0, "pmxvf64gernp XM0_YM0", "AT,XAp,XB,XMSK,YMSK", 0b00110000}, /* bcwp */
  { &test_pmxvf64gernp_XM0_YM1, "pmxvf64gernp XM0_YM1", "AT,XAp,XB,XMSK,YMSK", 0b00110000}, /* bcwp */
  { &test_pmxvf64gernp_XM11_YM0, "pmxvf64gernp XM11_YM0", "AT,XAp,XB,XMSK,YMSK", 0b00110000}, /* bcwp */
  { &test_pmxvf64gernp_XM11_YM1, "pmxvf64gernp XM11_YM1", "AT,XAp,XB,XMSK,YMSK", 0b00110000}, /* bcwp */
  { &test_pmxvf64gerpn_XM0_YM0, "pmxvf64gerpn XM0_YM0", "AT,XAp,XB,XMSK,YMSK", 0b00110000}, /* bcwp */
  { &test_pmxvf64gerpn_XM0_YM1, "pmxvf64gerpn XM0_YM1", "AT,XAp,XB,XMSK,YMSK", 0b00110000}, /* bcwp */
  { &test_pmxvf64gerpn_XM11_YM0, "pmxvf64gerpn XM11_YM0", "AT,XAp,XB,XMSK,YMSK", 0b00110000}, /* bcwp */
  { &test_pmxvf64gerpn_XM11_YM1, "pmxvf64gerpn XM11_YM1", "AT,XAp,XB,XMSK,YMSK", 0b00110000}, /* bcwp */
  { &test_pmxvf64gerpp_XM0_YM0, "pmxvf64gerpp XM0_YM0", "AT,XAp,XB,XMSK,YMSK", 0b00110000}, /* bcwp */
  { &test_pmxvf64gerpp_XM0_YM1, "pmxvf64gerpp XM0_YM1", "AT,XAp,XB,XMSK,YMSK", 0b00110000}, /* bcwp */
  { &test_pmxvf64gerpp_XM11_YM0, "pmxvf64gerpp XM11_YM0", "AT,XAp,XB,XMSK,YMSK", 0b00110000}, /* bcwp */
  { &test_pmxvf64gerpp_XM11_YM1, "pmxvf64gerpp XM11_YM1", "AT,XAp,XB,XMSK,YMSK", 0b00110000}, /* bcwp */
  { &test_pmxvf64ger_XM0_YM0, "pmxvf64ger XM0_YM0", "AT,XAp,XB,XMSK,YMSK", 0b00110000}, /* bcwp */
  { &test_pmxvf64ger_XM0_YM1, "pmxvf64ger XM0_YM1", "AT,XAp,XB,XMSK,YMSK", 0b00110000}, /* bcwp */
  { &test_pmxvf64ger_XM11_YM0, "pmxvf64ger XM11_YM0", "AT,XAp,XB,XMSK,YMSK", 0b00110000}, /* bcwp */
  { &test_pmxvf64ger_XM11_YM1, "pmxvf64ger XM11_YM1", "AT,XAp,XB,XMSK,YMSK", 0b00110000}, /* bcwp */
  { &test_pmxvi4ger8pp_XM0_YM0_PM0, "pmxvi4ger8pp XM0_YM0_PM0", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi4ger8pp_XM0_YM0_PM45, "pmxvi4ger8pp XM0_YM0_PM45", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi4ger8pp_XM0_YM1_PM0, "pmxvi4ger8pp XM0_YM1_PM0", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi4ger8pp_XM0_YM1_PM45, "pmxvi4ger8pp XM0_YM1_PM45", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi4ger8pp_XM11_YM0_PM0, "pmxvi4ger8pp XM11_YM0_PM0", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi4ger8pp_XM11_YM0_PM45, "pmxvi4ger8pp XM11_YM0_PM45", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi4ger8pp_XM11_YM1_PM0, "pmxvi4ger8pp XM11_YM1_PM0", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi4ger8pp_XM11_YM1_PM45, "pmxvi4ger8pp XM11_YM1_PM45", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi4ger8_XM0_YM0_PM0, "pmxvi4ger8 XM0_YM0_PM0", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi4ger8_XM0_YM0_PM45, "pmxvi4ger8 XM0_YM0_PM45", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi4ger8_XM0_YM1_PM0, "pmxvi4ger8 XM0_YM1_PM0", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi4ger8_XM0_YM1_PM45, "pmxvi4ger8 XM0_YM1_PM45", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi4ger8_XM11_YM0_PM0, "pmxvi4ger8 XM11_YM0_PM0", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi4ger8_XM11_YM0_PM45, "pmxvi4ger8 XM11_YM0_PM45", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi4ger8_XM11_YM1_PM0, "pmxvi4ger8 XM11_YM1_PM0", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi4ger8_XM11_YM1_PM45, "pmxvi4ger8 XM11_YM1_PM45", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi8ger4pp_XM0_YM0_PM0, "pmxvi8ger4pp XM0_YM0_PM0", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi8ger4pp_XM0_YM0_PM5, "pmxvi8ger4pp XM0_YM0_PM5", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi8ger4pp_XM0_YM13_PM0, "pmxvi8ger4pp XM0_YM13_PM0", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi8ger4pp_XM0_YM13_PM5, "pmxvi8ger4pp XM0_YM13_PM5", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi8ger4pp_XM11_YM0_PM0, "pmxvi8ger4pp XM11_YM0_PM0", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi8ger4pp_XM11_YM0_PM5, "pmxvi8ger4pp XM11_YM0_PM5", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi8ger4pp_XM11_YM13_PM0, "pmxvi8ger4pp XM11_YM13_PM0", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi8ger4pp_XM11_YM13_PM5, "pmxvi8ger4pp XM11_YM13_PM5", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi8ger4spp_XM0_YM0_PM0, "pmxvi8ger4spp XM0_YM0_PM0", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi8ger4spp_XM0_YM0_PM5, "pmxvi8ger4spp XM0_YM0_PM5", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi8ger4spp_XM0_YM13_PM0, "pmxvi8ger4spp XM0_YM13_PM0", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi8ger4spp_XM0_YM13_PM5, "pmxvi8ger4spp XM0_YM13_PM5", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi8ger4spp_XM11_YM0_PM0, "pmxvi8ger4spp XM11_YM0_PM0", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi8ger4spp_XM11_YM0_PM5, "pmxvi8ger4spp XM11_YM0_PM5", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi8ger4spp_XM11_YM13_PM0, "pmxvi8ger4spp XM11_YM13_PM0", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi8ger4spp_XM11_YM13_PM5, "pmxvi8ger4spp XM11_YM13_PM5", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi8ger4_XM0_YM0_PM0, "pmxvi8ger4 XM0_YM0_PM0", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi8ger4_XM0_YM0_PM5, "pmxvi8ger4 XM0_YM0_PM5", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi8ger4_XM0_YM13_PM0, "pmxvi8ger4 XM0_YM13_PM0", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi8ger4_XM0_YM13_PM5, "pmxvi8ger4 XM0_YM13_PM5", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi8ger4_XM11_YM0_PM0, "pmxvi8ger4 XM11_YM0_PM0", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi8ger4_XM11_YM0_PM5, "pmxvi8ger4 XM11_YM0_PM5", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi8ger4_XM11_YM13_PM0, "pmxvi8ger4 XM11_YM13_PM0", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi8ger4_XM11_YM13_PM5, "pmxvi8ger4 XM11_YM13_PM5", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi16ger2pp_XM0_YM0_PM0, "pmxvi16ger2pp XM0_YM0_PM0", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi16ger2pp_XM0_YM0_PM1, "pmxvi16ger2pp XM0_YM0_PM1", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi16ger2pp_XM0_YM13_PM0, "pmxvi16ger2pp XM0_YM13_PM0", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi16ger2pp_XM0_YM13_PM1, "pmxvi16ger2pp XM0_YM13_PM1", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi16ger2pp_XM11_YM0_PM0, "pmxvi16ger2pp XM11_YM0_PM0", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi16ger2pp_XM11_YM0_PM1, "pmxvi16ger2pp XM11_YM0_PM1", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi16ger2pp_XM11_YM13_PM0, "pmxvi16ger2pp XM11_YM13_PM0", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi16ger2pp_XM11_YM13_PM1, "pmxvi16ger2pp XM11_YM13_PM1", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi16ger2spp_XM0_YM0_PM0, "pmxvi16ger2spp XM0_YM0_PM0", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi16ger2spp_XM0_YM0_PM1, "pmxvi16ger2spp XM0_YM0_PM1", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi16ger2spp_XM0_YM13_PM0, "pmxvi16ger2spp XM0_YM13_PM0", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi16ger2spp_XM0_YM13_PM1, "pmxvi16ger2spp XM0_YM13_PM1", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi16ger2spp_XM11_YM0_PM0, "pmxvi16ger2spp XM11_YM0_PM0", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi16ger2spp_XM11_YM0_PM1, "pmxvi16ger2spp XM11_YM0_PM1", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi16ger2spp_XM11_YM13_PM0, "pmxvi16ger2spp XM11_YM13_PM0", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi16ger2spp_XM11_YM13_PM1, "pmxvi16ger2spp XM11_YM13_PM1", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi16ger2s_XM0_YM0_PM0, "pmxvi16ger2s XM0_YM0_PM0", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi16ger2s_XM0_YM0_PM1, "pmxvi16ger2s XM0_YM0_PM1", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi16ger2s_XM0_YM13_PM0, "pmxvi16ger2s XM0_YM13_PM0", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi16ger2s_XM0_YM13_PM1, "pmxvi16ger2s XM0_YM13_PM1", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi16ger2s_XM11_YM0_PM0, "pmxvi16ger2s XM11_YM0_PM0", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi16ger2s_XM11_YM0_PM1, "pmxvi16ger2s XM11_YM0_PM1", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi16ger2s_XM11_YM13_PM0, "pmxvi16ger2s XM11_YM13_PM0", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi16ger2s_XM11_YM13_PM1, "pmxvi16ger2s XM11_YM13_PM1", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi16ger2_XM0_YM0_PM0, "pmxvi16ger2 XM0_YM0_PM0", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi16ger2_XM0_YM0_PM1, "pmxvi16ger2 XM0_YM0_PM1", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi16ger2_XM0_YM13_PM0, "pmxvi16ger2 XM0_YM13_PM0", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi16ger2_XM0_YM13_PM1, "pmxvi16ger2 XM0_YM13_PM1", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi16ger2_XM11_YM0_PM0, "pmxvi16ger2 XM11_YM0_PM0", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi16ger2_XM11_YM0_PM1, "pmxvi16ger2 XM11_YM0_PM1", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi16ger2_XM11_YM13_PM0, "pmxvi16ger2 XM11_YM13_PM0", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_pmxvi16ger2_XM11_YM13_PM1, "pmxvi16ger2 XM11_YM13_PM1", "AT,XA,XB,XMSK,YMSK,PMSK"}, /* bcwp */
  { &test_xvbf16ger2nn, "xvbf16ger2nn", "AT,XA,XB", 0b00001111}, /* bcs */
  { &test_xvbf16ger2np, "xvbf16ger2np", "AT,XA,XB", 0b00001111}, /* bcs */
  { &test_xvbf16ger2pn, "xvbf16ger2pn", "AT,XA,XB", 0b00001111}, /* bcs */
  { &test_xvbf16ger2pp, "xvbf16ger2pp", "AT,XA,XB", 0b00001111}, /* bcs */
  { &test_xvbf16ger2, "xvbf16ger2", "AT,XA,XB", 0b00001111}, /* bcs */
  { &test_xvf16ger2nn, "xvf16ger2nn", "AT,XA,XB", 0b00001111}, /* bcs */
  { &test_xvf16ger2np, "xvf16ger2np", "AT,XA,XB", 0b00001111}, /* bcs */
  { &test_xvf16ger2pn, "xvf16ger2pn", "AT,XA,XB", 0b00001111}, /* bcs */
  { &test_xvf16ger2pp, "xvf16ger2pp", "AT,XA,XB", 0b00001111}, /* bcs */
  { &test_xvf16ger2, "xvf16ger2", "AT,XA,XB", 0b00001111}, /* bcs */
  { &test_xvf32gernn, "xvf32gernn", "AT,XA,XB", 0b00001111}, /* bcs */
  { &test_xvf32gernp, "xvf32gernp", "AT,XA,XB", 0b00001111}, /* bcs */
  { &test_xvf32gerpn, "xvf32gerpn", "AT,XA,XB", 0b00001111}, /* bcs */
  { &test_xvf32gerpp, "xvf32gerpp", "AT,XA,XB", 0b00001111}, /* bcs */
  { &test_xvf32ger, "xvf32ger", "AT,XA,XB", 0b00001111}, /* bcs */
  { &test_xvf64gernn, "xvf64gernn", "AT,XAp,XB", 0b00110000}, /* bcs */
  { &test_xvf64gernp, "xvf64gernp", "AT,XAp,XB", 0b00110000}, /* bcs */
  { &test_xvf64gerpn, "xvf64gerpn", "AT,XAp,XB", 0b00110000}, /* bcs */
  { &test_xvf64gerpp, "xvf64gerpp", "AT,XAp,XB", 0b00110000}, /* bcs */
  { &test_xvf64ger, "xvf64ger", "AT,XAp,XB", 0b00110000}, /* bcs */
  { &test_xvi4ger8pp, "xvi4ger8pp", "AT,XA,XB"}, /* bcs */
  { &test_xvi4ger8, "xvi4ger8", "AT,XA,XB"}, /* bcs */
  { &test_xvi8ger4pp, "xvi8ger4pp", "AT,XA,XB"}, /* bcs */
  { &test_xvi8ger4spp, "xvi8ger4spp", "AT,XA,XB"}, /* bcs */
  { &test_xvi8ger4, "xvi8ger4", "AT,XA,XB"}, /* bcs */
  { &test_xvi16ger2pp, "xvi16ger2pp", "AT,XA,XB"}, /* bcs */
  { &test_xvi16ger2spp, "xvi16ger2spp", "AT,XA,XB"}, /* bcs */
  { &test_xvi16ger2s, "xvi16ger2s", "AT,XA,XB"}, /* bcs */
  { &test_xvi16ger2, "xvi16ger2", "AT,XA,XB"}, /* bcs */
  { &test_xxmfacc, "xxmfacc", "AS"}, /* bcs */
  { &test_xxmtacc, "xxmtacc", "AT"}, /* bcs */
  { &test_xxsetaccz, "xxsetaccz", "AT"}, /* bcs */
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
