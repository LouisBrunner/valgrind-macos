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

static void test_brh (void) {
  __asm__ __volatile__ ("brh %0, %1" : "=r" (ra) : "r" (rs) );
}
static void test_brw (void) {
  __asm__ __volatile__ ("brw %0, %1" : "=r" (ra) : "r" (rs) );
}
static void test_brd (void) {
  __asm__ __volatile__ ("brd %0, %1" : "=r" (ra) : "r" (rs) );
}
static void test_plxvp_off0 (void) {
  __asm__ __volatile__ ("plxvp 20, 0(%0), 0" :: "r" (ra) );
}
static void test_plxvp_off8 (void) {
  __asm__ __volatile__ ("plxvp 20, 8(%0), 0" :: "r" (ra) );
}
static void test_plxvp_off16 (void) {
  __asm__ __volatile__ ("plxvp 20, 16(%0), 0" :: "r" (ra) );
}
static void test_plxvp_off24 (void) {
  __asm__ __volatile__ ("plxvp 20, 24(%0), 0" :: "r" (ra) );
}
static void test_plxvp_off32 (void) {
  __asm__ __volatile__ ("plxvp 20, 32(%0), 0" :: "r" (ra) );
}
static void test_cfuged (void) {
  __asm__ __volatile__ ("cfuged %0, %1, %2" : "=r" (ra) : "r" (rs), "r" (rb) );
}
static void test_cntlzdm (void) {
  __asm__ __volatile__ ("cntlzdm %0, %1, %2" : "=r" (ra) : "r" (rs), "r" (rb) );
}
static void test_cnttzdm (void) {
  __asm__ __volatile__ ("cnttzdm %0, %1, %2" : "=r" (ra) : "r" (rs), "r" (rb) );
}
static void test_pdepd (void) {
  __asm__ __volatile__ ("pdepd %0, %1, %2" : "=r" (ra) : "r" (rs), "r" (rb) );
}
static void test_pextd (void) {
  __asm__ __volatile__ ("pextd %0, %1, %2" : "=r" (ra) : "r" (rs), "r" (rb) );
}
static void test_vgnb_2 (void) {
  __asm__ __volatile__ ("vgnb %0, %1, 2" : "=r" (rt) : "v" (vrb) );
}
static void test_vgnb_3 (void) {
  __asm__ __volatile__ ("vgnb %0, %1, 3" : "=r" (rt) : "v" (vrb) );
}
static void test_vgnb_4 (void) {
  __asm__ __volatile__ ("vgnb %0, %1, 4" : "=r" (rt) : "v" (vrb) );
}
static void test_vgnb_5 (void) {
  __asm__ __volatile__ ("vgnb %0, %1, 5" : "=r" (rt) : "v" (vrb) );
}
static void test_vgnb_6 (void) {
  __asm__ __volatile__ ("vgnb %0, %1, 6" : "=r" (rt) : "v" (vrb) );
}
static void test_vgnb_7 (void) {
  __asm__ __volatile__ ("vgnb %0, %1, 7" : "=r" (rt) : "v" (vrb) );
}
static void test_setbc_0_cr0s (void) {
  SET_CR(0x00000000);
  __asm__ __volatile__ ("setbc 26, 0");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setbc_0_cr1s (void) {
  SET_CR(0xffffffff);
  __asm__ __volatile__ ("setbc 26, 0");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setbc_0_creb (void) {
  SET_CR(0xaaaaaaaa);
  __asm__ __volatile__ ("setbc 26, 0");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setbc_0_crob (void) {
  SET_CR(0x55555555);
  __asm__ __volatile__ ("setbc 26, 0");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setbc_3_cr0s (void) {
  SET_CR(0x00000000);
  __asm__ __volatile__ ("setbc 26, 3");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setbc_3_cr1s (void) {
  SET_CR(0xffffffff);
  __asm__ __volatile__ ("setbc 26, 3");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setbc_3_creb (void) {
  SET_CR(0xaaaaaaaa);
  __asm__ __volatile__ ("setbc 26, 3");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setbc_3_crob (void) {
  SET_CR(0x55555555);
  __asm__ __volatile__ ("setbc 26, 3");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setbc_7_cr0s (void) {
  SET_CR(0x00000000);
  __asm__ __volatile__ ("setbc 26, 7");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setbc_7_cr1s (void) {
  SET_CR(0xffffffff);
  __asm__ __volatile__ ("setbc 26, 7");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setbc_7_creb (void) {
  SET_CR(0xaaaaaaaa);
  __asm__ __volatile__ ("setbc 26, 7");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setbc_7_crob (void) {
  SET_CR(0x55555555);
  __asm__ __volatile__ ("setbc 26, 7");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setbc_8_cr0s (void) {
  SET_CR(0x00000000);
  __asm__ __volatile__ ("setbc 26, 8");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setbc_8_cr1s (void) {
  SET_CR(0xffffffff);
  __asm__ __volatile__ ("setbc 26, 8");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setbc_8_creb (void) {
  SET_CR(0xaaaaaaaa);
  __asm__ __volatile__ ("setbc 26, 8");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setbc_8_crob (void) {
  SET_CR(0x55555555);
  __asm__ __volatile__ ("setbc 26, 8");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setbc_31_cr0s (void) {
  SET_CR(0x00000000);
  __asm__ __volatile__ ("setbc 26, 31");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setbc_31_cr1s (void) {
  SET_CR(0xffffffff);
  __asm__ __volatile__ ("setbc 26, 31");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setbc_31_creb (void) {
  SET_CR(0xaaaaaaaa);
  __asm__ __volatile__ ("setbc 26, 31");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setbc_31_crob (void) {
  SET_CR(0x55555555);
  __asm__ __volatile__ ("setbc 26, 31");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setbcr_0_cr0s (void) {
  SET_CR(0x00000000);
  __asm__ __volatile__ ("setbcr 26, 0");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setbcr_0_cr1s (void) {
  SET_CR(0xffffffff);
  __asm__ __volatile__ ("setbcr 26, 0");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setbcr_0_creb (void) {
  SET_CR(0xaaaaaaaa);
  __asm__ __volatile__ ("setbcr 26, 0");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setbcr_0_crob (void) {
  SET_CR(0x55555555);
  __asm__ __volatile__ ("setbcr 26, 0");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setbcr_3_cr0s (void) {
  SET_CR(0x00000000);
  __asm__ __volatile__ ("setbcr 26, 3");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setbcr_3_cr1s (void) {
  SET_CR(0xffffffff);
  __asm__ __volatile__ ("setbcr 26, 3");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setbcr_3_creb (void) {
  SET_CR(0xaaaaaaaa);
  __asm__ __volatile__ ("setbcr 26, 3");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setbcr_3_crob (void) {
  SET_CR(0x55555555);
  __asm__ __volatile__ ("setbcr 26, 3");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setbcr_7_cr0s (void) {
  SET_CR(0x00000000);
  __asm__ __volatile__ ("setbcr 26, 7");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setbcr_7_cr1s (void) {
  SET_CR(0xffffffff);
  __asm__ __volatile__ ("setbcr 26, 7");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setbcr_7_creb (void) {
  SET_CR(0xaaaaaaaa);
  __asm__ __volatile__ ("setbcr 26, 7");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setbcr_7_crob (void) {
  SET_CR(0x55555555);
  __asm__ __volatile__ ("setbcr 26, 7");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setbcr_8_cr0s (void) {
  SET_CR(0x00000000);
  __asm__ __volatile__ ("setbcr 26, 8");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setbcr_8_cr1s (void) {
  SET_CR(0xffffffff);
  __asm__ __volatile__ ("setbcr 26, 8");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setbcr_8_creb (void) {
  SET_CR(0xaaaaaaaa);
  __asm__ __volatile__ ("setbcr 26, 8");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setbcr_8_crob (void) {
  SET_CR(0x55555555);
  __asm__ __volatile__ ("setbcr 26, 8");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setbcr_31_cr0s (void) {
  SET_CR(0x00000000);
  __asm__ __volatile__ ("setbcr 26, 31");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setbcr_31_cr1s (void) {
  SET_CR(0xffffffff);
  __asm__ __volatile__ ("setbcr 26, 31");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setbcr_31_creb (void) {
  SET_CR(0xaaaaaaaa);
  __asm__ __volatile__ ("setbcr 26, 31");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setbcr_31_crob (void) {
  SET_CR(0x55555555);
  __asm__ __volatile__ ("setbcr 26, 31");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setnbc_0_cr0s (void) {
  SET_CR(0x00000000);
  __asm__ __volatile__ ("setnbc 26, 0");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setnbc_0_cr1s (void) {
  SET_CR(0xffffffff);
  __asm__ __volatile__ ("setnbc 26, 0");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setnbc_0_creb (void) {
  SET_CR(0xaaaaaaaa);
  __asm__ __volatile__ ("setnbc 26, 0");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setnbc_0_crob (void) {
  SET_CR(0x55555555);
  __asm__ __volatile__ ("setnbc 26, 0");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setnbc_3_cr0s (void) {
  SET_CR(0x00000000);
  __asm__ __volatile__ ("setnbc 26, 3");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setnbc_3_cr1s (void) {
  SET_CR(0xffffffff);
  __asm__ __volatile__ ("setnbc 26, 3");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setnbc_3_creb (void) {
  SET_CR(0xaaaaaaaa);
  __asm__ __volatile__ ("setnbc 26, 3");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setnbc_3_crob (void) {
  SET_CR(0x55555555);
  __asm__ __volatile__ ("setnbc 26, 3");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setnbc_7_cr0s (void) {
  SET_CR(0x00000000);
  __asm__ __volatile__ ("setnbc 26, 7");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setnbc_7_cr1s (void) {
  SET_CR(0xffffffff);
  __asm__ __volatile__ ("setnbc 26, 7");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setnbc_7_creb (void) {
  SET_CR(0xaaaaaaaa);
  __asm__ __volatile__ ("setnbc 26, 7");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setnbc_7_crob (void) {
  SET_CR(0x55555555);
  __asm__ __volatile__ ("setnbc 26, 7");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setnbc_8_cr0s (void) {
  SET_CR(0x00000000);
  __asm__ __volatile__ ("setnbc 26, 8");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setnbc_8_cr1s (void) {
  SET_CR(0xffffffff);
  __asm__ __volatile__ ("setnbc 26, 8");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setnbc_8_creb (void) {
  SET_CR(0xaaaaaaaa);
  __asm__ __volatile__ ("setnbc 26, 8");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setnbc_8_crob (void) {
  SET_CR(0x55555555);
  __asm__ __volatile__ ("setnbc 26, 8");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setnbc_31_cr0s (void) {
  SET_CR(0x00000000);
  __asm__ __volatile__ ("setnbc 26, 31");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setnbc_31_cr1s (void) {
  SET_CR(0xffffffff);
  __asm__ __volatile__ ("setnbc 26, 31");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setnbc_31_creb (void) {
  SET_CR(0xaaaaaaaa);
  __asm__ __volatile__ ("setnbc 26, 31");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setnbc_31_crob (void) {
  SET_CR(0x55555555);
  __asm__ __volatile__ ("setnbc 26, 31");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setnbcr_0_cr0s (void) {
  SET_CR(0x00000000);
  __asm__ __volatile__ ("setnbcr 26, 0");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setnbcr_0_cr1s (void) {
  SET_CR(0xffffffff);
  __asm__ __volatile__ ("setnbcr 26, 0");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setnbcr_0_creb (void) {
  SET_CR(0xaaaaaaaa);
  __asm__ __volatile__ ("setnbcr 26, 0");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setnbcr_0_crob (void) {
  SET_CR(0x55555555);
  __asm__ __volatile__ ("setnbcr 26, 0");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setnbcr_3_cr0s (void) {
  SET_CR(0x00000000);
  __asm__ __volatile__ ("setnbcr 26, 3");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setnbcr_3_cr1s (void) {
  SET_CR(0xffffffff);
  __asm__ __volatile__ ("setnbcr 26, 3");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setnbcr_3_creb (void) {
  SET_CR(0xaaaaaaaa);
  __asm__ __volatile__ ("setnbcr 26, 3");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setnbcr_3_crob (void) {
  SET_CR(0x55555555);
  __asm__ __volatile__ ("setnbcr 26, 3");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setnbcr_7_cr0s (void) {
  SET_CR(0x00000000);
  __asm__ __volatile__ ("setnbcr 26, 7");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setnbcr_7_cr1s (void) {
  SET_CR(0xffffffff);
  __asm__ __volatile__ ("setnbcr 26, 7");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setnbcr_7_creb (void) {
  SET_CR(0xaaaaaaaa);
  __asm__ __volatile__ ("setnbcr 26, 7");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setnbcr_7_crob (void) {
  SET_CR(0x55555555);
  __asm__ __volatile__ ("setnbcr 26, 7");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setnbcr_8_cr0s (void) {
  SET_CR(0x00000000);
  __asm__ __volatile__ ("setnbcr 26, 8");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setnbcr_8_cr1s (void) {
  SET_CR(0xffffffff);
  __asm__ __volatile__ ("setnbcr 26, 8");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setnbcr_8_creb (void) {
  SET_CR(0xaaaaaaaa);
  __asm__ __volatile__ ("setnbcr 26, 8");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setnbcr_8_crob (void) {
  SET_CR(0x55555555);
  __asm__ __volatile__ ("setnbcr 26, 8");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setnbcr_31_cr0s (void) {
  SET_CR(0x00000000);
  __asm__ __volatile__ ("setnbcr 26, 31");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setnbcr_31_cr1s (void) {
  SET_CR(0xffffffff);
  __asm__ __volatile__ ("setnbcr 26, 31");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setnbcr_31_creb (void) {
  SET_CR(0xaaaaaaaa);
  __asm__ __volatile__ ("setnbcr 26, 31");
  GET_CR(current_cr); SET_CR_ZERO;
}
static void test_setnbcr_31_crob (void) {
  SET_CR(0x55555555);
  __asm__ __volatile__ ("setnbcr 26, 31");
  GET_CR(current_cr); SET_CR_ZERO;
}
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
static void test_vcntmbb_0 (void) {
  __asm__ __volatile__ ("vcntmbb %0, %1, 0" : "=r" (rt) : "v" (vrb) );
}
static void test_vcntmbb_1 (void) {
  __asm__ __volatile__ ("vcntmbb %0, %1, 1" : "=r" (rt) : "v" (vrb) );
}
static void test_vcntmbh_0 (void) {
  __asm__ __volatile__ ("vcntmbh %0, %1, 0" : "=r" (rt) : "v" (vrb) );
}
static void test_vcntmbh_1 (void) {
  __asm__ __volatile__ ("vcntmbh %0, %1, 1" : "=r" (rt) : "v" (vrb) );
}
static void test_vcntmbw_0 (void) {
  __asm__ __volatile__ ("vcntmbw %0, %1, 0" : "=r" (rt) : "v" (vrb) );
}
static void test_vcntmbw_1 (void) {
  __asm__ __volatile__ ("vcntmbw %0, %1, 1" : "=r" (rt) : "v" (vrb) );
}
static void test_vcntmbd_0 (void) {
  __asm__ __volatile__ ("vcntmbd %0, %1, 0" : "=r" (rt) : "v" (vrb) );
}
static void test_vcntmbd_1 (void) {
  __asm__ __volatile__ ("vcntmbd %0, %1, 1" : "=r" (rt) : "v" (vrb) );
}
static void test_vextractbm (void) {
  __asm__ __volatile__ ("vextractbm %0, %1 " : "=r" (rt) : "v" (vrb) );
}
static void test_vextracthm (void) {
  __asm__ __volatile__ ("vextracthm %0, %1 " : "=r" (rt) : "v" (vrb) );
}
static void test_vextractwm (void) {
  __asm__ __volatile__ ("vextractwm %0, %1 " : "=r" (rt) : "v" (vrb) );
}
static void test_vextractdm (void) {
  __asm__ __volatile__ ("vextractdm %0, %1 " : "=r" (rt) : "v" (vrb) );
}
static void test_vextractqm (void) {
  __asm__ __volatile__ ("vextractqm %0, %1 " : "=r" (rt) : "v" (vrb) );
}

static test_list_t testgroup_generic[] = {
  { &test_brd, "brd", "RA,RS"}, /* bcs */
  { &test_brh, "brh", "RA,RS"}, /* bcs */
  { &test_brw, "brw", "RA,RS"}, /* bcs */
  { &test_cfuged, "cfuged", "RA,RS,RB"}, /* bcs */
  { &test_cntlzdm, "cntlzdm", "RA,RS,RB"}, /* bcs */
  { &test_cnttzdm, "cnttzdm", "RA,RS,RB"}, /* bcs */
  { &test_paddi_0, "paddi 0", "RT,RA,SI,R"}, /* bcwp */
  { &test_paddi_12, "paddi 12", "RT,RA,SI,R"}, /* bcwp */
  { &test_paddi_48, "paddi 48", "RT,RA,SI,R"}, /* bcwp */
  { &test_paddi_98, "paddi 98", "RT,RA,SI,R"}, /* bcwp */
  { &test_pdepd, "pdepd", "RA,RS,RB"}, /* bcs */
  { &test_pextd, "pextd", "RA,RS,RB"}, /* bcs */
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
  { &test_plxvp_off0, "plxvp off0", "XTp,D(RA),R"}, /* bcwp */
  { &test_plxvp_off8, "plxvp off8", "XTp,D(RA),R"}, /* bcwp */
  { &test_plxvp_off16, "plxvp off16", "XTp,D(RA),R"}, /* bcwp */
  { &test_plxvp_off24, "plxvp off24", "XTp,D(RA),R"}, /* bcwp */
  { &test_plxvp_off32, "plxvp off32", "XTp,D(RA),R"}, /* bcwp */
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
  { &test_setbcr_0_cr0s, "setbcr 0_cr0s", "RT,BI"}, /* bcwp */
  { &test_setbcr_0_cr1s, "setbcr 0_cr1s", "RT,BI"}, /* bcwp */
  { &test_setbcr_0_creb, "setbcr 0_creb", "RT,BI"}, /* bcwp */
  { &test_setbcr_0_crob, "setbcr 0_crob", "RT,BI"}, /* bcwp */
  { &test_setbcr_3_cr0s, "setbcr 3_cr0s", "RT,BI"}, /* bcwp */
  { &test_setbcr_3_cr1s, "setbcr 3_cr1s", "RT,BI"}, /* bcwp */
  { &test_setbcr_3_creb, "setbcr 3_creb", "RT,BI"}, /* bcwp */
  { &test_setbcr_3_crob, "setbcr 3_crob", "RT,BI"}, /* bcwp */
  { &test_setbcr_7_cr0s, "setbcr 7_cr0s", "RT,BI"}, /* bcwp */
  { &test_setbcr_7_cr1s, "setbcr 7_cr1s", "RT,BI"}, /* bcwp */
  { &test_setbcr_7_creb, "setbcr 7_creb", "RT,BI"}, /* bcwp */
  { &test_setbcr_7_crob, "setbcr 7_crob", "RT,BI"}, /* bcwp */
  { &test_setbcr_8_cr0s, "setbcr 8_cr0s", "RT,BI"}, /* bcwp */
  { &test_setbcr_8_cr1s, "setbcr 8_cr1s", "RT,BI"}, /* bcwp */
  { &test_setbcr_8_creb, "setbcr 8_creb", "RT,BI"}, /* bcwp */
  { &test_setbcr_8_crob, "setbcr 8_crob", "RT,BI"}, /* bcwp */
  { &test_setbcr_31_cr0s, "setbcr 31_cr0s", "RT,BI"}, /* bcwp */
  { &test_setbcr_31_cr1s, "setbcr 31_cr1s", "RT,BI"}, /* bcwp */
  { &test_setbcr_31_creb, "setbcr 31_creb", "RT,BI"}, /* bcwp */
  { &test_setbcr_31_crob, "setbcr 31_crob", "RT,BI"}, /* bcwp */
  { &test_setbc_0_cr0s, "setbc 0_cr0s", "RT,BI"}, /* bcwp */
  { &test_setbc_0_cr1s, "setbc 0_cr1s", "RT,BI"}, /* bcwp */
  { &test_setbc_0_creb, "setbc 0_creb", "RT,BI"}, /* bcwp */
  { &test_setbc_0_crob, "setbc 0_crob", "RT,BI"}, /* bcwp */
  { &test_setbc_3_cr0s, "setbc 3_cr0s", "RT,BI"}, /* bcwp */
  { &test_setbc_3_cr1s, "setbc 3_cr1s", "RT,BI"}, /* bcwp */
  { &test_setbc_3_creb, "setbc 3_creb", "RT,BI"}, /* bcwp */
  { &test_setbc_3_crob, "setbc 3_crob", "RT,BI"}, /* bcwp */
  { &test_setbc_7_cr0s, "setbc 7_cr0s", "RT,BI"}, /* bcwp */
  { &test_setbc_7_cr1s, "setbc 7_cr1s", "RT,BI"}, /* bcwp */
  { &test_setbc_7_creb, "setbc 7_creb", "RT,BI"}, /* bcwp */
  { &test_setbc_7_crob, "setbc 7_crob", "RT,BI"}, /* bcwp */
  { &test_setbc_8_cr0s, "setbc 8_cr0s", "RT,BI"}, /* bcwp */
  { &test_setbc_8_cr1s, "setbc 8_cr1s", "RT,BI"}, /* bcwp */
  { &test_setbc_8_creb, "setbc 8_creb", "RT,BI"}, /* bcwp */
  { &test_setbc_8_crob, "setbc 8_crob", "RT,BI"}, /* bcwp */
  { &test_setbc_31_cr0s, "setbc 31_cr0s", "RT,BI"}, /* bcwp */
  { &test_setbc_31_cr1s, "setbc 31_cr1s", "RT,BI"}, /* bcwp */
  { &test_setbc_31_creb, "setbc 31_creb", "RT,BI"}, /* bcwp */
  { &test_setbc_31_crob, "setbc 31_crob", "RT,BI"}, /* bcwp */
  { &test_setnbcr_0_cr0s, "setnbcr 0_cr0s", "RT,BI"}, /* bcwp */
  { &test_setnbcr_0_cr1s, "setnbcr 0_cr1s", "RT,BI"}, /* bcwp */
  { &test_setnbcr_0_creb, "setnbcr 0_creb", "RT,BI"}, /* bcwp */
  { &test_setnbcr_0_crob, "setnbcr 0_crob", "RT,BI"}, /* bcwp */
  { &test_setnbcr_3_cr0s, "setnbcr 3_cr0s", "RT,BI"}, /* bcwp */
  { &test_setnbcr_3_cr1s, "setnbcr 3_cr1s", "RT,BI"}, /* bcwp */
  { &test_setnbcr_3_creb, "setnbcr 3_creb", "RT,BI"}, /* bcwp */
  { &test_setnbcr_3_crob, "setnbcr 3_crob", "RT,BI"}, /* bcwp */
  { &test_setnbcr_7_cr0s, "setnbcr 7_cr0s", "RT,BI"}, /* bcwp */
  { &test_setnbcr_7_cr1s, "setnbcr 7_cr1s", "RT,BI"}, /* bcwp */
  { &test_setnbcr_7_creb, "setnbcr 7_creb", "RT,BI"}, /* bcwp */
  { &test_setnbcr_7_crob, "setnbcr 7_crob", "RT,BI"}, /* bcwp */
  { &test_setnbcr_8_cr0s, "setnbcr 8_cr0s", "RT,BI"}, /* bcwp */
  { &test_setnbcr_8_cr1s, "setnbcr 8_cr1s", "RT,BI"}, /* bcwp */
  { &test_setnbcr_8_creb, "setnbcr 8_creb", "RT,BI"}, /* bcwp */
  { &test_setnbcr_8_crob, "setnbcr 8_crob", "RT,BI"}, /* bcwp */
  { &test_setnbcr_31_cr0s, "setnbcr 31_cr0s", "RT,BI"}, /* bcwp */
  { &test_setnbcr_31_cr1s, "setnbcr 31_cr1s", "RT,BI"}, /* bcwp */
  { &test_setnbcr_31_creb, "setnbcr 31_creb", "RT,BI"}, /* bcwp */
  { &test_setnbcr_31_crob, "setnbcr 31_crob", "RT,BI"}, /* bcwp */
  { &test_setnbc_0_cr0s, "setnbc 0_cr0s", "RT,BI"}, /* bcwp */
  { &test_setnbc_0_cr1s, "setnbc 0_cr1s", "RT,BI"}, /* bcwp */
  { &test_setnbc_0_creb, "setnbc 0_creb", "RT,BI"}, /* bcwp */
  { &test_setnbc_0_crob, "setnbc 0_crob", "RT,BI"}, /* bcwp */
  { &test_setnbc_3_cr0s, "setnbc 3_cr0s", "RT,BI"}, /* bcwp */
  { &test_setnbc_3_cr1s, "setnbc 3_cr1s", "RT,BI"}, /* bcwp */
  { &test_setnbc_3_creb, "setnbc 3_creb", "RT,BI"}, /* bcwp */
  { &test_setnbc_3_crob, "setnbc 3_crob", "RT,BI"}, /* bcwp */
  { &test_setnbc_7_cr0s, "setnbc 7_cr0s", "RT,BI"}, /* bcwp */
  { &test_setnbc_7_cr1s, "setnbc 7_cr1s", "RT,BI"}, /* bcwp */
  { &test_setnbc_7_creb, "setnbc 7_creb", "RT,BI"}, /* bcwp */
  { &test_setnbc_7_crob, "setnbc 7_crob", "RT,BI"}, /* bcwp */
  { &test_setnbc_8_cr0s, "setnbc 8_cr0s", "RT,BI"}, /* bcwp */
  { &test_setnbc_8_cr1s, "setnbc 8_cr1s", "RT,BI"}, /* bcwp */
  { &test_setnbc_8_creb, "setnbc 8_creb", "RT,BI"}, /* bcwp */
  { &test_setnbc_8_crob, "setnbc 8_crob", "RT,BI"}, /* bcwp */
  { &test_setnbc_31_cr0s, "setnbc 31_cr0s", "RT,BI"}, /* bcwp */
  { &test_setnbc_31_cr1s, "setnbc 31_cr1s", "RT,BI"}, /* bcwp */
  { &test_setnbc_31_creb, "setnbc 31_creb", "RT,BI"}, /* bcwp */
  { &test_setnbc_31_crob, "setnbc 31_crob", "RT,BI"}, /* bcwp */
  { &test_vcntmbb_0, "vcntmbb 0", "RT,VRB,MP"}, /* bcwp */
  { &test_vcntmbb_1, "vcntmbb 1", "RT,VRB,MP"}, /* bcwp */
  { &test_vcntmbd_0, "vcntmbd 0", "RT,VRB,MP"}, /* bcwp */
  { &test_vcntmbd_1, "vcntmbd 1", "RT,VRB,MP"}, /* bcwp */
  { &test_vcntmbh_0, "vcntmbh 0", "RT,VRB,MP"}, /* bcwp */
  { &test_vcntmbh_1, "vcntmbh 1", "RT,VRB,MP"}, /* bcwp */
  { &test_vcntmbw_0, "vcntmbw 0", "RT,VRB,MP"}, /* bcwp */
  { &test_vcntmbw_1, "vcntmbw 1", "RT,VRB,MP"}, /* bcwp */
  { &test_vextractbm, "vextractbm", "RT,VRB"}, /* bcs */
  { &test_vextractdm, "vextractdm", "RT,VRB"}, /* bcs */
  { &test_vextracthm, "vextracthm", "RT,VRB"}, /* bcs */
  { &test_vextractqm, "vextractqm", "RT,VRB"}, /* bcs */
  { &test_vextractwm, "vextractwm", "RT,VRB"}, /* bcs */
  { &test_vgnb_2, "vgnb 2", "RT,VRB,N"}, /* bcwp */
  { &test_vgnb_3, "vgnb 3", "RT,VRB,N"}, /* bcwp */
  { &test_vgnb_4, "vgnb 4", "RT,VRB,N"}, /* bcwp */
  { &test_vgnb_5, "vgnb 5", "RT,VRB,N"}, /* bcwp */
  { &test_vgnb_6, "vgnb 6", "RT,VRB,N"}, /* bcwp */
  { &test_vgnb_7, "vgnb 7", "RT,VRB,N"}, /* bcwp */
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
