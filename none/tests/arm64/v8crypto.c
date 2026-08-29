
#include <stdio.h>
#include <assert.h>
#include <string.h>  // memset
#include "simd.h"

/* ---------------------------------------------------------------- */
/* -- Tests, in the same order that they appear in main()        -- */
/* ---------------------------------------------------------------- */

// ======================== CRYPTO ========================

GEN_TWOVEC_TEST(aesd_16b_16b,    "aesd v6.16b,  v27.16b",  6, 27)
GEN_TWOVEC_TEST(aese_16b_16b,    "aese v6.16b,  v27.16b",  6, 27)
GEN_TWOVEC_TEST(aesimc_16b_16b,  "aesimc v6.16b,  v27.16b",  6, 27)
GEN_TWOVEC_TEST(aesmc_16b_16b,   "aesmc v6.16b,  v27.16b",  6, 27)

GEN_THREEVEC_TEST(sha1c_q_s_4s,     "sha1c q29, s28, v27.4s", 29,28,27)
GEN_TWOVEC_TEST(sha1h_s_s,          "sha1h s6,  s27",  6, 27)
GEN_THREEVEC_TEST(sha1m_q_s_4s,     "sha1m q29, s28, v27.4s", 29,28,27)
GEN_THREEVEC_TEST(sha1p_q_s_4s,     "sha1p q29, s28, v27.4s", 29,28,27)
GEN_THREEVEC_TEST(sha1su0_4s_4s_4s, "sha1su0 v29.4s, v28.4s, v27.4s", 29,28,27)
GEN_TWOVEC_TEST(sha1su1_4s_4s,      "sha1su1 v6.4s,  v27.4s",  6, 27)

GEN_THREEVEC_TEST(sha256h2_q_q_4s,  "sha256h2 q29, q28, v27.4s", 29,28,27)
GEN_THREEVEC_TEST(sha256h_q_q_4s,   "sha256h q29, q28, v27.4s", 29,28,27)
GEN_TWOVEC_TEST(sha256su0_4s_4s,    "sha256su0 v6.4s,  v27.4s",  6, 27)
GEN_THREEVEC_TEST(sha256su1_4s_4s_4s, "sha256su1 v29.4s, v28.4s, v27.4s",                                       29,28,27)

GEN_BINARY_TEST(pmull,  1q, 1d,  1d)
GEN_BINARY_TEST(pmull2, 1q, 2d,  2d)

/* ---------------------------------------------------------------- */
/* -- main()                                                     -- */
/* ---------------------------------------------------------------- */

int main ( void )
{
   assert(sizeof(V128) == 16);
   // ======================== CRYPTO ========================

   // aesd       16b (aes single round decryption)
   // aese       16b (aes single round encryption)
   // aesimc     16b (aes inverse mix columns)
   // aesmc      16b (aes mix columns)
   if (1) DO50( test_aesd_16b_16b(TyNONE) );
   if (1) DO50( test_aese_16b_16b(TyNONE) );
   if (1) DO50( test_aesimc_16b_16b(TyNONE) );
   if (1) DO50( test_aesmc_16b_16b(TyNONE) );

   // sha1c      q_s_4s
   // sha1h      s_s
   // sha1m      q_s_4s
   // sha1p      q_s_4s
   // sha1su0    4s_4s_4s
   // sha1su1    4s_4s
   if (1) DO50( test_sha1c_q_s_4s(TyNONE) );
   if (1) DO50( test_sha1h_s_s(TyNONE) );
   if (1) DO50( test_sha1m_q_s_4s(TyNONE) );
   if (1) DO50( test_sha1p_q_s_4s(TyNONE) );
   if (1) DO50( test_sha1su0_4s_4s_4s(TyNONE) );
   if (1) DO50( test_sha1su1_4s_4s(TyNONE) );

   // sha256h2   q_q_4s
   // sha256h    q_q_4s
   // sha256su0  4s_4s
   // sha256su1  4s_4s_4s
   if (1) DO50( test_sha256h2_q_q_4s(TyNONE) );
   if (1) DO50( test_sha256h_q_q_4s(TyNONE) );
   if (1) DO50( test_sha256su0_4s_4s(TyNONE) );
   if (1) DO50( test_sha256su1_4s_4s_4s(TyNONE) );

   // pmull{2} 1q_1d_1d,1q_2d_2d
   if (1) test_pmull_1q_1d_1d(TyD);
   if (1) test_pmull2_1q_2d_2d(TyD);

   return 0;
}


/* ---------------------------------------------------------------- */
/* -- Alphabetical list of insns                                 -- */
/* ---------------------------------------------------------------- */
/*
   aesd     16b (aes single round decryption)
   aese     16b (aes single round encryption)
   aesimc   16b (aes inverse mix columns)
   aesmc    16b (aes mix columns)

   sha1c       q_s_4s
   sha1h       s_s
   sha1m       q_s_4s
   sha1p       q_s_4s
   sha1su0     4s_4s_4s
   sha1su1     4s_4s
   sha256h2    q_q_4s
   sha256h     q_q_4s
   sha256su0   4s_4s
   sha256su1   4s_4s_4s
*/


/* ---------------------------------------------------------------- */
/* -- List of insns, grouped somewhat by laneage configuration   -- */
/* ---------------------------------------------------------------- */
/*

   ======================== CRYPTO ========================

   aesd       16b (aes single round decryption)
   aese       16b (aes single round encryption)
   aesimc     16b (aes inverse mix columns)
   aesmc      16b (aes mix columns)

   sha1c      q_s_4s
   sha1h      s_s
   sha1m      q_s_4s
   sha1p      q_s_4s
   sha1su0    4s_4s_4s
   sha1su1    4s_4s

   sha256h2   q_q_4s
   sha256h    q_q_4s
   sha256su0  4s_4s
   sha256su1  4s_4s_4s
*/
