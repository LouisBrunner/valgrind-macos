
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr filter_nan

EXTRA_DIST = \
	bug484426.stdout.exp bug484426.stderr.exp bug484426.vgtest \
	bug489338.stdout.exp bug489338.stderr.exp bug489338.vgtest \
	crc32.stdout.exp crc32.stderr.exp crc32.vgtest \
	cvtf_imm.stdout.exp cvtf_imm.stderr.exp cvtf_imm.vgtest \
	dc_cvax.vgtest dc_cvax.stderr.exp \
	fcvta_s_u.vgtest fcvta_s_u.stderr.exp \
	fp_and_simd.stdout.exp fp_and_simd.stderr.exp fp_and_simd.vgtest \
	frinta_frintn.stderr.exp frinta_frintn.vgtest \
	integer.stdout.exp integer.stderr.exp integer.vgtest \
	memory_test.stdout.exp memory_test.stderr.exp memory_test.vgtest \
		memory_test.stdout.exp-clang \
	atomics_v81.stdout.exp atomics_v81.stderr.exp atomics_v81.vgtest \
	simd_v81.stdout.exp simd_v81.stderr.exp simd_v81.vgtest \
	simd_dotprod.stdout.exp simd_dotprod.stderr.exp simd_dotprod.vgtest \
	fmadd_sub.stdout.exp fmadd_sub.stderr.exp fmadd_sub.vgtest \
	fp_and_simd_v82.stdout.exp fp_and_simd_v82.stderr.exp \
	fp_and_simd_v82.vgtest \
	ldxp_stxp_basisimpl.stdout.exp ldxp_stxp_basisimpl.stderr.exp \
	ldxp_stxp_basisimpl.vgtest \
	ldxp_stxp_fallbackimpl.stdout.exp ldxp_stxp_fallbackimpl.stderr.exp \
	ldxp_stxp_fallbackimpl.vgtest sha512_v82.stdout.exp sha512_v82.stderr.exp \
	sha512_v82.vgtest

check_PROGRAMS = \
	allexec \
	bug484426 \
	bug489338 \
        cvtf_imm \
        dc_cvax \
	fp_and_simd \
	integer \
	memory_test \
	fmadd_sub \
	ldxp_stxp

if BUILD_ARMV8_CRC_TESTS
  check_PROGRAMS += crc32
endif

if BUILD_ARMV81_TESTS
  check_PROGRAMS += atomics_v81 simd_v81
endif

if BUILD_ARMV82_TESTS
  check_PROGRAMS += fp_and_simd_v82
endif

if HAVE_SHA3
  check_PROGRAMS += sha512_v82
endif

if HAVE_CONSTEXPR_IF
  check_PROGRAMS += fcvta_s_u frinta_frintn
endif

if BUILD_ARMV82_DOTPROD_TESTS
  check_PROGRAMS += simd_dotprod
endif

AM_CFLAGS    += @FLAG_M64@
AM_CXXFLAGS  += @FLAG_M64@
AM_CCASFLAGS += @FLAG_M64@

allexec_CFLAGS     = $(AM_CFLAGS) @FLAG_W_NO_NONNULL@

bug484426_SOURCES  = bug484426.cpp
bug484426_CXXFLAGS = ${AM_CXXFLAGS} -std=c++11

crc32_CFLAGS       = $(AM_CFLAGS) -march=armv8-a+crc
atomics_v81_CFLAGS = $(AM_CFLAGS) -march=armv8.1-a
simd_v81_CFLAGS    = $(AM_CFLAGS) -march=armv8.1-a+crypto
simd_dotprod_CFLAGS    = $(AM_CFLAGS) -march=armv8.2-a+dotprod
fp_and_simd_CFLAGS = $(AM_CFLAGS) -march=armv8-a+crypto
fp_and_simd_v82_CFLAGS = $(AM_CFLAGS) -march=armv8.2-a+fp16+crypto
# SHA-512 is not part of SHA-3, either as an algorithm or in Arm features, but
# GCC lumps SHA-512 and SHA-3 extensions together as "sha3".
sha512_v82_CFLAGS = $(AM_CFLAGS) -march=armv8.2-a+sha3
fcvta_s_u_SOURCES  = fcvta_s_u.cpp
fcvta_s_u_CXXFLAGS = ${AM_CXXFLAGS} -std=c++17
frinta_frintn_SOURCES  = frinta_frintn.cpp
frinta_frintn_CXXFLAGS = ${AM_CXXFLAGS} -std=c++17
integer_CFLAGS     = $(AM_CFLAGS) -g -O0 -DTEST_BFM=0

fp_and_simd_SOURCES     = fp_and_simd.c simd.h simd.c
fp_and_simd_v82_SOURCES = fp_and_simd_v82.c simd.h simd.c
sha512_v82_SOURCES      = sha512_v82.c simd.h simd.c

bug489338_LDADD    = -lm
fp_and_simd_LDADD  = -lm
simd_v81_LDADD     = -lm
fp_and_simd_v82_LDADD  = -lm
sha512_v82_LDADD  = -lm
