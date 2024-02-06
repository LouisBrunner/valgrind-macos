
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr

EXTRA_DIST = \
        crc32.stdout.exp crc32.stderr.exp crc32.vgtest \
        cvtf_imm.stdout.exp cvtf_imm.stderr.exp cvtf_imm.vgtest \
	fp_and_simd.stdout.exp fp_and_simd.stderr.exp fp_and_simd.vgtest \
	integer.stdout.exp integer.stderr.exp integer.vgtest \
	memory.stdout.exp memory.stderr.exp memory.vgtest \
	atomics_v81.stdout.exp atomics_v81.stderr.exp atomics_v81.vgtest \
	simd_v81.stdout.exp simd_v81.stderr.exp simd_v81.vgtest \
	simd_dotprod.stdout.exp simd_dotprod.stderr.exp simd_dotprod.vgtest \
        fmadd_sub.stdout.exp fmadd_sub.stderr.exp fmadd_sub.vgtest \
	fp_and_simd_v82.stdout.exp fp_and_simd_v82.stderr.exp \
	fp_and_simd_v82.vgtest \
	ldxp_stxp_basisimpl.stdout.exp ldxp_stxp_basisimpl.stderr.exp \
	ldxp_stxp_basisimpl.vgtest \
	ldxp_stxp_fallbackimpl.stdout.exp ldxp_stxp_fallbackimpl.stderr.exp \
	ldxp_stxp_fallbackimpl.vgtest

check_PROGRAMS = \
	allexec \
        cvtf_imm \
	fp_and_simd \
	integer \
	memory \
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

if BUILD_ARMV82_DOTPROD_TESTS
  check_PROGRAMS += simd_dotprod
endif

AM_CFLAGS    += @FLAG_M64@
AM_CXXFLAGS  += @FLAG_M64@
AM_CCASFLAGS += @FLAG_M64@

allexec_CFLAGS     = $(AM_CFLAGS) @FLAG_W_NO_NONNULL@

crc32_CFLAGS       = $(AM_CFLAGS) -march=armv8-a+crc
atomics_v81_CFLAGS = $(AM_CFLAGS) -march=armv8.1-a
simd_v81_CFLAGS    = $(AM_CFLAGS) -march=armv8.1-a+crypto
simd_dotprod_CFLAGS    = $(AM_CFLAGS) -march=armv8.2-a+dotprod
fp_and_simd_CFLAGS = $(AM_CFLAGS) -march=armv8-a+crypto
fp_and_simd_v82_CFLAGS = $(AM_CFLAGS) -march=armv8.2-a+fp16+crypto
integer_CFLAGS     = $(AM_CFLAGS) -g -O0 -DTEST_BFM=0

fp_and_simd_LDADD  = -lm
simd_v81_LDADD     = -lm
fp_and_simd_v82_LDADD  = -lm
