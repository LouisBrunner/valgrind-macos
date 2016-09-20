
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr

EXTRA_DIST = \
        crc32.stdout.exp crc32.stderr.exp crc32.vgtest \
        cvtf_imm.stdout.exp cvtf_imm.stderr.exp cvtf_imm.vgtest \
	fp_and_simd.stdout.exp fp_and_simd.stderr.exp fp_and_simd.vgtest \
	integer.stdout.exp integer.stderr.exp integer.vgtest \
	memory.stdout.exp memory.stderr.exp memory.vgtest

check_PROGRAMS = \
	allexec \
        cvtf_imm \
	fp_and_simd \
	integer \
	memory

if BUILD_ARMV8_CRC_TESTS
  check_PROGRAMS += crc32
endif

AM_CFLAGS    += @FLAG_M64@
AM_CXXFLAGS  += @FLAG_M64@
AM_CCASFLAGS += @FLAG_M64@

allexec_CFLAGS     = $(AM_CFLAGS) @FLAG_W_NO_NONNULL@

crc32_CFLAGS       = $(AM_CFLAGS) -march=armv8-a+crc
fp_and_simd_CFLAGS = $(AM_CFLAGS) -march=armv8-a+crypto
integer_CFLAGS     = $(AM_CFLAGS) -g -O0 -DTEST_BFM=0

fp_and_simd_LDADD  = -lm
