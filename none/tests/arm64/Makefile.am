
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr

EXTRA_DIST = \
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

AM_CFLAGS    += @FLAG_M64@
AM_CXXFLAGS  += @FLAG_M64@
AM_CCASFLAGS += @FLAG_M64@

allexec_CFLAGS     = $(AM_CFLAGS) @FLAG_W_NO_NONNULL@

fp_and_simd_CFLAGS = $(AM_CFLAGS) -march=armv8-a+crypto
integer_CFLAGS     = $(AM_CFLAGS) -g -O0 -DTEST_BFM=0

fp_and_simd_LDADD  = -lm
