
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr

noinst_HEADERS = testinst.h

EXTRA_DIST = \
	atomic.stdout.exp atomic.stderr.exp atomic.vgtest \
	compressed.stdout.exp compressed.stderr.exp compressed.vgtest \
	csr.stdout.exp csr.stderr.exp csr.vgtest \
	float32.stdout.exp float32.stderr.exp float32.vgtest \
	float64.stdout.exp float64.stderr.exp float64.vgtest \
	integer.stdout.exp integer.stderr.exp integer.vgtest \
	muldiv.stdout.exp muldiv.stderr.exp muldiv.vgtest

check_PROGRAMS = \
	allexec \
	atomic \
	compressed \
	csr \
	float32 \
	float64 \
	integer \
	muldiv

AM_CFLAGS    += @FLAG_M64@
AM_CXXFLAGS  += @FLAG_M64@
AM_CCASFLAGS += @FLAG_M64@

allexec_CFLAGS = $(AM_CFLAGS) @FLAG_W_NO_NONNULL@
