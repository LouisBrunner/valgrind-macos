
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = \
	filter_stderr filter_minimal

EXTRA_DIST = \
	bug341419.vgtest bug341419.stderr.exp \
	cet_nops_gs.stderr.exp cet_nops_gs.stdout.exp cet_nops_gs.vgtest

check_PROGRAMS = \
	bug341419 \
	cet_nops_gs

AM_CFLAGS    += @FLAG_M64@
AM_CXXFLAGS  += @FLAG_M64@
AM_CCASFLAGS += @FLAG_M64@
