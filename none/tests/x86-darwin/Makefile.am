
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = \
	filter_stderr

EXTRA_DIST = \
	bug341419.vgtest bug341419.stderr.exp \
	bug350062.vgtest bug350062.stderr.exp \
	cet_nops_gs.stderr.exp cet_nops_gs.stdout.exp cet_nops_gs.vgtest

check_PROGRAMS = \
	bug341419 \
	bug350062 \
	cet_nops_gs

AM_CFLAGS    += @FLAG_M32@ $(FLAG_MMMX) $(FLAG_MSSE)
AM_CXXFLAGS  += @FLAG_M32@ $(FLAG_MMMX) $(FLAG_MSSE)
AM_CCASFLAGS += @FLAG_M32@
