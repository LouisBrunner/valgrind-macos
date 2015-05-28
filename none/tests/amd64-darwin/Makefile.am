
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = \
	filter_stderr filter_minimal

EXTRA_DIST = \
	bug341419.vgtest bug341419.stderr.exp

check_PROGRAMS = \
	bug341419

AM_CFLAGS    += @FLAG_M64@
AM_CXXFLAGS  += @FLAG_M64@
AM_CCASFLAGS += @FLAG_M64@
