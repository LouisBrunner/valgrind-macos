
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = \
	filter_stderr filter_minimal

EXTRA_DIST = \
	bug345887.stderr.exp bug345887.vgtest

check_PROGRAMS = \
	bug345887

AM_CFLAGS    += @FLAG_M64@
AM_CXXFLAGS  += @FLAG_M64@
AM_CCASFLAGS += @FLAG_M64@
