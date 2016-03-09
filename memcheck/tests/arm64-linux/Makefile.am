
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = \
	filter_stderr

noinst_HEADERS = scalar.h

EXTRA_DIST = \
	scalar.stderr.exp scalar.vgtest

check_PROGRAMS = \
	scalar


AM_CFLAGS    += @FLAG_M64@
AM_CXXFLAGS  += @FLAG_M64@
AM_CCASFLAGS += @FLAG_M64@

