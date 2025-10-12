
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr filter_arg_check

EXTRA_DIST = \
	scalar_arg_check.vgtest \
	scalar_arg_check.stderr.exp

check_PROGRAMS = \
	scalar_arg_check

AM_CFLAGS    += @FLAG_M64@
AM_CXXFLAGS  += @FLAG_M64@
AM_CCASFLAGS += @FLAG_M64@

