
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr

EXTRA_DIST = \
	context_float.stdout.exp context_float.stderr.exp context_float.vgtest \
	context_integer.stdout.exp context_integer.stderr.exp context_integer.vgtest \
	scalar.stderr.exp scalar.vgtest

check_PROGRAMS = \
	context_float \
	context_integer \
	scalar

AM_CFLAGS    += @FLAG_M64@
AM_CXXFLAGS  += @FLAG_M64@
AM_CCASFLAGS += @FLAG_M64@
