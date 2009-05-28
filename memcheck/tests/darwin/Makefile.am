
include $(top_srcdir)/Makefile.tool-tests.am

noinst_SCRIPTS = filter_stderr

noinst_HEADERS = scalar.h

EXTRA_DIST = $(noinst_SCRIPTS) \
	scalar.stderr.exp scalar.vgtest \
	scalar_fork.stderr.exp scalar_fork.vgtest \
	scalar_vfork.stderr.exp scalar_vfork.vgtest

check_PROGRAMS = \
	scalar scalar_fork scalar_vfork


AM_CFLAGS    += @FLAG_M32@ $(FLAG_MMMX) $(FLAG_MSSE)
AM_CXXFLAGS  += @FLAG_M32@ $(FLAG_MMMX) $(FLAG_MSSE)
AM_CCASFLAGS += @FLAG_M32@

