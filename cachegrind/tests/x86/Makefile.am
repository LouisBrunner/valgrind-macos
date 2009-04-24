
include $(top_srcdir)/Makefile.tool-tests.am

noinst_SCRIPTS = filter_stderr

EXTRA_DIST = $(noinst_SCRIPTS) \
	fpu-28-108.vgtest fpu-28-108.stderr.exp

check_PROGRAMS = \
	fpu-28-108


AM_CFLAGS   += @FLAG_M32@ $(FLAG_MMMX) $(FLAG_MSSE)
AM_CXXFLAGS += @FLAG_M32@ $(FLAG_MMMX) $(FLAG_MSSE)
AM_CCASFLAGS = $(AM_CPPFLAGS) @FLAG_M32@

fpu_28_108_SOURCES	= fpu-28-108.S
