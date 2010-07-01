
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = \
	filter_stderr

EXTRA_DIST = \
	defcaexpr.vgtest defcaexpr.stderr.exp \
	int3-x86.vgtest int3-x86.stderr.exp int3-x86.stdout.exp

check_PROGRAMS = \
	defcaexpr \
	int3-amd64


AM_CFLAGS    += @FLAG_M32@ $(FLAG_MMMX) $(FLAG_MSSE)
AM_CXXFLAGS  += @FLAG_M32@ $(FLAG_MMMX) $(FLAG_MSSE)
AM_CCASFLAGS += @FLAG_M32@

