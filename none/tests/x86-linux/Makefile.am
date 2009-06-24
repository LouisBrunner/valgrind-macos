
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = \
	filter_stderr

EXTRA_DIST = \
	seg_override.stderr.exp seg_override.stdout.exp seg_override.vgtest \
	sigcontext.stdout.exp sigcontext.stderr.exp sigcontext.vgtest

check_PROGRAMS = \
	seg_override \
	sigcontext

AM_CFLAGS    += @FLAG_M32@ $(FLAG_MMMX) $(FLAG_MSSE)
AM_CXXFLAGS  += @FLAG_M32@ $(FLAG_MMMX) $(FLAG_MSSE)
AM_CCASFLAGS += @FLAG_M32@


