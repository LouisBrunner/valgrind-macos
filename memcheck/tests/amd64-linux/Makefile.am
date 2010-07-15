
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = \
	filter_stderr filter_defcfaexpr

EXTRA_DIST = \
	defcfaexpr.vgtest defcfaexpr.stderr.exp \
	int3-amd64.vgtest int3-amd64.stderr.exp int3-amd64.stdout.exp

check_PROGRAMS = \
	defcfaexpr \
	int3-amd64


AM_CFLAGS    += @FLAG_M64@
AM_CXXFLAGS  += @FLAG_M64@
AM_CCASFLAGS += @FLAG_M64@

defcfaexpr_SOURCES	= defcfaexpr.S

