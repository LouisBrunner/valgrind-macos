
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr

EXTRA_DIST = \
	acc.stderr.exp acc.vgtest \
	basic.stderr.exp basic.vgtest \
	big.stderr.exp big.vgtest \
	empty.stderr.exp empty.vgtest \
	sig.stderr.exp sig.vgtest \
	single.stderr.exp single.vgtest

check_PROGRAMS = \
	acc \
	basic \
	big \
	empty \
	sig \
	single

AM_CFLAGS   += $(AM_FLAG_M3264_PRI)
AM_CXXFLAGS += $(AM_FLAG_M3264_PRI)

