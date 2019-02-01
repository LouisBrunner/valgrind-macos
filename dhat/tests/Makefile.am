
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr

EXTRA_DIST = \
	acc.post.exp acc.stderr.exp acc.vgtest \
	basic.post.exp basic.stderr.exp basic.vgtest \
	big.post.exp big.stderr.exp big.vgtest \
	empty.post.exp empty.stderr.exp empty.vgtest \
	sig.post.exp sig.stderr.exp sig.vgtest \
	single.post.exp single.stderr.exp single.vgtest

check_PROGRAMS = \
	acc \
	basic \
	big \
	empty \
	sig \
	single

AM_CFLAGS   += $(AM_FLAG_M3264_PRI)
AM_CXXFLAGS += $(AM_FLAG_M3264_PRI)

