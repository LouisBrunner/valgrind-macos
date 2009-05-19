
include $(top_srcdir)/Makefile.tool-tests.am

noinst_SCRIPTS = filter_stderr

EXTRA_DIST = $(noinst_SCRIPTS) \
	blockfault.stderr.exp blockfault.vgtest \
	mremap.stderr.exp mremap.stderr.exp-glibc27 mremap.stdout.exp \
	    mremap.vgtest \
	mremap2.stderr.exp mremap2.stdout.exp mremap2.stdout.exp2 \
	    mremap2.stdout.exp-glibc28-amd64 mremap2.vgtest

check_PROGRAMS = \
	blockfault \
	mremap \
	mremap2


AM_CFLAGS   += $(AM_FLAG_M3264_PRI)
AM_CXXFLAGS += $(AM_FLAG_M3264_PRI)

