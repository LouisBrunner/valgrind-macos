
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr

EXTRA_DIST = \
	blockfault.stderr.exp blockfault.vgtest \
	mremap.stderr.exp mremap.stderr.exp-glibc27 mremap.stdout.exp \
	    mremap.vgtest \
	mremap2.stderr.exp mremap2.stdout.exp mremap2.vgtest \
	mremap3.stderr.exp mremap3.stdout.exp mremap3.vgtest \
	stack-overflow.stderr.exp stack-overflow.vgtest

check_PROGRAMS = \
	blockfault \
	mremap \
	mremap2 \
	mremap3 \
	stack-overflow


AM_CFLAGS   += $(AM_FLAG_M3264_PRI)
AM_CXXFLAGS += $(AM_FLAG_M3264_PRI)
