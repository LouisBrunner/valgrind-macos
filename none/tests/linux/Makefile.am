
include $(top_srcdir)/Makefile.tool-tests.am

noinst_SCRIPTS = filter_stderr

EXTRA_DIST = $(noinst_SCRIPTS) \
	mremap.stderr.exp mremap.stdout.exp mremap.vgtest \
	mremap2.stderr.exp mremap2.stdout.exp mremap2.stdout.exp2 \
	    mremap2.stdout.exp-glibc28-amd64 mremap2.vgtest

check_PROGRAMS = \
	mremap \
	mremap2

