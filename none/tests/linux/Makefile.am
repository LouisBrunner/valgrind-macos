noinst_SCRIPTS = filter_stderr

EXTRA_DIST = $(noinst_SCRIPTS) \
	mremap.stderr.exp mremap.stdout.exp mremap.vgtest \
	mremap2.stderr.exp mremap2.stdout.exp mremap2.stdout.exp2 \
	    mremap2.stdout.exp-glibc28-amd64 mremap2.vgtest

check_PROGRAMS = \
	mremap \
	mremap2

AM_CPPFLAGS = -I$(top_srcdir) -I$(top_srcdir)/include -I$(top_builddir)/include
AM_CFLAGS   = $(WERROR) -Winline -Wall -Wshadow -g $(AM_FLAG_M3264_PRI)
AM_CXXFLAGS = $(AM_CFLAGS)
