
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = \
	filter_stderr

EXTRA_DIST = $(noinst_SCRIPTS) \
	badsioc.stderr.exp badsioc.stdout.exp badsioc.vgtest \
	fadvise64.stderr.exp fadvise64.stdout.exp fadvise64.vgtest

check_PROGRAMS = \
	bad_sioc \
	fadvise64

fadvise64_CFLAGS = $(AM_CFLAGS) -Wno-uninitialized
