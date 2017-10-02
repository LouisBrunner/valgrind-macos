
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = \
	filter_stderr

EXTRA_DIST = $(noinst_SCRIPTS) \
	bad_sioc.stderr.exp bad_sioc.stdout.exp bad_sioc.vgtest \
	fadvise64.stderr.exp fadvise64.stderr.exp-BE \
	fadvise64.stdout.exp fadvise64.vgtest


check_PROGRAMS = \
	bad_sioc      \
	fadvise64

fadvise64_CFLAGS = $(AM_CFLAGS) -Wno-uninitialized
