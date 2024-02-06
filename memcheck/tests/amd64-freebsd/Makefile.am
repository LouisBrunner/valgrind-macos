
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr

EXTRA_DIST = \
	posix_fadvise.vgtest \
	posix_fallocate.vgtest \
	posix_fadvise.stderr.exp \
	posix_fallocate.stderr.exp \
	reallocarray.vgtest \
	reallocarray.stderr.exp \
	reallocf.vgtest \
	reallocf.stderr.exp

check_PROGRAMS = \
	posix_fadvise posix_fallocate reallocarray reallocf

AM_CFLAGS    += @FLAG_M64@
AM_CXXFLAGS  += @FLAG_M64@
AM_CCASFLAGS += @FLAG_M64@

posix_fallocate_CFLAGS       = $(AM_CFLAGS) @FLAG_W_NO_UNINITIALIZED@
posix_fadvise_CFLAGS       = $(AM_CFLAGS) @FLAG_W_NO_UNINITIALIZED@
