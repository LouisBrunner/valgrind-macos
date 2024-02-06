
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr

EXTRA_DIST = \
	posix_fadvise.vgtest \
	posix_fallocate.vgtest \
	posix_fadvise.stderr.exp \
	posix_fallocate.stderr.exp \
	reallocarray.vgtest \
	reallocarray.stderr.exp

check_PROGRAMS = \
	posix_fadvise posix_fallocate reallocarray

# Linux also adds  $(FLAG_MMMX) $(FLAG_MSSE) to the first two
AM_CFLAGS    += @FLAG_M32@
AM_CXXFLAGS  += @FLAG_M32@
AM_CCASFLAGS += @FLAG_M32@

posix_fallocate_CFLAGS       = $(AM_CFLAGS) @FLAG_W_NO_UNINITIALIZED@
posix_fadvise_CFLAGS       = $(AM_CFLAGS) @FLAG_W_NO_UNINITIALIZED@
