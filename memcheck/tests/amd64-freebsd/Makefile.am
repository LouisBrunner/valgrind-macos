
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr filter_arg_check

EXTRA_DIST = \
	posix_fadvise.vgtest \
	posix_fallocate.vgtest \
	posix_fadvise.stderr.exp \
	posix_fallocate.stderr.exp \
	reallocarray.vgtest \
	reallocarray.stderr.exp \
	reallocf.vgtest \
	reallocf.stderr.exp \
	scalar_arg_check.vgtest \
	scalar_arg_check.stderr.exp

check_PROGRAMS = \
	posix_fadvise posix_fallocate reallocarray reallocf scalar_arg_check

AM_CFLAGS    += @FLAG_M64@
AM_CXXFLAGS  += @FLAG_M64@
AM_CCASFLAGS += @FLAG_M64@

posix_fallocate_CFLAGS	= $(AM_CFLAGS) @FLAG_W_NO_UNINITIALIZED@
posix_fadvise_CFLAGS	= $(AM_CFLAGS) @FLAG_W_NO_UNINITIALIZED@
reallocarray_CFLAGS	= ${AM_CFLAGS} @FLAG_W_NO_ALLOC_SIZE_LARGER_THAN@

