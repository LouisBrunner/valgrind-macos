
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = \
	filter_defcfaexpr filter_scalar filter_stderr

noinst_HEADERS = scalar.h

EXTRA_DIST = \
	access_below_sp_1.vgtest \
	access_below_sp_1.stderr.exp access_below_sp_1.stdout.exp \
	access_below_sp_2.vgtest \
	access_below_sp_2.stderr.exp access_below_sp_2.stdout.exp \
	defcfaexpr.vgtest defcfaexpr.stderr.exp \
	int3-amd64.vgtest int3-amd64.stderr.exp int3-amd64.stdout.exp \
	scalar.stderr.exp scalar.vgtest \
	reallocarray.vgtest reallocarray.stderr.exp

check_PROGRAMS = \
	access_below_sp \
	defcfaexpr \
	int3-amd64 \
	scalar

if HAVE_REALLOCARRAY
check_PROGRAMS += reallocarray
endif

AM_CFLAGS    += @FLAG_M64@
AM_CXXFLAGS  += @FLAG_M64@
AM_CCASFLAGS += @FLAG_M64@

defcfaexpr_SOURCES	= defcfaexpr.S
defcfaexpr_CFLAGS	= $(AM_CFLAGS) @FLAG_NO_PIE@ 
reallocarray_CFLAGS	= $(AM_CFLAGS) @FLAG_W_NO_ALLOC_SIZE_LARGER_THAN@
scalar_CFLAGS = $(AM_CFLAGS) @FLAG_W_NO_UNINITIALIZED@

# Avoid linker warnings
int3_amd64_LDFLAGS = @FLAG_NO_PIE@
