
SUBDIRS = . unused

noinst_SCRIPTS = \
	vg_regtest \
	filter_addresses \
	filter_discards \
	filter_numbers \
	filter_stderr_basic \
	filter_test_paths

EXTRA_DIST = $(noinst_SCRIPTS)

check_PROGRAMS = \
	cputest \
	toobig-allocs \
	true

AM_CFLAGS   = $(WERROR) -Winline -Wall -Wshadow -g
AM_CXXFLAGS = $(AM_CFLAGS)

# generic C ones
cputest_SOURCES 	= cputest.c
cputest_CFLAGS  	= $(AM_CFLAGS) -D__$(VG_ARCH)__
toobig_allocs_SOURCES	= toobig-allocs.c
true_SOURCES 		= true.c

