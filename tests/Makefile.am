
SUBDIRS = . unused

noinst_SCRIPTS = \
	vg_regtest
	filter_addresses \
	filter_discards \
	filter_stderr_basic \
	filter_test_paths

check_PROGRAMS = \
	true

AM_CFLAGS   = $(WERROR) -Winline -Wall -Wshadow -g
AM_CXXFLAGS = $(AM_CFLAGS)

# generic C ones
true_SOURCES 	= true.c

EXTRA_DIST = $(noinst_SCRIPTS)

