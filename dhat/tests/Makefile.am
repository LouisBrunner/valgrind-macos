
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr filter_copy filter_user_histo

EXTRA_DIST = \
	acc.stderr.exp acc.vgtest \
	ad-hoc.stderr.exp ad-hoc.vgtest \
	basic.stderr.exp basic.vgtest \
	big.stderr.exp big.vgtest \
	copy.stderr.exp copy.vgtest \
	empty.stderr.exp empty.vgtest \
	sig.stderr.exp sig.vgtest \
	single.stderr.exp single.vgtest \
	user_histo1.stderr.exp user_histo1.vgtest \
	user_histo1.stdout.exp

check_PROGRAMS = \
	acc \
	ad-hoc \
	basic \
	big \
	copy \
	empty \
	sig \
	single \
	user_histo1

AM_CFLAGS   += $(AM_FLAG_M3264_PRI)
AM_CXXFLAGS += $(AM_FLAG_M3264_PRI)

# We don't care about uninitialized or unused malloc results
basic_CFLAGS  = $(AM_CFLAGS) -Wno-uninitialized
big_CFLAGS  = $(AM_CFLAGS) -Wno-unused-result

# Prevent the copying functions from being inlined
copy_CFLAGS = $(AM_CFLAGS) -fno-builtin

user_histo1_SOURCES = user_histo1.cpp
user_histo1_CXXFLAGS = $(AM_CXXFLAGS) -std=c++11
