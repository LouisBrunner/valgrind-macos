noinst_SCRIPTS = filter_stderr filter_pushfpopf filter_tronical

EXTRA_DIST = $(noinst_SCRIPTS) \
	fpeflags.stderr.exp fpeflags.vgtest \
	pushfpopf.stderr.exp pushfpopf.stdout.exp pushfpopf.vgtest \
	tronical.stderr.exp tronical.vgtest

check_PROGRAMS = \
	fpeflags pushfpopf tronical

AM_CPPFLAGS = -I$(top_srcdir)/include
AM_CFLAGS   = $(WERROR) -Winline -Wall -Wshadow -g 
AM_CXXFLAGS = $(AM_CFLAGS)

fpeflags_SOURCES	= fpeflags.c
# must be built with these flags -- bug only occurred with them
fpeflags_CFLAGS		= $(AM_CFLAGS) -march=i686
pushfpopf_SOURCES 	= pushfpopf_c.c pushfpopf_s.s
tronical_SOURCES 	= tronical.S
