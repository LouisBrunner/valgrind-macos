noinst_SCRIPTS = filter_stderr filter_pushfpopf filter_tronical

noinst_HEADERS = scalar.h

EXTRA_DIST = $(noinst_SCRIPTS) \
	fpeflags.stderr.exp fpeflags.vgtest \
	pushfpopf.stderr.exp pushfpopf.stdout.exp pushfpopf.vgtest \
	scalar.stderr.exp scalar.vgtest \
	scalar_fork.stderr.exp scalar_fork.vgtest \
	scalar_exit_group.stderr.exp scalar_exit_group.vgtest \
	scalar_supp.stderr.exp scalar_supp.vgtest scalar_supp.supp \
	scalar_vfork.stderr.exp scalar_vfork.vgtest \
	tronical.stderr.exp tronical.vgtest

check_PROGRAMS = \
	scalar_exit_group scalar_fork scalar_supp scalar_vfork \
	fpeflags pushfpopf scalar tronical

AM_CPPFLAGS = -I$(top_srcdir)/include
AM_CFLAGS   = $(WERROR) -Winline -Wall -Wshadow -g 
AM_CXXFLAGS = $(AM_CFLAGS)

fpeflags_SOURCES	= fpeflags.c
# must be built with these flags -- bug only occurred with them
fpeflags_CFLAGS		= $(AM_CFLAGS) -march=i686
pushfpopf_SOURCES 	= pushfpopf_c.c pushfpopf_s.s
scalar_SOURCES 		= scalar.c
scalar_exit_group_SOURCES 	= scalar_exit_group.c
scalar_fork_SOURCES 	= scalar_fork.c
scalar_supp_SOURCES 	= scalar_supp.c
scalar_vfork_SOURCES 	= scalar_vfork.c
tronical_SOURCES 	= tronical.S
