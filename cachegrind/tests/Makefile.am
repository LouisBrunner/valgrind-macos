
# For AM_FLAG_M3264_PRI
include $(top_srcdir)/Makefile.flags.am

SUBDIRS = .
if VGCONF_ARCHS_INCLUDE_X86
SUBDIRS += x86
endif

DIST_SUBDIRS = x86 .

noinst_SCRIPTS = filter_stderr filter_cachesim_discards

EXTRA_DIST = $(noinst_SCRIPTS) \
	chdir.vgtest chdir.stderr.exp \
	clreq.vgtest clreq.stderr.exp \
	dlclose.vgtest dlclose.stderr.exp dlclose.stdout.exp \
	wrap5.vgtest wrap5.stderr.exp wrap5.stdout.exp

check_PROGRAMS = \
	chdir clreq dlclose myprint.so

AM_CPPFLAGS = -I$(top_srcdir)/include
AM_CFLAGS   = $(WERROR) -Winline -Wall -Wshadow -g $(AM_FLAG_M3264_PRI)

# C ones
dlclose_LDADD		= -ldl
myprint_so_SOURCES	= myprint.c
myprint_so_LDFLAGS	= $(AM_FLAG_M3264_PRI) -shared -fPIC
myprint_so_CFLAGS	= $(AM_FLAG_M3264_PRI) -fPIC
