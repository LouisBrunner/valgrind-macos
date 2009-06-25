
include $(top_srcdir)/Makefile.tool-tests.am

SUBDIRS = .
if VGCONF_ARCHS_INCLUDE_X86
SUBDIRS += x86
endif

DIST_SUBDIRS = x86 .

dist_noinst_SCRIPTS = filter_stderr filter_cachesim_discards

EXTRA_DIST = \
	chdir.vgtest chdir.stderr.exp \
	clreq.vgtest clreq.stderr.exp \
	dlclose.vgtest dlclose.stderr.exp dlclose.stdout.exp \
	notpower2.vgtest notpower2.stderr.exp \
	wrap5.vgtest wrap5.stderr.exp wrap5.stdout.exp

check_PROGRAMS = \
	chdir clreq dlclose myprint.so

AM_CFLAGS   += $(AM_FLAG_M3264_PRI)
AM_CXXFLAGS += $(AM_FLAG_M3264_PRI)

# C ones
dlclose_LDADD		= -ldl
if VGCONF_OS_IS_DARWIN
myprint_so_LDFLAGS	= $(AM_CFLAGS) -dynamic -dynamiclib -all_load -fpic
else
myprint_so_LDFLAGS	= $(AM_CFLAGS) -shared -fPIC
endif
myprint_so_SOURCES	= myprint.c	# Only needed for automake-1.7.
myprint_so_CFLAGS	= $(AM_CFLAGS) -fPIC
