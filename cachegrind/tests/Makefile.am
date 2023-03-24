
include $(top_srcdir)/Makefile.tool-tests.am

SUBDIRS = .
if VGCONF_ARCHS_INCLUDE_X86
SUBDIRS += x86
endif

DIST_SUBDIRS = x86 .

dist_noinst_SCRIPTS = filter_stderr filter_cachesim_discards

# Note that `test.c` and `a.c` are not compiled.
# They just serve as input for cg_annotate in `ann1a` and `ann1b`.
EXTRA_DIST = \
	ann-diff1.post.exp ann-diff1.stderr.exp ann-diff1.vgtest \
		ann-diff2a.cgout ann-diff2b.cgout \
	ann1a.post.exp ann1a.stderr.exp ann1a.vgtest ann1.cgout \
	ann1b.post.exp ann1b.stderr.exp ann1b.vgtest ann1b.cgout \
	ann2.post.exp ann2.stderr.exp ann2.vgtest ann2.cgout \
	chdir.vgtest chdir.stderr.exp \
	clreq.vgtest clreq.stderr.exp \
	dlclose.vgtest dlclose.stderr.exp dlclose.stdout.exp \
	notpower2.vgtest notpower2.stderr.exp \
	test.c a.c \
	wrap5.vgtest wrap5.stderr.exp wrap5.stdout.exp

check_PROGRAMS = \
	chdir clreq dlclose myprint.so

AM_CFLAGS   += $(AM_FLAG_M3264_PRI)
AM_CXXFLAGS += $(AM_FLAG_M3264_PRI)

# C ones
if !VGCONF_OS_IS_FREEBSD
dlclose_LDADD		= -ldl
endif
if VGCONF_OS_IS_DARWIN
myprint_so_LDFLAGS	= $(AM_CFLAGS) -dynamic -dynamiclib -all_load -fpic
else
myprint_so_LDFLAGS	= $(AM_CFLAGS) -shared -fPIC
endif
myprint_so_CFLAGS	= $(AM_CFLAGS) -fPIC
