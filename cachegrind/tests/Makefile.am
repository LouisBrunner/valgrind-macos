
include $(top_srcdir)/Makefile.tool-tests.am

SUBDIRS = .
if VGCONF_ARCHS_INCLUDE_X86
SUBDIRS += x86
endif

DIST_SUBDIRS = x86 .

dist_noinst_SCRIPTS = \
	filter_stderr \
	filter_cachesim_discards \
	filter_clreq3

# Note that `test.c` and `a.c` are not compiled.
# They just serve as input for cg_annotate in `ann1a` and `ann1b`.
EXTRA_DIST = \
	ann-diff1.post.exp ann-diff1.stderr.exp ann-diff1.vgtest \
	ann-diff2.post.exp ann-diff2.stderr.exp ann-diff2.vgtest \
		ann-diff2a.cgout ann-diff2b.cgout \
		ann-diff2-aux/ann-diff2-basic.rs \
	ann-diff3.post.exp ann-diff3.stderr.exp ann-diff3.vgtest \
	ann-diff4.post.exp ann-diff4.stderr.exp ann-diff4.vgtest \
		ann-diff4a.cgout ann-diff4b.cgout \
		ann-diff4a-aux/w.rs ann-diff4a-aux/x.rs ann-diff4a-aux/y.rs \
		ann-diff4a-aux/z.rs \
		ann-diff4b-aux/w.rs ann-diff4b-aux/x.rs ann-diff4b-aux/y.rs \
	ann-merge1.post.exp ann-merge1.stderr.exp ann-merge1.vgtest \
		ann-merge1a.cgout ann-merge1b.cgout \
		ann-merge-x.rs ann-merge-y.rs \
	ann-merge2.post.exp ann-merge2.stderr.exp ann-merge2.vgtest \
	ann1a.post.exp ann1a.stderr.exp ann1a.vgtest ann1.cgout \
	ann1b.post.exp ann1b.stderr.exp ann1b.vgtest ann1b.cgout \
	ann2.post.exp ann2.stderr.exp ann2.vgtest ann2.cgout \
		ann2-basic.rs ann2-more-recent-than-cgout.rs \
		ann2-negatives.rs ann2-past-the-end.rs \
	chdir.vgtest chdir.stderr.exp \
	clreq.vgtest clreq.stderr.exp \
	clreq2a.vgtest clreq2a.stderr.exp \
	clreq2b.vgtest clreq2b.stderr.exp \
	clreq3.vgtest clreq3.stderr.exp \
	dlclose.vgtest dlclose.stderr.exp dlclose.stdout.exp \
	notpower2.vgtest notpower2.stderr.exp \
	test.c a.c \
	wrap5.vgtest wrap5.stderr.exp wrap5.stdout.exp

check_PROGRAMS = \
	chdir clreq clreq2 dlclose myprint.so

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
