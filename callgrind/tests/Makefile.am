
include $(top_srcdir)/Makefile.tool-tests.am

SUBDIRS = .
DIST_SUBDIRS = .

dist_noinst_SCRIPTS = filter_stderr

EXTRA_DIST = \
	clreq.vgtest clreq.stderr.exp \
	simwork1.vgtest simwork1.stdout.exp simwork1.stderr.exp \
	simwork2.vgtest simwork2.stdout.exp simwork2.stderr.exp \
	simwork3.vgtest simwork3.stdout.exp simwork3.stderr.exp \
	notpower2.vgtest notpower2.stderr.exp \
	notpower2-wb.vgtest notpower2-wb.stderr.exp \
	notpower2-hwpref.vgtest notpower2-hwpref.stderr.exp \
	notpower2-use.vgtest notpower2-use.stderr.exp \
	threads.vgtest threads.stderr.exp \
	threads-use.vgtest threads-use.stderr.exp

check_PROGRAMS = clreq simwork threads

AM_CFLAGS   += $(AM_FLAG_M3264_PRI)
AM_CXXFLAGS += $(AM_FLAG_M3264_PRI)

threads_LDADD = -lpthread
