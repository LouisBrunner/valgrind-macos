
include $(top_srcdir)/Makefile.tool-tests.am

SUBDIRS = .
DIST_SUBDIRS = .

dist_noinst_SCRIPTS = filter_stderr

EXTRA_DIST = \
	ann1.post.exp ann1.stderr.exp ann1.vgtest \
	ann2.post.exp ann2.stderr.exp ann2.vgtest \
	clreq.vgtest clreq.stderr.exp \
	simwork1.vgtest simwork1.stdout.exp simwork1.stderr.exp \
	simwork2.vgtest simwork2.stdout.exp simwork2.stderr.exp \
	simwork3.vgtest simwork3.stdout.exp simwork3.stderr.exp \
	simwork-both.vgtest simwork-both.stdout.exp simwork-both.stderr.exp \
	simwork-branch.vgtest simwork-branch.stdout.exp simwork-branch.stderr.exp \
	simwork-cache.vgtest simwork-cache.stdout.exp simwork-cache.stderr.exp \
	notpower2.vgtest notpower2.stderr.exp \
	notpower2-wb.vgtest notpower2-wb.stderr.exp \
	notpower2-hwpref.vgtest notpower2-hwpref.stderr.exp \
	notpower2-use.vgtest notpower2-use.stderr.exp \
	threads.vgtest threads.stderr.exp \
	threads-use.vgtest threads-use.stderr.exp \
	find-source.vgtest find-source.stderr.exp find-source.post.exp

check_PROGRAMS = clreq simwork threads

AM_CFLAGS   += $(AM_FLAG_M3264_PRI)
AM_CXXFLAGS += $(AM_FLAG_M3264_PRI)

threads_LDADD = -lpthread
