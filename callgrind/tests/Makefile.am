# For AM_FLAG_M3264_PRI
include $(top_srcdir)/Makefile.flags.am

SUBDIRS = .
DIST_SUBDIRS = .

noinst_SCRIPTS = filter_stderr

EXTRA_DIST = $(noinst_SCRIPTS) \
             clreq.vgtest clreq.stderr.exp \
             simwork1.vgtest simwork1.stdout.exp simwork1.stderr.exp \
             simwork2.vgtest simwork2.stdout.exp simwork2.stderr.exp \
             simwork3.vgtest simwork3.stdout.exp simwork3.stderr.exp \
             threads.vgtest threads.stderr.exp

check_PROGRAMS = clreq simwork threads

AM_CPPFLAGS = -I$(top_srcdir)/include
AM_CFLAGS   = $(WERROR) -Winline -Wall -Wshadow -g $(AM_FLAG_M3264_PRI)

threads_LDADD = -lpthread
