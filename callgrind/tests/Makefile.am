# For AM_FLAG_M3264_PRI
include $(top_srcdir)/Makefile.flags.am

SUBDIRS = .
DIST_SUBDIRS = .

noinst_SCRIPTS = filter_stderr

EXTRA_DIST = clreq.vgtest clreq.stderr.exp 

check_PROGRAMS = clreq

AM_CPPFLAGS = -I$(top_srcdir)/include
AM_CFLAGS   = $(WERROR) -Winline -Wall -Wshadow -g $(AM_FLAG_M3264_PRI)


