
# For AM_FLAG_M3264_PRI
include $(top_srcdir)/Makefile.flags.am

EXTRA_DIST = \
	many-xpts.vgperf

check_PROGRAMS = \
	many-xpts

AM_CFLAGS   = $(WERROR) -Winline -Wall -Wshadow -g -O $(AM_FLAG_M3264_PRI)
AM_CPPFLAGS = -I$(top_srcdir) -I$(top_srcdir)/include -I$(top_builddir)/include
AM_CXXFLAGS = $(AM_CFLAGS)
