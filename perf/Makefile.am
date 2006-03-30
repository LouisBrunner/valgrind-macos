
# For AM_FLAG_M3264_PRI
include $(top_srcdir)/Makefile.flags.am

noinst_SCRIPTS = vg_perf

EXTRA_DIST = $(noinst_SCRIPTS) \
	bigcode1.vgperf \
	bigcode2.vgperf \
	bz2.vgperf \
	fbench.vgperf \
	ffbench.vgperf \
	heap.vgperf \
	sarp.vgperf \
	tinycc.vgperf \
	test_input_for_tinycc.c

check_PROGRAMS = \
	bigcode bz2 fbench ffbench heap sarp tinycc

AM_CFLAGS   = $(WERROR) -Winline -Wall -Wshadow -g -O $(AM_FLAG_M3264_PRI)
AM_CPPFLAGS = -I$(top_srcdir) -I$(top_srcdir)/include -I$(top_builddir)/include
AM_CXXFLAGS = $(AM_CFLAGS)

# Extra stuff
fbench_CFLAGS   = $(AM_FLAG_M3264_PRI) -g -O2
fbench_LDADD	= 
ffbench_LDADD	= -lm

tinycc_CFLAGS	= $(AM_CFLAGS) -Wno-shadow
