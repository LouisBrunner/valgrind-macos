
noinst_SCRIPTS = vg_perf

EXTRA_DIST = $(noinst_SCRIPTS) \
	bigcode.vgperf \
	bz2.vgperf \
	fbench.vgperf \
	ffbench.vgperf \
	sarp.vgperf

check_PROGRAMS = \
	bigcode bz2 fbench ffbench sarp

AM_CFLAGS   = $(WERROR) -Winline -Wall -Wshadow -g -O
AM_CPPFLAGS = -I$(top_srcdir) -I$(top_srcdir)/include -I$(top_builddir)/include
AM_CXXFLAGS = $(AM_CFLAGS)

# Extra stuff
fbench_LDADD		= -lm
ffbench_LDADD		= -lm

