
noinst_SCRIPTS = vg_perf

EXTRA_DIST = $(noinst_SCRIPTS) \
	ffbench.vgperf \
	sarp.vgperf

check_PROGRAMS = \
	ffbench sarp

AM_CFLAGS   = $(WERROR) -Winline -Wall -Wshadow -g -O
AM_CPPFLAGS = -I$(top_srcdir) -I$(top_srcdir)/include -I$(top_builddir)/include
AM_CXXFLAGS = $(AM_CFLAGS)

# Extra stuff
ffbench_LDADD		= -lm

