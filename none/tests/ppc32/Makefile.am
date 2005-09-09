
noinst_SCRIPTS = filter_cpuid filter_stderr filter_int

CLEANFILES = $(addsuffix .c,$(INSN_TESTS))

EXTRA_DIST = $(noinst_SCRIPTS) \
	lsw.stderr.exp lsw.stdout.exp lsw.vgtest

check_PROGRAMS = \
	lsw

AM_CFLAGS   = $(WERROR) -Winline -Wall -Wshadow -g -I$(top_srcdir)/include
AM_CXXFLAGS = $(AM_CFLAGS)
