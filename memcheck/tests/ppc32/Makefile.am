
noinst_SCRIPTS = filter_stderr

EXTRA_DIST = $(noinst_SCRIPTS) \
	power_ISA2_05.stderr.exp power_ISA2_05.stdout.exp power_ISA2_05.vgtest

check_PROGRAMS = \
	power_ISA2_05

AM_CFLAGS   = $(WERROR) -Winline -Wall -Wshadow -g -I$(top_srcdir)/include @FLAG_M32@
AM_CXXFLAGS = $(AM_CFLAGS) @FLAG_M32@
