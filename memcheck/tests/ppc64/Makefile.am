
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr

EXTRA_DIST = $(noinst_SCRIPTS) \
	power_ISA2_05.stderr.exp power_ISA2_05.stdout.exp power_ISA2_05.vgtest

check_PROGRAMS = \
	power_ISA2_05

power_ISA2_05_CFLAGS = $(AM_CFLAGS) $(WERROR) -Winline -Wall -Wshadow -g \
		-I$(top_srcdir)/include @FLAG_M64@
