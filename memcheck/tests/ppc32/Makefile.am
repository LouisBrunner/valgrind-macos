
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr

EXTRA_DIST = $(noinst_SCRIPTS) \
	power_ISA2_05.stderr.exp power_ISA2_05.stdout.exp power_ISA2_05.vgtest \
	power_ISA2_05.stdout.exp_Without_FPPO \
	power_ISA2_07.stdout.exp \
	power_ISA2_07.stderr.exp power_ISA2_07.vgtest

check_PROGRAMS = \
	power_ISA2_05 power_ISA2_07

power_ISA2_05_CFLAGS = $(AM_CFLAGS) $(WERROR) -Winline -Wall -Wshadow -g \
		-I$(top_srcdir)/include @FLAG_M32@

if HAS_ISA_2_07
BUILD_FLAGS_ISA_2_07 =  -mhtm -mcpu=power8
ISA_2_07_FLAG = -DHAS_ISA_2_07
else
BUILD_FLAGS_ISA_2_07 =
ISA_2_07_FLAG =
endif

power_ISA2_07_CFLAGS = $(AM_CFLAGS) $(WERROR) -Winline -Wall -Wshadow -g \
		$(ISA_2_07_FLAG)  -I$(top_srcdir)/include @FLAG_M32@
