
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr

EXTRA_DIST = \
	v6int.stdout.exp v6int.stderr.exp v6int.vgtest

check_PROGRAMS = \
	v6int

AM_CFLAGS    += @FLAG_M32@
AM_CXXFLAGS  += @FLAG_M32@
AM_CCASFLAGS += @FLAG_M32@

# need to specify cpu here because inline assembly in v6int.c
# contains various v6 and v7 insns, which the assembler by
# default rejects.
v6int_CFLAGS 	= $(AM_CFLAGS) -g -O -mcpu=cortex-a8
