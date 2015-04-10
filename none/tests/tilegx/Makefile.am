
include $(top_srcdir)/Makefile.tool-tests.am

EXTRA_DIST =

check_PROGRAMS = \
	allexec

AM_CFLAGS    += @FLAG_M64@
AM_CXXFLAGS  += @FLAG_M64@
AM_CCASFLAGS += @FLAG_M64@

allexec_CFLAGS          = $(AM_CFLAGS) @FLAG_W_NO_NONNULL@
