
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = \
	filter_stderr

EXTRA_DIST = \
	bug345887.stderr.exp bug345887.vgtest \
	map_32bits.stderr.exp map_32bits.vgtest

check_PROGRAMS = \
	bug345887 map_32bits

AM_CFLAGS    += @FLAG_M64@
AM_CXXFLAGS  += @FLAG_M64@
AM_CCASFLAGS += @FLAG_M64@
