
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = \
	filter_stderr

EXTRA_DIST = \
	bug345887.stderr.exp bug345887.vgtest \
	cet_nops_fs.stderr.exp cet_nops_fs.stdout.exp cet_nops_fs.vgtest \
	cet_nops_gs.stderr.exp cet_nops_gs.stdout.exp cet_nops_gs.vgtest \
	map_32bits.stderr.exp map_32bits.vgtest

check_PROGRAMS = \
	bug345887 \
	cet_nops_fs \
	cet_nops_gs \
	map_32bits

AM_CFLAGS    += @FLAG_M64@
AM_CXXFLAGS  += @FLAG_M64@
AM_CCASFLAGS += @FLAG_M64@
