
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = \
	filter_stderr

EXTRA_DIST = \
	bug345887.stderr.exp bug345887.vgtest \
	cet_nops_fs.stderr.exp cet_nops_fs.stdout.exp cet_nops_fs.vgtest \
	cet_nops_gs.stderr.exp cet_nops_gs.stdout.exp cet_nops_gs.vgtest \
	hang.stderr.exp hang.vgtest \
	seg_override.stderr.exp seg_override.stdout.exp seg_override.vgtest \
	sigcontext.stdout.exp sigcontext.stderr.exp sigcontext.vgtest

check_PROGRAMS = \
	bug345887 \
	cet_nops_fs \
	cet_nops_gs \
	hang \
	seg_override \
	sigcontext

AM_CFLAGS    += @FLAG_M32@ $(FLAG_MMMX) $(FLAG_MSSE)
AM_CXXFLAGS  += @FLAG_M32@ $(FLAG_MMMX) $(FLAG_MSSE)
AM_CCASFLAGS += @FLAG_M32@
