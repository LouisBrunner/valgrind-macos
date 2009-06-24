
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = \
	filter_scalar_exit_group \
	filter_stderr

noinst_HEADERS = scalar.h

EXTRA_DIST = \
	bug133694.vgtest bug133694.stderr.exp bug133694.stdout.exp \
	int3-x86.vgtest int3-x86.stderr.exp int3-x86.stdout.exp \
	scalar.stderr.exp scalar.vgtest \
	scalar_fork.stderr.exp scalar_fork.vgtest \
	scalar_exit_group.stderr.exp \
	scalar_exit_group.vgtest \
	scalar_supp.stderr.exp \
	scalar_supp.vgtest scalar_supp.supp \
	scalar_vfork.stderr.exp scalar_vfork.vgtest

check_PROGRAMS = \
	bug133694 \
	int3-x86 \
	scalar scalar_exit_group scalar_fork scalar_supp scalar_vfork


AM_CFLAGS    += @FLAG_M32@ $(FLAG_MMMX) $(FLAG_MSSE)
AM_CXXFLAGS  += @FLAG_M32@ $(FLAG_MMMX) $(FLAG_MSSE)
AM_CCASFLAGS += @FLAG_M32@

