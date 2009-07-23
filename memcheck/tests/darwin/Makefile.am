
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr

noinst_HEADERS = scalar.h

EXTRA_DIST = \
	aio.stderr.exp aio.vgtest \
	env.stderr.exp env.vgtest \
	pth-supp.stderr.exp pth-supp.vgtest \
	scalar.stderr.exp scalar.vgtest \
	scalar_fork.stderr.exp scalar_fork.vgtest \
	scalar_nocancel.stderr.exp scalar_nocancel.vgtest \
	scalar_vfork.stderr.exp scalar_vfork.vgtest

check_PROGRAMS = \
	aio \
	env \
	pth-supp \
	scalar \
	scalar_fork \
	scalar_nocancel \
	scalar_vfork


AM_CFLAGS    += @FLAG_M32@ $(FLAG_MMMX) $(FLAG_MSSE)
AM_CXXFLAGS  += @FLAG_M32@ $(FLAG_MMMX) $(FLAG_MSSE)
AM_CCASFLAGS += @FLAG_M32@

