
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr filter_scalar

noinst_HEADERS = scalar.h

EXTRA_DIST = \
	aio.stderr.exp aio.vgtest \
	aligned_alloc.vgtest \
	bug437790.vgtest bug437790.stdout.exp \
	deep_badparam.stderr.exp deep_badparam.stdout.exp deep_badparam.vgtest \
	env.vgtest \
	ioctl-tiocsbrk.vgtest \
	pth-supp.vgtest \
	pth-undocumented.vgtest \
	mkfifo.vgtest \
	scalar.stderr.exp scalar.vgtest \
	scalar_fork.stderr.exp scalar_fork.vgtest \
	scalar_nocancel.stderr.exp scalar_nocancel.vgtest \
	scalar_vfork.stderr.exp scalar_vfork.vgtest

check_PROGRAMS = \
	aio \
	aligned_alloc \
	bug437790 \
	deep_badparam \
	env \
	ioctl-tiocsbrk \
	pth-supp \
	pth-undocumented \
	mkfifo \
	scalar \
	scalar_fork \
	scalar_nocancel \
	scalar_vfork


AM_CFLAGS    += $(AM_FLAG_M3264_PRI) $(FLAG_MMMX) $(FLAG_MSSE)
AM_CXXFLAGS  += $(AM_FLAG_M3264_PRI) $(FLAG_MMMX) $(FLAG_MSSE)
AM_CCASFLAGS += $(AM_FLAG_M3264_PRI)

pth_undocumented_CFLAGS		= ${AM_CFLAGS} @FLAG_W_NO_DEPRECATED_DECLARATIONS@
mkfifo_CFLAGS			= ${AM_CFLAGS} @FLAG_W_NO_DEPRECATED_DECLARATIONS@
scalar_CFLAGS			= ${AM_CFLAGS} @FLAG_W_NO_DEPRECATED_DECLARATIONS@
scalar_nocancel_CFLAGS		= ${AM_CFLAGS} @FLAG_W_NO_DEPRECATED_DECLARATIONS@
