
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr

noinst_HEADERS = scalar.h

EXTRA_DIST = \
	aio.stderr.exp aio.vgtest \
	aligned_alloc.stderr.exp aligned_alloc.vgtest \
	bug437790.vgtest bug437790.stdout.exp bug437790.stderr.exp \
	deep_badparam.stderr.exp deep_badparam.stdout.exp deep_badparam.vgtest \
	env.stderr.exp env.vgtest \
	ioctl-tiocsbrk.stderr.exp ioctl-tiocsbrk.vgtest \
	pth-supp.stderr.exp pth-supp.vgtest \
	pth-undocumented.stderr.exp pth-undocumented.stdout.exp pth-undocumented.vgtest \
	mkfifo.stderr.exp mkfifo.vgtest \
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

