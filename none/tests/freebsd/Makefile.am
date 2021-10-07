
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr
EXTRA_DIST = \
	auxv.vgtest \
	auxv.stderr.exp \
	auxv.stderr.exp-32on64 \
	auxv.stderr.exp-freebsd13 \
	osrel.vgtest \
	osrel.stderr.exp \
	osrel.stdout.exp \
	swapcontext.vgtest \
	swapcontext.stderr.exp \
	swapcontext.stdout.exp

check_PROGRAMS = \
	auxv osrel swapcontext

AM_CFLAGS   += $(AM_FLAG_M3264_PRI)
AM_CXXFLAGS += $(AM_FLAG_M3264_PRI)

auxv_CFLAGS = ${AM_CFLAGS}
osrel_CFLAGS = ${AM_CFLAGS}
swapcontext_CFLAGS = ${AM_CFLAGS}

