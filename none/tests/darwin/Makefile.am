
include $(top_srcdir)/Makefile.tool-tests.am

noinst_SCRIPTS = filter_stderr

EXTRA_DIST = $(noinst_SCRIPTS) \
	apple-main-arg.stderr.exp apple-main-arg.vgtest \
	rlimit.stderr.exp rlimit.vgtest

check_PROGRAMS = \
	apple-main-arg \
	rlimit


AM_CFLAGS   += $(AM_FLAG_M3264_PRI)
AM_CXXFLAGS += $(AM_FLAG_M3264_PRI)

