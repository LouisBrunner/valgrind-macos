
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr

EXTRA_DIST = \
	access_extended.stderr.exp access_extended.vgtest \
	apple-main-arg.stderr.exp apple-main-arg.vgtest \
	bug254164.stderr.exp bug254164.vgtest \
	rlimit.stderr.exp rlimit.vgtest

check_PROGRAMS = \
	access_extended \
	apple-main-arg \
	bug254164 \
	rlimit


AM_CFLAGS   += $(AM_FLAG_M3264_PRI)
AM_CXXFLAGS += $(AM_FLAG_M3264_PRI)

