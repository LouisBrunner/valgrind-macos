
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr

EXTRA_DIST = \
	access_extended.stderr.exp access_extended.vgtest \
	apple-main-arg.stderr.exp apple-main-arg.vgtest \
	bug228343.stderr.exp bug228343.stdout.exp bug228343.vgtest \
	bug254164.stderr.exp bug254164.vgtest \
	bug390269.stderr.exp bug390269.vgtest \
	rlimit.stderr.exp rlimit.vgtest

check_PROGRAMS = \
	access_extended \
	apple-main-arg \
	bug228343 \
	bug254164 \
	bug390269 \
	rlimit


AM_CFLAGS   += $(AM_FLAG_M3264_PRI)
AM_CXXFLAGS += $(AM_FLAG_M3264_PRI)

