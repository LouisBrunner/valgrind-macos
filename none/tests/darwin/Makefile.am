
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr

EXTRA_DIST = \
	access_extended.stderr.exp access_extended.vgtest \
	apple-main-arg.vgtest \
	bug228343.stdout.exp bug228343.vgtest \
	bug254164.vgtest \
	bug390269.vgtest \
	bug411369.vgtest \
	procfs-cmdline-exe.stderr.exp-without-readlinkat \
	procfs-cmdline-exe.stderr.exp-with-readlinkat \
	procfs-cmdline-exe.vgtest \
	rlimit.stderr.exp rlimit.vgtest

check_PROGRAMS = \
	access_extended \
	apple-main-arg \
	bug228343 \
	bug254164 \
	bug390269 \
	bug411369 \
	procfs-cmdline-exe \
	rlimit


AM_CFLAGS   += $(AM_FLAG_M3264_PRI)
AM_CXXFLAGS += $(AM_FLAG_M3264_PRI)

access_extended_CFLAGS	= ${AM_CFLAGS} @FLAG_W_NO_DEPRECATED_DECLARATIONS@
bug228343_CFLAGS	= ${AM_CFLAGS} @FLAG_W_NO_DEPRECATED_DECLARATIONS@
