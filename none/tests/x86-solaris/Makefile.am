
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = \
	coredump_single_thread_mdb \
	coredump_single_thread_sse_mdb \
	filter_stderr

EXTRA_DIST = \
	cet_nops_fs.stdout.exp cet_nops_fs.vgtest \
	coredump_single_thread.post.exp coredump_single_thread.stderr.exp \
	coredump_single_thread.vgtest \
	coredump_single_thread_sse.post.exp coredump_single_thread_sse.stderr.exp \
	coredump_single_thread_sse.vgtest \
	coredump_single_thread_sse.post.exp-illumos \
	syscalls.vgtest

check_PROGRAMS = \
	cet_nops_fs \
	coredump_single_thread \
	coredump_single_thread_sse \
	syscalls

AM_CFLAGS    += @FLAG_M32@ ${FLAG_MSSE}
AM_CXXFLAGS  += @FLAG_M32@
AM_CCASFLAGS += @FLAG_M32@

coredump_single_thread_CFLAGS = ${AM_CFLAGS} @FLAG_W_NO_DEPRECATED@

