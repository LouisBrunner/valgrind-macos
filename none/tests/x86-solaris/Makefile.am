
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = \
	coredump_single_thread_mdb \
	coredump_single_thread_sse_mdb \
	filter_stderr
	

EXTRA_DIST = \
	cet_nops_fs.stderr.exp cet_nops_fs.stdout.exp cet_nops_fs.vgtest \
	coredump_single_thread.post.exp coredump_single_thread.stderr.exp \
	coredump_single_thread.stdout.exp coredump_single_thread.vgtest \
	coredump_single_thread_sse.post.exp coredump_single_thread_sse.stderr.exp \
	coredump_single_thread_sse.stdout.exp coredump_single_thread_sse.vgtest \
	syscalls.stderr.exp syscalls.stdout.exp syscalls.vgtest

check_PROGRAMS = \
	cet_nops_fs \
	coredump_single_thread \
	coredump_single_thread_sse \
	syscalls

AM_CFLAGS    += @FLAG_M32@
AM_CXXFLAGS  += @FLAG_M32@
AM_CCASFLAGS += @FLAG_M32@

