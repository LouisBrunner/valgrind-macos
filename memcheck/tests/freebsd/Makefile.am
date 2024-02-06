
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr filter_pts dump_stdout filter_sigwait \
	filter_scalar filter_realpathat filter_fstat filter_eventfd2 \
	toucher1 toucher2

EXTRA_DIST = \
	access.vgtest \
	access.stderr.exp \
	aio.vgtest aio.stderr.exp \
	aiov.vgtest aiov.stderr.exp \
	aio_read.vgtest aio_read.stderr.exp aio_read.stdout.exp \
	aio_read.in \
	aligned_alloc.vgtest aligned_alloc.stderr.exp \
	aligned_alloc_xml.vgtest aligned_alloc_xml.stderr.exp \
	aligned_allocs_supp.vgtest \
	aligned_allocs_supp.stderr.exp \
	aligned_allocs_supp.supp \
	bug464476.vgtest \
	bug464476.stderr.exp \
	bug464476.stdout.exp \
	bug464476_abs_symlink.vgtest \
	bug464476_abs_symlink.stderr.exp \
	bug464476_abs_symlink.stdout.exp \
	bug464476_rel_symlink.vgtest \
	bug464476_rel_symlink.stderr.exp \
	bug464476_rel_symlink.stdout.exp \
	bug470713.vgtest bug470713.stderr.exp \
		bug470713.stdout.exp \
	capsicum.vgtest \
	capsicum.stderr.exp \
	chflags.vgtest\
	chflags.stderr.exp \
		chflags.stderr.exp-x86 \
	chmod_chown.vgtest \
	chmod_chown.stderr.exp \
	close_range.vgtest close_range.stderr.exp \
	delete_sized_mismatch.vgtest \
	delete_sized_mismatch.stderr.exp \
	delete_sized_mismatch_xml.vgtest \
	delete_sized_mismatch_xml.stderr.exp \
	errno_aligned_allocs.vgtest \
	errno_aligned_allocs.stderr.exp \
	eventfd1.vgtest \
	eventfd1.stderr.exp eventfd1.stdout.exp \
	eventfd2.vgtest \
	eventfd2.stderr.exp \
	extattr.vgtest \
	extattr.stderr.exp \
	fexecve.vgtest \
	fexecve.stderr.exp \
	file_locking_wait6.vgtest \
	file_locking_wait6.stderr.exp \
	get_set_context.vgtest \
	get_set_context.stderr.exp \
	get_set_context.stderr.exp-x86 \
	get_set_login.vgtest \
	get_set_login.stderr.exp \
	getfh.vgtest \
	getfh.stderr.exp \
	getfsstat.vgtest \
	getfsstat.stderr.exp \
	getfsstat.supp \
		getfsstat.stderr.exp-x86 \
	kqueue.vgtest \
	kqueue.stderr.exp \
	kqueue.stdout.exp \
	kqueuex.vgtest \
	kqueuex.stderr.exp \
	kqueuex.stdout.exp \
	linkat.vgtest \
	linkat.stderr.exp \
        memalign.vgtest memalign.stderr.exp \
	memalign_supp.vgtest memalign_supp.stderr.exp \
	memalign_supp.supp \
	misc.vgtest \
	misc.stderr.exp \
	openpty.vgtest \
	openpty.stderr.exp \
	pdfork_pdkill.vgtest \
	pdfork_pdkill.stderr.exp \
	realpathat.vgtest \
	realpathat.stderr.exp \
	revoke.vgtest \
	revoke.stderr.exp \
	scalar.h scalar.vgtest \
	scalar.stderr.exp \
		scalar.stderr.exp-x86 \
	scalar_abort2.vgtest \
	scalar_13_plus.vgtest \
	scalar_13_plus.stderr.exp \
	scalar_15_plus.vgtest \
	scalar_15_plus.stderr.exp \
	scalar_abort2.stderr.exp \
	scalar_fork.vgtest \
	scalar_fork.stderr.exp \
	scalar_pdfork.vgtest \
	scalar_pdfork.stderr.exp \
	scalar_thr_exit.vgtest \
	scalar_thr_exit.stderr.exp \
	scalar_vfork.vgtest \
	scalar_vfork.stderr.exp \
	sctp.vgtest \
	sctp.stderr.exp \
	sctp.stdout.exp \
	sctp2.vgtest \
	sctp2.stderr.exp \
	sctp2.stdout.exp \
	setproctitle.vgtest \
	setproctitle.stderr.exp \
	setproctitle.stdout.exp \
	sigwait.vgtest \
	sigwait.stdout.exp \
	sigwait.stderr.exp \
		sigwait.stderr.exp-x86 \
	stat.vgtest \
	stat.stderr.exp \
		stat.stderr.exp-x86 \
	statfs.vgtest \
	statfs.stderr.exp \
	static_allocs.vgtest \
	static_allocs.stderr.exp \
	strlcat_strlcpy.vgtest \
	strlcat_strlcpy.stderr.exp \
	supponlyobj.vgtest \
	supponlyobj.stderr.exp \
	supponlyobj.supp \
	timerfd.vgtest \
	timerfd.stderr.exp \
	timerfd.stdout.exp \
	timing_safe.vgtest \
	timing_safe.stderr.exp \
	utimens.vgtest \
	utimens.stderr.exp \
	utimes.vgtest \
	utimes.stderr.exp-x86 \
	utimes.stderr.exp

check_PROGRAMS = \
	access aio aio_read aligned_alloc bug464476 bug470713 \
	capsicum chflags \
	chmod_chown delete_sized_mismatch errno_aligned_allocs \
	extattr \
	fexecve \
	file_locking_wait6 \
	get_set_context get_set_login getfh \
	kqueue linkat memalign misc \
	openpty \
	pdfork_pdkill getfsstat inlinfo inlinfo_nested.so \
	revoke scalar \
	scalar_fork scalar_thr_exit scalar_abort2 scalar_pdfork \
	scalar_vfork \
	sctp sctp2 \
	setproctitle \
	sigwait \
	stat \
	statfs \
	static_allocs \
	strlcat_strlcpy \
	timing_safe \
	utimens \
	utimes

AM_CFLAGS   += $(AM_FLAG_M3264_PRI)
AM_CXXFLAGS += $(AM_FLAG_M3264_PRI)

if HAVE_AIO_READV
check_PROGRAMS += aiov
endif

inlinfo_SOURCES = inlinfo.c
inlinfo_DEPENDENCIES = inlinfo_nested.so
inlinfo_LDFLAGS = -Wl,-rpath,$(top_builddir)/memcheck/tests/freebsd
inlinfo_LDADD = inlinfo_nested.so
inlinfo_nested_so_SOURCES = inlinfo_nested.c
inlinfo_nested_so_CFLAGS = $(AM_CFLAGS) -fPIC @FLAG_W_NO_UNINITIALIZED@
inlinfo_nested_so_LDFLAGS = -Wl,-rpath,$(top_builddir)/memcheck/tests/freebsd -shared -fPIC

bug464476_SOURCES = bug464476.cpp
bug470713_SOURCES = bug470713.cpp

if FREEBSD_VERS_13_PLUS
check_PROGRAMS += realpathat scalar_13_plus eventfd1 eventfd2
scalar_13_plus_CFLAGS = ${AM_CFLAGS} -g
endif

if FREEBSD_VERS_15_PLUS
check_PROGRAMS += kqueuex scalar_15_plus timerfd
scalar_15_plus_CFLAGS = ${AM_CFLAGS} -g
timerfd_LDFLAGS = -lm
endif

if HAVE_CLOSE_RANGE
check_PROGRAMS += close_range
endif

aligned_alloc_CFLAGS = ${AM_CFLAGS} @FLAG_W_NO_NON_POWER_OF_TWO_ALIGNMENT@

close_range_CFLAGS = ${AM_CFLAGS} @FLAG_W_NO_UNUSED_BUT_SET_VARIABLE@

delete_sized_mismatch_CXXFLAGS = ${AM_CXXFLAGS} --std=c++14
delete_sized_mismatch_SOURCES = delete_sized_mismatch.cpp

errno_aligned_allocs_CFLAGS = ${AM_CFLAGS} @FLAG_W_NO_NON_POWER_OF_TWO_ALIGNMENT@

extattr_CFLAGS = ${AM_CFLAGS} @FLAG_W_NO_UNUSED_BUT_SET_VARIABLE@

memalign_CFLAGS = ${AM_CFLAGS} @FLAG_W_NO_NON_POWER_OF_TWO_ALIGNMENT@

openpty_LDFLAGS = ${AM_LDFLAGS} -lutil

scalar_CFLAGS = ${AM_CFLAGS} -g
