
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr

EXTRA_DIST = \
	brk.stderr.exp brk.vgtest \
	capget.vgtest capget.stderr.exp capget.stderr.exp2 capget.stderr.exp3 \
	debuginfod-check.stderr.exp debuginfod-check.vgtest.in \
	debuginfod-check.pl \
	dlclose_leak-no-keep.stderr.exp dlclose_leak-no-keep.stdout.exp \
	    dlclose_leak-no-keep.vgtest \
	dlclose_leak.stderr.exp dlclose_leak.stdout.exp \
	    dlclose_leak.vgtest \
	ioctl-tiocsig.vgtest ioctl-tiocsig.stderr.exp \
	lsframe1.vgtest lsframe1.stdout.exp lsframe1.stderr.exp \
	lsframe2.vgtest lsframe2.stdout.exp lsframe2.stderr.exp \
	memfd_create.vgtest memfd_create.stderr.exp \
	rfcomm.vgtest rfcomm.stderr.exp \
	sigqueue.vgtest sigqueue.stderr.exp \
	stack_changes.stderr.exp stack_changes.stdout.exp \
	    stack_changes.stdout.exp2 stack_changes.vgtest \
	stack_switch.stderr.exp stack_switch.vgtest \
	syscalls-2007.vgtest syscalls-2007.stderr.exp \
	syslog-syscall.vgtest syslog-syscall.stderr.exp \
	sys-copy_file_range.vgtest sys-copy_file_range.stderr.exp \
	sys-openat.vgtest sys-openat.stderr.exp sys-openat.stdout.exp \
	sys-statx.vgtest sys-statx.stderr.exp \
	timerfd-syscall.vgtest timerfd-syscall.stderr.exp \
	with-space.stderr.exp with-space.stdout.exp with-space.vgtest \
	proc-auxv.vgtest proc-auxv.stderr.exp getregset.vgtest \
	getregset.stderr.exp getregset.stdout.exp \
	sys-preadv_pwritev.vgtest sys-preadv_pwritev.stderr.exp \
	sys-preadv2_pwritev2.vgtest sys-preadv2_pwritev2.stderr.exp \
	sys-execveat.vgtest sys-execveat.stderr.exp sys-execveat.stdout.exp \
	enomem.vgtest enomem.stderr.exp enomem.stdout.exp

check_PROGRAMS = \
	brk \
	capget \
	check_preadv2_pwritev2 \
	debuginfod-check \
	dlclose_leak dlclose_leak_so.so \
	ioctl-tiocsig \
	getregset \
	lsframe1 \
	lsframe2 \
	rfcomm \
	sigqueue \
	stack_switch \
	syscalls-2007 \
	syslog-syscall \
	sys-statx \
	timerfd-syscall \
	proc-auxv \
	sys-execveat \
	check_execveat \
	enomem

if HAVE_AT_FDCWD
check_PROGRAMS += sys-openat
endif

if HAVE_MEMFD_CREATE
check_PROGRAMS += memfd_create
endif

if HAVE_COPY_FILE_RANGE
        check_PROGRAMS += sys-copy_file_range
endif

if HAVE_PREADV_PWRITEV
        check_PROGRAMS += sys-preadv_pwritev
endif

if HAVE_PREADV2_PWRITEV2
        check_PROGRAMS += sys-preadv2_pwritev2
endif

if HAVE_SETCONTEXT
        check_PROGRAMS += stack_changes
endif

AM_CFLAGS   += $(AM_FLAG_M3264_PRI)
AM_CXXFLAGS += $(AM_FLAG_M3264_PRI)

stack_switch_LDADD    = -lpthread
timerfd_syscall_LDADD = -lrt

# Build shared object for dlclose_leak
dlclose_leak_so_so_SOURCES = dlclose_leak_so.c
dlclose_leak_so_so_CFLAGS  = $(AM_CFLAGS) -fpic -g -O0
dlclose_leak_so_so_LDFLAGS  = -fpic $(AM_FLAG_M3264_PRI) -shared -Wl,-soname \
                              -Wl,dlclose_leak_so.so

dlclose_leak_SOURCES          = dlclose_leak.c
dlclose_leak_DEPENDENCIES     = dlclose_leak_so.so
# Do NOT uncomment the below line: we must not link with the .so,
# in order to properly test a 'fully dynamic' use of dlopen/dlclose
# dlclose_leak_LDADD            = dlclose_leak_so.so
dlclose_leak_LDADD            = -ldl
dlclose_leak_LDFLAGS          = $(AM_FLAG_M3264_PRI) \
                                -Wl,-rpath,$(top_builddir)/memcheck/tests/linux
