
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr

EXTRA_DIST = \
	brk.stderr.exp brk.vgtest \
	capget.vgtest capget.stderr.exp capget.stderr.exp2 capget.stderr.exp3 \
	dlclose_leak-no-keep.stderr.exp dlclose_leak-no-keep.stdout.exp \
	    dlclose_leak-no-keep.vgtest \
	dlclose_leak.stderr.exp dlclose_leak.stdout.exp \
	    dlclose_leak.vgtest \
	ioctl-tiocsig.vgtest ioctl-tiocsig.stderr.exp \
	lsframe1.vgtest lsframe1.stdout.exp lsframe1.stderr.exp \
	lsframe2.vgtest lsframe2.stdout.exp lsframe2.stderr.exp \
	rfcomm.vgtest rfcomm.stderr.exp \
	sigqueue.vgtest sigqueue.stderr.exp \
	stack_changes.stderr.exp stack_changes.stdout.exp \
	    stack_changes.stdout.exp2 stack_changes.vgtest \
	stack_switch.stderr.exp stack_switch.vgtest \
	syscalls-2007.vgtest syscalls-2007.stderr.exp \
	syslog-syscall.vgtest syslog-syscall.stderr.exp \
	sys-openat.vgtest sys-openat.stderr.exp sys-openat.stdout.exp \
	sys-statx.vgtest sys-statx.stderr.exp \
	timerfd-syscall.vgtest timerfd-syscall.stderr.exp \
	with-space.stderr.exp with-space.stdout.exp with-space.vgtest \
	proc-auxv.vgtest proc-auxv.stderr.exp getregset.vgtest \
	getregset.stderr.exp getregset.stdout.exp

check_PROGRAMS = \
	brk \
	capget \
	dlclose_leak dlclose_leak_so.so \
	ioctl-tiocsig \
	getregset \
	lsframe1 \
	lsframe2 \
	rfcomm \
	sigqueue \
	stack_changes \
	stack_switch \
	syscalls-2007 \
	syslog-syscall \
	sys-statx \
	timerfd-syscall \
	proc-auxv

if HAVE_AT_FDCWD
check_PROGRAMS += sys-openat
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
