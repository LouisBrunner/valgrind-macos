
include $(top_srcdir)/Makefile.tool-tests.am

dist_noinst_SCRIPTS = filter_stderr

EXTRA_DIST = \
	brk.stderr.exp brk.vgtest \
	capget.vgtest capget.stderr.exp capget.stderr.exp2 \
	lsframe1.vgtest lsframe1.stdout.exp lsframe1.stderr.exp \
	lsframe2.vgtest lsframe2.stdout.exp lsframe2.stderr.exp \
	sigqueue.vgtest sigqueue.stderr.exp \
	stack_changes.stderr.exp stack_changes.stdout.exp \
	    stack_changes.stdout.exp2 stack_changes.vgtest \
	stack_switch.stderr.exp stack_switch.vgtest \
	syscalls-2007.vgtest syscalls-2007.stderr.exp \
	syslog-syscall.vgtest syslog-syscall.stderr.exp \
	timerfd-syscall.vgtest timerfd-syscall.stderr.exp \
	with-space.stderr.exp with-space.stdout.exp with-space.vgtest

check_PROGRAMS = \
	brk \
	capget \
	lsframe1 \
	lsframe2 \
	sigqueue \
	stack_changes \
	stack_switch \
	syscalls-2007 \
	syslog-syscall \
	timerfd-syscall


AM_CFLAGS   += $(AM_FLAG_M3264_PRI)
AM_CXXFLAGS += $(AM_FLAG_M3264_PRI)

stack_switch_LDADD    = -lpthread
timerfd_syscall_LDADD = -lrt

