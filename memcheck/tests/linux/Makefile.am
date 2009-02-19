
include $(top_srcdir)/Makefile.tool-tests.am

noinst_SCRIPTS = filter_stderr

EXTRA_DIST = $(noinst_SCRIPTS) \
	brk.stderr.exp brk.vgtest \
	capget capget.stderr.exp \
	stack_switch.stderr.exp stack_switch.vgtest \
	timerfd-syscall timerfd-syscall.stderr.exp

check_PROGRAMS = \
	brk \
	capget \
	stack_switch \
	timerfd-syscall

stack_switch_LDADD    = -lpthread
timerfd_syscall_LDADD = -lrt
