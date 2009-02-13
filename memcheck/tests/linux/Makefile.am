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

AM_CPPFLAGS  = -I$(top_srcdir)/include
AM_CFLAGS    = $(WERROR) $(AM_FLAG_M3264_PRI) -Winline -Wall -Wshadow -g \
		$(FLAG_MMMX) $(FLAG_MSSE)
AM_CXXFLAGS  = $(AM_CFLAGS)

stack_switch_LDADD    = -lpthread
timerfd_syscall_LDADD = -lrt
