noinst_SCRIPTS = filter_stderr

EXTRA_DIST = $(noinst_SCRIPTS) \
	capget capget.stderr.exp \
	timerfd-syscall timerfd-syscall.stderr.exp

check_PROGRAMS = \
	capget \
	timerfd-syscall

AM_CPPFLAGS  = -I$(top_srcdir)/include
AM_CFLAGS    = $(WERROR) @FLAG_M32@ -Winline -Wall -Wshadow -g \
		$(FLAG_MMMX) $(FLAG_MSSE)
AM_CXXFLAGS  = $(AM_CFLAGS)
AM_CCASFLAGS = @FLAG_M32@

timerfd_syscall_LDADD = -lrt
