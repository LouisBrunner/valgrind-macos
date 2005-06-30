##---------------------------------------------------------------------------
## These test core error checking, eg. "silly values" for malloc/calloc,
## pthread errors (and suppressions), signal handling errors, invalid fds for
## blocking syscalls, etc.
##---------------------------------------------------------------------------

noinst_SCRIPTS = filter_stderr

EXTRA_DIST = $(noinst_SCRIPTS) \
	erringfds.stderr.exp erringfds.stdout.exp erringfds.vgtest \
	sigkill.stderr.exp sigkill.stderr.exp2 sigkill.vgtest \
	stack_changes.vgtest

check_PROGRAMS = \
	erringfds sigkill stack_changes

AM_CFLAGS   = $(WERROR) -Winline -Wall -Wshadow -g -O0
AM_CPPFLAGS = -I$(top_srcdir)/include
AM_CXXFLAGS = $(AM_CFLAGS)

sigkill_SOURCES		= sigkill.c

# Stack tests
stack_changes_SOURCES   = stack_changes.c
