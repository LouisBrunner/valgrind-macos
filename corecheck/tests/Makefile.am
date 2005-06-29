##---------------------------------------------------------------------------
## These test core error checking, eg. "silly values" for malloc/calloc,
## pthread errors (and suppressions), signal handling errors, invalid fds for
## blocking syscalls, etc.
##---------------------------------------------------------------------------

noinst_SCRIPTS = filter_stderr

EXTRA_DIST = $(noinst_SCRIPTS) \
	erringfds.stderr.exp erringfds.stdout.exp erringfds.vgtest \
	pth_atfork1.stderr.exp pth_atfork1.stdout.exp pth_atfork1.vgtest \
	pth_cancel1.stderr.exp pth_cancel1.stdout.exp pth_cancel1.vgtest \
	pth_cancel2.stderr.exp pth_cancel2.vgtest \
	pth_cvsimple.stderr.exp pth_cvsimple.stdout.exp pth_cvsimple.vgtest \
	pth_empty.stderr.exp pth_empty.vgtest \
	pth_exit.stderr.exp pth_exit.vgtest \
	pth_exit2.stderr.exp pth_exit2.vgtest \
	pth_mutexspeed.stderr.exp \
	pth_mutexspeed.stdout.exp pth_mutexspeed.vgtest \
	pth_once.stderr.exp pth_once.stdout.exp pth_once.vgtest \
	pth_rwlock.stderr.exp pth_rwlock.vgtest \
	sigkill.stderr.exp sigkill.stderr.exp2 sigkill.vgtest \
	stack_changes.vgtest \
	res_search.stderr.exp res_search.stdout.exp res_search.vgtest \
	threadederrno.stderr.exp threadederrno.stdout.exp \
	threadederrno.vgtest

check_PROGRAMS = \
	erringfds \
	sigkill res_search \
	pth_atfork1 pth_cancel1 pth_cancel2 pth_cvsimple pth_empty \
	pth_exit pth_exit2 pth_mutexspeed pth_once pth_rwlock \
	threadederrno \
	stack_changes

AM_CFLAGS   = $(WERROR) -Winline -Wall -Wshadow -g -O0
AM_CPPFLAGS = -I$(top_srcdir)/include
AM_CXXFLAGS = $(AM_CFLAGS)

sigkill_SOURCES		= sigkill.c

# Pthread ones
pth_atfork1_SOURCES	= pth_atfork1.c
pth_atfork1_LDADD	= -lpthread
pth_cancel1_SOURCES	= pth_cancel1.c
pth_cancel1_LDADD	= -lpthread
pth_cancel2_SOURCES	= pth_cancel2.c
pth_cancel2_LDADD	= -lpthread
pth_cvsimple_SOURCES	= pth_cvsimple.c
pth_cvsimple_LDADD	= -lpthread
pth_empty_SOURCES 	= pth_empty.c
pth_empty_LDADD 	= -lpthread
pth_exit_SOURCES 	= pth_exit.c
pth_exit_LDADD	 	= -lpthread
pth_exit2_SOURCES 	= pth_exit2.c
pth_exit2_LDADD	 	= -lpthread
pth_mutexspeed_SOURCES	= pth_mutexspeed.c
pth_mutexspeed_LDADD	= -lpthread
pth_once_SOURCES	= pth_once.c
pth_once_LDADD		= -lpthread
pth_rwlock_SOURCES	= pth_rwlock.c
pth_rwlock_LDADD	= -lpthread
res_search_SOURCES      = res_search.c
res_search_LDADD        = -lresolv -lpthread
threadederrno_SOURCES	= threadederrno.c
threadederrno_LDADD	= -lpthread

# Stack tests
stack_changes_SOURCES   = stack_changes.c
