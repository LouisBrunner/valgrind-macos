## Process this file with automake to produce Makefile.in

##---------------------------------------------------------------------------
## These test core error checking, eg. "silly values" for malloc/calloc,
## pthread errors (and suppressions), signal handling errors, invalid fds for
## blocking syscalls, etc.
##---------------------------------------------------------------------------

noinst_PROGRAMS = \
	erringfds malloc3 sigkill \
	pth_atfork1 pth_cancel2 pth_cvsimple pth_empty \
	pth_mutexspeed pth_once

CFLAGS   = $(WERROR) -Winline -Wall -Wshadow -g
CXXFLAGS = $(CFLAGS)

# C ones
erringfds_SOURCES 	= erringfds.c
malloc3_SOURCES 	= malloc3.c
sigkill_SOURCES 	= sigkill.c

# Pthread ones
pth_atfork1_SOURCES	= pth_atfork1.c
pth_atfork1_LDADD	= -lpthread
pth_cancel2_SOURCES	= pth_cancel2.c
pth_cancel2_LDADD	= -lpthread
pth_cvsimple_SOURCES	= pth_cvsimple.c
pth_cvsimple_LDADD	= -lpthread
pth_empty_SOURCES 	= pth_empty.c
pth_empty_LDADD 	= -lpthread
pth_mutexspeed_SOURCES	= pth_mutexspeed.c
pth_mutexspeed_LDADD	= -lpthread
pth_once_SOURCES	= pth_once.c
pth_once_LDADD		= -lpthread


