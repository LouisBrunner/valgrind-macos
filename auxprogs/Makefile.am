
SUBDIRS = . 

AM_CPPFLAGS = -I$(top_srcdir)/include -I$(top_srcdir)/coregrind \
		-DVG_LIBDIR="\"$(libdir)"\"
AM_CFLAGS = $(WERROR) -Winline -Wall -O -g

bin_PROGRAMS = valgrind-listener

valgrind_listener_SOURCES = \
	valgrind-listener.c

