
SUBDIRS = . 

AM_CPPFLAGS = -I$(top_builddir)/include -I$(top_srcdir)/include \
		-I$(top_builddir)/coregrind -I$(top_srcdir)/coregrind
AM_CFLAGS = $(WERROR) -Winline -Wall -O -g

bin_PROGRAMS = valgrind-listener

valgrind_listener_SOURCES = \
	valgrind-listener.c

