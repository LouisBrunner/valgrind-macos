
SUBDIRS = . 

INCLUDES = -I$(top_srcdir)/include -I$(top_srcdir)/coregrind

CFLAGS = $(WERROR) -DVG_LIBDIR="\"$(libdir)"\" \
		-Winline -Wall -O -g

bin_PROGRAMS = valgrind-listener

valgrind_listener_SOURCES = \
	valgrind-listener.c

