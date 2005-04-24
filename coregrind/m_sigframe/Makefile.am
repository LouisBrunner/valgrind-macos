include $(top_srcdir)/Makefile.all.am
include $(top_srcdir)/Makefile.core-AM_CPPFLAGS.am

AM_CFLAGS = $(WERROR) -Wmissing-prototypes -Winline -Wall -Wshadow -O -g

EXTRA_DIST = \
	README_SIGFRAME.txt

noinst_LIBRARIES = libsigframe.a

libsigframe_a_SOURCES = \
	sigframe-@VG_PLATFORM@.c

if USE_PIE
libsigframe_a_CFLAGS = $(AM_CFLAGS) -fpie
else
libsigframe_a_CFLAGS = $(AM_CFLAGS)
endif
