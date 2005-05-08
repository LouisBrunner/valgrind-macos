include $(top_srcdir)/Makefile.all.am
include $(top_srcdir)/Makefile.core-AM_CPPFLAGS.am

AM_CFLAGS = $(WERROR) -Wmissing-prototypes -Winline -Wall -Wshadow -O -g

noinst_HEADERS = \
	core_os.h

noinst_LIBRARIES = libos.a

libos_a_SOURCES = \
	core_os.c \
	sema.c

if USE_PIE
libos_a_CFLAGS = $(AM_CFLAGS) -fpie
else
libos_a_CFLAGS = $(AM_CFLAGS)
endif
