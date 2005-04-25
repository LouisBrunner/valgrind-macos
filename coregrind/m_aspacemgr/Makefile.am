include $(top_srcdir)/Makefile.all.am
include $(top_srcdir)/Makefile.core-AM_CPPFLAGS.am

AM_CFLAGS = $(WERROR) -Wmissing-prototypes -Winline -Wall -Wshadow -O -g

EXTRA_DIST = \
	README_ASPACEMGR.txt

noinst_LIBRARIES = libaspacemgr.a

libaspacemgr_a_SOURCES = \
	read_procselfmaps.c \
	aspacemgr.c

if USE_PIE
libaspacemgr_a_CFLAGS = $(AM_CFLAGS) -fpie
else
libaspacemgr_a_CFLAGS = $(AM_CFLAGS)
endif
