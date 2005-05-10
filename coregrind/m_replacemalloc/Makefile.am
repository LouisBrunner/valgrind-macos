include $(top_srcdir)/Makefile.all.am
include $(top_srcdir)/Makefile.core-AM_CPPFLAGS.am

AM_CFLAGS = $(WERROR) -Wmissing-prototypes -Winline -Wall -Wshadow -O -g

EXTRA_DIST = \
	README_REPLACEMALLOC.txt

noinst_LIBRARIES = \
	libreplacemalloc_core.a \
	libreplacemalloc_toolpreload.a

libreplacemalloc_core_a_SOURCES = \
	replacemalloc_core.c

if USE_PIE
libreplacemalloc_core_a_CFLAGS 	= $(AM_CFLAGS) -fpie
else
libreplacemalloc_core_a_CFLAGS	= $(AM_CFLAGS)
endif

libreplacemalloc_toolpreload_a_SOURCES = \
	vg_replace_malloc.c
libreplacemalloc_toolpreload_a_CFLAGS = \
	$(AM_CFLAGS) -fpic -fno-omit-frame-pointer


