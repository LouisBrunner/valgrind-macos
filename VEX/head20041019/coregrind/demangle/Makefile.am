include $(top_srcdir)/Makefile.all.am
include $(top_srcdir)/Makefile.core-AM_CPPFLAGS.am

AM_CFLAGS = $(WERROR) -Winline -Wall -Wshadow -O -g

noinst_HEADERS = \
	ansidecl.h     \
	dyn-string.h   \
	demangle.h     \
	safe-ctype.h 

noinst_LIBRARIES = libdemangle.a

libdemangle_a_SOURCES = \
	cp-demangle.c cplus-dem.c dyn-string.c safe-ctype.c

## Ignore harmless warnings for these ones
cp-demangle.o: CFLAGS += -Wno-unused -Wno-shadow 
cplus-dem.o: CFLAGS += -Wno-unused

