include $(top_srcdir)/Makefile.all.am
include $(top_srcdir)/Makefile.core-AM_CPPFLAGS.am

## Test repeated in both arms of the if-then-else because older versions of
## automake don't seem to like having += within an if-then-else.
if USE_PIE
AM_CFLAGS = $(WERROR) -Winline -Wall -Wshadow -O -g -fpie
else
AM_CFLAGS = $(WERROR) -Winline -Wall -Wshadow -O -g
endif

noinst_HEADERS = \
	ansidecl.h     \
	dyn-string.h   \
	demangle.h     \
	safe-ctype.h 

noinst_LIBRARIES = libdemangle.a

libdemangle_a_SOURCES = \
	cp-demangle.c \
	cplus-dem.c \
	demangle.c \
	dyn-string.c \
	safe-ctype.c

## Ignore harmless warnings for these ones
cp-demangle.o: CFLAGS += -Wno-unused -Wno-shadow 
cplus-dem.o: CFLAGS += -Wno-unused

