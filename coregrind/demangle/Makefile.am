
AM_CPPFLAGS = -I$(top_srcdir)/coregrind -I$(top_srcdir)/include
AM_CFLAGS = $(WERROR) -Winline -Wall -Wshadow -O -fomit-frame-pointer -g

noinst_HEADERS = \
	ansidecl.h     \
	dyn-string.h   \
	demangle.h     \
	safe-ctype.h 

noinst_LIBRARIES = libdemangle.a

libdemangle_a_SOURCES = \
	cp-demangle.c cplus-dem.c dyn-string.c safe-ctype.c

cp-demangle.o: CFLAGS += -Wno-unused -Wno-shadow 
cplus-dem.o: CFLAGS += -Wno-unused

