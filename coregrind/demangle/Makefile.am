
INCLUDES = -I$(top_srcdir)/coregrind -I$(top_srcdir)/include

AM_CFLAGS = $(WERROR) -Winline -Wall -Wshadow -O -fomit-frame-pointer -g

noinst_HEADERS = \
	ansidecl.h     \
	dyn-string.h   \
	demangle.h     \
	safe-ctype.h 

noinst_LIBRARIES = libdemangle.a

libdemangle_a_SOURCES = \
	cp-demangle.c cplus-dem.c dyn-string.c safe-ctype.c

# some files don't like my config.h, so just pretend it does not exist...

cp-demangle.o: cp-demangle.c
	$(COMPILE) -Wno-unused -Wno-shadow -c $< -UHAVE_CONFIG_H

dyn-string.o: dyn-string.c
	$(COMPILE) -c $< -UHAVE_CONFIG_H

cplus-dem.o: cplus-dem.c
	$(COMPILE) -Wno-unused -c $<
