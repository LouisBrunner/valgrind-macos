
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
cp_demange_c_CFLAGS = -Wno-unused -Wno-shadow -UHAVE_CONFIG_H
dyn_string_c_CFLAGS = -UHAVE_CONFIG_H
cplus_dem_c_CFLAGS = -Wno-unused
