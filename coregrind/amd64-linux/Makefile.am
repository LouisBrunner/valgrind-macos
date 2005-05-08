include $(top_srcdir)/Makefile.all.am
include $(top_srcdir)/Makefile.core-AM_CPPFLAGS.am

AM_CFLAGS = $(WERROR) -Winline -Wall -Wshadow -O -fomit-frame-pointer -g

noinst_HEADERS = \
	core_platform.h \
	vki_unistd.h

noinst_LIBRARIES = libplatform.a


libplatform_a_SOURCES = \
	core_platform.c \
	ldt.c

if USE_PIE
libplatform_a_CFLAGS = $(AM_CFLAGS) -fpie
else
libplatform_a_CFLAGS = $(AM_CFLAGS)
endif
