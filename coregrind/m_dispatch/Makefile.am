include $(top_srcdir)/Makefile.all.am
include $(top_srcdir)/Makefile.core-AM_CPPFLAGS.am

AM_CFLAGS = $(WERROR) -Wmissing-prototypes -Winline -Wall -Wshadow -O -g

noinst_LIBRARIES = libdispatch.a

## XXX: this won't work with "make dist" -- only one dispatch-XXX.S file
## will be put in the tarball.  Problem exists in various other Makefiles.
libdispatch_a_SOURCES = \
	dispatch-@VG_ARCH@.S

if USE_PIE
libdispatch_a_CFLAGS = $(AM_CFLAGS) -fpie
else
libdispatch_a_CFLAGS = $(AM_CFLAGS)
endif
