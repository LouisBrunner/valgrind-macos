include $(top_srcdir)/Makefile.all.am
include $(top_srcdir)/Makefile.core-AM_CPPFLAGS.am

noinst_LIBRARIES = libdispatch.a

## XXX: this won't work with "make dist" -- only one dispatch-XXX.S file
## will be put in the tarball.  Problem exists in various other Makefiles.
libdispatch_a_SOURCES = \
	dispatch-@VG_ARCH@.S
