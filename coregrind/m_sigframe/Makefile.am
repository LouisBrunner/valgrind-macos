include $(top_srcdir)/Makefile.all.am
include $(top_srcdir)/Makefile.core.am

# Remember to include all the platform-specific files in the distribution.
EXTRA_DIST = \
	$(addsuffix .c,$(addprefix sigframe-,$(VG_PLATFORM_ALL)))

noinst_LIBRARIES = libsigframe.a

libsigframe_a_SOURCES = \
	sigframe-@VG_PLATFORM@.c
