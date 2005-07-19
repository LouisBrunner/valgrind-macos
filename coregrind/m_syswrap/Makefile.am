include $(top_srcdir)/Makefile.all.am
include $(top_srcdir)/Makefile.core-AM_CPPFLAGS.am

# Remember to include all the OS/platform-specific files in the distribution.
EXTRA_DIST = \
	$(addsuffix .S,$(addprefix syscall-,$(VG_PLATFORM_ALL))) \
	$(addsuffix .c,$(addprefix syswrap-,$(VG_OS_ALL))) \
	$(addsuffix .c,$(addprefix syswrap-,$(VG_PLATFORM_ALL)))

noinst_HEADERS = \
	priv_types_n_macros.h \
	priv_syswrap-generic.h \
	priv_syswrap-linux.h \
	priv_syswrap-main.h

noinst_LIBRARIES = libsyswrap.a

libsyswrap_a_SOURCES = \
	syscall-@VG_PLATFORM@.S \
	syswrap-generic.c \
	syswrap-@VG_OS@.c \
	syswrap-@VG_PLATFORM@.c \
	syswrap-main.c
