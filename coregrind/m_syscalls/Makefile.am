include $(top_srcdir)/Makefile.all.am
include $(top_srcdir)/Makefile.core-AM_CPPFLAGS.am

EXTRA_DIST = \
	README_SYSCALLS.txt

noinst_HEADERS = \
	priv_syscalls.h \
	priv_syscalls-linux.h \
	priv_syscalls-amd64-linux.h \
	priv_syscalls-arm-linux.h \
	priv_syscalls-x86-linux.h

noinst_LIBRARIES = libsyscalls.a

libsyscalls_a_SOURCES = \
	syscall-@VG_PLATFORM@.S \
	syscalls.c \
	syscalls-@VG_OS@.c \
	syscalls-@VG_PLATFORM@.c
