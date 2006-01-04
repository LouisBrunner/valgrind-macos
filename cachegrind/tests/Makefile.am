
# For AM_FLAG_M3264_PRI
include $(top_srcdir)/Makefile.flags.am

SUBDIRS = .
if VG_X86_LINUX
SUBDIRS += x86
endif
if VG_AMD64_LINUX
SUBDIRS += amd64
endif
if VG_PPC32_LINUX
SUBDIRS += ppc32
endif
if VG_PPC64_LINUX
SUBDIRS += ppc64
endif

DIST_SUBDIRS = ${VG_ARCH_ALL} .

noinst_SCRIPTS = filter_stderr filter_cachesim_discards

EXTRA_DIST = $(noinst_SCRIPTS) \
	chdir.vgtest chdir.stderr.exp \
	clreq.vgtest clreq.stderr.exp \
	dlclose.vgtest dlclose.stderr.exp dlclose.stdout.exp

check_PROGRAMS = \
	chdir clreq dlclose myprint.so

AM_CFLAGS   = $(WERROR) -Winline -Wall -Wshadow -g $(AM_FLAG_M3264_PRI)

# C ones
dlclose_LDADD		= -ldl
myprint_so_SOURCES	= myprint.c
myprint_so_LDFLAGS	= $(AM_FLAG_M3264_PRI) -shared -fPIC
myprint_so_CFLAGS	= $(AM_FLAG_M3264_PRI) -fPIC
