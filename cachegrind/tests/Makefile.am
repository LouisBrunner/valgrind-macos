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

DIST_SUBDIRS = ${VG_ARCH_ALL} .

noinst_SCRIPTS = filter_stderr filter_cachesim_discards

EXTRA_DIST = $(noinst_SCRIPTS) \
	chdir.vgtest chdir.stderr.exp \
	clreq.vgtest clreq.stderr.exp \
	dlclose.vgtest dlclose.stderr.exp dlclose.stdout.exp

check_PROGRAMS = \
	chdir clreq dlclose myprint.so

AM_CFLAGS   = $(WERROR) -Winline -Wall -Wshadow -g

# C ones
dlclose_LDADD		= -ldl
myprint_so_SOURCES	= myprint.c
myprint_so_LDFLAGS	= -shared -fPIC
myprint_so_CFLAGS	= -fPIC
