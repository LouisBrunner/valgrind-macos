
# For AM_FLAG_M3264_PRI
include $(top_srcdir)/Makefile.flags.am

SUBDIRS = .
if VGP_X86_LINUX
SUBDIRS += x86
endif
if VGP_AMD64_LINUX
SUBDIRS += amd64
endif
if VGP_PPC32_LINUX
SUBDIRS += ppc32
endif
if VGP_PPC64_LINUX
SUBDIRS += ppc64
endif

DIST_SUBDIRS = ${VG_ARCH_ALL} .

noinst_SCRIPTS = filter_stderr filter_cachesim_discards

EXTRA_DIST = $(noinst_SCRIPTS) \
	chdir.vgtest chdir.stderr.exp \
	clreq.vgtest clreq.stderr.exp \
	dlclose.vgtest dlclose.stderr.exp dlclose.stdout.exp \
	wrap5.vgtest wrap5.stderr.exp wrap5.stdout.exp

check_PROGRAMS = \
	chdir clreq dlclose myprint.so wrap5

AM_CPPFLAGS = -I$(top_srcdir)/include
AM_CFLAGS   = $(WERROR) -Winline -Wall -Wshadow -g $(AM_FLAG_M3264_PRI)

# C ones
dlclose_LDADD		= -ldl
myprint_so_SOURCES	= myprint.c
myprint_so_LDFLAGS	= $(AM_FLAG_M3264_PRI) -shared -fPIC
myprint_so_CFLAGS	= $(AM_FLAG_M3264_PRI) -fPIC

if VGP_PPC32_AIX5
if VGP_PPC64_AIX5
else
 # persuade the AIX linker not to junk apparently unused
 # function descriptors.  Unfortunately -Wl,-G -Wl,-bnogc
 # produces a link error on 64-bit AIX, hence only 32-bit
 # gets these flags.
 wrap5_LDFLAGS		= $(AM_FLAG_M3264_PRI) -Wl,-G -Wl,-bnogc
endif
endif
