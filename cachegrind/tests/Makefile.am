SUBDIRS      = ${VG_ARCH}     .
DIST_SUBDIRS = ${VG_ARCH_ALL} .

noinst_SCRIPTS = filter_stderr filter_cachesim_discards

EXTRA_DIST = $(noinst_SCRIPTS) \
	chdir.vgtest chdir.stderr.exp \
	dlclose.vgtest dlclose.stderr.exp dlclose.stdout.exp

check_PROGRAMS = \
	chdir dlclose myprint.so

AM_CFLAGS   = $(WERROR) -Winline -Wall -Wshadow -g

# C ones
chdir_SOURCES		= chdir.c
dlclose_SOURCES		= dlclose.c
dlclose_LDADD		= -ldl
myprint_so_SOURCES	= myprint.c
myprint_so_LDFLAGS	= -shared -fPIC
myprint_so_CFLAGS	= -fPIC
