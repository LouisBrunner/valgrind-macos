noinst_SCRIPTS = filter_stderr

EXTRA_DIST = $(noinst_SCRIPTS) \
	dlclose.vgtest dlclose.stderr.exp \
	dlclose.stdout.exp \
	fpu-28-108.vgtest fpu-28-108.stderr.exp

check_PROGRAMS = \
	dlclose fpu-28-108 myprint.so

AM_CFLAGS   = $(WERROR) -Winline -Wall -Wshadow -g
AM_CXXFLAGS = $(AM_CFLAGS)
AM_CCASFLAGS = $(AM_CFLAGS)

# C ones
dlclose_SOURCES		= dlclose.c
dlclose_LDADD		= -ldl
myprint_so_SOURCES	= myprint.c
myprint_so_LDFLAGS	= -shared

fpu_28_108_SOURCES	= fpu-28-108.S
