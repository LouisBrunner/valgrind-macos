noinst_SCRIPTS = filter_stderr

EXTRA_DIST = filter_stderr \
	dlclose.vgtest dlclose.stderr.exp \
	dlclose.stdout.exp \
	fpu-28-108.vgtest fpu-28-108.stderr.exp

noinst_PROGRAMS = \
	dlclose fpu-28-108 myprint.so

CFLAGS   = $(WERROR) -Winline -Wall -Wshadow -g
CXXFLAGS = $(CFLAGS)
CCASFLAGS = $(CFLAGS)

# C ones
dlclose_SOURCES		= dlclose.c
dlclose_LDADD		= -ldl
myprint_so_SOURCES	= myprint.c
myprint_so_LDFLAGS	= -shared

fpu_28_108_SOURCES	= fpu-28-108.S
