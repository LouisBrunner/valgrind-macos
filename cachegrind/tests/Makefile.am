## Process this file with automake to produce Makefile.in

##---------------------------------------------------------------------------
## Cachegrind ones.
##---------------------------------------------------------------------------

noinst_SCRIPTS = filter_stderr

EXTRA_DIST = 
	$(noinst_SCRIPTS) \
	dlclose.vgtest dlclose.stderr.exp dlclose.stderr.exp.hd \
	dlclose.stdout.exp \
	fpu-28-108.vgtest fpu-28-108.stderr.exp fpu-28-108.stderr.exp.hd \
	fpu-28-108.stdout.exp

noinst_PROGRAMS = \
	dlclose fpu-28-108 myprint.so

CFLAGS   = $(WERROR) -Winline -Wall -Wshadow -g
CXXFLAGS = $(CFLAGS) 

# C ones
dlclose_SOURCES		= dlclose.c
dlclose_LDADD		= -ldl
myprint_so_SOURCES	= myprint.c
myprint_so_LDFLAGS	= -shared

fpu_28_108_SOURCES	= fpu-28-108.S

##myprint.so$(EXEEXT): $(myprint_so_OBJECTS)
##	$(CC) $(CFLAGS) -shared -o myprint.so $(myprint_so_OBJECTS)



