include $(top_srcdir)/Makefile.all.am

# HACK WARNING: automake isn't good at supporting non-$(CC) compilers.
# But we need to use $(MPI_CC) for the MPI stuff.  So we have this mpi stuff
# in its own directory so we can use the following blunt instruments, which
# override the C compiler and all the default flags.
CC = $(MPI_CC)
DEFS =
DEFAULT_INCLUDES =
CPPFLAGS =
CFLAGS =
LDFLAGS =

EXTRA_DIST = \
	libmpiwrap_aix5.exp \
	mpiwrap_type_test.c

#----------------------------------------------------------------------------
# libmpiwrap-<platform>.so
#----------------------------------------------------------------------------

noinst_PROGRAMS  =
if BUILD_MPIWRAP_PRI
noinst_PROGRAMS += libmpiwrap-@VGCONF_ARCH_PRI@-@VGCONF_OS@.so
endif
if BUILD_MPIWRAP_SEC
noinst_PROGRAMS += libmpiwrap-@VGCONF_ARCH_SEC@-@VGCONF_OS@.so
endif

if VGCONF_OS_IS_DARWIN
noinst_DSYMS = $(noinst_PROGRAMS)
endif


if VGCONF_OS_IS_LINUX
 CFLAGS_MPI  = -g -O -fno-omit-frame-pointer -Wall -fpic
 LDFLAGS_MPI = -fpic -shared
 MPI_FLAG_M3264_PRI = $(AM_FLAG_M3264_PRI)
 MPI_FLAG_M3264_SEC = $(AM_FLAG_M3264_SEC)
endif
if VGCONF_OS_IS_AIX5
 # MPICC on AIX is called XLC.  It uses weird options.
 CFLAGS_MPI = -g -O -qflag=w:w -qlanglvl=extended
 LDFLAGS_MPI = -bE:libmpiwrap_aix5.exp -bM:SRE -bnoentry \
			-qflag=w:w -qlanglvl=extended
 # The sed bit changes eg. "maix32" to "q32" which is the XLC option.
 MPI_FLAG_M3264_PRI = `echo $(AM_FLAG_M3264_PRI) | sed s/maix/q/g`
 MPI_FLAG_M3264_SEC = `echo $(AM_FLAG_M3264_SEC) | sed s/maix/q/g`
endif
if VGCONF_OS_IS_DARWIN
 CFLAGS_MPI  = -g -O -fno-omit-frame-pointer -Wall -dynamic
 LDFLAGS_MPI = -dynamic -dynamiclib -all_load
 MPI_FLAG_M3264_PRI = $(AM_FLAG_M3264_PRI)
 MPI_FLAG_M3264_SEC = $(AM_FLAG_M3264_SEC)
endif


if BUILD_MPIWRAP_PRI
libmpiwrap_@VGCONF_ARCH_PRI@_@VGCONF_OS@_so_SOURCES  = libmpiwrap.c
libmpiwrap_@VGCONF_ARCH_PRI@_@VGCONF_OS@_so_CPPFLAGS = -I../include
libmpiwrap_@VGCONF_ARCH_PRI@_@VGCONF_OS@_so_CFLAGS   = \
	$(CFLAGS_MPI) $(MPI_FLAG_M3264_PRI)
libmpiwrap_@VGCONF_ARCH_PRI@_@VGCONF_OS@_so_LDFLAGS  = $(LDFLAGS_MPI)
endif
if BUILD_MPIWRAP_SEC
libmpiwrap_@VGCONF_ARCH_SEC@_@VGCONF_OS@_so_SOURCES  = libmpiwrap.c
libmpiwrap_@VGCONF_ARCH_SEC@_@VGCONF_OS@_so_CPPFLAGS = -I../include
libmpiwrap_@VGCONF_ARCH_SEC@_@VGCONF_OS@_so_CFLAGS   = \
	$(CFLAGS_MPI) $(MPI_FLAG_M3264_SEC)
libmpiwrap_@VGCONF_ARCH_SEC@_@VGCONF_OS@_so_LDFLAGS  = $(LDFLAGS_MPI)
endif

#----------------------------------------------------------------------------
# General stuff
#----------------------------------------------------------------------------

all-local: inplace-noinst_PROGRAMS inplace-noinst_DSYMS

clean-local: clean-noinst_DSYMS

install-exec-local: install-noinst_PROGRAMS install-noinst_DSYMS

