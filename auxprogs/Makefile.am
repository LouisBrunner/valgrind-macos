include $(top_srcdir)/Makefile.all.am
include $(top_srcdir)/Makefile.flags.am
include $(top_srcdir)/Makefile.core-tool.am

bin_PROGRAMS = valgrind-listener

noinst_SCRIPTS = gen-mdg DotToScc.hs Merge3Way.hs primes.c \
		gsl16test gsl16-badfree.patch gsl16-wavelet.patch \
		gsl19test \
		ppcfround.c ppc64shifts.c libmpiwrap.c mpiwrap_type_test.c \
		aix5_VKI_info.c libmpiwrap_aix5.exp \
		aix5_proc_self_sysent.c \
		posixtestsuite-1.5.1-diff-results \
		posixtestsuite-1.5.1-diff.txt

EXTRA_DIST = $(noinst_SCRIPTS)


#------------------------- listener -----------------------
# Build valgrind_listener for the primary target only.
#
valgrind_listener_SOURCES = valgrind-listener.c

valgrind_listener_CPPFLAGS  = $(AM_CPPFLAGS_PRI) -I$(top_srcdir)/coregrind
valgrind_listener_CFLAGS    = $(AM_CFLAGS_PRI)
valgrind_listener_CCASFLAGS = $(AM_CCASFLAGS_PRI)
valgrind_listener_LDFLAGS   = $(AM_CFLAGS_PRI)
#
#----------------------------------------------------------

#------------------------- mpi wrappers -----------------------
# Build libmpiwrap.so for the primary target, and for the secondary
# target if relevant.
#
# This is really horrible.
#
# automake isn't good at supporting non-$(CC) compilers.
# But we need to use $(MPI_CC) here.  Hence the nasty hack of
# directly saying how to build libmpiwrap-*.so, instead of
# using automake's standard gunk.
#
# XXX: if we put the MPI stuff in its own directory and manually added
# "CC = $(MPI_CC)" then I think the usual automake approach would work.
# --njn
#
if VGCONF_OS_IS_AIX5
 HACKY_FLAGS_PRI = -g -O -bE:libmpiwrap_aix5.exp -bM:SRE -bnoentry \
			-qflag=w:w -qlanglvl=extended \
			`echo $(AM_FLAG_M3264_PRI) | sed s/maix/q/g`
 HACKY_FLAGS_SEC = -g -O -bE:libmpiwrap_aix5.exp -bM:SRE -bnoentry \
			-qflag=w:w -qlanglvl=extended \
			`echo $(AM_FLAG_M3264_SEC) | sed s/maix/q/g`
else
if VGCONF_OS_IS_DARWIN
 HACKY_FLAGS_PRI = -g -O -fno-omit-frame-pointer -Wall -dynamic \
			-dynamiclib -all_load $(AM_FLAG_M3264_PRI)
 HACKY_FLAGS_SEC = -g -O -fno-omit-frame-pointer -Wall -dynamic \
			-dynamiclib -all_load $(AM_FLAG_M3264_SEC)
else
 HACKY_FLAGS_PRI = -g -O -fno-omit-frame-pointer -Wall -fpic -shared \
			$(AM_FLAG_M3264_PRI)
 HACKY_FLAGS_SEC = -g -O -fno-omit-frame-pointer -Wall -fpic -shared \
			$(AM_FLAG_M3264_SEC)
endif
endif


## First, we have to say how to build the .so's ..
##
noinst_PROGRAMS =
noinst_DSYMS =
if BUILD_MPIWRAP_PRI
noinst_PROGRAMS += libmpiwrap-@VGCONF_ARCH_PRI@-@VGCONF_OS@.so
if VGCONF_OS_IS_DARWIN
noinst_DSYMS    += libmpiwrap-@VGCONF_ARCH_PRI@-@VGCONF_OS@.so
endif
libmpiwrap-@VGCONF_ARCH_PRI@-@VGCONF_OS@.so: libmpiwrap.c
	$(MPI_CC) $(HACKY_FLAGS_PRI) \
		-I../include \
		-o libmpiwrap-@VGCONF_ARCH_PRI@-@VGCONF_OS@.so libmpiwrap.c
endif
if BUILD_MPIWRAP_SEC
noinst_PROGRAMS += libmpiwrap-@VGCONF_ARCH_SEC@-@VGCONF_OS@.so
if VGCONF_OS_IS_DARWIN
noinst_DSYMS    += libmpiwrap-@VGCONF_ARCH_SEC@-@VGCONF_OS@.so
endif
libmpiwrap-@VGCONF_ARCH_SEC@-@VGCONF_OS@.so: libmpiwrap.c
	$(MPI_CC) $(HACKY_FLAGS_SEC) \
		-I../include \
		-o libmpiwrap-@VGCONF_ARCH_SEC@-@VGCONF_OS@.so libmpiwrap.c
endif


## And here we say how to install them.
##
# The following install hack is serialised by "libmpiwrap.so".
# Hence force -j 1.
.NOTPARALLEL:

clean-local: clean-noinst_DSYMS

install-exec-local: install-noinst_PROGRAMS install-noinst_DSYMS


## And some hacks to keep 'make dist' happy.
## It would seem that using nodist_SOURCES is the right fix, but
## I can't figure out how to do it.
##
#nodist_SOURCES        = libmpiwrap-@VGCONF_ARCH_PRI@-@VGCONF_OS@.c
#nodist_SOURCES        = libmpiwrap-@VGCONF_ARCH_SEC@-@VGCONF_OS@.c

libmpiwrap-.c:
	rm -f libmpiwrap-.c
	touch libmpiwrap-.c

libmpiwrap-@VGCONF_ARCH_PRI@-@VGCONF_OS@.c:
	rm -f libmpiwrap-@VGCONF_ARCH_PRI@-@VGCONF_OS@.c
	touch libmpiwrap-@VGCONF_ARCH_PRI@-@VGCONF_OS@.c

if VGCONF_HAVE_PLATFORM_SEC_CAPS
libmpiwrap-@VGCONF_ARCH_SEC@-@VGCONF_OS@.c:
	rm -f libmpiwrap-@VGCONF_ARCH_SEC@-@VGCONF_OS@.c
	touch libmpiwrap-@VGCONF_ARCH_SEC@-@VGCONF_OS@.c
endif

#
#----------------------------------------------------------

